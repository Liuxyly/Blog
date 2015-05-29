package com.test;

import java.io.File;
import java.io.FileOutputStream;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Properties;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import com.sap.conn.jco.JCoContext;
import com.sap.conn.jco.JCoDestination;
import com.sap.conn.jco.JCoDestinationManager;
import com.sap.conn.jco.JCoException;
import com.sap.conn.jco.JCoFunction;
import com.sap.conn.jco.JCoFunctionTemplate;
import com.sap.conn.jco.ext.DestinationDataProvider;
import com.sap.conn.jco.ext.Environment;
import com.sap.conn.jco.ext.JCoSessionReference;
import com.sap.conn.jco.ext.SessionException;
import com.sap.conn.jco.ext.SessionReferenceProvider;

/**
 * 
 * MultiThreadedExample is rather complex. It demonstrates演示 how to use the
 * SessionReferenceProvider会话提供者 defined in the package com.sap.conn.jco.ext.
 * 
 * Before discussing讨论 situations情况 requiring要求 SessionReferenceProvider, we
 * provide a short description of how the JCo Runtime handles the stateful（有状态）
 * and stateless（无状态） calls by default. By default all RFC calls
 * 默认情况下所有JCoFunction.execute执行都是无状态的 (JCoFunction.execute(JCoDestination)) are
 * stateless. That means the ABAP context associated with the connection will be
 * destroyed（意味着上下连接被销毁）. Some RFC modules save a particular state/data in the
 * ABAP context's area（有些函数组下的多个RFM会共用一全局变量）. In order to keep a JCo connection
 * and use it for subsequent (stateful) calls（为了保持多个RFM在同一连接中顺序调用）, the
 * JCoConext.begin(JCoDestination) API can be used. In the case of multithreaded
 * applications some calls to a destination can be executed concurrently（同时，并发）,
 * so JCo Runtime（JCo运行时环境需） needs to associate a particular call or connection
 * to an internal session. By default JCo Runtime associates each thread with a
 * session of its（默认情况下每个线程都有它自己的会话） own, so that most applications that execute
 * all stateful requests en bloc（整体） or at least in the same thread will run
 * correctly.
 *
 * 
 * Applications that wish to execute calls belonging to a stateful sequence by
 * employing（采用） different threads have to implement and register the
 * SessionReferenceProvider. The main goal of（主要目标） the implementation is to
 * determine to which session the calls executing in the current thread belong.
 *
 * 
 * This example defines MultiStepJob having several execution
 * steps（该示例的任务有多个步骤）. The test starts a certain number of threads (see
 * runJobs). Each thread is designed to take a job, execute one step, and put 
 * the job back to the shared job list. There are two jobs as an example:
 * StatelessMultiStepExample and StatefulMultiStepExample. Both invoke the same
 * RFC modules, but StatefulMultiStepExample uses JCoContext.begin and
 * JCoContext.end to specify the stateful calls.
 * 
 * To be able to execute a stateful call sequence distributed over several
 * steps, we register a custom implementation of SessionReferenceProvider called
 * MySessionReferenceProvider. The idea behind MySessionReferenceProvider is
 * simple: each thread holds the current session reference in its local storage.
 * To achieve（实现） that WorkerThread.run sets this session reference before
 * executing the next step and removes it after the step is finished.
 */

public class MultiThreadedExample {
	private static BlockingQueue<MultiStepJob> queue = new LinkedBlockingQueue<MultiStepJob>();
	private static JCoFunctionTemplate incrementCounterTemplate, getCounterTemplate;

	// 任务接口
	interface MultiStepJob {
		String getName();// 任务名
		boolean isFinished();// 任务是否
		public void runNextStep();// 运行任务
		public void cleanUp();// 清除任务
	}

	// 无状态远程RFM的调用（增加计数与读取计数RFM虽然在这里是在同一会话中调用的——不一定是在同一连接中，
	// 但没有调用JCoContext.begin方法让ABAP保留每次被调用后的内存状态：计数器全局变量 count的值）
	static class StatelessMultiStepExample implements MultiStepJob {
		static AtomicInteger JOB_COUNT = new AtomicInteger(0);
		int jobID = JOB_COUNT.addAndGet(1);// 任务编号
		private int calls;// 需要调用多少次
		JCoDestination destination;// 远程目标
		int executedCalls = 0;// 记录调用次数，即任务步骤
		Exception ex = null;// 记录任务执行过程出现的异常
		int remoteCounter;// 计数结果
		
		public StatelessMultiStepExample(JCoDestination destination, int calls/* 调用次数 */) {
			this.calls = calls;
			this.destination = destination;
		}

		public boolean isFinished() {
			// 如果Z_INCREMENT_COUNTER已经调用了10次，或者调用过程中出现了异常时，表示任务已完成
			return executedCalls == calls || ex != null;
		}

		public String getName() {// 任务名
			return "无状态调用 Job-" + jobID;
		}

		// 任务的某一步，究竟有多少步则外界来传递进来的calls变量来控制
		public void runNextStep() {
			try {
				
				// 注：在调用远程RFC功能函数（如这里的incrementCounter、getCounter）之前，JCo框架会去调用
				// SessionReferenceProvider的getCurrentSessionReference()方法，
				// 取得当前任务所对应的远程会话，确保同一任务是在同一远程会话中执行的
				
				// 增加计数：即RFM中的count全局变量加一
				JCoFunction incrementCounter = incrementCounterTemplate.getFunction();
				incrementCounter.execute(destination);
				executedCalls++;// 调用了多少次
				if (isFinished()) {// 任务完后（这里调用10次），才读取计数器
					JCoFunction getCounter = getCounterTemplate.getFunction();
					getCounter.execute(destination);
					// 读取计数：即读取RFM中的count全局变量
					remoteCounter = getCounter.getExportParameterList().getInt( "GET_VALUE");
				}
			} catch (JCoException je) {
				ex = je;
			} catch (RuntimeException re) {
				ex = re;
			}
		}

		public void cleanUp() {// 任务结束后，清除任务
			StringBuilder sb = new StringBuilder("任务 ").append(getName()).append(" 结束：");
			if (ex != null) {
				sb.append("异常结束 ").append(ex.toString());
			} else {
				sb.append("成功执行完，计数器值 = ").append(remoteCounter);
			}
			System.out.println(sb.toString());
		}
	}

	// 有状态远程RFM调用（增加计数与读取计数RFM在同一远程会话中执行，保留了内存状态：计数器全局变量 count的值）
	static class StatefulMultiStepExample extends StatelessMultiStepExample {
		StatefulMultiStepExample(JCoDestination destination, int calls) {
			super(destination, calls);
		}

		@Override
		public String getName() {
			return "有状态调用 Job-" + jobID;
		}

		@Override
		public void runNextStep() {
			
			// 如果是任务的第一步，则需要让ABAP端保留函数运行后的上下文（内存）状态
			if (executedCalls == 0) {
				// begin()与end()之间表示多个RFM执行会在同一个连接中执行，并且这之间的多个RFM属于同一个LUW，并且按照调用的顺序来执行
				// ****不管是否有无状态RFM调用（加begin后无状态调用至少还可以保证同一任务中多个函数调用的顺序），都要确保同一任务
				// ****（多个RFM所组成的远程调用任务）在同一会话中执行，要做到这一点，在Java端需要保证不同线程（同一线程也是）
				// ****在执行同一任务时，JCo连接与远程会话都要是同一个
				JCoContext.begin(destination);// 开启状态调用，会话在begin与end之间不会被重置与关闭，这样
				// SAP端用户的上下文件就会被保持
			}
			super.runNextStep();
		}

		@Override
		public void cleanUp() {
			try {
				JCoContext.end(destination);
			} catch (JCoException je) {
				ex = je;
			}
			super.cleanUp();
		}
	}

	static class MySessionReference implements JCoSessionReference {// 远程会话实现
		static AtomicInteger atomicInt = new AtomicInteger(0);
		// 远程会话ID
		private String id = "session-" + String.valueOf(atomicInt.addAndGet(1));;
		public void contextFinished() {
		}
		public void contextStarted() {
		}
		public String getID() {
			return id;
		}
	}

	// 工作线程，用来执行前面定义的任务：StatelessMultiStepExample、StatefulMultiStepExample
	static class WorkerThread extends Thread {
		
		// 任务与远程会话映射关系表：确保同一任务要在同一远程会话中执行
		static Hashtable<MultiStepJob, MySessionReference> sessions =
				new Hashtable<MultiStepJob, MySessionReference>();
		
		// ThreadLocal：线程全局变量局部化，即将原本共享的属性全局变量在每个线程中都拷贝一份，不会让它们再在不同的线程中共享，
		// 每个线程拿到的都是自己所独享的，所以看似全局共享的属性在多线程情况下，也不会出现多线程并发问题
		// 当前线程所使用的远程会话
		static ThreadLocal<MySessionReference> localSessionReference =
				new ThreadLocal<MySessionReference>();

		// 同步器：倒计时闭锁；threadCount为倒计数值，直到该数为0时，await()才会结束继续往下执行
		// CountDownLatch同步器的作用就是让所有线程都准备好以后，真正同时开始执行，这样不会因为先创建的
		// 的线程就会先执行，可以真正模拟多线程同时执行的情况，这样在研究多线程在访问同一临界资源时，容易发现线程并发问题
		private CountDownLatch startSignal;// 开始阀：所以线程都已启动并就绪时，所有线程不再阻塞
		private CountDownLatch doneSignal;// 结束阀：所以线程结束后，主线程才结束
		WorkerThread(CountDownLatch startSignal, CountDownLatch doneSignal) {
			this.startSignal = startSignal;
			this.doneSignal = doneSignal;
		}

		// 工作线程
		public void run() {
			startSignal.countDown();
			try {
				startSignal.await();// 所有线程都已经运行到这里后，才开始一起同时向下执行，否则一直阻塞
				// 某一时间段内（即一次循环）只执行某个任务的一个步骤
				for (;;) {// 直到任务队列中没有任务时退出
					// 出队，工作线程从任务队列中取任务：如果等10秒都未取到，则返回NULL
					MultiStepJob job = queue.poll(10, TimeUnit.SECONDS);
					// stop if nothing to do
					if (job == null) {// 如果任务队列中没有任务后，工作线程将退出
						return;
					}
					
					// 取任务所对应的远程会话，确保每个任务使用同一远程会话
					MySessionReference sesRef = sessions.get(job);
					if (sesRef == null) {// 如果是第一次，则新创建一个远程会话，再将任务与该会话进行绑定
						sesRef = new MySessionReference();
						sessions.put(job, sesRef);
					}

					// 存储当前线程所使用的远程会话。该值的读取是在调用远程RFM前，由JCo框架的
					// SessionReferenceProvider的getCurrentSessionReference()方法来读取
					// ****不管是否有无状态RFM调用，最好都要确保同一任务（多个RFM所组成的远程调用任务）在同一会话中执行
					// ****，要做到这一点，在Java端需要保证不同线程（同一线程也是）在执行同一任务时，远程会话要是同一个
					// 注：同一任务需要设置为同一远程会话，不同任务不能设置为相同的远程会话，否则计数器会在多个任务中共用
					localSessionReference.set(sesRef);
					System.out.println("任务 " + job.getName() + " 开始执行.");
					try {
						// 执行任务
						job.runNextStep();
					} catch (Throwable th) {
						th.printStackTrace();
					}

					// 如果任务完成（调用远程RFM计数器函数10次）
					if (job.isFinished()) {
						System.out.println("任务 " + job.getName() + " 执行完成.");
						
						// 如果任务执行完了，则从映射表是删除任务与远程会话映射记录
						sessions.remove(job);
						job.cleanUp();// 任务的所有步骤执行完后，输出任务结果
					} else {
						System.out.println("任务 " + job.getName() + " 未完成，重新放入任务队列，等待下次继续执行.");
						// 如果发现任务还没有执行完，则重新放入任务队列中，等待下一次继续执行。从这里可以看出
						// 计数器的增加与读取可能是由不同的工作线程来完成的，但要确保同一任务是在同一远程会话中调用的
						queue.add(job);
					}

					// 当某个任务某一步执行完后，清除当前线程所存储的远程会话。
					//注：这里的工作线程某一时间段内（即一次循环内）只能执行一个任务
					localSessionReference.set(null);
				}
			} catch (InterruptedException e) {
				e.printStackTrace();
			} finally {
				doneSignal.countDown();
			}
		}
	}

	// 远程会话提供者：负责拿到当前任务的远程会话
	static class MySessionReferenceProvider implements SessionReferenceProvider {
		public JCoSessionReference getCurrentSessionReference(String scopeType) {
			// 从当前线程中读取相应的远程会话，这样确保了同一任务中多个RFM的调用是在同一远程会话连接中执行的
			MySessionReference sesRef = WorkerThread.localSessionReference.get();

			if (sesRef != null) {
				return sesRef;
			} throw new RuntimeException("Unknown thread:" + Thread.currentThread().getId());
		}

		// 远程会话是否活着，JCo框架调用此来决定此连接是否销毁？
		public boolean isSessionAlive(String sessionId) {
			Collection<MySessionReference> availableSessions = WorkerThread.sessions.values();
			
			for (MySessionReference ref : availableSessions) {
				if (ref.getID().equals(sessionId)) {
					return true;
				}
			}
			return false;
		}

		public void jcoServerSessionContinued(String sessionID)
				throws SessionException {
		}
		
		public void jcoServerSessionFinished(String sessionID) {
		}

		public void jcoServerSessionPassivated(String sessionID)
				throws SessionException {

		}

		public JCoSessionReference jcoServerSessionStarted()
				throws SessionException {
			return null;
		}
	}

	// 创建任务与工作线程并拉起
	static void runJobs(JCoDestination destination, int jobCount,
			int threadCount) {
		System.out.println(">>>启动");
		for (int i = 0; i < jobCount; i++) {// 5*2=10 个任务（一半是状态调用，一半是无状态调用）
			
			// 添加RFM无状态调用任务
			queue.add(new StatelessMultiStepExample(destination, 10/*
																	 * 
																	 * 每个任务需要调用10次
																	 * 
																	 * Z_INCREMENT_COUNTER
																	 * 
																	 * 后，任务才算完成
																	 */));
			// 添加RFM有状态调用任务
			queue.add(new StatefulMultiStepExample(destination, 10));
		}
		CountDownLatch startSignal = new CountDownLatch(threadCount);
		CountDownLatch doneSignal = new CountDownLatch(threadCount);
		
		for (int i = 0; i < threadCount; i++) {
			// 2 个工作线程，共同来完成10 个任务
			new WorkerThread(startSignal, doneSignal).start();// 创建并启动工作线程
		}

		System.out.println(">>>等待执行任务... ");

		try {
			doneSignal.await();// 主线程等待所有工作任务线程完成后，才结束
		} catch (InterruptedException ie) {
			ie.printStackTrace();
		}
		System.out.println(">>>完成");
	}

	public static void main(String[] argv) {
		// JCo.setTrace(5, ".");
		Environment.registerSessionReferenceProvider(new MySessionReferenceProvider());
		try {
			JCoDestination destination = JCoDestinationManager.getDestination(ABAP_AS);
			
			// 远程函数模板
			incrementCounterTemplate = destination.getRepository()
					// 增加计数：即RFM中的count全局变量加一
					.getFunctionTemplate("Z_INCREMENT_COUNTER");
			getCounterTemplate = destination.getRepository()
					// 读取计数：RFM中的count全局变量
					.getFunctionTemplate("Z_GET_COUNTER");

			if (incrementCounterTemplate == null || getCounterTemplate == null) {
				throw new RuntimeException(
						"This example cannot run without Z_INCREMENT_COUNTER and Z_GET_COUNTER functions");
			}

			// 2 个工作线程，5*2=10 个任务（一半是状态调用，一半是无状态调用）
			runJobs(destination, 5, 2);
		} catch (JCoException je) {
			je.printStackTrace();
		}
	}

	// 连接属性配置文件名，名称可以随便取
	static String ABAP_AS = "ABAP_AS_WITH_POOL";
	static {
		Properties connectProperties = new Properties();
		connectProperties.setProperty(DestinationDataProvider.JCO_ASHOST, "dlces1007");
		connectProperties.setProperty(DestinationDataProvider.JCO_SYSNR, "07");
		connectProperties.setProperty(DestinationDataProvider.JCO_CLIENT, "800");
		connectProperties.setProperty(DestinationDataProvider.JCO_USER, "DL_YULIU");

		// 注：密码是区分大小写的，要注意大小写
		connectProperties.setProperty(DestinationDataProvider.JCO_PASSWD, "Pi3.1415926");
		connectProperties.setProperty(DestinationDataProvider.JCO_LANG, "en");
		
		// ****使用连接池的方式
		connectProperties.setProperty(DestinationDataProvider.JCO_PEAK_LIMIT, "2");
		connectProperties.setProperty(DestinationDataProvider.JCO_POOL_CAPACITY, "3");

		// 需要将属性配置保存属性文件，该文件的文件名为 ABAP_AS_WITHOUT_POOL.jcoDestination，
		// JCoDestinationManager.getDestination()调用时会需要该连接配置文件，后缀名需要为jcoDestination
		createDataFile(ABAP_AS, "jcoDestination", connectProperties);
	}

	// 基于上面设定的属性生成连接属性配置文件
	static void createDataFile(String name, String suffix, Properties properties) {
		File cfg = new File("/" + name + "." + suffix);
		if (!cfg.exists()) {
			try {
				FileOutputStream fos = new FileOutputStream(cfg, false);
				properties.store(fos, "for tests only !");
				fos.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
}
