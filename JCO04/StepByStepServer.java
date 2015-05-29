package com.test;

import java.io.File;
import java.io.FileOutputStream;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;

import com.sap.conn.jco.JCo;
import com.sap.conn.jco.JCoCustomRepository;
import com.sap.conn.jco.JCoDestinationManager;
import com.sap.conn.jco.JCoException;
import com.sap.conn.jco.JCoFunction;
import com.sap.conn.jco.JCoFunctionTemplate;
import com.sap.conn.jco.JCoListMetaData;
import com.sap.conn.jco.JCoMetaData;
import com.sap.conn.jco.ext.DestinationDataProvider;
import com.sap.conn.jco.ext.ServerDataProvider;
import com.sap.conn.jco.server.DefaultServerHandlerFactory;
import com.sap.conn.jco.server.JCoServer;
import com.sap.conn.jco.server.JCoServerContext;
import com.sap.conn.jco.server.JCoServerContextInfo;
import com.sap.conn.jco.server.JCoServerErrorListener;
import com.sap.conn.jco.server.JCoServerExceptionListener;
import com.sap.conn.jco.server.JCoServerFactory;
import com.sap.conn.jco.server.JCoServerFunctionHandler;
import com.sap.conn.jco.server.JCoServerState;
import com.sap.conn.jco.server.JCoServerStateChangedListener;
import com.sap.conn.jco.server.JCoServerTIDHandler;

public class StepByStepServer {
   static String SERVER_NAME1 = "SERVER";
   static String DESTINATION_NAME1 = "ABAP_AS_WITHOUT_POOL";
   static String DESTINATION_NAME2 = "ABAP_AS_WITH_POOL";
   static MyTIDHandler myTIDHandler = null;
   static {
      JCo.setTrace(4, null);// 打开调试
      Properties connectProperties = new Properties();
      
      // ******直连
      connectProperties.setProperty(DestinationDataProvider.JCO_ASHOST, "dlces1007");
      connectProperties.setProperty(DestinationDataProvider.JCO_SYSNR, "07");
      connectProperties.setProperty(DestinationDataProvider.JCO_CLIENT, "800");
      connectProperties.setProperty(DestinationDataProvider.JCO_USER, "DL_YULIU");
      connectProperties.setProperty(DestinationDataProvider.JCO_PASSWD, "Pi3.1415926");
      connectProperties.setProperty(DestinationDataProvider.JCO_LANG, "en");
      createDataFile(DESTINATION_NAME1, "jcoDestination", connectProperties);
      
      // ******连接池
      connectProperties.setProperty(DestinationDataProvider.JCO_POOL_CAPACITY, "3");
      connectProperties.setProperty(DestinationDataProvider.JCO_PEAK_LIMIT, "2");
      createDataFile(DESTINATION_NAME2, "jcoDestination", connectProperties);
      
      // ******JCo sever
      Properties servertProperties = new Properties();
      servertProperties.setProperty(ServerDataProvider.JCO_GWHOST, "dlces1007");
      
      // TCP服务sapgw是固定的，后面的00就是SAP实例系统编号，也可直接是端口号（端口号可以在
      // etc/server文件中找sapgw00所对应的端口号）
      servertProperties.setProperty(ServerDataProvider.JCO_GWSERV, "sapgw07");
      
      // 这里的程序ID来自于SM59中设置的Program ID，必须相同
      servertProperties.setProperty(ServerDataProvider.JCO_PROGID, "ZRFC_CON_001");
      servertProperties.setProperty(ServerDataProvider.JCO_REP_DEST, DESTINATION_NAME2);
      servertProperties.setProperty(ServerDataProvider.JCO_CONNECTION_COUNT, "2");
      
      createDataFile(SERVER_NAME1, "jcoServer", servertProperties);
   }
   static void createDataFile(String name, String suffix, Properties properties) {
      File cfg = new File(name + "." + suffix);
      if (!cfg.exists()) {
         try {
            FileOutputStream fos = new FileOutputStream(cfg, false);
            properties.store(fos, "for tests only !");
            fos.close();
         } catch (Exception e) {
            throw new RuntimeException(
                   "Unable to create the destination file "
                         + cfg.getName(), e);
         }
      }
   }
   
   // 处理来自ABAP端的调用请求，实现注册过的虚拟函数真正功能
   static class StfcConnectionHandler implements JCoServerFunctionHandler {
      public void handleRequest(JCoServerContext serverCtx,
    	// 处理远程调用请求
        JCoFunction function) {
    	  
         System.out.println("----------------------------------------------------------------");
         System.out.println("call              : " + function.getName());// ABAP调用的是哪个函数
         System.out.println("ConnectionId      : " + serverCtx.getConnectionID());
         System.out.println("SessionId         : " + serverCtx.getSessionID());
         System.out.println("TID               : " + serverCtx.getTID());
         System.out.println("repository name   : " + serverCtx.getRepository().getName());
         System.out.println("is in transaction : " + serverCtx.isInTransaction());
         System.out.println("is stateful       : " + serverCtx.isStatefulSession());
         System.out.println("----------------------------------------------------------------");
         System.out.println("gwhost: " + serverCtx.getServer().getGatewayHost());
         System.out.println("gwserv: " + serverCtx.getServer().getGatewayService());
         System.out.println("progid: " + serverCtx.getServer().getProgramID());
         System.out.println("----------------------------------------------------------------");
         System.out.println("attributes  : ");
         System.out.println(serverCtx.getConnectionAttributes().toString());
         System.out.println("----------------------------------------------------------------");
         System.out.println("CPIC conversation ID: " + serverCtx.getConnectionAttributes()
        		 .getCPICConversationID());
         System.out.println("----------------------------------------------------------------");
         System.out.println("req text: " + function.getImportParameterList().getString("REQUTEXT"));
         // function.getExportParameterList().setValue("ECHOTEXT",
         // function.getImportParameterList().getString("REQUTEXT"));
         // function.getExportParameterList().setValue("RESPTEXT",
         // "Java服务端响应的消息");
      }
   }
   
   static class MyThrowableListener implements JCoServerErrorListener,
	JCoServerExceptionListener {// 服务异常监听器
	   
	public void serverErrorOccurred(JCoServer jcoServer,
			String connectionId, JCoServerContextInfo serverCtx, Error error) {
		    System.out.println(">>> Error occured on "
		    		+ jcoServer.getProgramID() + " connection " + connectionId);
    	  	error.printStackTrace();
      }
	
      public void serverExceptionOccurred(JCoServer jcoServer,
            String connectionId, JCoServerContextInfo serverCtx,
            Exception error) {
         System.out.println(">>> Error occured on "
                + jcoServer.getProgramID() + " connection " + connectionId);
         error.printStackTrace();
      }
   }
   
   static class MyStateChangedListener implements
         JCoServerStateChangedListener {// 服务状态改变监听器
	   
      public void serverStateChangeOccurred(JCoServer server,
    		  JCoServerState oldState, JCoServerState newState) {
    	  
         // Defined states are: STARTED启动, DEAD死, ALIVE活, STOPPED停止;
         // see JCoServerState class for details.
         // Details for connections managed by a server instance
         // are available via JCoServerMonitor.
         System.out.println("Server state changed from "
                + oldState.toString() + " to " + newState.toString()
                + " on server with program id " + server.getProgramID());
      }
   }
   
   // 简单调用：提供的函数需要在ABAP签名
   static void simpleServer() {
	   
      JCoServer server;
      
      try {
         server = JCoServerFactory.getServer(SERVER_NAME1);
      } catch (JCoException ex) {
         throw new RuntimeException("Unable to create the server "
                + SERVER_NAME1 + " because of " + ex.getMessage(), ex);
      }
      JCoServerFunctionHandler stfcConnectionHandler = new StfcConnectionHandler();
      DefaultServerHandlerFactory.FunctionHandlerFactory factory =
    		  new DefaultServerHandlerFactory.FunctionHandlerFactory();
      
      // 向SAP服务器注册可提供的函数有哪些，告诉SAP系统，Java这边可以提供STFC_CONNECTION这样一个远程函数，但具体的功能由StfcConnectionHandler来完成
      // 注：因该可以注册多个这样的虚拟函数，都由 JCoServerFunctionHandler
      // 的实现类来处理，在处理时可以由JCoFunction参数来判断具体是哪个函数，走不同的处理逻辑
      // 注：STFC_CONNECTION需要先在SAP端定义（但不需要在ABAP中实现），否则需要在Java端动态创建函数对象仓库（请参考staticRepository方法）
      
      factory.registerHandler("STFC_CONNECTION", stfcConnectionHandler);
      server.setCallHandlerFactory(factory);
      
      // ********* 添加一些连接状态监听处理器，便于在连接过程中定位问题（可以不用设置）
      MyThrowableListener eListener = new MyThrowableListener();// 异常监听，在连接过程中出问题时会被监听到
      server.addServerErrorListener(eListener);
      server.addServerExceptionListener(eListener);
      MyStateChangedListener slistener = new MyStateChangedListener();// 连接状态监听
      server.addServerStateChangedListener(slistener);
      
      server.start();
   }
   
   // 在Java服务端定义远程函数（不需要在ABAP端进行函数的签名定义）
   static void staticRepository() {
	   
      JCoListMetaData impList = JCo.createListMetaData("IMPORT");
      impList.add("REQUTEXT", JCoMetaData.TYPE_CHAR, 100, 50, 0, null, null,
            JCoListMetaData.IMPORT_PARAMETER, null, null);
      impList.lock();// 锁住，不允许再修改
      JCoListMetaData expList = JCo.createListMetaData("EXPORT");
      expList.add("RESPTEXT", JCoMetaData.TYPE_CHAR, 100, 50, 0, null, null,
            JCoListMetaData.EXPORT_PARAMETER, null, null);
      expList.add("ECHOTEXT", JCoMetaData.TYPE_CHAR, 100, 50, 0, null, null,
            JCoListMetaData.EXPORT_PARAMETER, null, null);
      
      expList.lock();
      
      // 注：ZSTFC_CONNECTION函数不必要在ABAP端时行定义了（只定义签名，不需要实现），因为在这里（Java）
      // 进行了动态的函数对象创建的创建与注册，这与上面simpleServer方法示例是不一样的
      JCoFunctionTemplate fT = JCo.createFunctionTemplate("ZSTFC_CONNECTION",
            impList, expList, null, null, null);
      JCoCustomRepository cR = JCo
            .createCustomRepository("MyCustomRepository");
      cR.addFunctionTemplateToCache(fT);
      
      JCoServer server;
      try {
         server = JCoServerFactory.getServer(SERVER_NAME1);
      } catch (JCoException ex) {
         throw new RuntimeException("Unable to create the server "
                + SERVER_NAME1 + " because of " + ex.getMessage(), ex);
      }
      
      String repDest = server.getRepositoryDestination();
      if (repDest != null) {
         try {
            cR.setDestination(JCoDestinationManager.getDestination(repDest));
         } catch (JCoException e) {
            e.printStackTrace();
            System.out
                   .println(">>> repository contains static function definition only");
         }
      }
      server.setRepository(cR);
      JCoServerFunctionHandler requestHandler = new StfcConnectionHandler();
      DefaultServerHandlerFactory.FunctionHandlerFactory factory = new DefaultServerHandlerFactory.FunctionHandlerFactory();
      factory.registerHandler(fT.getName(), requestHandler);
      server.setCallHandlerFactory(factory);
      server.start();
   }
   
   /*
    * 该类用于在ABAP进行事务调用（CALL FUNCTION func IN BACKGROUND TASK DESTINATION dest）
    * 时， Java端需要实时告诉ABAP端目前事务处理的情况（状态），即Java与ABAP之间的事务状态的交流
    */
   static class MyTIDHandler implements JCoServerTIDHandler {
	   
      // 存储事务状态信息
      Map<String, TIDState>availableTIDs = new Hashtable<String, TIDState>();
      
      // 18662702337
      // 当一个事务性RFM从ABAP端进行调用时，会触发此方法
      public boolean checkTID(JCoServerContext serverCtx, String tid) {
    	  
         // This example uses a Hashtable to store status information.
         // Normally, however,
         // you would use a database. If the DB is down throw a
         // RuntimeException at
         // this point. JCo will then abort the tRFC and the R/3 backend will
         // try again later.
         System.out.println("TID Handler: checkTID for " + tid);
         TIDState state = availableTIDs.get(tid);
         if (state == null) {
            availableTIDs.put(tid, TIDState.CREATED);
            return true;
         }
         if (state == TIDState.CREATED || state == TIDState.ROLLED_BACK) {
            return true;
         }
         return false;
         
         // "true" means that JCo will now execute the transaction, "false"
         // means
         // that we have already executed this transaction previously, so JCo
         // will
         // skip the handleRequest() step and will immediately return an OK
         // code to R/3.
      }
      
      // 事件提交时触发
      public void commit(JCoServerContext serverCtx, String tid) {
         System.out.println("TID Handler: commit for " + tid);
         
         // react on commit, e.g. commit on the database;
         // if necessary throw a RuntimeException, if the commit was not
         // possible
         availableTIDs.put(tid, TIDState.COMMITTED);
      }
      
      // 事务回滚时触发
      public void rollback(JCoServerContext serverCtx, String tid) {
         System.out.println("TID Handler: rollback for " + tid);
         availableTIDs.put(tid, TIDState.ROLLED_BACK);
         // react on roll back, e.g. roll back on the database
      }
      public void confirmTID(JCoServerContext serverCtx, String tid) {
         System.out.println("TID Handler: confirmTID for " + tid);
         try {
            // clean up the resources
         }
         // catch(Throwable t) {} //partner（代码ABAP对方） won't react on an
         // exception at
         // this point
         finally {
            availableTIDs.remove(tid);
         }
      }
      private enum TIDState {
         CREATED, COMMITTED, ROLLED_BACK, CONFIRMED;
      }
   }
   
   /**
    * Follow server example demonstrates how to implement the support for tRFC
    * calls, calls executed BACKGROUND TASK. At first we write am
    * implementation for JCoServerTIDHandler interface. This implementation is
    * registered by the server instance and will be used for each call send in
    * "background task". Without such implementation JCo runtime deny any tRFC
    * calls. See java doc for interface JCoServerTIDHandler for details.
    */
   
   // 支持事务性调用，但究竟如果使用，有什么作用不清楚！！！
   static void transactionRFCServer() {
	   
      JCoServer server;
      try {
         server = JCoServerFactory.getServer(SERVER_NAME1);
      } catch (JCoException ex) {
         throw new RuntimeException("Unable to create the server "
                + SERVER_NAME1 + " because of " + ex.getMessage(), ex);
      }
      
      JCoServerFunctionHandler stfcConnectionHandler = new StfcConnectionHandler();
      DefaultServerHandlerFactory.FunctionHandlerFactory factory =
    		  new DefaultServerHandlerFactory.FunctionHandlerFactory();
      factory.registerHandler("STFC_CONNECTION", stfcConnectionHandler);
      server.setCallHandlerFactory(factory);
      
      // ***添加事务处理器
      myTIDHandler = new MyTIDHandler();
      server.setTIDHandler(myTIDHandler);
      
      server.start();
   }
   public static void main(String[] a) {
      //simpleServer();
      staticRepository();
      //transactionRFCServer();
   }
}
