package com.test;

import java.util.HashMap;
import java.util.Properties;

import com.sap.conn.jco.AbapException;
import com.sap.conn.jco.JCoDestination;
import com.sap.conn.jco.JCoDestinationManager;
import com.sap.conn.jco.JCoException;
import com.sap.conn.jco.JCoField;
import com.sap.conn.jco.JCoFunction;
import com.sap.conn.jco.JCoStructure;
import com.sap.conn.jco.JCoTable;
import com.sap.conn.jco.ext.DestinationDataEventListener;
import com.sap.conn.jco.ext.DestinationDataProvider;
import com.sap.conn.jco.ext.Environment;

public class CustomSAPDestinationDataProvider {

	static class MyDestinationDataProvider implements DestinationDataProvider {

		@SuppressWarnings("unused")
		private DestinationDataEventListener eL;
		private HashMap<String, Properties> destinations;
		private static MyDestinationDataProvider provider = new MyDestinationDataProvider();

		private MyDestinationDataProvider() {// 单例模式
			if (provider == null) {
				destinations = new HashMap<String, Properties>();
			}
		}

		public static MyDestinationDataProvider getInstance() {
			return provider;
		}

		// 实现接口：获取连接配置属性
		public Properties getDestinationProperties(String destinationName) {
			if (destinations.containsKey(destinationName)) {
				return destinations.get(destinationName);
			} else {
				throw new RuntimeException("Destination " + destinationName
						+ " is not available");
			}
		}

		public void setDestinationDataEventListener(
				DestinationDataEventListener eventListener) {
			this.eL = eventListener;
		}

		public boolean supportsEvents() {
			return true;
		}

		/**
		 * 
		 * Add new destination 添加连接配置属性
		 * 
		 * @param properties
		 * 
		 *            holds all the required data for a destination
		 **/

		void addDestination(String destinationName, Properties properties) {
			synchronized (destinations) {
				destinations.put(destinationName, properties);
			}
		}
	}

	public static void accessSAPStructure() throws JCoException {
		JCoDestination destination = JCoDestinationManager
				.getDestination("ABAP_AS");
		JCoFunction function = destination.getRepository().getFunction(
				"RFC_SYSTEM_INFO");// 从对象仓库中获取 RFM 函数
		if (function == null)
			throw new RuntimeException("RFC_SYSTEM_INFO not found in SAP.");
		try {
			function.execute(destination);
		} catch (AbapException e) {
			System.out.println(e.toString());
			return;
		}

		JCoStructure exportStructure = function.getExportParameterList()
				.getStructure("RFCSI_EXPORT");
		System.out.println("System info for "
				+ destination.getAttributes().getSystemID() + ":\n");
		for (int i = 0; i < exportStructure.getMetaData().getFieldCount(); i++) {
			System.out.println(exportStructure.getMetaData().getName(i) + ":\t"
					+ exportStructure.getString(i));
		}

		// JCo still supports the JCoFields, but direct access via getXX is more
		// efficient as field iterator 也可以使用下面的方式来遍历
		System.out.println("The same using field iterator: \nSystem info for "
				+ destination.getAttributes().getSystemID() + ":\n");

		for (JCoField field : exportStructure) {
			System.out.println(field.getName() + ":\t" + field.getString());
		}

		// *********也可直接通过结构中的字段名或字段所在的索引位置来读取某个字段的值
		System.out.println("RFCPROTO:\t" + exportStructure.getString(0));
		System.out.println("RFCPROTO:\t"
				+ exportStructure.getString("RFCPROTO"));
	}
	public static void workWithTable() throws JCoException {
		   JCoDestination destination = JCoDestinationManager.getDestination("ABAP_AS");
		   //从对象仓库中获取 RFM 函数：获取公司列表
		   JCoFunction function = destination.getRepository().getFunction("BAPI_COMPANYCODE_GETLIST");
		   if (function == null)
			   throw new RuntimeException("BAPI_COMPANYCODE_GETLIST not found in SAP.");
		   try {
			   function.execute(destination);
		   } catch (AbapException e) {
		       System.out.println(e.toString());
		   return ;
		   }

		   JCoStructure Structure = function.getExportParameterList().getStructure("RETURN");

		   //判断读取是否成功
		   if (!(Structure.getString("TYPE").equals("") || Structure.getString("TYPE").equals("S"))) {
			   throw new RuntimeException(Structure.getString("MESSAGE"));
		   }

		   //获取Table参数：COMPANYCODE_LIST
		   JCoTable codes = function.getTableParameterList().getTable("COMPANYCODE_LIST");
		   for (int i = 0; i < codes.getNumRows(); i++) {//遍历Table
		       codes.setRow(i);//将行指针指向特定的索引行
		       System.out.println(codes.getString("COMP_CODE") + '\t' + codes.getString("COMP_NAME"));
		   }

		   // move the table cursor to first row
		   codes.firstRow();//从首行开始重新遍历 codes.nextRow()：如果有下一行，下移一行并返回True
		   for (int i = 0; i < codes.getNumRows(); i++, codes.nextRow()) {
		   //进一步获取公司详细信息
		       function = destination.getRepository().getFunction("BAPI_COMPANYCODE_GETDETAIL");
		       if (function == null)
		    	   throw new RuntimeException("BAPI_COMPANYCODE_GETDETAIL not found in SAP.");

		       function.getImportParameterList().setValue("COMPANYCODEID", codes.getString("COMP_CODE"));

		   // We do not need the addresses, so set the corresponding parameter
		   // to inactive.
		   // Inactive parameters will be either not generated or at least
		   // converted. 不需要返回COMPANYCODE_ADDRESS参数（但服务器端应该还是组织了此数据，只是未经过网络传送？）
		       function.getExportParameterList().setActive("COMPANYCODE_ADDRESS", false);

		       try {
		    	   function.execute(destination);
		       } catch (AbapException e) {
		    	   System.out.println(e.toString());
		    	   return ;
		       }

		       Structure = function.getExportParameterList().getStructure("RETURN");
		       if (!(Structure.getString("TYPE").equals("")
		          || Structure.getString("TYPE").equals("S") || Structure
		          .getString("TYPE").equals("W"))) {
		    	   throw new RuntimeException(Structure.getString("MESSAGE"));
		       }

		       JCoStructure detail = function.getExportParameterList().getStructure("COMPANYCODE_DETAIL");
		       System.out.println(detail.getString("COMP_CODE") + '\t'
		          + detail.getString("COUNTRY") + '\t'
		          + detail.getString("CITY"));
		   }
		}

	public static void main(String[] args) throws Exception {
		// 获取单例
		// MyDestinationDataProvider myProvider =
		// MyDestinationDataProvider.getInstance();
		// Register the MyDestinationDataProvider 环境注册
		Environment.registerDestinationDataProvider(MyDestinationDataProvider
				.getInstance());

		// TEST 01：直接测试
		// ABAP_AS is the test destination name ：ABAP_AS为目标连接属性名（只是逻辑上的命名）
		String destinationName = "ABAP_AS";
		System.out.println("Test destination - " + destinationName);
		Properties connectProperties = new Properties();
		connectProperties.setProperty(DestinationDataProvider.JCO_ASHOST,
				"dlces1007");
		connectProperties.setProperty(DestinationDataProvider.JCO_SYSNR, "07");
		connectProperties
				.setProperty(DestinationDataProvider.JCO_CLIENT, "800");
		connectProperties.setProperty(DestinationDataProvider.JCO_USER,
				"DL_YULIU");
		connectProperties.setProperty(DestinationDataProvider.JCO_PASSWD,
				"Pi3.1415926");
		connectProperties.setProperty(DestinationDataProvider.JCO_LANG, "en");

		// Add a destination
		MyDestinationDataProvider.getInstance().addDestination(destinationName,
				connectProperties);

		// Get a destination with the name of "ABAP_AS"
		JCoDestination DES_ABAP_AS = JCoDestinationManager
				.getDestination(destinationName);

		// Test the destination with the name of "ABAP_AS"
		try {
			DES_ABAP_AS.ping();
			System.out.println("Destination - " + destinationName + " is ok");
		} catch (Exception ex) {
			ex.printStackTrace();
			System.out.println("Destination - " + destinationName
					+ " is invalid");
		}

		// TEST 02：连接池测试
		// Add another destination to test
		// ABAP_AS2 is the test destination name
		String destinationName2 = "ABAP_AS2";
		System.out.println("Test destination - " + destinationName2);
		Properties connectProperties2 = new Properties();
		connectProperties2.setProperty(DestinationDataProvider.JCO_ASHOST,
				"dlces1007");
		connectProperties2.setProperty(DestinationDataProvider.JCO_SYSNR, "07");
		connectProperties2.setProperty(DestinationDataProvider.JCO_CLIENT,
				"800");
		connectProperties2.setProperty(DestinationDataProvider.JCO_USER,
				"DL_YULIU");
		connectProperties2.setProperty(DestinationDataProvider.JCO_PASSWD,
				"Pi3.1415926");
		connectProperties2.setProperty(DestinationDataProvider.JCO_LANG, "en");

		connectProperties2.setProperty(DestinationDataProvider.JCO_PEAK_LIMIT,
				"10");
		connectProperties2.setProperty(
				DestinationDataProvider.JCO_POOL_CAPACITY, "3");

		// Add a destination
		MyDestinationDataProvider.getInstance().addDestination(
				destinationName2, connectProperties2);

		// Get a destination with the name of "ABAP_AS2"
		JCoDestination DES_ABAP_AS2 = JCoDestinationManager
				.getDestination(destinationName2);

		// Test the destination with the name of "ABAP_AS2"
		try {
			DES_ABAP_AS2.ping();
			System.out.println("Destination - " + destinationName2 + " is ok");
		} catch (Exception ex) {
			ex.printStackTrace();
			System.out.println("Destination - " + destinationName2
					+ " is invalid");
		}
		
		//accessSAPStructure();
		workWithTable();
	}
}
