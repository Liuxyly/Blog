package com.test;

import com.sap.mw.jco.IFunctionTemplate;
import com.sap.mw.jco.JCO;

public class SAPLogon {
	public JCO.Client mConnection;
	public JCO.Repository mRepository;
	public SAPLogon(String client, String userid, String password, String language,
		   String ip, String system_number){
		try {
			mConnection = JCO.createClient(client,
					userid,
					password,
					language,
					ip,
					system_number);
			mConnection.connect();
			mRepository = new JCO.Repository("Lee", mConnection);
			System.out.println("SAP连接成功");
			//调用RFC函数
			IFunctionTemplate ft = mRepository.getFunctionTemplate("BAPI_MATERIAL_GET_DETAIL");
			JCO.Function f = ft.getFunction();
			//1.传入参数为Field
			f.getImportParameterList().setValue("000000000000001924", "MATERIAL");
			f.getImportParameterList().setValue("1000", "PLANT");
			mConnection.execute(f);
			//1.返回参数为Field
			JCO.Structure struct = f.getExportParameterList().getStructure("MATERIAL_GENERAL_DATA");
			String name = struct.getString("MATL_DESC");
			JCO.Structure __return = f.getExportParameterList().getStructure("RETURN");
			String message = __return.getString("MESSAGE");
			System.out.println("物料名:"+name);
			String MATL_TYPE = struct.getString("MATL_TYPE");
			System.out.println("物料类型:"+MATL_TYPE);
			System.out.println("Message:" + message);
			mRepository = new JCO.Repository("my_repository", mConnection);
			mConnection.disconnect();
		} catch (Exception ex) {
			ex.printStackTrace();
			System.exit(1);
		}
	}
	public static void main(String [] arg){
		SAPLogon mySAP = new SAPLogon("800", "DL_YULIU", "Pi3.1415926", "ja", "dlces1007", "07");
		SAPServer myServer = new SAPServer(mySAP.mRepository); 
		myServer.start();
	}
}
