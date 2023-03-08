package com.test;

import com.sap.mw.jco.IRepository;
import com.sap.mw.jco.JCO;
import com.sap.mw.jco.JCO.Client;
import com.sap.mw.jco.JCO.Function;
import com.sap.mw.jco.JCO.ParameterList;
import com.sap.mw.jco.JCO.Repository;
import com.sap.mw.jco.JCO.Server;

public class Service extends Server {
	private static Client client;
	
	public Service(String gwhost, String gwserv, String progid, IRepository rep){
		super(gwhost, gwserv, progid, rep);
		
		super.setProperty("unicode", "1");
	    
		if (client == null) {
			client = JCO.createClient("800", "DL_YULIU", "xxxxxx", "JA", "xxxxxx", "00");
			client.connect();
		}
	}
	
	@Override
	protected void handleRequest(Function function) throws Exception{
		ParameterList input = function.getImportParameterList();
		ParameterList output = function.getExportParameterList();
		String number1, number2, operator, result;
		number1 = input.getString("NUMBER1");
		number2 = input.getString("NUMBER2");
		operator = input.getString("OPERATOR");
		
		if (function.getName().equals("ZRFC_CALCULATE_JAVA")) {
			result = getResult(number1, number2, operator);
			System.out.println("Calculating=>  " + number1 + operator + number2 + " = " + result);
			
			output.setValue(result, "RESULT");
		}
	}

	private String getResult(String number1, String number2, String operator) {
		Repository rep = new Repository("", client);
		Function func = rep.getFunctionTemplate("ZRFC_CALCULATE").getFunction();
		ParameterList input = func.getImportParameterList();
		input.setValue(number1, "NUMBER1");
		input.setValue(number2, "NUMBER2");
		input.setValue(operator, "OPERATOR");
		
		func.setImportParameterList(input);
		client.execute(func);
		
		return func.getExportParameterList().getString("RESULT");
	}
}
