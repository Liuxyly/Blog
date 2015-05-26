package com.test;

import com.sap.mw.jco.JCO;
import com.sap.mw.jco.JCO.Client;
import com.sap.mw.jco.JCO.Repository;

public class JCoClient {

	public static void main(String [] arg){
		Client client = JCO.createClient("800", "DL_YULIU", "Pi3.1415926", "JA", "10.254.161.194", "00");
		client.connect();
		Repository rep = new JCO.Repository("", client);
		
		new Service("10.254.161.194", "nw7", "ZRFC_CON_001",rep).start();
		System.out.println("Serviceis started.");
	}

}
