package com.test;

import com.sap.mw.jco.JCO;
import com.sap.mw.jco.JCO.Server;

public class SAPServer extends Server {
	public SAPServer(JCO.Repository repo){
		super ("dlces1007", "sapgw00", "ABC", repo);
	}
}
