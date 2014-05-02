/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package yparser;

import java.util.LinkedList;

/**
 *
 * @author dsb
 */
public class connection {
    public final static String quote="'";
    public final static String comma=",";
    
    public String name1, role1, end1;
    public String name2, role2, end2;
    public connection(String n1, String r1, String e1, String n2, String r2, String e2) {
        name1=n1;
        role1=r1;
        end1 =e1;
        name2=n2;
        role2=r2;
        end2 =e2;
        Main.connectionS.add(this);
    }
    
    public static void dump() {
        boolean dynamic=true;
        Main.out.println("table(yumlAssociation,[\"name1\",\"role1\",end1,\"name2\",\"role2\",end2]).");
        for( connection c : Main.connectionS) {
            String name1 = c.name1.replace("<<Interface>>;", "");
            String name2 = c.name2.replace("<<Interface>>;", "");
            Main.out.println("yumlAssociation("+ 
                    quote + name1 + quote + comma + quote + c.role1 + quote + comma + c.end1 + comma +
                    quote + name2 + quote + comma + quote + c.role2 + quote + comma + c.end2 +  ").");
            dynamic=false;  // we've output one tuple
        }
        if (dynamic)
            Main.out.println(":- dynamic yumlAssociation/6.");
        Main.out.println();
    }
}
