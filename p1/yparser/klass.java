/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package yparser;

import java.util.HashMap;

/**
 *
 * @author dsb
 */
public class klass {

    static final String comma = ",";
    static final String quote = "'";
    // variables
    public String id;
    public String name;
    public String fields;
    public String methods;

    public klass(String[] array) {
        this(array[0], array[1], array[2]);
    }

    public klass(String n, String f, String m) {
        id = "c" + Main.kounter++;
        name = n;
        methods = m;
        fields = f;
        klass k = Main.klassS.get(n);
        if (k != null) {
            id = k.id;
            Main.kounter--;
            if (f.equals("")) {
                fields = k.fields;
            }
            if (m.equals("")) {
                methods = k.methods;
            }
        }
        Main.klassS.put(n, this);
    }

    public static void dump() {
        boolean dynamic = true;
        Main.out.println("table(yumlClass,[id,\"name\",\"fields\",\"methods\"]).");
        for (klass i : Main.klassS.values()) {
            if (!i.name.startsWith("<<Interface>>;")) {
                Main.out.println("yumlClass(" + i.id + comma + quote + i.name + quote + comma
                        + quote + i.fields + quote + comma + quote + i.methods + quote + ").");
                dynamic = false;  // we've output one tuple
            }
        }
        if (dynamic)
            Main.out.println(":- dynamic yumlClass/4.");
        Main.out.println();
        
        dynamic = true;
        Main.out.println("table(yumlInterface,[id,\"name\",\"methods\"]).");
        for (klass i : Main.klassS.values()) {
            if (i.name.startsWith("<<Interface>>;")) {
                String iname = i.name.replace(" ","").replace("<<Interface>>;","");
                Main.out.println("yumlInterface(" + i.id + comma + quote + iname + quote + comma
                        + quote + i.methods + quote + ").");
                dynamic = false;  // we've output one tuple
            }
        }
        if (dynamic)
            Main.out.println(":- dynamic yumlInterface/3.");
        Main.out.println();
    }

    public static String toId(String name) {
        klass k = Main.klassS.get(name);
        if (k == null) {
            Main.err("unknown class " + name);
            return "c-1"; // something that will make Prolog complain
        } else {
            return k.id;
        }
    }
}
