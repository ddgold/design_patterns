/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package yparser;

import java.io.*;
import java.util.HashMap;
import java.util.LinkedList;

/**
 *
 * @author dsb
 */
public class Main {

    final static String gen = "'^'";
    final static String comp = "'++'";
    final static String agg = "'<>'";
    final static String none = "''";
    final static String arrow1 = "'<'";
    final static String arrow2 = "'>'";
    final static String comma = ",";
    final static String commaApost = ",'";
    final static String apostComma = "',";
    final static String equals = " --> ";
    final static String apost = "'";
    // variables
    static int lineCount = 0;
    static String line, original;
    static String name1, end1, role1;
    static String name2, end2, role2;
    static String[] array1, array2;
    
    // public variables to other classes
    static HashMap<String, klass> klassS;
    static LinkedList<connection> connectionS;
    static int kounter;
    static PrintStream out;

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        int i;
        int lineNum=0;
        
        if (args.length !=1) {
            System.err.println("yumlParse.Main <name-of-yuml-file-without-.yuml>");
            return;
        }
        // initialize static variables
         klassS = new HashMap<String, klass>();
         connectionS = new LinkedList<connection>();
         kounter = 0;
         
        try {
        BufferedReader in = new BufferedReader(new FileReader(args[0]+".yuml"));
        out = new PrintStream(args[0]+".yuml.pl");
        while (true) {
            // foreach line
            name1 = end1 = role1 = name2 = end2 = role2 = "";

            original = line = in.readLine();
            lineNum++;
            if (line == null) {
                break;
            }
            lineCount++;
            line = line.trim();
            if (line.equals("") || line.startsWith("//")) {
                continue;
            }

            // lines must start with [name1]
            if (!line.startsWith("[")) {
                err(lineCount + ": can't parse line.  skipping " + line);
                continue;
            }
            for (i = 1; i < line.length(); i++) {
                if (line.charAt(i) == ']') {
                    break;
                }
            }
            name1 = line.substring(1, i);
            line = line.substring(i + 1);

            // so we have now parsed [name1]
            // lets check to see if it has too many substructures and proceed to create the klass object
            array1 = checkNameStructure(name1);
            new klass(array1);



            if (line.equals("")) {
//                System.out.println(lineCount + ": " + original + equals + name1);
//                if (array1.length > 1) {
//                    printArray("array1 :", array1);
//                }
                continue;
            }



            // the line could end now, or continue with an association
            if (line.startsWith("<>")) {
                end1 = agg;
                line = line.substring(2);
            } else if (line.startsWith("++")) {
                end1 = comp;
                line = line.substring(2);
            } else if (line.startsWith("<")) {
                end1 = arrow1;
                line = line.substring(1);
            } else if (line.startsWith("^")) {
                end1 = gen;
                line = line.substring(1);
            } else {
                end1 = none;
            }
            // end1 contains the arrow ending type
            // now everthing from here to '-' is role1
            for (i = 0; i < line.length(); i++) {
                if (line.charAt(i) == '-') {
                    break;
                }
            }
            role1 = line.substring(0, i);

            // now everything from here up to the next token is 
            // role2
            line = line.substring(i + 1);
            for (i = 0; i < line.length(); i++) {
                if (line.substring(i).startsWith("[")) {
                    role2 = line.substring(0, i);
                    line = line.substring(i);
                    end2 = none;
                    break;
                } else if (line.substring(i).startsWith("++")) {
                    role2 = line.substring(0, i);
                    line = line.substring(i + 2);
                    end2 = comp;
                    break;
                } else if (line.substring(i).startsWith("<>")) {
                    role2 = line.substring(0, i);
                    line = line.substring(i + 2);
                    end2 = agg;
                    break;
                } else if (line.substring(i).startsWith(">")) {
                    role2 = line.substring(0, i);
                    line = line.substring(i + 1);
                    end2 = arrow2;
                    break;
                } else if (line.substring(i).startsWith("^")) {
                    role2 = line.substring(0, i);
                    line = line.substring(i + 1);
                    end2 = gen;
                    break;
                }
            }
            if (!line.startsWith("[")) {
                err("expecting [name] after '" + line + "'");
                continue;
            }
            line = line.substring(1); // trim off '['
            for (i = 0; i < line.length(); i++) {
                if (line.charAt(i) == ']') {
                    break;
                }
            }
            name2 = line.substring(0, i);
            line = line.substring(i + 1).trim();
            if (!line.equals("")) {
                err("skipping contents '" + line + "' on line " + lineCount);
            }

            // lets check to see if it has too many substructures, and proceed to create the klass object
            array2 = checkNameStructure(name2);
            new klass(array2);
            // and now create a connection
            new connection(name1,role1,end1, name2, role2,end2);
            //            System.out.println(lineCount + ": " + original + equals + name1 + comma + end1 + commaApost + role1 + apost
            //                    + commaApost + role2 + apostComma + end2 + comma + name2);
            //            if (array1.length > 1) {
            //                printArray("array1: ", array1);
            //            }
            //            if (array2.length > 1) {
            //                printArray("array2: ", array2);
            //            }
        }
        klass.dump();
        connection.dump();
        }
        catch(Exception e) {
            System.err.println("error in parsing "+args[0] + " at line " + lineNum + 
                    " : "+ e.getMessage());
            
        }
    }

    public static void err(String msg) {
        System.err.println(lineCount + ": " + msg);
    }

    public static String[] checkNameStructure(String name) {

        String[] array = name.replace("|", "@").split("@");
        if (array.length > 3) {
            err("too many substructures in " + name);
        }
        switch (array.length) {
            case 3:
                break;  // all is OK
            case 2:
                String[] result = {array[0], array[1], ""};
                array = result;
                break;  
            case 1:
            default:
                String[] results = {array[0], "", ""};
                array = results;
        }
        return array;
    }

    // for debugging
    public static void printArray(String header, String[] array) {
        System.out.print(header);
        for (String x : array) {
            System.out.print(x + "@");
        }
        System.out.println();
    }
}
