/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package parallelsort;

import java.io.*;

/**
 *
 * @author Don
 */
public class HashSplit extends Thread {
    BufferedReader in;
    PrintStream out[];

    HashSplit( BufferedReader in, PrintStream out0,
            PrintStream out1, PrintStream out2, PrintStream out3 ) {
        this.in = in;
        this.out = new PrintStream[] { out0, out1, out2, out3 };
    }

     public void run() {
        try {
            String input;
            while (true) {
                input = in.readLine();
                if (input == null) {
                    break;
                }
                int hash = myhash(input);
                out[hash].println(input);
                // System.out.println( hash + " " + input );
                out[hash].flush();
            }
            for (int i = 0; i<4; i++) { out[i].flush(); out[i].close(); }
        } catch (IOException e) {
            ReportError.msg(this.getClass().getName() + e);
        }
    }

    int myhash(String s) {
        return (Math.abs(s.hashCode()) % 4);
    }

}
