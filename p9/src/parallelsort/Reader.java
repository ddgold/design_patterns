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
public class Reader extends Thread {

    BufferedReader in;
    PrintStream out;

    public Reader(String fileName, PrintStream out) {
        try {
            in = new BufferedReader(new InputStreamReader(new FileInputStream(fileName)));
        } catch (Exception e) {
            ReportError.msg(e.getMessage());
        }
        this.out = out;
    }

    public void run() {
        try {
            String input;
            while (true) {
                input = in.readLine();
                if (input == null) {
                    break;
                }
                out.println(input);
                out.flush();
            }
            out.flush();
            out.close();
        } catch (IOException e) {
            ReportError.msg(this.getClass().getName() + e);
        }
    }
}
