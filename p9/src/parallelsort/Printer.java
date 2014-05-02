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
public class Printer extends Thread {
    BufferedReader in;

    public Printer( BufferedReader in ) {
        this.in = in;
    }

    public void run() {
        try {
            String input;
            while (true) {
                input = in.readLine();
                if (input == null) {
                    in.close();
                    break;
                }
                System.out.println(input);
            }
            System.out.flush();
        } catch (IOException e) {
            ReportError.msg(this.getClass().getName() + e);
        }
    }

}
