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
public class Ground extends Thread {
BufferedReader in;

    public Ground( BufferedReader in ) {
        this.in = in;
    }

    public void run() {
        try {
            String input;
            while (true) {
                input = in.readLine();
                if (input == null) {
                    break;
                }
                // do nothing
            }
            System.out.flush();
        } catch (IOException e) {
            ReportError.msg(this.getClass().getName() + e);
        }
    }
}
