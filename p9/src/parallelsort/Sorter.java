/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package parallelsort;

import java.io.*;
import java.util.*;

/**
 *
 * @author Don
 */
public class Sorter extends Thread {

    BufferedReader in;
    PrintStream out;
    List<String> listOfWords;

    public Sorter(BufferedReader in, PrintStream out) {
        this.in = in;
        this.out = out;
        listOfWords = new LinkedList<String>();
        if (out == null || in == null) {
            ReportError.msg("Sort out or in is null");
        }
    }

    public void run() {
        try {
            String input;
            while (true) {
                input = in.readLine();
                if (input == null) {
                    break;
                }
                listOfWords.add(input);
                out.flush();
            }
            Collections.sort(listOfWords);
            for (String i : listOfWords) {
                out.println(i);
            }
            out.flush();
            out.close();
        } catch (IOException e) {
            ReportError.msg(this.getClass().getName()
                    + " WriteReversedThread run: " + e);
        }
    }

    void finish() {
    }
}
