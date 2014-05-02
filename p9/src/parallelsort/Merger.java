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
public class Merger extends Thread {

    static String LAST = "|";
    static String START = "$";
    BufferedReader in[];
    PrintStream out;
    String input[] = new String[]{START, START, START, START};

    Merger(BufferedReader in0, BufferedReader in1,
            BufferedReader in2, BufferedReader in3, PrintStream out) {
        this.in = new BufferedReader[]{in0, in1, in2, in3};
        this.out = out;
    }

    public void run() {
        readInput(0);
        readInput(1);
        readInput(2);
        readInput(3);

        while (true) {
            int smallest = 0;
            for (int i = 1; i < 4; i++) {
                if (input[smallest].compareTo(input[i]) > 0) {
                    smallest = i;
                }
            }
            if (input[smallest].equals(LAST)) {
                break;
            }
            out.println(input[smallest]);
            out.flush();
            readInput(smallest);
        }
        out.flush();
        out.close();
    }

    void readInput(int line) {
        if (input[line].equals(LAST)) {
            return;
        }
        try {
            input[line] = in[line].readLine();
            if (input[line] == null) {
                input[line] = LAST;
            }
        } catch (IOException e) {
            ReportError.msg(this.getClass().getName() + e);
        }
    }
}
