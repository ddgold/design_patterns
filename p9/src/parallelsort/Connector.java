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
public class Connector {
    public BufferedReader in;
    public PrintStream out;

    Connector( ) {
         try {
            PipedOutputStream writeEnd = new PipedOutputStream();
            PipedInputStream  readEnd =  new PipedInputStream(writeEnd);
            out = new PrintStream(writeEnd);
            in  =  new BufferedReader(new InputStreamReader(readEnd));
         }
         catch (Exception e ) { ReportError.msg(this.getClass().getName() + e); }
    }

}
