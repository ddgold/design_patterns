/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package parallelsort;

/**
 *
 * @author Don
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        test3();
    }

    public static void test1() {
        // read --> sort --> print
        System.out.println("Starting Test1{)");
        Connector read_sort = new Connector();
        Connector sort_print = new Connector();
        Reader r = new Reader("words.txt", read_sort.out);
        Sorter s = new Sorter(read_sort.in, sort_print.out);
        Printer p = new Printer(sort_print.in);
        r.start();
        s.start();
        p.start();
        System.out.println("-----------------");
    }

    public static void test2(){
        // read-->hash--0-->print
        //            --1-->ground
        //            --2-->ground
        //            --3-->ground
        System.out.println("Starting Test2{)");
        Connector read_hash = new Connector();
        Connector hash_print = new Connector();
        Connector hash_null1 = new Connector();
        Connector hash_null2 = new Connector();
        Connector hash_null3 = new Connector();
        Reader r = new Reader("words1.txt", read_hash.out);
        HashSplit h = new HashSplit(read_hash.in, hash_print.out, hash_null1.out,
                  hash_null2.out, hash_null3.out);
        Printer p = new Printer( hash_print.in);
        Ground  g1 = new Ground( hash_null1.in);
        Ground  g2 = new Ground( hash_null2.in);
        Ground  g3 = new Ground( hash_null3.in);
        r.start();
        h.start();
        p.start();
        g1.start();
        g2.start();
        g3.start();
        System.out.println("-----------------");
    }

    static void test3() {
        // read-->hash--0-->sort0------merge-->print
        //            --1-->sort1-----|
        //            --2-->sort2-----|
        //            --3-->sort3-----|
        System.out.println("Starting Test3");
        Connector read_hash = new Connector();
        Connector hash_sort0 = new Connector();
        Connector hash_sort1 = new Connector();
        Connector hash_sort2 = new Connector();
        Connector hash_sort3 = new Connector();
        Connector sort0_merge = new Connector();
        Connector sort1_merge = new Connector();
        Connector sort2_merge = new Connector();
        Connector sort3_merge = new Connector();
        Connector merge_print = new Connector();

        Reader r = new Reader("words.txt", read_hash.out);
        // Printer p = new Printer(read_hash.in);
        HashSplit h = new HashSplit(read_hash.in, hash_sort0.out, hash_sort1.out,
                hash_sort2.out, hash_sort3.out);
        Sorter s0 = new Sorter( hash_sort0.in, sort0_merge.out);
        Sorter s1 = new Sorter( hash_sort1.in, sort1_merge.out);
        Sorter s2 = new Sorter( hash_sort2.in, sort2_merge.out);
        Sorter s3 = new Sorter( hash_sort3.in, sort3_merge.out);
        Merger m = new Merger( sort0_merge.in, sort1_merge.in,
                sort2_merge.in, sort3_merge.in, merge_print.out);
        Printer p = new Printer( merge_print.in);
        r.start();
        h.start();
        s0.start();
        s1.start();
        s2.start();
        s3.start();
        m.start();
        p.start();
    }
}
