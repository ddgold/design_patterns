package graff;

import java.util.LinkedList;

// *************************************************************************
   
public class Edge extends Neighbor {
    public  Vertex start;
    public int weight;

        
    public void EdgeConstructor(  Vertex the_start, 
                      Vertex the_end ){ EdgeConstructor( the_start, the_end, 0 ); }
        
        
    public void EdgeConstructor( Vertex the_start,  Vertex the_end, int the_weight ) {
	start = the_start;
        end = the_end;
        weight = the_weight;
    }
        
    // Constructor Loophole removed
    // public void EdgeConstructor($TEqn.Vertex the_start, 
    //                    $TEqn.Vertex the_end) {
    // Super($TEqn.Vertex, $TEqn.Vertex).EdgeConstructor(the_start,the_end);
    // }
                 
    public void adjustAdorns( Edge the_edge ) {
        weight = the_edge.weight;
    }
        
    public void display() {
        System.out.print( " Weight=" + weight );
        System.out.println( " start=" + start.name + " end=" + end.name );
    }
}
