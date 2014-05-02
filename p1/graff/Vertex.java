package graff;

import java.util.LinkedList;

import java.lang.Integer;

// of graph
   
  // ***********************************************************************
   
public class Vertex {
    public LinkedList neighbors;
      
    public String name;
    public boolean visited;
    public int VertexCycle;
    public int VertexColor;
    public int VertexNumber;
 
    public Vertex() {
        VertexConstructor();
    }

    public void VertexConstructor() {
        name      = null;
        neighbors = new LinkedList();
        visited = false;
    }
      
    public void addNeighbor( Neighbor n ) {
        neighbors.add( n );
    }

    public  Vertex assignName( String name ) {
        this.name = name;
        return ( Vertex ) this;
    }
   
    public void dftNodeSearch( WorkSpace w ) {
        int           s, c;
        Vertex v;
        Neighbor n;

        // Step 1: Do preVisitAction. 
        //            If we've already visited this node return

        w.preVisitAction( ( Vertex ) this );
         
        if ( visited )
            return;

        // Step 2: else remember that we've visited and 
        //         visit all neighbors

        visited = true;
         
        s = neighbors.size();
        for ( c = 0; c < s; c++ ) 
                {
            n = ( Neighbor ) neighbors.get( c );
            v = n.end;
            w.checkNeighborAction( ( Vertex ) this, v );
            v.dftNodeSearch( w );
        }
        ;
     
        // Step 3: do postVisitAction now
        w.postVisitAction( ( Vertex ) this );
    }



    public void display() {
        System.out.print( " # "+ VertexNumber + " " );
        System.out.print( " VertexCycle# " + VertexCycle + " " );
        if ( visited )
            System.out.print( "  visited" );
        else
            System.out.println( " !visited" );
       int s = neighbors.size();
        int i;

        System.out.print( " Node " + name + " connected to: " );

        for ( i=0; i<s; i++ ) 
         {
            Neighbor theNeighbor = ( Neighbor ) neighbors.get( i );
            Vertex v = theNeighbor.end;
            System.out.print( v.name + ", " );
        } // white ->0, gray ->1, black->2
        System.out.println();
    }
 
    public void init_vertex( WorkSpace w ) {
        visited = false;
        w.init_vertex( ( Vertex ) this );
    }
}
