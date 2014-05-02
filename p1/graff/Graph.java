package graff;

import java.util.LinkedList;

import java.lang.Integer;

import java.io.*;

// *************************************************************************
   
public class Graph {
    public static final boolean isDirected = true; // File handler for reading
    public static int ch; // Character to read/write
    
    // timmings
    static long last=0, current=0, accum=0;
      
    public static void endProfile()
     {
        current = System.currentTimeMillis();
        accum = accum + ( current-last );
        System.out.println( "Time elapsed: " + accum + " milliseconds" );
    }
      
    public static void resumeProfile()
     {
        current = System.currentTimeMillis();
        last = current;
    }
      
    public static void startProfile()
     {
        accum = 0;
        current = System.currentTimeMillis();
        last = current;
    }
      
    public static void stopProfile()
     {
        current = System.currentTimeMillis();
        accum = accum + ( current - last );
    }
    public LinkedList vertices;
    public LinkedList edges;
    
    public Reader inFile;
 
    public Graph() {
        vertices = new LinkedList();
        edges = new LinkedList();
    }
              
    public boolean CycleCheck() {
        CycleWorkSpace c = new CycleWorkSpace( isDirected );
        GraphSearch( c );
        return c.AnyCycles;
    }
   
    // Graph search receives a Working Space, and 
    public void GraphSearch( WorkSpace w ) {
        int           s, c;
        Vertex  v;
  
        // Step 1: initialize visited member of all nodes

        s = vertices.size();
        if ( s == 0 )
            return; // if there are no vertices return
         
        // Initializig the vertices
        for ( c = 0; c < s; c++ ) {
            v = ( Vertex ) vertices.get( c );
            v.init_vertex( w );
        }

        // Step 2: traverse neighbors of each node
        for ( c = 0; c < s; c++ ) {
            v = ( Vertex ) vertices.get( c );
            if ( !v.visited ) {
                w.nextRegionAction( v );
                v.dftNodeSearch( w );
            }
        } //end for
    }

    public void NumberVertices() {
        GraphSearch( new NumberWorkSpace() );
    }

    // Adds an edge with weights
    public void addAnEdge( Vertex start,  Vertex end, int weight )
    {
        Edge theEdge = new  Edge();
        theEdge.EdgeConstructor( start, end, weight );
        addEdge( theEdge );
    }
   
    public void addEdge( Edge the_edge ) {
        Vertex start = the_edge.start;
        Vertex end = the_edge.end;
        edges.add( the_edge );
        start.addNeighbor( new  Neighbor( end,the_edge ) );
    }
      
    // This method adds only the edge and not the neighbor
    // used in Transpose
    public void addOnlyEdge( Edge the_edge ) {
        edges.add( the_edge );
    }
   
    public void addVertex( Vertex v ) {
        vertices.add( v );
    }
       
    public void display() {
        int i;
                                   
        System.out.println( "******************************************" );
        System.out.println( "Vertices " );
        for ( i=0; i<vertices.size(); i++ )
            ( ( Vertex ) vertices.get( i ) ).display();
         
        System.out.println( "******************************************" );
        System.out.println( "Edges " );
        for ( i=0; i<edges.size(); i++ )
            ( ( Edge ) edges.get( i ) ).display();
                
        System.out.println( "******************************************" );
    }
      
    // Finds a vertex given its name in the vertices list
    public  Vertex findsVertex( String theName )
      {
        int i=0;
        Vertex theVertex;
        
        // if we are dealing with the root
        if ( theName==null )
            return null;
            
        for( i=0; i<vertices.size(); i++ )
        {
            theVertex = ( Vertex )vertices.get( i );
            if ( theName.equals( theVertex.name ) )
                return theVertex;
        }
        return null;
    }
      
    public int readNumber() throws IOException
    {
        int index =0;
        char[] word = new char[80];
        int ch=0;
                
        ch = inFile.read();
        while( ch==32 )
            ch = inFile.read(); // skips extra whitespaces
                
        while( ch!=-1 && ch!=32 && ch!=10 ) // while it is not EOF, WS, NL
        {
            word[index++] = ( char )ch;
            ch = inFile.read();
        }
        word[index]=0;
                
        String theString = new String( word );
                
        theString = new String( theString.substring( 0,index ) );
        return Integer.parseInt( theString,10 );
    }

    // Fall back method that stops the execution of programs


    // Executes Number Vertices
    public void run( Vertex s )
     {
        NumberVertices();
        System.out.println( " Cycle? " + CycleCheck() );
    }
      
    public void runBenchmark( String FileName ) throws IOException
    {
        try {
            inFile = new FileReader( FileName );
        }
        catch ( IOException e )
        {
            System.out.println( "Your file " + FileName + " cannot be read" );
        }
    }
      
    public void stopBenchmark() throws IOException
    {
        inFile.close();
    }
      
}
