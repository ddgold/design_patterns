package quark;

class intro extends I {
   String name;

   intro(String name) { this.name = name; }

   public tree apply( I i ) { return new intsum( i, this ); }

   public tree apply( A a ) { return new advprog( a, this ); }

   public tree apply( G g ) { return new gadvprog( g, this ); }

   public tree apply( H h ) { return this; }
   
   public String toString() { return name; }

   public String eval( String fnc ) { return fnc + name; }
}
