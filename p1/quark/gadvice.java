package quark;

class gadvice extends G {
   String name;

   gadvice( String name ) { this.name = name; }

   public tree apply( G g ) { return new gadvsum( g, this ); }

   public tree apply( H h ) { return new ghoaadv( h, this ); }

   public String toString() { return name; }

   public String eval( String fnc ) { return fnc+name; }
}
