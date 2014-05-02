package quark;

class advice extends A {
   String name;

   advice( String name ) { this.name = name; }

   // use standard tree rewrites
   //
   public String toString() { return name; }

   public String eval( String fnc ) { return fnc + name; }
}
