// This is a crude parser for Yuml.
// The Yuml grammar for class diagrams is essentially the following
// (but note, Yuml is a bit more general)

YumlSpec : Line+ ;    // one or more lines

Line: Box | Connection ;

Box : '[' Class ']' ;  

Connection : Box [End1] [Role1] '-' [Role2] [End2] Box ;

End1 : '<>' | '++' | '^' | '<' ;
End2 : '<>' | '++' | '^' | '>' ;

Role1 :  String ;   // String that has no ']' chars

Class : String                         // name only
      | String '|' String              // name and methods only
      | String '|' String '|' String   // name, field, and methods
      ;

