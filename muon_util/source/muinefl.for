      REAL FUNCTION MUINEFL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return inefficient length of WAMUS chamber.
C-                         MC version geometry "0" has no value for real wire
C-                         length and no value for difference between external
C-                         size of chamber and real wire length. So this 
C-                         function provide a difference. One can get wire
C_                         length as 
C 
C-                         Real_wire_length = external_length - MUINEFL()
C-                                          = Q(LMGEO+34)     - MUINEFL() 
C-                         
C-
C-   Inputs  : None
C-   Controls: None
C-
C-   Created   7-MAY-1992   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      MUINEFL = 9.525
  999 RETURN
      END
