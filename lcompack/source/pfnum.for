      FUNCTION PFNUM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the last value of PF, the number of the PF
C-                         key struck.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C-   Updated  20-OCT-1991   Harrison B. Prosper  
C-    Add IABS around PF key 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PFNUM
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
C----------------------------------------------------------------------
      PFNUM=IABS(PF)
      RETURN
      END
