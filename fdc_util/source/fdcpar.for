      LOGICAL FUNCTION FDCPAR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization for FDC stuff
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  31-MAR-1990   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL OK
      LOGICAL FTIMEP,FDBHST
C
C----------------------------------------------------------------------
C
      OK = FTIMEP()
      OK = FDBHST() .OR. OK
C
      FDCPAR = OK
C
  999 RETURN
      END
