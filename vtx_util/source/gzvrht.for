      INTEGER FUNCTION GZVRHT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GET POINTER TO VRHT -- USER BANK
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-JUN-1994   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUSER.LINK'
C----------------------------------------------------------------------
      GZVRHT = LQ(LHEAD-IZUSER)
  999 RETURN
      END
