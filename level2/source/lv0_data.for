      FUNCTION LV0_DATA()
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : THIS IS A LOGICAL FUNCTION THAT WILL EXTRACT
C-                         THE LEVEL0 DATA(FAST Z INFORMATION FOR NOW) FROM
C_                         TRGR BANK AND PUT RESULTS INTO BANK
C-                         L0VT WHICH HANGS UNDER FILT. THE FUNCTION IS
C-                         TRUE IS THE BANK WAS SUCCESSFULLY CREATED
C-                         AND FILLED, FALSE IF THE BANK WAS NOT
C-                         BOOKED
C-                          This is an interface routine for package LV0_DATA
C-
C-   INPUTS  : VERTEX INFORMATION FROM TRGR BANK
C-   OUTPUTS : TRUE FOR SUCCESSFULLY FILLING BANK WITH DATA
C-   CONTROLS: NONE
C-
C-   CREATED   5-JUN-1992   TOM FAHLAND
C-   Updated  19-JUL-1992   James T. Linnemann  more paranoid zebra handling
C-                                              make parameter for z bin size
C-   Updated  21-SEP-1992   Jeffrey Bantly   include level0 slow z result
C-   Updated  14-OCT-1992   James T. Linnemann   call lower level routines
C----------------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LV0_DATA
      INTEGER GZL0VT, LL0VT
C----------------------------------------------------------------------------
      CALL L0VTFL
      LL0VT = GZL0VT()
      LV0_DATA = (LL0VT .GT.0) 
  999 RETURN
      END
