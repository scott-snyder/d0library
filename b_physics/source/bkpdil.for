      SUBROUTINE BKPDIL(LPDIL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book a PDIL bank
C-   
C-   Outputs : 
C-     LPDIL = pointer to created bank
C-
C-   Created   4-DEC-1991   Daria Zieminska   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZPDIL.LINK/LIST'
      INTEGER LPDIL, LPARH, GZPARH, IXPDIL, ND
      PARAMETER( ND = 27 )
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
      LPDIL = 0
      IF(FIRST)THEN
        CALL MZFORM('PDIL','6I -F',IXPDIL)        
        FIRST = .FALSE.
      ENDIF
      LPARH = GZPARH()
      IF(LPARH.EQ.0) GO TO 999
      CALL MZBOOK(IXMAIN,LPDIL,LPARH,-IZPDIL,'PDIL',3,1,ND,IXPDIL,0)
  999 RETURN
      END
