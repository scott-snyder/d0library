      FUNCTION NLEGOS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Return the total number of LEGO banks
C-
C-   Created  16-FEB-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NLEGOS,N,LEGO,GZLEGO
C----------------------------------------------------------------------
      N=0
      LEGO=GZLEGO(1)
    1 IF(LEGO.NE.0) THEN
        N=N+1
        LEGO=LQ(LEGO)
        GOTO 1
      ENDIF
      NLEGOS=N
  999 RETURN
      END
