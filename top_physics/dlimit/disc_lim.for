      REAL FUNCTION DISC_LIM(LUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GIVEN THE LUMINOSITY, YIELDS DISCOVERY LIMIT
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    LUM
      INCLUDE 'D0$INC:TOP_CROSS.INC'
      INCLUDE 'D0$INC:SIG_EFF.INC'
      EXTERNAL FLIM
      REAL    R,EPS
      INTEGER MAXF
      DATA MAXF/100/ !NUMBER OF ITERATONS MAX.
      DATA EPS/.01/
C----------------------------------------------------------------------
      LUMIN=LUM  !LUMINOSITY
      IF ( IEN.EQ.18 ) THEN
        CALL RZERO(80.,200.,DISC_LIM,R,EPS,MAXF,FLIM)
      ELSEIF ( IEN.EQ.20 ) THEN
        CALL RZERO(80.,250.,DISC_LIM,R,EPS,MAXF,FLIM)
      ENDIF
  999 RETURN
      END
