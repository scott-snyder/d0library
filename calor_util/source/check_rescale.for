      SUBROUTINE CHECK_RESCALE(NAME,OLD,NEW,TOL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : if old differs from new by tolerance, will alarm
C-
C-   Inputs  : NAME = NAME OF QUANTITIY
C-             OLD = OLD QUANTITIY
C-             NEW = new quantity
C-             TOL = tolerance as a fraction
C-   Outputs :
C-   Controls:
C-
C-   Created  21-NOV-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    OLD,NEW,TOL
      REAL    FRC,FRC1
      CHARACTER*(*) NAME
      CHARACTER*80 MESSAGE
      INTEGER TRULEN
C----------------------------------------------------------------------
      FRC1 = OLD+NEW
      IF ( FRC1.NE.0.0 ) THEN
        FRC = (OLD-NEW)/FRC1
        
        IF ( ABS(FRC).GT.TOL ) THEN
          MESSAGE = NAME(1:TRULEN(NAME))//' OUT OF TOLERANCE '
          CALL ERRMSG('CALORIMETER','CHECK_RESCALE',
     &      MESSAGE,'W')
        ENDIF
      ELSE
        RETURN
      ENDIF
  999 RETURN
      END
