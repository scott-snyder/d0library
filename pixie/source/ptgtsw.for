      SUBROUTINE PTGTSW
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Lest the users enter a wire number to display 
C-          its FADC trace and the traces of its surrounding wires.
C-
C-
C-   Created   6-FEB-1989   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*37 PROMT
      CHARACTER*80 STRING
      INTEGER LENGTH,WIRE,ITRY
C----------------------------------------------------------------------
      DATA PROMT/'Enter wire num to display FADC traces'/
C----------------------------------------------------------------------
      ITRY=0
  10  CALL PURSTR(PROMT,STRING,LENGTH)
      IF(LENGTH.NE.0)THEN
        READ(STRING(1:LENGTH),*,ERR=700)WIRE
      ENDIF
      IF ((WIRE.GE.1).AND.(WIRE.LE.256)) THEN
        CALL PTSURW(WIRE)      ! Displays FADC traces of the wire and
C                               ! its sourrounding wires
      ELSE
        ITRY=1
        GO TO 710
      ENDIF
      GO TO 999
  700 IF (ITRY.GT.0) GOTO 999
      ITRY = 1
      CALL PUMESS(' ERROR IN reading wire number - try again')
      GO TO 10
  710 IF(ITRY.GT.0) GO TO 999
      ITRY= 1
      CALL PUMESS(' ERROR IN wire number - try again')
      GO TO 10
  999 RETURN
      END
