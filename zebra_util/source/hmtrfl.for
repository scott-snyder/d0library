      SUBROUTINE HMTRFL(NEVENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank HMTR
C-
C-   Inputs  :NEVENT = Event number counter.
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990 14:17:30.58  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
C
      INTEGER NEVENT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(LHMTR.EQ.0)THEN
        CALL ERRMSG('HMATRIX','HMTRFL',
     &    ' HMTR bank does not exist ','W')
C
        RETURN
      ENDIF
C
      IF ( FIRST ) THEN
C do initialization here if necessary.
        FIRST = .FALSE.
        IC(LHMTR+1) = 1               ! Bank version
      ENDIF
      IC(LHMTR+2) = NEVENT
  999 RETURN
      END
