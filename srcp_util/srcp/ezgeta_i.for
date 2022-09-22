      SUBROUTINE EZGETA_i (PARAM1,JSTART,JEND,JSTEP,IVAL,IER)
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM1
      INTEGER       JSTART
      INTEGER       JEND
      INTEGER       JSTEP
      INTEGER       IVAL
      INTEGER       IER
C
      call ezgeta (param1, jstart, jend, jstep, ival, ier)
      return
      end
