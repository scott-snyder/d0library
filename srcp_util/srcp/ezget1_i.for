      SUBROUTINE EZGET1_i (ID,JSTART,JEND,JSTEP,IVAL,IER)
      IMPLICIT NONE
C
      INTEGER       ID
      INTEGER       JSTART
      INTEGER       JEND
      INTEGER       JSTEP
      INTEGER       IVAL(*)
      INTEGER       IER

      call ezget1 (id, jstart, jend, jstep, ival, ier)
      return
      end
