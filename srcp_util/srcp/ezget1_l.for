      SUBROUTINE EZGET1_l (ID,JSTART,JEND,JSTEP,IVAL,IER)
      IMPLICIT NONE
C
      INTEGER       ID
      INTEGER       JSTART
      INTEGER       JEND
      INTEGER       JSTEP
      logical       IVAL(*)
      INTEGER       IER

      call ezget1 (id, jstart, jend, jstep, ival, ier)
      return
      end
