      SUBROUTINE D0DBL3_END_JOURNAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  To close the jounal file
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   4-MAR-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INTEGER ERR
C
      IF(OPTJ) THEN
        CALL FZENDO(JUNIT,'T')
        CLOSE(UNIT=JUNIT)
        CALL RLUNIT(171,JUNIT,ERR)
        OPTJ = .FALSE.
        JUNIT = -1
      ELSE
        CALL INTMSG( ' Journal file is not open.')
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
