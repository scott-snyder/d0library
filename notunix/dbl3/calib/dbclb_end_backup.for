      SUBROUTINE DBCLB_END_BACKUP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  To close the backup file
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   4-MAR-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DBSTP.INC'
      INTEGER ERR
C
      IF(OPTB) THEN
        CALL FZENDO(BUNIT,'T')
        CLOSE(UNIT=BUNIT)
        CALL RLUNIT(15,BUNIT,ERR)
        OPTB = .FALSE.
        BUNIT = -1
      ELSE
        CALL INTMSG( ' Backup file is not open.')
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
