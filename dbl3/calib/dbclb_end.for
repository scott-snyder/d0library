      SUBROUTINE DBCLB_END
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DBL3 - CLOSE DBL3
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   03-JUN-1992   S. Abachi
C-   Modified  10-JUN-1994   S. Abachi   LFORCE and LVSN added
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      CHARACTER*80 MSG
C----------------------------------------------------------------------
C
      DOPT = 0
      LFORCE = .FALSE.
      LVSN = .FALSE.
      IF (CALL_DBEND) THEN
        CALL DBENDF('D0STP')
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB(' DBENDF')
        ENDIF
        CALL_DBEND = .FALSE.
      ELSE
        WRITE(MSG,10)
   10   FORMAT(' Attempt to Call DBEND the second time. Aborted ')
        CALL INTMSG(MSG)
      ENDIF
C
C&IF VAXVMS
      IF(OPTJ) THEN
        CALL DBCLB_END_JOURNAL
        OPTJ = .FALSE.
      ENDIF
C&ELSE
C&ENDIF
C
  999 RETURN
      END
