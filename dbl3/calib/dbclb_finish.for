      SUBROUTINE DBCLB_FINISH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DBL3 - WRAP UP AND GO HOME
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   17-NOV-1989   S. Abachi, Jan Guida, S. Rajagopalan
C-   Modified  Oct-92        J.Green   Change DBEND to DBENDF
C-   Modified  10-JAN-1994   S. Abachi  LFORCE & LVSN added
C-   Modified  21-JAN-1994   S. Abachi  Choice of top direct name added
C-   Modified  24-JAN-1994   S. Abachi  Reading server changes added
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER IER
      CHARACTER*80 MSG
C----------------------------------------------------------------------
C
      IF(RCSERVER) THEN
C&IF VAXVMS,VAXELN
        CALL INTMSG
     &    ('DBCLB_FINISH: RCSERVER NOT AVAILABLE ON THIS MACHINE.')
C&ENDIF
        GOTO 999
      ENDIF
      DOPT = 0
      LFORCE = .FALSE.
      LVSN = .FALSE.
      IF (CALL_DBEND) THEN
        CALL DBENDF(TOPN)
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB(' DBEND')
        ENDIF
        CALL_DBEND = .FALSE.
        CLOSE(UNIT=DBLUN)
      ELSE
        WRITE(MSG,10)
   10   FORMAT(' Attempt to Call DBEND the second time. Aborted ')
        CALL INTMSG(MSG)
      ENDIF
      TOPN = 'D0STP'
C
      IF(OPTJ) THEN
        CALL DBCLB_END_JOURNAL
        OPTJ = .FALSE.
      ENDIF
C
      IF(SERVER) THEN
CC        CALL CPC_FINISH
      ENDIF
C
  999 RETURN
      END
