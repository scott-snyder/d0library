      SUBROUTINE UREQST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-         Handle Interrupt menu requests (called by PROCES)
C-
C-   Created   4-NOV-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NTOT,NRUN,NEVT,RUNNO,EVONUM,EVTCNT
      CHARACTER*48 EVTMSG
      CHARACTER*76 MSG
      LOGICAL INTREQ,FLGVAL
C----------------------------------------------------------------------
C
      IF(INTREQ()) THEN
C
        IF(FLGVAL('STATUS')) THEN
          NTOT=EVTCNT()
          WRITE(MSG,102) EVTMSG(),NTOT
          CALL INTMSG(MSG)
          CALL USRPST
          CALL FLGSET('STATUS',.FALSE.)
        ENDIF
C
        IF(FLGVAL('SUMMARIES')) THEN
          CALL CANMEN
          CALL SUMARY
          CALL FLGSET('SUMMARIES',.FALSE.)
        ENDIF
C              
        IF(FLGVAL('QUIT')) THEN
          CALL CANMEN
          CALL QUIT
          CALL FLGSET('QUIT',.FALSE.)
        ENDIF
C              
        IF(FLGVAL('EXAMINE')) THEN
          IF(FLGVAL('CANCEL_INTER')) THEN
            CALL CANMEN
            CALL FLGSET('CANCEL_INTER',.FALSE.)
          ENDIF
          IF ( FLGVAL('CANCEL_EXAMINE') ) THEN
            CALL CANMEN
            CALL FLGSET('CANCEL_EXAMINE',.FALSE.)
            CALL FLGSET('EXAMINE',.FALSE.)
          ELSE
            CALL HISPAK(.TRUE.)
          ENDIF
        ENDIF
C
      ENDIF
  999 RETURN
  102 FORMAT(A48,',  Total read=',I5)
      END
