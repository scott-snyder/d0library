C DEC/CMS REPLACEMENT HISTORY, Element DMPPRO.FOR
C *5    15-DEC-1989 10:37:20 SERBAN "fixed skipping in automatic dump"
C *4     8-NOV-1989 14:02:27 SERBAN "increment NFDONE and NHDONE in DMP_ONLIST"
C *3     3-NOV-1989 12:09:59 SERBAN "use DMP_ONLIST to handle lists of events"
C *2    10-AUG-1989 15:44:26 SERBAN "add call to DMPBNK for HEAD"
C *1    30-NOV-1988 11:28:43 SERBAN "manage processed event dump"
C DEC/CMS REPLACEMENT HISTORY, Element DMPPRO.FOR
      SUBROUTINE DMPPRO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Handle processed event dumps, call user hook DMPUSR
C-
C-
C-   Created  29-JUL-1988   Serban D. Protopopescu
C-   Updated  15-DEC-1989   Serban D. Protopopescu   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DUMP.INC'
      INTEGER NOLD,I,IER
      INTEGER NEVT,EVTCNT
      LOGICAL FLGVAL,FIRST,DMP_ONLIST
      SAVE FIRST,NOLD
      DATA FIRST,NOLD/.TRUE.,0/
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL DMPBNK('HEAD',.TRUE.)  ! make sure HEAD is always dumped
        CALL DMPBNK('TSUM',.TRUE.)  ! make sure TSUM is always dumped
        FIRST=.FALSE.
      ENDIF
C
      IF(FLGVAL('DUMP_EVENT')) THEN
        CALL DMPDOF
        CALL FLGSET('DUMP_EVENT',.FALSE.)
      ENDIF
      IF(.NOT.FLGVAL('DUMP_PROCES')) GOTO 999 ! this dump is not allowed
C
C         manual dump request
      IF(FLGVAL('DUMPF_REQ')) THEN
        CALL DMPDOF
        CALL FLGSET('DUMPF_REQ',.FALSE.)
C
      ELSE
        IF(NFDONE.GE.NDUMP) THEN ! no automatic dump
          NOLD=EVTCNT()
C
        ELSE
C
C         automatic dump request
          IF ( NSKIP.GE.0 ) THEN     ! skip events
            NEVT=EVTCNT()
            IF((NEVT-NOLD).GT.NSKIP) THEN
              NFDONE=NFDONE+1
              CALL DMPDOF
            ELSE
              IF(NFDONE.GE.NDUMP) NOLD=NEVT
            ENDIF
C
          ELSE IF( NSKIP.LT.0) THEN  ! check list
            IF ( DMP_ONLIST('PROD') ) THEN
              CALL DMPDOF
              NOLD=NEVT
            ENDIF
          ENDIF
C
        ENDIF
C
      ENDIF
C
  999 RETURN
      END
