      SUBROUTINE DMPRAW
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       Handle raw (hexadecimal) event dumps
C-
C-   Created  29-JUL-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:DUMP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NOLD,I,IER
      INTEGER NEVT,EVTCNT
      LOGICAL FLGVAL,DMP_ONLIST
C----------------------------------------------------------------------
C
      IF(FLGVAL('DUMP_NONE_H')) GOTO 999  ! this dump not allowed
C
C         manual dump request
      IF(FLGVAL('DUMPH_REQ')) THEN
        CALL DMPDOH
        CALL FLGSET('DUMPH_REQ',.FALSE.)
C
      ELSE
        IF(NHDONE.GE.NDUMP) THEN ! no automatic dump
          NOLD=EVTCNT()
        ELSE
C
C         automatic dump request
          IF ( NSKIP.GT.1 ) THEN     ! skip events
            NEVT=EVTCNT()
            IF((NEVT-NOLD).LT.NSKIP) THEN
              NHDONE=NHDONE+1
              CALL DMPDOH
            ENDIF
C
          ELSE IF( NSKIP.LT.0) THEN  ! check list
            IF(DMP_ONLIST('RAWD')) THEN
              CALL DMPDOH
            ENDIF
          ENDIF
C
        ENDIF
C
      ENDIF
C
  999 RETURN
      END
