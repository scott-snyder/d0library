      SUBROUTINE FDTBTP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find a testbeam timing pulse given its channel
C-                         location.
C-
C-   Inputs  : none
C-   Outputs :
C-
C-   Created  11-NOV-1988   Jeffrey Bantly
C-   Updated  20-MAR-1990   Jeffrey Bantly  general cleanup
C-   Updated  20-MAR-1991   Jeffrey Bantly  change FGTPDS, updated PARAMS,RCP
C-   Updated  15-JUL-1991   Susan K. Blessing   Remove VZERO calls.
C-   Updated  12-MAY-1992   Susan K. Blessing  Declare PEDS(2) (removed from
C-    FDEVNT.INC).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDEVNT.INC'
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE
      INTEGER I, JJ, LUNDBG, IER, ICALL
      INTEGER TIMPCH, TIMPCU, TIMPCQ, TIMPCS, TIMPCW, TIMCHA
      INTEGER EVTDAT(0:LFADC-1), EVTDIF(0:LFADC-1)
      INTEGER RUNTYPE
C
      REAL    TMPUBN2
      REAL    PEDS(2)
C
      LOGICAL CMATCH
C
      SAVE ICALL
      DATA ICALL / 0 /
C----------------------------------------------------------------------
      IF( ICALL .EQ. 0 ) THEN
        ICALL = 1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('RUNTYPE',RUNTYPE,IER)
        CALL EZGET('TIMPCH',TIMPCH,IER)
        CALL EZGET('TIMPCU',TIMPCU,IER)
        CALL EZGET('TIMPCQ',TIMPCQ,IER)
        CALL EZGET('TIMPCS',TIMPCS,IER)
        CALL EZGET('TIMPCW',TIMPCW,IER)
        CALL FCODER(TIMCHA,TIMPCH,TIMPCU,TIMPCQ,TIMPCS,TIMPCW,0,2)
        CALL EZSET('TIMCHA',TIMCHA,IER)
        CALL EZRSET
      ENDIF
C
      HALF=TIMPCH
      UNIT=TIMPCU
      QUAD=TIMPCQ
      SECTOR=TIMPCS
      WIRE=TIMPCW
C
C  Get timing pulse channel data
C
      FINDTP = .FALSE.
      CALL FDUNPK(TIMCHA, EVTDAT, FINDTP)
      IF ( .NOT. FINDTP ) GOTO 999
      CALL FGTLPD(HALF,UNIT,QUAD,SECTOR,WIRE,PEDS(1),PEDS(2))
C                                                  ! valid for TB only
      IF ( PEDS(1) .LE. 0.0 ) THEN
        PEDS(1) = 10.0                  ! use fake values
        PEDS(2) = 1.0
      ENDIF
      CALL FORDER(EVTDAT,HALF,UNIT,QUAD,SECTOR,WIRE)
C
C  Calculate first difference
C
      EVTDIF(0)=0
      DO 30 JJ=1,LFADC-1
        IF ( EVTDAT(JJ-1) .EQ. 0 ) THEN
          EVTDIF(JJ)=0
        ELSE
          EVTDIF(JJ)=EVTDAT(JJ)-EVTDAT(JJ-1)
        ENDIF
   30 CONTINUE
C
C  Find timing pulse
C
      IF ( RUNTYPE.GT.2 ) THEN
        CALL FDTSUB_D0( EVTDIF, TMPUBN, TMPUBN2, FINDTP)
      ELSE
        CALL FDTSUB(EVTDAT, EVTDIF, TMPUBN, FINDTP)
      ENDIF
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
