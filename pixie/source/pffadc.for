      SUBROUTINE PFFADC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action routine to display the raw FADC data
C-                         of a single FDC sense wire or delay line
C-                         and can also display the first differences.
C-
C-   Inputs  : none
C-   Outputs : the display
C-
C-   Created  20-APR-1989   Jeffrey Bantly
C-   Updated  19-MAR-1990   Jeffrey Bantly  use logical format
C-   Updated   5-NOV-1990   Jeffrey Bantly  can now turn off hits location page
C-                                          that appears before user choices.
C-   Updated  23-JAN-1991   Jeffrey Bantly  check for CDD3 bank
C-   Updated  20-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK
C-   Updated   2-MAR-1992   Robert E. Avery  Update for New Pixie, and
C-      generally bring back up to date.
C-   Updated  19-JUN-1992   Robert E. Avery  Change dialogue (no references
C-                                                      to unit number) 
C-   Updated  25-JUN-1992   Robert E. Avery  Allow user to choose vert. scale. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
      INTEGER HALF, UNIT, QUAD, SECTOR, WIRE
      INTEGER LKCDD2,LKCDD3
      INTEGER II, JJ
      INTEGER IER, ICALL
      INTEGER LEN
      INTEGER VSCALE
      INTEGER TRACE_TYPE
      INTEGER PSHLEV
      LOGICAL PULSHP
      LOGICAL EZERROR
      LOGICAL FLGVAL,HARDCOPY
      CHARACTER*80 PROM1,PROM2,PROM3,PROM4
      CHARACTER*60 ANSWER
C
      DATA PROM1 /' Unit T(heta), P(hi)? (2=Back to Menu)>'/
      DATA PROM2 /' Wire? (0-9 for Theta 8,9=DL and 0-15 for Phi)>'/
      DATA PROM3 /
     &  ' Trace type?(1-5,1=orig,2=left,3=subtr,4=1&2,5=1&3,6=1&2&3)'/
      DATA PROM4 /' Enter vertical scale (1-1000, 0=auto scale) >'/ 
      DATA ICALL /0/
C----------------------------------------------------------------------
C
      LKCDD2=LQ(LHEAD-IZCDD2)
      LKCDD3=LQ(LHEAD-IZCDD3)
      IF(LKCDD2.LE.5.AND.LKCDD3.LE.5) THEN
        CALL INTMSG(' No FADC CDD3 bank present')
        GOTO 999
      ENDIF
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFFADC','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
C
      CALL PUGETV( 'FDC UNIT', UNIT)
      CALL PUGETV( 'FDC WIRE', WIRE)
      CALL PUGETV( 'FDC PULSHP', PULSHP)
C
      IF(ICALL.EQ.0) THEN
        ICALL = 1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('PULSE_SHAPE_LEVEL',PSHLEV,IER)
        CALL EZRSET
      ENDIF
C
      HARDCOPY = FLGVAL('HARDCOPY')
      IF ( .NOT.HARDCOPY ) THEN
C
        CALL OUTMSG('1')
        ANSWER=' '
        CALL GETPAR(1,PROM1,'U',ANSWER)
        CALL SWORDS(ANSWER,II,JJ,LEN)
        IF ( LEN.NE.0 ) THEN
          IF (ANSWER(1:1).EQ.'T'.OR.ANSWER(1:1).EQ.'t') THEN
            UNIT = 0
          ELSE IF (ANSWER(1:1).EQ.'P'.OR.ANSWER(1:1).EQ.'p') THEN
            UNIT = 1
          ELSE
            READ ( ANSWER(1:LEN),*,ERR=900) UNIT
          END IF
        END IF
        IF (UNIT.EQ.2) GOTO 900
        IF (UNIT.LT.0 .OR. UNIT.GT.1) UNIT = 0
C
        CALL PFPICK_SECTOR(UNIT,HALF,QUAD,SECTOR)
        IF (HALF.EQ.2) GOTO 900
C
        ANSWER=' '
        CALL GETPAR(1,PROM2,'U',ANSWER)
        CALL SWORDS(ANSWER,II,JJ,LEN)
        IF ( LEN.NE.0 ) THEN
          READ ( ANSWER(1:LEN),*,ERR=900) WIRE
        END IF
        IF ( UNIT.EQ.0 ) THEN
          IF (WIRE.LT.0 .OR. WIRE.GT.9) WIRE = 0
        ELSE
          IF (WIRE.LT.0 .OR. WIRE.GT.15) WIRE = 0
        ENDIF
C
        TRACE_TYPE = 1
        IF( PULSHP .AND. (PSHLEV.GT.1) ) THEN
          ANSWER=' '
          CALL GETPAR(1,PROM3,'U',ANSWER)
          CALL SWORDS(ANSWER,II,JJ,LEN)
          IF(LEN.NE.0) READ(ANSWER(1:LEN),*,ERR=200) TRACE_TYPE
          IF(TRACE_TYPE .LT. 1 .OR. TRACE_TYPE .GT. 6 ) THEN
  200       TRACE_TYPE = 1
          ENDIF
        ENDIF
        CALL PUSETV('FDC HALF',HALF)
        CALL PUSETV('FDC UNIT',UNIT)
        CALL PUSETV('FDC QUAD',QUAD)
        CALL PUSETV('FDC SECT',SECTOR)
        CALL PUSETV('FDC WIRE',WIRE)
C
        CALL PUGETV( 'ELECT VERT SCALE', VSCALE)
        ANSWER=' '
        CALL GETPAR(1,PROM4,'U',ANSWER)
        CALL SWORDS(ANSWER,II,JJ,LEN)
        IF(LEN.NE.0) READ(ANSWER(1:LEN),*,ERR=300) VSCALE
        IF (VSCALE.LT.0 .OR. VSCALE.GT.1000) THEN
  300     VSCALE = 1000
        ENDIF
        CALL PUSETV( 'ELECT VERT SCALE', VSCALE)
C
      ENDIF
C
      CALL PF1ADC(TRACE_TYPE)
C
C-----------------------------------------------------------------------
  900 CONTINUE
      CALL EZRSET
C
  999 RETURN
      END
