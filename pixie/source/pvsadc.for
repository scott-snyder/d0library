      SUBROUTINE PVSADC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action routine to display raw FADC data
C-                         for the VTX z strips.  Either single or multiple
C-                         channels can be displayed.  For a single channel,
C-                         the first differences are also shown.
C-
C-   Inputs  : none
C-   Outputs : the display
C-
C-   Created  15-OCT-1989  Peter Grudberg from PFFADC
C-   Updated  24-SEP-1990   Lupe Howell   Implementing PIXIE_RCP
C-   Updated   5-FEB-1991   Lupe Howell   Purging the segments was eliminated
C-      since PURSTR does not use reatined segments anymore.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'

      INTEGER NBCHAN, NMCHAN, NUMTRY, LENGTH
      INTEGER ZLAY, ZLAYER, ISTRIP, STRIP
      INTEGER MXSTRP, MAXSTR(0:5)
      INTEGER IZEND, ZEND, MXZEND, DETTYP
      CHARACTER*60 PROMPT, STRING, MESSAG
      CHARACTER*4 CVAL, REM
      INTEGER IER,TYP,II,JJ
      LOGICAL EZERROR
      PARAMETER ( DETTYP = 1 )          ! strips (not wires)
      DATA MAXSTR / -1, -1, 159, 191, 191, 127 /
      DATA NBCHAN / 1 /
C----------------------------------------------------------------------
      CALL JBGBAT(0)                    ! start batch-of-updates - only
      ! redraw screen once
      NUMTRY = 3                        ! give 3 tries for correct input
    1 CONTINUE
C
C ****  Get the necessary information to identify FADC data desired
C
      WRITE (PROMPT,100) NBCHAN
  100 FORMAT(' Number of channels to plot (1 or 16) ? [',I2,']>')
   10 CONTINUE
      CALL OUTMSG('1')
      CALL GETPAR(1,PROMPT,'U',STRING)
      CALL SWORDS(STRING,II,JJ,LENGTH)
      IF ( LENGTH .GT. 0 ) THEN
        READ (STRING(1:LENGTH),*,ERR=700) NMCHAN
      ELSE
        NMCHAN = NBCHAN
      ENDIF
      IF ( NMCHAN .NE. 1 .AND. NMCHAN .NE. 16 ) THEN
        NUMTRY = NUMTRY - 1
        WRITE (MESSAG,160) NUMTRY
        CALL PUMESS(MESSAG)
        IF ( NUMTRY .GT. 0 ) GOTO 10
        CALL JENBAT                       ! Clear dialogue
        GOTO 999
      ENDIF
      NBCHAN = NMCHAN
C
C ****  Picking PIXIE RCP
C
      CALL EZPICK('PX_VTXDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PVSADC','Cannot find PX_VTXDIS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get some VTX constants
C
      CALL EZ_GET_ARRAY('PXPARAMS','VTX STRIP LAYER',1,ZLAY,
     &       CVAL,TYP,REM,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('PIXIE','PVSADC',
     &      'PXPARAMS NOT FOUND','W')
        GOTO 900
      ENDIF
      WRITE (PROMPT,110) ZLAY
  110 FORMAT(' Strip layer number (2-5) ? [',I1,']>')
   20 CONTINUE
      CALL GETPAR(1,PROMPT,'U',STRING)
      CALL SWORDS(STRING,II,JJ,LENGTH)
      IF ( LENGTH .GT. 0 ) THEN
        READ (STRING(1:LENGTH),*,ERR=700) ZLAYER
      ELSE
        ZLAYER = ZLAY
      ENDIF
      IF ( ZLAYER .LT. 2 .OR. ZLAYER .GT. 5 ) THEN
        NUMTRY = NUMTRY - 1
        WRITE (MESSAG,160) NUMTRY
        CALL PUMESS(MESSAG)
        IF ( NUMTRY .GT. 0 ) GOTO 20
        CALL JENBAT                       ! Clear dialogue
        GOTO 900
      ENDIF
      CALL EZ_SET_ARRAY('PXPARAMS','VTX STRIP LAYER',ZLAYER,IER)
C
      CALL EZ_GET_ARRAY('PXPARAMS','VTX STRIP',1,ISTRIP,CVAL,
     &       TYP,REM,IER)
      MXSTRP = MAXSTR(ZLAYER)
      IF ( ISTRIP .GT. MXSTRP ) ISTRIP = MXSTRP
      IF ( NMCHAN .EQ. 1 ) THEN
        WRITE (PROMPT,120) MXSTRP, ISTRIP
  120   FORMAT(' Strip number (0-',I3,') ? [',I3,']>')
      ELSE
        WRITE (PROMPT,130) MXSTRP, ISTRIP
  130   FORMAT(' Starting strip number (0-',I3,') ? [',I3,']>')
      ENDIF
   30 CONTINUE
      CALL GETPAR(1,PROMPT,'U',STRING)
      CALL SWORDS(STRING,II,JJ,LENGTH)
      IF ( LENGTH .GT. 0 ) THEN
        READ (STRING(1:LENGTH),*,ERR=700) STRIP
      ELSE
        STRIP = ISTRIP
      ENDIF
      IF ( STRIP .LT. 0 .OR. STRIP .GT. MXSTRP ) THEN
        NUMTRY = NUMTRY - 1
        WRITE (MESSAG,160) NUMTRY
        CALL PUMESS(MESSAG)
        IF ( NUMTRY .GT. 0 ) GOTO 30
        CALL JENBAT                       ! Clear dialogue
        GOTO 900
      ENDIF
      CALL EZ_SET_ARRAY('PXPARAMS','VTX STRIP',STRIP,IER)
C
      IF ( ZLAYER .EQ. 2 ) THEN         ! Strips split at z=0
        CALL EZ_GET_ARRAY('PXPARAMS','VTX STRIP END',1,IZEND,
     &       CVAL,TYP,REM,IER)
        IF ( NMCHAN .EQ. 1 ) THEN
          MXZEND = 1
          IF ( IZEND .GT. MXZEND ) IZEND = 0
          WRITE (PROMPT,140) IZEND
  140     FORMAT(' Which end: 0(-z), 1(+z) ? [',I1,']>')
        ELSE
          MXZEND = 2
          WRITE (PROMPT,150) IZEND
  150     FORMAT(' Which end: 0(-z), 1(+z), 2(both) ? [',I1,']>')
        ENDIF
   40   CONTINUE
        CALL GETPAR(1,PROMPT,'U',STRING)
        CALL SWORDS(STRING,II,JJ,LENGTH)
        IF ( LENGTH .GT. 0 ) THEN
          READ (STRING(1:LENGTH),*,ERR=700) ZEND
        ELSE
          ZEND = IZEND
        ENDIF
        IF ( ZEND .LT. 0 .OR. ZEND .GT. MXZEND ) THEN
          NUMTRY = NUMTRY - 1
          WRITE (MESSAG,160) NUMTRY
          CALL PUMESS(MESSAG)
          IF ( NUMTRY .GT. 0 ) GOTO 40
          CALL JENBAT                       ! Clear dialogue
          GOTO 900
        ENDIF
        CALL EZ_SET_ARRAY('PXPARAMS','VTX STRIP END',ZEND,IER)
      ENDIF
C
C ****  Call the correct routine for the plot type
C
      CALL JENBAT                       ! do all purges
      IF( NMCHAN .EQ. 16 ) CALL PVFSEC(DETTYP)
      IF( NMCHAN .EQ.  1 ) CALL PV1ADC(DETTYP)
      GOTO 900
C
C ****  In case of read error, allow two retries to input numbers
C
  700 CONTINUE
      NUMTRY = NUMTRY - 1
      WRITE (MESSAG,160) NUMTRY
  160 FORMAT(' Input error, ',I1,' tries remaining')
      CALL PUMESS(MESSAG)
      IF ( NUMTRY .GT. 0 ) GOTO 1
      CALL JENBAT                       ! Clear dialogue
C-----------------------------------------------------------------------
C
C ****  Reseting RCP file
C
  900 CALL EZRSET
  999 CONTINUE
      RETURN
      END
