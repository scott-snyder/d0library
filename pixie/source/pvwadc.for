      SUBROUTINE PVWADC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action routine to display raw FADC data
C-                         for the VTX sense wires..  Either a full sector
C-                         or a single channel can be displayed.  For a
C-                         single channel, the first differences are also
C-                         shown.
C-
C-   Inputs  : none
C-   Outputs : the display
C-
C-   Created  15-OCT-1989  Peter Grudberg from PFFADC
C-   Updated  24-SEP-1990   Lupe Howell   Implementing PIXIE_RCP
C-   Updated   5-FEB-1991   Lupe Howell   Purging the segments was eliminated
C-      since PURSTR does not use reatined segments anymore.
C-   Updated  29-MAY-1992   Peter M. Grudberg Fix rcp bugs
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
C
      INTEGER NUMTRY, LENGTH
      INTEGER PLOTYP, PLTYPE
      INTEGER LAY, LAYER, SEC, SECTOR, MXSEC, MAXSEC(0:2)
      INTEGER IWIR, WIRE, IEND, END, DETTYP,II,JJ
      CHARACTER*60 PROMPT, STRING, MESSAG
      DATA MAXSEC / 15, 31, 31 /
      DATA PLOTYP / 1 /
      CHARACTER*4 CVAL, REM
      INTEGER IER,TYP
      LOGICAL EZERROR
      PARAMETER ( DETTYP = 0 )                 ! Wires (not strips)
C----------------------------------------------------------------------
      CALL JBGBAT(0)                    ! start batch-of-updates - only
      ! redraw screen once
      NUMTRY = 3                        ! give 3 tries for correct input
    1 CONTINUE
C
C ****  Get the necessary information to identify FADC data desired
C
      WRITE (PROMPT,100) PLOTYP
  100 FORMAT(' Plot sector(1) or channel(2)? [',I1,']>')
   10 CONTINUE
      CALL OUTMSG('1')
      CALL GETPAR(1,PROMPT,'U',STRING)
      CALL SWORDS(STRING,II,JJ,LENGTH)
      IF (LENGTH .GT. 0) THEN
        READ (STRING(1:LENGTH),*,ERR=700) PLTYPE
      ELSE
        PLTYPE = PLOTYP
      ENDIF
      IF (PLTYPE .NE. 1 .AND. PLTYPE .NE. 2) THEN
        NUMTRY = NUMTRY - 1
        WRITE (MESSAG,150) NUMTRY
        CALL PUMESS(MESSAG)
        IF ( NUMTRY .GT. 0 ) GOTO 10
        CALL JENBAT            ! End batch of updates (clear screen)
        GOTO 999
      ENDIF
      PLOTYP = PLTYPE
C
C ****  Picking PIXIE RCP
C
      CALL EZPICK('PX_VTXDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PVWADC','Cannot find PX_VTXDIS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get some VTX constants
C
      CALL EZ_GET_ARRAY('PXPARAMS','VTX LAYER',1,LAY,CVAL,
     &       TYP,REM,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('PIXIE','PVWADC',
     &      'PXPARAMS NOT FOUND','W')
        GOTO 900
      ENDIF
      WRITE (PROMPT,110) LAY
  110 FORMAT (' VTX Layer number (0-2) ? [',I1,']>')
   20 CONTINUE
CC      CALL PURSTR(PROMPT,STRING,LENGTH)
      CALL GETPAR(1,PROMPT,'U',STRING)
      CALL SWORDS(STRING,II,JJ,LENGTH)
      IF ( LENGTH .GT. 0 ) THEN
        READ (STRING(1:LENGTH),*,ERR=700) LAYER
      ELSE
        LAYER = LAY
      ENDIF
      IF ( LAYER .LT. 0 .OR. LAYER .GT. 2 ) THEN
        NUMTRY = NUMTRY - 1
        WRITE (MESSAG,150) NUMTRY
        CALL PUMESS(MESSAG)
        IF ( NUMTRY .GT. 0 ) GOTO 20
        CALL JENBAT            ! End batch of updates (clear screen)
        GOTO 900
      ENDIF
      CALL EZ_SET_ARRAY('PXPARAMS','VTX LAYER',LAYER,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','VTX SECTOR',1,SEC,CVAL,
     &       TYP,REM,IER)
      MXSEC = MAXSEC(LAYER)
      WRITE (PROMPT,120) MXSEC, SEC
  120 FORMAT (' VTX Sector number (0-',I2,') ? [',I2,']>')
   30 CONTINUE
CC      CALL PURSTR(PROMPT,STRING,LENGTH)
      CALL GETPAR(1,PROMPT,'U',STRING)
      CALL SWORDS(STRING,II,JJ,LENGTH)
      IF ( LENGTH .GT. 0 ) THEN
        READ (STRING(1:LENGTH),*,ERR=700) SECTOR
      ELSE
        SECTOR = SEC
      ENDIF
      IF ( SECTOR .LT. 0 .OR. SECTOR .GT. MXSEC ) THEN
        NUMTRY = NUMTRY - 1
        WRITE (MESSAG,150) NUMTRY
        CALL PUMESS(MESSAG)
        IF ( NUMTRY .GT. 0 ) GOTO 30
        CALL JENBAT            ! End batch of updates (clear screen)
        GOTO 900
      ENDIF
      CALL EZ_SET_ARRAY('PXPARAMS','VTX SECTOR',SECTOR,IER)
      IF ( PLTYPE .EQ. 2 ) THEN         ! Get channel to plot
C
        CALL EZ_GET_ARRAY('PXPARAMS','VTX WIRE',1,IWIR,CVAL,
     &       TYP,REM,IER)
        WRITE (PROMPT,130) IWIR
  130   FORMAT(' VTX Wire number (0-7) ? [',I1,']>')
   40   CONTINUE
CC        CALL PURSTR(PROMPT,STRING,LENGTH)
        CALL GETPAR(1,PROMPT,'U',STRING)
        CALL SWORDS(STRING,II,JJ,LENGTH)
        IF ( LENGTH .GT. 0 ) THEN
          READ (STRING(1:LENGTH),*,ERR=700) WIRE
        ELSE
          WIRE = IWIR
        ENDIF
        IF ( WIRE .LT. 0 .OR. WIRE .GT. 7 ) THEN
          NUMTRY = NUMTRY - 1
          WRITE (MESSAG,150) NUMTRY
          CALL PUMESS(MESSAG)
          IF ( NUMTRY .GT. 0 ) GOTO 40
          CALL JENBAT            ! End batch of updates (clear screen)
          GOTO 900
        ENDIF
        CALL EZ_SET_ARRAY('PXPARAMS','VTX WIRE',IWIR,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','VTX WIRE END',1,IEND,CVAL,
     &       TYP,REM,IER)
        WRITE (PROMPT,140) IEND
  140   FORMAT(' VTX Wire end (0-1) ? [',I1,']>')
   50   CONTINUE
CC        CALL PURSTR(PROMPT,STRING,LENGTH)
        CALL GETPAR(1,PROMPT,'U',STRING)
        CALL SWORDS(STRING,II,JJ,LENGTH)

        IF ( LENGTH .GT. 0 ) THEN
          READ (STRING(1:LENGTH),*,ERR=700) END
        ELSE
          END = IEND
        ENDIF
        IF ( END .LT. 0 .OR. END .GT. 1 ) THEN
          NUMTRY = NUMTRY - 1
          WRITE (MESSAG,150) NUMTRY
          CALL PUMESS(MESSAG)
          IF ( NUMTRY .GT. 0 ) GOTO 50
          CALL JENBAT            ! End batch of updates (clear screen)
          GOTO 900
        ENDIF
        CALL EZ_SET_ARRAY('PXPARAMS','VTX WIRE END',END,IER)
      ENDIF
C
C ****  Call the correct routine for the plot type
C
      CALL JENBAT                       ! Do all purges
      IF ( PLTYPE .EQ. 1 ) CALL PVFSEC(DETTYP) ! Full sector display
      IF ( PLTYPE .EQ. 2 ) CALL PV1ADC(DETTYP) ! one wire channel
      GOTO 900
C
C ****  In case of read error, allow two retries to input numbers
C
  700 CONTINUE
      NUMTRY = NUMTRY - 1
      WRITE (MESSAG,150) NUMTRY
  150 FORMAT(' Input error, ',I1,' tries remaining')
      CALL PUMESS(MESSAG)
      IF ( NUMTRY .GT. 0 ) GOTO 1
      CALL JENBAT            ! End batch of updates (clear screen)
C-----------------------------------------------------------------------
C
C ****  Reseting RCP file
C
  900 CALL EZRSET
  999 CONTINUE
      RETURN
      END
