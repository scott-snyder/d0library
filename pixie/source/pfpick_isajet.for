      SUBROUTINE PFPICK_ISAJET(TRKNUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Requests which FDC ISAJET track the user wants 
C-                         to display.
C-
C-   Inputs  : none
C-   Outputs : TRKNUM = Track number
C-
C-   Created   7-FEB-1992   Robert E. Avery
C-   Updated  17-FEB-1992   Robert E. Avery  If hardcopy, don't ask questions.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C Output:
      INTEGER TRKNUM
C
C Local:
      INTEGER RUNSAV, IDSAV, RUN, ID
      INTEGER LFITR, GZFITR
      INTEGER ITRK, NTRK
      INTEGER LEN,IER,II,JJ
      INTEGER HALF
      INTEGER ISA_ID 
      INTEGER LAST_TRKNUM 
C
      REAL    TAN_THETA
      REAL    TRKDAT(9)
C
      CHARACTER*60 PROM
      CHARACTER*80 STRING
      CHARACTER*112 FTEXT
      CHARACTER*1 CHALF(0:1)
C
      LOGICAL EZERROR
      LOGICAL PRINT_LIST, PRINT_LIST_RCP
      LOGICAL FLGVAL,HARDCOPY 
C
      DATA PROM/' Enter Track Number (default prev or 1 )>'/
      DATA LAST_TRKNUM /1/
      DATA CHALF/'N','S'/
C
C----------------------------------------------------------------------
C
      HARDCOPY = FLGVAL('HARDCOPY')
      IF ( HARDCOPY ) THEN         
        TRKNUM = LAST_TRKNUM
        GOTO 999
      ENDIF
C
      LFITR = GZFITR()
      IF ( LFITR .LE. 0 ) THEN
        TRKNUM = 0
        CALL INTMSG(' No FITR bank present.')
        GOTO 999
      ENDIF
      NTRK = IQ( LFITR + 1 )
      IF ( NTRK .LE. 0 ) THEN
        TRKNUM = 0
        CALL INTMSG(' No FDC ISAJET tracks available.')
        GOTO 999
      ENDIF
C
C  Decide whether or not to print full list:
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFPICK_ISAJET','Cannot find PX_FDCDIS_RCP',
     &       'W')
        GOTO 999
      ENDIF
      CALL PUGETV('FDC PRINT LIST',PRINT_LIST_RCP)
      CALL EZRSET
C
      IF ( PRINT_LIST_RCP ) THEN
        PRINT_LIST = .TRUE.
      ELSE
        CALL EVNTID(RUN,ID)
        IF (RUN.NE.RUNSAV .OR. ID.NE.IDSAV) THEN
          RUNSAV=RUN
          IDSAV=ID
          PRINT_LIST = .TRUE.
        ELSE
          PRINT_LIST = .FALSE.
        ENDIF
      ENDIF
C
C  Display listing of tracks.
C
      IF ( PRINT_LIST ) THEN
        CALL INTMSG(' ')
        FTEXT = '     Trknum  ISA-ID    Half  P-ISA(GEV)'
        CALL INTMSG(FTEXT)
C
        DO ITRK=  1, NTRK
          CALL GTFITR(ITRK, TRKDAT)
          ISA_ID = TRKDAT(8)
          TAN_THETA = TAN(TRKDAT(5))
          IF ( TAN_THETA .GT. 0 ) THEN
            HALF = 1
          ELSE
            HALF = 0
          ENDIF
C
          WRITE (FTEXT,102) ITRK,ISA_ID,HALF, TRKDAT(6)
  102     FORMAT(1X,3I8,F10.3)
          CALL INTMSG(FTEXT)
        ENDDO
      ENDIF
C
C  Make choice of track to display
C
      CALL OUTMSG('1')
      IF ( NTRK.EQ.1) THEN
        TRKNUM = 1
      ELSE
        CALL OUTMSG(' Choose an FDC Isajet Track to Display ')
        STRING=' '
        LEN=0
        CALL GETPAR(1,PROM,'U',STRING)
        CALL SWORDS(STRING,II,JJ,LEN)
        IF ( LEN .NE. 0 ) THEN
          READ(STRING(1:LEN),*,ERR=980) TRKNUM
        ELSE
          TRKNUM = LAST_TRKNUM 
        END IF
        IF (TRKNUM.LT.1 .OR. TRKNUM.GT.NTRK) THEN
  980     CONTINUE
          TRKNUM = 1
          CALL OUTMSG(' Choice outside limits, use default value of 1.')
        END IF
      ENDIF
      WRITE(FTEXT,100) TRKNUM
  100 FORMAT('  Isajet track number chosen is ',I4,'  ')
      CALL OUTMSG(FTEXT)
      LAST_TRKNUM = TRKNUM 
C
      GOTO 999
C
C----------------------------------------------------------------------
C
  999 RETURN
      END
