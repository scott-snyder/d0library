      FUNCTION XYVERT_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do initialization for XYVERT package
C-
C-
C-   Created  10-OCT-1992   Alexandre Zinchenko
C-   Updated  16-DEC-1992   A. Zinchenko - correct logical name of RCPE-file
C-   Updated  17-JAN-1993   A. Zinchenko - change directory name for RZ-file
C-   Updated  01-MAR-1993   A. Zinchenko - add 6th variable in Ntuple
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IER, LUN
      LOGICAL XYVERT_INI, OK, IDOVTX, VTRINI
      LOGICAL EZERROR
      CHARACTER*10 RCPFIL
      PARAMETER ( RCPFIL = 'XYVERT_RCP' )
      CHARACTER*8 VARS(6)
      DATA VARS /'X', 'Y', 'CHI2', 'PHI', 'ZVERT', 'NVERT'/
C----------------------------------------------------------------------
      XYVERT_INI = .TRUE.
      CALL INRCP(RCPFIL,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('INRCP ERROR','XYVERT_INI',
     &    'Attempt to read XYVERT_RCP failed','F')
        XYVERT_INI = .FALSE.
      ENDIF
      CALL EZPICK('XYVERT_RCP',IER)
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('XYVERT','XYVERT_INI',
     &    'Unable to find bank XYVERT_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGET('DO_VTX_TRACK',IDOVTX,IER)
      CALL EZRSET
      IF (IDOVTX) OK = VTRINI()
      CALL INRCPE('XYVERTE_RCP',IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('INRCPE ERROR','XYVERT_INI',
     &    'Attempt to read XYVERTE_RCP failed','F')
        XYVERT_INI = .FALSE.
      ENDIF
      XYVERT_INI = OK
C
C ****  Open RZ-file
C
C      CALL DHDIR(RCPFIL,'HBOOK_DIRECTORY',IER,' ')
C      CALL GTUNIT(600,LUN,IER)
C      CALL D0RZOPEN(LUN,'XYVERT.HST','UO',4096,OK)
C      XYVERT_INI = XYVERT_INI .AND. OK
C      CALL HRFILE(LUN,'AAABBB','N')
C
C ****  Book histograms here . . .
C
C-    Book Ntuple for vertex parameters.
Cc      CALL DHDIR(RCPFIL,'HBOOK_DIRECTORY',IER,' ')
C      CALL HBOOKN(1,'LAYER 0 SEGMENTS',6,'AAABBB',1000,VARS)
C      CALL HBOOKN(2,'LAYER 1 SEGMENTS',6,'AAABBB',1000,VARS)
C      CALL HBOOKN(3,'LAYER 2 SEGMENTS',6,'AAABBB',1000,VARS)
C      CALL HBOOKN(4,'TRACKS          ',6,'AAABBB',1000,VARS)
C----------------------------------------------------------------------
  999 RETURN
      END
