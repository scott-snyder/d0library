      FUNCTION CFRAME_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize Calorimeter Frame: Program builder
C-                         version.
C-
C-                         Read in calorimeter control file and calori-
C-                         meter geometry. The control file is given the
C-                         name `CALFRAME_RCP'. The logical name:
C-
C-                            CALFRAME_RCP
C-
C-                         should be DEFINEd to be the name of the
C-                         required RCP control file. This program
C-                         contains the Package ON/OFF switches DO_xxxx.
C-
C-                         NOTE: INZCOM(2) and INZSTP MUST be called before
C-                         calling CFRAME_INI
C-
C-   Returned value  : True if no error
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-JUN-1989   Rajendran Raja
C-   Updated  21-SEP-1989   Chip Stewart
C-   COMBINES FUNCTIONS OF CALINI AND CFRINI.
C-   Updated  28-SEP-1989   Harrison B. Prosper
C-   Call PBDINI here rather than in CBEGIN
C-   Updated  12-OCT-1989   Harrison B. Prosper
C-   Moved calor package code to CALOR_INI.FOR
C-   Updated  16-NOV-1989   Boaz Klima
C-   Added XMODE to EVOPWO argument list
C-   Updated  11-DEC-1989   Harrison B. Prosper
C-      Use new RCP routine EZGETS
C-   Updated  17-SEP-1990   Harrison B. Prosper
C-      Add call to INRCPE
C-   Updated  21-SEP-1990   K. Wyatt Merritt
C-      Add booking of flags DUMP_THIS_EVENT,WRITE_THIS_EVENT,WRITE_STREAMS
C-   Updated   8-APR-1991   Scott Snyder
C-       Add EZRSET call.
C-   Updated   9-DEC-1991   Harrison B. Prosper
C-      Add call to DO_HBOOK_OPEN
C-   Updated  12-MAR-1992   Harrison B. Prosper
C-   Updated  22-APR-1992   Harrison B. Prosper
C-      Go back to previous version for now
C-   Updated   1-MAY-1992   Harrison B. Prosper
C-      If PIXIE package is off then turn off ALL display packages
C-   Updated   2-JUN-1992   Harrison B. Prosper
C-      Get Output mode from RCP-file
C-   Updated  23-SEP-1993   Rajendran Raja  Add error handling for VAX 
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CFRAME_INI
      LOGICAL CALDDF
      LOGICAL PBD_GET_NEXT_FLAG
      LOGICAL OK,OK1,TURN_OFF_DISPLAYS,MORE,ON
      LOGICAL LSTAND,LDST,LHBOOK,WRITE_STREAMS
C
      INTEGER ILEN,INUM,IPT,ERROR(100)
      LOGICAL LERR(100)
      LOGICAL CONTIN,COUNT,TYPE,LOG
      INTEGER ERR_NUM,MAX
      EQUIVALENCE (LERR,ERROR)
C
      INTEGER LENGTH,IER,I,J,K,II
C
      CHARACTER*2 XMODE
      CHARACTER*80 FILNAM
      CHARACTER*32 PACKAGE
      CHARACTER*(*) RCP_FRAME, RCP_EDIT
C
      PARAMETER( RCP_FRAME = 'CALFRAME_RCP' )   ! Logical name of control file
      PARAMETER( RCP_EDIT  = 'CALFRAME_RCPE' )
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST  /.TRUE./
C----------------------------------------------------------------------
C
      IF( .NOT. FIRST ) GOTO 999  ! make sure CFRAME_INI is executed only once
      FIRST = .FALSE.
C
C **** Book flag to allow single event dump request from user
C
      CALL FLGBK('DUMP_THIS_EVENT',1)
C
C **** Book flags controlling event writing
C
      CALL FLGBK('WRITE_THIS_EVENT',1)
      CALL FLGBK('WRITE_STREAMS',1)
C
C ****  Read frame parameters into an SRCP bank
C
      CALL INRCP (RCP_FRAME,IER)
      OK = IER .EQ. 0
C
      IF ( OK ) THEN
C
C ****  Apply RCP edit file to RCP bank
C
        CALL INRCPE (RCP_EDIT,IER)
        IF ( IER .EQ. 0 ) THEN
          CALL ERRMSG('CALORIMETER','CFRAME_INI',
     &      'RCP edit file has been applied','S')
        ENDIF
C
C ****  Pick CALFRAME RCP bank
C
        CALL EZPICK(RCP_FRAME)
C
C ****  Set up flags for event writing
C
        CALL EZGET('STAND_OUT',LSTAND,IER)
        CALL EZGET('DST_OUT',LDST,IER)
C
        WRITE_STREAMS = LSTAND .OR. LDST
        CALL SET_WRITE_DEFAULT(WRITE_STREAMS)
C
        CALL FLGSET('WRITE_STREAMS',WRITE_STREAMS)
        CALL FLGSET('WRITE_THIS_EVENT',WRITE_STREAMS)
C
C ****  Set up STA OUTPUT file
C
        IF( LSTAND ) THEN
          CALL EZGETS('STAND_OUT_FILE',1,FILNAM,LENGTH,IER)
          CALL EZ_STORE_NAME('STANDARD_OUTPUT',FILNAM(1:LENGTH),IER)
C
C ****  Get output mode
C
          CALL EZGETS('STAND_OUT_FILE',2,XMODE,LENGTH,IER)
          IF ( IER .EQ. 0 ) THEN
            XMODE = XMODE(1:LENGTH)
          ELSE
            XMODE = ' '   !Native mode
          ENDIF
C
C ****  Open file
C
          CALL EVOPWO('STA',FILNAM,XMODE,OK1)  ! OPEN STANDARD OUTPUT FILE
          OK = OK.AND.OK1
        ENDIF
C
C ****  Set up DST OUTPUT file
C
        IF( LDST ) THEN
          CALL EZGETS('DST_OUT_FILE',1,FILNAM,LENGTH,IER)
          CALL EZ_STORE_NAME('DST_OUTPUT',FILNAM(1:LENGTH),IER)
C
          CALL EZGETS('DST_OUT_FILE',2,XMODE,LENGTH,IER)
          IF ( IER .EQ. 0 ) THEN
            XMODE = XMODE(1:LENGTH)
          ELSE
            XMODE = ' '   !Native mode
          ENDIF
C
          CALL EVOPWO('DST',FILNAM,XMODE,OK1)    ! OPEN DST OUTPUT FILE
          OK = OK.AND.OK1
        ENDIF
C
C ****  INITIALIZE PROGRAM BUILDER switches
C
        CALL PBDINI
C
C ****  OVERWRITE PBDINI SWITCHES USING DO_XXXX SWITCHES
C ****  IN CALFRAME_RCP FILE
C
        CALL SET_SWITCHES (RCP_FRAME,'DO_',' ')
C
C ****  IF PIXIE package is OFF then turn off ALL pixie display packages.
C ****  Display packages are named xxxxxDIS
C
        II = 1
        TURN_OFF_DISPLAYS = .FALSE.
        MORE = PBD_GET_NEXT_FLAG(PACKAGE,ON,II)
        DO WHILE ( MORE )
          MORE = PBD_GET_NEXT_FLAG(PACKAGE,ON,II)
          IF ( MORE ) THEN
            IF ( PACKAGE(1:5) .EQ. 'PIXIE' ) THEN
              IF ( ON ) THEN
                MORE = .FALSE.              !PIXIE is ON so exit
              ELSE
                TURN_OFF_DISPLAYS = .TRUE.  !PIXIE is OFF so turn off displays
              ENDIF
            ELSE
              IF ( TURN_OFF_DISPLAYS ) THEN
                CALL WORD(PACKAGE,I,J,K)
                IF ( PACKAGE(J-2:J) .EQ. 'DIS' ) THEN
                  CALL PBD_SET_FLAG(PACKAGE,.FALSE.)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
C ****  Open HBOOK RZ file
C
C      CALL EZGET('HBOOK_SAVE',LHBOOK,IER)
C
C      IF ( LHBOOK ) THEN
C        CALL DO_HBOOK_OPEN('HBOOK_FILE',IER)
C        IF ( IER.NE.0 ) THEN
C          CALL ERRMSG('NO_HBOOK_FILE_NAME','CFRAME_INI',
C     &    ' HBOOK_FILE name NOT FOUND','W')
C        ENDIF
C      ENDIF
C
C ****  Initialize Dump files
C
        CALL CPRTST
C
C
C ****  do error handling in program
C
        CALL EZGETA('ERROR_HANDLING',0,0,0,ILEN,IER)
        CALL EZGETA('ERROR_HANDLING',1,ILEN,1,ERROR,IER)
        INUM = ILEN/6
        IPT = 1
        DO I = 1 , INUM
          ERR_NUM = ERROR(IPT)
          CONTIN = LERR(IPT+1)
          COUNT = LERR(IPT+2)
          TYPE = LERR(IPT+3)
          LOG = LERR(IPT+4)
          MAX = ERROR(IPT+5)
          IPT = IPT + 6
          CALL ERRSET(ERR_NUM,CONTIN,COUNT,TYPE,LOG,MAX)
        ENDDO
C
        CALL EZRSET
      END IF
C
      CFRAME_INI = OK
  999 RETURN
      END
