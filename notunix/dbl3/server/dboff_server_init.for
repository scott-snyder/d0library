      SUBROUTINE DBOFF_SERVER_INIT(DBFILE1,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DBL3 initialization
C-
C-   Inputs  :
C-              DBFILE1 - DBL3 data base name.
C-
C-   Outputs :  IOK    - .TRUE. if no error
C-   Controls:
C-
C-   Created  06-AUG-1992   S. Abachi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DBFILE1
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:LKCALIB.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:D0DB_IDS.INC'
      INCLUDE 'D0$INC:DB_SRVR_NAMES.INC'
      INCLUDE 'D0$INC:DB_SRVR_PARAM.INC'
      CHARACTER*80 MSG_STRING
      CHARACTER*80 DBFILE
      CHARACTER*80 DBFILE2, LOCAL_DBFILE
      CHARACTER*2 CHOPT
      CHARACTER*1 COPT
      LOGICAL IOK,FIRST
      INTEGER L,IOS,I,DOLL,IENT,ERR,NIOS,CLEN,MRK
C&IF VAXVMS
C&ELSE
C&      INTEGER CONTEXT
C&      LOGICAL FOUND, LIB$FIND_FILE
C&ENDIF
      DATA IENT,FIRST/0,.TRUE./
C----------------------------------------------------------------------
C
      DBFILE = DBFILE1
      CALL UPCASE(DBFILE, DBFILE)
C
      IF(FIRST) THEN
        CALL GTUNIT(12,DBLUN,ERR)
        FIRST = .FALSE.
        CALL_DBEND = .FALSE.
      ENDIF
      NIOS = 0
C
      DBFILE2 = DBFILE
    5 CONTINUE
C
      MRK = INDEX(DBFILE2,']')
      IF(MRK .NE. 0) THEN
        DBFILE2 = DBFILE2(MRK+1:)
      ENDIF
C
      DOLL = INDEX(DBFILE2,'$')
      IF(DOLL .EQ. 0)  THEN
        WRITE(MSG_STRING,90)
   90   FORMAT(' DBCLB_INITIALIZE: Improper database file name ')
        CALL INTMSG(MSG_STRING)
        IOK = .FALSE.
        GOTO 999
      ENDIF
C
C- First exceptions:
C
      IF( DBFILE2(DOLL+1:DOLL+3) .EQ. 'GLB') THEN
        GOTO 50
      ENDIF
C
C- Now general situation:
C
      DO I=1,NUM_DB
        IF( DBFILE2(DOLL+1:DOLL+3) .EQ. DB_CURRENT_LNAME(1:3) ) THEN
          GOTO 50
        ENDIF
      ENDDO
C
      WRITE(MSG_STRING,90)
      CALL INTMSG(MSG_STRING)
      GOTO 999
C
   50 IF(IDVSTP.EQ.0)THEN
        CALL MZEBRA(0)
        CALL INZSTP
        CALL CONSTP
      ENDIF
C
      CALL MZBOOK(IDVSTP,L,L,2,'$RZ$',0,0,25000,2,-1)
      CALL MZDROP(IDVSTP,L,' ')
C
      IF (IENT.EQ.0) THEN
        CALL MZLINK(IDVSTP,'/DBSTP/',LTDIR,LKEYS(0),LDATA(255))
        CALL CLBLNK
        IENT = 1
      ENDIF
C
   20 CONTINUE
C&IF VAXVMS
      LOCAL_DBFILE = DBFILE
C&ELSE
C&      CONTEXT = 0
C&      FOUND = LIB$FIND_FILE(DBFILE, LOCAL_DBFILE, CONTEXT)
C&      CALL LIB$FIND_FILE_END(CONTEXT)
C&      IF(.NOT.FOUND .OR. LOCAL_DBFILE.EQ.' ')THEN
C&        IOS = 30
C&        GO TO 900
C&      ENDIF
C&ENDIF
      OPEN(UNIT=DBLUN,FILE=LOCAL_DBFILE,STATUS='OLD',SHARED,
     &       ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1024,IOSTAT=IOS)
C
  900 CONTINUE
C
      IF(IOS.NE.0)THEN
        IF(IOS .EQ. 30) THEN
          NIOS = NIOS + 1
          IF(NIOS .LT. 2) THEN
            WRITE(MSG_STRING,94) DBFILE,IOS
   94       FORMAT(' File ',A20, ' could not be accessed.'
     &                        ' Will try again. IOSTAT = ',I3)
            CALL INTMSG(MSG_STRING)
            GOTO 20
          ENDIF
        ENDIF
        WRITE(MSG_STRING,95)DBFILE,IOS
   95   FORMAT(' Error opening file ',A20,'  IOSTAT = ',I3)
        CALL ERRDB(MSG_STRING)
        CALL INTMSG(MSG_STRING)
        IOK = .FALSE.
        GOTO 999
      ENDIF
C
      CHOPT = 'SU'
      CALL DBINIT(IDVSTP,DBLUN,TOPD,LTDIR,0,CHOPT)
C
      IF (IQUEST(1).NE.0) THEN
        CALL ERRDB('DBINIT')
        IOK = .FALSE.
        CALL DBCLB_END
        CLOSE(UNIT=DBLUN)
        GO TO 999
      ENDIF
C
      CALL_DBEND = .TRUE.              ! Must call DBEND and set true
C
      CALL RZLDIR('//',' ')
      CALL DBLOGL(DBLUN,0)
      IOK = .TRUE.
C
   97 CONTINUE
C
  999 RETURN
      END
