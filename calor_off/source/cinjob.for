      SUBROUTINE CINJOB (FILNAM,NEVMAX,QUIT,COMMAND,CONTROL,NCONT,
     &  NEVENT,XOPT,WT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return data filename, number of events to
C-                         process and read in the calibration file.
C-                         NEVENT is zeroed by CINJOB.
C-
C-   Inputs  : None
C-   Outputs : FILNAM      Filename of data input file
C-             NEVMAX      Number of events to process; if zero process
C-                         all events in file.
C-             QUIT        If TRUE then no more files are to be processed
C-             COMMAND(*)       [C*]    Skip/Proc command
C-             CONTROL(*)       [I]     Skip/Proc event control
C-             NCONT            [I]     Number of control words
C-             NEVENT           [I]     Number of events read
C-             XOPT             [C*]    Input mode
C-             WT               [R]     Weight associated with file
C-                                      if unspecified =1.0
C-
C-   Controls:
C-
C-   Created  23-JAN-1989   Harrison B. Prosper, John Womersley
C-   Updated  11-DEC-1989   Harrison B. Prosper
C-      Adapt to new RCP string format
C-   Updated  26-JAN-1990   Harrison B. Prosper
C-      Add data file list feature and Skip/Read control
C-   Updated   8-AUG-1990   Harrison B. Prosper
C-      Fixed bug in processing of DATAFILE_LIST;
C-      Also generalize so that one can use data file lists
C-      produced with dir/col=1/nohead/notrail
C-   Updated   5-NOV-1991   Krzysztof L. Genser
C-       to handle FATMEN long generic names
C-   Updated  19-FEB-1992   Chip Stewart
C-       IF NEVMAX negative and (NEVENT.GE.-NEVMAX) then goto next run
C-   Updated   1-MAY-1992   Harrison B. Prosper
C-       Handle new DATAFILES format (no calibration file)
C-   Updated  19-MAY-1992   Harrison B. Prosper
C-       Add XOPT option
C-   Updated  15-JAN-1993   W. Dharmaratna,Harrison B. Prosper
C-                         Fix SKIP/PROC logic
C-   Updated   4-MAY-1993   Rajendran Raja  Added WT to the argument
C-   Updated  24-NOV-1995   Rajendran Raja  made PROC/EVENT applicable to all
C-                          the files in Datafile_list if NEVMAX<0.0
C-        
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FILE_NAMES.INC'
      INCLUDE 'D0$PARAMS:CALID.DEF'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      CHARACTER*(*) FILNAM
      INTEGER       NEVMAX
      LOGICAL       QUIT
      CHARACTER*(*) COMMAND
      CHARACTER*80  COMMAND_SAVE
      INTEGER       CONTROL(*)
      INTEGER       NCONT, NCONT_SAVE
      INTEGER       NEVENT
      CHARACTER*(*) XOPT
      CHARACTER*32  STRING
      REAL    WT

C
      INTEGER IPBASE
      INTEGER IPNUMB
      INTEGER IPEVEN
      INTEGER IPFILE
      INTEGER IPCALI
      INTEGER NWORDS
      INTEGER JJFILE,KKFILE,NFILES,LENGTH,IER,TOTAL,LUN,LDIR
      INTEGER II,JJ,KK,LL,I,I1,J1,K1
      CHARACTER*255 DIRECTORY,RECORD,RECORD1,FILE_NAME
      CHARACTER*255 CFILNM
      INTEGER MAXSIZE
      PARAMETER( MAXSIZE = 2000 )
      INTEGER IFILE(MAXSIZE),ITYPE(MAXSIZE)
      REAL    RFILE(MAXSIZE)
      EQUIVALENCE (IFILE,RFILE)
C
      LOGICAL FIRST,OPENED,ACTIVE,OK
      INTEGER IW,JW,LW,WTYPE
      INTEGER IT,JT,NT
      REAL    VALUE
C----------------------------------------------------------------------
      SAVE IPBASE,JJFILE,NFILES,IFILE,TOTAL,NCONT_SAVE,COMMAND_SAVE
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        QUIT  = .FALSE.
        OPENED= .FALSE.
C
C ****  Get a unit number
C
        CALL GTUNIT (CALID,LUN,IER) ! Get a unit number
C
C ****  Get some information about the format of the DATA_FILES structure
C
        CALL EZPICK ('CALFRAME_RCP')
        CALL EZGET_VALUE_TYPE ('DATA_FILES',IFILE,ITYPE,TOTAL,IER)
        CALL EZRSET
C
        NFILES = IFILE(1)               ! Number of files to process
        IPBASE = 2                      ! Base Pointer to DATA_FILES sub-struc.
        JJFILE = 0                      ! Zero file counter
        FILE_NAME = ' '                 ! Clear file-name
      ENDIF
C
      NCONT = 0
      COMMAND = ' '
C
C ****  Check for keyword LIST
C
  100 CONTINUE
      IF ( INDEX(FILE_NAME,'LIST') .EQ. 0 ) THEN
C
        JJFILE = JJFILE + 1               ! Increment file counter
        IF ((JJFILE .LE. NFILES) .AND. (IPBASE .LT. TOTAL)) THEN
C
C ****  Get number of events to process
C
          NEVMAX = IFILE(IPBASE)
          IPBASE = IPBASE + 1
C
C ****  Get data file name
C
          CALL EZGETC1 (IFILE,ITYPE,IPBASE,FILNAM,LENGTH)
          FILNAM = FILNAM(1:LENGTH)
          FILE_NAME = FILNAM              ! Note name
C
          IF ( IPBASE .LE. TOTAL ) THEN
C
C ****  Check if next record is a CHARACTER
C ****  If it is of CHARACTER type then check for SKIP/PROC commands
C ****  or 'X' or ' '
C
            IF ( ITYPE(IPBASE) .GE. VTCHR ) THEN
C
C ****  Check for Skip/Process command
C
              CALL EZGETC1 (IFILE,ITYPE,IPBASE,STRING,LENGTH)
              STRING = STRING(1:LENGTH)
              CALL UPCASE (STRING(1:LENGTH),STRING(1:LENGTH))
              CALL WORD(STRING,I1,J1,K1)
C
              IF ( STRING(1:4) .EQ. 'SKIP' .OR.
     &             STRING(1:4) .EQ. 'PROC' ) THEN
C
C ****  Check next word; it should be an INTEGER
C
                IF ( ITYPE(IPBASE) .EQ. VTINT ) THEN
                  COMMAND = STRING
                  NCONT = 1
                  CONTROL(NCONT) = IFILE(IPBASE)
                  IPBASE = IPBASE + 1
                  DO WHILE ( (IPBASE .LE. TOTAL ) .AND.
     &                       (ITYPE(IPBASE) .EQ. VTINT) )
                    NCONT = NCONT + 1
                    CONTROL(NCONT) = IFILE(IPBASE)
                    IPBASE = IPBASE + 1
                  ENDDO
                  NCONT_SAVE = NCONT
                  COMMAND_SAVE = COMMAND
                ELSE
                  CALL ERRMSG('BAD_SYNTAX_IN_DATAFILES',
     &              'CINJOB','SKIP/PROC command without argument','W')
                  IPBASE = IPBASE + 1
                  NCONT  = 0                   ! No control words
                  COMMAND = ' '
                ENDIF
C
C ****  Next variable MUST be Input specifier
C
                CALL EZGETC1 (IFILE,ITYPE,IPBASE,STRING,LENGTH)
                STRING = STRING(1:LENGTH)
                CALL UPCASE (STRING(1:LENGTH),STRING(1:LENGTH))
                CALL WORD(STRING,I1,J1,K1)
              ENDIF
C
C ****  Must be Input mode specifier
C
              IF ( (STRING(I1:J1) .EQ. ' ') .OR.
     &                 (INDEX(STRING(I1:J1),'N').GT.0) ) THEN
                XOPT = ' '   !NATIVE MODE
                IF( INDEX(STRING(1:2),'T').GT.0) XOPT = 'T'  !TAPE (NO XCHKR)
              ELSEIF ( (STRING(I1:J1) .EQ. 'EX') .OR.
     &                 (INDEX(STRING(I1:J1),'X').GT.0) ) THEN
                XOPT = 'X'   !EXCHANGE MODE
                IF( INDEX(STRING(I1:J1),'T').GT.0) XOPT = 'XT' !TAPE (NO XCHKR)
              ELSE
                XOPT = STRING(I1:J1) ! ?
              ENDIF
C
C ****  NOW CHECK FOR WEIGHT specification if any.
C
              CALL EZGETC1 (IFILE,ITYPE,IPBASE,STRING,LENGTH)
              STRING = STRING(1:LENGTH)
              CALL UPCASE (STRING(1:LENGTH),STRING(1:LENGTH))
              CALL WORD(STRING,I1,J1,K1)
              IF ( STRING(I1:J1).EQ.'WEIGHT' ) THEN
C NEXT WORD MUST BE THE WEIGHT 
                WT = RFILE(IPBASE)
                IPBASE = IPBASE + 1
              ELSE
C
C ****  WEIGHT NOT SPECIFIED
C
                IPBASE = IPBASE - 1
              ENDIF
            ENDIF
          ENDIF
        ELSE
C
C ****  End-of-Job
C
          FILNAM = ' '
          NEVMAX = 0
          QUIT   = .TRUE.
          IF ( OPENED ) THEN
            CLOSE(UNIT=LUN)
          ENDIF
          GOTO 999
        ENDIF
      ENDIF
C
C ****  If this is a file-list then read next file name from list
C
      IF ( FILE_NAME(1:13) .EQ. 'DATAFILE_LIST' ) THEN

        IF ( .NOT. OPENED ) THEN
C
          CALL D0OPEN (LUN,FILE_NAME,'FI',OK)
C
C ****  Get directory containing data files
C
          IF ( OK ) THEN
            LDIR = 0                    ! Length of directory name
            DIRECTORY = ' '
C
            ACTIVE = .TRUE.
            DO WHILE (ACTIVE)
              READ(LUN,FMT='(A)',END=200) RECORD
              CALL UPCASE(RECORD,RECORD)
C
              IF ( INDEX(RECORD,'DIRECTORY') .GT. 0 ) THEN
                CALL WORD(RECORD,II,JJ,LL)     ! Remove word DIRECTORY
                RECORD = RECORD(JJ+1:)
                CALL WORD(RECORD,II,JJ,LDIR)   ! Extract directory name
                DIRECTORY = RECORD(II:JJ)      ! Get directory name
                GOTO 250
              ENDIF
            ENDDO
  200       CONTINUE
C
C ****  Directory not found; assume that this data file list
C ****  was created with DIR/NOHEAD/NOTRAIL. Re-open file.
C
            CLOSE(UNIT=LUN)
            CALL D0OPEN (LUN,FILE_NAME,'FI',OK)
          ELSE
            CALL ERRMSG('CALOR_OFF','CINJOB',
     &        'Data file list NOT found','F')
          ENDIF
C
  250     CONTINUE
          OPENED = .TRUE.               ! Data-file list is OPEN
        ENDIF
C
C ****  Get next file-name unless number of events read is
C ****  greater than NEVMAX, in which case close list file
C ****  and go get next datafile to read.
C
        IF ( ((NEVENT .LT. NEVMAX) .AND. OPENED )
     &    .OR. ( (NEVMAX.LT.0) .AND. OPENED ) ) THEN
C
          ACTIVE = .TRUE.
          DO WHILE (ACTIVE)
            READ(LUN,FMT='(A)',END=300) RECORD
            CALL UPCASE(RECORD,RECORD)
            RECORD1 = RECORD
            CALL SWORDS(RECORD1,IT,JT,NT)
            CALL WORD(RECORD,II,JJ,LL)
C
C ****  Skip blank lines and comment lines etc.
C
            IF ( (LL .GT. 0)              .AND.
     &           (RECORD(II:II) .NE. '!') .AND.
     &           (RECORD(II:JJ+1) .NE. 'TOTAL ') ) THEN
              ACTIVE = .FALSE.
              RECORD = RECORD(II:JJ)
C
C ****  Remove version number
C
              I = INDEX(RECORD(1:LL),';')
              IF ( I .GT. 0 ) THEN
                LL = I - 1  ! Reduce length
              ENDIF
C
C ****  Prefix file-name with directory name if directory
C ****  name was found in the previous scan of the datafile
C ****  list; otherwise use the file name as is.
C
              IF ( LDIR .GT. 0 ) THEN
                FILNAM = DIRECTORY(1:LDIR)//RECORD(1:LL)
              ELSE
                FILNAM = RECORD(1:LL)
              ENDIF
C
C ****  got the filename. See if WEIGHT specified as next word.
C
              WT = 1.0
              IF ( JJ+1.LE.JT) THEN
                CALL WORD(RECORD1(JJ+1:JT),IW,JW,LW)
                IF ( LW.GT.0 ) THEN
C some string present
                  WT = VALUE(RECORD1(JJ+1:JT),IW,LW,WTYPE)
                  IF ( WTYPE.GT.3 ) THEN
                    WT = 1.0
C Not a real number or integer.
                  ENDIF
                ENDIF
              ENDIF
              IF ( (NEVMAX.LT.0) .AND. OPENED ) THEN
                NEVENT = 0
                NCONT = NCONT_SAVE
                COMMAND = COMMAND_SAVE
              ENDIF
              GOTO 999
            ENDIF
          ENDDO
C
        ENDIF
C
C ****  End-of-list-file; Get next list-file or data-file
C
  300   CONTINUE
        CLOSE(UNIT=LUN)
        FILE_NAME = ' '
        OPENED = .FALSE.
        NEVENT = 0
        GOTO 100
      ELSE
C
C ****  This is not a datafile-list
C
        NEVENT = 0                      ! Zero event counter
      ENDIF
C
  999 RETURN
      END
