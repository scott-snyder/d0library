      SUBROUTINE NTUPLE_FILE_OPEN (USER_ID,NEWFILE,FILENAME,
     &                             RECORD_LENGTH,TOP_DIRECTORY,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Open an RZ a file for logging/reading Ntuples,
C-   if it has not yet been opened.
C-
C-   Inputs  : USER_ID        [I]   User ID for GTUNIT
C-             NEWFILE        [L]   TRUE if a new file
C-             FILENAME       [C*]  Filename to store Ntuple
C-             RECORD_LENGTH  [I]   Record length (=8191 is suggested)
C-                                  or 0 for an existing file.
C-             TOP_DIRECTORY  [C*]  Arbitrary name assigned to top RZ
C-                                  directory.
C-
C-   Outputs : STATUS               0 --> OK
C-   Controls:
C-
C-   Created  23-JUN-1991   B.S.Acharya
C-   Updated   4-SEP-1991   Harrison B. Prosper
C-      Use HROPEN
C-   Updated   7-NOV-1991   Harrison B. Prosper
C-    fix counter bug
C-   Updated  14-NOV-1991   Harrison B. Prosper
C-    Restore directory
C-   Updated  27-NOV-1991   Harrison B. Prosper
C-      Make compatible with DHDIR
C-   Updated   2-DEC-1991   Harrison B. Prosper
C-   Updated   9-DEC-1991   Harrison B. Prosper
C-      Check if file already open
C-   Updated   2-JAN-1992   Harrison B. Prosper
C-      Fix book-keeping of top-dir
C-   Updated  13-FEB-1992   Marc Paterno  Set maximum number or records in RZ
C-   file to 65,000 (ZEBRA maximum limit).  Read record length from file if
C-   NEWFILE is false.
C-   Updated  17-JUN-1992   Harrison B. Prosper  
C-      Set default directory to //PAWC automatically using DHDIR 
C-   Updated  18-JUN-1992   Harrison B. Prosper  
C-      Cause HROPEN to figure out LREC when CHOPT='Q', (See PAW). 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER USER_ID
      LOGICAL NEWFILE
      CHARACTER*(*) FILENAME
      INTEGER RECORD_LENGTH
      CHARACTER*(*) TOP_DIRECTORY
C
      INTEGER STATUS
C----------------------------------------------------------------------
      LOGICAL NEW, FOUND,YES
      INTEGER IUNIT
      INTEGER I,II,IDX,LTOP,LFILE
      CHARACTER*2   CHOPT
      CHARACTER*80  REMARK,CURDIR
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:NT_HOUSEKEEP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C----------------------------------------------------------------------
      LOGICAL FIRST,MESSAGE_ON
      SAVE FIRST,MESSAGE_ON
      DATA FIRST      /.TRUE./
      DATA MESSAGE_ON /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        NFILES = 0
        CALL VZERO (COUNTER, MAXUNIT)
      ENDIF
C
      LREC   = RECORD_LENGTH
      LTOP   = LEN(TOP_DIRECTORY)
      LFILE  = LEN(FILENAME)
      STATUS = 0
C
C ****  Check if file already specified.
C
      IF ( NFILES .GT. 0 ) THEN
        CALL LOCSTR(TOP_DIRECTORY(1:LTOP),TOPDIR_LIST,NFILES,FOUND,II)
        IF ( FOUND ) THEN
          GOTO 999
        ENDIF
      ENDIF
C
      IUSER  = USER_ID
C
C
C ****  Option 'Q' allows resetting of maximum number of records in RZ output
C ****  file.  IQUEST(10) = n sets this maximum to n.
C
      IQUEST(10) = 65000                ! the maximum allowed by RZMAKE in
C                                       ! CNL201a (note the different value in
C                                       ! HRFILE).
      IF ( NEWFILE ) THEN
        CHOPT = 'NQ'                     ! open a new file
      ELSE
        CHOPT = 'Q'                      ! read only
C        INQUIRE (FILE=FILENAME(1:LFILE), RECL=LREC, IOSTAT=STATUS)
C        LREC = LREC / 4                 ! Convert from bytes to longwords
        LREC = 0                        ! This should cause HROPEN to get
                                        ! correct LREC.
        IF ( STATUS .NE. 0 )THEN
          REMARK = 'Cannot open file '//FILENAME(1:LFILE)
          CALL ERRMSG('BAD_OPEN','NTUPLE_FILE_OPEN',REMARK,'S')
          RETURN
        ENDIF
      ENDIF
C
C -- Fetch a logical unit to open file:
C
      CALL GTUNIT(IUSER,IUNIT,STATUS)
      IF( STATUS.NE.0 )THEN
        CALL ERRMSG('BAD_UNIT','NTUPLE_FILE_OPEN',
     &              'Could not get a unit number','S')
        RETURN
      ENDIF
C
C ****  Open RZ file
C
      STATUS = 0
      CALL HROPEN(IUNIT,TOP_DIRECTORY(1:LTOP),
     &            FILENAME(1:LFILE),CHOPT,LREC,STATUS)
      IF ( STATUS.NE.0 )THEN
        REMARK = 'Cannot open file '//FILENAME(1:LFILE)
        CALL ERRMSG('BAD_OPEN','NTUPLE_FILE_OPEN',REMARK,'S')
        GOTO 999
      ENDIF
C
C ****  Add to internal lists
C
      CALL LOCSTR1
     &  (TOP_DIRECTORY(1:LTOP),TOPDIR_LIST,IDLIST,NFILES,NEW,II)
      IDX = IDLIST(II)
      UNIT_LIST(IDX)  = IUNIT
      NEW_FILE(IDX)   = NEWFILE
      FILE_LIST(IDX)  = FILENAME(1:LFILE)
      COUNTER(IDX)    = 0
C
C ****  Display directory
C
      IF ( MESSAGE_ON ) THEN
        CALL HCDIR(CURDIR,'R')
        REMARK = ' ----> Opened HBOOK File  :   '//FILENAME
        CALL INTMSG(REMARK)
        REMARK = '       with top-directory : '//CURDIR
        CALL INTMSG(REMARK)
        CALL INTMSG(' ')
      ENDIF
C
C ****  Declare this topdir to DHDIR and set the directory to //PAWC
C
C
C ****  the following two commneted lines seem to be part of the problem
C ****  R.Raja 27-Dec-1993
C
C      CALL DHDIR_DECLARE_FILE(TOP_DIRECTORY(1:LTOP))
C      CALL DHSETDIR('//PAWC',STATUS)
      RETURN
C
      ENTRY NTUPLE_FILE_OPEN_MESSAGE(YES)
      MESSAGE_ON = YES
  999 RETURN
      END
