      SUBROUTINE D0HINDEX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : List and plot histograms in the
C-   current directory.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls:
C-
C-   Created  22-APR-1992   Harrison B. Prosper
C-   Updated   6-MAY-1992   Harrison B. Prosper
C-      Use LISBOX
C-   Updated   7-MAY-1992   Harrison B. Prosper
C-      Bug fix
C-   Updated   8-MAY-1992   Harrison B. Prosper  
C-      Add entry point D0HINDEX_REFRESH 
C-   Updated   9-MAY-1992   Susan K. Blessing  Compare new directory name
C-    with previous directory name.
C-   Updated  11-MAY-1992   Harrison B. Prosper  
C-      Provide option to save listing file 
C-   Updated   5-JUN-1992   Harrison B. Prosper  
C-      Call PFLABL to label PF keys 
C-   Updated  21-JUN-1992   Harrison B. Prosper  
C-      Change title 
C-   Updated  20-JUL-1992   Boaz Klima, Chip Stewart
C       Global Title NEWDIR
C-   Modified 18-AUG-1992   sss - compile on ibm
C-   Updateda 24-AUG-1992   S. Hagopian, allow for user header
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MAXRECORD,ROW,COL,MAXDIR
      PARAMETER( MAXRECORD = 1000 )
      PARAMETER( ROW = 5)
      PARAMETER( COL = 2)
      PARAMETER( MAXDIR =  100 )
      CHARACTER*(*) D0HINDEX_FILE
      PARAMETER( D0HINDEX_FILE = 'SYS$LOGIN:D0HINDEX' )
C
      CHARACTER*80 LASTDIR,DIRLIST(MAXDIR),SAVEDIR,NEWDIR
      CHARACTER*76 RECORD,REMARK,BUFFER(MAXRECORD)
      REAL    VALUE
      INTEGER I, J, K, ID, N, NRECORD, COUNT, HID, II
      INTEGER HISTID(MAXRECORD),IDIR(MAXRECORD), NDIR, JDIR
      LOGICAL OK, ACTIVE, REFRESH, FLAG, KEEP
      LOGICAL FLGVAL
C----------------------------------------------------------------------
      SAVE REFRESH, HISTID, IDIR, DIRLIST, BUFFER, NDIR, NRECORD
      DATA REFRESH  /.TRUE./
      DATA SAVEDIR  /' '/
      DATA KEEP     /.FALSE./
C----------------------------------------------------------------------
C
C ****  Check whether to keep file
C
      CALL TRNLNM('D0HINDEX$KEEP',REMARK,J)
      KEEP = REMARK(1:1) .EQ. 'Y'
C
C ****  Get current directory
C
      CALL HCDIR(NEWDIR,'R')
      CALL INTMSG(' ')
      REMARK = ' The Current Directory is '//NEWDIR
      CALL INTMSG(REMARK)
C
C **** Set  Global Title NEWDIR if no user header
C
      IF(.NOT.FLGVAL('FLG_USERHEAD'))THEN
        REMARK = NEWDIR//' Histograms$'
        CALL HTITLE(REMARK)
      ENDIF
C
      IF (NEWDIR.NE.SAVEDIR) THEN
        REFRESH = .TRUE.
        SAVEDIR = NEWDIR
      END IF
C
C ****  Direct I/O to a file
C
      IF ( REFRESH ) THEN
C
        CALL D0OPEN(6,D0HINDEX_FILE,'OF',OK)
        IF ( .NOT. OK ) GOTO 1000
C
C ****  List IDs
C
        CALL HLDIR(SAVEDIR,'T')
        CLOSE(UNIT=6)
C
C ****  Read in file
C
        CALL D0OPEN(80,D0HINDEX_FILE,'IF',OK)
        IF ( .NOT. OK ) GOTO 1000
C
        NRECORD = 0
        NDIR    = 0
        ACTIVE  = .TRUE.
C
        DO WHILE ( ACTIVE )
          READ(UNIT=80,FMT='(A)',END=900) RECORD
          CALL WORD(RECORD,I,J,K)
C
C ****  Ignore blank lines
C
          IF ( K .GT. 0 ) THEN
C
C ****  Get directory name and set directory
C
            I = INDEX(RECORD,'//')
            IF ( I .GT. 0 ) THEN
C
C ****  Directory found
C
              IF ( NDIR .LT. MAXDIR ) THEN
                NDIR = NDIR + 1
                DIRLIST(NDIR) = RECORD(I:)
                CALL HCDIR(DIRLIST(NDIR),' ')
              ELSE
                NDIR = MAXDIR + 1
              ENDIF
            ENDIF
C
C ****  Store  ALL records
C
            IF ( NRECORD .LT. MAXRECORD ) THEN
              NRECORD = NRECORD + 1
              BUFFER(NRECORD) = RECORD
              IDIR(NRECORD)   = NDIR    ! Pointer to directory
              HISTID(NRECORD) = VALUE(RECORD,I,J,K)
            ENDIF
C
          ENDIF
        ENDDO
C
  900   CONTINUE
        CLOSE(UNIT=80)
C
C ****  Delete file
C
        IF ( .NOT. KEEP ) THEN
          CALL D0OPEN(80,D0HINDEX_FILE,'M',OK)
          CLOSE(UNIT=80,STATUS='DELETE')
        ENDIF
      ENDIF
C
C ****  Fill list
C
      LASTDIR = ' '
      CALL LISFIL(0,' ')
      DO II = 1, NRECORD
        RECORD = BUFFER(II)
C
C ****  Get Number of entries
C
        IF ( HISTID(II) .GT. 0 ) THEN
C
C ****  Set directory
C
          JDIR = IDIR(II)
          IF ( DIRLIST(JDIR) .NE. LASTDIR ) THEN
            CALL HCDIR(DIRLIST(JDIR),' ')
            LASTDIR = DIRLIST(JDIR)
          ENDIF
C
          CALL HNOENT(HISTID(II),COUNT)
          WRITE(RECORD(65:76),'(''('',I10,'')'')') COUNT
        ENDIF
C
        CALL LISFIL(1,RECORD)
      ENDDO
C
      CALL OUTMSG(' ')
      CALL OUTMSG(' ')
      CALL OUTMSG(' ')
      CALL OUTMSG(' ')
      CALL OUTMSG(' ')
C
      CALL PFLABL(' ',' ',' ','BACK')
C
      N  = 1
      ID = 2
      DO WHILE ( N .GT. 0 )
C
C ****  Display list
C
        CALL LISKEEP
        CALL LISDEF(ID)
        CALL LISPOS(ROW,COL)
        CALL LISBOX
     &    ('HISTOGRAMS; Double Click PF1 to SELECT',' ',N,ID,1)
        IF ( N .GT. 0 ) THEN
C
C ****  Get histogram ID
C
          CALL LISGET(RECORD)
          HID = VALUE(RECORD,I,J,K)
          IF ( HID .GT. 0 ) THEN
            JDIR = IDIR(ID)
            IF ( (JDIR .GT. 0) .AND. (JDIR .LE. MAXDIR) ) THEN
              REMARK = ' ===> Directory : '//DIRLIST(JDIR)
              CALL OUTMSG(' ')
              CALL OUTMSG(REMARK)
              RECORD = ' '//RECORD
              CALL OUTMSG(RECORD)
C
C ****  Plot histogram; first set the directory
C
              CALL HCDIR(DIRLIST(JDIR),' ')
              CALL D0HINT
              CALL D0HPID(HID)
              CALL D0HCOM
            ELSE
              CALL OUTMSG(' *** Problem setting directory')
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C ****  Back to original directory
C
      CALL HCDIR(SAVEDIR,' ')
  999 RETURN
C
C ****  ERRORS
C
 1000 CONTINUE
      REMARK = ' Unable to open file '//D0HINDEX_FILE
      CALL OUTMSG(REMARK)
      RETURN
C
      ENTRY D0HINDEX_REFRESH(FLAG)
      REFRESH = FLAG
      RETURN
      END
