      SUBROUTINE D0DAD_SCAN(FNAME,FINUE,UINECOPT,IERR)
C-------------------------------------------------------------------------
C  Scan a given input file(set) to create the file catalog and unsorted 
C  event catalog used in direct access zebra I/O.
C
C  Author:  John D. Hobbs
C  Date:     1-NOV-1993
C
C  INPUTS: 
C     FNAME  - C - Name (set) of input Zebra files to scan)
C     FUENAM - C - Name of output unsorted event catalog.
C     UECOPT - C - Options for unsorted event catalog OPEN STATUS>
C     FFCNAM - C - Name of output file catalog.
C     UFCOPT - C - Options for file catalog OPEN STATUS>
C     
C  OUTPUTS: 
C     IERR   - I - Error return, 0 ==> All is OK.
C-------------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) FNAME,FINUE,UINECOPT*1
      INTEGER   IERR
C
      INCLUDE  'D0$INC:zebcom.inc'
      INCLUDE  'D0$INC:quest.inc'
      INCLUDE  'D0$INC:d0dadcom.inc'
      INCLUDE  'D0$INC:d0dad.inc'
C
      INTEGER  OK,ILEN,IFNLEN,IFUNIT,IXWIPE
      INTEGER  NHMAX,LSTART,NEVTOT,NEV,IER,ikey
      PARAMETER(NHMAX=1000)
      INTEGER  NHEAD,IUHEAD(NHMAX),IFILE,ICNTXT
      LOGICAL  LFLOOP,LEXIST,LDEBUG,LOK
      CHARACTER*128 INPUT_FILE,CINP
      CHARACTER*2 CHOPT
      INTEGER LENOCC
      LOGICAL LIB$FIND_FILE
      EXTERNAL LENOCC,LIB$FIND_FILE
C-------------------------------------------------------------------------
C
      IERR=0
      LFLOOP=.FALSE.
      NEVTOT=0
C&IF VAXVMS
      CHOPT='XI'
C&ELSE
C&      CHOPT='LI'
C&ENDIF
      INPUT_FILE=FNAME
      IFUNIT=IDADOK
      FUENAM=FINUE
      UECOPT=UINECOPT
      CALL D0DAD_GTUNIT(IDADZB,IKEY,IERR)
C
      IF( LDDBG.GT.3 ) THEN
         WRITE(*,*) ' '
         WRITE(*,*) ' *** Entry into d0dad_scan ***'
         WRITE(*,3001) FNAME(1:LENOCC(FNAME))
         WRITE(*,3002) 'UEvent ',FUENAM(1:LENOCC(FUENAM)),UECOPT
 3001    FORMAT(' Input file: ',A)
 3002    FORMAT(' ',A7,' filename: ',A,', Option: ',A1)
      ENDIF
C
C  Allow a D0USER style list of filenames.  
C
      CINP=INPUT_FILE
      CALL CUTOL(CINP)
      IF( CINP.EQ.'file_names' ) THEN
         LFLOOP=.TRUE.
         CALL D0OPEN(IFUNIT,'file_names','IFLN',LOK)
         IF( .NOT.LOK ) THEN
           IERR = -6
           GOTO 999
         ENDIF
      ENDIF
C
C  Open the output unsorted event catalog...
C
      CALL D0DAD_OPEN(JFUE,FUENAM,UECOPT,IDADUE,IERR)
      IF( IERR.NE.0 ) THEN
         IF( LDDBG.GT.0 ) WRITE(*,8001) 
 8001    FORMAT(' Error ',I4,' opening d0dad uevent catalog.')
         IERR = -1
         GOTO 999
      ENDIF
C
 20   CONTINUE
C
      IF( LFLOOP ) THEN
         IERR=0
         READ(IFUNIT,'(A)',END=999) INPUT_FILE
      ENDIF
C
C  Translate to true file name.  Write original name to UE file...
C
      CFNAME=INPUT_FILE
      ILEN=LENOCC(INPUT_FILE)
      IF( CFNAME(1:12).EQ.'D0$DATA$DST:' ) CALL CUTOL(CFNAME(13:ILEN))
      IF( .NOT.LIB$FIND_FILE(CFNAME,INPUT_FILE,ICNTXT) ) THEN
        CALL LIB$FIND_FILE_END(ICNTXT)
        ICNTXT=0
        GOTO 903
      ENDIF
      CALL LIB$FIND_FILE_END(ICNTXT)
      ICNTXT=0
C
      NEV=0
CJDHC&IF VAXVMS
      CALL D0OPEN(IDADZB,INPUT_FILE(1:LENOCC(INPUT_FILE)),'IX',LOK)
      IF( .NOT.LOK ) GOTO 903
CJDH      OPEN(IDADZB,FILE=INPUT_FILE,STATUS='OLD',FORM='UNFORMATTED',
CJDH     +   READONLY,ERR=903,IOSTAT=IERR)
CJDHC&ELSE
CJDHC&      CALL CFOPEN(IQUEST(1),0,8190,'r',1,INPUT_FILE,OK)
CJDHC&      IF( OK.NE.0 ) GOTO 903
CJDHC&      IDADZB=IQUEST(1)
CJDHC&ENDIF
      CALL FZFILE(IDADZB,8190,CHOPT)
      IF( IQUEST(1).NE.0 ) THEN
         WRITE(*,8002) IQUEST(1)
 8002    FORMAT(' D0DAD_SCAN: Error ',I4,'returned from FZFILE')
         IERR = -2
         GOTO 999
      ENDIF
C
C     Write new file to the output file...
C
      CALL UEPUT(IDADUE,-1,0,0,0,0,CFNAME,CGNAME,CTAPE,CFCCOM,IERR)
      IF( LDDBG.GT.3 ) WRITE(*,1001) CFNAME(1:LENOCC(CFNAME))
 1001 FORMAT('  New File:',A)
C
C     Scan the file...
C
 10   CONTINUE
         NHEAD=NHMAX
         CALL MZWIPE(IXWIPE)
         IF( HEADER_ONLY ) THEN
           CALL FZIN(IDADZB,IXMAIN,0,0,'S',NHEAD,IUHEAD)
         ELSE
           CALL FZIN(IDADZB,IXMAIN,LHEAD,1,' ',NHEAD,IUHEAD)
         ENDIF
C
         IF( IQUEST(1).eq.-2 .OR. IQUEST(1).EQ.-3 ) THEN
            IF( LDDBG.GT.2 ) WRITE(*,8003) IQUEST(1)
 8003       FORMAT(' D0DAD_SCAN: Error',I4,' from FZIN. Skip event.')
            GOTO 10
         ENDIF
C
         IF( IQUEST(1).EQ.0 ) THEN
            NEVTOT=NEVTOT+1
            NEV=NEV+1
            LDEBUG=MOD(NEVTOT,100).EQ.0 .AND. LDDBG.GT.3
            IF( LDEBUG ) WRITE(*,1002) NEVTOT,NEV
 1002       FORMAT('       Event ',I8,' (this file, ',I8,')')
            IDRUN=IUHEAD(6)+IROFF
            IDEVNT=IUHEAD(9)+IEOFF
            IF( IUHEAD(14).LT.6 ) THEN
              ISTMSK(1)=IUHEAD(10)
              ISTMSK(2)=0
            ELSE
              ISTMSK(1)=IUHEAD(31)
              ISTMSK(2)=IUHEAD(32)
            ENDIF
            IDZRN=IQUEST(5)
            IDZBO=IQUEST(6)
            CALL UEPUT(IDADUE,IDRUN,IDEVNT,ISTMSK(1),IDZRN,IDZBO,
     +           CFNAME,CGNAME,CTAPE,CFCCOM,IERR)
            IF( LDDBG.GT.4 ) THEN
              WRITE(*,1003)IDRUN,IDEVNT,ISTMSK(1),ISTMSK(2),IDZRN,IDZBO
 1003         FORMAT(6I10)
            ENDIF
            GOTO 10
         ENDIF
C
         IF( IQUEST(1).EQ.1 ) THEN
            IF( LDDBG.GT.2) WRITE(*,9001) 
 9001       FORMAT('     Found start of run. Continueing...')
            GOTO 10
         ENDIF
         IF( IQUEST(1).EQ.2 ) THEN
            IF( LDDBG.GT.2 ) WRITE(*,9002) 
 9002       FORMAT('     Found end of run. Continueing...')
            GOTO 10
         ENDIF
         IF( IQUEST(1).EQ.3 ) THEN
            IF( LDDBG.GT.2 ) WRITE(*,9003) NEV
 9003       FORMAT('     Found ZEBRA EOF after',I6,' events.',
     +             ' Close file and go on...',/)
            GOTO 30
         ENDIF
         IF( IQUEST(1).EQ.4 ) THEN
            IF( LDDBG.GT.2 ) WRITE(*,9004) 
 9004       FORMAT('     SYSEOF: Can continue...')
            GOTO 30
         ENDIF
         IF( IQUEST(1).EQ.5 ) THEN
            IF( LDDBG.GT.2 ) WRITE(*,9005)
 9005       FORMAT('     SYSEOF: Cannot continue with this file...')
            GOTO 30
         ENDIF
         IF( IQUEST(1).EQ.6 ) THEN
            IF( LDDBG.GT.2 ) WRITE(*,9006) 
 9006       FORMAT('     Attempted to read beyond EOD...')
            GOTO 30
         ENDIF
         IF( IQUEST(1).LT.0 ) THEN
            IF( LDDBG.GT.2 ) WRITE(*,9007) IQUEST(1)
 9007       FORMAT(' Error',I10,' from FZIN. Skip file')
            GOTO 30
         ENDIF
         IERR = -3
         IF( LDDBG.GT.0 ) WRITE(*,9008) IQUEST(1)
 9008    FORMAT('     FZIN: Unknown return code=',I10,'. Stop')
         GOTO 999
 30      CONTINUE
         CALL FZENDI(IDADZB,'TX')
CJDHC&IF VAXVMS
         CLOSE(IDADZB)
CJDHC&ELSE
CJDHC&         CALL CFCLOS(IDADZB,' ')
CJDHC&ENDIF
         IF( .NOT.LFLOOP ) GOTO 999
      GOTO 20
C
C     Done.  Clean up and exit.
C
 999  CONTINUE
      IF( LDDBG.GT.3 ) WRITE(*,1005) NEVTOT
 1005 FORMAT(' Scan finished: ',I6,' events read')
      CALL D0DAD_CLOSE(IDADUE,IERR)
      RETURN

 901  CONTINUE
      IF( LDDBG.GT.0 ) WRITE(*,9901) IERR
 9901 FORMAT(' Error ',I4,' opening d0dad unsorted event file.')
      IERR = -4
      RETURN

 902  CONTINUE
      IF( LDDBG.GT.0 ) WRITE(*,9902) IERR
 9902 FORMAT(' D0DAD_SCAN: Error ',I4,' opening d0dad file catalog.')
      IERR = -5
      RETURN

 903  CONTINUE
      IF(LDDBG.GT.2) WRITE(*,9903) IERR,INPUT_FILE(1:LENOCC(INPUT_FILE))
 9903 FORMAT(' D0DAD_SCAN: Error ',I4,' opening ',A,/,
     +       '    skip this file...')
      IF( LFLOOP ) GOTO 20
      GOTO 999
C
      END
