      SUBROUTINE D0DAD_FXCOPY(FINDF,FOUTFX,IERR)
C-------------------------------------------------------------------------
C  Read ZEBRA files using d0dad files.
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:quest.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INCLUDE 'D0$INC:d0dad.inc'
      INTEGER  IOLDFILE,IERR,NHEAD,NHMAX,IZBOUT,IKEY
      INTEGER  LSTART,NEV,ICLEN,IRUN,IEVENT,IFILE,IREC,IOFFS,IXWIPE
      INTEGER  LENOCC
      EXTERNAL LENOCC
      LOGICAL  ZEBFOPEN
      PARAMETER(NHMAX=2000)
      INTEGER IUHEAD(NHMAX)
      CHARACTER*(*) FINDF,FOUTFX
      CHARACTER*200 CGNAM,CTAP*40,FINFC*132,FZBTMP*132
      CHARACTER*3 CHOPT
C
      LOGICAL D0DAD_RE_RANGE
      EXTERNAL D0DAD_RE_RANGE
C
      DATA ZEBFOPEN/.FALSE./
C
C  Copy input file names to common storage.
C
      FDFNAM=FINDF
C
C  Diagnostic output....
C
      IF( LDDBG.GT.3 ) THEN
         WRITE(*,*) ' '
         WRITE(*,*) ' ********* In FXCOPY **********'
         WRITE(*,*) ' D0DadFile: ',FDFNAM(1:LENOCC(FDFNAM))
         write(*,*) ' OutputFile: ',FOUTFX(1:LENOCC(FOUTFX))
         WRITE(*,*) ' '
      ENDIF
C
      LRECL=8190
      CALL D0DAD_GTUNIT(IDADZB,IKEY,IERR)
      CALL D0DAD_GTUNIT(IZBOUT,IKEY,IERR)
C
C  Open the input files ...
C
      CALL D0DAD_SYSOPEN(FDFNAM,IDADDF,IERR)
      IF( IERR.NE.0 ) GOTO 901
C
C  Open the output file 
C
C&IF VAXVMS
      OPEN(IZBOUT,FILE=FZBNAM(1:LENOCC(FZBNAM)),STATUS='NEW',
     +   FORM='UNFORMATTED',RECL=LRECL,ERR=905,IOSTAT=IERR,
     +   BLOCKSIZE=4*LRECL)
      CHOPT='XO'
C&ELSE
C&      CALL CFOPEN(IQUEST(1),0,LRECL,'w',1,FZBNAM,IERR)
C&      IZBOUT=IQUEST(1)
C&      IF( IERR.NE.0 ) GOTO 905
C&      CHOPT='LO'
C&ENDIF
      CALL FZFILE(IZBOUT,LRECL,CHOPT)
      IF( IQUEST(1).NE.0 ) GOTO 908
C
C  Main event reading loop
C
 20   CONTINUE
C
C     Read the event record from the d0dad file...
C
        CALL D0DAD_DFREAD(IDADDF,IERR)
        IF( IERR.EQ.3 ) GOTO 998
        IF( IERR.NE.0 ) GOTO 903

C   Check for run range OK.

        CALL D0DAD_LASTIN(IRUN,IEVENT)
        IF( .NOT.D0DAD_RE_RANGE(ISELR,ISELE,IRUN,IEVENT) ) GOTO 20

C     OK. Write to output file.

        NEV=NEV+1
        NHEAD=IQ(LHEAD-1)
        IF(NHEAD.GT.NHMAX) NHEAD=NHMAX
        CALL UCOPY(IQ(LHEAD+1),IUHEAD,NHEAD)
        CALL FZOUT(IZBOUT,IXMAIN,LHEAD,1,' ',2,NHEAD,IUHEAD)
        IF( IQUEST(1).NE.0 ) GOTO 911

      GOTO 20
C
C     Done.  Clean up and exit...
C
 998  CONTINUE
      WRITE(*,1998) NEV
 1998 FORMAT(' Reached end of d0dad file after ',I6,' events. Done.')
      CALL FZENDO(IZBOUT,'TX')
CJDHC&IF VAXVMS
      CLOSE(IZBOUT)
CJDHC&ELSE
CJDHC&      CALL CFCLOS(IZBOUT,' ')
CJDHC&ENDIF
      CALL D0DAD_CLOSE(IDADDF,IERR)
      CALL D0DAD_CLOSE(IDADFC,IERR)
      GOTO 999
C
C  Error reporting...
C
 901  CONTINUE
      WRITE(*,*) ' FXCOPY: Error ',IERR,' opening d0dad file.'
      ierr = -1
      GOTO 999

 902  CONTINUE
      WRITE(*,*) ' FXCOPY: Error ',IERR,' opening names file.'
      IERR = -2
      GOTO 999

 903  CONTINUE
      WRITE(*,*) ' FXCOPY: Error ',IERR,' reading  d0dad file.'
      IERR = -3
      GOTO 999

 904  CONTINUE
      WRITE(*,*) ' FXCOPY: Error ',IERR,' reading names file.'
      IERR = -4
      GOTO 999

 905  CONTINUE
      WRITE(*,*) ' FXCOPY: Error ',IERR,' from OPEN of input file.'
      IERR = -5
      GOTO 999

 906  CONTINUE
      WRITE(*,*) ' FXCOPY: HARD Error',IQUEST(1),' returned from FZIN'
      IERR = -6
      GOTO 999

 907  CONTINUE
      WRITE(*,*) ' FXCOPY: Error ',IQUEST(1),' returned from FZIN'
      IERR = -7
      GOTO 999

 908  CONTINUE
      WRITE(*,*) ' FXCOPY: Error returned from FZFILE(input)=',IQUEST(1)
      IERR = -8
      GOTO 999

 909  CONTINUE
      WRITE(*,*) ' FXCOPY: Error ',IERR,' from OPEN of output file.'
      IERR = -9
      GOTO 999

 910  CONTINUE
      WRITE(*,*) ' FXCOPY: Error ',IQUEST(1),' from FZFILE(output)'
      IERR = -10
      GOTO 999

 911  CONTINUE
      WRITE(*,*) ' FXCOPY: Error ',IQUEST(1),' returned from FZOUT'
      IERR = -11
      GOTO 999

C
 999  CONTINUE
      RETURN
      END
