      SUBROUTINE D0DAD_USER(FTEXT,FNDF,FNEC,CDFOPT,IERR)
C-------------------------------------------------------------------------
C  Create a d0dad file from an input run/event list and event and file
C    catalogs.  The file names are specified in cparse.for.  FZBNAM is
C    the name of the evenlist file.
C
C  Author:  John D. Hobbs
C  Date:    16-NOV-1993
C  Modifications:
C     30-Mar-1994 JDH Add creating userfile from a D0DAD file.
C  Modifications:
C     07-Jul-1994 JDH Add call to ECGET_TIMESTAMP and associated code.
C
C  INPUTS: 
C     FTEXT  - C - Input text file containing RUN/EVENT pairs.
C     FNDF   - C - Output D0DAD file name
C     FNEC   - C - Name of input EC or DF.
C     CDFOPT - C - Option string for open of output d0dad file.
C  OUTPUTS: 
C     IERR   - I - Error return, 0 ==> All is OK.
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE  'D0$INC:d0dadcom.inc'
      INCLUDE  'D0$INC:d0dad.inc'
      INTEGER  IERR,NEV,NSEL0,IFID,IDATE(2)
      LOGICAL  LEXIST,LSTDIN,OK
      INTEGER  LENOCC,IERRUN,NEVERR,IOLDRUN,IFTYPE,IRUN,IEVENT
      EXTERNAL LENOCC
      CHARACTER*(*) FTEXT,FNEC,FNDF,CDFOPT
      CHARACTER*200 CTEMP
C
      IF( LDDBG.GT.4 ) THEN
         WRITE(*,9901) FTEXT(1:LENOCC(FTEXT)),FNDF(1:LENOCC(FNDF)),
     +      FNEC(1:LENOCC(FNEC))
 9901    FORMAT(/,'  ** Entering D0DAD_USER ***',/,
     +          '      Input run/event file: ',A,/,
     +          '      Output d0dad file: ',A,/,
     +          '      Event catalog: ',A,/)
      ENDIF
C
C  Open the input text file (for streaming from DF, this file must
C  be sorted by run and event)...
C
      IERRUN=0
      CTEMP=FTEXT
      LSTDIN=.TRUE.
      CALL CLTOU(CTEMP)
      IF(CTEMP.NE.'-' .AND. CTEMP.NE.'SYS$INPUT' .AND. CTEMP.NE.'STDIN')
     +THEN
         LSTDIN=.FALSE.
         INQUIRE(FILE=FTEXT,EXIST=LEXIST)
         IF( .NOT.LEXIST ) THEN
            WRITE(*,9001) FTEXT(1:LENOCC(FTEXT))
 9001       FORMAT(' D0DAD_USER: File "',A,'" does not exist')
            IERR = -5
            GOTO 999
         ENDIF
         CALL D0OPEN(IDADOK,FTEXT(1:LENOCC(FTEXT)),'IF',OK)
         IF( .NOT.OK ) GOTO 901
      ENDIF
C
C  Open input file...
C
      CALL D0DAD_FTYPE(FNEC,IFTYPE)
      IF( IFTYPE.EQ.JFEC ) THEN
        CALL D0DAD_OPEN(JFEC,FNEC,'R',IDADEC,IERR)
        IF( IERR.NE.0 ) THEN
          WRITE(*,*) ' D0DAD_USER: Error ',IERR,' opening Event Catalog'
          IERR = -1
          GOTO 998
        ENDIF
        CDFTAG=CECTAG
      ELSEIF( IFTYPE.EQ.JFDF ) THEN
        CALL D0DAD_OPEN(JFDF,FNEC,'R',IDADEC,IERR)
        IF( IERR.NE.0 ) THEN
          WRITE(*,*) ' D0DAD_USER: Error ',IERR,' opening INPUT d0dadf'
          IERR = -7
          GOTO 998
        ENDIF
      ELSE
        WRITE(*,*)' D0DAD_USER: Unknown file type: ',IFTYPE
        IERR = -8
        GOTO 998
      ENDIF
C
C  Open the dad file...
C
      CALL D0DAD_OPEN(JFDF,FNDF,CDFOPT,IDADDF,IERR)
      IF( IERR.NE.0 ) THEN
         WRITE(*,*)' D0DAD_USER: Error',IERR,' opening D0dad file'
         IERR = -2
         GOTO 998
      ENDIF
C
C  Do the file making...
C
      NEV=0
      NSEL0=0
 10   CONTINUE
C
C    Get next event from input file
C
         IF( LSTDIN ) THEN
            READ(*,*,END=20) IDRUN,IDEVNT
         ELSE
            READ(IDADOK,*,END=20) IDRUN,IDEVNT
         ENDIF
         NEV=NEV+1
C
C    Print a warning
C
         IF( IDRUN.NE.IOLDRUN ) THEN
           IF( NEVERR.NE.0 ) WRITE(*,9008) NEVERR,IOLDRUN
 9008      FORMAT('   ',I8,' events missing from run ',I7)
           NEVERR=0
           IOLDRUN=IDRUN
         ENDIF
C
C    Get next event from master file (EC or DF)
C
         IF( IFTYPE.EQ.JFEC ) THEN

C    Get next event from EC

           CALL ECGET(IDADEC,IDRUN,IDEVNT,ISTMSK,IFID,IDZRN,IDZBO,IERR)
C         Check for real errors
           IF( IERR.LE.-3 ) THEN
             WRITE(*,9003) IERR,IDADEC,'ECGET'
 9003        FORMAT(' D0DAD_USER: Error ',I3,' from ',A5,' on unit',I3)
             IERR = -3 
             GOTO 998
           ENDIF
C        Check for run not found
           IF( IERR.EQ.-1 ) THEN
             IF( IERRUN.NE.IDRUN ) THEN
                WRITE(*,9007) IDRUN
 9007           FORMAT(' Run ',I8,' not in event catalog')
             ENDIF
             NEVERR=NEVERR+1
             IERRUN=IDRUN
             GOTO 10
           ENDIF
           IERRUN=0
C        Check for event not found
           IF( IERR.EQ.-2 ) THEN
             IF( LDDBG.GT.2 ) WRITE(*,9005) IDRUN,IDEVNT,'event catalog'
 9005        FORMAT(' Run/Event',2I8,' not in ',A)
             GOTO 10
           ENDIF
           CALL ECGET_TIMESTAMP(IDADEC,IDATE)
         ELSE
C
C    Get next event from D0DAD file.
C
 30        CONTINUE
           CALL DFGET(IDADEC,IRUN,IEVENT,IFID,IDZRN,IDZBO,IDATE,IERR)
           IF( IERR.NE.0 ) THEN
             WRITE(*,9003) IERR,IDADEC,'DFGET'
             IERR = -3 
             GOTO 998
           ENDIF
C
           IF(  IRUN.LT.IDRUN
     +      .OR. IRUN.EQ.IDRUN .AND. IEVENT.LT.IDEVNT ) GOTO 30
C
           IF(  IRUN.EQ.IDRUN .AND. IEVENT.GT.IDEVNT
     +      .OR. IRUN.GT.IDRUN ) THEN
             IF( LDDBG.GT.2 ) WRITE(*,9005) IDRUN,IDEVNT,'d0dad file'
             GOTO 10
           ENDIF
C
         ENDIF
C
C    Add event to output file
C
         CALL DFPUT(IDADDF,IDRUN,IDEVNT,IFID,IDZRN,IDZBO,IDATE,IERR)
         IF( IERR.LT.0 ) THEN
            IF( LDDBG.GT.0 ) WRITE(*,9004) IERR,IDADDF
 9004       FORMAT(' D0DAD_USER: Error ',I3,' from DFPUT on unit',I3)
            IERR = -4 
            GOTO 998
         ELSE
            IF(LDDBG.GT.6) WRITE(*,9006) IDRUN,IDEVNT,IFID,IDZRN,IDZBO
 9006       FORMAT(' R/E: ',2I8,' found in file',I6,' at ',2I7)
         ENDIF
         NSEL0=NSEL0+1
C
      GOTO 10
C
 20   CONTINUE
      IF( .NOT.LSTDIN ) CLOSE(IDADOK)
      CALL D0DAD_CLOSE(IDADEC,IERR)
      CALL D0DAD_CLOSE(IDADDF,IERR)
C
 999  CONTINUE
      WRITE(*,9902) NEV,NSEL0
 9902 FORMAT(' D0DAD_USER: Read ',I6,' event numbers.  Found ',I6)
      RETURN
C
 998  CONTINUE
      RETURN
C
 901  CONTINUE
      IERR = -6
      RETURN
      END
