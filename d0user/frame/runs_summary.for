      SUBROUTINE RUNS_SUMMARY(NTOT,NRUNC,RUNS,EVRUNS,EVSKIP,EVPROC,
     &  EVREAD,NFILE,FILE,FEVSKIP,FEVPROC,FEVREAD,OUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      print job summary for D0USER
C-   Inputs  : 
C-     NTOT= total number of events
C-     NRUNC= total number of runs
C-     RUNS = list of runs
C-     EVRUNS= number of events in run
C-     EVSKIP= number of events skipped in run
C-     EVPROC= number of events processed in run
C-     NFILE = number of files
C-     FEVSKIP= number of events in file skipped
C-     FEVPROC= number of events in file processed
C-     FEVREAD= number of events in file read
C-   Outputs : 
C-     OUT = summary output unit number
C-     all input arrays get zeroed
C-
C-   Created  19-JAN-1993   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NTOT,NRUNC
      INTEGER RUNS(*),EVRUNS(*),EVSKIP(*),EVPROC(*)
      INTEGER EVREAD(*)
      INTEGER NFILE,FEVSKIP(*),FEVPROC(*),FEVREAD(*)
      CHARACTER*(*) FILE(*)
      INTEGER OUT,SSUNIT,NPROC,NCALLS,I
      CHARACTER*78 MSG
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL SSOPEN('D0USER_SUMMARY')
        OUT=SSUNIT()
        WRITE(OUT,111)
        FIRST=.FALSE.
        NCALLS=1
      ENDIF
      IF(NCALLS.GT.1) WRITE(OUT,211) NCALLS
C
      IF (NFILE.EQ.1) THEN
        FEVREAD(1) = NTOT
      ELSE
        FEVREAD(NFILE) = NTOT
        DO I = 1, NFILE-1
          FEVREAD(NFILE) = FEVREAD(NFILE) - FEVREAD(I)
        END DO
      END IF
C
      IF (NRUNC.EQ.1) THEN
        EVREAD(1) = NTOT
      ELSE
        EVREAD(NRUNC) = NTOT
        DO I = 1, NRUNC-1
          EVREAD(NRUNC) = EVREAD(NRUNC) - EVREAD(I)
        END DO
      END IF
C
      WRITE(OUT,120) NFILE
  120 FORMAT('  Number of files processed =',I6)
      DO I = 1, NFILE
        IF (FEVSKIP(I).EQ.-1) THEN
          WRITE(OUT,122)
  122     FORMAT(' Skipped events = -1 indicates wild card usage;',/,
     &      ' skipping controlled by first file in the series.')
          GO TO 200
        END IF
      END DO
  200 CONTINUE
C
      WRITE(OUT,121)
     &  (FILE(I)(1:127),FEVSKIP(I),FEVPROC(I),FEVREAD(I),I=1,NFILE)
  121 FORMAT(' File name '/,1X,A127,/,
     &  8X,'Number of events skipped =',I7,/,
     &  8X,'Number of events processed =',I7,/,
     &  8X,'Number of events read =',I7,/)
C
      WRITE(OUT,*) ' Number of runs processed =', NRUNC
      WRITE(OUT,112) (RUNS(I),EVSKIP(I),EVPROC(I),EVREAD(I),I=1,NRUNC)
      CALL EVTMSG(MSG(1:48))
      DO I = 1, NRUNC
        NPROC = NPROC + EVPROC(I)
        RUNS(I)=0
        EVSKIP(I)=0
        EVPROC(I)=0
        EVREAD(I)=0
      END DO
      NRUNC=0
      WRITE(OUT,113) MSG(1:48),NTOT,NPROC
C
      NTOT=0
      NFILE=0
      NCALLS=NCALLS+1
  999 RETURN
  111 FORMAT(10X,60('*'),/,10X,'*',58X,'*'/,10X,'*',20X,
     &  'STANDARD OUTPUT', 23X,'*',/,10X,'*',58X,'*',
     &  /,10X,60('*'),/)
  112 FORMAT(' Run=',I8,
     &  ',  Number of events skipped =',I7,/,
     &  13X,'   Number of events processed =',I7,/,
     &  13X,'   Number of events read =',I7,/)
  113 FORMAT(' Last event read: ',A48,/,
     &  ' Total number of events read =',I7,/,
     &  ' Total number of events processed =',I7,/)
  211 FORMAT(10X,60('*'),/,10X,'*',58X,'*'/,10X,'*',20X,
     &  'PART',I3, 33X,'*',/,10X,'*',58X,'*',
     &  /,10X,60('*'),/)
      END
