      SUBROUTINE ECGRUN(IRUN,IREC,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Load the data for one run into a Zebra bank.
C-     Also saves timestamp for last run loaded.  If the catalog has
C-     been opened for reading and some other process has opened it for
C-     writing, do not do the load and return an error code.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-Jan-1994   John D. Hobbs
C-   Modified 07-Jul-1994   JDH - Save time stamp in EC bank
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER IRUN,IERR,I,J,K,NEVTS
      INTEGER IRPREC,IRECEC,IDAT(KRECEC),NRUNS,NW,LTMP,IREC,IDATE
C-----------------------------------------------------------------------
C
      IERR=0
C
C  Check if the requested run is the current run
C
      IF( LRDAT.GT.0 ) THEN
        IREC=IQ(LECHD+JIRUN)
        IF( IQ(LECHD+JNRUN).EQ.IRUN ) GOTO 999
        CALL MZDROP(IXDDAD,LRDAT,' ')
        LRDAT=0
        IQ(LECHD+JIRUN)=0
        IQ(LECHD+JNRUN)=0
      ENDIF
C
C  Check for write lock (only if reading job).  ECOPEN (via D0DAD_OPEN)
C  prohibits multiple writers
C
      IF( IQ(LECHD+JRW).EQ.0 ) THEN
        CALL ECCHECK_LOCK(IERR)
        IF( IERR.LT.0 ) THEN
          IERR=-4
          GOTO 999
        ENDIF
      ENDIF
C
C  Setup local copies of header values
C
      IRPREC=IQ(LECHD+NDEC+JEC_IRPREC)
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      NW=IRECEC*IRPREC
C
      NRUNS=IQ(LECHD+NDEC+JEC_IECRUN)
      CALL ECFIND(IQ(LRUNS+1),NRUNS,IRECEC,IRUN,1,IREC,IDAT,IERR)
      IF( IERR.EQ.1 ) THEN
        IERR=1
        GOTO 999
      ENDIF
      IF( IERR.NE.0 ) THEN
        IERR = -1
        GOTO 999
      ENDIF
C
C  Found run in catalog.   Load it.
C
      LTMP=LRUNS+IRECEC*(IREC-1)
      NW=IRECEC*IQ(LTMP+4)
      IQ(LECHD+JTS)=IQ(LTMP+6)
      CALL MZBOOK(IXDDAD,LRDAT,LECHD,-LLRDAT,'RDAT',0,0,NW,2,0)
      IF( LRDAT.LE.0 ) THEN
        IERR = -2
        GOTO 999
      ENDIF
      IQ(LECHD+JIRUN)=IREC
      IQ(LECHD+JNRUN)=IRUN
      IQ(LECHD+JIEVT)=0
      IQ(LECHD+JNEVT)=0
      LTMP=LRUNS+IRECEC*(IREC-1)
      CALL ECLOAD(IQ(LRDAT+1),IQ(LTMP+2),NW/IRECEC,IERR)
      IF( IERR.NE.0 ) THEN
        IERR = -3
        GOTO 999
      ENDIF
C
C  Make optional consistancy check of event numbers (monotonically
C  increasing)
C
      IF( LEVT_CHECK ) THEN
        NEVTS=NW/IRECEC
        DO I=2,NEVTS
          J=IRECEC*(I-1)    ! Nth event pointer
          K=IRECEC*(I-2)    ! (N-1)st event pointer
          IF( IQ(LRDAT+J+1).LE.IQ(LRDAT+K+1) ) THEN
            IERR=-5
            GOTO 999
          ENDIF
        ENDDO
      ENDIF
C
 999  CONTINUE
      RETURN
      END
