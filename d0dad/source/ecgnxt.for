      SUBROUTINE ECGNXT(IR,IDAT,NMAX,IERR)
C----------------------------------------------------------------------
C- C- PURPOSE AND METHODS : Read the next event from an event catalog.
C- 
C- INPUTS  :
C- OUTPUTS : IR   - Run Number
C-           IDAT - Event record
C-           NMAX - Size of IDAT
C-           IERR - 0==>OK, 1==>End-of-catalog, else ERROR 
C- CONTROLS: 
C- 
C- CREATED 14-NOV-1993 John D Hobbs 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      INTEGER IR,IDAT(*),NMAX,IERR
      INTEGER IRUN,IRECEC,IT,NEVTS,LTMP,IREC
C----------------------------------------------------------------------
C
      IRUN=0
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
C
C  Check for no run data loaded or no history...
C
      IF( LRDAT.LE.0 ) IRUN=IQ(LRUNS+1)
C
C  Check for reached end of current run
C
      IF( IRUN.EQ.0 ) THEN
        IT=IQ(LECHD+JIRUN)
        NEVTS=IQ(LRUNS+IRECEC*(IT-1)+4)
        IF( IQ(LECHD+JIEVT).GE.NEVTS ) THEN
          IRUN=IQ(LECHD+JIRUN)
          IRUN=IRUN+1
          IF( IRUN.GT.IQ(LECHD+NDEC+JEC_IECRUN) ) THEN
            IERR=1
            GOTO 999
          ENDIF
          IRUN=IQ(LRUNS+IRECEC*(IRUN-1)+1)
        ENDIF
      ENDIF
C
C  If next run is needed, load...
C
      IF( IRUN.NE.0 ) THEN
         CALL ECGRUN(IRUN,IREC,IERR)
         IF( IERR.EQ.1 ) THEN
           IERR=1
           GOTO 999
         ELSEIF( IERR.NE.0 ) THEN
           IERR = -1
           GOTO 999
         ENDIF
      ENDIF
C
C  Get next event from currently loaded run data.
C
      IQ(LECHD+JIEVT)=IQ(LECHD+JIEVT)+1
      LTMP=LQ(LECHD-LLRDAT)
      LTMP=LTMP+IRECEC*(IQ(LECHD+JIEVT)-1)
      CALL UCOPY(IQ(LTMP+1),IDAT,MIN(IRECEC,NMAX))
      IR=IQ(LECHD+JNRUN)
      IERR=0
C
 999  CONTINUE
      RETURN
      END
