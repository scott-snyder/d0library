      SUBROUTINE ECINT(IR,IDAT,NMAX,IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Update an event catalog using an internal
C-      sorting routine and storage
C-
C-   INPUTS  : 
C-   OUTPUTS : 
C-   CONTROLS: 
C-
C-   CREATED   10-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc/NOLIST'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER IR,IDAT(*),NMAX,IERR
      INTEGER LLEVT,NINC,LDUMMY,NLEVTS,IRLOC,IRECEC,IRPREC
C----------------------------------------------------------------------
C
      IERR=0
      IF( LDATA.LE.0 ) THEN
        NLEVTS=0
        IRLOC=0
      ELSE
        NLEVTS=IQ(LDATA+2)
        IRLOC=IQ(LDATA+1)
      ENDIF
      IF( NLEVTS.EQ.0 ) GOTO 10
      IF( IR.EQ.IRLOC ) GOTO 10
C
C  Need to update event catalog (onto next run)...
C
      CALL ECSWRT(IERR)
      IF( IERR.NE.0 ) THEN
         WRITE(*,9901) IERR,IQ(LECHD+JLUN)
 9901    FORMAT(' ECINT: Error ',I4,' returned from ECSWRT on unit',I3)
         IERR = -1
         GOTO 999
      ENDIF
      NLEVTS=0
C
C  If run = -1, do not save.  This is the 'flush buffers' run number.
C
      IF( IR.EQ.(-1) ) GOTO 999
C
C  Insert event record into internal storage...
C
 10   CONTINUE
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      IRPREC=IQ(LECHD+NDEC+JEC_IRPREC)
      NINC=IRECEC*IRPREC
      IF( LDATA.LE.0 ) THEN
         CALL MZBOOK(IXDDAD,LDATA,LECHD,-LLDATA,'DATA',0,0,NINC+2,2,-1)
         IF( LDATA.LE.0 ) THEN
            IERR = -3
            GOTO 999
         ENDIF
      ENDIF
      IF( (NLEVTS*IRECEC+2).GE.IQ(LDATA-1) ) THEN
         CALL MZPUSH(IXDDAD,LDATA,0,NINC,'I')
         IF( LDATA.LE.0 ) THEN
            IERR = -2
            GOTO 999
         ENDIF
      ENDIF
C
      NLEVTS=NLEVTS+1
      IQ(LDATA+1)=IR
      IQ(LDATA+2)=NLEVTS
      LLEVT=LDATA+IRECEC*(NLEVTS-1)+2
      CALL UCOPY(IDAT,IQ(LLEVT+1),MIN(IRECEC,NMAX))
C
 999  CONTINUE
      RETURN
      END
