      SUBROUTINE DFPUT(ILUN,IRUN,IEVT,IFID,IZRN,IZBO,IDATE,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write a data record to a d0dad file.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-NOV-1993   John D Hobbs
C-   Modified 21-FEB-1994   John D Hobbs
C-     Convert to CFIO routines.
C-   Modified 16-MAR-1995   JDH Convert to fortran direct access I/O
C-     for better efficiency
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,IRUN,IEVT,IFID,IZRN,IZBO,IERR,IOFFS,BUFFER_POS
      INTEGER IREC(JRECDF),I,IROLD,IDATE(2),OUT_REC
      LOGICAL EOB
C----------------------------------------------------------------------
C
      CALL DFLSET(ILUN,IERR)
      IF( IERR.NE.0 ) GOTO 996
      IOFFS=IQ(LDFHD+JDFIOFF)
C
C  Setup the next slot in the buffer (flush if necessary)
C
 10   CONTINUE
      EOB = MOD(JRECDF*IOFFS,JPRECDF).EQ.0
      IF( EOB .AND. IOFFS.NE.0 ) THEN
        OUT_REC = (JRECDF*IOFFS)/JPRECDF+1
        CALL UCOPY(IQ(LDFHD+NDDF+JDFHED+1),DF_IOREC,JPRECDF)
        WRITE(ILUN,REC=OUT_REC,ERR=997) DF_IOREC
        CALL VZERO(IQ(LDFHD+NDDF+JDFHED+1),JPRECDF)
        CALL VZERO(DF_IOREC,JPRECDF)
        CALL UCOPY(IQ(LDFHD+NDDF+1),DF_IOREC,JDFHED)
        WRITE(ILUN,REC=1,ERR=997) DF_IOREC
        IQ(LDFHD+JDFDIRT)=0
      ENDIF
C
C  Add a run record to the buffer
C
      IROLD=IQ(LDFHD+JDFRUN)
      IF( IRUN.NE.IROLD ) THEN
        CALL VZERO(IREC,JRECDF)
        IQ(LDFHD+JDFRUN)=IRUN
        IREC(1)=-1
        IREC(2)=IRUN
        IREC(3)=IDATE(1)
        IREC(4)=IDATE(2)
        IF( IQ(LDFHD+JDFBS).NE.0 ) CALL VXINVB(IREC,JRECDF)
        IOFFS=IOFFS+1
        BUFFER_POS=MOD(JRECDF*(IOFFS-1),JPRECDF)+1
        CALL UCOPY(IREC,IQ(LDFHD+NDDF+JDFHED+BUFFER_POS),JRECDF)
        IQ(LDFHD+JDFDIRT)=1
        IQ(LDFHD+NDDF+JDFLREC) = IOFFS
        GOTO 10
      ENDIF
C
C  Add an event record to the buffer
C
      IREC(1)=IEVT
      IREC(2)=IFID
      IREC(3)=IZRN
      IREC(4)=IZBO
      IF( IQ(LDFHD+JDFBS).NE.0 ) CALL VXINVB(IREC,JRECDF)
      IOFFS=IOFFS+1
      BUFFER_POS=MOD(JRECDF*(IOFFS-1),JPRECDF)+1
      CALL UCOPY(IREC,IQ(LDFHD+NDDF+JDFHED+BUFFER_POS),JRECDF)
      IQ(LDFHD+JDFDIRT)=1
      IQ(LDFHD+NDDF+JDFLREC) = IOFFS
C
      IQ(LDFHD+JDFIOFF) = IOFFS
C
  999 CONTINUE
      IERR=0
      RETURN
C
 996  CONTINUE
      IERR = -3
      RETURN
C
 997  CONTINUE
      IERR = -1
      RETURN
C
 998  CONTINUE
      IERR = -2
      RETURN
      END
