      SUBROUTINE DFGET(ILUN,IRUN,IEVT,IFID,IZRN,IZBO,IDATE,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a data record from a d0dad file.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-   Modified 21-FEB-1994   John D Hobbs
C-     Convert to CFIO routines and added byteswapping test.
C-   Modified 19-JAN-1995   JDH 
C-     Check for zero valued event entries at end of file.  These can
C-     occur when files are binary-ftp'd between different operating
C-     systems.
C-   Modified 16-MAR-1995   JDH
C-     Switch to fortran direct access I/O
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,IRUN,IEVT,Ifid,IZRN,IZBO,IERR
      INTEGER IREC(JRECDF),NREAD,ILASTEV(5),IDATE(2),IRECIN
      INTEGER LASTEVENT(5),ILDATE(2),IEVENT,NEVENT,IOFFS,IPOSN
      SAVE    LASTEVENT,ILDATE
C----------------------------------------------------------------------
C
      CALL DFLSET(ILUN,IERR)
      IF( IERR.NE.0 )GOTO 996
C
C  Read via backward compatibility...
C
      IF( IQ(LDFHD+NDDF+JXXVER).LT.106 ) THEN
        CALL DFGET_OLD(ILUN,IRUN,IEVT,IFID,IZRN,IZBO,IDATE,IERR)
        IF( IERR.EQ.1 ) RETURN
        GOTO 999
      ENDIF
C
C  Read direct access versions...
C  Check for end-of-file
C
      NEVENT=IQ(LDFHD+NDDF+JDFLREC)
      IOFFS=IQ(LDFHD+JDFIOFF)
      IF( IOFFS.EQ.NEVENT ) GOTO 998
C
C  Get the event data
C
 10   CONTINUE
      IF( IOFFS.EQ.NEVENT ) GOTO 998
      IF( MOD(JRECDF*IOFFS,JPRECDF).EQ.0 ) THEN
        IRECIN=(JRECDF*IOFFS)/JPRECDF+1   ! Skip the header record
        READ(ILUN,REC=IRECIN+1,ERR=997) DF_IOREC
        CALL UCOPY(DF_IOREC,IQ(LDFHD+NDDF+JDFHED+1),JPRECDF)
      ENDIF
      IPOSN = MOD(IOFFS*JRECDF,JPRECDF)+1
      CALL UCOPY(IQ(LDFHD+NDDF+JDFHED+IPOSN),IREC,JRECDF)
      IOFFS = IOFFS+1
      IF( IREC(1).EQ.0 ) GOTO 10   ! Skip records having 0 event number
      IF( IQ(LDFHD+JDFBS).NE.0 ) CALL VXINVB(IREC,JRECDF)

C Check for a new run record...

      IF( IREC(1).EQ.-1 ) THEN
        IQ(LDFHD+JDFRUN)=IREC(2)
        ILDATE(1)=IREC(3)
        ILDATE(2)=IREC(4)
        GOTO 10
      ENDIF

C Regular event data.  Copy and return after saving pointers to buffer
C and event count...

      IQ(LDFHD+JDFIOFF) = IOFFS
      IRUN=IQ(LDFHD+JDFRUN)
      IEVT=IREC(1)
      IFID=IREC(2)
      IZRN=IREC(3)
      IZBO=IREC(4)
      IDATE(1)=ILDATE(1)
      IDATE(2)=ILDATE(2)
      LASTEVENT(1)=IRUN
      CALL UCOPY(IREC(1),LASTEVENT(2),4)
C
 999  CONTINUE
      IERR=0
      RETURN
C
 996  CONTINUE
      D0DAD_ERRTXT=' Error finding Zebra bank for D0DAD file'
      IF( LDDBG.GT.0 ) CALL ERRMSG(' ','DFGET',D0DAD_ERRTXT,'W')
      IERR=-2
      RETURN
C
 997  CONTINUE
      D0DAD_ERRTXT=' Error reading D0DAD file'
      IF( LDDBG.GT.0 ) CALL ERRMSG(' ','DFGET',D0DAD_ERRTXT,'W')
      IERR=-1
      RETURN
C
 998  CONTINUE
      D0DAD_ERRTXT=' EOF from input D0DAD file'
      IF( LDDBG.GT.2 ) CALL ERRMSG(' ','DFGET',D0DAD_ERRTXT,'I')
      IERR = 1
      RETURN
C
C  Get last run/event read in
C
      ENTRY D0DAD_LASTIN(ILASTev)
      CALL UCOPY(LASTEVENT,ILASTEV,5)
      RETURN
C
      END
