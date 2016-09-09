      SUBROUTINE ECSWRT(IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Sort events in the internal storage and
c-      write them to an event catalog.
C-
C-   INPUTS  : 
C-   OUTPUTS : 
C-   CONTROLS: 
C-
C-   CREATED  10-NOV-1993   John D Hobbs
C-            10-JAN-1994   John D Hobbs - Use Zebra for working space
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc/NOLIST'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,IERR
      INTEGER IRUN,NLEVTS,IRECEC,IREC,IDAT(KRECEC),NOLD,LBAD
      INTEGER NDUP,NENEW,IREC1,NRECS,NTOT
C----------------------------------------------------------------------
C
      IERR=0
      NOLD=0
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      IRUN=IQ(LDATA+1)
      NLEVTS=IQ(LDATA+2)
C
C  If run is already in catalog, load it...
C
      CALL ECGRUN(IRUN,IREC,IERR)
      IF( IERR.LT.0 ) THEN
        IF(LDDBG.GT.10) WRITE(*,9002) IERR,'ECGRUN'
 9002   FORMAT(' ECSWRT: Error ',I4,' returned from ',A)
        IERR = -2
        GOTO 999
      ELSEIF( IERR.EQ.0 ) THEN
        NOLD=IQ(LRDAT-1)/IRECEC
      ENDIF
C
C  Get some working storage in D0DAD division hanging from EC bank...
C
      CALL MZBOOK(IXDDAD,LRANK,LECHD,-LLRANK,'RANK',0,0,NLEVTS,2,-1)
      CALL MZBOOK(IXDDAD,LORDER,LECHD,-LLORDER,'ORDE',0,0,NLEVTS,2,-1)
      CALL MZBOOK(IXDDAD,LISOLD,LECHD,-LLISOLD,'OLDE',0,0,NLEVTS,2,-1)
      LBAD=LRANK.LE.0 .OR. LORDER.LE.0 .OR. LISOLD.LE.0 
      IF(LBAD.ne.0) THEN
        IF(LDDBG.GT.10) WRITE(*,9003) LRANK,LORDER,LISOLD
 9003   FORMAT(' ECSWRT: Error allocating Zebra space.  Pointers: ',3I9)
        IERR = -3
        GOTO 999
      ENDIF
C
C  Sort internal storage and copy into order.  Use 2D sorting routines..
C
      IF( LDDBG.GT.10 ) WRITE(*,1001) NLEVTS,IRUN
      CALL ECSORT(NLEVTS,IRECEC,1,IQ(LDATA+3),
     +     IQ(LORDER+1),IQ(LRANK+1),IERR)
      CALL ECFLIP(NLEVTS,IRECEC,IQ(LDATA+3),
     +     IQ(LORDER+1),IQ(LRANK+1),IDAT,IERR)
      IF( LDDBG.GT.10 ) WRITE(*,1004) NLEVTS,IRUN
 1004 FORMAT(' ECSWRT: Sorted and moved ',I6,' events in run ',I6)
C
C  Done with temporary sorting arrays.  
C
      CALL MZDROP(IXDDAD,LRANK,' ')
      CALL MZDROP(IXDDAD,LORDER,' ')
      LRANK=0
      LORDER=0
C
C  Check for duplicate entries.   This does occasionally happen if the
C  same event is processed twice or if the UE file has been run on the
C  same input file multiple times.
C
      CALL ECDUPS(IQ(LDATA+3),NLEVTS,IRECEC,JEVNT,JFID,NDUP)
C
C  Setup for rest of processing.  Start by computing space needed in
C  catalog...
C
      NLEVTS=NLEVTS-NDUP
      IF( LDDBG.GT.10 ) WRITE(*,1006) NLEVTS
 1006 FORMAT(' ECSWRT: After removing duplicates,',I6,' events left.')
      NENEW=NLEVTS
C
C  See if this is a new run.  If so, space needed is already computed.
C
      IF( LRDAT.LE.0 ) GOTO 20
C
C   Not a new run.  Compute space allowing for events in new sample
C   which were already in catalog.
C
      CALL ECUNIQ(IRECEC,IQ(LRDAT+1),NOLD,IQ(LDATA+3),NLEVTS,
     +   IQ(LISOLD+1),NENEW,IERR)
      IF( IERR.NE.0 ) THEN
        IF(LDDBG.GT.10) WRITE(*,9002) IERR,'ECUNIQ'
        IERR = -4
        GOTO 999
      ENDIF
C
C  Merge any existing events with new ones...
C
 20   CONTINUE
      NTOT=NOLD+NENEW
      CALL MZBOOK(IXDDAD,LDNEW,LECHD,-LLDNEW,'DNEW',0,0,NTOT*IRECEC
     +   ,2,-1)
      CALL ECMERG(IRECEC,IQ(LRDAT+1),NOLD,IQ(LDATA+3),NLEVTS,
     +   IQ(LISOLD+1),1,IQ(LDNEW+1),NTOT,IERR)
      IF( IERR.NE.0 ) THEN
        IF( LDDBG.GT.10 ) WRITE(*,9002) IERR,'ECMERG'
        IERR = -6
        GOTO 999
      ENDIF
C
C  This is start of code which actually modifies event catalog.  Move
C  write protection here.
C
      ILUN=IQ(LECHD+JLUN)
      CALL ECDIRT(ILUN,IRUN,IERR)
      IF( IERR.NE.0 ) THEN
        IERR = -1
        GOTO 999
      ENDIF
C
C  Make necessary space allocation in catalog and update variables...
C  Write updated header(?) and run sections in ECIRUN
C
      CALL ECIRUN(IREC,NENEW,NTOT,IERR)
      IF( IERR.NE.0 ) THEN
        IF(LDDBG.GT.10) WRITE(*,9002) IERR,'ECIRUN'
        IERR = -5
        GOTO 999
      ENDIF
C
C  Write the updated run data, tweak the header
C
      CALL ECFIND(IQ(LRUNS+1),IQ(LECHD+NDEC+JEC_IECRUN),IRECEC,
     +   IQ(LDATA+1),1,IREC1,IDAT,IERR)
      IF( IERR.LT.0 ) THEN
        IF(LDDBG.GT.10) WRITE(*,9007) IERR
 9007   FORMAT(' ECSWRT: Error ',I10,' returned from ECFIND for insert')
        IERR = -7
        GOTO 999
      ENDIF
      CALL ECWRIT(IQ(LDNEW+1),IDAT(JLRBEG),NTOT,IERR)
C
C  The writing to the catalog is finished.  Clear dirty bit.
C
      CALL ECDIRT(ILUN,0,IERR)
      IF( IERR.NE.0 ) THEN
        IERR = -8
        GOTO 999
      ENDIF
C
 999  CONTINUE
C
C  We have just completely finished with one run.  Release all working
C  banks... (Keep just 'RUNS' bank)
C
      IF(LRANK.GT.0 ) CALL MZDROP(IXDDAD,LRANK,' ')
      IF(LORDER.GT.0) CALL MZDROP(IXDDAD,LORDER,' ')
      IF(LISOLD.GT.0) CALL MZDROP(IXDDAD,LISOLD,' ')
      IF(LDNEW.GT.0 ) CALL MZDROP(IXDDAD,LDNEW,' ')
      IF(LDATA.GT.0 ) CALL MZDROP(IXDDAD,LDATA,' ')
      IF(LRDAT.GT.0 ) CALL MZDROP(IXDDAD,LRDAT,' ')
      LRANK=0
      LORDER=0
      LISOLD=0
      LDNEW=0
      LDATA=0
      LRDAT=0
C
      RETURN
 1001 FORMAT(' ECSWRT: About to sort for ',I6,' events in run',I7)
 1002 FORMAT(' ECSWRT: Run ',I6,' has entries from ',I6,' to ',I6,
     +   ' and total space for',I6,' entries (overlap=',L1,')')
 1003 FORMAT(' ECSWRT: Run ',I6,' has overlapping entries.')
 1005 FORMAT(' ECSWRT: Run/Event ',I8,'/',I8,' rec/ierr=',I7,'/',I3)
      END
