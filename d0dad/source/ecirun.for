      SUBROUTINE ECIRUN(IRUN,NNEED,NTOT,IERR)
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS : Do the necessary work to make space for a
C-      set of event records in an event catalog.
C-
C-   INPUTS  : IRUN - Record number in Run section for insertion.
C-             NNEED - Number of entries needed for current run.
C-   OUTPUTS : NTOT  - 
C-   CONTROLS: 
C-
C-   CREATED   10-NOV-1993   John D Hobbs
C-   MODIFIED  08-Jun-1994   John D Hobbs - Fix bug in determining last
C-     record of event data as stored in run section for runs other than
C-     first or last...
C-   MODIFIED  21-DEC-1994   JDH - Add computation of JEC_EXTRA
C-   MODIFIED  16-JAN-1994   JDH - Add checksums to header/run section
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER INREC,NNEED,IERR,NTOT
      LOGICAL LISNEW,LSTAMPED,LSTATE,SPACE_OK
      INTEGER IRECEC,IRPREC,IRUN,IREC0,IREC1,NIN,ISPACE,DATE_TIME(*)
      INTEGER NRUNS,NRMAX,LREC,J,IRDAT(KRECEC),NPREC,ILUN,IOFFS,IECDLT
      INTEGER NLLAST,ILAST,NWPREC,NWIN,NWTOT,IDATE,ITIME,IDT
      INTEGER CHECKSUM32
      SAVE    IDATE,ITIME,LSTAMPED
      DATA    LSTAMPED/.FALSE./
C----------------------------------------------------------------------
C
      IERR=0
      IF( .NOT.LSTAMPED ) CALL DATIME(IDATE,ITIME)
      IDT=IOR(IDATE,ISHFT(ITIME,20))
      IRECEC=IQ(LECHD+NDEC+JEC_IRECEC)
      IRPREC=IQ(LECHD+NDEC+JEC_IRPREC)
      IECDLT=IQ(LECHD+NDEC+JEC_IECDLT)
      NRUNS=IQ(LECHD+NDEC+JEC_IECRUN)
      NRMAX=IQ(LECHD+NDEC+JEC_IRUN1)-IQ(LECHD+NDEC+JEC_IRUN0)+1
      NRMAX=IRPREC*NRMAX
      NWPREC=IRECEC*IRPREC
      ISPACE=0
      NPREC=0
C
C  If no space is needed, just update the timestamp...
C
      IF( NNEED.LE.0 ) GOTO 30
C
C  Check if run is not in database.  (Run would be loaded in ECSWRT)
C
      LISNEW=LRDAT.LE.0
      IF(LISNEW) THEN
        IF( NRUNS.GE.NRMAX ) THEN
          IERR = -1
          GOTO 999
        ENDIF
        GOTO 10
      ENDIF
C
C  It's there.  Check if a PUSH for space is needed...
C
      IREC0=IQ(LRUNS+IRECEC*(IRUN-1)+JLRBEG)
      IREC1=IQ(LRUNS+IRECEC*(IRUN-1)+JLREND)
      NIN=IQ(LRUNS+IRECEC*(IRUN-1)+JNEVTS) 
      IF( LDDBG.GT.10 ) WRITE(*,1004) IDATE,IREC0,IREC1,NIN
C
      ISPACE=IRPREC*(IREC1-IREC0+1)-NIN
      IF( ISPACE.GE.NNEED ) GOTO 30
C
C  Space is needed.  Compute the number of physical records required.
C  (Compare number required with minimum additional allocation as well)
C  Do the push and insert null event records...
C
 10   CONTINUE
      NPREC=(NNEED-ISPACE)/IRPREC + 1
      IF( MOD(NPREC,IECDLT).NE.0 ) NPREC=IECDLT*((NPREC/IECDLT)+1)
      J=IQ(LECHD+NDEC+JEC_IECRUN)
      IQ(LECHD+NDEC+JEC_EXTRA)=MAX(IQ(LECHD+NDEC+JEC_EXTRA)-NPREC,0)
      IF( LISNEW ) THEN
        ILAST=IRUN
        IOFFS=IRECEC
      ELSE
        ILAST=IRUN+1
        IOFFS=0
      ENDIF
      DO WHILE( J.GE.ILAST )
C
        LREC=LRUNS+IRECEC*(J-1)+1
        CALL UCOPY(IQ(LREC),IRDAT,IRECEC)
C    Push in ECPUSH after allocating a run-size buffer.
        NWIN=IQ(LPREC-1)
        NWTOT=IRECEC*IRPREC*(IRDAT(JLREND)-IRDAT(JLRBEG)+1)
        IF( NWIN.LT.NWTOT ) CALL MZPUSH(IXDDAD,LPREC,0,NWTOT-NWIN,'I')
        CALL ECPUSH(IQ(LECHD+JLUN),IRDAT(JLRBEG),IRDAT(JLREND),NPREC,
     +       IQ(LPREC+1),NWPREC,IERR)
        IF( IERR.NE.0 ) THEN
          IERR = -5
          GOTO 999
        ENDIF
C
        IRDAT(JLRBEG)=IRDAT(JLRBEG)+NPREC
        IRDAT(JLREND)=IRDAT(JLREND)+NPREC
C      LREC is an unprotected link.  ECPUSH might cause grabage collection
        LREC=LRUNS+IRECEC*(J-1)+1
        CALL UCOPY(IRDAT,IQ(LREC+IOFFS),IRECEC)
        J=J-1
C
      ENDDO
C
C  Restore the page record to its usual size
C
      IF( IQ(LPREC-1).NE.NWPREC ) THEN
        NWIN=NWPREC-IQ(LPREC-1)
        CALL MZPUSH(IXDDAD,LPREC,0,NWIN,'I')
      ENDIF
C
C  Update the run section and header to reflect the new space.
C
 30   CONTINUE
      IOFFS=IRECEC*(IRUN-1)+1
      CALL UCOPY(IQ(LRUNS+IOFFS),IRDAT,IRECEC)
      SPACE_OK = NNEED.LT.0 .OR. ISPACE.GE.NNEED

C  Either first run or after last run with space needed

      IF(IQ(LECHD+NDEC+JEC_IECRUN).EQ.(IRUN-1) .AND. .NOT.SPACE_OK) THEN
C      Run section
        IRDAT(JRUNNO)=IQ(LDATA+1)
        IRDAT(JLRBEG)=IQ(LECHD+NDEC+JEC_IEVT1)+1
        IRDAT(JLREND)=IRDAT(2)+NPREC-1
        IRDAT(JNEVTS)=NTOT
        IRDAT(JCHECK)=CHECKSUM32(IQ(LDNEW+1),NTOT*IRECEC,0)
        IRDAT(JDATTIM)=IDT
        CALL UCOPY(IRDAT,IQ(LRUNS+IOFFS),IRECEC)
C      Header
        IQ(LECHD+NDEC+JEC_IECRUN)=IQ(LECHD+NDEC+JEC_IECRUN)+1
        IQ(LECHD+NDEC+JEC_IEVT1)=IQ(LECHD+NDEC+JEC_IEVT1)+NPREC
        NLLAST = IQ(LECHD+NDEC+JEC_IEVT1)-IQ(LECHD+NDEC+JEC_IEVT0)
        NLLAST = IRPREC*NLLAST + IRPREC*MOD(NNEED,IRPREC)
        IQ(LECHD+NDEC+JEC_IECEVT)=NLLAST
        NRUNS=IQ(LECHD+NDEC+JEC_IECRUN)*IRECEC
        IQ(LECHD+NDEC+JEC_RCHECK)=CHECKSUM32(IQ(LRUNS+1),NRUNS,0)

C  Run in range of existing runs and space needed

      ELSEIF(IQ(LECHD+NDEC+JEC_IECRUN).GE.IRUN .AND. .NOT.SPACE_OK) THEN
C      Run section
        IRDAT(JRUNNO)=IQ(LDATA+1)
        IRDAT(JLRBEG)=IRDAT(2)
        IF(LISNEW) THEN
          IRDAT(JLREND)=IRDAT(JLRBEG)+NPREC-1
        ELSE
          IRDAT(JLREND)=IRDAT(JLREND)+NPREC
        ENDIF
        IRDAT(JNEVTS)=NTOT
        IRDAT(JCHECK)=CHECKSUM32(IQ(LDNEW+1),NTOT*IRECEC,0)
        IRDAT(JDATTIM)=IDT
        CALL UCOPY(IRDAT,IQ(LRUNS+IOFFS),IRECEC)
C      Header
        IF(LISNEW) IQ(LECHD+NDEC+JEC_IECRUN)=IQ(LECHD+NDEC+JEC_IECRUN)+1
        ILAST=IRECEC*(IQ(LECHD+NDEC+JEC_IECRUN)-1)
        IF( IQ(LRUNS+ILAST+3).GT.IQ(LECHD+NDEC+JEC_IEVT1))
     +     IQ(LECHD+NDEC+JEC_IEVT1)=IQ(LRUNS+ILAST+JLREND)
        NLLAST = IQ(LECHD+NDEC+JEC_IEVT1)-IQ(LECHD+NDEC+JEC_IEVT0)
        NLLAST = IRPREC*NLLAST + IRPREC*MOD(NNEED,IRPREC)
        IQ(LECHD+NDEC+JEC_IECEVT)=NLLAST
        NRUNS=IQ(LECHD+NDEC+JEC_IECRUN)*IRECEC
        IQ(LECHD+NDEC+JEC_RCHECK)=CHECKSUM32(IQ(LRUNS+1),NRUNS,0)

C  No new space needed, just update number of events, checksum
C  and timestamp...

      ELSEIF( SPACE_OK ) THEN
        IRDAT(JNEVTS)=NTOT
        IRDAT(JCHECK)=CHECKSUM32(IQ(LDNEW+1),NTOT*IRECEC,0)
        IRDAT(JDATTIM)=IDT
        CALL UCOPY(IRDAT,IQ(LRUNS+IOFFS),IRECEC)
        NRUNS=IQ(LECHD+NDEC+JEC_IECRUN)*IRECEC
        IQ(LECHD+NDEC+JEC_RCHECK)=CHECKSUM32(IQ(LRUNS+1),NRUNS,0)

C  Error

      ELSE
        IERR = -2
        GOTO 999
      ENDIF
C
C  Write out the updated run section
C
      NRUNS=IQ(LRUNS-1)/IRECEC
CJDH      NRUNS=IQ(LECHD+NDEC+JEC_IRUN1)-IQ(LECHD+NDEC+JEC_IRUN0)+1
      CALL ECWRIT(IQ(LRUNS+1),IQ(LECHD+NDEC+JEC_IRUN0),NRUNS,IERR)
      IF( IERR.NE.0 ) THEN
        IERR = -3
      ENDIF
C
C  Write out the updated header
C
      CALL ECHUPD(ILUN,IERR)
      IF( IERR.NE.0 ) THEN
         WRITE(*,9901) IERR,ILUN
 9901    FORMAT(' ECIRUN: Error ',I4, ' from ECHUPD to unit',I3)
         IERR = -4
         GOTO 999
      ENDIF
C
 999  CONTINUE
      RETURN
C
C
      ENTRY ECSET_TIMESTAMP(DATE_TIME,LSTATE)
      IDATE=DATE_TIME(1)
      ITIME=DATE_TIME(2)
      LSTAMPED=LSTATE
      RETURN
C
 1001 FORMAT(' ECIRUN: Run ',I7,' at record ',I7,' in run section.')
 1002 FORMAT(' ECIRUN: New run ',I7,' to insert at record ',I7)
 1003 FORMAT(' ECIRUN: Inserting run ',I7,' required',I7,' pushes')
 1004 FORMAT(' ECIRUN: (',I6,') Run record range from ',I6,' to',I6,
     +  '. Space requested for ',I6,' events')
 1005 FORMAT(' ECIRUN: First run ',I7,' to insert at record ',I7)
      END
