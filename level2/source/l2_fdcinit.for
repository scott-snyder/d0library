      SUBROUTINE L2_FDCINIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Parameter initialization for L2_TRAK package
C-                         ( modified from FTRPAR() )
C-   Outputs : NONE
C-
C-   Created  12-OCT-1988   Daria Zieminska
C-   Updated  30-JAN-1990   Jeffrey Bantly  changed to RCP read-in filename
C-   Updated   7-MAY-1990   Susan K. Blessing  put MAX_FDCRT in for FDBINI
C-   Updated  20-AUG-1990   Susan K. Blessing  put CALTYPE in to initialize
C-                                             pedestals, gains, T0's
C-   Updated  28-NOV-1990   Jeffrey Bantly  re-organization inc
C-                                          UNIX-compatibility
C-   Updated  24-JAN-1991   Jeffrey Bantly  improve message handling
C-   Updated  14-JUN-1991   Jeffrey Bantly  allow multiple MC runs
C-   Updated  16-AUG-1991   Robert E. Avery  Include FDC shift, and move
C-                                      FPLTRK,FSPLNK calls to FTRINI
C-   Updated  21-OCT-1991   Robert E. Avery  Move geometry initialization to
C-                                      subroutine FGEOM_INIT.
C-   Updated  10-DEC-1991   Robert E. Avery  Move FGEOM_INIT into FTRINI,
C-                                      Call FTRINI in case it wasn't already.
C-   Modified 20-May-1992   Yi-Cheng Liu   to initialize parameters for
C-                                         FDC in Level-2, FTRINI renamed
C-                                         to be L2_FTRINI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER RUN,RUNNO,IER
      INTEGER MAX_FDCRT,PREV_RUN,RUNTYPE
C
      CHARACTER*5 GNSTR,PEDSTR,TIMSTR
      CHARACTER*80 TEXT
C
      LOGICAL OK, MCDATA, FIRST
      LOGICAL GN_INI,PD_INI,TM_INI
      LOGICAL FILL_STP
      LOGICAL EZERROR
      LOGICAL L2_FTRINI
C
      SAVE FIRST,MAX_FDCRT,PREV_RUN,RUNTYPE
      DATA MCDATA/.FALSE./    !????? Always so or depends on data ????
      DATA FIRST/.TRUE./
      DATA PD_INI,GN_INI,TM_INI/.TRUE.,.FALSE.,.FALSE./
      DATA PREV_RUN/99999/
      DATA FILL_STP /.FALSE./
C-------------------------------------------------------------------------
C      RUN=RUNNO()
C      IF(RUN .EQ. PREV_RUN .AND. .NOT.FIRST) THEN
C        FTRPAR=.TRUE.  ---- Need to be deleted, but check to see what 
C        GOTO 999           ! else to do in this step.
C       ELSE
C        PREV_RUN=RUN
C      ENDIF
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        OK = L2_FTRINI()    ! Just in case it hasn't been called yet
        IF ( .NOT.OK ) THEN
          GOTO 999
        ENDIF
      ENDIF
C
      CALL EZPICK('FTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('FTRAKS','L2_FDCINIT',
     &              'FTRAKS_RCP not found.','F')
      ENDIF
      CALL EZGET('RUNTYPE',RUNTYPE,IER)
      CALL EZGET('FILL_STP',FILL_STP,IER)
      CALL EZRSET
C
C  Check consistancy of RCP file and data type.
C
      MCDATA =  IQ(LHEAD+1) .GT. 1000 ! 5-999 : REAL EVENT DATA
      IF(MCDATA) THEN                 ! 1005-1999 : MC DATA
        IF(RUNTYPE.NE.0) THEN
          CALL ERRMSG('FTRAKS','L2_FDCINIT',
     &        'Inconsistant FTRAKS_RCP for MC datafile','F')
        ENDIF
      ELSE
        IF(RUNTYPE.EQ.0) THEN
          CALL ERRMSG('FTRAKS','L2_FDCINIT',
     &        'Inconsistant FTRAKS_RCP for non-MC datafile','F')
        ENDIF
      ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   TEMPORARY SAVER ! ( YI-CHENG LIU )
C
C      PREV_RUN = RUNNO()
C      IF (PREV_RUN.EQ.0) PREV_RUN=99999
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  Read in FDC STP values from DBL3, if necessary
C
      IF (.NOT.MCDATA) THEN          ! Monte Carlo data, done
C                                    ! Real data, read values if nec.
        IF(RUNTYPE.LE.5) THEN
          CALL FCODER_INI(RUNTYPE,RUN)
        ENDIF
C
C  Get calibration choices
C
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('GN_INI',GN_INI,IER)
        CALL EZGET('PD_INI',PD_INI,IER)
        CALL EZGET('TM_INI',TM_INI,IER)
        CALL EZGET('MAX_FDCRT',MAX_FDCRT,IER)
        CALL EZRSET
        GNSTR='     '
        IF(GN_INI) GNSTR='GAINS'
        PEDSTR='     '
        IF(PD_INI) PEDSTR='PEDS '
        TIMSTR='     '
        IF(TM_INI) TIMSTR='TIMES'
        WRITE(TEXT,101) GNSTR,PEDSTR,TIMSTR
  101   FORMAT(' Reading from FDC DBL3 database, the ',A5,',',A5,',',A5)
C
        CALL INTMSG(TEXT)
        CALL FDBINI(PREV_RUN,MAX_FDCRT,PD_INI,TM_INI,GN_INI,OK)
        IF (.NOT.OK) THEN
          CALL INTMSG(' L2_FDCINIT: Error in FDBINI.')
        ELSE
          CALL INTMSG(' Finished reading FDC DBL3 database.')
        END IF
      END IF
C
C Modify STP banks with parameters from RCP:
C( Not sure if need to do this at the time. )
      IF ( FILL_STP ) THEN
        CALL FDC_FILL_STP
      ENDIF
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
