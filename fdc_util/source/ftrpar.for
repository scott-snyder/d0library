      FUNCTION FTRPAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Parameter initialization for FTRAKS package
C-
C-   Outputs : .TRUE. if run is to be processed (.FALSE. if skipped)
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
C-   Updated  13-AUG-1992   Robert E. Avery  Add call to FDC_MCCHECK for MC.
C-   Updated  16-SEP-1992   Susan K. Blessing  Remove message about reading
C-    DBL3 file.
C-   Updated   3-DEC-1992   Robert E. Avery   Change argument list for
C-      FDBINI. Get xx_INI parameters from within FDBINI.
C-   Updated  11-DEC-1992   Susan K. Blessing  Add BYPASS_DBL3_ERROR
C-    switch.
C-   Updated   1-FEB-1993   Robert E. Avery  MC defined as RUNTYPE <= 0,
C-    not necessarily =0 (for special cases).
C-   Updated   5-MAY-1993   Robert E. Avery  Move DBL3 stuff to Event entry. 
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL FTRPAR
C
      INTEGER RUN,RUNNO,IER
      INTEGER PREV_RUN,RUNTYPE
C
      LOGICAL TRACKING_DONE 
      LOGICAL OK, MCDATA, FIRST
      LOGICAL EZERROR
      LOGICAL FTRINI
C
      SAVE FIRST, PREV_RUN, RUNTYPE
C
      DATA FIRST/.TRUE./
      DATA PREV_RUN/-1/
C
C-------------------------------------------------------------------------
      FTRPAR=.FALSE.
      RUN=RUNNO()
      IF (RUN .EQ. PREV_RUN .AND. .NOT.FIRST) THEN
        FTRPAR=.TRUE.
        GOTO 999
      ELSE
        PREV_RUN=RUN
      ENDIF
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        OK = FTRINI()    ! Just in case it hasn't been called yet
        IF ( .NOT.OK ) THEN
          FTRPAR = .FALSE.
          GOTO 999
        ENDIF
C
        CALL EZPICK('FTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('FTRAKS','FTRPAR','FTRAKS_RCP not found.','F')
        ENDIF
        CALL EZGET('RUNTYPE',RUNTYPE,IER)
        CALL EZRSET
C
      ENDIF
C
C  Check consistancy of RCP file and data type.
C
      MCDATA =  IQ(LHEAD+1) .GT. 1000
      IF(MCDATA) THEN
        IF(RUNTYPE.GT.0) THEN
          CALL ERRMSG('FTRAKS','FTRPAR',
     &        'Inconsistant FTRAKS_RCP for MC datafile','F')
        ENDIF
C
C  Check if old version of MC data, if so select correct stp banks.
C
        CALL FDC_MCCHECK
      ELSE
        IF(RUNTYPE.LE.0) THEN
          CALL ERRMSG('FTRAKS','FTRPAR',
     &        'Inconsistant FTRAKS_RCP for non-MC datafile','F')
        ENDIF
      ENDIF
C
      CALL FCODER_INI(RUNTYPE,RUN)
C
C----------------------------------------------------------------------
      FTRPAR=.TRUE.
C
  999 CONTINUE
      RETURN
      END
