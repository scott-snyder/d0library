      SUBROUTINE TDBINI(CRUN,MAX_TRDCRT,PD_INI,GN_INI,DB_OPENED,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialzation routine for Pedestals.
C-                         To be called at the beginning of every run.
C-   Inputs  : CRUN = Current Run Number (I6 format)
C-             MAX_TRDCRT = number of crates being read in (count from 0)
C-             CALTYPE = Calibration data requested for declared by a set bit.
C-                      - 3 bits assigned
C-                      Least signficant bit = pedestal's
C-                      Second bit = Gain's
C-                      Most significant bit = Electronic T0's.
C- An input of 0 or 7 represents ALL calibration data has been requested for.
C-   Outputs : none
C-   Controls: IOK = .TRUE. if everything goes well.
C-
C-   Created  14-DEC-1989   Srini Rajagopalan
C-   Updated  20-JUN-1990   Srini Rajagopalan, Accomodate Gain's and T0's
C-   Updated  20-SEP-1990   JFG adapted to TRD 
C-   Updated  13-AUG-1991   Susan K. Blessing   Change call to TDBINI to 
C-    use logicals for pedestal and gain initialization.  This is faster,
C-    much more straightforward, and is how the other CD subdetectors do it.
C-   Updated  13-AUG-1991   Susan K. Blessing   Add logical DB_OPENED to
C-    flag if DBCLB_INITIALIZE has been called for this run.
C-   Updated  20-SEP-1990   JFG remove calls to BLTxxH but added check on 
C-                              the existance of these banks
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER LENGTH,IER
      INTEGER CRUN
      INTEGER MAX_TRDCRT
      INTEGER GZSTRD, GZTPDH, GZTGAI
C
      LOGICAL IOK,FIRST
      LOGICAL PD_INI,GN_INI
      LOGICAL DB_OPENED
C
      SAVE FIRST
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      IOK = .TRUE.
C
C  *** Make top level Logical STP tree structure if necessary.
C
      IF (FIRST) THEN
        IF (LSTRD.LE.0) LSTRD = GZSTRD()
        IF (LSTRD.LE.0) THEN
          CALL ERRMSG('TRDPAR','TDBINI','STRD bank not found','W')
          IOK = .FALSE.
          GOTO 999
        ENDIF
        IF (LTPDH.LE.0) LTPDH = GZTPDH()
        IF (LTPDH.LE.0) THEN
          CALL ERRMSG('TRDPAR','TDBINI','TPDH bank not found','W')
          IOK = .FALSE.
          GOTO 999
        ENDIF
        IF (LTGAI.LE.0) LTGAI = GZTGAI()
        IF (LTGAI.LE.0) THEN
          CALL ERRMSG('TRDPAR','TDBINI','TGAI bank not found','W')
          IOK = .FALSE.
          GOTO 999
        ENDIF
C "General operations will have to be included" (TGEN/TROP)
        FIRST = .FALSE.
      ENDIF
C
C  *** Close DBL3 database if necessary.
C
      IF (DB_OPENED) THEN
        CALL DBCLB_FINISH
        DB_OPENED = .FALSE.
      END IF
C
      IF (PD_INI .AND. IOK) 
     &                  CALL TPDINI(CRUN,MAX_TRDCRT,DB_OPENED,IOK)
      IF (GN_INI  .AND. IOK) 
     &                  CALL TGNINI(CRUN,MAX_TRDCRT,DB_OPENED,IOK)
C
  999 RETURN
      END
