      SUBROUTINE FDBINI(CRUN,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialzation routine for Pedestals.
C-                         To be called at the beginning of every run.
C-   Inputs  : CRUN = Current Run Number (I6 format)
C-
C-   Outputs : IOK = .TRUE. if everything goes well.
C-
C-   Created  14-DEC-1989   Srini Rajagopalan
C-   Updated  20-JUN-1990   Srini Rajagopalan, Accomodate Gain's and T0's
C-   Updated   4-APR-1991   Jeffrey Bantly  read DBL3 file name from RCP 
C-   Updated  13-AUG-1991   Susan K. Blessing   Add logical DB_OPENED to 
C-    flag if DBCLB_INITIALIZE has been called for this run.  
C-   Updated   4-NOV-1991   Jeffrey Bantly  remove calls to BLFxxH routines
C-                                          substitute link checks 
C-   Updated  29-SEP-1992   Robert E. Avery   Get offline dbl3 constants,
C-     by calling new routines, FxxINI_OFFLINE. 
C-     Remove GN_INI,PD_INI,TM_INI from argument list, get from FTRAKS_RCP. 
C-   Updated   1-FEB-1993   Robert E. Avery   Add RCP parameter, DBG_FDBRUN.
C-      If DBG_FDBOFF is true, use DBG_FDBRUN to determine which 
C-      DBL3 file to open (using DBCLB_INITIALIZE_FORCE_RUN).
C-   Updated  13-MAY-1993   Robert E. Avery  Move FILL_STP to this routine,
C-      so that it is done before OFFLINE calib read in. Also change name
C-      of RCP parameter DBG_FDBRUN to FDBRUN (and remove DBG_FDBRUN).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER CRUN
      INTEGER MAX_FDCRT
      INTEGER GZSFDC,GZFGNH,GZFPDH,GZFTMH
      INTEGER FDBRUN 
C
      LOGICAL IOK,FIRST,IER
      LOGICAL GN_INI,PD_INI,TM_INI
      LOGICAL GN_PROC_INI,PD_PROC_INI,TM_PROC_INI
      LOGICAL FTIMEP
      LOGICAL DB_OPENED
      LOGICAL FILL_STP
C
      SAVE FIRST,DB_OPENED,FILL_STP
C
      DATA FDBRUN /-1/
      DATA FIRST /.TRUE./
      DATA DB_OPENED/.FALSE./
C----------------------------------------------------------------------
C
      IOK = .TRUE.
C
C  *** Check top level Logical STP tree structure.
C
      IF (FIRST) THEN
        IF (LSFDC.LE.0) LSFDC = GZSFDC()
        IF (LSFDC.LE.0) THEN
          CALL ERRMSG('FTRAKS','FDBINI','SFDC bank not found','W')
          IOK = .FALSE.
          GOTO 999
        ENDIF
        IF (LFPDH.LE.0) LFPDH = GZFPDH()
        IF (LFPDH.LE.0) THEN
          CALL ERRMSG('FTRAKS','FDBINI','FPDH bank not found','W')
          IOK = .FALSE.
          GOTO 999
        ENDIF
        IF (LFGNH.LE.0) LFGNH = GZFGNH()
        IF (LFGNH.LE.0) THEN
          CALL ERRMSG('FTRAKS','FDBINI','FGNH bank not found','W')
          IOK = .FALSE.
          GOTO 999
        ENDIF
        IF (LFTMH.LE.0) LFTMH = GZFTMH()
        IF (LFTMH.LE.0) THEN
          CALL ERRMSG('FTRAKS','FDBINI','FTMH bank not found','W')
          IOK = .FALSE.
          GOTO 999
        ENDIF
C
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('GN_INI',GN_INI,IER)
        CALL EZGET('PD_INI',PD_INI,IER)
        CALL EZGET('TM_INI',TM_INI,IER)
        CALL EZGET('GN_PROC_INI',GN_PROC_INI,IER)
        CALL EZGET('PD_PROC_INI',PD_PROC_INI,IER)
        CALL EZGET('TM_PROC_INI',TM_PROC_INI,IER)
        CALL EZGET('FDBRUN_FORCE',FDBRUN,IER)
        CALL EZGET('MAX_FDCRT',MAX_FDCRT,IER)
        CALL EZGET('FILL_STP',FILL_STP,IER)
        CALL EZRSET
C
C First modify STP banks with parameters from RCP:
C
        IF ( FILL_STP ) THEN
          CALL FDC_FILL_STP
        ENDIF
C
        FIRST = .FALSE.
      ENDIF
C
C Allow possibility to force using a particular database for offline constants:
C
      IF ( FDBRUN.GT.-1 ) THEN    
        CALL DBCLB_INITIALIZE_FORCE_RUN(FDBRUN)        
      ENDIF
C
C Start with offline banks
C
      IF (PD_PROC_INI .AND. IOK) 
     &          CALL FPDINI_OFFLINE(CRUN,DB_OPENED,IOK)
      IF (GN_PROC_INI .AND. IOK) 
     &          CALL FGNINI_OFFLINE(CRUN,DB_OPENED,IOK)
      IF (TM_PROC_INI .AND. IOK) 
     &          CALL FTMINI_OFFLINE(CRUN,DB_OPENED,IOK)
C
C If necessary, close offline database, then back to std. online database.      
      IF ( FDBRUN.GT.-1 ) THEN    
        IF (DB_OPENED) THEN
          CALL DBCLB_FINISH
          DB_OPENED = .FALSE.
        END IF
        CALL DBCLB_INITIALIZE_FORCE_RUN(CRUN)     
      END IF
      IOK=.TRUE.
C
C Update with online banks, if requested
C
      IF (PD_INI .AND. IOK) 
     &          CALL FPDINI(CRUN,MAX_FDCRT,DB_OPENED,IOK)
      IF (GN_INI .AND. IOK) 
     &          CALL FGNINI(CRUN,MAX_FDCRT,DB_OPENED,IOK)
      IF (TM_INI .AND. IOK) 
     &          CALL FTMINI(CRUN,MAX_FDCRT,DB_OPENED,IOK)

C
C  *** Close DBL3 database if necessary.
C
      IF (DB_OPENED) THEN
        CALL DBCLB_FINISH
        DB_OPENED = .FALSE.
      END IF
C
C-------------------------------------------------------------------------
  999 RETURN
      END
