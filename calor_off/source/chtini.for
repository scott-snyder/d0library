      FUNCTION CHTINI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Job initialization step for CAHITS calorimeter hits package
C-
C-   Returned value  : TRUE if success
C-   Inputs  : none
C-   Outputs : none
C-   Controls: CAHITS_RCP
C-
C-   Created  18-JAN-1989   Serban D. Protopopescu
C-   Updated  1-APR-1992   Chip Stewart - read CAD_STPFILE and CSF_STPFILE
C-   Updated  11-FEB-1994   Jan Guida  Add reading of CCPT_STPFILE 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CHTINI
      CHARACTER*32 FILNAM,CAPFIL
      INTEGER IER,TRULEN,LENF,LENC
      LOGICAL FIRST,DO_CMODGN,LBUILD
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCAEP.INC'
      CHARACTER CAD_STPFILE*80,CSF_STPFILE*80,RCP_FILE*80,RCPE_FILE*80
      CHARACTER CCPT_STPFILE*80
      CHARACTER MSG*80
      PARAMETER( RCP_FILE = 'CAHITS_RCP' )      ! Logical RCP FILE
      PARAMETER( CAD_STPFILE = 'CAD_STPFILE' )  ! Logical for CAD lookup table 
      PARAMETER( CSF_STPFILE = 'CSF_STPFILE' )  ! Logical for CSF lookup table 
      PARAMETER( CCPT_STPFILE = 'CCPT_STPFILE' ) ! Logical for cap lookup table 
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        CHTINI=.FALSE.
C
        PTZFLG = .TRUE.
C       read in files
        CALL INRCP(RCP_FILE,IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 999              ! failed
C
        RCPE_FILE = RCP_FILE
        RCPE_FILE = RCPE_FILE(1:TRULEN(RCPE_FILE))//'E'
        CALL INRCPE(RCPE_FILE,IER)     ! read overwrite file (RCPE)
        IF(IER.EQ.0) CALL ERRMSG('CAHITS','CHTINI',
     &  ' Default CAHITS_RCP modified','W')
C
        CALL EZPICK(RCP_FILE)              ! select CAHITS bank
        CALL EZERR(IER)
        IF(IER.EQ.0) THEN
          CHTINI=.TRUE.
          CALL EZRSET
        ELSE
          CALL ERRMSG('CAHITS','CHTINI',
     &      ' CAHITS_RCP file does not have a CAHITS bank.','W')
        ENDIF
C
C ****  Get name of CAD ADDRESS LOOK-UP TABLE FILE
C
        CALL EZPICK ( RCP_FILE )
        CALL EZGETS (CAD_STPFILE,1,FILNAM,LENF,IER)
        CALL EZGETS (CCPT_STPFILE,1,CAPFIL,LENC,IER)
        CALL EZGET ('DO_CMODGN',DO_CMODGN,IER)
        CALL EZRSET
        CHTINI = CHTINI .AND. (IER .EQ. 0)
C
C ****  Read in CAD ADDRESS LOOKUP TABLE 
C
        IF ( CHTINI ) THEN
          CALL CADSTP (FILNAM(1:LENF),IER)
          CHTINI = IER .EQ. 0
          MSG = 'READ '//FILNAM(1:LENF) 
          IF(IER.NE.0) CALL ERRMSG('CADSTP FAILS','CHTINI',MSG,'W')
          IF (DO_CMODGN) THEN
            CALL CAPSTP (CAPFIL(1:LENC),IER)
            CHTINI = IER .EQ. 0
            MSG = 'READ '//CAPFIL(1:LENC) 
            IF(IER.NE.0) CALL ERRMSG('CAPSTP FAILS','CHTINI',MSG,'W')
          ENDIF
        ELSE
          MSG = 'NO READ'//CAD_STPFILE//' IN '//RCP_FILE
          CALL ERRMSG('CAHITS_FAIL','CHTINI',MSG,'W')
        ENDIF
C
C ****  Get name of CSF_STPFILE SAMPLING WEIGHT LOOK-UP TABLE 
C
        CALL EZPICK ( RCP_FILE )
        CALL EZGETS (CSF_STPFILE,1,FILNAM,LENF,IER)
        CALL EZGET ('BUILD_CSF',LBUILD,IER)
        CALL EZRSET
        CHTINI = CHTINI .AND. (IER .EQ. 0)
C
C ****  Read in CAD ADDRESS LOOKUP TABLE 
C
        IF ( CHTINI ) THEN
          IF(.NOT.LBUILD) THEN
            CALL CSFSTP (FILNAM(1:LENF),IER)
            IF(IER.NE.0) LBUILD=.TRUE.
          END IF
          IF(LBUILD) THEN
C
C ****  build CSF from RCP
C
            CALL CSFBUILD('D0$CALOR_OFF:CSF.RCP',IER)
          END IF
          CHTINI = IER .EQ. 0
          MSG = 'READ '//FILNAM(1:LENF) 
          IF(IER.NE.0) CALL ERRMSG('CSFSTP FAILS','CHTINI',MSG,'W')
        ELSE
          MSG = 'NO READ'//CSF_STPFILE//' IN '//RCP_FILE
          CALL ERRMSG('CAHITS_FAIL','CHTINI',MSG,'W')
        ENDIF
C
        CALL INRCP('CALICD_RCP',IER)     ! read ICD/MSGP constants
        IF(IER.NE.0) GO TO 999
      ELSE
        CHTINI=.TRUE.
      ENDIF
  999 RETURN
      END
