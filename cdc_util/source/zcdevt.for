C----------------------------------------------------------------------
      LOGICAL FUNCTION ZCDEVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Performs ZCD-specific reconstruction of the
C-                         event
C-
C-   Returned value  : .True.  - success
C-                     .False. - otherwise
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  13-MAR-1993   Gregory L. Landsberg
C-   Updated  16-OCT-1995   Freddie Landry  renamed from T0DEVT to ZCDEVT 
C-                                          replaced t0d with zcd everywhere
C-                                          replace t0 with zd everywhere
C-                                          changed the order of subroutine
C-                                      calls so that ZCDTRK is called 3rd
C-                                      rather then 1st.
C-   Updated   8-NOV-1995   Norman A. Graf   Added check on Run 1c data
C-
C----------------------------------------------------------------------

      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:ZCDREC.INC'
      INCLUDE      'D0$INC:ZCDLN1.INC'
      INCLUDE      'D0$INC:ZCDLN2.INC'
      INTEGER       GZDTRK, ZCD_EV, IERR, RUNSAV, EVTSAV, RUNNO, EVONUM
      LOGICAL       HIT_FLG
C
      DATA          ZCD_EV /0/, RUNSAV /-1/, EVTSAV /-1/
C
      SAVE          ZCD_EV, RUNSAV, EVTSAV
C
      ZCDEVT = .TRUE.
      IF ( .NOT. L_ZCD_REC ) RETURN
      ZCD_EV = ZCD_EV + 1
      IF ( ZCD_EV .NE. N_ZCD_EVT ) RETURN
      ZCD_EV = 0
C
C ****  Ensure that we are going to process a different event
C
      IF(RUNNO().LT.93888) RETURN
      IF (RUNSAV .EQ. RUNNO()) THEN
        IF (EVTSAV .EQ. EVONUM()) RETURN
        EVTSAV = EVONUM()
      ELSE
        RUNSAV = RUNNO()
      END IF
C
C ****  Set HBOOK directory
C
      IF ( L_HIST ) THEN
        CALL DHDIR('ZCD_RCP','HBOOK_DIRECTORY',IERR,' ')
        IF (IERR .NE. 0)
     &    CALL ERRMSG('ZCD','ZCDEVT','Can''t set HBOOK directory','W')
      END IF
C
C ****  Reset the second temporary link area and deactivate it
C
      CALL MZLINT(IXCOM,'/ZCDLN2/',ZCDLNT2,LZDRW,ZCDLNT2)
      ZCDLNT2(1) = 0
C
C
      CALL ZDRWFL

      HIT_FLG =.FALSE.
      CALL ZCDHITS(HIT_FLG)
C
      RETURN
      END
