C----------------------------------------------------------------------
      LOGICAL FUNCTION T0DEVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Performs T0D-specific reconstruction of the
C-                         event
C-
C-   Returned value  : .True.  - success
C-                     .False. - otherwise
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  13-MAR-1993   Gregory L. Landsberg
C-   Updated  11-APR-1993   Qizhong Li-Demarteau  fixed the calls to functions
C-   Updated   7-NOV-1995   Norman A. Graf   Added check on run range validity.
C-                          Hardcoded 93888 as end of Run1b.
C-   Updated  16-NOV-1995   Norman A. Graf   Added call to ZCDEVT and RTSTFL
C-
C----------------------------------------------------------------------

      IMPLICIT      NONE
      INCLUDE      'D0$INC:ZEBCOM.INC'
      INCLUDE      'D0$INC:T0DREC.INC'
      INCLUDE      'D0$INC:T0DLNT1.INC'
      INCLUDE      'D0$INC:T0DLNT2.INC'
      INTEGER       GZDTRK, T0D_EV, IERR, RUNSAV, EVTSAV, RUNNO, EVONUM
      LOGICAL       HIT_IN_T0D, TRK_IN_T0D, T0HITS, T0TRKS
      LOGICAL       FIRST,OK,ZCDEVT,CALL_ZCD,CALL_FIBERS
      LOGICAL       CALL_PRESHOWER,PSRWFL
      integer i_t0d_evt
C
      DATA          T0D_EV /0/, RUNSAV /-1/, EVTSAV /-1/, FIRST /.TRUE./
C
      SAVE          T0D_EV, RUNSAV, EVTSAV
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('ZCD_RCP')
        CALL EZGET('CALL_ZCD',CALL_ZCD,IERR)
        CALL EZGET('CALL_FIBERS',CALL_FIBERS,IERR)
        CALL EZGET('CALL_PRESHOWER',CALL_PRESHOWER,IERR)
        CALL EZRSET
      ENDIF
C
      T0DEVT = .TRUE.
C
C ****  Ensure that we are going to process a different event
C
      IF (RUNSAV .EQ. RUNNO()) THEN
        IF (EVTSAV .EQ. EVONUM()) RETURN
        EVTSAV = EVONUM()
      ELSE
        RUNSAV = RUNNO()
      END IF
C
C ****  Run 1a,b (T0D) or Run1c (ZCD)?
C
      IF(RUNNO().LT.93888) THEN
        IF ( .NOT. L_T0D_REC ) RETURN
        T0D_EV = T0D_EV + 1
        i_t0d_evt = n_t0d_evt
        IF ( T0D_EV .NE. i_T0D_EVT ) RETURN
        T0D_EV = 0
C
C ****  Set HBOOK directory
C
        IF ( L_HIST ) THEN
          CALL DHDIR('T0D_RCP','HBOOK_DIRECTORY',IERR,' ')
          IF (IERR .NE. 0)
     &      CALL ERRMSG('T0D','T0DEVT','Can''t set HBOOK directory','W')
        END IF
C
C ****  Reset the second temporary link area and deactivate it
C
        CALL MZLINT(IXCOM,'/T0DLNT2/',T0DLNT2,LT0RW,T0DLNT2)
        T0DLNT2(1) = 0
C
        T0DLNT1(1) = 1    ! Activates first temporary link area
        LDTRK = GZDTRK(0)
        DO WHILE (LDTRK .GT. 0)   ! Loop over DTRAKS
          CALL T0DTRK(LDTRK)
          LDTRK = LQ(LDTRK)
        END DO
        T0DLNT1(1) = 0    ! Deactivates first  temporary link area
C
        CALL T0RWFL
        HIT_IN_T0D = T0HITS()
        TRK_IN_T0D = T0TRKS()
      ELSE
        IF(CALL_ZCD) OK = ZCDEVT()
        IF(CALL_FIBERS) CALL RTSTFL
        IF(CALL_PRESHOWER) OK=PSRWFL()
      ENDIF
C
      RETURN
      END
