      FUNCTION L1SIM_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call initialization subroutine for each subsystem in
C-      the L1SIM package.
C-
C-   Return Value: Success if .TRUE. All subsystems call ERRMSG with a fatal
C-      message if there is an error, so if this routine exits then all weht
C-      well.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   4-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated   3-DEC-1991   Philippe Laurens, Steven Klocek   
C-                      Added call to L15_FRAMEWORK_INIT.
C-   Updated  15-DEC-1991 Philippe Laurens, Steven Klocek   
C-                      move to L1_FW_AND_CT_INIT
C-                              CALL L1_FW_AND_CT_CLEAR
C-                              CALL L1_READ_L1SIM_RCP
C-                      move from L1_READ_L1SIM_RCP
C-                              CALL INZSTP
C-                              CALL CHTINI
C-                              CALL INRCP (L1SIM_RCP)
C-   Updated   23-MAR-1992  Amber Boehnlein, added call to ESUM filling routine
C-   Updated  14-JUL-1992   Philippe Laurens, Steven Klocek   
C-                      - remove ESUM filling routines (now done in Level 2)
C-   Updated  10-nov-1992   Philippe Laurens, Steven Klocek   
C-                      - read in RCP edit file L1SIM_RCPE 
C-                      - call CALOR_INI (CALOR package) needed by CGEV code
C-                      - remove call to INZSTP, not needed
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      LOGICAL L1SIM_INI
      CHARACTER*72 V, VLEVEL1, VCALOR_OFF
      INTEGER      IER
C
C       display version banner
C       ======================
      V = VLEVEL1 ()
      CALL INTMSG ( ' L1SIM>' // V )
      V = VCALOR_OFF ()
      CALL INTMSG ( ' L1SIM>' // V )
C
C       Initialize CAHITS_RCP (and CALOR), used by Calorimeter library routines
C       =====================
      CALL CALOR_INI ()
      CALL CHTINI ()
C
C       Read control file into an SRCP bank
C       ===================================
      CALL INRCP (L1SIM_RCPFILE,IER)
C
      IF (IER .NE. 0) THEN
        CALL ERRMSG(' INRCP','L1SIM_INI', 'Could not read: '
     &    // L1SIM_RCPFILE, 'F')
        GOTO 999
      ENDIF
C
C       Read RCP edit file L1SIM_RCPE
C       =============================
      CALL INRCPE (L1SIM_RCPEFILE,IER) ! Don't care if file not found
C
C       Level 1.5 
C       =========
      CALL L15_FRAMEWORK_INIT
C
C       Framework and calorimeter trigger
C       ================================
      CALL L1_FW_AND_CT_INIT
C
C       Muon Trigger
C       ============
      CALL L1_MUON_INIT
C
C       user provided subsystem
C       =======================
      CALL L1_USER_TERMS_INIT
C
C       Assign Return value
C       ===================
      L1SIM_INI = .TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
