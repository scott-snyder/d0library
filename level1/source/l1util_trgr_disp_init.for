      LOGICAL FUNCTION L1UTIL_TRGR_DISP_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization for the TRGR_DISP package.
C-
C-   Returned value  : Success if .TRUE.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   3-FEB-1992   Philippe Laurens, Steven Klocek
C-   Updated  28-SEP-1992   Philippe Laurens, Steven Klocek  
C-                              Version banner now includes the name TRGR_DISP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
C
      CHARACTER*72 V, VLEVEL1
      INTEGER IER
C
      L1UTIL_TRGR_DISP_INIT = .TRUE.
C
C
C       display version banner
C       ======================
      V = VLEVEL1 ()
      CALL INTMSG ( ' TRGR_DISP>' // V )
C
C       Initialize /ZEBSTP/
C       ===================
      CALL INZSTP
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
C
C       clear common blocks
      CALL L1_FW_AND_CT_CLEAR
C
C       read L1SIM.RCP parameters for the framework and cal trig.
      CALL L1_FW_AND_CT_RCP
C
C       Load in the files
      CALL L1C_INIT_LSM
      CALL L1FW_INIT_RES
C----------------------------------------------------------------------
  999 RETURN
      END
