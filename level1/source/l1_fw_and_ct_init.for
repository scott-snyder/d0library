      SUBROUTINE L1_FW_AND_CT_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read in the LSM object file, the TCC message file,
C-      and the resource RCP file.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   5-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          - Changed routine from an entry point to a
C-                            subroutine. 
C-   Updated   7-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Added a new error type, no recognizable messages.
C-   Updated  15-DEC-1991 Philippe Laurens, Steven Klocek   
C-                      move from L1SIM_INI
C-                      CALL L1_FW_AND_CT_CLEAR
C-                      CALL L1_READ_L1SIM_RCP (renamed L1_FW_AND_CT_RCP)
C-   Updated   4-FEB-1992   Philippe Laurens, Steven Klocek  
C-                      Split initialization into several pieces (LSM, Resource
C-                      file, Programming file) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL_STRINGS.INC'
C
C       clear common blocks
      CALL L1_FW_AND_CT_CLEAR
C
C       read L1SIM.RCP parameters for the framework and cal trig.
      CALL L1_FW_AND_CT_RCP
C
C       Read in LSM file
C
      CALL L1C_INIT_LSM
C
C     Read in programming file
C
      CALL L1_FW_AND_CT_INIT_PROGR
C
C       Read in resource RCP file
C
      CALL L1FW_INIT_RES
C
 3000 CONTINUE
      RETURN
      END
