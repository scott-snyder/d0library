      SUBROUTINE L1FW_INIT_RES
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Load in the Resource RCP file.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   3-FEB-1992   Philippe Laurens, Steven Klocek
C-                      Moved this code here from L1_FW_AND_CT_INIT.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL_STRINGS.INC'
C
      INTEGER IER
C
      CALL INRCP (RESOURCE_FILE_NAME(1:RESOURCE_FILE_NAME_LENGTH),IER)
C
      IF (IER .NE. 0) THEN
        CALL ERRMSG(' INRCP','L1_FW_AND_CT_INIT',
     &    'Could not read: ' // 
     &    RESOURCE_FILE_NAME(1:RESOURCE_FILE_NAME_LENGTH), 'F')
        GOTO 999
      ENDIF
C
      CALL L1FW_INIT_ANDOR_SOURCES()
C
C----------------------------------------------------------------------
  999 RETURN
      END
