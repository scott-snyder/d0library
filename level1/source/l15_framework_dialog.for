      SUBROUTINE L15_FRAMEWORK_DIALOG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get values of run parameters related to Level 1.5 
C-      interactively.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   4-DEC-1991   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
C
C       Calculate complete Level 1.5 decision for each event
C
      CALL L1UTIL_GET_PARAM_DIALOG(L15_CERTIFIED,
     &  'Level 1.5 decision certified on each event '
     &  // '(hardware mode = N) ?',
     &  'Level 1.5 decision will be certified on each event'
     &  // ' (i.e. NOT like hardware)',
     &  'Level 1.5 decision will be calculated as needed' 
     &  // ' (i.e. like hardware)')

C
      IF (L15_CERTIFIED .EQV. .TRUE.) THEN
        CALL INTMSG( ' **** WARNING ****'
     &  // ' This mode does not correspond to hardware operation' )
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
