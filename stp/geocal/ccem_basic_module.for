      SUBROUTINE CCEM_BASIC_MODULE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the basic CCEM module parameters
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  01-DEC-1989   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Include files
      INCLUDE 'D0$INC:SCCEM_MODULE.INC'
C  Integers
      INTEGER IER
C----------------------------------------------------------------------
C  Select the CC SRCP file
C----------------------------------------------------------------------
      CALL EZPICK ( 'CENTRAL' )
C----------------------------------------------------------------------
C  Get module dimensions from endplate dimensions and positions
C----------------------------------------------------------------------
      CALL EZGET ( 'CCEM_MODULE_INNER_RADIUS',
     &              CCEM_MODULE_INNER_RADIUS, IER )
      CALL EZGET ( 'CCEM_MODULE_OUTER_RADIUS',
     &              CCEM_MODULE_OUTER_RADIUS, IER )
      CALL EZGET ( 'CCEM_MODULE_INNER_WIDTH',
     &              CCEM_MODULE_INNER_WIDTH, IER )
      CALL EZGET ( 'CCEM_MODULE_OUTER_WIDTH',
     &              CCEM_MODULE_OUTER_WIDTH, IER )
      CALL EZGET ( 'CCEM_MODULE_LENGTH',
     &              CCEM_MODULE_LENGTH, IER )
C----------------------------------------------------------------------
C  Compute module volume
C----------------------------------------------------------------------
      CCEM_MODULE_VOLUME = 0.5 * (CCEM_MODULE_INNER_WIDTH +
     &                            CCEM_MODULE_OUTER_WIDTH) *
     &                           (CCEM_MODULE_OUTER_RADIUS -
     &                            CCEM_MODULE_INNER_RADIUS) *
     &                            CCEM_MODULE_LENGTH
      RETURN
      END
