      SUBROUTINE PX_SETUP_SEQUENCE_INTERRUPT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets up an interrupt routine to end the 
C-   sequence display.  The interrupt routine is called PX_STOP_SEQUENCE
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  26-SEP-1991   Lupe Howell, Harrison B. Prosper
C-   Updated  15-JAN-1993   Lupe Howell Removed the machine blocks
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      EXTERNAL PX_STOP_SEQUENCE
C----------------------------------------------------------------------
C
C *** Setup the interrupt menu
C
      CALL INTMEN('SEQUENCE MODE','STOP_SEQUENCE',PX_STOP_SEQUENCE)
  999 RETURN
      END
