      SUBROUTINE TOP_LEPTONS_EVENT_FIX(NO_RUN,NO_EVT,NO_L1EVT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Specific Event Fixup Routine
C-
C-                         nb. This routine must be called before doing
C-                             any lepton selection or the event kinematics
C-                             will get screwed up!
C-
C-   Inputs  : 
C-               NO_RUN   - Run Number
C-               NO_EVT   - Event Number (from software)
C-               NO_L1EVT - Event Number (from data logger)
C-
C-   Outputs :
C-               Edited PMUO,PELC Banks.
C- 
C-   Controls: 
C-
C-   Created  12-FEB-1993   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER NO_FIX,NO_RUN,NO_EVT,NO_L1EVT
C
      DATA NO_FIX/0/
C
      IF(NO_FIX.LT.1) GO TO 999
C----------------------------------------------------------------------
  999 RETURN
      END
