      FUNCTION L2JETS_CURRENT_PARAM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the last parameter set number that
C-                         L2JETS processed.
C-   Related ENTRY's
C-      L2JETS_CURRENT_ESUM_OBJECT  : Return ID of last ESUM object that
C-                                    L2JETS filled
C-      L2JETS_CURRENT_IND_PARAM    : Return last independent parameter set
C-                                    that L2JETS processed
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-MAR-1992   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$PARAMS:L2JETS.PARAMS'
      INCLUDE 'D0$INC:L2JETS_CONT.INC'    ! L2JETS control common
      INTEGER L2JETS_CURRENT_PARAM 
      INTEGER L2JETS_CURRENT_IND_PARAM
      INTEGER L2JETS_CURRENT_ESUM_OBJECT
C----------------------------------------------------------------------
      L2JETS_CURRENT_PARAM = NOWPARAM
      RETURN

      ENTRY L2JETS_CURRENT_IND_PARAM()
      L2JETS_CURRENT_IND_PARAM = NOW_IND_PARAM
      RETURN

      ENTRY L2JETS_CURRENT_ESUM_OBJECT()
      L2JETS_CURRENT_ESUM_OBJECT = 0
      IF ( NOW_IND_PARAM .GT. 0 ) L2JETS_CURRENT_ESUM_OBJECT = ID_JET_1
     &  - 1 + NOW_IND_PARAM

  999 RETURN
      END
