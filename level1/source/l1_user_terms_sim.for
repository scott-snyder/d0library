      SUBROUTINE L1_USER_TERMS_SIM(NUM_ANDOR_USED, ANDOR_STATES,
     &  ANDOR_INDICES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy routine to perform event processing to allow
C-      a private trigger subsystem to be attached. 
C-
C-   Inputs  : none
C-   Outputs : NUM_ANDOR_USED   The number of Andor Terms this routine is
C-                              returning.
C-             ANDOR_STATES     The state of each returned Andor Term.
C-             ANDOR_INDICES    The corresponding Andor Term number for 
C-                              each returned Andor Term.
C-   Controls: none
C-
C-   Created   6-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NUM_ANDOR_USED
      LOGICAL ANDOR_STATES
      INTEGER ANDOR_INDICES
C
      NUM_ANDOR_USED = 0
C----------------------------------------------------------------------
  999 RETURN
C#######################################################################
      ENTRY L1_USER_TERMS_FILL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy routine to perform TRGR bank filling by user
C-    trigger subsystem.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-
C-   Created  21-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      RETURN
C#######################################################################
      ENTRY L1_USER_TERMS_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy routine to allow initialization of some 
C-      private trigger subsystem.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      RETURN
C#######################################################################
      ENTRY L1_USER_TERMS_DIALOG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy routine to provide user dialog of some private
C-      trigger subsytem.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      RETURN
C#######################################################################
      ENTRY L1_USER_TERMS_SSUM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy routine to allow some private trigger
C-    subsystem to add information to the standard summary file.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      RETURN
C#######################################################################
      ENTRY L1_USER_TERMS_DEFDUMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy routine to interactively define the dumps for
C-      some private trigger subsystem.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      RETURN
C#######################################################################
      ENTRY L1_USER_TERMS_DUMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy routine to perform an event dump of some
C-      private trigger subsystem.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      RETURN
      END
