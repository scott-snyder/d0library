      FUNCTION CL2TEST_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-              TEST CL2 routines
C-              Entry test initialization
C-   Inputs  : NONE 
C-   Outputs :  zebra surveys, dump of bank(s), histograms; timings
C-   Controls:  CL2TEST_RCP
C-
C#######################################################################
C     ENTRY CL2TEST_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compare results from CL2 fast conversion
C-                         routines to CAHITS.  Tests full event CAEP, PNUT,
C-                         and Level 2 candidates
C-   Inputs  : CAEP from CAHITS and CL2HITS
C-   Outputs : histograms and dumps of differing channels; timing of unpacking
C-   Controls: CL2TEST_RCP
C-
C-   Updated 13-MAY-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
C#######################################################################
C     ENTRY CL2TEST_END()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :end of run processing for cl2_test
C-              print timing numbers for event processing, and histos
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-MAY-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CL2TEST_INI,CL2TEST_EVT,CL2TEST_END
      LOGICAL CL2TEST_INIT,CL2TEST_HITS,CL2TEST_HITS_END
      LOGICAL CL2TEST_CANDS,CL2TEST_CANDS_END
      INTEGER LUN,USUNIT
C-
C----------------------------------------------------------------------
      CL2TEST_INI = CL2TEST_INIT()
      CALL ERRINI(7,.TRUE.)             ! force WARNING messages to file
      CALL ERRMAX(' ',10,10)            ! allow 10 messages of each type
      RETURN
C#######################################################################
      ENTRY CL2TEST_EVT()
C-
C----------------------------------------------------------------------
      CL2TEST_EVT = .TRUE.
      CL2TEST_EVT = CL2TEST_EVT.AND.CL2TEST_HITS()
      CL2TEST_EVT = CL2TEST_EVT.AND.CL2TEST_CANDS()
      RETURN
C#######################################################################
      ENTRY CL2TEST_END()
C----------------------------------------------------------------------
      CL2TEST_END = .TRUE.
      CL2TEST_EVT = CL2TEST_EVT.AND.CL2TEST_HITS_END()
      CL2TEST_EVT = CL2TEST_EVT.AND.CL2TEST_CANDS_END()
      LUN = USUNIT()
      CALL HOUTPU(LUN)
      CALL HISTDO
      RETURN
      END
