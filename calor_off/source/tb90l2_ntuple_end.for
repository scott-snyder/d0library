      FUNCTION TB90L2_NTUPLE_END ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CALLED at end of program - PBD can not have two hooks
C-                         with same name.
C-   Inputs  : none
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-APR-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TB90L2_NTUPLE_END ,TB90L2_NTUPLE_STORE 
C----------------------------------------------------------------------
      TB90L2_NTUPLE_END = TB90L2_NTUPLE_STORE ()
  999 RETURN
      END
