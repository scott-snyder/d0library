      SUBROUTINE L2_TEST_PARAMETERS(NEWPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy init routine for TOOL1
C-
C-   Inputs  : NEWPAR : Number of parameter sets to read
C-   Outputs : None
C-   Controls: None
C-
C-   Created  31-JAN-1989   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      BYTE NEWPAR
      LOGICAL GOTEM,GOT
      INTEGER NPARIN,NPAR
      SAVE NPARIN,GOTEM
      DATA GOTEM /.FALSE./
C----------------------------------------------------------------------
      NPARIN = NEWPAR
      GOTEM = .TRUE.
      RETURN
C#######################################################################
      ENTRY L2_TEST_GOT_PARAMETERS(GOT,NPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FETCH # PARAMETERS AND WHETHER THE PARAMETERS
C-        ENTRY WAS EVER CALLED
C-
C-   Inputs  : 
C-   Outputs : GOT   .TRUE. IF THIS WAS EVER CALLED
C-             NPAR  = # PARAMETER SETS CLAIMED AT START TIME
C-   Controls: 
C-
C-   Created   2-MAY-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      GOT = GOTEM
      NPAR = NPARIN
  999 RETURN
      END
