      logical function MULIB_END()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books and fills histograms for checking CAD bank
C-                         filling in GEANT and unpacking in the CAHITS package
C-
C-   Inputs  :  NONE
C-   Outputs : NONE
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      MULIB_END = .TRUE.
	
	CALL MULIB_END_RUN
C
  999 RETURN
      END
