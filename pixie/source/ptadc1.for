      SUBROUTINE PTADC1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action routine to display the FADC traces of
C-       a specific wire and layer for the TRD.  Gets the LAYER and 
C-       WIRE number from the user.
C-
C-   Created  18-JAN-1989   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER WIRE,LAYER
C----------------------------------------------------------------------
      CALL PTEGFL              !  Fills the common blocks
      CALL PTGTWI(WIRE,LAYER)  !  Gets WIRE and LAYER from user and
C                              !    interprets them
      CALL PT1WIR(WIRE,LAYER)  !  Display the FADC traces.
  999 RETURN
      END
