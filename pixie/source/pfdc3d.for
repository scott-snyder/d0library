      SUBROUTINE PFDC3D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make 3D display for just the FDC
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  25-JUN-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL PFDC3D_GEO
      CALL PZ_VERTEX_3D
      CALL PFHITS_3D
      CALL PFTRK_3D
C
  999 RETURN
      END
