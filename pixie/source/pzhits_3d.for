      SUBROUTINE PZHITS_3D
c----------------------------------------------------------------------
C-
C-   Purpose and Methods : draw Central detector hits in 3D
C-                         if compressed hits bank exist, use compressed 
C-                         hits banks, otherwise use normal hits bank
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  12-DEC-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      CALL PDHITS_3D      ! draw CDC hits in 3D
C      CALL PFHITS_3D      ! draw FDC hits in 3D
C      CALL PVHITS_3D      ! draw VTX hits in 3D
C
  999 RETURN
      END
