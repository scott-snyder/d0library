      SUBROUTINE EUGETM(ISEG, MATX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Return the rotation matrices induced by E&S 3D
C-                          transformations.
C-
C-   Inputs  : ISEG      Any segment number involved.
C-   Outputs : MATX(96)  Output array as follows:
C-                       MATX(1:16) ----> Viewing matrix (4X4)
C-                       MATX(17:32) ----> 3D rotation matrix (4X4)
C-                       MATX(33:48) ----> Total matrix (4X4)
C-                       MATX(49:64) ----> Inverse of FIRST
C-                       MATX(65:80) ----> Inverse of SECOND
C-                       MATX(81:96) ----> Inverse of THIRD
C-   Controls: 
C-
C-   Created  11-JUN-1990   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ISEG
      INTEGER ILIST(1), ICODE
      REAL MATX(96)
C
      ILIST(1) = ISEG
      ICODE = 30013
      CALL JESCAP(ICODE, 1, 0, ILIST, MATX)
C
  999 RETURN
      END
