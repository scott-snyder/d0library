      FUNCTION GZFXDA(H,U,Q,S)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call GZFPDA (for phi) or GZFTDA (for theta)
C-                         Returns pointer to zebra bank
C- 
C-   Inputs  : Half, Unit, Quadrant, Sector for theta   IN LOGICAL COORDINATES
C-             Half, Sector for phi (others ignored)
C-   Outputs : pointer to zebra bank (0 if not booked)
C-   Controls: 
C-
C-   Created  19-JAN-1990   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER GZFXDA
      INTEGER H,U,Q,S
      INTEGER GZFPDA
      INTEGER GZFTDA
C
C----------------------------------------------------------------------
C
      GZFXDA = 0
C
      IF (U.EQ.0) THEN
        GZFXDA = GZFTDA(H,Q,S)
      ELSE
        GZFXDA = GZFPDA(H,S)
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
