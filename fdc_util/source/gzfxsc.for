      FUNCTION GZFXSC(H,U,QD,S)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call GZFPSC (for phi) or GZFTSC (for theta)
C-                         Returns pointer to zebra bank
C- 
C-   Inputs  : Half, Unit, Quadrant, Sector for theta   IN LOGICAL COORDINATES
C-             Half, Sector for phi (others ignored)
C-   Outputs : pointer to zebra bank
C-   Controls: 
C-
C-   Created  19-JAN-1990   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER GZFXSC
      INTEGER H,U,QD,S
      INTEGER GZFPSC
      INTEGER GZFTSC
C
C----------------------------------------------------------------------
C
      GZFXSC = 0
C
      IF (U.EQ.0) THEN
        GZFXSC = GZFTSC(H,QD,S)
      ELSE
        GZFXSC = GZFPSC(H,S)
      END IF
C-----------------------------------------------------------------------
  999 RETURN
      END
