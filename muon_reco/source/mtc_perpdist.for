      SUBROUTINE MTC_PERPDIST(APT1,ALN1,ALN2,DIST,IERR)
C----------------------------------------------------------------------
C- MTC_PERPDIST: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : Find the perpendicular distance from a
C-      point to a line in 3 space
C-
C-   Inputs  : APT1(3) - the x,y,z coordinates of the point
C-             ALN1(3), ALN2(3) - are two (x,y,z) points on the line
C-   Output  : DIST - distance from the point to the line
C-             IERR --> 0 ok
C-                      1 indicates the two points that
C-                      are supposed to define the line
C-                      are identical
C-
C-   Created   9-AUG-1993   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input
      REAL APT1(*),ALN2(*), ALN1(*)
C- output
      REAL DIST
      INTEGER IERR
C- local variables ...
      REAL A(3), B(3), C, ALEN,BLEN,CLEN, COST, PARADIST
      INTEGER III
C----------------------------------------------------------------------
C- A(3) is the vector from the vertex to the point apt1(3) ...
      A(1) = APT1(1) - ALN1(1)
      A(2) = APT1(2) - ALN1(2)
      A(3) = APT1(3) - ALN1(3)
      ALEN = SQRT(A(1)**2 + A(2)**2 + A(3)**2)
C- B(3) is the vector from the vertex to the point on the line ALN2(3) ...
      B(1) = ALN2(1) - ALN1(1)
      B(2) = ALN2(2) - ALN1(2)
      B(3) = ALN2(3) - ALN1(3)
      BLEN = SQRT(B(1)**2 + B(2)**2 + B(3)**2)
C- C is the dot product of vectors A and B ...
      C    = A(1)*B(1) + A(2)*B(2) + A(3)*B(3)
C- See if any vector lengths are zero ...
      IF(BLEN.EQ.0.) THEN
ccc        WRITE(6,80)
ccc     &    (APT1(III),III=1,3),(ALN2(III),III=1,3),(ALN1(III),III=1,3)
ccc   80   FORMAT(' MTC_PERPDIST: input error!!! ',3F5.0,2X,3F5.0,2X,3F5.0)
        DIST = -50.
        IERR = 1
        RETURN
      END IF
      IF(ALEN.EQ.0.) THEN
        DIST = 0.
        IERR = 0
        RETURN
      END IF
C- Compute the parallel and perpendicular distance along the line ...
      IERR = 0
      COST = C / (ALEN*BLEN)
      PARADIST = ALEN * COST
      IF(( ABS(ALEN)-ABS(PARADIST) ).LT.0.) THEN
        DIST = 0.
      ELSE
        DIST = SQRT(ALEN**2 - PARADIST**2)
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
