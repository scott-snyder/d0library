      SUBROUTINE SORT_VEC_DIST(DIMEN,PVEC,NPOINT,VECLST,DELR,INDEX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a set of NPOINT points in a DIMEN
C-                         dimensional space, this routine calculates
C-                         their distances DELR from a point PVEC (in
C-                         the same space) specifed by the user and
C-                         ranks them in ascending order of those
C-                         distances. Function VDIST and routine
C-                         SORTZV from CERNLIB have been used.
C-
C-   Inputs  : DIMEN          [I]   Dimensionality of the space
C-             PVEC(DIMEN)    [F]   The point from which distances are
C-                                  to be calculated
C-             NPOINT         [I]   Number of vectors to be sorted
C-             VECLST(DIMEN*NPOINT) [F] Array of vectors to be sorted.
C-                                  In pqr... space, this array must
C-                                  be ordered as p(1),q(1),r(1),...,
C-                                  p(2),q(2),r(2),...,.....,p(NPOINT),
C-                                  q(NPOINT),r(NPOINT),...
C-   Outputs : DELR(NPOINT)   [F]   Array of distances (in the same order
C-                                  as vectors were given through VECLST)
C-             INDEX(NPOINT)  [I]   Array of ranks i.e. the INDEX(1)th
C-                                  vector in VECLST is closest to PVEC
C-                                  and so on.
C-
C-   Controls: none
C-
C-   Created  25-NOV-1991   Dhiman Chakraborty
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DIMEN,NPOINT,INDEX(NPOINT)
      REAL    PVEC(DIMEN),VECLST(DIMEN*NPOINT),DELR(NPOINT),VDIST
      INTEGER I,J
      EXTERNAL VDIST
C----------------------------------------------------------------------
      DO 50 I = 1,NPOINT
        J = (I-1)*DIMEN + 1
        DELR(I) = VDIST(VECLST(J),PVEC,DIMEN)
   50  CONTINUE
       CALL SORTZV(DELR,INDEX,NPOINT,1,0,0)
  999 RETURN
      END
