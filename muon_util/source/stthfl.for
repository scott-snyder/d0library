      SUBROUTINE STTHFL(ITRAK,NHIT,IADD,GEOM,DIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill bank 'STTH': SAMUS hits on track
C-
C-   Inputs  : ITRAK      -   Track number in MUOT bank
C-             NHIT       -   Number of hits on track (<=40)
C-             IADD(NHIT) -   Cell address (256*Module + tube)
C-             GEOM(6,NHIT)-   Tube geometry info (x,y,z,dx,dy,dz)
C-             DIST(NHIT) -   Drift-distance
C-   Outputs :
C-   Controls:
C-
C-   Created   5-MAY-1991   O.Eroshin
C-   Recreated 13-JUN-1994  M. Fortner made into general purpose routine
C-   Updated  20-DEC-1994   Igor V. Mandrichenko   Store geometry of the
C-                          tube and drift distance
C-   Updated  04-FEB-1995   I.M. New format of STTH
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER  ITRAK,NHIT,NMAX,N_STTH
      PARAMETER (NMAX=40, N_STTH=8)
      INTEGER  IADD(NMAX)
      REAL DIST(NMAX),GEOM(6,NMAX)
      INTEGER  I,ISTTH,LSTTH
      INTEGER  GZMUOT,LMUOT
      EXTERNAL GZMUOT
C
      IF (NHIT.GT.NMAX) NHIT=NMAX
      LMUOT = GZMUOT(ITRAK)
      CALL BKSTTH(LMUOT,NHIT*N_STTH,LSTTH)
      IF (LSTTH.EQ.0) GO TO 999
C
      ISTTH = LSTTH
      DO I=1,NHIT
        IQ(ISTTH+1) = IADD(I)
        Q(ISTTH+2) = GEOM(1,I)
        Q(ISTTH+3) = GEOM(2,I)
        Q(ISTTH+4) = GEOM(3,I)
        Q(ISTTH+5) = GEOM(4,I)
        Q(ISTTH+6) = GEOM(5,I)
        Q(ISTTH+7) = GEOM(6,I)
        Q(ISTTH+8) = DIST(I)
        ISTTH = ISTTH + N_STTH
      END DO
C
  999 RETURN
      END
