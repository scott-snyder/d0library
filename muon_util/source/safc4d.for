C+
      SUBROUTINE SAFC4D (NFUN, NPAR, FUN, PAR, FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the sum of distance between 
C-                         track after magnet and hits.
C-
C-   Inputs  : NFUN - number of functions,
C-             NPAR - number of track parameters,
C-             PAR - array of parameters,
C-             FLAG - is not used.
C-   Outputs : FUN - array of functions,
C-   Controls: none.
C-
C-   Created  18-DEC-1992   Alexander Efimov
C-   Updated  25-OCT-1993   Alexander Efimov   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NFUN, NPAR, FLAG
      REAL    FUN(*), PAR(*)
      INTEGER NPL
      PARAMETER (NPL=24)
      COMMON /COMSAFCN4/ ZMIN, ZMAX, ROAD, ROAD2, 
     &                   MONITOR, HADR(3,NPL)
      REAL    ZMIN, ZMAX, ROAD, ROAD2
      INTEGER MONITOR, HADR
      REAL    W, W1, W2, DIST, LINE(6)
      INTEGER LD, JC, J, OK
C
      LINE(1) = PAR(1)
      LINE(2) = PAR(2)
      LINE(3) = ZMIN
      LINE(4) = PAR(3) - PAR(1)
      LINE(5) = PAR(4) - PAR(2)
      LINE(6) = ZMAX - ZMIN
      W = 1.0 / SQRT (LINE(4)**2 + LINE(5)**2 + LINE(6)**2)
      LINE(4) = LINE(4) * W
      LINE(5) = LINE(5) * W
      LINE(6) = LINE(6) * W
      DO J = 1, MONITOR
        JC = HADR(2,J)
        LD = HADR(3,J)
        CALL SADS2L (LINE, C(JC+1), DIST, W1, W2, OK)
        DIST = SQRT (DIST)
        FUN(J) = ABS(DIST - Q(LD+4)) / ROAD
      END DO
C
      RETURN
      END
