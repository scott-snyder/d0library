C+
      SUBROUTINE SAFC2D (NFUN, NPAR, FUN, PAR, FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the sum of distance between 
C-                         track before magnet and hits.
C-
C-   Inputs  : NFUN - number of functions,
C-             NPAR - number of track parameters,
C-             PAR - array of parameters,
C-             FLAG - is not used.
C-   Outputs : FUN - array of functions,
C-   Controls: none.
C-
C-   Created  18-DEC-1992   Alexander Efimov
C-   Updated  10-DEC-1993   Alexander Efimov   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NFUN, NPAR, FLAG
      REAL    FUN(*), PAR(*)
      INTEGER NPL
      PARAMETER (NPL=12)
      COMMON /COMSAFCN2/ ZMIN, ZMAX, ROAD, ROAD2, VERTEX(3),
     &                   MONITOR, HADR(3,NPL)
      REAL    ZMIN, ZMAX, ROAD, ROAD2, VERTEX
      INTEGER MONITOR, HADR
      REAL    LINE(6), XX, YY, ZZ, W, W1, W2, DIST
      INTEGER LD, JC, J, OK
      REAL    SXVER, SYVER, SZVER
      SAVE    SXVER, SYVER, SZVER, FIRST
      INTEGER IERR
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
C ****  initializing
C
      IF (FIRST) THEN
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET  ('SXVER', SXVER, IERR)
        CALL EZGET  ('SYVER', SYVER, IERR)
        CALL EZGET  ('SZVER', SZVER, IERR)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
C ****  FUNctions calculation
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
      CALL SADSPL (VERTEX, LINE, DIST, W)
      XX = LINE(1) + W * LINE(4)
      YY = LINE(2) + W * LINE(5)
      ZZ = LINE(3) + W * LINE(6)
      XX = (XX - VERTEX(1)) / SXVER
      YY = (YY - VERTEX(2)) / SYVER
      ZZ = (ZZ - VERTEX(3)) / SZVER
      FUN(1) = SQRT (XX*XX + YY*YY + ZZ*ZZ)
      DO J = 1, MONITOR
        JC = HADR(2,J)
        LD = HADR(3,J)
        CALL SADS2L (LINE, C(JC+1), DIST, W1, W2, OK)
        DIST = SQRT (DIST)
        FUN(J+1) = ABS(DIST - Q(LD+4)) / ROAD
      END DO
C
      RETURN
      END
