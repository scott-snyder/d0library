C+
      SUBROUTINE SSWFCN1(NFUN, NPAR, FUN, PAR, FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the sum of distance between 
C-                         track after magnet and SAMUS and WAMUS hits
C-
C-   Inputs  : NFUN - number of functions,
C-             NPAR - number of track parameters,
C-             PAR - array of parameters,
C-             FLAG - is not used.
C-   Outputs : FUN - array of functions,
C-   Controls: none.
C-
C-   Created  5-JUL-1994   Joao de Mello, Neto
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
C>
      COMMON /CSWWFCN1/ ZMIN, ZMAX, ROAD,
     &        MONITOR, HADR(3,NPL), IH_TRACK_BEST,X,Y,Z, WMON,
     &        WROAD2 
      REAL    ZMIN, ZMAX, ROAD
      INTEGER MONITOR, HADR
      REAL WROAD2, X(12,2), Y(12,2), Z(12)
      INTEGER IH_TRACK_BEST(12), WMON
      INTEGER NMWPL
      REAL POINT(3)
      INTEGER IWMON,IPOS
      REAL    LINE(6), XX, YY, ZZ, W, W1, W2, DIST
      INTEGER LD, JC, J, OK
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
C ****  initializing
C
      IF (FIRST) THEN
        NMWPL = 12
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
C
C *** Find the distance between the track and each SAMUS tube
C
      DO J = 1, MONITOR
        JC = HADR(2,J)
        LD = HADR(3,J)
        CALL SADS2L (LINE, C(JC+1), DIST, W1, W2, OK)
        DIST = SQRT (DIST)
        FUN(J) = ABS(DIST - Q(LD+4)) / ROAD
      END DO
C
C *** Loop over the WAMUS points in this track
C
      J=0
      DO IWMON = 1, WMON
        IPOS = IH_TRACK_BEST(IWMON)
        IF (IPOS.GT.0) THEN
          POINT(1) = X(IWMON,IPOS)
          POINT(2) = Y(IWMON,IPOS)
          POINT(3) = Z(IWMON)
          CALL SADSPL (POINT, LINE, DIST, W)
          XX = LINE(1) + W * LINE(4)
          YY = LINE(2) + W * LINE(5)
          ZZ = LINE(3) + W * LINE(6)
          XX = XX - POINT(1)
          YY = YY - POINT(2)
          ZZ = ZZ - POINT(3) 
          J=J+1
          FUN(MONITOR+J) = SQRT ((XX*XX + YY*YY + ZZ*ZZ)/WROAD2)
        ENDIF
      END DO
C
      RETURN
      END
