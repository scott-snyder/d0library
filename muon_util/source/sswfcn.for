C+
      SUBROUTINE SSWFCN (NFUN, NPAR, FUN, PAR, FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the sum of distance between 
C-                         track and hits (tubes and points) after magnet.
C-                         The vector IHITAD(12) points to the address of
C-                         the hit cells in the SSW work area.
C-   Inputs  : NFUN - number of functions,
C-             NPAR - number of track parameters,
C-             PAR - array of parameters,
C-             FLAG- not used
C-   Outputs : FUN - array of functions
C-   Controls: none.
C-
C-             Based on Efimov's SAFCN2
C-
C-   Created  31-MAY-1994   Joao R.T. de Mello Neto
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
C
      COMMON /CSSWFCN/ ZMIN, ZMAX, ROAD, ROAD2,
     &       MONITOR, HADR(3,NPL), IHTAD(12), WMON, WROAD
      INTEGER IHTAD, WMON
      REAL    ZMIN, ZMAX, ROAD, ROAD2, WROAD
      INTEGER NMWPL
      INTEGER MONITOR, HADR
      REAL    LINE(6), XX, YY, ZZ, W, W1, W2, DIST
      INTEGER LD, JC, J,I,LP, OK
      REAL    HIT(3)
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
C ****  calculate the track (line) from the parameters
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
C *** Calculate the distance between each wire and the track
C
      DO J = 1, MONITOR
        LD = HADR(3,J)
        JC = IQ(LD+3)
        CALL SADS2L (LINE, C(JC+1), DIST, W1, W2, OK)
        FUN(J) = SQRT(DIST) / ROAD
      END DO
C
C *** Loop over points (WAMUS)
C
      J=0
      DO I = 1, NMWPL
        IF (IHTAD(I).GT.0) THEN
          J = J + 1
          LP = IHTAD(I) + 3
          HIT(1) = IQ(LP)
          HIT(2) = IQ(LP+1)
          HIT(3) = IQ(LP+2)
          CALL SADSPL (HIT, LINE, DIST, W)
          XX = LINE(1) + W * LINE(4)
          YY = LINE(2) + W * LINE(5)
          ZZ = LINE(3) + W * LINE(6)
          XX = XX - HIT(1)
          YY = YY - HIT(2)
          ZZ = ZZ - HIT(3)
          FUN(MONITOR+J) = SQRT ((XX*XX + YY*YY + ZZ*ZZ)/WROAD)
        END IF
      END DO 
C      
      RETURN
      END
