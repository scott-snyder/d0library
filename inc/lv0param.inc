C----------------------------------------------------------------------
C-
C-   Created   8-DEC-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      COMMON /LV0PARAM/ INFO, N, TSUM, T2SUM, TMAX, TMIN, TCOR, TAVE,
     &  VTX, SIGMA, MI, MV, INTER, GOODZ, QSUM, NUMHITS, EXCLUSIVE,
     &  SHORTY, DATA_WORD
      REAL    INFO(80,3)
      INTEGER N(3,2), TSUM(3,2), T2SUM(3,2), TMAX(3,2), TMIN(3,2)
      INTEGER TCOR(80), TAVE(2)
      REAL    VTX, SIGMA(3), QSUM
      INTEGER MI, MV, INTER, GOODZ
      INTEGER NUMHITS, EXCLUSIVE, SHORTY
      INTEGER DATA_WORD(123)
