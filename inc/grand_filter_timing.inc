C----------------------------------------------------------------------
C-
C-   Created 19-JAN-1992   James T. Linnemann   
C-      hold sums over bins of filter timing info
C----------------------------------------------------------------------
      REAL WTSUM,GRAND_AVGBITS(2)
      REAL GRAND_NCALL,GRAND_SNCALL(0:127),GRAND_TNCALL(MAXTOOL)
      REAL    GRAND_AVG,GRAND_SAVG(0:127),GRAND_TAVG(MAXTOOL)
      REAL    GRAND_SIGMA,GRAND_SSIGMA(0:127),GRAND_TSIGMA(MAXTOOL)
      COMMON/GRAND_FILTER_TIMING/WTSUM,GRAND_AVGBITS,
     &         GRAND_NCALL,GRAND_SNCALL,GRAND_TNCALL,
     &  GRAND_AVG,GRAND_SAVG,GRAND_TAVG, 
     & GRAND_SIGMA,GRAND_SSIGMA,GRAND_TSIGMA

