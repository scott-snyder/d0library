dir
C----------------------------------------------------------------------
C-
C- common block for filter summary (FSUM) I/O
C-   Created   4-FEB-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      INTEGER NRUN_READ
      REAL    EVENT_WEIGHT,CROSS_SECT
      INTEGER FNCALL,SNCALL(0:127),TNCALL(MAXTOOL),TOT_EVENTS_READ(3)
      REAL    AVGBITS(2),FAVG,SAVG(0:127),TAVG(MAXTOOL)
      REAL    FSIGMA,SSIGMA(0:127),TSIGMA(MAXTOOL),TIME
      COMMON/FSUM/ NRUN_READ, EVENT_WEIGHT,CROSS_SECT,TOT_EVENTS_READ, 
     &  AVGBITS,FNCALL,FAVG,FSIGMA,TIME,
     &    SNCALL,SAVG,SSIGMA,TNCALL,TAVG,TSIGMA


