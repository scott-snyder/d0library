C----------------------------------------------------------------------
C-
C-   Created   12-JAN-1993   Andrew G. Brandt
C-   Updated   02-NOV-1994   Andrew G. Brandt Update for CW
C-   Updated   06-DEC-1995   Andrew G. Brandt Add LUM 
C-
C----------------------------------------------------------------------
C
C QCD_EVT_INFO
C
      COMMON /QCD_EVT_INFO/RUNNUM,EVTNUM,BAD_WORD,N_BAD,RALG
      INTEGER RUNNUM,EVTNUM,BAD_WORD,N_BAD
      REAL RALG
      CHARACTER*255 QCD_EVT_INFO_TAG
      DATA QCD_EVT_INFO_TAG/'RUNNUM:I,EVTNUM:I,BAD_WORD:I,N_BAD:I,
     &     ALGORISM:R'/
C
C QCD_EVT_INFO2
C
      COMMON /QCD_EVT_INFO2/LUM
      REAL LUM
      CHARACTER*255 QCD_EVT_INFO2_TAG
      DATA QCD_EVT_INFO2_TAG/'LUM:R'/
C
C QCD_MC_INFO
C
      COMMON /QCD_MC_INFO/WEIGHT,CROSS_SECTION
      REAL    WEIGHT,CROSS_SECTION
      CHARACTER*255 QCD_MC_INFO_TAG
      DATA QCD_MC_INFO_TAG /'ISA_WGT:R,ISA_XSEC:R'/
C
C QCD_RUN
C
      COMMON /QCD_RUN/IRUN,ZOFF(2)
      INTEGER IRUN
      REAL ZOFF
