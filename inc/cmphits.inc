      INTEGER MAXHITS_PER_WIRE
      PARAMETER (MAXHITS_PER_WIRE = 20)
      LOGICAL USE_HITS
      INTEGER ITMP_CMPHIT,IOK_CMPHIT
      INTEGER ITMP_CDHIT,IOK_CDHIT
      COMMON/COMPRESSED_HITS/USE_HITS
      COMMON/CMPHITS_SAVE_OK/ITMP_CMPHIT(MAXHITS_PER_WIRE), 
     &   IOK_CMPHIT(0:1,0:1,MAXHITS_PER_WIRE)
      COMMON/CDHITS_SAVE_OK/ITMP_CDHIT(MAXHITS_PER_WIRE), 
     &   IOK_CDHIT(0:1,0:1,MAXHITS_PER_WIRE)
      