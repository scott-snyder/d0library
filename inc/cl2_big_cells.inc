C----------------------------------------------------------------------
C-    list of big (hot?) cells for potential killing
C-
C-   Created  24-OCT-1992   James T. Linnemann
C----------------------------------------------------------------------
      INTEGER MAXBIG          
      PARAMETER( MAXBIG = 1 )
      INTEGER NBIG            
C
      REAL  ETMIN_DEFAULT     
      PARAMETER( ETMIN_DEFAULT = 10.0  )
      REAL  ETMIN_CURRENT     
C
C...coordinates of candidates
      INTEGER IETA_BIG(MAXBIG),IPHI_BIG(MAXBIG),LYR_BIG(MAXBIG)
      REAL  ET_BIG(MAXBIG)    
C
C...whether this candidate is still included in CAEP, PNUT banks
      LOGICAL IN_CAEP_BIG(MAXBIG),IN_PNUT_BIG(MAXBIG)
C
      COMMON /CL2_BIG_CELLS/  IN_CAEP_BIG,IN_PNUT_BIG,
     &  IETA_BIG,IPHI_BIG,LYR_BIG,ET_BIG,ETMIN_CURRENT,NBIG

