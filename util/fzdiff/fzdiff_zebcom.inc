C
C  ZEBCOM is the main zebra common block for event data storage
C
      INTEGER NNQ,NREF
      PARAMETER (NNQ=1000000)
      PARAMETER (NREF=9)
      COMMON/ZEBCOM/IXCOM,IXMAIN,IXDVR,FENCE,LHEAD,LHEADR,LREF,
     &  ZSTOR,ENDZS
      INTEGER IXCOM    
     &       ,IXMAIN   
     &       ,IXDVR    
      INTEGER FENCE(8),LREF(NREF),ZSTOR(NNQ),ENDZS
      INTEGER LHEAD     
      INTEGER LHEADR    
      REAL Q(NNQ)
      INTEGER IQ(NNQ),LQ(NNQ)
      EQUIVALENCE (LHEAD,LQ(1)),(LQ(9),IQ(1),Q(1))
C
