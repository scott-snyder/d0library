      INTEGER CHR_CRD,WRD_CRD       
C                                   
      PARAMETER( CHR_CRD = 80)
      PARAMETER( WRD_CRD = CHR_CRD/4)
C
      CHARACTER*4 CARD(WRD_CRD)
      CHARACTER*(CHR_CRD) CRD,CRDRAW
      INTEGER     ICARD(WRD_CRD)
      EQUIVALENCE (CARD,ICARD),(CARD,CRD)

