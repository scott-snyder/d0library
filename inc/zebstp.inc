C ZEBSTP.INC
      INTEGER NNC
      PARAMETER (NNC=200000)
      COMMON/ZEBSTP/IXSTP,IDVSTP,IDVSUM,FENSTP,LSTPH,LZSUM,
     1 LSLV0,LLPDH,LLGNH,LLTMH,LLGEH,LLV,               
     2 LSMUO,LMPDH,LMGNH,LMTMH,LMGEH,LMU,               
     3 LSVTX,LVPDH,LVGNH,LVTMH,LVGEH,LBMXY,LVT,         
     4 LSCDC,LDPDH,LDGNH,LDTMH,LDGEH,LDALH,LDC,         
     5 LSTRD,LTPDH,LTGAI,LTGEN,LTGEO,LTCAN,LTLIK,LTR,   
     6 LSFDC,LFPDH,LFGNH,LFTMH,LFGEH,LFD,               
     7 LSCAL,LCPDH,LCGNH,LCGEH,LCA,                     
     8 ZCONS,ENDZC
      INTEGER IXSTP,IDVSTP,IDVSUM,FENSTP(10),LZSUM,ZCONS(NNC),ENDZC
      REAL C(NNC)
      INTEGER IC(NNC),LC(NNC)
      EQUIVALENCE (LSTPH,LC(1)),(LC(9),IC(1),C(1))
      INTEGER LSTPH 
     $,  LSLV0      
     $,  LLPDH      
     $,  LLGNH      
     $,  LLTMH      
     $,  LLGEH      
     $,  LLV(10)    
C
      INTEGER LSMUO 
     $,  LMPDH      
     $,  LMGNH      
     $,  LMTMH      
     $,  LMGEH      
     $,  LMU(10)    
C
C LBMXY added NGraf 8/16/94
      INTEGER LSVTX 
     $,  LVPDH      
     $,  LVGNH      
     $,  LVTMH      
     $,  LVGEH      
     $,  LBMXY      
     $,  LVT(9)     
C
      INTEGER LSCDC 
     $,  LDPDH      
     $,  LDGNH      
     $,  LDTMH      
     $,  LDGEH      
     $,  LDALH      
     $,  LDC(9)     
C
C LTCAN and LTLIK add Azlyber 8/1/94
      INTEGER LSTRD 
     $,  LTPDH      
     $,  LTGAI      
     $,  LTGEN      
     $,  LTGEO      
     $,  LTCAN      
     $,  LTLIK      ! TRD header for JFL's likelihood tables
     $,  LTR(8)     
C
      INTEGER LSFDC 
     $,  LFPDH      
     $,  LFGNH      
     $,  LFTMH      
     $,  LFGEH      
     $,  LFD(10)    
C
      INTEGER LSCAL 
     $,  LCPDH      
     $,  LCGNH      
     $,  LCGEH      
     $,  LCA(11)    
C
