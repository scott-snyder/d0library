C
C--     Zebra working space for NOISY package;
C
      INTEGER NNWNOI,NWRNOI
      PARAMETER (NNWNOI=1000000)
      PARAMETER (NWRNOI=9)
      COMMON/ZEBNOI/IXNOI,IDVNOI,FENNOI,LNOIH,LRNOI,LSTEPN,
     2 ZNOISY,ENDZN
C
      INTEGER IXNOI,IDVNOI,LSTEPN,FENNOI(10),ZNOISY(NNWNOI),ENDZN
      REAL NS(NNWNOI)
      INTEGER INS(NNWNOI),LNS(NNWNOI)
      INTEGER*2 KWNOI(2*NNWNOI)
      EQUIVALENCE (LNOIH,LNS(1)),(LNS(9),INS(1),NS(1),KWNOI(1))
      INTEGER LNOIH               
     $,  LRNOI(NWRNOI)
C
