C
C ZEBRA COMMON BLOCK IN D0 MODE.  R.RAJA 24-APR-89. Q->QQ ETC
C
      COMMON /ZEBQ/  IQFENC(4), LQQ(100) 
                      DIMENSION    IQQ(92),        QQ(92) 
                      EQUIVALENCE (IQQ(1),LQQ(9)), (QQ(1),IQQ(1))   
      INTEGER IQFENC ,LQQ,IQQ
      REAL QQ
