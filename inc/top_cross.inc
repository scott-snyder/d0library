C----------------------------------------------------------------------
C-
C-   Created  23-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      INTEGER ND
      PARAMETER( ND = 151 )
      COMMON /TOP_CROSS/ TMASS_18,TTB_QQB_18,TTB_GG_18,TTB_CROSS_18,
     &  TMASS_20,TTB_QQB_20,TTB_GG_20,TTB_CROSS_20,
     &  TM,RAT_BORN,RAT_NNLO,TM_N18,TTB_CROSS_N18,N18,
     &  TM_N20,TTB_CROSS_N20,N20,
     &  M18,M20,DISC_LIMIT_18,DISC_LIMIT_20,IEN
C
      DOUBLE PRECISION TMASS_18(ND),TTB_QQB_18(ND),TTB_GG_18(ND),
     &  TTB_CROSS_18(ND)
      DOUBLE PRECISION TMASS_20(ND),TTB_QQB_20(ND),TTB_GG_20(ND),
     &  TTB_CROSS_20(ND)
      REAL    TM(ND),RAT_BORN(ND)
      REAL    RAT_NNLO(ND)
      REAL    TM_N18(ND),TTB_CROSS_N18(ND)
      INTEGER N18
      REAL    TM_N20(ND),TTB_CROSS_N20(ND)
      INTEGER N20
      INTEGER M20,M18
      INTEGER NDISC
      PARAMETER( NDISC =100  )
      REAL    DISC_LIMIT_18(NDISC)
      REAL    DISC_LIMIT_20(NDISC)
      DATA M20/3/
      DATA M18/3/   
      INTEGER IEN
C
