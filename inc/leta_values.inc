C            LETA_VALUES.INC
C----------------------------------------------------------------------
C-
C-   Created  25-APR-1994   Lewis Taylor Goss
C-
C----------------------------------------------------------------------
C     *** values to be stored in the result bank LETA ***
      INTEGER PAIR_STATUS,PAIR_TYPE,OBJN_VAL
      REAL PAIR_VAL(2,3),PAIR_ETAGAP
C
      COMMON /LETA_VALUES/ PAIR_VAL,PAIR_ETAGAP,PAIR_STATUS,PAIR_TYPE,
     &    OBJN_VAL
