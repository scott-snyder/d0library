C            RMAS_VALUES.INC
C----------------------------------------------------------------------
C-
C-   Created  15-DEC-1993   Kathy Fatyga
C-
C----------------------------------------------------------------------
C     *** values to be stored in the result bank RMAS ***
      INTEGER PAIR_STATUS,PAIR_TYPE,OBJN_VAL
      REAL PAIR_VAL(2,3),PAIR_MASS,PAIR_BOOST
C     **** values to keep track of which pair is stored ****
      LOGICAL PAIR_PASS 
      INTEGER N_STAT 
C
      COMMON /RMAS_VALUES/ PAIR_VAL,PAIR_MASS,PAIR_BOOST,
     &    PAIR_STATUS,PAIR_TYPE,OBJN_VAL,PAIR_PASS,N_STAT
