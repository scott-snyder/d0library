C----------------------------------------------------------------------
C-
C-   Created  26-APR-1991   Alan M. Jonckheere - Geant 3.14
C-
C----------------------------------------------------------------------
      COMMON/GCPARM/IPARAM,PCUTGA,PCUTEL,PCUTNE,PCUTHA,PCUTMU
     +             ,NSPARA,MPSTAK,NPGENE
      REAL PACUTS(5)
      EQUIVALENCE (PACUTS(1),PCUTGA)
C
      INTEGER IPARAM,MPSTAK,NSPARA,NPGENE
      REAL    PCUTGA,PCUTEL,PCUTNE,PCUTHA,PCUTMU
C
