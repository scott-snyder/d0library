C----------------------------------------------------------------------
C-
C-   Created  26-APR-1991   Alan M. Jonckheere - Geant 3.14
C-
C----------------------------------------------------------------------
      INTEGER MXSLNK, ISLINK, LSLAST, LSCAN, LSTEMP, LSPARA, LSERAY
C
      PARAMETER(MXSLNK=100)
      COMMON/GCSCAL/ ISLINK(MXSLNK)
      EQUIVALENCE (LSLAST,ISLINK(MXSLNK))
      EQUIVALENCE (LSCAN ,ISLINK(1)),(LSTEMP,ISLINK(2))
      EQUIVALENCE (LSPARA,ISLINK(3)),(LSERAY,ISLINK(4))
C
