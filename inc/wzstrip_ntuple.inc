C----------------------------------------------------------------------
C-
C-   Created  31-OCT-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      INTEGER MAXNTU, MAXVAR
      PARAMETER (MAXNTU = 5, MAXVAR = 10)
C
      COMMON /WZSTRIP_NTUPLE/NTUPLE_ID(MAXNTU),NTUPLES,
     &  XTUPLE(MAXVAR,MAXNTU),TOPDIR,STATUS
      CHARACTER*16 TOPDIR
      REAL    XTUPLE
      INTEGER NTUPLES, NTUPLE_ID
      INTEGER STATUS
C
