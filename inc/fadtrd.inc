C----------------------------------------------------------------------
C-
C-   Created   9-MAY-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      INTEGER NBFAD,NBMTRA
      PARAMETER( NBFAD =128  )
      PARAMETER(NBMTRA  =1  )
      COMMON /FADTRD/ FADCTR(NBFAD,6,NBMTRA),TRNBCH
      REAL FADCTR
      INTEGER TRNBCH
