C----------------------------------------------------------------------
C-
C-   Created   3-JUN-1994   Richard V. Astur
C-
C----------------------------------------------------------------------
C: Link area
      INTEGER DUMM(2)
      INTEGER NNLNK
      INTEGER LKVEC, LKMAP              
      INTEGER LKMKL                     
      INTEGER LKHIS                     
      INTEGER KKMAP, KKVEC              
      PARAMETER (NNLNK = 10)
      INTEGER LLINK( NNLNK )
      COMMON /KTJET_LINK/DUMM, LLINK
C---Links
      EQUIVALENCE( LKMAP, LLINK(1) )
      EQUIVALENCE( LKVEC, LLINK(2) )
      EQUIVALENCE( LKMKL, LLINK(3) )
      EQUIVALENCE( LKHIS, LLINK(4) )
      EQUIVALENCE( KKMAP, LLINK(5) )
      EQUIVALENCE( KKVEC, LLINK(6) )

