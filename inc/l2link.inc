C----------------------------------------------------------------------
C-  Zebra link area especially for L2JETS
C-   Created   1-JUN-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      INTEGER DUM(2)
      INTEGER NLNK
      PARAMETER (NLNK = 10)
      INTEGER L2LINK( NLNK )
      COMMON /L2LINK/ DUM,L2LINK
C---Links
      INTEGER LJAUX,LJETS,LJTSH,LTRGR
      EQUIVALENCE( LJAUX, L2LINK(1) ) 
      EQUIVALENCE( LJETS, L2LINK(2) )
      EQUIVALENCE( LJTSH, L2LINK(3) )
      EQUIVALENCE( LTRGR, L2LINK(4) )
