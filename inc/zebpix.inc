C----------------------------------------------------------------------
C-
C-   Created   8-JUL-1988   Olivier Callot
C-
C-   This is the PIXIE screen description ZEBRA common bloc
C----------------------------------------------------------------------
      INTEGER NNP, NMXSCR
      PARAMETER( NNP = 5000 )
      PARAMETER( NMXSCR = 20 )
      INTEGER IXPIX, IDVPIX, FENPIX, LPIXIE, LSCREN, PXSTOR, ENDZP
      COMMON /ZEBPIX/ IXPIX, IDVPIX, FENPIX(10), LPIXIE, 
     &                LSCREN( NMXSCR), PXSTOR( NNP ), ENDZP
      REAL    P( NNP )
      INTEGER IP(NNP), LP(NNP)
      EQUIVALENCE ( LPIXIE, LP(1) ), ( LP(9), IP(1), P(1) )
