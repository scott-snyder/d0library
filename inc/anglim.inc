C----------------------------------------------------------------------
C-
C-   Created  17-NOV-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      INTEGER NTRSUP
      COMMON /ANGLIM/ NTROAD,PHIMIN,PHIMAX,TTHINF,TTHSUP,PTTR
      PARAMETER( NTRSUP =20 )
      REAL PHIMIN(NTRSUP),PHIMAX(NTRSUP),TTHINF(NTRSUP),TTHSUP(NTRSUP)
      REAL PTTR(NTRSUP)
      INTEGER NTROAD
