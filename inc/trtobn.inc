C----------------------------------------------------------------------
C
C ****  /TRTOBN/ contains links from track to FADC bank
C
C-
C-   Created  27-FEB-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      INTEGER NMAXBN,NTRM
      PARAMETER( NMAXBN =  100)
      PARAMETER( NTRM=10)
      COMMON /TRTOBN/ TRTOBN(2),TLPINF(NTRM,6,NMAXBN),NBPNT(6,NMAXBN)
      INTEGER         TRTOBN   ,TLPINF               ,NBPNT
