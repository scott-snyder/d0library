C
C- S. Abachi
C
      INTEGER NCLMAX
      PARAMETER (NCLMAX=500)
      INTEGER NCELL,IETAC(NCLMAX),IPHIC(NCLMAX),LAYERC(NCLMAX)
      REAL DECELL(NCLMAX),TLEN(NCLMAX)
      COMMON /MCACEL/ NCELL,IETAC,IPHIC,LAYERC,DECELL,TLEN
C
C-     NCELL   : Number of calorimeter cells hit by muon
C-     IETAC   : List of ieta's of cells
C-     IPHIC   : List of iphi's of cells
C-     LAYERC  : List of layer's of cells
C-     DECELL  : enenrgy measured in each cell
C-     TLEN    : Track lengths in each cell
