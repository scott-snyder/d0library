C
C- S. Abachi
C
      INTEGER NCLMAX1
      PARAMETER (NCLMAX1=500)
      INTEGER NCELL1,IETAC1(NCLMAX1),IPHIC1(NCLMAX1),LAYERC1(NCLMAX1)
      REAL DECELL1(NCLMAX1)
      COMMON /MCACEL1/ NCELL1,IETAC1,IPHIC1,LAYERC1,DECELL1 
C
C-    NCELL1   : Number of calorimeter adjacent cells hit by muon
C-    IETAC1   : List of ieta's of adjacent cells
C-    IPHIC1   : List of iphi's of adjacent cells
C-    LAYERC1  : List of layer's of adjacent cells
C-    DECELL1  : enenrgy measured in adjacent each cell
