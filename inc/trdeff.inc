C----------------------------------------------------------------------
C-
C-   Created  15-NOV-1989   A. Zylberstejn
C-   TRD computed electron efficiencies and hadron rejection
C-
C- ELEFF(1)= El. efficiency E tot
C- ELEFF(2)= El. efficiency E tot  3 TEC hit
C- ELEFF(3)= El. efficiency Trunc. mean
C- ELEFF(4)= El. efficiency Trunc. mean 3 TEC hit
C- ELEFF(5)= El. efficiency Likelihood E tot
C- ..............To be completed.........
C----------------------------------------------------------------------
      COMMON /TRDEFF/ ELEFF(8),PIREJ(8)
      REAL ELEFF,PIREJ
