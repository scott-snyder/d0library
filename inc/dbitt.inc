C----------------------------------------------------------------------
C-
C-   Created  12-SEP-1989   Dale A. Ross, MSU
C-
C----------------------------------------------------------------------
C
C    Conversion table between the Data Block Index and the L-1
C    (L1ETAC,L1PHIC) indices.
C
      COMMON /DBITT/ DBITT
      BYTE           DBITT(0:(NETAL1*NPHIL1*2)-1,2)
C
C                    DBITT(dbi,1) <--- L1ETAC
C                    DBITT(dbi,2) <--- L1PHIC
C-                        (range of dbi is [0,2*NPHIL1*NETAL1-1] )
