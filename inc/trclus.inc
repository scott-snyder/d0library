C----------------------------------------------------------------------
C-  TRD CLUSTERS
C-
C-   Created  12-NOV-1990   A. Zylberstejn
C-
C----------------------------------------------------------------------
      INTEGER NMCLUS,NCLUST
      PARAMETER (NMCLUS=20)
      REAL ENERCL,POSICL
      COMMON/TRCLUS/NCLUST(3),ENERCL(NMCLUS,3),POSICL(NMCLUS,3)
