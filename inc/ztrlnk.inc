C----------------------------------------------------------------------
C-  a temporary link area for ZTRKs (Central Detector tracks) in a road
C-
C-   Created  04-NOV-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      INTEGER ZMAX
      PARAMETER( ZMAX = 450 )
      COMMON /ZTRLNK/ ZTRLNK(2), ZLINKS(ZMAX)
C
      INTEGER ZTRLNK, ZLINKS
