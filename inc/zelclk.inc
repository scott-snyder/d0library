C----------------------------------------------------------------------
C-  a temporary link area for ZTRKs (Central Detector tracks) in a road
C-
C-   Created  19-NOV-1990   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      INTEGER ZTMAX
      PARAMETER( ZTMAX = 450 )
      COMMON /ZELCLK/ ZELCLK(2), LDTRK, LVTXT, LFDCT, LZTMP(ZTMAX),
     &                ZLINKT(ZTMAX)
C
      INTEGER ZELCLK, ZLINKT, LDTRK, LVTXT, LFDCT, LZTMP
