C----------------------------------------------------------------------
C-
C-   Created  15-NOV-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      INTEGER LNKMX
      PARAMETER( LNKMX = 5 )
      COMMON /CSFWC/ STRLNK(LNKMX),REFLNK(LNKMX)
      INTEGER LCSFW,LCSFC,LCSFC1,LCSFH
      EQUIVALENCE (STRLNK(1),LCSFW), (STRLNK(2),LCSFC),
     &  (STRLNK(3),LCSFC1),(STRLNK(4),LCSFH)
      INTEGER STRLNK,REFLNK
