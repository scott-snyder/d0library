C----------------------------------------------------------------------
C-
C-   Created  16-MAY-1989   Serban D. Protopopescu
C-   Updated   1-JUN-1991   Rajendran Raja   
C-
C----------------------------------------------------------------------
      INTEGER NPAWC
      PARAMETER (NPAWC=100000)
      INTEGER PAGELEN
      PARAMETER( PAGELEN = 128 )        
      INTEGER MAXPAGES
      PARAMETER( MAXPAGES = NPAWC/PAGELEN )
      CHARACTER*32 GNAME
C
      COMMON/PAWC/NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LPMAIN,
     &  HCV(9989)
C
      REAL HMEMOR(PAGELEN*MAXPAGES)
      INTEGER NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC,LPMAIN,HCV
      EQUIVALENCE(NWPAW,HMEMOR(1))
      INTEGER IP(2),LP(8000)
      REAL    P(2)
      EQUIVALENCE (LP(1),LPMAIN),(IP(1),LP(9)),(P(1),IP(1))
