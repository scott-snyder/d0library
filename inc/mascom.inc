C++++	This is Mascom.Inc
C
C		It is the storage for the invariant mass calculations
C
      COMMON /MASCOM/   LIMAS,  IBYX,   VAM,   LIA,   LIP,   NUA,   NUP
C
      INTEGER*4         LIMAS(100),    IBYX(18)
      INTEGER*4         LIA(6),        LIP(18),         NUA,
     1  NUP
C
      REAL*8            VAM(246),      PS(4)
      REAL*8            DFMEF(100),    DPS(4,4),        FMEF(100)
C
      EQUIVALENCE            (DFMEF ,VAM(   1)),  (DPS   ,VAM( 101)),
     1  (FMEF  ,VAM( 117)),  (PS    ,VAM( 217))
C
C----	End of Mascom.Inc
