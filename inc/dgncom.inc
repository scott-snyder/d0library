C----------------------------------------------------------------------
C-
C-   Created  10-MAR-1995   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      INTEGER MAXTAG
      PARAMETER( MAXTAG = 512 )
C
      INTEGER NTAG, ITAG(MAXTAG), NFIELD
      CHARACTER*5  PREFIX(MAXTAG)
      CHARACTER*32 TAG(MAXTAG), FIELD(MAXTAG)
      REAL    XTUPLE(MAXTAG),XSCALE(MAXTAG)
      LOGICAL DONE_BEGIN
C
      COMMON /DGNCOM/ NTAG, ITAG, NFIELD, DONE_BEGIN, XTUPLE, XSCALE
      COMMON /DGNCOMC/TAG, FIELD, PREFIX
