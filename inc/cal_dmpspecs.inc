C----------------------------------------------------------------------
C
C   Created  17-APR-1989   K. Wyatt Merritt
C
C----------------------------------------------------------------------
C
      LOGICAL HEX_DUMP,ALL_CRATES,ALL_BANKS
C
      INTEGER CRATE_LIST(6),EVT_LIST(10)
C
      CHARACTER*4 BANK_LIST(4)
C
      COMMON /CAL_DMPSPECS/ HEX_DUMP,ALL_CRATES,ALL_BANKS,CRATE_LIST,
     &  EVT_LIST,BANK_LIST
