C----------------------------------------------------------------------
C-
C-   Created  21-FEB-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
C
      COMMON /LSQ_PARS/ TOP_DIRECTORY,
     &  SUB_DIRECTORY,
     &  RZ_UNIT,KEY_DESCR,CHTAG,
     &  MSG,NEW_RZ
C
      CHARACTER*32 SUB_DIRECTORY
      CHARACTER*80 MSG
      CHARACTER*80 TOP_DIRECTORY
      INTEGER NKEYS
      PARAMETER( NKEYS = 1 )
      CHARACTER*4 CHTAG(NKEYS),KEY_DESCR
      INTEGER RZ_UNIT
      LOGICAL NEW_RZ
C

