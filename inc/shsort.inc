C----------------------------------------------------------------------
C-
C-   Created  25-SEP-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      INTEGER NSRTMX
      PARAMETER( NSRTMX = 1000 )
      COMMON /SHSORT/ RKEY_SRT(NKEY,NSRTMX),CYC_SRT(NSRTMX),
     &  IETAC_SRT(NSRTMX),IPHIC_SRT(NSRTMX),KEYS_SRT(NSRTMX),
     &  P4_SRT(4,NSRTMX),ITRA_SRT(NSRTMX),
     &  IMAP(NSRTMX),NSRT
      INTEGER RKEY_SRT,CYC_SRT,IETAC_SRT,IPHIC_SRT,KEYS_SRT,IMAP,NSRT
      REAL    P4_SRT
      INTEGER ITRA_SRT
