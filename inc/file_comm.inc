C----------------------------------------------------------------------
C-
C-   Created  29-NOV-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      CHARACTER FILEVN*80,EVOUT*80,BEGOUT*80,EVBIG*80
      CHARACTER*80 FILGET
      INTEGER MAXSEL,IRDUNI,EVREAD
      LOGICAL RUNOK
      COMMON /FILE_COMM/ FILEVN,EVOUT,BEGOUT,EVBIG,MAXSEL,IRDUNI,EVREAD,
     &       RUNOK,FILGET
