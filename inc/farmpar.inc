C----------------------------------------------------------------------
C-
C-   Created   4-MAR-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      INTEGER NUMUSE,OUTUNI,OUTNUM
      CHARACTER*64 PARIN(6),MSG_SENT
      CHARACTER*14 PROCNAM
      CHARACTER*6 HOSTNAM
      INTEGER FARM_EVENT_IN,FARM_EVENT_OUT
      INTEGER FARM_BARRIER,FARM_INDX
      COMMON/FARMPAR/NUMUSE,PARIN,PROCNAM,HOSTNAM,OUTUNI,OUTNUM,
     &               MSG_SENT,FARM_EVENT_IN,FARM_EVENT_OUT,FARM_BARRIER,
     &               FARM_INDX
