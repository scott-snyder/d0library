      SUBROUTINE DMP_SETNUM(NUM_DUMPS,NUM_SKIPS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      set number of dumps to do and number of events to skip
C-      in DUMP facility
C-   Inputs  : 
C-   NUM_DUMPS = number of dumps requested
C-   NUM_SKIPS = number of events to skip
C-
C-   Created  18-SEP-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUM_DUMPS,NUM_SKIPS
      INCLUDE 'D0$INC:DUMP.INC'
C----------------------------------------------------------------------
C
      NDUMP=NUM_DUMPS
      NSKIP=NUM_SKIPS
  999 RETURN
      END
