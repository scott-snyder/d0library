      SUBROUTINE GET_EVENT_SIZE(NWDS_USED,NWDS_RAW,NWDS_ADDED,
     &  NWDS_MAX,NWDS_BNK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return event size in ZEBCOM
C-      to measure input event size, should be run before any new banks added
C-
C-   Inputs  : zebra banks
C-   Outputs : size occupied by:
C-            NWDS_USED whole event (lncluding dead banks)
C-            NWDS_RAW  raw data
C-            NWDS_ADDED   data beyond raw
C-                         if run on raw .ZRD it gives L2 banks size
C-            NWDS_MAX     max # words allowed in this division
C-            NWDS_BNK(8)  size of individual raw data banks 
C-   Controls:
C-
C-   Created   7-SEP-1992   James T. Linnemann
C-   Updated  22-SEP-1992   Du\v{s}an Ne\v{s}i\'{c} debug, Add NWDS_BNK(8)
C-   Updated  27-OCT-1992   James T. Linnemann  fix initialization, comments 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER I, IER, IQUEST
      INTEGER LRAW
      INTEGER NEED,  N_EXTEND
      INTEGER NWDS_ADDED, NWDS_BNK(8), NWDS_MAX, NWDS_USED, NWDS_RAW
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      COMMON/QUEST/IQUEST(100)
C----------------------------------------------------------------------
C
C...see Zebra MZ refernce manual 4.02
      NEED = 1      !don't really try to trigger garbage collection
      CALL MZNEED(IXMAIN,NEED,'G')
C
C...number of extra words needed to get NEED
      N_EXTEND = IQUEST(11)     !negative if you don't have enough
C              ! will garb collect to try to get as close as possible to request
C
C...number of words actually occupied by banks in this division, including dead
      NWDS_USED = IQUEST(12)
C
C...max number of words for this division
      NWDS_MAX = IQUEST(13)
C
C...now get # raw data words = HEAD + TRGR + ....
      NWDS_RAW = IQ(LHEAD-1)    !head bank
C
      DO I = 1,8
        NWDS_BNK(I) = 0
        LRAW = LQ(LHEAD-I)
        IF (LRAW .GT. 0) THEN 
          NWDS_BNK(I) = IQ(LRAW-1)
          NWDS_RAW = NWDS_RAW + IQ(LRAW-1)
        ENDIF
      ENDDO
C
C...if this is run before RECO does anything, it gives you how much L2 added
      NWDS_ADDED = NWDS_USED - NWDS_RAW
  999 RETURN
      END
