      FUNCTION VCHT_PACK_HIT(IMATCH,ITIME,ID,ISTAT,IPEAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pack the first word of a VCHT hit
C-
C-   Returned value  : Input data packed into a single word
C-   Inputs  : IMATCH = 1 if matched hit, else 0 (1 bit)
C-             ITIME = (Drift_time + Time_offset) / Least_count (15 bits)
C-             ID = 2*WIRE + END (4 bits)
C-             ISTAT = hit status (2 bits)
C-             IPEAK = peak height in fadc counts (10 bits)
C-
C-   Created  27-OCT-1993   Peter Grudberg
C-   Updated  11-FEB-1994   Ed Oltman  VERSION = 1
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:VCHT.PARAMS'
C
      INTEGER VCHT_PACK_HIT
      INTEGER IMATCH, ITIME, ID, ISTAT, IPEAK
C
      INTEGER PACKED_WORD, OFFS
C----------------------------------------------------------------------
C
      PACKED_WORD = 0
      OFFS = 0
      CALL MVBITS(IPEAK,0,NBITPEAK,PACKED_WORD,OFFS)
      OFFS = OFFS + NBITPEAK
      CALL MVBITS(ISTAT,0,NBITSTAT,PACKED_WORD,OFFS)
      OFFS = OFFS + NBITSTAT
      CALL MVBITS(ID,0,NBITID,PACKED_WORD,OFFS)
      OFFS = OFFS + NBITID
      CALL MVBITS(ITIME,0,NBITTIME,PACKED_WORD,OFFS)
      OFFS = OFFS + NBITTIME
      CALL MVBITS(IMATCH,0,NBITMATCH,PACKED_WORD,OFFS)
      VCHT_PACK_HIT = PACKED_WORD
C
  999 RETURN
      END
