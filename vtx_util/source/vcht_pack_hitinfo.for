      FUNCTION VCHT_PACK_HITINFO(IWINDOW,IWEIGHT,THR1,THR2,THR3,TABLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pack hitfinding information word for the header of
C-                         the VCHT bank.
C-
C-   Returned value  : bit-packed word
C-   Inputs  : IWINDOW = time-matching window (1 ns least count)
C-             IWEIGHT = weight parameter used in timefinding (.05 least count)
C-             THR1,THR2,THR3 = hitfinding thresholds
C-             TABLE = version number of bilinear table used
C-
C-   Created  29-OCT-1993   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER VCHT_PACK_HITINFO
      INTEGER IWINDOW, IWEIGHT, THR1, THR2, THR3, TABLE
C
      INTEGER PACKED_WORD, OFFS
      INTEGER NBITWINDOW, NBITWEIGHT, NBITTHR, NBITTABLE
      DATA NBITWINDOW, NBITWEIGHT, NBITTHR, NBITTABLE / 6, 5, 5, 4 /
C----------------------------------------------------------------------
C
      PACKED_WORD = 0
      OFFS = 0
      CALL MVBITS(TABLE,0,NBITTABLE,PACKED_WORD,OFFS)
      OFFS = OFFS + NBITTABLE
      CALL MVBITS(THR3,0,NBITTHR,PACKED_WORD,OFFS)
      OFFS = OFFS + NBITTHR
      CALL MVBITS(THR2,0,NBITTHR,PACKED_WORD,OFFS)
      OFFS = OFFS + NBITTHR
      CALL MVBITS(THR1,0,NBITTHR,PACKED_WORD,OFFS)
      OFFS = OFFS + NBITTHR
      CALL MVBITS(IWEIGHT,0,NBITWEIGHT,PACKED_WORD,OFFS)
      OFFS = OFFS + NBITWEIGHT
      CALL MVBITS(IWINDOW,0,NBITWINDOW,PACKED_WORD,OFFS)
      VCHT_PACK_HITINFO = PACKED_WORD
C
  999 RETURN
      END
