      FUNCTION VCHT_PACK_SECHEAD(SECTOR_ID, NHIT_VSEC, WORD_COUNT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pack the VCHT bank sector header word.
C-
C-   Returned value  : bit-packed word combining all inputs into a single word
C-   Inputs  : SECTOR_ID = 32*LAYER + SECTOR (8 bits)
C-             NHIT_VSEC = Number of VSEC hits for current sector (8 bits)
C-             WORD_COUNT = Number of words needed for the current sector
C-                  (equals twice the number of VWDA hits for this sector plus
C-                  one to hold this header word)
C-
C-      Format of SECHEAD word:
C-              SECTOR_ID(8 bits)|NHIT_VSEC(10 bits)|WORD_COUNT(14 bits)
C-            (MSB)                                                  (LSB)
C-
C-   Created  27-OCT-1993   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER VCHT_PACK_SECHEAD
      INTEGER SECTOR_ID, NHIT_VSEC, WORD_COUNT
C
      INTEGER PACKED_WORD
      INTEGER NBITID, NBITHIT, NBITCOUNT
      DATA NBITID, NBITHIT, NBITCOUNT / 8, 11, 13 /
C----------------------------------------------------------------------
C
      PACKED_WORD = 0
      CALL MVBITS(WORD_COUNT,0,NBITCOUNT,PACKED_WORD,0)
      CALL MVBITS(NHIT_VSEC,0,NBITHIT,PACKED_WORD,NBITCOUNT)
      CALL MVBITS(SECTOR_ID,0,NBITID,PACKED_WORD,NBITCOUNT+NBITHIT)
      VCHT_PACK_SECHEAD = PACKED_WORD
C
  999 RETURN
      END
