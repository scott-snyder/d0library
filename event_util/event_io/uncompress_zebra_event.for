      INTEGER FUNCTION UNCOMPRESS_ZEBRA_EVENT(IBUF, ISIZE, OBUF, 
     &  MAX_OSIZE, COMP_ALG, OSIZE, CHECKSUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Apply uncompression from the specified
C-                         input memory buffer to the output buffer.
C-                         This routine sets up virtual input and output
C-                         streams and calls the appropriate uncompression
C-                         routine.
C-
C-   Returned value:  Irrelevant for main entry point.
C-
C-   Inputs  : IBUF      - Input buffer.
C-             ISIZE     - Input buffer size (longwords).
C-             OBUF      - Output buffer.
C-             MAX_OSIZE - Maximum size of output buffer (longwords).
C-             COMP_ALG  - Index of uncompression algorithm
C-   Outputs : OSIZE     - Size of uncompressed data in output buffer.
C-             CHECKSUM  - Checksum of uncompressed data.
C-   Controls:
C-
C-   Entry points: UNCOMPRESS_ZEBRA_EVENT_GETB   - Get bit(s) from IBUF.
C-                                                 (Big endian).
C-                 UNCOMPRESS_ZEBRA_EVENT_LGETB  - Get bit(s) from IBUF.
C-                                                 (Little endian).
C-                 UNCOMPRESS_ZEBRA_EVENT_PUTC   - Put character into OBUF.
C-                 UNCOMPRESS_ZEBRA_EVENT_FLUSHC - Flush character buffer.
C- 
C-   Created  28-SEP-1994   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER UNCOMPRESS_ZEBRA_EVENT_PUTC
      INTEGER UNCOMPRESS_ZEBRA_EVENT_FLUSHC
      INTEGER UNCOMPRESS_ZEBRA_EVENT_GETB
      INTEGER UNCOMPRESS_ZEBRA_EVENT_LGETB
      INTEGER IBUF(*), OBUF(*)
      INTEGER ISIZE, MAX_OSIZE, OSIZE, COMP_ALG, ILIMIT, OLIMIT
C-
C- Pointers for fetching characters from input buffer and putting bits
C- into output buffer.
C-
      INTEGER OMASK, PMASK
      BYTE OMASKB(4), PMASKB(4)
      EQUIVALENCE (OMASK, OMASKB(1)), (PMASK, PMASKB(1))
      INTEGER OPT, BYTES      ! Next character to fill in OBUF.
      INTEGER IPT             ! Next character to fetch from IBUF.
      INTEGER IMASK, BITS     ! Single word bit buffer.  At any time,
                              !   the rightmost BITS bits are significant.
      INTEGER DATA            ! Output data (PUTC).
      INTEGER NBITS           ! Number of bits to fetch (GETB).
      INTEGER SDATA, EXCESS
      INTEGER CHECKSUM, LOCAL_CHECKSUM
      INTEGER IBUF_ADD, OBUF_ADD
C&IF VAXVMS
C&ELSE
C&      INTEGER D0_LOC
C&ENDIF
      SAVE IPT, IMASK, BYTES, OPT, OMASK, BITS, LOCAL_CHECKSUM,
     &  ILIMIT, OLIMIT, IBUF_ADD, OBUF_ADD
C----------------------------------------------------------------------
      UNCOMPRESS_ZEBRA_EVENT = 0
C-
C- Set pointers for new event.
C-
      OMASK = 0               ! 4-byte output buffer.
      OPT = 0                 ! The number of words written to OBUF.
      BYTES = 0               ! The number of valid bytes in OMASK.
      IMASK = 0               ! 32-bit input buffer.
      IPT = 0                 ! The number of words read from in IBUF.
      BITS = 0                ! The number of valid bits in IMASK.
      OLIMIT = MAX_OSIZE
      ILIMIT = ISIZE
      LOCAL_CHECKSUM = 0
C&IF VAXVMS
      IBUF_ADD = %LOC(IBUF)-4
      OBUF_ADD = %LOC(OBUF)-4
C&ELSE
C&      IBUF_ADD = D0_LOC(IBUF)-4
C&      OBUF_ADD = D0_LOC(OBUF)-4
C&ENDIF
C-
C- Uncompress data.  The uncompression routines are called with OPT and
C- LOCAL_CHECKSUM as arguments to prevent the optimizer from registerizing 
C- these variables.  The uncompression routines don't have arguments, but 
C- they modify OPT and LOCAL_CHECKSUM via ENTRY points.
C-
      IF(COMP_ALG.EQ.1)THEN
        CALL UNCOMPRESS_LZSS(OPT, LOCAL_CHECKSUM)
        CHECKSUM = LOCAL_CHECKSUM
        OSIZE = OPT
      ELSEIF(COMP_ALG.EQ.2)THEN
        CALL UNCOMPRESS_LZW(OPT, LOCAL_CHECKSUM)
        CHECKSUM = LOCAL_CHECKSUM
        OSIZE = OPT
      ELSEIF(COMP_ALG.EQ.3)THEN
        CALL UNCOMPRESS_ZIP(OPT, LOCAL_CHECKSUM)
        CHECKSUM = LOCAL_CHECKSUM
        OSIZE = OPT
      ELSE
        CALL UNCOMPRESS_NONE(OPT, LOCAL_CHECKSUM)
        CHECKSUM = LOCAL_CHECKSUM
        OSIZE = OPT
      ENDIF
      GO TO 999
C
      ENTRY UNCOMPRESS_ZEBRA_EVENT_PUTC(DATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Utility entry point to write one character
C-                         to output buffer.
C-
C-   Returned value:  Irrelevant
C-
C----------------------------------------------------------------------
      UNCOMPRESS_ZEBRA_EVENT_PUTC = 0
      IF(BYTES.GE.4)THEN
        OPT = OPT + 1
        IF(OPT.GT.OLIMIT)CALL ERRMSG('Buffer overflow', 
     &    'UNCOMPRESS_ZEBRA_EVENT_PUTC', ' ', 'F')
C-
C- Put next word into output buffer.  Convert to integer byte-order (swap 
C- on VAX) and update checksum.
C-
        CALL UCOPY(OMASK,%VAL(OBUF_ADD+4*OPT),1)
        PMASKB(1) = OMASKB(BYTE4)
        PMASKB(2) = OMASKB(BYTE3)
        PMASKB(3) = OMASKB(BYTE2)
        PMASKB(4) = OMASKB(BYTE1)
        LOCAL_CHECKSUM = IEOR(LOCAL_CHECKSUM, PMASK)
C        print *,opt,local_checksum
        BYTES = 0
        OMASK = 0
      ENDIF
      BYTES = BYTES + 1
      IF(DATA.LT.128)THEN
        OMASKB(BYTES) = DATA
      ELSE
        OMASKB(BYTES) = DATA-256
      ENDIF
      GO TO 999
C
      ENTRY UNCOMPRESS_ZEBRA_EVENT_FLUSHC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Utility entry point to flush output buffer.
C-
C-   Returned value:  Irrelevant
C-
C----------------------------------------------------------------------
      UNCOMPRESS_ZEBRA_EVENT_FLUSHC = 0
      IF(BYTES.GE.1)THEN
        OPT = OPT + 1
        IF(OPT.GT.OLIMIT)CALL ERRMSG('Buffer overflow', 
     &    'UNCOMPRESS_ZEBRA_EVENT_FLUSHC', ' ', 'F')
C-
C- Put next word into output buffer.  Convert to integer byte-order (swap 
C- on VAX) and update checksum.
C-
        CALL UCOPY(OMASK,%VAL(OBUF_ADD+4*OPT),1)
        PMASKB(1) = OMASKB(BYTE4)
        PMASKB(2) = OMASKB(BYTE3)
        PMASKB(3) = OMASKB(BYTE2)
        PMASKB(4) = OMASKB(BYTE1)
        LOCAL_CHECKSUM = IEOR(LOCAL_CHECKSUM, PMASK)
C        print *,opt,local_checksum
        BYTES = 0
        OMASK = 0
      ENDIF
      GO TO 999
        
      ENTRY UNCOMPRESS_ZEBRA_EVENT_GETB(NBITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Utility entry point to get an arbitrary number
C-                         of bits from input buffer.  Bit buffer emptied
C-                         from most significant end.
C-
C-   Returned value:  Bits right-justified or -1 if empty.
C-
C-   Inputs  : NBITS     - Number of bits.
C-   Outputs :
C-   Controls:
C-
C----------------------------------------------------------------------
      EXCESS = BITS - NBITS    ! The number of extra bits in the input buffer.
      IF(EXCESS.GE.0)THEN
C- Enough bits available in IMASK
        SDATA = IBITS(IMASK, EXCESS, NBITS)
        BITS = EXCESS
      ELSE
C- Not enough bits available in IMASK.  Need to get next word, but first
C- transfer all remaining bits in IMASK.
        IF(BITS.GT.0)THEN
          SDATA = IBITS(IMASK, 0, BITS)*2**(-EXCESS)
        ELSE
          SDATA = 0
        ENDIF
C- Load IMASK with next word from the input buffer, if there is one.  If
C- not, return -1 or crash on subsequent attempts.
        IPT = IPT + 1
        IF(IPT.GT.ILIMIT)THEN
          IF(IPT.GT.ILIMIT+1)CALL ERRMSG('No more data', 
     &      'UNCOMPRESS_ZEBRA_EVENT_GETB', ' ', 'F')
          UNCOMPRESS_ZEBRA_EVENT_GETB = -1
          BITS = 0
          GO TO 999
        ENDIF
        CALL UCOPY(%VAL(IBUF_ADD+4*IPT), IMASK, 1)
C- Now fill remaining bits.
        SDATA = SDATA + IBITS(IMASK, 32+EXCESS, -EXCESS)
        BITS = 32+EXCESS
      ENDIF
      UNCOMPRESS_ZEBRA_EVENT_GETB = SDATA
      GO TO 999

      ENTRY UNCOMPRESS_ZEBRA_EVENT_LGETB(NBITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Utility entry point to get an arbitrary number
C-                         of bits from input buffer.  Bit buffer emptied
C-                         from least significant end.
C-
C-   Returned value:  Bits right-justified or -1 if empty.
C-
C-   Inputs  : NBITS     - Number of bits.
C-   Outputs :
C-   Controls:
C-
C----------------------------------------------------------------------
      EXCESS = BITS - NBITS    ! The number of extra bits in the input buffer.
      IF(EXCESS.GE.0)THEN
C- Enough bits available in IMASK
        SDATA = IBITS(IMASK, 32-BITS, NBITS)
        BITS = EXCESS
      ELSE
C- Not enough bits available in IMASK.  Need to get next word, but first
C- transfer all remaining bits in IMASK.
        IF(BITS.GT.0)THEN
          SDATA = IBITS(IMASK, 32-BITS, BITS)
        ELSE
          SDATA = 0
        ENDIF
C- Load IMASK with next word from the input buffer, if there is one.  If
C- not, return SDATA from previous word and crash on subsequent attempts.
        IPT = IPT + 1
        IF(IPT.GT.ILIMIT)THEN
          IF(IPT.GT.ILIMIT+1)CALL ERRMSG('No more data', 
     &      'UNCOMPRESS_ZEBRA_EVENT_LGETB', ' ', 'F')
          UNCOMPRESS_ZEBRA_EVENT_LGETB = SDATA
          BITS = 0
          GO TO 999
        ENDIF
        CALL UCOPY(%VAL(IBUF_ADD+4*IPT), IMASK, 1)
C- Now fill remaining bits.
        SDATA = SDATA + IBITS(IMASK, 0, -EXCESS)*2**(NBITS+EXCESS)
        BITS = 32+EXCESS
      ENDIF
      UNCOMPRESS_ZEBRA_EVENT_LGETB = SDATA
      GO TO 999

 999  RETURN
      END

