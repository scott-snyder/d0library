      INTEGER FUNCTION COMPRESS_ZEBRA_EVENT(IBUF, ISIZE, OBUF, 
     &  MAX_OSIZE, COMP_ALG, RANDOM, OSIZE, CHECKSUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Apply compression from the specified
C-                         input memory buffer to the output buffer.
C-                         This routine sets up virtual input and output
C-                         streams and calls the appropriate compression
C-                         routine.
C-
C-   Returned value:  Irrelevant for main entry point.
C-
C-   Inputs  : IBUF      - Input buffer.
C-             ISIZE     - Input buffer size (longwords).
C-             OBUF      - Output buffer.
C-             MAX_OSIZE - Maximum size of output buffer (longwords).
C-             COMP_ALG  - Index of compression algorithm
C-             RANDOM    - If .true., reinitialize dictionary between 
C-                         events to allow random access and reordering.
C-   Outputs : OSIZE     - Size of compressed data in output buffer.
C-             CHECKSUM  - Checksum for uncompressed data.
C-   Controls:
C-
C-   Entry points: COMPRESS_ZEBRA_EVENT_GETC   - Get character from IBUF.
C-                 COMPRESS_ZEBRA_EVENT_PUTB   - Put bit(s) into OBUF.
C-                                               (Big endian).
C-                 COMPRESS_ZEBRA_EVENT_LPUTB  - Put bit(s) into OBUF.
C-                                               (Little endian).
C-                 COMPRESS_ZEBRA_EVENT_FLUSHB - Flush bit buffer.
C- 
C-   Created  28-SEP-1994   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER COMPRESS_ZEBRA_EVENT_GETC
      INTEGER COMPRESS_ZEBRA_EVENT_PUTB
      INTEGER COMPRESS_ZEBRA_EVENT_LPUTB
      INTEGER COMPRESS_ZEBRA_EVENT_FLUSHB
      INTEGER IBUF(*), OBUF(*)
      INTEGER ISIZE, MAX_OSIZE, OSIZE, COMP_ALG, OLIMIT, ILIMIT
      LOGICAL RANDOM
C-
C- Pointers for fetching characters from input buffer and putting bits
C- into output buffer.
C-
      INTEGER IMASK, JMASK
      BYTE IMASKB(4), JMASKB(4)
      EQUIVALENCE (IMASK, IMASKB(1)), (JMASK, JMASKB(1))
      INTEGER IPT, BYTES      ! Next character to fetch from IBUF
      INTEGER OPT             ! Next character to fill in OBUF
      INTEGER OMASK, BITS     ! Partially constructed character.
      INTEGER DATA, NBITS     ! Arguments to put entry point
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
      COMPRESS_ZEBRA_EVENT = 0
C-
C- Set pointers for new event.
C-
      IMASK = 0               ! 4-byte input buffer.
      IPT = 0                 ! The number of words read from IBUF.
      BYTES = 0               ! The number of valid bytes in IMASK.
      OMASK = 0               ! 32-byt output buffer.
      OPT = 0                 ! The number of words written to OBUF.
      BITS = 0                ! The number of valid bits in OMASK.
      ILIMIT = ISIZE
      OLIMIT = MAX_OSIZE
      LOCAL_CHECKSUM = 0
C&IF VAXVMS
      IBUF_ADD = %LOC(IBUF)-4
      OBUF_ADD = %LOC(OBUF)-4
C&ELSE
C&      IBUF_ADD = D0_LOC(IBUF)-4
C&      OBUF_ADD = D0_LOC(OBUF)-4
C&ENDIF
C-
C- Compress data.  The compression routines are called with OPT and
C- LOCAL_CHECKSUM as arguments to prevent the optimizer from registerizing 
C- these variables.  The compression routines don't have arguments, but 
C- they modify OPT and LOCAL_CHECKSUM via ENTRY points.
C-
      IF(COMP_ALG.EQ.1)THEN
        IF(RANDOM)CALL COMPRESS_LZSS_INI
        CALL COMPRESS_LZSS(OPT, LOCAL_CHECKSUM)
        CHECKSUM = LOCAL_CHECKSUM
        OSIZE = OPT
      ELSEIF(COMP_ALG.EQ.2)THEN
        IF(RANDOM)CALL COMPRESS_LZW_INI
        CALL COMPRESS_LZW(OPT, LOCAL_CHECKSUM)
        CHECKSUM = LOCAL_CHECKSUM
        OSIZE = OPT
      ELSEIF(COMP_ALG.EQ.3)THEN
        IF(RANDOM)CALL COMPRESS_ZIP_INI
        CALL COMPRESS_ZIP(OPT, LOCAL_CHECKSUM)
        CHECKSUM = LOCAL_CHECKSUM
        OSIZE = OPT
      ELSE
        IF(RANDOM)CALL COMPRESS_NONE_INI
        CALL COMPRESS_NONE(OPT, LOCAL_CHECKSUM)
        CHECKSUM = LOCAL_CHECKSUM
        OSIZE = OPT
      ENDIF
      GO TO 999
C
      ENTRY COMPRESS_ZEBRA_EVENT_GETC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Utility entry point to return next character
C-                         in input buffer.
C-
C-   Returned value:  Character (0-255) or -1 if empty.
C-
C----------------------------------------------------------------------
      IF(BYTES.LE.0)THEN
        IF(IPT.GE.ILIMIT)THEN
          COMPRESS_ZEBRA_EVENT_GETC = -1
          GO TO 999
        ENDIF
        IPT = IPT + 1
C-
C- Get next word from input buffer.  Convert to integer byte-order (swap 
C- on VAX) and update checksum.
C-
        CALL UCOPY(%VAL(IBUF_ADD+4*IPT), IMASK, 1)
        JMASKB(1) = IMASKB(BYTE4)
        JMASKB(2) = IMASKB(BYTE3)
        JMASKB(3) = IMASKB(BYTE2)
        JMASKB(4) = IMASKB(BYTE1)
        LOCAL_CHECKSUM = IEOR(LOCAL_CHECKSUM, JMASK)
        BYTES = 4
      ENDIF
      COMPRESS_ZEBRA_EVENT_GETC = IMASKB(5-BYTES)
      IF(COMPRESS_ZEBRA_EVENT_GETC.LT.0)
     &  COMPRESS_ZEBRA_EVENT_GETC = COMPRESS_ZEBRA_EVENT_GETC + 256
      BYTES = BYTES - 1
      GO TO 999
        
      ENTRY COMPRESS_ZEBRA_EVENT_PUTB(DATA, NBITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Utility entry point to put an arbitrary number
C-                         of bits into output buffer.  Bit buffer
C-                         filled from most significant end.
C-
C-   Returned value:  Irrelevant
C-
C-   Inputs  : DATA      - Input data.
C-             NBITS     - Number of bits right-justified.
C-   Outputs :
C-   Controls:
C-
C----------------------------------------------------------------------
      COMPRESS_ZEBRA_EVENT_PUTB = 0
      SDATA = IAND(DATA, 2**NBITS-1)    ! Mask off unwanted bits
      EXCESS = 32 - BITS - NBITS        ! The number of extra bits in OMASK.
      IF(EXCESS.GE.0)THEN
C- Enough room in OMASK.
        OMASK = OMASK + SDATA*2**EXCESS
        BITS = BITS + NBITS
      ELSE
C- Not enough room in OMASK.  
C- First fill the rest of OMASK and write it to OBUF.
        OMASK = OMASK + SDATA/2**(-EXCESS)
        OPT = OPT + 1
        IF(OPT.GT.OLIMIT)CALL ERRMSG('Buffer overflow', 
     &    'COMPRESS_ZEBRA_EVENT_PUTB', ' ', 'F')
        CALL UCOPY(OMASK, %VAL(OBUF_ADD+4*OPT), 1)
C- Now start output buffer for next word.
        OMASK = ISHFT(SDATA, EXCESS+32)
        BITS = -EXCESS
      ENDIF
      GO TO 999
        
      ENTRY COMPRESS_ZEBRA_EVENT_LPUTB(DATA, NBITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Utility entry point to put an arbitrary number
C-                         of bits into output buffer.  Bit buffer filled
C-                         from least significant end.
C-
C-   Returned value:  Irrelevant
C-
C-   Inputs  : DATA      - Input data.
C-             NBITS     - Number of bits right-justified.
C-   Outputs :
C-   Controls:
C-
C----------------------------------------------------------------------
      COMPRESS_ZEBRA_EVENT_LPUTB = 0
      SDATA = IAND(DATA, 2**NBITS-1)    ! Mask off unwanted bits
      EXCESS = 32 - BITS - NBITS        ! The number of extra bits in OMASK.
      IF(EXCESS.GE.0)THEN
C- Enough room in OMASK.
        OMASK = OMASK + ISHFT(SDATA,BITS)
        BITS = BITS + NBITS
      ELSE
C- Not enough room in OMASK.  
C- First fill the rest of OMASK and write it to OBUF.
        OMASK = OMASK + ISHFT(SDATA,BITS)
        OPT = OPT + 1
        IF(OPT.GT.OLIMIT)CALL ERRMSG('Buffer overflow', 
     &    'COMPRESS_ZEBRA_EVENT_LPUTB', ' ', 'F')
        CALL UCOPY(OMASK, %VAL(OBUF_ADD+4*OPT), 1)
C- Now start output buffer for next word.
        OMASK = ISHFT(SDATA, BITS-32)
        BITS = -EXCESS
      ENDIF
      GO TO 999
        
      ENTRY COMPRESS_ZEBRA_EVENT_FLUSHB()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Utility entry point to flush output buffer.
C-
C-   Returned value:  Irrelevant
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C----------------------------------------------------------------------
      COMPRESS_ZEBRA_EVENT_FLUSHB = 0
      IF(BITS.GT.0)THEN
        OPT = OPT + 1
        IF(OPT.GT.OLIMIT)CALL ERRMSG('Buffer overflow', 
     &    'COMPRESS_ZEBRA_EVENT_FLUSHB', ' ', 'F')
        CALL UCOPY(OMASK, %VAL(OBUF_ADD+4*OPT), 1)
        BITS = 0
        OMASK = 0
      ENDIF
 999  RETURN
      END
