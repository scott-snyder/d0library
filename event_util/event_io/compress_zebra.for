       LOGICAL FUNCTION COMPRESS_ZEBRA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts an entire Zebra structure to a single 
C-                         bank (ZDST) and optionally compresses it.
C-
C-   Returned value:  .true. -- everything OK.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Entry points:  COMPRESS_ZEBRA_DST - DST mode compression.
C-                  COMPRESS_ZEBRA_INI - Initialization.
C-                  COMPRESS_ZEBRA_END - Summary.
C-
C-   Created  28-Sep-1994   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL COMPRESS_ZEBRA_DST, COMPRESS_ZEBRA_INI, COMPRESS_ZEBRA_END
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZZDST.LINK'
C-
C- Variables from COMPRESS_ZEBRA_RCP.
C-
      LOGICAL DO_COMPRESS_ZEBRA
C-
C- Address of memory buffer and Zebra I/O stuff
C-
      INTEGER BUFAD              ! Current buffer address.
      INTEGER BSIZE              ! Current buffer sizes in longwords.
      LOGICAL OK, LIB$GET_VM, LIB$FREE_VM
      INTEGER MLUN
      INTEGER RECORD_SIZE, MIN_BUFFER_SIZE, MAX_BUFFER_SIZE
      INTEGER USIZE, CSIZE       ! Uncompressed/compressed event size
      INTEGER MAX_CSIZE          ! Upper limit of compressed size.
C-
C- Compression parameters
C-
      INTEGER COMP_ALG
      LOGICAL RANDOM
      LOGICAL KEEP_HSTR
      LOGICAL RECYCLE_ZDST, RECYCLE
      INTEGER RECYCLE_NUM
      INTEGER CHECKSUM
      INTEGER OFFSET
      LOGICAL STA_MODE, DST_MODE, UDST_DROP_RECO
      INTEGER VERSION
      PARAMETER (VERSION=1)
      LOGICAL ROUND
      INTEGER FLOATING_BITS
      INTEGER ZIP_PACK_LEVEL
      INTEGER LZSS_INDEX_BITS, LZSS_LENGTH_BITS
      INTEGER LZW_DICTIONARY_BITS
C-
C- Statistics
C-
      INTEGER NUM_EVENT(2)
      INTEGER INPUT_SIZE(2), OUTPUT_SIZE(2)
      REAL RATIO(2)
C-
C- Links, etc.
C-
      INTEGER LZDST, GZZDST
      INTEGER LUDST, GZUDST
      INTEGER LRECO, GZRECO
      INTEGER LANLS, GZANLS
      INTEGER LHSUM, GZHSUM
      INTEGER L, NL
      INTEGER LBANK, LCOMP
      INTEGER LZLAST
C-
C- Other variables and functions.
C-
      INTEGER I
      INTEGER IER
      INTEGER LUN, SSUNIT
      INTEGER IHOOK        ! 1 = STA, 2 = DST
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C-
C- Main entry point.
      COMPRESS_ZEBRA = .TRUE.
      IF(.NOT.STA_MODE)GO TO 999
      IHOOK = 1
      GO TO 1
C
      ENTRY COMPRESS_ZEBRA_DST()
C-
C- DST mode entry point.
C-
      COMPRESS_ZEBRA_DST = .TRUE.
      IF(.NOT.DST_MODE)GO TO 999
      IHOOK = 2
C-
C- Come here from either the regular entry point (COMPRESS_ZEBRA) or the
C- DST mode entry point (COMPRESS_ZEBRA_DST).  Compression may take place
C- during either or both entry points.
C-
 1    CONTINUE
      IF(LHEAD.EQ.0 .OR. .NOT.DO_COMPRESS_ZEBRA)GO TO 999
C-
C- Do not attempt to compress non-event data (STP files, etc.)
C-
      IF(IQ(LHEAD-4).NE.4HHEAD)GO TO 999
C-
C- Check for a ZDST bank.  Do not attempt to compress an event more than
C- once.
C-
      LZDST = GZZDST()
      IF(LZDST.NE.0)GO TO 999
      NUM_EVENT(IHOOK) = NUM_EVENT(IHOOK) + 1
C-
C- Drop STA banks.
C-
      IF(IHOOK.EQ.1)CALL EVMARK('STA')         ! Emulate action of EVMARK.
C-
C- UDST special processing goes here.  Call OUTUTAG to drop UTAG banks on
C- all events after the first.  Also, drop empty RECO banks added by
C- D0USER frame here.
C-
      LUDST = GZUDST()
      IF(LUDST.NE.0)THEN
        CALL OUTUTAG(20, NUM_EVENT(IHOOK))
        LRECO = GZRECO(0)
        IF(LRECO.NE.0 .AND. UDST_DROP_RECO)
     &    CALL MZDROP(IXCOM, LRECO, ' ')
      ENDIF
C-
C- Rounding of floating point goes here, if requested.
C-
      IF(ROUND)CALL ROUND_FLOATING(FLOATING_BITS)
C-
C- Here we drop the dataless headers RECO, ANLS and HSUM to avoid having 
C- to compress them, saving about 10-15 words for each bank, and also 
C- bringing HSTR beneath HEAD.  The banks that were hanging underneath 
C- them, if any, are shunted into a linear structure. 
C-
C- RECO bank.
C-
      LRECO = GZRECO()
      IF(LRECO.NE.0)THEN
        NL = IQ(LRECO-3)
        DO I = 1,NL
          LBANK = LQ(LRECO-I)
          IF(LBANK.NE.0)THEN
            IF(IQ(LBANK-4).EQ.4HHSTR)THEN
              L = LZLAST(IXCOM, LHEAD)
            ELSE
              L = LZLAST(IXCOM, LRECO)
            ENDIF
            CALL ZSHUNT(IXCOM, LBANK, L, 0, 1)
          ENDIF
        ENDDO
        CALL MZDROP(IXCOM, LRECO, ' ')
      ENDIF
C-
C- ANLS bank.
C-
      LANLS = GZANLS()
      IF(LANLS.NE.0)THEN
        NL = IQ(LANLS-3)
        DO I = 1,NL
          LBANK = LQ(LANLS-I)
          IF(LBANK.NE.0)THEN
            L = LZLAST(IXCOM, LANLS)
            CALL ZSHUNT(IXCOM, LBANK, L, 0, 1)
          ENDIF
        ENDDO
        CALL MZDROP(IXCOM, LANLS, ' ')
      ENDIF
C-
C- HSUM bank.
C-
      LHSUM = GZHSUM()
      IF(LHSUM.NE.0)THEN
        NL = IQ(LHSUM-3)
        DO I = 1,NL
          LBANK = LQ(LHSUM-I)
          IF(LBANK.NE.0)THEN
            L = LZLAST(IXCOM, LHSUM)
            CALL ZSHUNT(IXCOM, LBANK, L, 0, 1)
          ENDIF
        ENDDO
        CALL MZDROP(IXCOM, LHSUM, ' ')
      ENDIF
C-
C- Now shunt all banks hanging under the HEAD bank to linearly follow 
C- the HEAD bank.  This saves having to compress a copy of the HEAD bank.
C- Any HSTR banks hanging under RECO are already at the head of the 
C- structure following HEAD.
C-
      NL = IQ(LHEAD-3)
      DO I = 1, NL
        LBANK = LQ(LHEAD-I)
        IF(LBANK.NE.0)THEN
          L = LZLAST(IXCOM, LHEAD)
          CALL ZSHUNT(IXCOM, LBANK, L, 0, 1)
        ENDIF
      ENDDO
 10   CONTINUE
C-
C- Bail out if there are no banks linearly following the head bank at this
C- point.  In that case, the HEAD bank is the only bank and the event can not
C- be compressed.
C-
      IF(LQ(LHEAD).EQ.0)GO TO 999
C-
C- See if we want to recycle an existing ZDST bank instead of compressing.
C-
      RECYCLE = IHOOK.EQ.1 .AND. RECYCLE_ZDST .AND. RECYCLE_NUM.NE.0 
     &          .AND. LSLINK(RECYCLE_NUM).NE.0
C-
C- Capture banks following HEAD in linear memory buffer.  If KEEP_HSTR
C- is .TRUE., skip past one HSTR bank at the start of the linear structure.
C-
 20   CONTINUE
      LCOMP = LQ(LHEAD)
      IF(LCOMP.NE.0 .AND. LQ(LCOMP).NE.0 .AND.
     &   KEEP_HSTR .AND. IQ(LCOMP-4).EQ.4HHSTR)
     &   LCOMP = LQ(LCOMP)
      IF(.NOT.RECYCLE)THEN
        CALL FZOUT(MLUN, IXMAIN, LCOMP, 1, 'LP', 1, 0, 0)
C-
C- Check for not-enough-memory error return.  Expand buffer and retry up
C- to maximum memory size.
C-
        IF(IQUEST(1).EQ.-2 .AND. IQUEST(2).EQ.14)THEN
          IF(BSIZE.GE.MAX_BUFFER_SIZE)CALL ERRMSG('FZOUT failed',
     &      'COMPRESS_ZEBRA', 'Memory buffer at maximum', 'F')
          OK = LIB$FREE_VM(4*BSIZE, BUFAD)
          IF(.NOT.OK)CALL ERRMSG('LIB$FREE_VM failed', 'COMPRESS_ZEBRA',
     &      ' ', 'F')
          BSIZE = MIN0(2*BSIZE, MAX_BUFFER_SIZE)
          OK = LIB$GET_VM(4*BSIZE, BUFAD)
          IF(.NOT.OK)CALL ERRMSG('LIB$GET_VM failed', 'COMPRESS_ZEBRA', 
     &      ' ', 'F')
          CALL FZMEMO(MLUN, %VAL(BUFAD), BSIZE-2)
          GO TO 20
        ENDIF
C-
C- Check for other FZOUT errors.
C-
        IF(IQUEST(1).NE.0)CALL ERRMSG('FZOUT failed', 'COMPRESS_ZEBRA', 
     &    ' ', 'F')
C-
C- FZOUT was successful.  Get size of uncompressed memory data.
C-
        USIZE = IQUEST(9)
        INPUT_SIZE(IHOOK) = INPUT_SIZE(IHOOK) + USIZE
      ENDIF
C-
C- Drop the linear structure following the HEAD bank that was compressed.
C-
      CALL MZDROP(IXCOM, LCOMP, 'L')
C-
C- Assign maximum allowed size of compressed data.  Book an an empty ZDST
C- bank with this much space for data.
C-
      IF(.NOT.RECYCLE)THEN
        OFFSET = 7
        MAX_CSIZE = 2*USIZE
        CALL BKZDST(MAX_CSIZE+OFFSET, LZDST)
        IF(LZDST.EQ.0)CALL ERRMSG('BKZDST failed', 'COMPRESS_ZEBRA',
     &    ' ', 'F')
        IQ(LZDST+1) = VERSION
        IQ(LZDST+2) = 2**16*COMP_ALG + RECORD_SIZE
        IQ(LZDST+3) = USIZE
        IQ(LZDST+6) = OFFSET
C-
C- Copy/compress data to ZDST bank.
C-
        CALL COMPRESS_ZEBRA_EVENT(%VAL(BUFAD), USIZE, IQ(LZDST+OFFSET), 
     &    MAX_CSIZE, COMP_ALG, RANDOM, CSIZE, CHECKSUM)
        IQ(LZDST+4) = CSIZE
        IQ(LZDST+5) = CHECKSUM
C-
C- Adjust size of ZDST bank to minimum.
C-
        CALL MZPUSH(IXMAIN, LZDST, 0, CSIZE-MAX_CSIZE, 'I')
        OUTPUT_SIZE(IHOOK) = OUTPUT_SIZE(IHOOK) + CSIZE + OFFSET
      ELSE
C-
C- Here is where we shunt an old ZDST bank back under HEAD.
C-
        CALL ZSHUNT(IXCOM, LSLINK(RECYCLE_NUM), LHEAD, -IZZDST, 1)
        LZDST = GZZDST()
      ENDIF
C-
C- Shunt any banks linearly following HEAD (i.e. HSTR banks) to linearly
C- follow the ZDST bank.
C-
      IF(LQ(LHEAD).NE.0)THEN
        CALL ZSHUNT(IXCOM, LQ(LHEAD), LZDST, 0, 1)
      ENDIF
C-
C- Done.
C-
      GO TO 999
C 
      ENTRY COMPRESS_ZEBRA_INI()
C-
C- Initialization entry point
C-
      COMPRESS_ZEBRA_INI = .TRUE.
      IF(FIRST)THEN
        FIRST = .FALSE.
C-
C- Zero statistics
C-
        NUM_EVENT(IHOOK) = 0
        INPUT_SIZE(IHOOK) = 0
        OUTPUT_SIZE(IHOOK) = 0
C-
C- Read RCP parameters.  First read from COMPRESS_ZEBRA_RCP.
C-
        CALL EZPICK_NOMSG('COMPRESS_ZEBRA_RCP', IER)
        IF(IER.NE.0)THEN
          CALL INRCP('COMPRESS_ZEBRA_RCP', IER)
          CALL EZPICK_NOMSG('COMPRESS_ZEBRA_RCP', IER)
        ENDIF
        IF(IER.EQ.0)CALL EZGET('DO_COMPRESS_ZEBRA', DO_COMPRESS_ZEBRA, 
     &    IER)
        IF(IER.EQ.0)CALL EZGET('COMP_ALG', COMP_ALG, IER)
        IF(IER.EQ.0)CALL EZGET('RANDOM', RANDOM, IER)
        IF(IER.EQ.0)CALL EZGET('KEEP_HSTR', KEEP_HSTR, IER)
        IF(IER.EQ.0)CALL EZGET('RECYCLE_ZDST', RECYCLE_ZDST, IER)
        IF(IER.EQ.0)CALL EZGET('RECORD_SIZE', RECORD_SIZE, IER)
        IF(IER.EQ.0)CALL EZGET('MIN_BUFFER_SIZE', MIN_BUFFER_SIZE, IER)
        IF(IER.EQ.0)CALL EZGET('MAX_BUFFER_SIZE', MAX_BUFFER_SIZE, IER)
        IF(IER.EQ.0)CALL EZGET('STA_MODE', STA_MODE, IER)
        IF(IER.EQ.0)CALL EZGET('DST_MODE', DST_MODE, IER)
        IF(IER.EQ.0)CALL EZGET('ROUND_FLOATING', ROUND, IER)
        IF(IER.EQ.0)CALL EZGET('FLOATING_BITS', FLOATING_BITS, IER)
        IF(IER.EQ.0)CALL EZGET('ZIP_PACK_LEVEL', ZIP_PACK_LEVEL, IER)
        IF(IER.EQ.0)CALL EZGET('LZSS_INDEX_BITS', LZSS_INDEX_BITS, IER)
        IF(IER.EQ.0)CALL EZGET('LZSS_LENGTH_BITS', LZSS_LENGTH_BITS, 
     &    IER)
        IF(IER.EQ.0)CALL EZGET('LZW_DICTIONARY_BITS', 
     &    LZW_DICTIONARY_BITS, IER)
        IF(IER.EQ.0)CALL EZGET('UDST_DROP_RECO', UDST_DROP_RECO, IER)
        CALL EZRSET
        IF (IER.NE.0)CALL ERRMSG('Error in COMPRESS_ZEBRA_RCP',
     &    'COMPRESS_ZEBRA',' ','F')
C-
C- Allocate initial memory buffer and connect to Zebra I/O channel
C-
        IF(DO_COMPRESS_ZEBRA)THEN
          BSIZE = MAX0(MIN_BUFFER_SIZE, 128)
          OK = LIB$GET_VM(4*BSIZE, BUFAD)
          IF(.NOT.OK)CALL ERRMSG('Failed to allocate memory buffer',
     &      'COMPRESS_ZEBRA', ' ', 'F')
          CALL GTUNIT(999, MLUN, IER)
          IF(IER.NE.0)CALL ERRMSG('GTUNIT failed', 'COMPRESS_ZEBRA', 
     &      ' ', 'F')
          CALL FZFILE(MLUN, RECORD_SIZE, 'OM')
          CALL FZMEMO(MLUN, %VAL(BUFAD), BSIZE-2)
        ENDIF
C-
C- Allocate structure link if RECYCLE_ZDST is .TRUE.
C-
        IF(RECYCLE_ZDST)THEN
          CALL GSLINK('COMPRESS', RECYCLE_NUM)
          CALL UNCOMPRESS_ZEBRA_SET_LINK(RECYCLE_NUM)
        ELSE
          RECYCLE_NUM = 0
        ENDIF
C-
C- Algorithm parameters
C-
        IF(COMP_ALG.EQ.1)THEN
          CALL COMPRESS_LZSS_PARAM(LZSS_INDEX_BITS, LZSS_LENGTH_BITS)
        ELSEIF(COMP_ALG.EQ.2)THEN
          CALL COMPRESS_LZW_PARAM(LZW_DICTIONARY_BITS)
        ELSEIF(COMP_ALG.EQ.3)THEN
          CALL COMPRESS_ZIP_PARAM(ZIP_PACK_LEVEL)
        ENDIF
      ENDIF
      GO TO 999
C 
      ENTRY COMPRESS_ZEBRA_END()
C-
C- Job summary entry point
C-
      COMPRESS_ZEBRA_END = .TRUE.
      IF(.NOT.DO_COMPRESS_ZEBRA)GO TO 999
      DO IHOOK = 1,2
        IF(INPUT_SIZE(IHOOK).GT.0)THEN
          RATIO(IHOOK) = 
     &      FLOAT(OUTPUT_SIZE(IHOOK))/FLOAT(INPUT_SIZE(IHOOK))
        ELSE
          RATIO(IHOOK) = 0.
        ENDIF
      ENDDO
      LUN = SSUNIT()
      WRITE(LUN,500)(NUM_EVENT(IHOOK), INPUT_SIZE(IHOOK), 
     &  OUTPUT_SIZE(IHOOK), RATIO(IHOOK), IHOOK=1,2)
 500  FORMAT(/' COMPRESS_ZEBRA package statistics'/
     &  /1X,I8,' STA Events processed'
     &  /1X,I8,' STA Uncompressed size'
     &  /1X,I8,' STA Compressed size'
     &  /1X,F8.3,' STA Compression ratio'/
     &  /1X,I8,' DST Events processed'
     &  /1X,I8,' DST Uncompressed size'
     &  /1X,I8,' DST Compressed size'
     &  /1X,F8.3,' DST Compression ratio')
 999  RETURN
      END
