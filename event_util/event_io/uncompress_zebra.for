      SUBROUTINE UNCOMPRESS_ZEBRA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts a compressed Zebra structure (ZDST 
C-                         bank) back to a full Zebra structure.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Entry points:  UNCOMPRESS_ZEBRA_SET_LINK - Set link to save ZDST bank.
C-
C-   Created  28-Sep-1994   Herbert Greenlee
C-   Modified 24-Sep-1996   Scott Snyder   - Fix buffer overflow problem.
C-   Modified 16-Sep-1996   John Krane     - Change fatal errors to warnings
C-                                           for checksums and size
C-                                           set IQUEST flag also
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$LINKS:IZZDST.LINK'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
      INCLUDE 'D0$LINKS:IZMUD1.LINK'
      INCLUDE 'D0$LINKS:IZCDD1.LINK'
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
      INCLUDE 'D0$LINKS:IZCDD3.LINK'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INCLUDE 'D0$LINKS:IZFILT.LINK'
      INCLUDE 'D0$LINKS:IZRECO.LINK'
      INCLUDE 'D0$LINKS:IZANLS.LINK'
      INCLUDE 'D0$LINKS:IZBERD.LINK'
      INCLUDE 'D0$LINKS:IZHSUM.LINK'
      INCLUDE 'D0$LINKS:IZFAKE.LINK'
      INCLUDE 'D0$LINKS:IZGEAN.LINK'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$LINKS:IZISAB.LINK'
      INCLUDE 'D0$LINKS:IZUSER.LINK'
      INCLUDE 'D0$LINKS:IZWGHT.LINK'
      INCLUDE 'D0$LINKS:IZESUM.LINK'
      INCLUDE 'D0$LINKS:IZTSUM.LINK'
      INCLUDE 'D0$LINKS:IZJUTL.LINK'
      INCLUDE 'D0$LINKS:IZUDST.LINK'
      INCLUDE 'D0$LINKS:IZUCSH.LINK'
      INCLUDE 'D0$LINKS:IZUTAG.LINK'
      INCLUDE 'D0$LINKS:IZCAID.LINK'
      INCLUDE 'D0$LINKS:IZUINT.LINK'
      INCLUDE 'D0$LINKS:IZPROC.LINK'
      INCLUDE 'D0$LINKS:IZHITS.LINK'
      INCLUDE 'D0$LINKS:IZHSTR.LINK'
      INCLUDE 'D0$LINKS:IZKTCL.LINK'
C-
C- Variables from UNCOMPRESS_ZEBRA_RCP.
C-
C- Address of memory buffer and Zebra I/O stuff
C-
      INTEGER BUFAD              ! Current buffer addresses.
      INTEGER BSIZE              ! Current buffer sizes in longwords.
      INTEGER MIN_BSIZE
      LOGICAL OK, LIB$GET_VM, LIB$FREE_VM
      INTEGER MLUN
      INTEGER RECORD_SIZE
      INTEGER CSIZE, USIZE, USIZE1 ! Compressed/uncompressed event size.
C-
C- Compression parameters
C-
      INTEGER CHECKSUM, CHECKSUM1
      INTEGER OFFSET
      INTEGER COMP_ALG
      INTEGER NUM, RECYCLE_NUM
C-
C- Links, etc.
C-
      INTEGER LZDST, GZZDST
      INTEGER LANLS, LHSUM, LRECO
      INTEGER L
      INTEGER MAX_NUH
      PARAMETER(MAX_NUH = 100)
      INTEGER NUH, IUHEAD(MAX_NUH)
      LOGICAL FZFILE_CALLED
      INTEGER LZLAST
C-
C- Other variables and functions.
C-
      CHARACTER*4 CBANK
      INTEGER I
      INTEGER IER
      LOGICAL FIRST
      SAVE FIRST, RECYCLE_NUM
      DATA FIRST/.TRUE./
      DATA RECYCLE_NUM/0/       ! Can not be set in IF(FIRST) block.

      integer ihHEAD/4HHEAD/
      integer ihZDST/4HZDST/
      integer ihTRGR/4HTRGR/
      integer ihMUD1/4HMUD1/
      integer ihCDD1/4HCDD1/
      integer ihCDD2/4HCDD2/
      integer ihCDD3/4HCDD3/
      integer ihCDD4/4HCDD4/
      integer ihCAD1/4HCAD1/
      integer ihCAD2/4HCAD2/
      integer ihFILT/4HFILT/
      integer ihRECO/4HRECO/
      integer ihBERD/4HBERD/
      integer ihANLS/4HANLS/
      integer ihHSUM/4HHSUM/
      integer ihFAKE/4HFAKE/
      integer ihGEAN/4HGEAN/
      integer ihISAE/4HISAE/
      integer ihISAB/4HISAB/
      integer ihUSER/4HUSER/
      integer ihWGHT/4HWGHT/
      integer ihJUTL/4HJUTL/
      integer ihUDST/4HUDST/
      integer ihUCSH/4HUCSH/
      integer ihUTAG/4HUTAG/
      integer ihCAID/4HCAID/
      integer ihUINT/4HUINT/
      integer ihKTCL/4HKTCL/
      integer ihTSUM/4HTSUM/
      integer ihESUM/4HESUM/
      integer ihHSTR/4HHSTR/
      integer ihPROC/4HPROC/
      integer ihHITS/4HHITS/
      
C----------------------------------------------------------------------
C-
C- Initialization.
C-
      IF(FIRST)THEN
        FIRST = .FALSE.
C-
C- Allocate memory buffer.
C-
        BSIZE = 128
        OK = LIB$GET_VM(4*BSIZE, BUFAD)
        IF(.NOT.OK)CALL ERRMSG('Failed to allocate memory buffer',
     &    'UNCOMPRESS_ZEBRA', ' ', 'F')
C-
C- Set flag to call FZFILE once we are sure that we have a ZDST bank in 
C- /ZEBCOM/.
C-
        FZFILE_CALLED = .FALSE.
      ENDIF
C-
C- Do not attempt to uncompress non-event data.
C-
      IF(LHEAD.EQ.0)GO TO 999
      IF(IQ(LHEAD-4).NE.iHHEAD)GO TO 999
C-
C- Look for ZDST bank.
C-
      LZDST = GZZDST()
      IF(LZDST.EQ.0)GO TO 999
C-
C- Shunt any banks following ZDST (i.e. HSTR) to HEAD chain.
C-
      IF(LQ(LZDST).NE.0)THEN
        CALL ZSHUNT(IXCOM, LQ(LZDST), LHEAD, 0, 1)
      ENDIF
C-
C- Get event parameters.
C-
      COMP_ALG = IQ(LZDST+2)/2**16
      RECORD_SIZE = IQ(LZDST+2) - 2**16*COMP_ALG
      USIZE = IQ(LZDST+3)
      CSIZE = IQ(LZDST+4)
      CHECKSUM = IQ(LZDST+5)
      OFFSET = IQ(LZDST+6)
C-
C- Call FZFILE and connect unit to memory buffer if not done already.
C- This is not done during initialization routine because we need to
C- get the Zebra record size from the data.
C-
      IF(.NOT.FZFILE_CALLED)THEN
        FZFILE_CALLED = .TRUE.
        CALL GTUNIT(999, MLUN, IER)
        IF(IER.NE.0)CALL ERRMSG('GTUNIT failed', 'UNCOMPRESS_ZEBRA', 
     &    ' ', 'F')
        CALL FZFILE(MLUN, RECORD_SIZE, 'IM')
        CALL FZMEMO(MLUN, %VAL(BUFAD), BSIZE-2)
        LZDST = GZZDST()      ! Re-find LZDST bank (unprotected link).
      ENDIF
C-
C- Make sure that memory buffer is big enough to hold event with one physical
C- record to spare for padding (up to maximum buffer length).
C-
      MIN_BSIZE = USIZE+RECORD_SIZE
      IF(MIN_BSIZE.GT.BSIZE)THEN
        OK = LIB$FREE_VM(4*BSIZE, BUFAD)
        IF(.NOT.OK)CALL ERRMSG('LIB$FREE_VM failed', 'UNCOMPRESS_ZEBRA',
     &    ' ', 'F')
        BSIZE = MIN_BSIZE
        OK = LIB$GET_VM(4*BSIZE, BUFAD)
        IF(.NOT.OK)CALL ERRMSG('LIB$GET_VM failed', 'UNCOMPRESS_ZEBRA', 
     &    ' ', 'F')
        CALL FZMEMO(MLUN, %VAL(BUFAD), BSIZE-2)
        LZDST = GZZDST()      ! Re-find LZDST bank (unprotected link).
      ENDIF
C-
C- Copy/Uncompress data here.
C-
      CALL UNCOMPRESS_ZEBRA_EVENT(IQ(LZDST+OFFSET), CSIZE, %VAL(BUFAD),
     &  MIN_BSIZE, COMP_ALG, USIZE1, CHECKSUM1)
C-
C- Check consistency of uncompressed size and checksum as stored in bank and 
C- as determined locally.
C-
      IF(USIZE.NE.USIZE1)CALL ERRMSG('Uncompression error', 
     &  'UNCOMPRESS_ZEBRA', 'Wrong uncompressed size', 'W')
      IF(CHECKSUM.NE.CHECKSUM1)CALL ERRMSG('Uncompression error',
     &  'UNCOMPRESS_ZEBRA', 'Wrong uncompressed checksum', 'W')
C- Set DATA CORRUPT flag in IQUEST
      IF(USIZE.NE.USIZE1.OR.CHECKSUM.NE.CHECKSUM1) then
         IQUEST(1)=-3
         GOTO 999
      ENDIF
C-
C- If RECYCLE_NUM is non-zero, shunt the ZDST bank to the /ZLINKA/ 
C- link area instead of dropping it.
C-
      IF(RECYCLE_NUM.NE.0)THEN
        CALL ZSHUNT(IXCOM, LZDST, LSLINK(RECYCLE_NUM), 1, 1)
      ENDIF
C-
C- Drop banks under the HEAD bank.  This includes the ZDST bank that
C- contained the compressed information, and any banks added by the frame
C- since the event was read in.
C-
      CALL MZDROP(IXCOM, LHEAD, 'V')
C-
C- Read in event from memory to restore the original Zebra Structure.
C-
      NUH = 0
      CALL FZIN(MLUN, IXMAIN, LHEAD, 0, ' ', NUH, IUHEAD)
      IF(IQUEST(1).NE.0)CALL ERRMSG('FZIN failed', 'UNOMPRESS_ZEBRA', 
     &  ' ', 'F')
C-
C- Shunt all banks linearly following the HEAD bank to their original 
C- locations under the HEAD bank.
C-
      DO WHILE(LQ(LHEAD).NE.0)
        L = LZLAST(IXCOM, LHEAD)
        IF(IQ(L-4).EQ.iHZDST)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZZDST, 0)
        ELSEIF(IQ(L-4).EQ.iHTRGR)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZTRGR, 0)
        ELSEIF(IQ(L-4).EQ.iHMUD1)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZMUD1, 0)
        ELSEIF(IQ(L-4).EQ.iHCDD1)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZCDD1, 0)
        ELSEIF(IQ(L-4).EQ.iHCDD2)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZCDD2, 0)
        ELSEIF(IQ(L-4).EQ.iHCDD3)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZCDD3, 0)
        ELSEIF(IQ(L-4).EQ.iHCDD4)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZCDD4, 0)
        ELSEIF(IQ(L-4).EQ.iHCAD1)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZCAD1, 0)
        ELSEIF(IQ(L-4).EQ.iHCAD2)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZCAD2, 0)
        ELSEIF(IQ(L-4).EQ.iHFILT)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZFILT, 0)
        ELSEIF(IQ(L-4).EQ.iHRECO)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZRECO, 0)
        ELSEIF(IQ(L-4).EQ.iHBERD)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZBERD, 0)
        ELSEIF(IQ(L-4).EQ.iHANLS)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZANLS, 0)
        ELSEIF(IQ(L-4).EQ.iHHSUM)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZHSUM, 0)
        ELSEIF(IQ(L-4).EQ.iHFAKE)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZFAKE, 0)
        ELSEIF(IQ(L-4).EQ.iHGEAN)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZGEAN, 0)
        ELSEIF(IQ(L-4).EQ.iHISAE)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZISAE, 0)
        ELSEIF(IQ(L-4).EQ.iHISAB)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZISAB, 0)
        ELSEIF(IQ(L-4).EQ.iHUSER)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZUSER, 0)
        ELSEIF(IQ(L-4).EQ.iHWGHT)THEN
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZUSER, 0)
        ELSEIF(IQ(L-4).EQ.iHJUTL)THEN
          LANLS = LQ(LHEAD-IZANLS)
          IF(LANLS.EQ.0)CALL BKANLS(LANLS)
          L = LZLAST(IXCOM, LHEAD)
          CALL ZSHUNT(IXCOM, L, LANLS, -1, 0)
        ELSEIF(IQ(L-4).EQ.iHUDST)THEN
          LANLS = LQ(LHEAD-IZANLS)
          IF(LANLS.EQ.0)CALL BKANLS(LANLS)
          L = LZLAST(IXCOM, LHEAD)
          CALL ZSHUNT(IXCOM, L, LANLS, -IZUDST, 0)
        ELSEIF(IQ(L-4).EQ.iHUCSH)THEN
          LANLS = LQ(LHEAD-IZANLS)
          IF(LANLS.EQ.0)CALL BKANLS(LANLS)
          L = LZLAST(IXCOM, LHEAD)
          CALL ZSHUNT(IXCOM, L, LANLS, -IZUCSH, 0)
        ELSEIF(IQ(L-4).EQ.iHUTAG)THEN
          LANLS = LQ(LHEAD-IZANLS)
          IF(LANLS.EQ.0)CALL BKANLS(LANLS)
          L = LZLAST(IXCOM, LHEAD)
          CALL ZSHUNT(IXCOM, L, LANLS, -IZUTAG, 0)
        ELSEIF(IQ(L-4).EQ.iHCAID)THEN
          LANLS = LQ(LHEAD-IZANLS)
          IF(LANLS.EQ.0)CALL BKANLS(LANLS)
          L = LZLAST(IXCOM, LHEAD)
          CALL ZSHUNT(IXCOM, L, LANLS, -5, 0)
        ELSEIF(IQ(L-4).EQ.iHUINT)THEN
          LANLS = LQ(LHEAD-IZANLS)
          IF(LANLS.EQ.0)CALL BKANLS(LANLS)
          L = LZLAST(IXCOM, LHEAD)
          CALL ZSHUNT(IXCOM, L, LANLS, -IZUINT, 0)
        ELSEIF(IQ(L-4).EQ.iHKTCL)THEN
          LANLS = LQ(LHEAD-IZANLS)
          IF(LANLS.EQ.0)CALL BKANLS(LANLS)
          L = LZLAST(IXCOM, LHEAD)
          CALL ZSHUNT(IXCOM, L, LANLS, -IZKTCL, 0)
        ELSEIF(IQ(L-4).EQ.iHTSUM)THEN
          LHSUM = LQ(LHEAD-IZHSUM)
          IF(LHSUM.EQ.0)CALL BKHSUM(LHSUM)
          L = LZLAST(IXCOM, LHEAD)
          CALL ZSHUNT(IXCOM, L, LHSUM, -IZTSUM, 0)
        ELSEIF(IQ(L-4).EQ.iHESUM)THEN
          LHSUM = LQ(LHEAD-IZHSUM)
          IF(LHSUM.EQ.0)CALL BKHSUM(LHSUM)
          L = LZLAST(IXCOM, LHEAD)
          CALL ZSHUNT(IXCOM, L, LHSUM, -IZESUM, 0)
        ELSEIF(IQ(L-4).EQ.iHHSTR)THEN
          LRECO = LQ(LHEAD-IZRECO)
          IF(LRECO.EQ.0)CALL BKRECO(LRECO)
          L = LZLAST(IXCOM, LHEAD)
          CALL ZSHUNT(IXCOM, L, LRECO, -IZHSTR, 0)
        ELSEIF(IQ(L-4).EQ.iHPROC)THEN
          LRECO = LQ(LHEAD-IZRECO)
          IF(LRECO.EQ.0)CALL BKRECO(LRECO)
          L = LZLAST(IXCOM, LHEAD)
          CALL ZSHUNT(IXCOM, L, LRECO, -IZPROC, 0)
        ELSEIF(IQ(L-4).EQ.iHHITS)THEN
          LRECO = LQ(LHEAD-IZRECO)
          IF(LRECO.EQ.0)CALL BKRECO(LRECO)
          L = LZLAST(IXCOM, LHEAD)
          CALL ZSHUNT(IXCOM, L, LRECO, -IZHITS, 0)
        ELSE
          CALL UHTOC(IQ(L-4),4,CBANK,4)
          DO I = 1,4
            IF(CBANK(I:I).LT.' '.OR.CBANK(I:I).GT.'~')CBANK(I:I) = '.'
          ENDDO
          CALL ERRMSG('Unknown bank', 'UNCOMPRESS_ZEBRA', 
     +      'Unknown bank '//CBANK//', shunting to -IZZDST', 'W')
          CALL ZSHUNT(IXCOM, L, LHEAD, -IZZDST, 0)
        ENDIF
      ENDDO
      GO TO 999
C
      ENTRY UNCOMPRESS_ZEBRA_SET_LINK(NUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Entry point to set link to save ZDST bank.
C-
C-   Inputs  : NUM - Number of structural link in /ZLINKA/ link area.
C-   Outputs :
C-   Controls:
C-
C-   Created  28-Mar-1995   Herbert Greenlee
C-
C----------------------------------------------------------------------
      RECYCLE_NUM = NUM
 999  RETURN
      END
