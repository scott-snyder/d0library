      SUBROUTINE RECDD1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Translate CDD1 bank from old to new format
C-                         including crate header info etc. A new CDD1
C-                         bank is created (as USER bank, hanging from
C-                         HEAD) then the old bank is dropped and the
C-                         new one substituted.
C-
C-   Inputs  : Old CDD1 bank (wrong format)
C-   Outputs : New CDD1 bank with crate header info.
C-
C-   Created  14-SEP-1991   C. Klopfenstein
C-                          (from Srini's BLCDD2 routine).
C-   Modified March 92 - get rid of reference to CD link area.
C-   Mod. June 93 - use standalone temporary bank for reformatted data.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'
      INCLUDE 'D0$links:izcdd1.link'
C
      INTEGER Ltmpr
C
      INTEGER ICRT, ICRD, ICHN, CRATE
      INTEGER LAYER, SECTOR, WIRE, END, UBIT
      INTEGER NWORDS, NWDAT, NBHITS, LHIT, NBADC
      INTEGER KPDCDA
      INTEGER IHIT, IFADC, IPTR, IADC(4)
      INTEGER IVER, ISKP
C
      INTEGER WRCONT,WRNHDR,WRSYNC,WRVERS,WRDCNT
      INTEGER CWORD(0:3),MASK
      INTEGER J,WORD(2)
C      DATA MASK /'FFFF'x/
C      parameter (mask = 'FFFF'x)
      PARAMETER (MASK = 65535)
C
      INTEGER INDEX, ADDRESS, EXTRA
      INTEGER N_PREVIOUS
      INTEGER SECTOR_OFF, LAYER_OFF, DET_OFF, IDET
C      parameter (sector_off = '10'x)
      PARAMETER (SECTOR_OFF = 16)
C      parameter (layer_off = '200'x)
      PARAMETER (LAYER_OFF = 512)
C      parameter (det_off = '2000'x)
      PARAMETER (DET_OFF = 8192)
      PARAMETER (IDET = 0)  ! VTX:0, CDC:1, FDC:2, TRD:3
      INTEGER MAX_LENGTH
      PARAMETER (MAX_LENGTH = 512)
      INTEGER CHANNEL, FADC_DATA(MAX_LENGTH), BUFFER(MAX_LENGTH)
      INTEGER CHANNEL_LENGTH, POINT, CHNL_OFF
C      parameter (chnl_off = '10000'x)
      PARAMETER (CHNL_OFF = 65536)
      INTEGER ndata
C
      REAL TIME(64), AREA(64)
      DATA IVER /4/
      DATA WRNHDR /3/
C----------------------------------------------------------------------
C  check for old bank
      LCDD1 = LQ(LHEAD - IZCDD1)
      IF (LCDD1.LE.0) THEN
C        call intmsg(' CDD1 bank missing ')
        CALL ERRMSG('L2_CD_MATCH','RECDD1',' CDD1 bank missing ', 'W')
        RETURN
      ENDIF
C  book TMPR bank to use as buffer for new CDD1 data
      ndata = iq(lcdd1-1)
C  add extra words for extra crate headers/trailers, etc
C    (9 extra crates*8 words/crate + 16 datacable trailer words
C     - 2 words difference between MC crate header and real data header)
C   ===> 9*8 + 16 - 2 = 86
      ndata = ndata + 86
C      call BkTmpr(idet+1, ndata, ltmpr)  ! For now use Chris' old BKTMPR
      call BkTmpr(idet+1, ltmpr)
      point = 1
      if (ltmpr .le. 0)
     &  call ERRMSG('L2_CD_MATCH','RECDD1',' BKTMPR failed', 'W')
C
      ISKP = 0                          ! First Crate header
      WRDCNT = 0
      N_PREVIOUS = 0
      DO 100 ICRT = 0,9
        CRATE = ICRT*10 + 3
C
C *** Calculate header values
C
        CWORD(0) = CRATE
        CWORD(1) = 15
        CWORD(2) = 0
        CWORD(3) = 0
        CALL MVBITS(MASK,0,16,WRSYNC,0)
        CALL MVBITS(IQ(LHEAD+9),0,16,WRSYNC,16)
        CALL MVBITS(IVER,0,29,WRVERS,0)
        CALL MVBITS(1,0,3,WRVERS,29)
        CALL MVBITS(CWORD(0),0,8,WRCONT,24)
        CALL MVBITS(CWORD(1),0,8,WRCONT,16)
        CALL MVBITS(CWORD(2),0,8,WRCONT,8)
        CALL MVBITS(CWORD(3),0,8,WRCONT,0)
C
C *** Insert Crate Header block for each crate
C
        IQ(LTMPR + ISKP + 1) = WRNHDR
        IQ(LTMPR + ISKP + 2) = WRSYNC
        IQ(LTMPR + ISKP + 3) = WRCONT
        IQ(LTMPR + ISKP + 4) = WRVERS
C
        POINT = POINT + 4
        NWDAT = ISKP + 4
C
        DO 200 ICRD = 0,15
          DO 300 ICHN = 0,15
            CALL VFADCMAP(ICRT, ICRD, ICHN, ADDRESS)
C
C ****  Skip channels unimplemented in hardware
C
            IF ( ADDRESS .EQ. -1 ) GO TO 300
C  unpack raw data into an array
            CALL ZGEXPD(IDET + 1, ADDRESS, FADC_DATA, CHANNEL_LENGTH)
C  write into CDD1 bank
C check if still in bounds - extend bank if necessary
            IF ((POINT + CHANNEL_LENGTH/4) .GE. IQ(LTMPR-1)) THEN
              EXTRA = MAX(CHANNEL_LENGTH/4, 1000)
              CALL MZPUSH(IXCOM, LTMPR, 0, EXTRA, 'R')
            ENDIF
C
            DO INDEX = POINT, POINT + CHANNEL_LENGTH/4 - 1
              IQ(LTMPR + INDEX) = FADC_DATA(INDEX - POINT + 1)
            ENDDO
            POINT = POINT + CHANNEL_LENGTH/4
            NWDAT = NWDAT + CHANNEL_LENGTH/4
  300     CONTINUE
  200   CONTINUE
C
C *** Insert Trailer for every Crate data block
C
        IF ( (IQ(LTMPR-1)-NWDAT) .LE. 10 )
     &    CALL MZPUSH(IXCOM, LTMPR, 0, 100, 'R')
        ISKP = NWDAT
C
        WRDCNT = NWDAT + 4 - N_PREVIOUS
        N_PREVIOUS = N_PREVIOUS + WRDCNT
C
C  pack crate id and trigger number into WRSYNC -
C  crate id in low 2 bytes, trig num in high 2 bytes
C
        CALL MVBITS(CWORD(0),0,16,WRSYNC,0)
        CALL MVBITS(IQ(LHEAD+9),0,16,WRSYNC,16)
        IQ(LTMPR + ISKP + 1) = WRDCNT
        IQ(LTMPR + ISKP + 2) = WRSYNC
        IQ(LTMPR + ISKP + 3) = MASK
        IQ(LTMPR + ISKP + 4) = 0
        ISKP = NWDAT + 4
        POINT = POINT + 4
C
  100 CONTINUE
C
C *** Release empty space in bank CDD1. It may be necessary to extend bank
C *** to fit additional words.
C
      NWORDS = NWDAT + 4 + 16 - IQ(LTMPR-1)
      CALL MZPUSH(IXCOM,LTMPR,0,NWORDS,'R')
C
C  Drop old CDD1 bank and replace it with the USER bank
      CALL MZDROP(IXMAIN, LCDD1, ' ')
      CALL ZSHUNT(IXMAIN, LTMPR, LHEAD, -IZCDD1, 0)
C  fix link area
C      lcddn(3) = lq(lhead-izcdd1)
C
  999 RETURN
      END
