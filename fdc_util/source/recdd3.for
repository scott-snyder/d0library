      SUBROUTINE RECDD3
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Translate CDD3 bank from old to new format
C-                         including crate header info etc. A new CDD3
C-                         bank is created (as USER bank, hanging from
C-                         HEAD) then the old bank is dropped and the
C-                         new one substituted.
C-
C-   Inputs  : Old CDD3 bank (wrong format)
C-   Outputs : New CDD3 bank with crate header info.
C-
C-   Created  14-SEP-1991   C. Klopfenstein
C-                          (from Srini's BLCDD3 routine).
C-   Modified March 92 - get rid of reference to CD link area.
C-   Mod. June 93 - use standalone temporary bank for reformatted data.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:FDCLNK.INC'
      include 'D0$links:izcdd3.link'
C
      integer ltmpr
C
      INTEGER ICRT, ICRD, ICHN, crate
      integer half, unit, quad, sector, wire, ubit
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
      parameter (mask = 65535)
C
      integer index, address, extra
      integer n_previous
      integer sector_off, layer_off, det_off, idet
C      parameter (sector_off = '10'x)
      parameter (sector_off = 16)
C      parameter (layer_off = '200'x)
      parameter (layer_off = 512)
C      parameter (det_off = '2000'x)
      parameter (det_off = 8192)
      parameter (idet = 2)  ! VTX:0, CDC:1, FDC:2, TRD:3
      integer max_length
      parameter (max_length = 512)
      integer channel, fadc_data(max_length), buffer(max_length)
      integer channel_length, point, chnl_off
C      parameter (chnl_off = '10000'x)
      parameter (chnl_off = 65536)
C
      REAL TIME(64), AREA(64)
      DATA IVER /4/
      DATA WRNHDR /3/
C----------------------------------------------------------------------
C  check for old bank
      LCDD3 = LQ(LHEAD - izcdd3)
      IF (LCDD3.LE.0) then
C        call intmsg(' CDD3 bank missing ')
        call ERRMSG('L2_CD_MATCH','RECDD1',' CDD1 bank missing ', 'W')
        return
      endif
C  book TMPR bank to use as buffer for new CDD3 data
      call BkTmpr(idet+1, ltmpr)
      point = 1
      if (ltmpr .le. 0)
     &  call ERRMSG('L2_CD_MATCH','RECDD3',' BKTMPR failed', 'W')
C
      ISKP = 0                          ! First Crate header
      WRDCNT = 0
      n_previous = 0
      DO 100 ICRT = 0,11
        crate = ICRT*10 + 5
C
C *** Calculate header values
C
        CWORD(0) = crate
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
        point = point + 4
        NWDAT = ISKP + 4
C
        DO 200 ICRD = 0,15
          DO 300 ICHN = 0,15
            call fdecode_elect(crate, icrd, ichn,
     &        half, unit, quad, sector, wire, ubit)
            if ( half .eq. -1 ) go to 300   ! unfilled slot in fadc crate
C  get FADC data from old CDD3 bank - lookup using layer/sector/wire number
            call FCODER(address, half, unit, quad, sector,
     &                  wire, ubit, 2)
C
            IF (ubit.NE.0) THEN         ! No Hits for unused channel
              NBHITS = 0
              LHIT = 0
              channel_length = 4
C              fadc_data(1) = address * chnl_off + channel_length
              fadc_data(1) = ISHFT(address, 16) + channel_length
C              fadc_data(1) = channel_length
            else
C  unpack raw data into an array
              call ZGEXPD(idet + 1, address, fadc_data, channel_length)
            endif
C  write into CDD3 bank
  500       CONTINUE
C check if still in bounds - extend bank if necessary
            if ((point + channel_length/4) .ge. iq(ltmpr-1)) then
              extra = max(channel_length/4, 1000)
              call MZPUSH(ixcom, ltmpr, 0, extra, 'R')
            endif
C
            do index = point, point + channel_length/4 - 1
              IQ(ltmpr + index) = fadc_data(index - point + 1)
            enddo
            point = point + channel_length/4
            nwdat = nwdat + channel_length/4
  400       CONTINUE
  300     CONTINUE
  200   CONTINUE
C
C *** Insert Trailer for every Crate data block
C
        IF ( (IQ(LTMPR-1)-NWDAT) .LE. 10 )
     &    CALL MZPUSH(ixcom, LTMPR, 0, 100, 'R')
        ISKP = NWDAT
C wrong
C        WRDCNT = NWDAT + 4 - WRDCNT
C
        wrdcnt = NWDAT + 4 - n_previous
        n_previous = n_previous + wrdcnt
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
        point = point + 4
C
  100 CONTINUE
C
C *** Release empty space in bank CDD3. It may be necessary to extend bank
C *** to fit additional words.
C
      NWORDS = NWDAT + 4 + 16 - IQ(LTMPR-1)
      CALL MZPUSH(ixcom,LTMPR,0,NWORDS,'R')
C
C  Drop old CDD3 bank and replace it with the USER bank
      call MZDROP(ixmain, lcdd3, ' ')
      call ZSHUNT(ixmain, ltmpr, lhead, -izcdd3, 0)
C  fix link area
C      lcddn(3) = lq(lhead-izcdd3)
C
  999 RETURN
      END
