      SUBROUTINE L2FDC(ok)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find FDC hits in level-2. The raw
C-   FADC data bank CDD3 is dropped, and replaced by hits.
C-   The output bank is called CDH3.
C-
C-   Inputs  : Pulse-finding parameters from L2CDHT_RCP bank,
C-             raw data from CDD3 bank.
C-   Outputs : logical ok = .true. if ok
C-             FDC hit data in (new) CDH3 bank.
C-   Controls: none.
C-
C-   Created   9-DEC-1992   Chris Klopfenstein
C-   Modified 27 April 93 CK - implement new bank structure. Results 
C-   now hang from FRES bank in FILT tree.
C_
C-   Mod. 18-may-93 CK - added return variable OK, reports status
C-   of hit-finding to caller. Added protection against running out
C-   of memory, and against missing STP banks.
C-
C-   FDC version cloned from L2CDC 25-May-93  - CK
C----------------------------------------------------------------------
      implicit none
      include 'D0$inc:Zebcom.inc'
      integer iquest
      common /quest/ iquest(100)
      logical ok
      logical EZError
      integer lcdd3, lcdh3
      integer ier, nwords, extra
      integer need, nwds_used, nwds_max
      integer point_cdd3, point_cdh3, point_next, offset
      integer icrate, crate
      integer length_cdd3, length_cdh3, crate_length, begin_crate
      integer crate_headlen, header_sync, controller, version
      integer crate_word_count, trailer_sync, token_pass, checksum
      integer ncrate, len_cable_trailer, max_depth
      integer crate_id, trignum
      integer channel_id, channel_length
      integer hit, nhit, maxhits
      parameter (maxhits = 32)
      integer status(maxhits), time(maxhits)
      integer area(maxhits), peak(maxhits)
C
      integer expdat(512)
      integer GZCDD3, GZSFDC, GZFPDH, GZL2FPDH, lfpdh
      logical first, inL2
      data first /.true./
C----------------------------------------------------------------------
      ok = .false.
      if (first) then
        first = .false.
        call EZPick('L2CDHT_RCP')
        if (EZError(ier)) then
          call ErrMsg('L2CDHT', 'L2FDC',
     &      'Unable to find bank L2CDHT_RCP', 'W')
          goto 999
        endif
        call EZGET('INL2', INL2, ier)
        call EZGET_i('NCRATE_FDC', ncrate, ier)
        if (ier .ne. 0) ncrate = 12
        call EZGET_i('LEN_CABLE_TRAILER', len_cable_trailer, ier)
        if (ier .ne. 0) len_cable_trailer = 16
        call EZGET_i('MAX_DEPTH', max_depth, ier)
        if (ier .ne. 0) max_depth = 512
C
C               .
C               .
C               .
C
        call EZRSET
      endif
C  check for FDC constants header, quit if missing
C      if (gzsfdc() .le. 0) goto 999
C  check for pedestal bank instead - need to know whether in L2 or offline here
      if (inL2) then
        lfpdh = gzl2fpdh()
      else
        lfpdh = gzfpdh()
      endif
      if (lfpdh .le. 0) goto 999
C  check for CDD3
      lcdd3 = GZCDD3()
      if (lcdd3 .le. 0) goto 999
C  book result bank CDH3
      call BKCDH3(lcdh3)
C
C  initialize pointers to position within CDD3, CDH3 banks
C
      length_cdd3 = IQ(lcdd3 - 1)
      point_cdd3 = lcdd3 + length_cdd3 - len_cable_trailer
      length_cdh3 = 0
      point_cdh3 = lcdh3 + 1
C
C  loop over crates
C
      do icrate = 0, ncrate - 1
C
C  read crate trailer and header words
C
        checksum = IQ(point_cdd3)
        token_pass = IQ(point_cdd3 - 1)
        trailer_sync = IQ(point_cdd3 - 2)
        crate_word_count = IQ(point_cdd3 - 3)
C
        crate_id = IBITS(trailer_sync, 0, 16)
        trignum = IBITS(trailer_sync, 16, 16)
C
        point_next = point_cdd3 - crate_word_count
        crate_headlen = IQ(point_next + 1)
        header_sync = IQ(point_next + 2)
        controller = IQ(point_next + 3)
        version = IQ(point_next + 4)
C
        point_cdd3 = point_cdd3 - 4
C
C  check trailer and header for consistency
C
        if ((IBITS(controller, 24, 8) .ne. crate_id) .or.
     &      (IBITS(header_sync, 16, 16) .ne. trignum)) then
          call ErrMsg('L2HITS', 'L2FDC',
     &      'crate header/trailer discrepancy', 'W')
          goto 666
        endif
C
C  Check if CDH3 bank big enough, extend if necessary. Assume that
C  2*nhits < no. words in raw data. Also assume that bank was 
C  booked large enough that extension should only rarely be needed.
C
        if ((point_cdh3 - lcdh3 + crate_word_count) .ge.
     &      IQ(lcdh3-1) ) then
          offset = point_cdh3 - lcdh3
          extra = max(crate_word_count, 4096)
C  check to see if we have enough space before doing the push.
C  quit and report bad if not enough room.
          need = 1
          call MZNEED(ixmain, need, 'G')
          nwds_used = IQUEST(12)
          nwds_max = IQUEST(13)
          if ((nwds_max - nwds_used) .lt. extra) goto 999
C  expand if needed
          call MZPUSH(ixcom, lcdh3, 0, extra, 'R')
          point_cdh3 = lcdh3 + offset
        endif
C
C  set bit in 'controller word' and copy crate header words to
C  CDH3 bank
C
        begin_crate = point_cdh3
        IQ(point_cdh3) = crate_headlen
        IQ(point_cdh3 + 1) = header_sync
        IQ(point_cdh3 + 2) = IBSET(controller, 5)
        IQ(point_cdh3 + 3) = version
        point_cdh3 = point_cdh3 + 4
C
C  find hits
C
        do while (point_cdd3 .gt. 
     &            point_next + crate_headlen + 1)
          channel_id = IBITS(IQ(point_cdd3), 16, 16)
          channel_length = IBITS(IQ(point_cdd3), 0, 16) / 4
          if (channel_length .gt. 1) then
            call L2CDUnpk(point_cdd3, max_depth, expdat, ier)
            call L2FDPuls(channel_id, max_depth, expdat, 
     &        maxhits, nhit, time, area, peak, status)
C
            do hit = 1, nhit
              IQ(point_cdh3) = channel_id +
     &          ISHFT(status(hit), 16) + ISHFT(peak(hit), 24)
              IQ(point_cdh3+1) = time(hit) +
     &          ISHFT(area(hit), 18)
              point_cdh3 = point_cdh3 + 2
            enddo
          endif
          point_cdd3 = point_cdd3 - channel_length
        enddo
C
C  update crate word count and copy crate trailer
C
        crate_length = point_cdh3 - begin_crate + 2
        IQ(point_cdh3) = crate_length
        IQ(point_cdh3 + 1) = crate_id + ISHFT(trignum, 16)
C
C  update pointers
C
        point_cdh3 = point_cdh3 + 2
        point_cdd3 = point_next
  666 enddo
C
C  release empty space in CDH3 bank
C
      nwords = point_cdh3 - lcdh3 - IQ(lcdh3 - 1)
      call MZPUSH(ixcom, lcdh3, 0, nwords, 'R')
C
C  here if all ok (failures jump to 999) so set logical OK.
C
      ok = .true.
  999 RETURN
      END
