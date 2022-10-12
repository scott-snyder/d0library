      SUBROUTINE L2CDC(ok)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find CDC hits in level-2. The raw
C-   FADC data bank CDD2 is dropped, and replaced by hits.
C-   The output bank is also called CDD2.
C-
C-   Inputs  : Pulse-finding parameters from L2CDC_RCP bank,
C-             raw data from CDD2 bank.
C-   Outputs : logical ok = .true. if ok
C-             CDC hit data in (new) CDH2 bank.
C-   Controls: none.
C-
C-   Created   9-DEC-1992   Chris Klopfenstein
C-   Modified 27 April 93 CK - implement new bank structure. Results 
C-   now hang from FRES bank in FILT tree.
C_
C-   Mod. 18-may-93 CK - added return variable OK, reports status
C-   of hit-finding to caller. Added protection against running out
C-   of memory, and against missing STP banks.
C
C  modified to use c version of l2cdpuls (different arguments),
C  and to get pedestal here (rather than in l2cdpuls).
C  sept. 93
C
C  modified to protect against corrupt data.
C  feb. 94
C----------------------------------------------------------------------
      implicit none
      include 'D0$inc:Zebcom.inc'
      include 'D0$inc:ZebStp.inc'
      integer iquest
      common /quest/ iquest(100)
      logical ok
      logical EZError
      integer lcdd2, lcdh2
      integer ier, nwords, extra
      integer need, nwds_used, nwds_max
      integer point_cdd2, point_cdh2, point_next, offset
      integer icrate, crate
      integer length_cdd2, length_cdh2, crate_length, begin_crate
      integer crate_headlen, header_sync, controller, version
      integer crate_word_count, trailer_sync, token_pass, checksum
      integer ncrate, len_cable_trailer, max_depth
      integer crate_id, trignum
      integer channel_id, channel_length
      integer hit, nhit, maxhits
      parameter (maxhits = 32)
C      integer status(maxhits), time(maxhits)
C      integer area(maxhits), peak(maxhits)
C  copy what DSECHT does... i.e. fix max hits per sector
      integer mxhtot
      parameter(mxhtot = 500)
      integer layer, sector, wire, maxhit_sector(0:3, 0:31)
      integer status(mxhtot), time(mxhtot)
      integer area(mxhtot), peak(mxhtot)
      integer error
C
      integer expdat(512)
      integer GZCDD2, GZSCDC, GZDPDH, GZL2DPDH
      integer lcdcped
      integer iped, jdpdl
C
      logical first, inL2
      data first /.true./
C----------------------------------------------------------------------
      ok = .false.
      if (first) then
        first = .false.
        call EZPick('L2CDHT_RCP')
C  temp fix
C        call EZPick('ZTRAKS_RCP')
        if (EZError(ier)) then
          call ErrMsg('L2CDHT', 'L2CDC',
     &      'Unable to find bank L2CDHT_RCP', 'W')
          goto 999
        endif
        call EZGET('INL2', INL2, ier)
        call EZGET_i('NCRATE', ncrate, ier)
        if (ier .ne. 0) ncrate = 6
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
C  check for CDC constants header, quit if missing
C      if (gzscdc() .le. 0) goto 999
C  check for pedestal bank instead - need to know whether in L2 or offline here
      if (inL2) then
        lcdcped = gzl2dpdh()
      else
        lcdcped = gzdpdh()
      endif
      if (lcdcped .le. 0) goto 999
C  check for CDD2
      lcdd2 = GZCDD2()
      if (lcdd2 .le. 0) goto 999
C  book result bank CDH2
      call BKCDH2(lcdh2)
C
C  init. max hits/sector array
C
      do layer = 0, 3
        do sector = 0, 31
          maxhit_sector(layer, sector) = mxhtot
        enddo
      enddo
C
C  initialize pointers to position within CDD2, CDH2 banks
C
      length_cdd2 = IQ(lcdd2 - 1)
      point_cdd2 = lcdd2 + length_cdd2 - len_cable_trailer
      length_cdh2 = 0
      point_cdh2 = lcdh2 + 1
C
C  loop over crates
C
      do icrate = 0, ncrate - 1
C
C  read crate trailer and header words
C
        checksum = IQ(point_cdd2)
        token_pass = IQ(point_cdd2 - 1)
        trailer_sync = IQ(point_cdd2 - 2)
        crate_word_count = IQ(point_cdd2 - 3)
C
        crate_id = IBITS(trailer_sync, 0, 16)
        trignum = IBITS(trailer_sync, 16, 16)
C
        point_next = point_cdd2 - crate_word_count
        crate_headlen = IQ(point_next + 1)
        header_sync = IQ(point_next + 2)
        controller = IQ(point_next + 3)
        version = IQ(point_next + 4)
C
        point_cdd2 = point_cdd2 - 4
C
C  check trailer and header for consistency
C
        if ((IBITS(controller, 24, 8) .ne. crate_id) .or.
     &      (IBITS(header_sync, 16, 16) .ne. trignum)) then
          call ErrMsg('L2HITS', 'L2CDC',
     &      'crate header/trailer discrepancy', 'W')
          goto 666
        endif
C
C  Check if CDH2 bank big enough, extend if necessary. Assume that
C  2*nhits < no. words in raw data. Also assume that bank was 
C  booked large enough that extension should only rarely be needed.
C
        if ((point_cdh2 - lcdh2 + crate_word_count) .ge.
     &      IQ(lcdh2-1) ) then
          offset = point_cdh2 - lcdh2
          extra = max(crate_word_count, 4096)
C  check to see if we have enough space before doing the push.
C  quit and report bad if not enough room.
          need = 1
          call MZNEED(ixmain, need, 'G')
          nwds_used = IQUEST(12)
          nwds_max = IQUEST(13)
          if ((nwds_max - nwds_used) .lt. extra) goto 999
C  expand if needed
          call MZPUSH(ixcom, lcdh2, 0, extra, 'R')
          point_cdh2 = lcdh2 + offset
        endif
C
C  set bit in 'controller word' and copy crate header words to
C  CDH2 bank
C
        begin_crate = point_cdh2
        IQ(point_cdh2) = crate_headlen
        IQ(point_cdh2 + 1) = header_sync
        IQ(point_cdh2 + 2) = IBSET(controller, 5)
        IQ(point_cdh2 + 3) = version
        point_cdh2 = point_cdh2 + 4
C
C  find hits
C
        do while (point_cdd2 .gt. 
     &            point_next + crate_headlen + 1)
          channel_id = IBITS(IQ(point_cdd2), 16, 16)
          channel_length = IBITS(IQ(point_cdd2), 0, 16) / 4
C  check for valid channel length
          if ((channel_length .le. 0) .or.
     &        (channel_length .gt. MAX_DEPTH/4)) then
            call ErrMsg('L2Hits', 'L2CDC',
     &        'Channel length out of range', 'W')
            goto 999
          endif
C
          if (channel_length .gt. 1) then
C  set max hits the DSECHT way
            layer = IBITS(channel_id, 9, 2)
            sector = IBITS(channel_id, 4, 5)
            wire = IBITS(channel_id, 0, 4)
C  get pedestal
            jdpdl = LC(lcdcped - (layer + 1))
            jdpdl = jdpdl + 4 +
     &              (sector * IC(jdpdl + 4) + wire) *
     &              IC(jdpdl + 3)
            iped = NINT(C(jdpdl + 1))
            call L2CDPuls(iq, point_cdd2, iped,
     &        channel_id,
     &        maxhit_sector(layer, sector), nhit, time, 
     &        area, peak, status, error)
C quit if L2CDPULS reports an error
            if (error .ne. 0) then
              call ErrMsg('L2CDHT', 'L2CDC',
     &                    'FADC error, skip event', 'W')
              goto 999
            endif
            maxhit_sector(layer, sector) = 
     &                 maxhit_sector(layer, sector) - nhit
C
            do hit = 1, nhit
              IQ(point_cdh2) = channel_id +
     &          ISHFT(status(hit), 16) + ISHFT(peak(hit), 24)
              IQ(point_cdh2+1) = time(hit) +
     &          ISHFT(area(hit), 18)
              point_cdh2 = point_cdh2 + 2
            enddo
          endif
          point_cdd2 = point_cdd2 - channel_length
        enddo
C
C  update crate word count and copy crate trailer
C
        crate_length = point_cdh2 - begin_crate + 2
        IQ(point_cdh2) = crate_length
        IQ(point_cdh2 + 1) = crate_id + ISHFT(trignum, 16)
C
C  update pointers
C
        point_cdh2 = point_cdh2 + 2
        point_cdd2 = point_next
  666 enddo
C
C  release empty space in CDH2 bank
C
      nwords = point_cdh2 - lcdh2 - IQ(lcdh2 - 1)
      call MZPUSH(ixcom, lcdh2, 0, nwords, 'R')
C
C  drop raw data bank and replace it with new CDD2 bank
C
C      call MZDROP(ixmain, lcdd2, ' ')
C      call ZSHUNT(ixmain, lcdh2, lhead, -izcdd2, 0)
C
C  here if all ok (failures jump to 999) so set logical OK.
C
      ok = .true.
  999 RETURN
      END
