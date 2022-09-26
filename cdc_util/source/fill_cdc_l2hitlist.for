      SUBROUTINE FILL_CDC_L2HITLIST(layer, sector, maxhits, nhits,
     &                              npulse, hitlist)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get CDC hits from compressed data (CDH2)
C-   for one layer, sector.
C-
C-   Inputs  : integer layer, sector
C-             integer maxhits = max hits allowed in sector
C-   Outputs : integer nhits = number of hits in this sector
C-             real hitlist(lpulse, maxhits) = array of hit info
C-             (lpulse is number of words/hit)
C-   Controls:
C-
C-   Created  27-APR-1993   Chris Klopfenstein
C-   Modified June 1994 CK. Bug fix. Layer and sector numbers
C-                          were being inadvertently modified here.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      include 'd0$inc:zebcom.inc'
      include 'd0$inc:cdpara.inc'
      integer layer, sector, nhits, maxhits, npulse(0:11)
      real hitlist(lpulse, maxhits)
C  map(layer, sector, wire, 1) = pointer to 1st hit on wire
C  map(layer, sector, wire, 2) = number of hits on wire
      integer map(0:3, 0:31, 0:10, 2)
      integer lcdh2, gzcdh2, length_cdh2
      integer point, channel_id
      integer begin_crate, end_crate
      integer nhits_crate, hit
      integer crate_length, header_length, trailer_length
      parameter (header_length = 4)
      parameter (trailer_length = 2)
      integer index
      integer wire, ubit
      integer n_layer, n_sector, n_wire
      parameter (n_layer = 4)
      parameter (n_sector = 32)
      parameter (n_wire = 11)
      integer cable_trailer_length
      parameter (cable_trailer_length = 0)
      integer event_number
      real time, area, width, peak
      integer status, label
      real fstatus, flabel
      equivalence (status, fstatus), (label, flabel)
      logical EZERROR
      integer err
      real tscale, tmax, armax, widmax, ffrqcy, trgoff
      integer iscale, imax, idummy
      real terrsw, terrdl
      integer n_hit_wire
      integer ilayer, isector
      logical first
      data event_number /-1/
      data first /.true./
C----------------------------------------------------------------------
      if (first) then
        first = .false.
        call EZPICK('DTRAKS_RCP')
        IF (EZERROR(ERR)) THEN
          CALL ERRMSG('L2CDHT','L2CDPULS',
     &      'Unable to find bank L2CDHT','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('TSCALE',ISCALE,ERR)
        tscale = float(iscale)
        CALL EZGET_i('TMAX',IMAX,ERR)
        tmax = float(imax)
        CALL EZGET_i('ARMAX',IMAX,ERR)
        armax = float(imax)
        CALL EZGET_i('WIDMAX',IMAX,ERR)
        widmax = float(imax)
        CALL EZGET('FFRQCY',FFRQCY,ERR)
        CALL EZGET('TRGOFF',trgoff,ERR)
        CALL EZGET('TERRDL',terrdl,ERR)
        CALL EZGET('TERRSW',terrsw,ERR)
        CALL EZRSET
      endif
C
      lcdh2 = gzcdh2()
      if (lcdh2 .le. 0) goto 999
C  Check whether this is a new event. If so build pointer map.
      if (IQ(lhead+9) .ne. event_number) then
        event_number = IQ(lhead+9)
        call VZERO_i(map, 2*n_layer*n_sector*n_wire)
        length_cdh2 = IQ(lcdh2 -1)
        point = lcdh2 + length_cdh2 - cable_trailer_length
        do while (point .gt. (lcdh2 + header_length))
          crate_length = IQ(point - trailer_length)
          point = point - crate_length
          nhits_crate = (crate_length - header_length
     &                  - trailer_length) / 2
          do hit = 0, nhits_crate - 1
            channel_id = IBITS(IQ(point + header_length + 2*hit), 0, 16)
            call DCODER(channel_id, ilayer, isector, wire, ubit, 1)
            if (ubit .ne. 0) then
              call ErrMsg('L2HITS','Count_L2CDC',
     &        'Unused channel found in L2HITS', 'W')
              goto 999
            endif
            if ((isector .lt. 0) .or. (isector .gt. 31) .or.
     &        (ilayer .lt. 0) .or. (ilayer .gt. 3)) then
              call ErrMsg('L2HITS','Count_L2CDC',
     &        'layer, sector out of bounds', 'W')
              goto 999
            endif
C check wire number - ignore special wire number (scintillator etc)
            if ((wire .ge. 0) .and. (wire .lt. n_wire)) then
C If this is the first hit on this channel, store pointer
              if (map(ilayer, isector, wire, 1) .eq. 0)
     &          map(ilayer, isector, wire, 1) =
     &          point + header_length + 2*hit
C  Increment count of hits
              map(ilayer, isector, wire, 2) =
     &          map(ilayer, isector, wire, 2) + 1
            endif
          enddo
        enddo
      endif
C
C  get hits for this layer, sector
C
      nhits = 0
      call VZERO(hitlist, lpulse*maxhits)
      do wire = 0, n_wire - 1
        n_hit_wire = map(layer, sector, wire, 2)
        npulse(wire) = n_hit_wire
        point = map(layer, sector, wire, 1)
        do hit = 1, n_hit_wire
          nhits = nhits + 1
          label = IBITS(IQ(point), 0, 16)
          status = IBITS(IQ(point), 16, 4)
          width = IBITS(IQ(point), 20, 4)
          peak = IBITS(IQ(point), 24, 8)
          time = IBITS(IQ(point + 1), 0, 18) / tscale
          area = IBITS(IQ(point + 1), 18, 14)
          hitlist(1, nhits) = flabel
          hitlist(2, nhits) = time - trgoff
          hitlist(3, nhits) = area
          hitlist(4, nhits) = 4. * width * 1000./FFRQCY
          hitlist(5, nhits) = peak
          if (wire .ge. nbsens) then
            hitlist(6, nhits) = terrdl
          else
            hitlist(6, nhits) = terrsw
          endif
          hitlist(7, nhits) = sqrt(area)      ! 'error' in pulse area
          hitlist(8, nhits) = fstatus
          point = point + 2
        enddo
      enddo
C
  999 RETURN
      END
