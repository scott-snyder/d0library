      logical function fzdiff_comp(l1, l2, chopt)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Detailed comparison of two zebra banks.
C-
C-   Returned value  : .TRUE.  - Comparison succeeded (banks agree).
C-                     .FALSE. - Comparison failed.
C-   Inputs  : l1 - Link of first bank.
C-             l2 - Link of second bank.
C-   Controls: chopt - Character options:
C-                     Q - Quiet.  Do not print error message if banks
C-                         diagree.  Default is to print error message.
C-                     V - Verbose.  Print words of banks.
C-
C-   Created   10-Sep-1992   Herbert Greenlee
C-   Updated   17-NOV-1994   Ulrich Heintz  add call to UDST_TAG 
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:zebcom.inc'
      include 'd0$inc:quest.inc'
      integer i, j, k, id
      integer l1, l2              ! Links to banks
      character*(*) chopt         ! Character options
      character*1 ch
      logical quiet, verbose, mismatch
      integer ls
      character*80 bkname(2)      ! Full pathnames
      integer lbkname(2)          ! Length of pathmanes
      character*80 chform(2)      ! Bank formats
      integer nc(2), ncw, lc
      character*1 type
      character*4 bank
      character*20 tol_param      ! RCP parameter
      integer ier                 ! Error flag
      logical detail              ! Detail comparison flag (RCP parameter)
      integer max_error           ! Maximum number of error messages (RCP)
      integer num_error           ! Number of data error messages
      integer ns, nl, nd          ! Bank size information
      character*8 tag,udst_tag    ! tag name of word in UDST bank
      integer jer                 ! Error flag
C-
C- Temporary variables used during comparison
C-
      real a(2), tol, rel, zero
      real dbla(2,2)
      double precision da(2), drel, dzero
      real dtol
      equivalence (dbla(1,1), da(1))
      integer hol(2), lout(2)
      logical printing
      character*4 chol(2)
      character*16 chout(2)
      equivalence (hol(1),chol(1))
C-
C- Functions
C-
      integer fzdiff_precomp
      logical ezerr
      logical first/.true./
C----------------------------------------------------------------------
      if(first)then
C-
C- Get static RCP parameters
C-
        first = .false.
        call ezpick('FZDIFF_RCP')
        if(ezerr(ier))then
          call errmsg('fzdiff','fzdiff_comp',
     &      'Unable to pick SRCP bank FZDIFF_RCP','F')
        endif
        call ezget('COMPARE_DATA', detail, ier)
        if(ier.eq.0)call ezget('MAX_DATA_ERROR', max_error, ier)
        if(ier.eq.0)then
          call ezget('ZERO_EQUIV', zero, ier)
          if(ier.ne.0)then
            ier = 0
            zero = 1.e-20
          endif
        endif
        dzero = zero
        if(ier.ne.0)then
          call errmsg('fzdiff', 'fzdiff_comp',
     &      'Error fetching RCP parameters', 'F')
        endif
      endif
C-
C- Scan options
C-
      quiet = .false.
      verbose = .false.
      do 10 i=1,len(chopt)
        call upcase(chopt(i:i), ch)
        if(ch.eq.'Q')quiet = .true.
        if(ch.eq.'V')verbose = .true.
 10   continue
      verbose = verbose.and..not.quiet
C-
C- Print full pathname(s)
C-
      if(.not.quiet)then
        call fzdiff_bkname(l1, bkname(1), lbkname(1))
        call fzdiff_bkname(l2, bkname(2), lbkname(2))
        if(bkname(1)(1:lbkname(1)).eq.bkname(2)(1:lbkname(2)))then
          print 20,bkname(1)(1:lbkname(1))
 20       format(1x,a)
        else
          print 30,(bkname(i)(1:lbkname(i)),i=1,2)
 30       format(1x,a,' = ',a)
        endif
      endif
C-
C- (Re)do precomparison.
C-
      fzdiff_comp = fzdiff_precomp(l1, l2, chopt)
C-
C- Obtain bank size information (number of links, data words) from the two
C- banks.  This should be identical for the two banks since the precomparison
C- succeeded (but we use the minimum of the two anyway).
C-
      ns = min0(iq(l1-2), iq(l2-2))
      nl = min0(iq(l1-3), iq(l2-3))
      nd = min0(iq(l1-1), iq(l2-1))
C-
C- Get floating point tolerance for this bank.
C-
      call ezpick('FZDIFF_RCP')
      if(ezerr(ier))then
        call errmsg('fzdiff','fzdiff_comp',
     &    'Unable to pick SRCP bank FZDIFF_RCP','F')
      endif
      call uhtoc(iq(l1-4),4,bank,4)
      tol_param = 'TOL_'//bank
      call ezget(tol_param, tol, ier)
      if(ier.ne.0)then
        ls = l1
 50     continue
        call uhtoc(iq(ls-4),4,bank,4)
        tol_param = 'TOL_'//bank//'_S'
        call ezget(tol_param, tol, ier)
        if(ier.ne.0)then
          ls = lq(ls+1)
          if(ls.ne.0)go to 50
        endif
      endif
      if(ier.ne.0)call ezget('TOL_DEFAULT', tol, ier)
      if(ier.ne.0)then
        call errmsg('fzdiff', 'fzdiff_comp',
     &    'Can''t determine floating point tolerance','F')
      endif
C-
C- Get double precision tolerance for this bank.
C-
      call uhtoc(iq(l1-4),4,bank,4)
      tol_param = 'DTOL_'//bank
      call ezget(tol_param, dtol, ier)
      if(ier.ne.0)then
        ls = l1
 60     continue
        call uhtoc(iq(ls-4),4,bank,4)
        tol_param = 'DTOL_'//bank//'_S'
        call ezget(tol_param, dtol, ier)
        if(ier.ne.0)then
          ls = lq(ls+1)
          if(ls.ne.0)go to 60
        endif
      endif
      if(ier.ne.0)call ezget('DTOL_DEFAULT', dtol, ier)
      if(ier.ne.0)then
        call errmsg('fzdiff', 'fzdiff_comp',
     &    'Can''t determine double precision tolerance','F')
      endif
      call ezrset
C-
C- Detailed comparison.  First compare whether the same structural and
C- reference links are filled.
C-
      do 150 i=1,ns
        if(lq(l1-i).ne.0.and.lq(l2-i).eq.0)then
          if(.not.quiet)print 110, i
 110      format(' ****** Extra filled structural link',i4,' in file 1')
          fzdiff_comp = .false.
        endif
        if(lq(l1-i).eq.0.and.lq(l2-i).ne.0)then
          if(.not.quiet)print 120, i
 120      format(' ****** Extra filled structural link',i4,' in file 2')
          fzdiff_comp = .false.
        endif
 150  continue
      do 250 i=ns+1,nl
        if(lq(l1-i).ne.0.and.lq(l2-i).eq.0)then
          if(.not.quiet)print 210, i
 210      format(' ****** Extra filled reference link',i4,' in file 1')
          fzdiff_comp = .false.
        endif
        if(lq(l1-i).eq.0.and.lq(l2-i).ne.0)then
          if(.not.quiet)print 220, i
 220      format(' ****** Extra filled reference link',i4,' in file 2')
          fzdiff_comp = .false.
        endif
 250  continue
C-
C- Obtain format information.
C-
      call mziotc(ixcom, l1, nc(1), chform(1))
      call mziotc(ixcom, l2, nc(2), chform(2))
      if(chform(1)(1:nc(1)).ne.chform(2)(1:nc(2)))then
        if(.not.quiet)print 270,(chform(i)(1:nc(i)),i=1,2)
 270    format(' ****** I/O characteristic differs: ',a,',',2x,a)
        fzdiff_comp = .false.
      endif
C-
C- Exit routine here if detail RCP parameter is false.
C-
      if(.not.detail)go to 999
C-
C- Compare data words based on type using the format information of bank 1.
C-
      num_error = 0
      id = 1
C-
C- Loop over words in I/O characteristic.
C-
      do while(nc(1).gt.0)
        do while(nc(1).gt.0.and.chform(1)(1:1).eq.' ')
          chform(1)(1:nc(1)-1) = chform(1)(2:nc(1))
          nc(1) = nc(1) - 1
        enddo
        ncw = index(chform(1)(1:nc(1)),' ')-1
        if(ncw.lt.0)ncw = nc(1)
C-
C- Extract the length and type fields of this word.
C-
        type = chform(1)(ncw:ncw)
        if(chform(1)(1:1).eq.'*' .and. ncw.eq.2)then
          lc = iq(l1+id)
          id = id + 1
        elseif(chform(1)(1:1).eq.'-' .and. ncw.eq.2)then
          lc = nd-id+1
        else
          lc = 0
          do i = 1,ncw-1
            if(chform(1)(i:i).lt.'0' .and. chform(1)(i:i).gt.'9')then
              print 290, chform(1)
 290          format(' Couldn''t crack I/O characteristic: ',a)
              fzdiff_comp = .false.
              go to 999
            endif
            lc = 10*lc + ichar(chform(1)(i:i)) - ichar('0')
          enddo
        endif
        lc = min0(lc, nd-id+1)
C-
C- Check various types handled here.
C-
C- Self-describing not implemented.
C-
        if(type.eq.'S')then
          print 300
 300      format(' ****** S format not handled')
          fzdiff_comp = .false.
          go to 999
C-
C- Repeat.
C-
        elseif(type.eq.'/')then
          chform(1)(nc(1)+1:2*nc(1)) = chform(1)(1:nc(1))
          nc(1) = 2*nc(1)
C-
C- Bitstring/integer/undefined -- require exact integer comparison
C-
        elseif(type.eq.'B' .or. type.eq.'I' .or. type.eq.'U')then
          do i = id, id+lc-1
            mismatch = iq(l1+i).ne.iq(l2+i)
            if(mismatch.or.verbose)tag=udst_tag(l1,i,jer)
            if(mismatch)then
              num_error = num_error + 1
              if(.not.quiet .and. num_error.le.max_error)then
                print 310,type,i,iq(l1+i),iq(l2+i),tag
 310            format(' ****** ',a1,'/ IQ(L+',i4,'):',2(12x,z8),2x,a8)
              endif
              fzdiff_comp = .false.
            elseif(verbose)then
              print 311,type,i,iq(l1+i),iq(l2+i),tag
 311          format('        ',a1,'/ IQ(L+',i4,'):',2(12x,z8),2x,a8)
            endif
          enddo
C-
C- Floating -- use tolerance.
C-
        elseif(type.eq.'F')then
          do i = id, id+lc-1
            a(1) = q(l1+i)
            a(2) = q(l2+i)
            if(a(1).eq.0. .and. a(2).eq.0.)then
              rel = 0.
            else
              rel = abs(a(1) - a(2))/amax1(abs(a(1)), abs(a(2)))
              if(abs(a(1)).le.zero .and. abs(a(2)).le.zero)rel = 0.
            endif
            mismatch = rel.gt.tol
            if(mismatch.or.verbose)tag=udst_tag(l1,i,jer)
            if(mismatch)then
              num_error = num_error + 1
              if(.not.quiet .and. num_error.le.max_error)then
                print 330,i,a,tag
 330            format(' ****** F/  Q(L+',i4,'):',2g20.6,2x,a8)
              endif
              fzdiff_comp = .false.
            elseif(verbose)then
              print 331,i,a,tag
 331          format('        F/  Q(L+',i4,'):',2g20.6,2x,a8)
            endif
          enddo
C-
C- Hollerith
C-
        elseif(type.eq.'H')then
          do i = id, id+lc-1
            hol(1) = iq(l1+i)
            hol(2) = iq(l2+i)
            mismatch = hol(1).ne.hol(2)
            if(mismatch.or.verbose)then
              do 346 j=1,2
                chout(j) = ' '
                lout(j) = 0
                do 345 k=1,4
                  printing = lge(chol(j)(k:k), ' ')
     &                 .and. lle(chol(j)(k:k), '~')
                  if(printing)then
                    lout(j) = lout(j) + 1
                    chout(j)(lout(j):lout(j)) = chol(j)(k:k)
                  else
                    lout(j) = lout(j) + 4
                    write(chout(j)(lout(j)-3:lout(j)),344)
     &                   ichar(chol(j)(k:k))
 344                format('\',o3.3)
                  endif
 345            continue
 346          continue
            endif
            if(mismatch)then
              num_error = num_error + 1
              if(.not.quiet .and. num_error.le.max_error)then
                tag=udst_tag(l1,i,jer)
                print 350,i,(chout(j),j=1,2),tag
 350            format(' ****** H/ IQ(L+',i4,'):',2(4x,a16),2x,a8)
              endif
              fzdiff_comp = .false.
            elseif(verbose)then
              print 351,i,(chout(j),j=1,2),tag
 351          format('        H/ IQ(L+',i4,'):',2(4x,a16),2x,a8)
            endif
          enddo
C-
C- Double -- use double tolerance.
C-
        elseif(type.eq.'D')then
          do i = id, id+lc-1, 2
            dbla(1,1) = q(l1+i)
            dbla(2,1) = q(l1+i+1)
            dbla(1,2) = q(l2+i)
            dbla(2,2) = q(l2+i+1)
            if(da(1).eq.0.d0 .and. da(2).eq.0.d0)then
              drel = 0.
            else
              drel = dabs(da(1) - da(2))/dmax1(dabs(da(1)), dabs(da(2)))
              if(abs(da(1)).le.dzero .and. abs(da(2)).le.dzero)drel = 0.
            endif
            mismatch = drel.gt.dtol
            if(mismatch)then
              num_error = num_error + 1
              if(.not.quiet .and. num_error.le.max_error)then
                print 340,i,i+1,da
 340            format(' ****** D/  Q(L+',i4,'),Q(L+',i4,'):',2d20.13)
              endif
              fzdiff_comp = .false.
            elseif(verbose)then
              print 341,i,i+1,da
 341          format('        D/  Q(L+',i4,'),Q(L+',i4,'):',2d20.13)
            endif
          enddo
        else
          num_error = num_error + 1
          if(.not.quiet .and. num_error.le.max_error)then
            print 400,type
 400        format(' ****** Unknown data type', a1)
          endif
          fzdiff_comp = .false.
        endif
C-
C- Advance to next word.
C-
        id = id + lc
        if(id.gt.nd)go to 999
        chform(1)(1:nc(1)-ncw-1) = chform(1)(ncw+2:nc(1))
        nc(1) = nc(1)-ncw-1
      enddo
      if(.not.quiet .and. .not.verbose .and. num_error.gt.max_error)then
        print 1100
 1100   format(' ****** There were additional data mismatches')
      endif
  999 return
      end
