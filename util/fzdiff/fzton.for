      program fzton
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Copy an FZ file to native mode (by default)
C-                         with optional simple filtering.  Other output 
C-                         formats (X, G, Z) are available by command
C-                         line options.
C-
C-   VMS Usage:
C-
C-      fzton file1/opt1 file2/opt2 key=arg1,arg2 key=arg,arg
C-
C-   UNIX Usage:
C-
C-      fzton.x file1@opt1 file2@opt2 key=arg1,arg2 key=arg,arg
C-
C-   Usage notes:
C-
C-   1.  Filename options are passed directory to D0OPEN.
C-
C-   2.  If the second file argument is missing it defaults to the
C-       null device (nl: or /dev/null).
C-
C-   3.  Keyword/argument combinations should not have embedded spaces.
C-
C-   4.  Keywords and arguments:
C-
C-       disp=[nz12] - Display data:
C-                     n = Run and event number.
C-                     z = Zebra bank structure.
C-                     1 = Level 1 triggers.
C-                     2 = Level 2 triggers.
C-       oform=[xgnz]- Output format.
C-       verify      - Verify using DZVERI before calling FZOUT.
C-       comp        - Compress using COMPRESS_ZEBRA.  COMPRESS_ZEBRA_RCP 
C-                     bank read from logical, local directory or d0library.
C-       uncomp      - Uncompress using UNCOMPRESS_ZEBRA.
C-       skip=<n>    - Skip n events before copying.
C-       copy=<n>    - Copy n events.
C-       run=<n>     - Copy only events from the specified run.
C-       event=<n>   - Copy only events with the specified number.
C-       skevt=<n>   - Copy all events except the specified one.
C-       drop=<bank1>,<bank2>,... 
C-                   - Banks to drop
C-       drop=raw    - Drop all 8 raw data banks.
C-       noeor       - Ignore begin and end of run records.
C-       trig=<trig1>,<trig2>,...
C-                   - Copy only events containing one of the listed triggers.
C-       filt=<filt1>,<filt2>,..
C-                   - Copy only events containing one of the listed filters.
C-       pmuo=<n>,<etamax>,<ptmin>
C-                   - Require n pmuo banks with (ifw4<=1) with 
C-                     abs(eta)<etamax and pt>ptmin.
C-       jets=<n>,<cone>,<etamax>,<ptmin>,<emfracmin>
C-                   - Require n cone jet jets banks with the specified cone
C-                     size and other parameters.
C-       pelc=<n>,<etamax>,<etmin>
C-
C-   Created   16-Oct-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$util$fzdiff:fzdiff_zebcom.inc'
      include 'd0$inc:quest.inc'
      include 'd0$links:izjets.link'
C-
C- User header vector (used by FZIN)
C-
      integer maxhead
      parameter (maxhead=100)
      integer nuh, iuhead(maxhead)
C-
C- 2-dimensaional arrays describing the two FZ files (1=input, 2=output).
C-
      character*256 file(2)       ! Filenames
      character*256 wildcard, input_file
      integer context
      logical output_open
      integer flen(2)             ! Number of characters in filename
      character*8 chopt(2)        ! D0open character options from command line.
      integer unit(2)             ! Fortran unit numbers
      character*1 cfile(2)        ! Character file index (i.e. 1 and 2)
C-
C- Error counters and verification
C-
      integer nfind_fail
      integer nerror, max_error
      logical verify, comp, uncomp, noeor
C-
C- For command line scanning
C-
      character*1024 line         ! Command line input buffer
      integer*2 llen              ! Number of characters in command line
      character*40 prompt         ! User prompt
      integer force               ! Force prompt flag
C-
C- Keywords and arguments.
C-
      integer max_arg, narg
      parameter (max_arg=100)
      character*12 key,arg(max_arg)
      logical nmode, xmode, gmode, zmode
      logical display_run_event, display_banks, display_trig, 
     &  display_filt
C-
C- Triggers.
C-
      integer ntrig, nfilt, ntrig_sel, nfilt_sel
      integer trig_bit(32), filt_bit(128)
      character*32 trig_name(32), filt_name(128)
      character*32 trig_name_sel(32), filt_name_sel(128)
      integer ltsum, point
      logical unpack_tsum, match_trig, match_filt
C-
C- Kinematic variables.
C-
      real eta, pt, et
C-
C- Muons.
C-
      integer npmuo, npmuo_sel, ifw4, lpmuo
      real pmuo_etamax, pmuo_ptmin
C-
C- Electrons.
C-
      integer npelc, npelc_sel, lpelc
      real pelc_etamax, pelc_etmin
C-
C- Jets.
C-
      integer njets, njets_sel, lcaph, ljets
      real jets_cone, jets_etamax, jets_etmin, jets_emfracmin
      real emfrac
C-
C- Other temporary variables
C-
      integer lrecl               ! FZFILE record length
      character*8 opt             ! Character options
      logical ok                  ! Logical return status
      integer ier                 ! Integer return status
      character*80 msg            ! Output message buffer
      integer i, j, k, l, n, ifile, nev, nfile, nskip, ncopy, 
     &  match_event, match_run, skip_event
      integer run, ev
      logical first_file
C-
C- List of banks to drop
C-
      integer ndrop, max_drop
      parameter (max_drop=100)
      integer hbank(max_drop)
C-
C- Option character
C-
      character*1 optch
      character*12 null_device
C&IF VAXVMS
      parameter (optch = '/')
      parameter (null_device = 'NL:')
C&ELSE
C&      parameter (optch = '@')
C&      parameter (null_device = '/dev/null')
C&ENDIF
C-
C- Functions
C-
      logical lib$get_foreign, lib$find_file
      integer gztsum, gzpmuo, gzcaph, gzpelc
      integer trulen
      integer lzfidh
      data nev, nfile/2*0/
      data output_open/.false./
      data nfind_fail/0/
      data nerror, max_error/0, 10/
C- Error variable to handle exit codes ( K. Denisenko)
      logical err /.false./
C-----------------------------------------------------------------------
C-
C- Initialize Zebra and read in RCP file
C-
      call mzebra(-3)
      call fzdiff_inzcom(2)
      call inzstp
C-
C- First get input files from command line or by prompting.
C-
      force = 0
      llen = 0
      do 20 ifile = 1,2
        write(cfile(ifile),'(i1)')ifile
C-
C- Get next word
C-
 10     continue
        call word(line(1:llen), i, j, n)
        if(n.eq.0)then
          if(ifile.eq.1)then
            prompt = 'Enter file '//cfile(ifile)//': '
            ok = lib$get_foreign(line, prompt(1:trulen(prompt)+1), 
     &           llen, force)
            if(.not.ok)go to 10
            call word(line(1:llen), i, j, n)
            if(n.eq.0)go to 10
          endif
        endif
        if(index(line(i:j),'=').ne.0)then
          file(ifile) = null_device
          go to 20
        endif
        if(n.ne.0)then
          file(ifile) = line(i:j)
          line(i:j) = ' '
        else
          file(ifile) = null_device
        endif
 20   continue
C-
C- Parse the remainder of the command line for keywords and arguments.
C-
      nskip = 0
      ncopy = 1000000
      verify = .false.
      comp = .false.
      uncomp = .false.
      noeor = .false.
      match_run = -1
      match_event = -1
      skip_event = -1
      ndrop = 0
      xmode = .true.
      gmode = .false.
      nmode = .false.
      zmode = .false.
      display_run_event = .false.
      display_banks = .false.
      display_trig = .false.
      display_filt = .false.
      unpack_tsum = .false.
      ntrig_sel = 0
      nfilt_sel = 0
      npmuo_sel = 0
      pmuo_etamax = 10.
      pmuo_ptmin = 0.
      npelc_sel = 0
      pelc_etamax = 10.
      pelc_etmin = 0.
      njets_sel = 0
      jets_cone = 0.
      jets_etamax = 10.
      jets_etmin = 0.
      jets_emfracmin = 0.
      
C- Top of keyword loop.
 30   continue
      call word(line(1:llen), i, j, n)
      if(n.eq.0)go to 40
      call cutol(line(i:j))
C- First extract keyword.
      k = index(line(i:j),'=') + i - 1
      if(k.lt.i)then
        key = line(i:j)
        line(i:j) = ' '
        i = j + 1
      else
        key = line(i:k-1)
        line(i:k) = ' '
        i = k + 1
      endif
C- Then extract arguments.
      narg = 0
      do while (i.le.j)
        narg = narg + 1
        k = index(line(i:j),',') + i - 1
        if(k.lt.i)then
          arg(narg) = line(i:j)
          line(i:j) = ' '
          i = j + 1
        else
          arg(narg) = line(i:k-1)
          line(i:k) = ' '
          i = k + 1
        endif
      enddo
C-
C- Process keyword/arguments here
C-
C- Skip
      if(key.eq.'skip'.and.narg.gt.0)then
        read(arg(1),'(bn,i12)')nskip
      endif
C-
C- Copy
C-
      if(key.eq.'copy'.and.narg.gt.0)then
        read(arg(1),'(bn,i12)')ncopy
      endif
C-
C- Verify
C-
      if(key.eq.'verify')then
        verify = .true.
      endif
C-
C- Compress
C-
      if(key.eq.'comp')then
        comp = .true.
        call inrcp('compress_zebra_rcp', ier)
        if(ier.ne.0)call inrcp('compress_zebra.rcp', ier)
        if(ier.ne.0)call inrcp('d0$event_util:compress_zebra.rcp', ier)
        if(ier.ne.0)call errmsg('fzton', 'fzton', 
     &    'Unable to read COMPRESS_ZEBRA_RCP', 'F')
        call compress_zebra_ini
      endif
C-
C- Noeor
C-
      if(key.eq.'noeor')then
        noeor = .true.
      endif
C-
C- Uncompress
C-
      if(key.eq.'uncomp')then
        uncomp = .true.
      endif
C-
C- Run
C-
      if(key.eq.'run'.and.narg.gt.0)then
        read(arg(1),'(bn,i12)')match_run
      endif
C-
C- Event
C-
      if(key.eq.'event'.and.narg.gt.0)then
        read(arg(1),'(bn,i12)')match_event
      endif
C-
C- Skip event
C-
      if(key.eq.'skevt'.and.narg.gt.0)then
        read(arg(1),'(bn,i12)')skip_event
      endif
C-
C- Drop banks.
C-
      if(key.eq.'drop')then
        do i = 1,narg
          if(arg(i).eq.'raw')then
            arg(i) = 'TRGR'
            arg(narg+1) = 'MUD1'
            arg(narg+2) = 'CDD1'
            arg(narg+3) = 'CDD2'
            arg(narg+4) = 'CDD3'
            arg(narg+5) = 'CDD4'
            arg(narg+6) = 'CAD1'
            arg(narg+7) = 'CAD2'
            narg = narg + 7
          endif
        enddo
        ndrop = narg
        do i=1,narg
          call cltou(arg(i))
          call uctoh(arg(i), hbank(i), 4, 4)
        enddo
      endif
C-
C- Display options
C-
      if(key.eq.'disp')then
        display_run_event = .true.
        if(narg.gt.0)then
          if(index(arg(1),'z').ne.0)display_banks = .true.
          if(index(arg(1),'1').ne.0)display_trig = .true.
          if(index(arg(1),'2').ne.0)display_filt = .true.
        endif
      endif
      if(display_trig.or.display_filt)unpack_tsum = .true.
C-
C- Output format
C-
      if(key.eq.'oform')then
        if(narg.eq.0)then
          narg = 1
          arg(1) = 'x'
        endif
        if(index(arg(1),'x').ne.0)then
          xmode = .true.
          gmode = .false.
          nmode = .false.
          zmode = .false.
        endif
        if(index(arg(1),'g').ne.0)then
          xmode = .false.
          gmode = .true.
          nmode = .false.
          zmode = .false.
        endif
        if(index(arg(1),'n').ne.0)then
          xmode = .false.
          gmode = .false.
          nmode = .true.
          zmode = .false.
        endif
        if(index(arg(1),'z').ne.0)then
          xmode = .false.
          gmode = .false.
          nmode = .false.
          zmode = .true.
        endif
      endif
C-
C- Trigger select.
C-
      if(key.eq.'trig')then
        unpack_tsum = .true.
        ntrig_sel = narg
        do i=1,narg
          call cltou(arg(i))
          trig_name_sel(i) = arg(i)
        enddo
      endif
C-
C- Filter select.
C-
      if(key.eq.'filt')then
        unpack_tsum = .true.
        nfilt_sel = narg
        do i=1,narg
          call cltou(arg(i))
          filt_name_sel(i) = arg(i)
        enddo
      endif
C-
C- Pmuo.
C-
      if(key.eq.'pmuo')then
        if(narg.ge.1)then
          read(arg(1),'(bn,i12)')npmuo_sel
        endif
        if(narg.ge.2)then
          read(arg(2),'(bn,f12.3)')pmuo_etamax
        endif
        if(narg.ge.3)then
          read(arg(3),'(bn,f12.3)')pmuo_ptmin
        endif
      endif
C-
C- Pelc.
C-
      if(key.eq.'pelc')then
        if(narg.ge.1)then
          read(arg(1),'(bn,i12)')npelc_sel
        endif
        if(narg.ge.2)then
          read(arg(2),'(bn,f12.3)')pelc_etamax
        endif
        if(narg.ge.3)then
          read(arg(3),'(bn,f12.3)')pelc_etmin
        endif
      endif
C-
C- Jets.
C-
      if(key.eq.'jets')then
        if(narg.ge.1)then
          read(arg(1),'(bn,i12)')njets_sel
        endif
        if(narg.ge.2)then
          read(arg(2),'(bn,f12.3)')jets_cone
        endif
        if(narg.ge.3)then
          read(arg(3),'(bn,f12.3)')jets_etamax
        endif
        if(narg.ge.4)then
          read(arg(4),'(bn,f12.3)')jets_etmin
        endif
        if(narg.ge.5)then
          read(arg(5),'(bn,f12.3)')jets_emfracmin
        endif
      endif
C-
C- Do next keyword
C-
      go to 30
 40   continue
C-
C- Parse filenames for d0open options 
C-
      do 100 ifile = 1,2
        chopt(ifile) = 'N'
        do 90 i=trulen(file(ifile)),1,-1
          if(file(ifile)(i:i).eq.optch)go to 91
 90    continue
        i=0
 91     continue
        if(i.ne.0)then
          chopt(ifile) = 'N'//file(ifile)(i+1:)
          file(ifile)(i:) = ' '
        endif
        flen(ifile) = trulen(file(ifile))
        if(ifile.eq.2)then
          if(xmode)chopt(ifile) = 'X'//chopt(ifile)
          if(gmode)chopt(ifile) = 'G'//chopt(ifile)
          if(zmode)chopt(ifile) = 'Z'//chopt(ifile)
        endif
        call upcase(chopt(ifile), chopt(ifile))
 100  continue
C-
C- Open next input file
C-
      context = 0
      wildcard = file(1)
 200  continue
      if(file(1).eq.'-'.and.context.eq.0)then
        read(*,'(a)',end=1000)wildcard
      endif
      first_file = context.eq.0
      ok = lib$find_file(wildcard(1:trulen(wildcard)), input_file, 
     &  context)
      if(.not.ok .or. input_file.eq.' ')then
        nfind_fail = nfind_fail + 1
        input_file = wildcard
        call lib$find_file_end(context)
        context = 0
        if(.not.first_file.or.nfind_fail.gt.1)then
          if(file(1).eq.'-')goto 200
          goto 1000
        endif
      endif
      print 201,input_file(1:trulen(input_file))
 201  format(/' Processing file: ',a)
      call gtunit(763, unit(1), ier)
      if(ier.ne.0)call errmsg('fzton', 'fzton', 
     &  'No unit available', 'F')
      opt = 'IU'//chopt(1)
      call d0open(unit(1), input_file, opt, ok)
      if(.not.ok)then
        msg = 'Unable to open file '//input_file
        call errmsg('fzton', 'fzton', msg, 'F')
      endif
      call xzrecl(lrecl, opt)
      call fzfile(unit(1), lrecl, opt)
C-
C- Open output file (if necessary).
C-
      if(.not.output_open)then
        call gtunit(763, unit(2), ier)
        if(ier.ne.0)call errmsg('fzton', 'fzton', 
     &    'No unit available', 'F')
        opt = 'OU'//chopt(2)
        call d0open(unit(2), file(2)(1:flen(2)), opt, ok)
        if(.not.ok)then
          msg = 'Unable to open file '//file(2)
          call errmsg('fzton', 'fzton', msg, 'F')
        endif
        call xzrecl(lrecl, opt)
        call fzfile(unit(2), lrecl, opt)
        output_open = .true.
      endif
C-
C- Skip events
C-
      do 400 i=1,nskip
        call mzwipe(ixmain)
        nuh = maxhead
        call fzin(unit(1), ixmain, lhead, 1, 'S', nuh, iuhead)
        if(iquest(1).lt.0)then
          msg = 'Error reading file '//input_file
          call errmsg('fzton', 'fzton', msg, 'W')
          go to 2000
        elseif(iquest(1).eq.1)then
          go to 400
        elseif(iquest(1).eq.2)then
          go to 400
        elseif(iquest(1).ge.3)then
          msg = ' End-of-file for file '//cfile(1)
          call intmsg(msg)
          call  fzendi(unit(1), 'T')
          go to 200
        endif
 400  continue
C-
C- Event loop
C-
 500  continue
C-
C- Read next event from file 1.
C-
      call mzwipe(ixmain)
      nuh = maxhead
      call fzin(unit(1), ixmain, lhead, 1, ' ', 
     &  nuh, iuhead)
C-
C- Here we test the fzin return status and handle errors and end of files
C- and print informative messages.
C-
      if(iquest(1).lt.0)then
        msg = 'Error reading file '//cfile(1)
        call errmsg('fzton', 'fzton', msg, 'W')
        nerror = nerror + 1
        if(nerror.le.max_error)go to 500
        err = .true.
        go to 1000
      elseif(iquest(1).eq.1)then
        if(noeor)then
          msg = ' Ignoring start-of-run record for file '//cfile(1)
        else
          msg = ' Start-of-run record for file '//cfile(1)
          call fzrun(unit(2), iquest(11), nuh, iuhead)
        endif
        call intmsg(msg)
        go to 500
      elseif(iquest(1).eq.2)then
        if(noeor)then
          msg = ' Ignoring end-of-run record for file '//cfile(1)
        else
          msg = ' End-of-run record for file '//cfile(1)
          call fzrun(unit(2), -1, nuh, iuhead)
        endif
        nfile = nfile + 1
        call intmsg(msg)
        go to 500
      elseif(iquest(1).ge.3)then
        msg = ' End-of-file for file '//cfile(1)
        call intmsg(msg)
        call  fzendi(unit(1), 'T')
        call d0close(unit(1), ' ', ok)
        if(.not.ok)print *,'d0close failed'
        call rlunit(763, unit(1), ier)
        go to 200
      endif
C-
C- Normal event (iquest(1) = 0) if we get here.  
C- Verify if requested.
C-
      if(verify)then
        call dzveri(' ', ixmain, 'CLSU')
        if(iquest(1).ne.0)go to 500
      endif
C-
C- Uncompression.
C-
      if(uncomp)then
        call uncompress_zebra
      endif
C-
C- See if we want to match event numbers.
C-
      if(match_event.ge.0 .and. (iq(lhead-4).ne.4hHEAD 
     &  .or. iq(lhead+9).ne.match_event))go to 500
      if(skip_event.ge.0 .and. (iq(lhead-4).ne.4hHEAD 
     &  .or. iq(lhead+9).eq.skip_event))go to 500
      if(match_run.ge.0 .and. (iq(lhead-4).ne.4hHEAD 
     &  .or. iq(lhead+6).ne.match_run))go to 500
C-
C- Unpack TSUM bank, if requested.
C-
      if(unpack_tsum)then
        ltsum = gztsum()
        if(ltsum.gt.0)then
          ntrig = iq(ltsum+4)
          nfilt = iq(ltsum+5)
        else
          ntrig = 0
          nfilt = 0
        endif
        point = ltsum+6
        do i = 1,ntrig
          trig_bit(i) = iq(point)
          call uhtoc(iq(point+1), 4, trig_name(i), 32)
          point = point + 9
        enddo
        do i = 1,nfilt
          filt_bit(i) = iq(point)
          call uhtoc(iq(point+1), 4, filt_name(i), 32)
          point = point + 9
        enddo
      endif
C-
C- Match triggers
C-
      match_trig = .false.
      do i = 1,ntrig_sel
        do j = 1,ntrig
          if(trig_name_sel(i).eq.trig_name(j))then
            match_trig = .true.
            go to 510
          endif
        enddo
      enddo
 510  continue
      if(.not.match_trig .and. ntrig_sel.ne.0)go to 500
C-
C- Match filters
C-
      match_filt = .false.
      do i = 1,nfilt_sel
        do j = 1,nfilt
          if(filt_name_sel(i).eq.filt_name(j))then
            match_filt = .true.
            go to 511
          endif
        enddo
      enddo
 511  continue
      if(.not.match_filt .and. nfilt_sel.ne.0)go to 500
C-
C- Muons.
C-
      npmuo = 0
      if(npmuo_sel.gt.0)then
        lpmuo = gzpmuo(0)
        do while(lpmuo.gt.0)
          ifw4 = iq(lpmuo+9)
          eta = q(lpmuo+16)
          pt = q(lpmuo+14)
          if(ifw4.le.1 .and. abs(eta).le.pmuo_etamax .and. 
     &      pt.ge.pmuo_ptmin)npmuo = npmuo + 1
          lpmuo = lq(lpmuo)
        enddo
      endif
      if(npmuo.lt.npmuo_sel)go to 500
C-
C- Electrons.
C-
      npelc = 0
      if(npelc_sel.gt.0)then
        lpelc = gzpelc()
        do while(lpelc.gt.0)
          eta = q(lpelc+9)
          et = q(lpelc+7)
          if(abs(eta).le.pelc_etamax .and. 
     &      et.ge.pelc_etmin)npelc = npelc + 1
          lpelc = lq(lpelc)
        enddo
      endif
      if(npelc.lt.npelc_sel)go to 500
C-
C- Jets.
C-
      njets = 0
      if(njets_sel.gt.0)then
        lcaph = gzcaph()
        do while(lcaph.gt.0)
          if(iq(lcaph+4).eq.2.and.abs(q(lcaph+6)-jets_cone).lt.0.01)then
            ljets = lq(lcaph-izjets)
            do while(ljets.gt.0)
              eta = q(ljets+9)
              et = sqrt(q(ljets+2)**2 + q(ljets+3)**2)
              emfrac = q(ljets+14)
              if(abs(eta).lt.jets_etamax .and. et.gt.jets_etmin
     &          .and. emfrac.gt.jets_emfracmin)njets = njets + 1
              ljets = lq(ljets)
            enddo
          endif
          lcaph = lq(lcaph)
        enddo
      endif
      if(njets.lt.njets_sel)go to 500
C-
C- Drop banks.
C-
      do i = 1, ndrop
        l = lzfidh(ixmain, hbank(i), 0)
        do while(l.gt.0)
          call mzdrop(ixcom, l, ' ')
          l = lzfidh(ixmain, hbank(i), l)
        enddo
      end do
C-
C- Display output if requested.
C-
      if(display_run_event)then
        if(iq(lhead-4).eq.4hHEAD)then
          ev = iq(lhead+9)
          run = iq(lhead+6)
          print 520, run, ev
 520      format(/' Run',i6,', event',i8)
        else
          print 521
 521      format(/' New event')
        endif
        if(display_banks)then
          print 522
 522      format(1x)
          call fzeventdis(lhead)
        endif
        if(display_trig)then
          print 525,ntrig,(trig_bit(i),trig_name(i),i=1,ntrig)
 525      format(/i3,' triggers:',(t20,i6,4x,a32))
        endif
        if(display_filt)then
          print 526,nfilt,(filt_bit(i),filt_name(i),i=1,nfilt)
 526      format(/i3,' filters:',(t20,i6,4x,a32))
        endif
      endif
C-
C- Compression.
C-
      if(comp)then
        call compress_zebra
        call compress_zebra_dst
      endif
C-
C- Write out event
C-
      call fzout(unit(2), ixmain, lhead, 1, ' ', 2, nuh,iuhead)
      if(iquest(1).ne.0)go to 2000
      nev = nev + 1
      if(nev.lt.ncopy)go to 500
C-
C- Successfull completeion.  Terminate output for file 2.
C-
 1000 continue
      call d0close(unit(1), ' ', ok)
      if(.not.ok)print *,'d0close failed'
      call rlunit(763, unit(1), ier)
      if(output_open)then
        call fzendo(unit(2), 'T')
C        call fzendo(unit(2), 'E2')
        call d0close(unit(2), ' ', ok)
        if(.not.ok)print *,'d0close failed'
        output_open = .false.
      endif
      print 1001, nev, nfile
 1001 format(1x,i6,' events in',i3,' runs copied')
      if(err) call exit(-2)
      stop
C-
C- Fatal errors handled here
C-
 2000 continue
      call zfatal
      call exit(-1)
      end
