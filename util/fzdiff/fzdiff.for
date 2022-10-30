      program fzdiff
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyze and compare two FZ files
C-
C-   VMS Usage:
C-
C-      fzdiff file1/opt1 file2/opt2
C-
C-   UNIX Usage:
C-
C-      fzdiff.x file1:opt1 file2:opt2
C-
C-   Notes:
C-
C-   1.  The command line options are arbitrary d0open options (e.g. 
C-       /x = exchange mode, /t = tape mode (no xchker)).  No translate
C-       (n) option is default on UNIX.
C-
C-   Created   8-Sep-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$util$fzdiff:fzdiff_zebcom.inc'
      include 'd0$inc:quest.inc'
C-
C- Link area for top level banks
C-
      common /fzdiff_lnk/ lfzhd(2)
      integer lfzhd
C-
C- User header vector (used by FZIN)
C-
      integer maxhead
      parameter (maxhead=100)
      integer nuh, iuhead(maxhead)
C-
C- 2-dimensaional arrays describing the two FZ files.
C-
      character*256 file(2)       ! Filenames
      integer flen(2)             ! Number of characters in filename
      character*8 chopt(2)        ! D0open character options from command line.
      integer unit(2)             ! Fortran unit numbers
      character*1 cfile(2)        ! Character file index (i.e. 1 and 2)
      integer pos(2)              ! Last returned value of iquest(1).
      integer evnum(2)            ! Event number
C-
C- For command line scanning
C-
      character*256 line          ! Command line input buffer
      integer*2 llen              ! Number of characters in command line
      character*40 prompt         ! User prompt
      integer force               ! Force prompt flag
C-
C- Global variables
C-
      integer skip_runs(2)        ! Number of runs to skip
      integer skip_events(2)      ! Number of events to skip
      integer num_events          ! Number of events to process
      logical same                ! Global comparison status
      logical uncompress          ! Uncompress flag
C-
C- List of banks to drop.
C-
      integer max_drop, ndrop
      parameter(max_drop=100)
      character*4 drop_banks(max_drop)
      integer hdrop(max_drop)
      equivalence(drop_banks(1), hdrop(1))
      integer lgo, lbank, lzscan
C-
C- Other temporary variables
C-
      integer lrecl               ! FZFILE record length
      character*8 opt             ! Character options
      logical ok                  ! Logical return status
      integer ier                 ! Integer return status
      character*80 msg            ! Output message buffer
      integer ifile, i, j, n
C-
C- Option character
C-
      character*1 optch
C&IF VAXVMS
      parameter (optch = '/')
      EXTERNAL LIB$FIXUP_FLT
C&ELSE
C&      parameter (optch = ':')
C&ENDIF
C-
C- Functions
C-
      logical lib$get_foreign
      integer trulen
      logical fzdiff_stcomp

      integer ihhead/4HHEAD/
C-----------------------------------------------------------------------
C&IF VAXVMS
      CALL SYS$SETEXV(%VAL(1),LIB$FIXUP_FLT,,)
C&ENDIF
C-
C- Initialize Zebra and read in RCP file
C-
      call mzebra(-3)
      call fzdiff_inzcom(2)
      call inzstp
      call mzlink(ixcom, '/fzdiff_lnk/', lfzhd(1), lfzhd(2), lfzhd(1))
      call inrcp('fzdiff_rcp', ier)
      if(ier.ne.0)then
        call inrcp('fzdiff.rcp', ier)
      endif
      if(ier.ne.0)then
        call inrcp('d0$util$fzdiff:fzdiff.rcp', ier)
      endif
      if(ier.ne.0)then
        call errmsg('fzdiff','fzdiff','Error reading RCP file','F')
      endif
      call ezpick('fzdiff_rcp')
      call ezget_i('NUM_EVENTS', num_events, ier)
      if(ier.eq.0)call ezget_i('SKIP_RUNS_1', skip_runs(1), ier)
      if(ier.eq.0)call ezget_I('SKIP_RUNS_2', skip_runs(2), ier)
      if(ier.eq.0)call ezget_i('SKIP_EVENTS_1', skip_events(1), ier)
      if(ier.eq.0)call ezget_i('SKIP_EVENTS_2', skip_events(2), ier)
      if(ier.eq.0)then
        call ezget_l('UNCOMPRESS_DATA', uncompress, ier)
        if(ier.ne.0)then
          ier = 0
          uncompress = .true.
        endif
      endif
      if(ier.eq.0)then
        call ez_get_chars('DROP_BANKS', ndrop, drop_banks, ier)
        if(ier.ne.0)then
          ier = 0
          ndrop = 0
        endif
      endif
      if(ier.ne.0)then
         call errmsg('fzdiff','fzdiff',
     &     'Failed to read required RCP parameter','F')
      endif
      if(ndrop.gt.max_drop)then
         call errmsg('fzdiff','fzdiff',
     &     'Too many banks to drop','F')
      endif
      call ezrset
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
          prompt = 'Enter file '//cfile(ifile)//': '
          ok = lib$get_foreign(line, prompt(1:trulen(prompt)+1), 
     &         llen, force)
          if(.not.ok)go to 10
          call word(line(1:llen), i, j, n)
          if(n.eq.0)go to 10
        endif
        file(ifile) = line(i:j)
        line(i:j) = ' '
 20   continue
C-
C- Parse filenames for d0open options 
C-
      do 30 ifile = 1,2
        chopt(ifile) = 'N'
        do 25 i=trulen(file(ifile)),1,-1
          if(file(ifile)(i:i).eq.optch)go to 26
 25     continue
        i=0
 26     continue
        if(i.ne.0)then
          chopt(ifile) = 'N'//file(ifile)(i+1:)
          file(ifile)(i:) = ' '
        endif
        flen(ifile) = trulen(file(ifile))
 30   continue
C-
C- Check if either filename is specified as '='
C-
      do 35 ifile = 1,2
        if(file(ifile).eq.'=')then
          file(ifile) = file(3-ifile)
          flen(ifile) = flen(3-ifile)
          chopt(ifile) = chopt(3-ifile)
        endif
 35   continue
C-
C- Open files
C-
      do 40 ifile = 1,2
        call gtunit(763, unit(ifile), ier)
        if(ier.ne.0)call errmsg('fzdiff', 'fzdiff', 
     &    'No unit available', 'F')
        opt = 'IU'//chopt(ifile)
        call d0open(unit(ifile), file(ifile)(1:flen(ifile)), opt, ok)
        if(.not.ok)then
          msg = 'Unable to open file '//cfile(ifile)
          call errmsg('fzdiff', 'fzdiff', msg, 'F')
        endif
        call xzrecl(lrecl, opt)
        call fzfile(unit(ifile), lrecl, opt)
 40   continue
C-
C- Event loop
C-
      pos(1) = 0
      pos(2) = 0
C-
C- Skip runs
C-
      do 120 ifile = 1,2
        do 110 i = 1, skip_runs(ifile)
          call fzin(unit(ifile), ixmain, lfzhd(ifile), 1, 'R', 
     &      nuh, iuhead)
        if(iquest(1).lt.0)then
          call errmsg('fzdiff','fzdiff','Error skipping runs','F')
        endif
 110    continue
 120  continue
C-
C- Skip events
C-
      do 150 ifile = 1,2
        do 140 i = 1, skip_events(ifile)
          call fzin(unit(ifile), ixmain, lfzhd(ifile), 1, 'E', 
     &      nuh, iuhead)
        if(iquest(1).lt.0)then
          call errmsg('fzdiff','fzdiff','Error skipping events','F')
        endif
 140    continue
 150  continue
C-
C- Event loop
C-
      same = .true.
 500  continue
      if(num_events.eq.0)go to 1000
C-
C- Read next event from both files.  
C-
      call mzwipe(ixmain)
      do 200 ifile = 1,2
        nuh = maxhead
        call fzin(unit(ifile), ixmain, lfzhd(ifile), 1, ' ', 
     &    nuh, iuhead)
        pos(ifile) = iquest(1)
C-
C- Here we test the fzin return status and handle errors and end of files
C- and print informative messages.
C-
        if(iquest(1).lt.0)then
          msg = 'Error reading file '//cfile(ifile)
          call errmsg('fzdiff', 'fzdiff', msg, 'W')
          go to 2000
        elseif(iquest(1).eq.1)then
          msg = ' Start-of-run record for file '//cfile(ifile)
          call intmsg(msg)
        elseif(iquest(1).eq.2)then
          msg = ' End-of-run record for file '//cfile(ifile)
          call intmsg(msg)
        elseif(iquest(1).ge.3)then
          msg = ' End-of-file for file '//cfile(ifile)
          call intmsg(msg)
          call  fzendi(unit(ifile), 'T')
        endif
 200  continue
C-
C- Check for phase errors between file 1 and file 2 here.
C-
      if(pos(1).ne.pos(2))then
        write(msg,210)pos(1),pos(2)
 210    format('Phase error: iquest = ',2i5,'.    Giving up')
        same = .false.
        call errmsg('fzdiff', 'fzdiff', msg, 'W')
        go to 1000
      endif
C-
C- If these are banks of type HEAD, then compare and print event numbers
C-
      if(iq(lfzhd(1)-4).eq.iq(lfzhd(2)-4) 
     &  .and. iq(lfzhd(2)-4).eq.ihHEAD)then
        evnum(1) = iq(lfzhd(1) + 9)
        evnum(2) = iq(lfzhd(2) + 9)
        if(evnum(1).eq.evnum(2))then
          print 220,evnum(1)
 220      format(/' Event ',i8/)
        else
          print 221,evnum(1),evnum(2)
 221      format(/' ****** Event numbers differ: file 1 = ',
     &      i8,', file 2 = ',i8/)
          same = .false.
        endif
      endif
C-
C- Skip past start/end run records
C-
      if(pos(1).eq.1.or.pos(1).eq.2)go to 500
C-
C- Quit if end of file
C-
      if(pos(1).ge.3)go to 1000
C-
C- Successfully read an event from both files.  Uncompress data.
C-
      if(uncompress)then
        do i = 1,2
          lhead = lfzhd(i)
          if(iq(lhead-4).eq.ihHEAD)call uncompress_zebra
        enddo
      endif
C-
C- Drop banks.
C-
      if(ndrop.gt.0)then
        lgo=0
 250    lbank=lzscan(ixmain,lgo)
        if(lbank.ne.0) then
          lgo=lbank
          if(iquest(1).eq.0)then
            do i=1,ndrop
              if(iq(lbank-4).eq.hdrop(i))then
                call mzdrop(ixcom,lbank,'l')
                goto 250
              endif
            enddo
          endif
          goto 250
        endif
      endif
C-
C- Compare.
C-
      ok = fzdiff_stcomp(lfzhd(1), lfzhd(2), ' ')
      same = same .and. ok
C-
C- Get next event
C-
      num_events = num_events - 1
      go to 500
C-
C- Successfull completeion
C-
 1000 continue
      if(same)then
        print 1001
 1001   format(/' Files AGREE')
      else
        print 1002
 1002   format(/' Files are DIFFERENT')
      endif
      stop
C-
C- Fatal errors handled here
C-
 2000 continue
      call zfatal
      stop
      end
