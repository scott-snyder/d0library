      SUBROUTINE FCONTROL(tag,nskip,ndum)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
c     tag = 0, read next
c           1, read till EOF
c           2, read next "nskip" records
c           3, enable d0xuser
c           4, disable d0xuser
c           5, clear statistics
c           6, calculate statistics
c           7, enable uDST to DST conversion before processing
c           8, disable uDST to DST conversion before processing
c           9, enable CAFIX before processing
c          10, disable CAFIX before processing
c
C-
C-   checks on the iouttype variable to see if we want any output
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer tag,nskip,ndum
c
      character*160 msg,filename,tank
      character*1 xopt
      integer iskip,ier,length,tlen
      logical ok,halted,user,d0xevent
c
c     NOT for STP files!
c
      if (file_type.eq.1.and.tag.lt.3) then
        call xerrmsg('STP File Record Already In Memory')
        return
      endif
c
      if (tag.eq.0) then
c
c       read next record
c
        if (.not.fzrz_opened) then
          call fwarning(%ref('No file currently open!!'))
          return
        endif
c
c       read in event and call d0xuser and d0 packages if (ok)
c
  101   continue
        call readit(.false.,fzrz_lun,ok,user)
        if (ok) then
          ok = d0xevent(halted,user)
        else
c
c         check:  if fzrz_opened.eq..false. then we can open the
c                 next file IF there's a wildcard present
c     
          if (wildc.and.(.not.fzrz_opened)) then
            call search_files(filename,ier)
            if (ier.eq.2) then
              call xerrmsg('No more files exist meeting wildcard spec')
              return
              endif
            length = tlen(filename)
            fzrz_file = filename(1:length)
            fzrz_len = length
            if (fzrz_mode.eq.0) then
              xopt = 'X'
            else
              xopt = 'N'
            endif
            call myopen(filename(1:length),xopt,fzrz_lun,ok)
            fzrz_opened = ok
            dbstore(0) = ixcom
            dbdiv(0) = ixmain
            if (.not.ok) then
              write(tank,
     &          '(''('''' FZ Open Failure On '''',a'',i2.2,'')'')')
     &          length
              write(msg,tank) filename(1:length)
              call xerrmsg(msg)
              write(*,tank) filename(1:length)
              return
            else
              write(tank,
     &        '(''('''' Successful open for file '''',a'',i2.2,'')'')')
     &        length
              write(msg,tank) filename(1:length)
              call xerrmsg(msg)
              write(*,tank) filename(1:length)
              call FSetFLabel(%ref(filename))
              goto 101
            endif
          endif
        endif
c
      else if (tag.eq.1) then
c
c       read till EOF (SCAN) - wildcards are ok here
c
c       loop over all records
c
  100   continue
        ok = .true.
        halted = .false.
        do while (ok)
c
c         read in event and call d0xuser and d0 packages if (ok)
c
          call readit(.false.,fzrz_lun,ok,user)
          if (ok) ok = d0xevent(halted,user)
        enddo
c
c       if we got here, then ok must be false - if there's a wildcard
c       in the filespec, try to open the next one (but only if the HALT
c       button was NOT pushed AND the reqmet was NOT true)
c
        if (wildc.and.(.not.halted)) then
c
c         end of file reached - open another
c
          call search_files(filename,ier)
          if (ier.eq.2) then
            call xerrmsg('No more files exist meeting wildcard spec')
            return
          endif
          length = tlen(filename)
          fzrz_file = filename(1:length)
          fzrz_len = length
          if (fzrz_mode.eq.0) then
            xopt = 'X'
          else
            xopt = 'N'
          endif
          call myopen(filename(1:length),xopt,fzrz_lun,ok)
          fzrz_opened = ok
          dbstore(0) = ixcom
          dbdiv(0) = ixmain
          if (.not.ok) then
            write(tank,
     &          '(''('''' FZ Open Failure On '''',a'',i2.2,'')'')')
     &          length
            write(msg,tank) filename(1:length)
            call xerrmsg(msg)
            write(*,tank) filename(1:length)
            return
          else
            write(tank,
     &        '(''('''' Successful open for file '''',a'',i2.2,'')'')')
     &        length
            write(msg,tank) filename(1:length)
            call xerrmsg(msg)
            write(*,tank) filename(1:length)
            call FSetFLabel(%ref(filename))
            goto 100
          endif
        endif
c
      else if (tag.eq.2) then
c
c       read next "nskip" records
c
        if (.not.fzrz_opened) then
          call fwarning(%ref('No file currently open!!'))
          return
        endif
        if (nskip.eq.0) return
        if (nskip.lt.1) then
          write(msg,'('' ILLEGAL "NSKIP" SPECIFICATION '',I8)') NSKIP
          CALL xerrmsg(msg)
          return
        endif
        do iskip=1,nskip
c
c       read in event and call d0xuser and d0 packages if (ok)
c
          call readit(.false.,fzrz_lun,ok,user)
          if (ok) ok = d0xevent(halted,user)
          call check_asynch()
          if (halt) then
            halt = .false.
            return
          endif
        enddo
c
      else if (tag.eq.3) then
c
c       turn it off
c
        dod0xuser = .false.
      else if (tag.eq.4) then
c
c       turn it on
c
        dod0xuser = .true.
c
      else if (tag.eq.5) then
c
c       clear stats
c
        nd0xtrue = 0
        nd0xfalse = 0
c
      else if (tag.eq.6) then
        nskip = nd0xtrue
        ndum = nd0xfalse
c
      else if (tag.eq.7) then
        udsttodst = .true.
      else if (tag.eq.8) then
        udsttodst = .false.
c
      else if (tag.eq.9) then
        docafix = .true.
      else if (tag.eq.10) then
        docafix = .false.
c
      else if (tag.eq.11) then
        docleanem = .true.
      else if (tag.eq.12) then
        docleanem = .false.
c
      else if (tag.eq.13) then
        docleanmu = .true.
      else if (tag.eq.14) then
        docleanmu = .false.
c
      endif
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      logical function refound(run,rec)
c
      implicit none
c
      include 'd0$xframe$source:d0map.inc'
c
      integer i,run,rec
c
      refound = .false.
      if (ntag.lt.1) return
c
      refound = .true.
      do i=1,ntag
        if (run.eq.irtag(i).and.rec.eq.ietag(i)) return
      enddo
c
      refound = .false.
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine d0xd0dad(drun,dev)
c
c     this thing calls the d0dad catalog access per event
c
      implicit none
c
      integer drun,dev

      include 'd0$xframe$source:d0map.inc'
c
      integer lun
      logical ok,user,halted,d0xevent
c
      nrun = drun
      nevo = dev
      call readit(.true.,lun,ok,user)
      if (ok) ok = d0xevent(halted,user)
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      logical function d0xevent(halted,user)
c
c     this guy will read in an event, look to see if the event needs
c     some histogramming, have d0xuser called, output, and etc.
c
c     inputs:  halted = .true.   if the d0xevent=.false. from being halted
c
      implicit none
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      logical halted,ok,user,meetit,reqmet,doout,refound,udst_to_dst
      logical cafix,cafix_begin
      logical compute_em_quality,compute_em_quality_ini
      logical compute_mu_quality,compute_mu_quality_ini
      integer nd,nuhead,iuhead(100)
c
c     preset
c
      d0xevent = .true.
      halted = .false.
c
c     do the micro-dst to dst conversion?
c
      if (udsttodst) then
        ok = udst_to_dst()
        dbhead(0) = lhead    !just in case!
      endif
c
c     compute_em_quality? 
c
      if (docleanem) then
        if (.not.cleaneminit) then
          cleaneminit = COMPUTE_EM_QUALITY_INI()
          if (.not.cleaneminit) then
            call fwarning(%ref('COMPUTE_EM_QUALITY_INI Failure'))
            return
          endif
        endif
        ok = compute_em_quality()
        dbhead(0) = lhead    !just in case!
      endif
c
c     compute_mu_quality? 
c
      if (docleanmu) then
        if (.not.cleanmuinit) then
          cleanmuinit = COMPUTE_MU_QUALITY_INI()
          if (.not.cleanmuinit) then
            call fwarning(%ref('COMPUTE_MU_QUALITY_INI Failure'))
            return
          endif
        endif
        ok = compute_mu_quality()
        dbhead(0) = lhead    !just in case!
      endif
c
c     cafix? 
c
      if (docafix) then
        if (.not.cafixinit) then
          cafixinit = cafix_begin()
          if (.not.cafixinit) then
            call fwarning(%ref('CAFIX_BEGIN Failure'))
            return
          endif
        endif
        ok = cafix()
        dbhead(0) = lhead    !just in case!
      endif
c
c     check if this event satisfies requirements criteria
c
      meetit = .false.
      if ((doreq.ne.0).and.nreqs.gt.0) meetit = reqmet()
c
c     histogram any variables based on requirement (0=hist, 1=tuples)
c
      call fxhist(0,meetit)
      call fxhist(1,meetit)
c
c     output events? note you cannot (presently) output events based on
c     an .or. or .and. between requirements/d0xuser
c
      doout = .false.
      if (iouttype.eq.1) then
c
c       output all of them
c
        doout = .true.
      else if (iouttype.eq.3) then
c
c       output only those found in the list
c
        if (refound(nrun,nevo)) then
          write(*,'('' ...Found run/event '',2i6)') nrun,nevo
          doout = .true.
        endif
      else if (iouttype.eq.5) then
c
c       output those which pass the cuts
c
        doout = meetit
c
      else if (iouttype.eq.6) then
c
c       output if user is returned .true.
c
        doout = user
      else if (iouttype.eq.7) then
c
c       write it out.  we abort down below
c
        doout = .true.
c
      endif
c
c     if doout is true, then output this one
c
      if (doout) then
        ND=min(50,IQ(LHEAD-1))
        DO NUHEAD=1,ND
          IUHEAD(NUHEAD)=IQ(LHEAD+NUHEAD)
        enddo
        if (nbdrop.gt.0) then
          if (dropkeep.eq.1) call mydrop('SAVE',nbdrop,cbdrop)
          if (dropkeep.eq.0) call mydrop('DROP',nbdrop,cbdrop)
          CALL FZOUT(85,IXMAIN,LHEAD,1,'M',2,ND,IUHEAD)
        else
          CALL FZOUT(85,IXMAIN,LHEAD,1,' ',2,ND,IUHEAD)
        endif
        nevout = nevout + 1
        call evoutc(nevout)
c
c       see if we are "counting" output
c
        if (iouttype.eq.7) then
          if (mod(nevout,nper).eq.0) d0xevent = .false.
        endif
      endif
c
c     see if we are being called
c
      call check_asynch()
      if (halt) then
        halt = .false.
        d0xevent = .false.
        halted = .true.
      endif
c
c     does requirement request processing to stop?
c
      if (reqstop.and.meetit) then
        halt = .false.
        d0xevent = .false.
        halted = .true.
      endif

c
c     that's all folks
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine cnout(dummy)
c
c     increment number of output events and change counter
c
c     dummy = 0, reset
c     dummy = 1, increment
c
      implicit none
c
      include 'd0$xframe$source:d0map.inc'
c
      integer dummy
c
      if (dummy.eq.0) then
        nevout = 0
        call evoutc(nevout)
      else
        nevout = nevout + 1
        call evoutc(nevout)
      endif
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine setd0dad(flag,iname,ilen)
c
      implicit none
c
      include 'd0$xframe$source:d0map.inc'
c
      integer flag,iname(*),ilen
c
      integer i
c
      if (flag.eq.0) then
c
c       do not grab catalog depending on run - use iname instead
c
        d0dadrdc = .false.
        call uhtoc(iname,4,d0dadcat,ilen)
        do i=ilen+1,256
          d0dadcat(i:i) = ' '
        enddo
      else
c
c       catalog will be run dependent
c
        d0dadrdc = .true.
      endif
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine iscafix(isit)
c
      include 'd0$xframe$source:d0map.inc'
c
      integer isit
c
      if (docafix) then
        isit = 1
      else
        isit = 0
      endif
dc
      return
      end
