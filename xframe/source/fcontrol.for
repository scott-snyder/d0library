      SUBROUTINE FCONTROL(tag,nskip)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
c     tag = 0, read next
c           1, read till EOF
c           2, read next "nskip" records
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
      integer tag,nskip
c
      character*80 msg
      integer iskip,jrun,jev
      logical ok,refound
      integer status,ilen,nd,nuhead,iuhead(50)
c
c     NOT for STP files!
c
      if (file_type.eq.1) then
        call xerrmsg('STP File Record Already In Memory')
        return
      endif
c
c     speficied?
c
      if (.not.fzrz_spec) then
        call xerrmsg('STP File NOT SPECIFIED')
        return
      endif
c
      if (tag.eq.0) then
c
c       read next record
c
        call readit(fzrz_lun,ok)
      else if (tag.eq.1) then
c
c       read till EOF (SCAN)
c
        ok = .true.
c
c         output from list?
c
        do while (ok)
c
c         read in next event
c
          call readit(fzrz_lun,ok)
c
c         output all events?
c
          if (ok.and.(iouttype.gt.0.and.iouttype.lt.3)) then
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
          endif
c
c         output from list?
c
          if (ok.and.(iouttype.gt.2.and.iouttype.lt.5)) then
            if (refound(nrun,nevo)) then
              write(*,'('' ...Found run/event '',2i6)') nrun,nevo
              ND=min(50,IQ(LHEAD-1))
              DO NUHEAD=1,ND
                IUHEAD(NUHEAD)=IQ(LHEAD+NUHEAD)
              enddo
              if (nbdrop.gt.0) then
                call mydrop('SAVE',nbdrop,cbdrop)
                CALL FZOUT(85,IXMAIN,LHEAD,1,'M',2,ND,IUHEAD)
              else
                CALL FZOUT(85,IXMAIN,LHEAD,1,' ',2,ND,IUHEAD)
              endif
            endif
          endif
c
c         see if we are being called
c
          call check_asynch()
          if (halt) then
            halt = .false.
            ok = .false.
          endif
        enddo
      else if (tag.eq.2) then
c
c       read next "nskip" records
c
        if (nskip.eq.0) return
        if (nskip.lt.1) then
          write(msg,'('' ILLEGAL "NSKIP" SPECIFICATION '',I8)') NSKIP
          CALL xerrmsg(msg)
          return
        endif
        do iskip=1,nskip
          call readit(fzrz_lun,ok)
          call check_asynch()
          if (halt) then
            halt = .false.
            return
          endif
        enddo
      endif
c
      return
      end
ccccccccccccccccccccccccccc
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
