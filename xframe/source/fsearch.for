      SUBROUTINE FSEARCH(tag,temp)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : handles the "search" buttons
c     tag = 0, by run number
c           1, by event number
c           2, by bank name
C-
C-   Inputs  : tag - from widget  temp - pointer to c char
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer tag,temp
c
      character*80 msg,tname,name(100)
      character*4 cbank,fbank
      integer len,ibank,tlen,pointer,length,bits(100),n1,n2,i
      logical forever,ok
c
      equivalence (ibank,cbank)
c
c     make sure file is opened
c
      if (.not.fzrz_opened) then
        call xerrmsg('Nothing read in yet...')
        return
      endif
c
      ibank = temp
      if (tag.eq.0) then
c
c       search by run number
c
        do while (nrun.ne.temp)
          call readit(fzrz_lun,ok)
          if (.not.ok) goto 111
          call check_asynch()
          if (halt) then
            halt = .false.
            return
          endif
        enddo
c
      else if (tag.eq.1) then
c
c       search by event number
c
        do while (nevo.ne.temp)
          call readit(fzrz_lun,ok)
          if (.not.ok) goto 111
          call check_asynch()
          if (halt) then
            halt = .false.
            return
          endif
        enddo
c
      else if (tag.eq.2) then
c
c       search by bank name
c
        forever = .true.
        call xerrmsg('Searching for bank "'//cbank(1:4)//'"')
        do while (forever)
          call blocat(ixmain,cbank,pointer)
          if (pointer.ne.0) goto 999
          call readit(fzrz_lun,ok)
          if (.not.ok) goto 111
          call check_asynch()
          if (halt) then
            halt = .false.
            return
          endif
        enddo
c
      else if (tag.eq.3) then
c
c       search by l1 trigger name
c
        call cfchar(0,temp,tname,length)
        forever = .true.
        call xerrmsg('Searching for L1 Trigger "'//tname(1:length)//'"')
        do while (forever)
          call readit(fzrz_lun,ok)
          if (.not.ok) goto 111
          call level1(n1,bits,name)
          if (n1.gt.0) then
            do i=1,n1
              if (tname(1:length).eq.name(i)(1:length)) then
                goto 999
              endif
            enddo
          endif
          call check_asynch()
          if (halt) then
            halt = .false.
            return
          endif
        enddo
c
      else if (tag.eq.4) then
c
c       search by l2 trigger name
c
        call cfchar(0,temp,tname,length)
        forever = .true.
        call xerrmsg('Searching for L2 Trigger "'//tname(1:length)//'"')
        do while (forever)
          call readit(fzrz_lun,ok)
          if (.not.ok) goto 111
          call level2(n1,bits,name)
          if (n1.gt.0) then
            do i=1,n1
              if (tname(1:length).eq.name(i)(1:length)) then
                goto 999
              endif
            enddo
          endif
          call check_asynch()
          if (halt) then
            halt = .false.
            return
          endif
        enddo
c
      endif
c
  999 continue
      write(msg,'(''REQUEST SATISFIED FOR Run/Event '',2I9)')
     &  nrun,nevo
      call xerrmsg(msg)
      return
  111 continue
      call xerrmsg('READ FAILURE OR EOF REACHED')
      return
      end
