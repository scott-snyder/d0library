      SUBROUTINE READIT(d0dadtype,lun,ok,user)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : my own version which calls D0 utilities
C-   and prints out stuff
C-
C-   Inputs  :  d0dadtype = .true. do d0dad catalog read, otherwise use
C-              evtrd
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer lun,ios,ier
      logical ok, d0xuser,user,d0dadtype
      character*80 msg
c
  100 continue
      if (d0dadtype) then
        call evtcat_read_event(nrun,nevo,ier)
        if (ier.ne.0) then
          if (ier.eq.-1) then
            call xerrmsg('(-1) Cannot find D0DAD Catalog File')
            call fwarning(%ref('(-1) Cannot find D0DAD Catalog File'))
          else if (ier.eq.-2) then
            call xerrmsg('(-2) Cannot open D0DAD Catalog File')
            call fwarning(%ref('(-2) Cannot open D0DAD Catalog File'))
          else if (ier.eq.-3) then
            call xerrmsg(
     &        '(-3) Run/Event pair not found in D0DAD Catalog File')
            call fwarning(%ref(
     &        '(-3) Run/Event pair not found in D0DAD Catalog File'))
          else if (ier.eq.-4) then
            call xerrmsg('(-4) Error in reading D0DAD Catalog File')
            call fwarning(
     &        %ref('(-4) Error in reading D0DAD Catalog File'))
          else if (ier.eq.-5) then
            call xerrmsg('(-5) Error in D0DAD_READEVENT')
            call fwarning(%ref('(-5) Error in D0DAD_READEVENT'))
          else if (ier.eq.-6) then
            call xerrmsg('(-6) Error opening D0DAD Catalog File')
            call fwarning(%ref('(-6) Error opening D0DAD Catalog File'))
          endif
          return
        endif
        ios = 0
      else
        CALL EVTRD(lun,IOS)  ! read events from file
      endif
c
      ok = .true.
      nrun = iq(lhead+6)
      nevo = iq(lhead+9)
      nevi1 = iq(lhead+7)
      nevi2 = iq(lhead+8)
      grun = iq(lhead+12)
      dbhead(pstore) = lhead
      IF(IOS.EQ.0) THEN
c
c       call user routine, can set ok for future filtering
c
        if (dod0xuser) then
          user = d0xuser(0)
          if (user) then
            nd0xtrue = nd0xtrue + 1
          else
            nd0xfalse = nd0xfalse + 1
          endif
        else
          user = .false.
        endif
c
        write(msg,'(
     &    '' EVENT: Run# '',I8,'' Event# '',I8,'' D0XUSER '',L1)')
     &    nrun,nevo,user
        call xerrmsg(' *** Event record found...')
        call xerrmsg(msg)
C
C           Begin run record
      ELSE IF(IOS.EQ.1) THEN
        call xerrmsg(' *** Begin run record found...')
C
C           End run record
      ELSE IF(IOS.EQ.2) THEN ! ENDRUN
c
c        these are "obsolete" - read again (I hope this doesn't
c        cause any trouble....)
c
        go to 100
c        ok = .false.
c        call xerrmsg(' *** End run record found...')
C
C         end-of-file
      ELSE IF(IOS.GT.2) THEN
        call xerrmsg(' *** End-of-file reached...closing...')
        call fzendi(lun,'TQ')
        call rlunit(d0xuserunit,lun,ier)
        if (ier.ne.0) then
          call fwarning(%ref('READIT Error releasing LUN'))
        endif
        close(lun)
        ok = .false.
        fzrz_opened = .false.
C
      ENDIF
c
      return
      end
