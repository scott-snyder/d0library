      SUBROUTINE FBLIST(mode,iname,result)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sets up list of banks allowed to display 
C-   in tree widget (easier to do in fortran)
C-
C-   Inputs  : mode: 0   reset
C-                   1   add
C-                  -1   check
C-                  -2   add if not there, otherwise subtract
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-JAN-1993   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      integer mode,iname,result
c
      integer n2,nallowed/0/
      character*4 callowed(1000)  !whew
      character*4 callowed2(1000)  !whew
c
      integer i,ibank
      character*4 cbank
c
      equivalence (ibank,cbank)
c
      ibank = iname
c
      if (mode.eq.-2) then
c
        result = 0
        do i=1,nallowed
          if (cbank.eq.callowed(i)) result = 1
        enddo
        if (result.eq.0) then
c
c         bank not present, add it
c
          nallowed = nallowed + 1
          callowed(nallowed) = cbank
        else
c
c         bank present, subtract it
c
          n2 = 0
          do i=1,nallowed
            if (cbank.ne.callowed(i)) then
              n2 = n2 + 1
              callowed2(n2) = callowed(i)
            endif
          enddo
          nallowed = n2
          do i=1,nallowed
            callowed(i) = callowed2(i)
          enddo
        endif
      else if (mode.eq.-1) then
        result = 0
        do i=1,nallowed
          if (cbank.eq.callowed(i)) result = 1
        enddo
c
      else if (mode.eq.0) then
        nallowed = 0
c
      else if (mode.eq.1) then
        nallowed = nallowed + 1
        callowed(nallowed) = cbank
c
      endif
c
      return
      end
c
      subroutine fbankok(this,parent,ok,st)
c
c     queries the list above, returns ok=0 if not ok, 1 if ok
c     this is this bank name, parent is the one above, if either is
c     HEAD/STPH then it's automatically ok, otherwise check list
c
      implicit none
c
      include 'D0$XFRAME$SOURCE:D0MAP.INC'
c
      integer this,parent,ok,st
c
      integer iname
      character*4 cname
c
      equivalence (iname,cname)
c
      ok = 1
      st = tstate
      iname = this
      if (file_type.eq.0.and.cname.eq.'HEAD') return
      if (file_type.eq.1.and.cname.eq.'STPH') return
      iname = parent
      if (file_type.eq.0.and.cname.eq.'HEAD') return
      if (file_type.eq.1.and.cname.eq.'STPH') return
c      
      call fblist(-1,parent,ok)
c
      return
      end
