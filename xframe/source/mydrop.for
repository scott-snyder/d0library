      SUBROUTINE MYDROP(which,n,name)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : drops banks
C-
C-   Inputs  : which = 'SAVE' or 'DROP'
C-             n=number of banks, name = c*4 list
C-   Outputs : 
C-   Controls: 
C-
C-   Created  18-JAN-1993   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
c
      include 'D0$XFRAME$SOURCE:D0MAP.INC'
c
      character*4 which,name(*)
      integer n
c
      integer ns,i,index,point,lzfidh,parent,nb,hbank(100)
      integer iname,nt
      character*1 mark/' '/
      character*4 cname
c
      equivalence (iname,cname)
c
      if (n.lt.1) return
c
      if (which.eq.'SAVE') then
        mark = ' '
      else if (which.eq.'DROP') then
        mark = '-'
      endif
c
c     here we drop/save all BUT the banks in the list
c
c     loop over list, find name of chain from HEAD down to bank
c
      nb = 0
      nt = 0
      do i=1,n
        index = 0
c
c       is this bank present?
c
  100   continue
        cname = name(i)
        point = lzfidh(ixmain,iname,index)
        if (point.ne.0) then
c
c         yes, bank is present
c
          index = point
c
c         save this one for mzmark
c
          nb = nb + 1
          nt = nt + 1
          hbank(nb) = iq(point-4)
c
c         now go up the chain till we get to head
c
          parent = lq(point+1)
  200     continue
c
c         is there a parent? if not, then probably HEAD but we 
c         will check the name to be sure
c
          if (parent.gt.0) then
            iname = iq(parent-4)
            if (cname.ne.'HEAD') then
              nb = nb + 1
c
c             ok, not the head, save this one and go up for more
c
              hbank(nb) = iname
              parent = lq(parent+1)
              goto 200
            endif
          endif
c
c         any more of these? 
c
          goto 100
        endif
      enddo
c
c     anything ?
c
      if (nt.eq.0) then
        cname = 'HEAD'
        call mzmark(ixcom,lhead,mark,1,iname)
      else
c
c       mark them 
c
        call mzmark(ixcom,lhead,mark,nb,hbank)
      endif
c
c     that's all folks
c
      return
      end
