      subroutine fzeventdis(lh)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display the bank structure of an event.
C-
C-   Inputs  : lh - Link of header bank.
C-
C-   Created   19-Aug-1993   Herbert Greenlee
C-
C-   Notes:
C-
C-   1.  This routine is based on a tree-walking algorithm that makes use
C-       if the following 4 motions, which are attempted in the order
C-       given below.
C-
C-       a) Down via the first unused structural link.
C-
C-       b) Next bank in linear structure via next link.
C-
C-       c) Up via up link then a) or b).
C-
C-       Tree walking finishes when no motions are possible (i.e. because 
C-       we have returned returned to the header).
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:zebcom.inc'
      integer lh
      integer i
C-
C- Tree-walking variables.
C-
      integer l                   ! Current bank
      integer maxlevel            ! Maximum tree level
      parameter (maxlevel=100)
      integer level               ! Current tree level (1 = header).
      integer nstruc(maxlevel)    ! Number of structural links descending
                                  ! from current banks.
      integer il(maxlevel)        ! Current structural link.
      integer nxt                 ! Next links from current banks
C-
C- Bank name variables.
C-
      character*256 bank          ! Full bank pathname
      integer lbank               ! Number of characters in bank pathname
      integer nl, ns, nd, nio, ntot
C----------------------------------------------------------------------
      level = 1
      l = lh
C-
C- Top of tree walking loop.
C-
      ntot = 0
 100  continue
C-
C- Print information about current bank.
C-
      nl = iq(l-3)
      ns = iq(l-2)
      nd = iq(l-1)
      nio = iand(ishft(iq(l),-18),15)
      ntot = ntot + nl + nd + nio + 10
      call fzdiff_bkname(l, bank, lbank)
      print 101, nl, nd, nio, ntot, bank(1:lbank)
 101  format(1x,'NL=',i3,', ND=',i6,', NIO=',i2, ', TOTAL=',i6,4x,a)
C-
C- Obtain number of structural links.
C-
      nstruc(level) = iq(l-2)
      il(level) = 0
C-
C- Here we attempt down motion via the first available structural link.
C-
 200  continue
      if(nstruc(level).eq.0)go to 300
      do i = il(level)+1 , nstruc(level)
        if(lq(l-i).gt.0)go to 201
      enddo
 201  continue
      il(level) = i
      if(il(level).gt.nstruc(level))go to 300
      if(level.ge.maxlevel)then
        call errmsg('fzdiff', 'fzdiff_flat', 
     &    'Maximum level exceeded' ,'W')
        go to 300
      endif
      l = lq(l-il(level))
      level = level + 1
      go to 100
C-
C- Here we attempt linear motion.
C-
 300  continue
      nxt = lq(l)
      if(nxt.eq.0)go to 400
      l = nxt
      go to 100
C-
C- Here we attempt up motion.
C-
 400  continue
      if(level.le.1)go to 999
      l = lq(l+1)
      if(l.eq.0)call zfatal
      level = level - 1
      go to 200
  999 return
      end
