      integer function fzdiff_stlink(l)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the structural link of the specified 
C-                         bank.
C-
C-   Returned value  : Number of structural link (negative or zero if 
C-                     standalone or top level).
C-   Inputs  : l - Link of bank.
C-
C-   Created   14-Oct-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:zebcom.inc'
      integer i, l, llin, lup, ns
      integer lzhead
C----------------------------------------------------------------------
      fzdiff_stlink = 0
C-
C- Get up link
C-
      lup = lq(l+1)
      if(lup.eq.0)go to 999
C-
C- Get link of head of linear structure
C-
      llin = lzhead(ixcom, l)
      if(llin.eq.0)llin = l
C-
C- Get the number of structural links from the supporting bank
C-
      ns = iq(lup-2)
      if(ns.le.0)go to 999
C-
C- Find the matching down link.
C-
      do 10 i=1,ns
        if(lq(lup-i).eq.llin)then
          fzdiff_stlink = -i
          go to 999
        endif
 10   continue
  999 return
      end
