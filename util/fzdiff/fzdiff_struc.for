      integer function fzdiff_struc(l1, l2, lmin)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find the first filled down link from a pair
C-                         of banks.
C-
C-   Returned value  : Index of down link (zero if non available).
C-   Inputs  : l1   - Link of first bank.
C-             l2   - Link of second bank.
C-             lmin - Lower limit of returned link index.
C-
C-   Created   8-Sep-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:zebcom.inc'
      integer l1, l2, lmin
      integer i
      integer nl                  ! Number of structural links
C----------------------------------------------------------------------
      nl = min0(iq(l1-2), iq(l2-2))
      do 10 i = lmin+1, nl
        if(lq(l1-i).ne.0 .and. lq(l2-i).ne.0)then
          fzdiff_struc = i
          go to 999
        endif
 10   continue
      fzdiff_struc = nl+1
  999 return
      end
