      subroutine fzdiff_bkname(l, bkname, lbkname)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generate a full character pathname for a 
C-    given bank.
C-
C-   Inputs  : l       - Link of bank
C-   Outputs : bkname  - Full character pathname
C-             lbkname - Length of pathname
C-
C-   Created   8-Sep-1992   Herbert Greenlee
C-
C----------------------------------------------------------------------
      implicit none
      include 'd0$inc:zebcom.inc'
      integer l
      character*(*) bkname
      integer lbkname
      integer llin                ! Link of first bank in linear st.
      integer nlin                ! Number of banks in linear st.
      integer n                   ! Number of current bank in lin. st.
      integer ltemp
      integer stlink              ! Number of structural link
      character*256 ctemp
      character*32 bktemp
C-
C- Functions
C-
      integer nzbank, trulen, lzhead
      integer fzdiff_stlink
      character*256 spaces
C----------------------------------------------------------------------
      bkname = ' '
      ltemp = l
C-
C- Pathname loop
C-
 100  continue
C-
C- Get address of head of linear structure.  Then get total number of banks
C- in linear structure and number of current bank.
C-
      llin = lzhead(ixcom, ltemp)
      if(llin.eq.0)then
        nlin = 0
      else
        nlin = nzbank(ixcom, llin)
      endif
      n = nlin + 1 - nzbank(ixcom, ltemp)
C-
C- Get number of structural link of current bank.
C-
      stlink = fzdiff_stlink(ltemp)
C-
C- Construct name of current bank.  Include number of structural link and 
C- position in linear structure, if relevant.
C-
      if(n.eq.1 .and. nlin.eq.1)then
        write(bktemp,220)iq(ltemp-4)
 220    format(a4)
      else
        write(bktemp,221)iq(ltemp-4),n,nlin
 221    format(a4,'(',i6,':',i6,')')
      endif
      if(stlink.lt.0)then
        ctemp = bktemp
        write(bktemp,222)stlink, ctemp(1:19)
 222    format(i7,':',a19)
      endif
C-
C- Add name of current bank to pathname
C-
      if(bkname.eq.' ')then
        ctemp = bktemp
      else
        ctemp = bktemp(1:trulen(bktemp))//'|'//bkname
      endif
      bkname = spaces(ctemp, 0)
C-
C- Go up, if possible
C-
      ltemp = lq(ltemp+1)
      if(ltemp.ne.0)go to 100
      lbkname = trulen(bkname)
  999 return
      end
