      SUBROUTINE XCRATE(bank,crate,nchans,chans,clist,ctot,ncol)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : for c*4 bank, get header, raw, and
C-   trailer info for crate and shove into appropriate text widgets
C-
C-   Inputs  : bank is the bank name (e.g. MUD1), crate is the crate
C-             of choice, ctot crates are present with clist/chans
C-             being the list and #chan per crate.
C-             nchans is current number of channels
C-             ncol (number of columns, or numbers, per row)
C-   Outputs :
C-   Controls:
C-
C-   Created   4-OCT-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer nchans,crate,ncol,chans(*),clist(*),ctot
      character*4 bank
c
      integer address,gzfind_crate,lbank
      integer header_pointer,header_length,
     &  data_pointer,data_length,trailer_pointer,trailer_length
      character*80 msg
c
c     assume that bank and crate are legal - no error checking,
c     should have done this before getting here
c
      call blocat(dbdiv(0),bank,lbank)
      if (lbank.lt.1) then
        msg = 'Status: Bank not found'//char(0)
        call rawstat(%ref(msg))
        return
      endif
c
      address = gzfind_crate(bank,lbank,crate)
      if (address.eq.0) then
        msg = 'Status: Crate not found'//char(0)
        call rawstat(%ref(msg))
        return
      else if (address.eq.-1) then
        msg = 'Status: Incorrect BANK link argument'//char(0)
        call rawstat(%ref(msg))
        return
      else if (address.eq.-2) then
        msg = 'Status: Bad Bank length'//char(0)
        call rawstat(%ref(msg))
        return
      else if (address.eq.-3) then
        msg = 'Status: Invalid Crate ID (corruptd data?)'//char(0)
        call rawstat(%ref(msg))
        return
      else if (address.eq.-4) then
        msg = 'Status: Invalide Crate length'//char(0)
        call rawstat(%ref(msg))
        return
      endif
c
c     ok, everything is hunkey dorey
c
      header_pointer = address
      header_length = iq(header_pointer) + 1
      data_pointer = header_pointer + header_length
      data_length = nchans
      trailer_pointer = data_pointer + data_length
      trailer_length = 4
      call ccrate(crate,chans,clist,ctot,ncol,
     &      iq(header_pointer),header_length,
     &      iq(trailer_pointer),trailer_length,
     &      iq(data_pointer),data_length)
c
      return
      END
