      SUBROUTINE FRAWDATA(tag,value,itemp,ncol)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : tag=0, go to absolute, 1=offset
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   4-OCT-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer tag,value,itemp,ncol
c
      integer maxcrate
      parameter (maxcrate = 1000)
      integer ibank,curpnt,crate,i,idum
      integer ncrates,icrates(maxcrate),ichans(maxcrate)
      character*4 bank
      character*80 msg
c
      equivalence (ibank,bank)
c
c     bank name?
c
      ibank = itemp
c
c     only allow raw data banks
c
      if (bank.ne.'MUD1'.and.bank.ne.'TRGR'.and.bank.ne.'CDD1'.and.
     &    bank.ne.'CDD2'.and.bank.ne.'CDD3'.and.bank.ne.'CDD4'.and.
     &    bank.ne.'CAD1'.and.bank.ne.'CAD2') then
        return
      endif
      msg = 'Status: OK'//char(0)
      call rawstat(%ref(msg))
c
c     test on tag
c
      if (tag.eq.0) then
c
c       changing number of columns....nothing to do
c
      else if (tag.eq.1) then
c
c       go to 1st crate - note that getcrate returns them
c       in reverse order
c
        call getcrate(bank,maxcrate,ncrates,icrates,ichans)
        if (ncrates.lt.1) then
          msg = 'Status: No Crates Found!'//char(0)
          call rawstat(%ref(msg))
          return
        endif
c
c       default:  do first one
c
        curpnt = ncrates
        crate = icrates(curpnt)
c
      else if (tag.eq.2) then
c
c       goto absolute crate number
c
        if (ncrates.lt.1) then
          msg = 'Status: No Crates Found!'//char(0)
          call rawstat(%ref(msg))
          return
        endif
        crate = value
c
c       find the pointer to this crate (should exist)
c
        idum = curpnt
        curpnt = 0
        do i=1,ncrates
          if (icrates(i).eq.crate) then
            curpnt = i
          endif
        enddo
c
c       no pointer - reset and return
c
        if (curpnt.eq.0) then
          msg = 'Status: Crate Not Found'//char(0)
          call rawstat(%ref(msg))
          curpnt = idum
          return
        endif
c
      else if (tag.eq.3) then
c
c       goto next crate
c
        if (ncrates.lt.1) then
          call rawstat(%ref('Status: No Crates Found!'),%val(24))
          return
        endif
        curpnt = curpnt - 1
        if (curpnt.eq.0) curpnt = ncrates
        crate = icrates(curpnt)
c
      endif
c
c     display the crate information
c
      call xcrate(bank,crate,ichans(curpnt),ichans,icrates,ncrates,ncol)
c
      return
      end
