      SUBROUTINE BLOCAT(main,bank,pointer)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to bank with name "BANK"
C-
C-   Inputs  : main - zebra divistion
C-             bank - bank name
C-             pointer - returned
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      INCLUDE 'D0$INC:ZEBQ.INC'
      include 'd0$inc:mzca.inc'
      include 'd0$inc:mzcb.inc'
c
      integer main,pointer
      character*(*) bank
c
      integer hbank,lzfidh,lbank,kbank,ns,i
      character*4 new_name,bankc
      integer inew_name
c
      equivalence (new_name,inew_name)
c
c     convert to uppercase hollerith (he's the guy who invented IBM)
c     and get the pointer
c
      bankc = bank
      call str$upcase(bankc,bankc)
      CALL UCTOH(BANKC,HBANK,4,4)
      pointer = LZFIDH(MAIN,HBANK,0)
c
c     if the pointer is non-zero, then this is the last bank in
c     the chain (if the chain is >1) - if it IS in a chain, get to
c     the first bank
c
      if (pointer.le.0) return
      if (lqq(pointer+kqs+1).le.0) return
c
c     go upwards (lqq(pointer+1)) to the origin bank
c
      lbank = lqq(pointer+kqs+1)
c
c     loop over pointers, looking for bank with same name as "BANK"
c
      ns = iqq(lbank+kqs-2)
      if (ns.lt.1) return
      do i=ns,1,-1
        kbank = lqq(lbank+kqs-i)
        inew_name = iqq(kbank+kqs-4)
        if (new_name(1:4).eq.bankc(1:4)) then
c
c         ok, we're back....this is the 1st in the chain we want
c
          pointer = kbank
          return
        endif
      enddo
c
c     if we've gotten to here - something's fishy...
c
      call xerrmsg('SEVERE CONFUSION IN BLOCAT!!!!')
c
      return
      end
