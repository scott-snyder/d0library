      SUBROUTINE FSET_STORE(mode)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sets store for xdbank:  0=com 1=stp, 2=geant, 3=wrk
C-
C-   Inputs  : mode
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
      INCLUDE 'D0$INC:MZCB.INC'
c
C-- The ZEBWRK bank is being handled thusly so as to avoid a big
C-- space allocation in jobs that do not use the common block.
      COMMON/ZEBWRK/IXWRK,IDVWRK
      INTEGER IXWRK,IDVWRK
C--------------The bank below has had its variables renamed so
C--------------as not to conflict with ZEBCOM.
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(9000)
      EQUIVALENCE (QG(1),IQG(1),LQG(9)),(LQG(1),LMAIN)
C
      INTEGER   NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,IQG(1),LQG(8000)
      REAL      GVERSN,ZVERSN,FENDQ,WS,QG(1)
c
      integer mode,zdiv,jbyt
c
      pstore = mode
c
c     get store addresses
c
      if (pstore.eq.0) then
        dbstore(pstore) = ixcom
        dbdiv(pstore) = ixmain
        dbname(pstore) = 'ZEBCOM  '
      else if (pstore.eq.1) then
        dbstore(pstore) = ixstp
        dbdiv(pstore) = idvstp
        dbname(pstore) = 'ZEBSTP  '
      else if (pstore.eq.2) then
        dbstore(pstore) = ixstor
        dbname(pstore) = 'GEANT   '
        call getpar(1,' Division IXDIV(0) OR IXCONS(1) > ','I',zdiv)
        if (zdiv.eq.0) then
          dbdiv(pstore) = ixdiv
        else
          dbdiv(pstore) = ixcons
        endif
      else if (pstore.eq.3) then
        dbname(pstore) = 'ZEBWRK  '
        dbstore(pstore) = ixwrk
        dbdiv(pstore) = idvwrk
c
      else if (pstore.eq.4) then
        d0xdisk = .true.
      else if (pstore.eq.5) then
        d0xdisk = .false.
      endif
c
c     ZEBRA INTERNAL ROUTINE TO SWITCH STORES
c
      if (pstore.lt.4) then
        if (jbyt(dbstore(pstore),27,6).ne.jqstor)
     &  call mzsdiv (dbstore(pstore),-7)
      endif
c
      return
      end
