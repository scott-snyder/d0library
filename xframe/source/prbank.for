      SUBROUTINE PRBANK(bank,addr,lun)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
c     dumps out bank info for bank "BANK" via prBANK routines
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'd0$xframe$source:d0map.inc'
c
      character*4 bank
      integer lun,addr
c
      character*80 prt
      integer i,names,nrow,extra,lzfidh,ipt
      parameter (names=75)
      character*4 banks(names),cfl
c
      data banks/
     &  'CACL','CAD1','CAD2','CADT','CAEH','CAEP','CAPH','CASH',
     &  'CATD','CATE','CDD1','CDD2','CDD3','CDD4','DTRH','DTRK',
     &  'ERMG','ESUM','FDCT','FILT','FRES','FTRH','HEAD','HITS',
     &  'HMTE','HMTP','HSTR','ISAC','ISAE','ISAJ','ISAL','ISAM',
     &  'ISAQ','ISAX','ISCL','ISJT','ISP1','ISP2','ISP3','ISRC',
     &  'ISV1','ISV2','JAUX','JETS','JNEP','JPTS','JTSH','L2EM',
     &  'MTRH','MUD1','MUOH','MUOT','PARH','PELC','PJET','PJHD','PJPT',
     &  'PLV0','PMUO','PNUT','PPHO','PTAU','RECO','TPRL',
     &  'TRDT','TRGR','TSUM','TTRH','VERH','VERT','VTRH','VTXT',
     &  'ZFIT','ZTRH','ZTRK'/
c
      call str$upcase(bank,bank)
c
c     check that bank is here
c
      call uctoh(bank,i,4,4)
      if (lzfidh(dbdiv(pstore),i,0).lt.1) then
        write(prt,'('' Bank '',a4,'' NOT PRESENT'')') bank
        call xerrmsg(prt)
        return
      endif
c
c     check "addr" - if it's 0, then dump all banks, else dump the
c     the bank at that address (assume that "bank" is still ok so you 
c     don't have to dig out the bank name from the address)
c
      if (addr.eq.0) then
        ipt = 0
        cfl = 'ALL'
      else
        ipt = addr
        cfl = 'ONE'
      endif
c
c     check against known list:
c
      if (bank.eq.'CACL') then
        call PRCACL(lun,ipt,0,cfl,0)
      else if (bank.eq.'CAD1') then
        call PRCAD1(lun,ipt,0,cfl,1)
      else if (bank.eq.'CAD2') then
        call PRCAD2(lun,ipt,0,cfl,1)
      else if (bank.eq.'CADT') then
        call PRCADT(lun,ipt,0,cfl,0)
      else if (bank.eq.'CAEH') then
        call PRCAEH(lun,ipt,0,cfl,0)
      else if (bank.eq.'CAEP') then
        call PRCAEP(lun,ipt,0,cfl,1)
      else if (bank.eq.'CAPH') then
        call PRCAPH(lun,ipt,0,cfl,0)
      else if (bank.eq.'CASH') then
        call PRCASH(lun,ipt,0,cfl,0)
      else if (bank.eq.'CATD') then
        call PRCATD(lun,ipt,0,cfl,0)
      else if (bank.eq.'CATE') then
        call PRCATE(lun,ipt,0,cfl,0)
      else if (bank.eq.'CDD1') then
        call PRCDD1(lun,ipt,0,cfl,3)
      else if (bank.eq.'CDD2') then
        call PRCDD2(lun,ipt,0,cfl,3)
      else if (bank.eq.'CDD3') then
        call PRCDD3(lun,ipt,0,cfl,3)
      else if (bank.eq.'CDD4') then
        call PRCDD4(lun,ipt,0,cfl,3)
      else if (bank.eq.'DTRH') then
cccc        call PRDTRH(lun,ipt,0,cfl,6)
        write(*,'('' ....PRDTRH not written yet....'')')
      else if (bank.eq.'DTRK') then
        call PRDTRK(lun,ipt,0,cfl,6)
      else if (bank.eq.'ERMG') then
        call PRERMG(lun,ipt,0,cfl,0)
      else if (bank.eq.'ESUM') then
        call PRESUM(lun,ipt,0,cfl,0)
      else if (bank.eq.'FDCT') then
        call PRFDCT(lun,ipt,0,cfl,3)
      else if (bank.eq.'FILT') then
        call PRFILT(lun,ipt,0,cfl,0)
      else if (bank.eq.'FRES') then
        call PRFRES(lun,ipt,0,cfl,2)
      else if (bank.eq.'FTRH') then
CCCC        call PRFTRH(lun,ipt,0,cfl,0)
        write(*,'('' ....PRFTRH not written yet....'')')
      else if (bank.eq.'HEAD') then
        call PRHEAD(lun,ipt,0,cfl,0)
      else if (bank.eq.'HITS') then
        call PRHITS(lun,ipt,0,cfl,0)
      else if (bank.eq.'HMTE') then
        call PRHMTE(lun,ipt,0,cfl,0)
      else if (bank.eq.'HMTP') then
        call PRHMTP(lun,ipt,0,cfl,0)
      else if (bank.eq.'HSTR') then
        call PRHSTR(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISAC') then
        call PRISAC(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISAE') then
        call PRISAE(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISAJ') then
        call PRISAJ(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISAL') then
        call PRISAL(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISAM') then
        call PRISAM(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISAQ') then
        call PRISAQ(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISAX') then
        call PRTEVZ(lun)
      else if (bank.eq.'ISCL') then
        call PRISCL(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISJT') then
        call PRISJT(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISP1') then
        call PRISP1(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISP2') then
        call PRISP2(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISP3') then
        call PRISP3(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISP3') then
        call PRISP3(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISRC') then
        call PRISRC(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISV1') then
        call PRISV1(lun,ipt,0,cfl,0)
      else if (bank.eq.'ISV2') then
        call PRISV2(lun,ipt,0,cfl,0)
      else if (bank.eq.'JAUX') then
        call PRJAUX(lun,ipt,0,cfl,0)
      else if (bank.eq.'JETS') then
        call PRJETS(lun,ipt,0,cfl,1)
      else if (bank.eq.'JNEP') then
        call PRJNEP(lun,ipt,0,cfl,0)
      else if (bank.eq.'JPTS') then
        call PRJPTS(lun,ipt,0,cfl,0)
      else if (bank.eq.'JTSH') then
        call PRJTSH(lun,ipt,0,cfl,0)
      else if (bank.eq.'L2EM') then
        call PRL2EM(lun,ipt,0,cfl,0)
      else if (bank.eq.'MTRH') then
        call PRMTRH(lun,ipt,0,cfl,0)
      else if (bank.eq.'MUD1') then
        call PRMUD1(lun,ipt,0,cfl,0)
      else if (bank.eq.'MUOT') then
        call PRMUOT(lun,ipt,0,cfl,0)
      else if (bank.eq.'MUOH') then
        call PRMUOH(lun,ipt,0,cfl,0)
      else if (bank.eq.'PARH') then
        call PRPARH(lun,ipt,0,cfl,0)
      else if (bank.eq.'PELC') then
        call PRPELC(lun,ipt,0,cfl,0)
      else if (bank.eq.'PJET') then
        call PRPJET(lun,ipt,0,cfl,0)
      else if (bank.eq.'PJHD') then
        call PRPJHD(lun,ipt,0,cfl,0)
      else if (bank.eq.'PJPT') then
CCCC        call PRPJPT(lun,ipt,0,cfl,0)
        write(*,'('' ....PRJPT not written yet....'')')
      else if (bank.eq.'PLV0') then
        write(*,'('' ....PRLV0 not written yet....'')')
CCCC        call PRPLV0(lun,ipt,0,cfl,0)
      else if (bank.eq.'PMUO') then
        call PRPMUO(lun,ipt,0,cfl,0)
      else if (bank.eq.'PNUT') then
        call PRPNUT(lun,ipt,0,cfl,1)
      else if (bank.eq.'PPHO') then
        call PRPPHO(lun,ipt,0,cfl,0)
      else if (bank.eq.'PTAU') then
        call PRPTAU(lun,ipt,0,cfl,0)
cccc      else if (bank.eq.'PVES') then
cccc        call PRPVES(lun,ipt,0,cfl,0)
      else if (bank.eq.'RECO') then
        call PRRECO(lun,ipt,0,cfl,0)
      else if (bank.eq.'TPRL') then
        call PRTPRL(lun,ipt,0,cfl,0)
      else if (bank.eq.'TRDT') then
        call PRTRDT(lun,ipt,0,cfl,0)
      else if (bank.eq.'TRGR') then
        call PRTRGR(lun,ipt,0,cfl,0)
      else if (bank.eq.'TSUM') then
        call PRTSUM(lun,ipt,0,cfl,0)
      else if (bank.eq.'TTRH') then
        call PRTTRH(lun,ipt,0,cfl,0)
      else if (bank.eq.'VERH') then
        call PRVERH(lun,ipt,0,cfl,0)
      else if (bank.eq.'VERT') then
        call PRVERT(lun,ipt,0,cfl,0)
      else if (bank.eq.'VTRH') then
        call PRVTRH(lun,ipt,0,cfl,0)
        call PRVTRH(lun,ipt,0,'SUM',0)
      else if (bank.eq.'VTXT') then
        call PRVTXT(lun,ipt,0,cfl,0)
      else if (bank.eq.'ZFIT') then
        call PRZFIT(lun,ipt,0,cfl,0)
      else if (bank.eq.'ZTRH') then
        write(*,'('' ....PRZTRH not written yet....'')')
cccc        call PRZTRH(lun,ipt,0,cfl,0)
      else if (bank.eq.'ZTRK') then
        call PRZTRK(lun,ipt,0,cfl,0)
      else
        nrow = names/8
        extra = names - 8*nrow
        write(prt,'(''('',i2,''(10x,8(3x,a4),/),10x,'',i1,
     &    ''(3x,a4))'')') nrow,extra
        write(*,'('' Printout for only the following banks: '')')
        write(*,prt) (banks(i),i=1,names)
        write(*,'('' ==> e-mail requests to FNALD0::DREW'')')
      endif
c
      return
      end
