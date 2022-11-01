      SUBROUTINE FGOBJ(threshold,use_ecor,qjcone,qjcul,qjzsp,qcdcorr,
     &  dojnep,nobj,maxobj,baddr,
     &  do_ele,do_gam,do_muo,do_jet,do_tau,do_met,
     &  type,et,eta,phi,mass,e,px,py,pz,pt,theta,emf,rjet,mt,
     &  nvert,zvert,dzvert,irun,iev)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns object info (pelc, ppho, muon, jets)
C-
C-   NOTE:  if path is set to ISAE then we fetch ISAQ and ISAJ (in the future)
C-
C-   Inputs  : lots
C-   Outputs : obj info
C-   Controls:
C-
C-   Created  16-DEC-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'd0$xframe$source:d0map.inc'
c
      integer use_ecor,dojnep,nobj,maxobj,type(*),baddr(*)
      integer nvert,irun,iev,qjcone,qjcul,qjzsp,qcdcorr
      integer do_ele,do_gam,do_muo,do_jet,do_tau,do_met
      real et(*),eta(*),phi(*),mass(*),e(*),px(*),py(*),pz(*),theta(*)
      real emf(*),rjet(*),mt(*),zvert(*),dzvert(*),pt(*),threshold
c
      integer lpoint,gzpelc,gzppho,gzpmuo,gzjets,i,itype/0/,gzptau
      integer icont(10),kvert(14),ivert,ipoint,ier,gzpnut,gzverh
      real vert(14),corr,excorr,eycorr,new_e,new_et,new_eta
      logical primary,secondary,ecorr,cor_data,cor_mc,lcone,lund,lzsp
      character*4 ctype
      character*132 message
      real pi,twopi,halfpi,rad
      PARAMETER (PI=3.141593,TWOPI=6.283185,HALFPI=1.570796)
      PARAMETER (RAD=0.017453293)
c
      equivalence (ctype,itype)
      equivalence (kvert,vert)
c
      ecorr = use_ecor.ne.0
      cor_data = use_ecor.eq.2
      cor_mc = use_ecor.eq.1
      nobj = 0
      irun = nrun
      iev = nevo
      lcone = qjcone.eq.-1
      lund = qjcul.eq.-1
      lzsp = qjzsp.eq.-1
c
c     get electrons
c
      if (do_ele.eq.0) goto 100
      lpoint = gzpelc()
      if (lpoint.lt.1) goto 100
      do while (lpoint.gt.0)
        if (q(lpoint+7).lt.threshold) goto 101
        nobj = nobj + 1
        baddr(nobj) = lpoint
        if (nobj.gt.maxobj) then
            write(*,'('' EVENT '',i6,'' MAXED out OBJECT PELC '')')
     &        iq(lhead+9)
          return
        endif
        et(nobj) = q(lpoint+7)
        eta(nobj) = q(lpoint+9)
        phi(nobj) = q(lpoint+10)
        mass(nobj) = 0.
        e(nobj) = q(lpoint+6)
        px(nobj) = q(lpoint+3)
        py(nobj) = q(lpoint+4)
        pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
        pz(nobj) = q(lpoint+5)
        theta(nobj) = q(lpoint+8)
        if (q(lpoint+15).lt.0.00001) then
          write(message,'('' PELC Object '',i2,
     &      '' has wierd core energy '',e10.4)') nobj,q(lpoint+15)
          message(50:53) = ctype
          call fwarning(%ref(message))
          emf(nobj) = -1.0
        else
          emf(nobj) = q(lpoint+17)/q(lpoint+15)   !em core/core energy
        endif
        rjet(nobj) = .01
        mt(nobj) = 0.
        type(nobj) = 0
c
  101   lpoint = lq(lpoint)          ! pointer to next electron
      enddo
  100 continue
c
c     get photons
c
      if (do_gam.eq.0) goto 200
      lpoint = gzppho()
      if (lpoint.lt.1) goto 200
      do while (lpoint.gt.0)
        if (q(lpoint+7).lt.threshold) goto 201
        nobj = nobj + 1
        baddr(nobj) = lpoint
        if (nobj.gt.maxobj) then
            write(*,'('' EVENT '',i6,'' MAXED out OBJECT PHOTON'')')
     &        iq(lhead+9)
          return
        endif
        baddr(nobj) = lpoint
        et(nobj) = q(lpoint+7)
        eta(nobj) = q(lpoint+9)
        phi(nobj) = q(lpoint+10)
        mass(nobj) = 0.
        e(nobj) = q(lpoint+6)
        px(nobj) = q(lpoint+3)
        py(nobj) = q(lpoint+4)
        pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
        pz(nobj) = q(lpoint+5)
        theta(nobj) = q(lpoint+8)
        if (q(lpoint+15).lt.0.00001) then
          write(message,'('' PPHO Object '',i2,
     &      '' has wierd core energy '',e10.4)') nobj,q(lpoint+15)
          message(50:53) = ctype
          call fwarning(%ref(message))
          emf(nobj) = -1.0
        else
          emf(nobj) = q(lpoint+17)/q(lpoint+15)   !em core/core energy
        endif
        rjet(nobj) = .01
        type(nobj) = 1
        mt(nobj) = 0.
c
  201   lpoint = lq(lpoint)          ! pointer to next photon
      enddo
  200 continue
c
c     get muons
c
      if (do_muo.eq.0) goto 300
      lpoint = gzpmuo(0)
      if (lpoint.lt.1) goto 300
      do while (lpoint.gt.0)
        if (q(lpoint+14).lt.threshold) goto 301
        nobj = nobj + 1
        baddr(nobj) = lpoint
        if (nobj.gt.maxobj) then
            write(*,'('' EVENT '',i6,'' MAXED out OBJECT MUON '')')
     &        iq(lhead+9)
          return
        endif
        et(nobj) = q(lpoint+14)
        eta(nobj) = q(lpoint+16)
        phi(nobj) = q(lpoint+17)
        mass(nobj) = 0.
        e(nobj) = q(lpoint+13)
        px(nobj) = q(lpoint+10)
        py(nobj) = q(lpoint+11)
        pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
        pz(nobj) = q(lpoint+12)
        theta(nobj) = q(lpoint+15)
        emf(nobj) = 0.
        rjet(nobj) = iq(lpoint+2)/14   !stick the charge here
        type(nobj) = 2
        mt(nobj) = 0.
c
  301   lpoint = lq(lpoint)          ! pointer to next muon
      enddo
  300 continue
c
c     gather verticies
c
      call gtverh(icont)
c
c     primary vertices only please
c
      nvert = 0
      if (icont(2).lt.1) goto 400
c
c     loop over all vertices (but keep only primary ones (for now?))
c
      do ivert=1,icont(2)+icont(3)
        call gtvert(ivert,vert)
        primary = (ibits(kvert(2),31,1).eq.1).or.
     &            (ibits(kvert(2),30,1).eq.1)
        if (primary) then
          nvert = nvert + 1
          zvert(nvert) = vert(5)
          dzvert(nvert) = vert(8)
        endif
      enddo
c
  400 continue
c
c     get jets - use jcone (0 means nn) - don't use set_caph
c
      if (do_jet.eq.0) goto 500
      call setcaph(lpoint)
      excorr = 0.
      eycorr = 0.
      if (lpoint.lt.1) goto 500
      do while (lpoint.gt.0)
        nobj = nobj + 1
        if (nobj.gt.maxobj) then
            write(*,'('' EVENT '',i6,'' MAXED out OBJECT JETS '')')
     &        iq(lhead+9)
          return
        endif
        ipoint = lpoint
c
c       energy corrections?
c
        corr = 1.0
        new_eta = -10000.
c
        et(nobj) = q(ipoint+6)
        eta(nobj) = q(ipoint+9)
c
c       MC corrections here
c
        if (cor_mc) then
          call jet_et_mccorr(eta(nobj),et(nobj),jcone,new_et)
          corr = new_et/et(nobj)
        endif
c
c       real data corrections here
c
        if (cor_data) then
          if (nvert.gt.0) then
            call qcd_jet_correction(ipoint,lzsp,lund,lcone,zvert(1),
     &          qcdcorr,new_e,new_et,new_eta,ier)
            if (ier.ne.0) then
              call fwarning(
     &            %ref('QCD_JET_CORRECTIONS returned non-zero error'))
            else
              corr = new_et/q(ipoint+6)
            endif
          endif
        endif
c
c       remove ele/pho (version>1 only)?
c
        if (dojnep.gt.0.and.iq(lpoint+1).ne.1) then
          if (lq(lpoint-2).gt.0) then
            ipoint = lq(lpoint-2)
            corr = 1.             !no corrections to these banks please!
            new_eta = -10000.
            et(nobj) = q(ipoint+6)
          endif
        endif
c
c       apply threshold?
c
        if (q(ipoint+6).lt.threshold) then
          nobj = nobj - 1
          goto 501
        endif
        baddr(nobj) = ipoint
c
c       correct missing et here - note that there will be no correction
c       to MET due to this jet if it's below threshold (see above cut)
c       also, there will be no correction using JNEP banks.
c       complicated eh?
c
        if (cor_data.or.cor_mc) then
          if (ipoint.eq.lpoint) then
            excorr = excorr + (et(nobj)-new_et)*cos(phi(nobj))
            eycorr = eycorr + (et(nobj)-new_et)*sin(phi(nobj))
          endif
        endif
        if (new_eta.gt.-1000.) eta(nobj) = new_eta
        phi(nobj) = q(ipoint+8)
        et(nobj) = corr*et(nobj)
        e(nobj) = corr*q(ipoint+5)
        px(nobj) = corr*q(ipoint+2)
        py(nobj) = corr*q(ipoint+3)
        pz(nobj) = corr*q(ipoint+4)
        pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
        mass(nobj) = sqrt( max(0., e(nobj)**2
     &    - px(nobj)**2 - py(nobj)**2 - pz(nobj)**2)   )
        theta(nobj) = q(ipoint+7)
        emf(nobj) = q(ipoint+14)
        rjet(nobj) = sqrt( q(ipoint+12)**2 + q(ipoint+13)**2 )
        type(nobj) = 3
        mt(nobj) = sqrt( max(0.,et(nobj)**2 - pt(nobj)**2 ) )
c
  501   lpoint = lq(lpoint)          ! pointer to next jet
      enddo
  500 continue
c
c
c     do neutrinos
c
      if (do_met.eq.0) goto 600
      lpoint = gzpnut(pnuttype)
      if (lpoint.lt.1) goto 600
      nobj = nobj + 1
      baddr(nobj) = lpoint
      if (nobj.gt.maxobj) then
        write(*,'('' EVENT '',i6,'' MAXED out OBJECT PNUT '')')
     &    iq(lhead+9)
        return
      endif
      et(nobj) = q(lpoint+7)
      eta(nobj) = q(lpoint+9)
      phi(nobj) = q(lpoint+10)
      mass(nobj) = 0.
      e(nobj) = q(lpoint+6)
      px(nobj) = q(lpoint+3)
      py(nobj) = q(lpoint+4)
      pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
      pz(nobj) = q(lpoint+5)
      theta(nobj) = q(lpoint+8)
      emf(nobj) = 0.
      rjet(nobj) = 0.
      type(nobj) = 4
      mt(nobj) = 0.
c
c     energy scale corrections?
c
      if (ecorr) then
        px(nobj) = px(nobj) + excorr
        py(nobj) = py(nobj) + eycorr
        pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
        et(nobj) = pt(nobj)
      endif
c
  600 continue
c
c     do taus
c
      if (do_tau.eq.0) goto 700
      lpoint = gzptau()
      if (lpoint.lt.1) goto 700
      do while (lpoint.gt.0)
        if (q(lpoint+7).lt.threshold) goto 701
        nobj = nobj + 1
        baddr(nobj) = lpoint
        if (nobj.gt.maxobj) then
          write(*,'('' EVENT '',i6,'' MAXED out OBJECT PTAU '')')
     &    iq(lhead+9)
          return
        endif
        et(nobj) = q(lpoint+7)
        eta(nobj) = q(lpoint+10)
        phi(nobj) = q(lpoint+9)
        mass(nobj) = 1.78
        e(nobj) = q(lpoint+6)
        px(nobj) = q(lpoint+3)
        py(nobj) = q(lpoint+4)
        pt(nobj) = sqrt( px(nobj)*px(nobj) + py(nobj)*py(nobj) )
        pz(nobj) = q(lpoint+5)
        theta(nobj) = q(lpoint+8)
        emf(nobj) = 0.
        rjet(nobj) = q(lpoint+11)
        type(nobj) = 5
        mt(nobj) = sqrt( max(0.,et(nobj)**2 - pt(nobj)**2 ) )
  701   lpoint = lq(lpoint)
      enddo
c
  700 continue
c
      return
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine fgeml(addr,depths,cquans)
c
c     input: ADDR   address of PELC/PPHO bank
c     output: depths(5) fractional energy in each of 5 EM layers (1-4+fh1)
c             cquans(*) various quantities from CLEANEM
c
      implicit none
c
      include 'D0$XFRAME$SOURCE:D0MAP.INC'
c
      integer addr
      real depths(5),cquans(*)
c
      integer icacl,icash,i,ieta_hot(5),iphi_hot(5),nv,status
      logical ok
      real edepth(5),e_hot(5)
c
      call uzero(depths,1,5)
      call uzero(cquans,1,100)
c
c     get list of various quantities (ala CLEANEM)
c
      call emstuff(addr,cquans)
c
      icacl = lq(addr-2)
      if (icacl.lt.1) return
      icash = lq(icacl-2)
      if (icash.lt.1) return
c
c     got index to cash - collect and return
c
      call cemdpth(icash,ieta_hot,iphi_hot,edepth,depths,e_hot)
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine fgcleanem(addr,yesno)
c
c     digs out the CLEANEM quantities, returns list of pass/fail
c
      implicit none
c
      include 'D0$XFRAME$SOURCE:D0MAP.INC'
c
      integer addr
      integer yesno(0:31)
c
      integer i,status
      logical btest
c
      status = iq(addr+30)
      do i=0,31
        if (btest(status,i)) then
          yesno(i) = 1
        else
          yesno(i) = 0
        endif
      enddo
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine fgemlist(addr,quans,inamquans,nquans,depths)
c
c     input: ADDR   address of PELC/PPHO bank
c     output: depths(5) fractional energy in each of 5 EM layers (1-4+fh1)
c             quans,inamquans,nquans various quantities
c
c     note: cphys calls this - it is an interface to emdispquans because
c           that routine returns character*10 words per quantity for
c           the title, but i have to pack it into integers which is a
c           pain - note that the packing will be 3 integers per title,
c           which means 12 bytes:  10 for the title, 1 for the '\0',
c           and one for spare
c
      implicit none
c
      include 'D0$XFRAME$SOURCE:D0MAP.INC'
c
      integer addr
      real depths(5),quans(*)
      integer nquans,inamquans(*)
c
      character*10 namquans(100)
      character*4 cname
      integer i,iname,ipt
c
      equivalence (cname,iname)
c
      call emdispquans(addr,quans,namquans,nquans,depths)
c
      if (nquans.lt.1) return
c
c     now unpack the suckers
c
      ipt = 1
      do i=1,nquans
        cname = namquans(i)(1:4)
        inamquans(ipt) = iname
        cname = namquans(i)(5:8)
        inamquans(ipt+1) = iname
        cname = namquans(i)(9:10)
        cname(3:3) = char(0)
        cname(4:4) = char(0)
        inamquans(ipt+2) = iname
        ipt = ipt + 3
      enddo
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine fgcatdt(ntowers)
c
c     returns the number of et,eta,phi of towers in the CATD bank
c     had and em are summed together
c
      implicit none
c
      include 'D0$XFRAME$SOURCE:D0MAP.INC'
c
      real energy(1:64,1:74,1:2)
      common /catdcom/energy
c
      integer ntowers,i,j
      integer lcatd,ipntem,ipnthd,ipntmu,nemtwr,nhdtwr,nmutwr
      real etmnem,etmnhd,emnmuo,etot
c
      ntowers = 0
      call gtcatd(lcatd,ipntem,ipnthd,ipntmu,nemtwr,nhdtwr,
     &  nmutwr,etmnem,etmnhd,emnmuo)
      if (lcatd.lt.1) return
c
c     unpack into array
c
      call unpcatd(energy)
      do i=1,64
        do j=1,74
          etot = -1.
          etot = energy(i,j,1) + energy(i,j,2)
          if (etot.gt.0.0) then
            ntowers = ntowers + 1
          endif
        enddo
      enddo
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine fgcatdts(ntowers,e_towers,eta_towers,phi_towers)
c
c     returns the number of et,eta,phi of towers in the CATD bank
c     had and em are summed together
c
      implicit none
c
      include 'D0$XFRAME$SOURCE:D0MAP.INC'
c
      real e_towers(*)
      integer eta_towers(*),phi_towers(*)
c
      real energy(1:64,1:74,1:2)
      common /catdcom/energy
c
      integer ntowers,i,j
      integer lcatd,ipntem,ipnthd,ipntmu,nemtwr,nhdtwr,nmutwr
      real etmnem,etmnhd,emnmuo,etot
c
      ntowers = 0
      call gtcatd(lcatd,ipntem,ipnthd,ipntmu,nemtwr,nhdtwr,
     &  nmutwr,etmnem,etmnhd,emnmuo)
      if (lcatd.lt.1) return
c
c     unpack into array
c
      do i=1,64
        do j=1,74
          etot = -1.
          etot = energy(i,j,1) + energy(i,j,2)
          if (etot.gt.0.0) then
            ntowers = ntowers + 1
            e_towers(ntowers) = etot
            eta_towers(ntowers) = j
            phi_towers(ntowers) = i
          endif
        enddo
      enddo
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine unpcatd(energy)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : unpack the catd bank in trigger tower format
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   8-OCT-1993   Sarah C. Eno
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      include 'D0$XFRAME$SOURCE:D0MAP.INC'

c functions
      integer gzcatd
c other variables
      integer i,j,k,l,m,itmp,itmp2
      integer jeta,jphi
      integer lcatd,nem,nhad,minem,minhd

      real energy(1:64,1:74,1:2)


      logical first
      data first/.true./


c------- begin code---------------------------------------------

      lcatd = gzcatd()


      if(first) then
        first=.false.
        minem = iq(lcatd+5)
        minhd = iq(lcatd+6)
cccc        write(*,*) 'minem minhd=',minem,minhd
      endif

      nem=iq(lcatd+8)
      nhad=iq(lcatd+9+nem)

      do i=1,nem
        itmp=iq(lcatd+8+i)
        jeta=iand(itmp,z'7f')
        jphi=iand(ishft(itmp,-7),z'7f')
c        if(jeta.le.37) then
c          jeta=jeta-38
c        else
c          jeta=jeta-37
c        endif
        energy(jphi,jeta,1)=iand(ishft(itmp,-19),z'1FFF')/10.
      enddo

      do i=1,nhad
        itmp=iq(lcatd+9+nem+i)
        jeta=iand(itmp,z'7f')
        jphi=iand(ishft(itmp,-7),z'7f')
c        if(jeta.le.37) then
c          jeta=jeta-38
c        else
c          jeta=jeta-37
c        endif
        energy(jphi,jeta,2)=iand(ishft(itmp,-19),z'1FFF')/10.
      enddo

C----------------------------------------------------------------------
  999 RETURN
      END
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
