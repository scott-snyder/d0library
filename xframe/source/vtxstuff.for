      SUBROUTINE VTXSTUFF(okok,maxtracks,vtxvers,nvtxtracks,
     & ptcorr,hvcorr,nhits,nsta,cdd1size,bankno,
     & vee,mu,e,tau,status,nwires,xybits,rzbits,nzhits,phi,xg,yg,
     & theta,vzgtheta,zg,chi2xy,chi2rz,dzdr,zvtx,errphi,errxy,
     & errtheta,errrz,ion,sintheta)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : dig out VTX stuff
C-
C-   Inputs  : 
C-   Outputs : okok = 0 nogood, 1=okok
C-   Controls: 
C-
C-   Created  18-OCT-1994   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      integer maxtracks,okok
      integer vtxvers,nvtxtracks,nhits,cdd1size,nsta,ptcorr,hvcorr
c
      integer lvtrh,gzvtrh,it,i,ldtrk
      include 'd0$inc:zebcom.inc'
c
      integer bankno(*),vee(*),mu(*),e(*),tau(*),status(*)
      integer nwires(*),xybits(*),rzbits(*),nzhits(*)
      real phi(*),xg(*),yg(*),theta(*),vzgtheta(*),zg(*),chi2xy(*)
      real chi2rz(*),dzdr(*),zvtx(*),errphi(*),errxy(*)
      real errtheta(*),errrz(*),ion(*),sintheta(*)
c
      okok = 0
      lvtrh = gzvtrh(0)
      if (lvtrh.lt.1) return
c
      vtxvers = iq(lvtrh+1)
      nvtxtracks = iq(lvtrh+2)
      nhits = iq(lvtrh+4)
      nsta = iq(lvtrh+5)
      cdd1size = iq(lvtrh+6)
      ptcorr = ibits(iq(lvtrh+3),1,1)
      hvcorr = ibits(iq(lvtrh+3),2,1)
      okok = 1
c
c     dig out vtxt stuff
c
      if (nvtxtracks.lt.1) return
      i = 0
      ldtrk = lq(lvtrh-1)
      if (ldtrk.lt.1) return
      do while (ldtrk.gt.0)
        i = i + 1
        if (i.eq.maxtracks) then
          call fwarning(%ref('Too many VTX tracks - over MAXIMUM'))
          return
        endif
c
        bankno(i) = iq(ldtrk-5)
        vee(i) = ibits(iq(ldtrk),9,1)
        mu(i) = ibits(iq(ldtrk),8,1)
        e(i) = ibits(iq(ldtrk),7,1)
        tau(i) = ibits(iq(ldtrk),6,1)
        status(i) = iq(ldtrk+1)
        nwires(i) = iq(ldtrk+2)
        xybits(i) = iq(ldtrk+3)
        rzbits(i) = iq(ldtrk+4)
        nwires(i) = iq(ldtrk+5)
        phi(i) = q(ldtrk+6)
        xg(i) = q(ldtrk+7)
        yg(i) = q(ldtrk+8)
        theta(i) = q(ldtrk+9)
        vzgtheta(i) = q(ldtrk+10)
        zg(i) = q(ldtrk+11)
        chi2xy(i) = q(ldtrk+12)
        chi2rz(i) = q(ldtrk+13)
        dzdr(i) = q(ldtrk+14)
        zvtx(i) = q(ldtrk+15)
        errphi(i) = q(ldtrk+16)
        errxy(i) = q(ldtrk+17)
        errtheta(i) = q(ldtrk+18)
        errrz(i) = q(ldtrk+19)
        ion(i) = q(ldtrk+20)
        sintheta(i) = q(ldtrk+21)
c
        ldtrk = lq(ldtrk)
      enddo
c
      return
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine numvtxtks(okok,num)
c
c     returns number of vtx tracks
c
      implicit none
c
      include 'd0$inc:zebcom.inc'
c
      integer okok,num
c
      integer lvtrh,gzvtrh
c
      lvtrh = gzvtrh(0)
      num = 0
      okok = 0
      if (lvtrh.lt.1) return
      num = iq(lvtrh+2)
      okok = 1
      return
      end
