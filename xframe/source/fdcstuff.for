      SUBROUTINE FDCSTUFF(okok,maxtracks,ftvers,nfdtracks,nfd0,nfd1,
     & nfdwires,cdd3size,alignlevel,fdfull,fdz0,fdz1,bankno,
     & vee,mu,e,jet,status,nhits,fdcbits,x0,y0,phi,dxdz,dydz,
     & chisq,ion,errion,theta,errphi,errtheta,nptsfit,
     & thetafit,errthetafit,phifit,errphifit,chisqfit,vtxfit)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : dig out FDC stuff
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
      integer ftvers,nfdtracks,nfd0,nfd1,nfdwires,cdd3size
      integer alignlevel,fdfull
      real fdz0,fdz1
c
      integer ldtrh,gzftrh,it,i,ldtrk
      include 'd0$inc:zebcom.inc'
c
      integer bankno(*),vee(*),mu(*),e(*),jet(*),status(*)
      integer nhits(*),fdcbits(*),nptsfit(*),vtxfit(*)
      real x0(*),y0(*),phi(*),dxdz(*),dydz(*),chisq(*),ion(*)
      real errion(*),theta(*),errphi(*),errtheta(*)
      real thetafit(*),errthetafit(*),phifit(*),errphifit(*),chisqfit(*)
c
      okok = 0
      ldtrh = gzftrh(0)
      if (ldtrh.lt.1) return
c
      ftvers = iq(ldtrh+1)
      fdfull = ibits(iq(ldtrh),2,1)
      nfdtracks = iq(ldtrh+2)
      fdz0 = q(ldtrh+3)
      fdz1 = q(ldtrh+4)
      nfd0 = iq(ldtrh+5)
      nfd1 = iq(ldtrh+6)
      nfdwires = iq(ldtrh+7)
      cdd3size = iq(ldtrh+8)
      alignlevel = iq(ldtrh+9)
      okok = 1
c
c     dig out fdct stuff
c
      if (nfdtracks.lt.1) return
      i = 0
      ldtrk = lq(ldtrh-1)
      if (ldtrk.lt.1) return
      do while (ldtrk.gt.0)
        i = i + 1
        if (i.eq.maxtracks) then
          call fwarning(%ref('Too many FDC tracks - over MAXIMUM'))
          return
        endif
c
        bankno(i) = iq(ldtrk-5)
        vee(i) = ibits(iq(ldtrk),9,1)
        e(i) = ibits(iq(ldtrk),7,1)
        mu(i) = ibits(iq(ldtrk),8,1)
        jet(i) = ibits(iq(ldtrk),6,1)
        status(i) = iq(ldtrk+1)
        fdcbits(i) = iq(ldtrk+3)
        x0(i) = q(ldtrk+4)
        y0(i) = q(ldtrk+5)
        phi(i) = q(ldtrk+6)
        dxdz(i) = q(ldtrk+7)
        dydz(i) = q(ldtrk+8)
        chisq(i) = q(ldtrk+19)
        ion(i) = q(ldtrk+20)
        errion(i) = q(ldtrk+21)
        theta(i) = q(ldtrk+22)
        errphi(i) = q(ldtrk+23)
        errtheta(i) = q(ldtrk+24)
        nptsfit(i) = iq(ldtrk+25)
        thetafit(i) = q(ldtrk+26)
        errthetafit(i) = q(ldtrk+27)
        phifit(i) = q(ldtrk+28)
        errphifit(i) = q(ldtrk+29)
        chisqfit(i) = q(ldtrk+30)
        vtxfit(i) = iq(ldtrk+31)
c
        ldtrk = lq(ldtrk)
      enddo
c
      return
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine numfdtks(okok,num)
c
c     returns number of fdc tracks
c
      implicit none
c
      include 'd0$inc:zebcom.inc'
c
      integer okok,num
c
      integer lftrh,gzftrh
c
      lftrh = gzftrh()
      num = 0
      okok = 0
      if (lftrh.lt.1) return
      num = iq(lftrh+2)
      okok = 1
      return
      end
