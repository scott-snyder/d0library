      SUBROUTINE CDSTUFF(okok,maxtracks,
     &   cdbvers,cdvers,ncdtracks,nseg0,nseg1,nseg2,nseg3,ncdsta,
     &   ncdhitscon,ncdrawlength,cdfull,bankno,
     &   nvers,tau,e,mu,vee,vid,edge,nztrks,seg0,seg1,seg2,seg3,nwires,
     &   xybit,rzbit,nz,ndof,phi,x0,y0,theta,r0,z0,chixy2,
     &   chirz2,errphi,errxy,errtheta,errz0,ion,errion,thetaz0cov)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : dig out CD stuff
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
c
      integer maxtracks,
     &   cdbvers,cdvers,ncdtracks,nseg0,nseg1,nseg2,nseg3,ncdsta,
     &   ncdhitscon,ncdrawlength,cdfull,okok
      integer nvers(*),tau(*),e(*),mu(*),vee(*),vid(*),edge(*)
      integer nztrks(*),seg0(*),seg1(*),seg2(*),seg3(*),nwires(*)
      integer xybit(*),rzbit(*),nz(*),ndof(*),bankno(*)
      real phi(*),x0(*),y0(*),theta(*),r0(*),z0(*),chixy2(*)
      real chirz2(*),errphi(*),errxy(*),errtheta(*),errz0(*)
      real ion(*),errion(*),thetaz0cov(*)
c
      integer ldtrh,gzdtrh,it,i,ldtrk
      include 'd0$inc:zebcom.inc'
c
      okok = 0
      ldtrh = gzdtrh(0)
      if (ldtrh.lt.1) return
c
      it = iq(ldtrh+0)
      cdvers = iq(ldtrh+1)
      ncdtracks = iq(ldtrh+2)
      nseg0 = iq(ldtrh+3)
      nseg1 = iq(ldtrh+4)
      nseg2 = iq(ldtrh+5)
      nseg3 = iq(ldtrh+6)
      ncdsta = iq(ldtrh+7)
      ncdhitscon = iq(ldtrh+8)
      ncdrawlength = iq(ldtrh+9)
c
      it = cdbvers
      
      cdbvers = ibits(it,13,5) ! keeps bits 13-17
      cdfull = ibits(it,12,1)  !bit 12=1 full
c
      okok = 1
c
c     dig out dtrk stuff
c
      if (ncdtracks.lt.1) return
      i = 0
      ldtrk = lq(ldtrh-1)
      if (ldtrk.lt.1) return
      do while (ldtrk.gt.0)
        i = i + 1
        if (i.eq.maxtracks) then
          call fwarning(%ref('Too many CD tracks - over MAXIMUM'))
          return
        endif
        bankno(i) = iq(ldtrk-5)
        nvers(i) = ibits(iq(ldtrk),13,5)
        tau(i) = ibits(iq(ldtrk),6,1)
        e(i) = ibits(iq(ldtrk),7,1)
        mu(i) = ibits(iq(ldtrk),8,1)
        vee(i) = ibits(iq(ldtrk),9,1)
        vid(i) = ibits(iq(ldtrk),10,2)
        edge(i) = ibits(iq(ldtrk+1),0,1)
        nztrks(i) = ibits(iq(ldtrk+1),1,2)
        seg0(i) = ibits(iq(ldtrk+1),4,4)
        seg1(i) = ibits(iq(ldtrk+1),11,4)
        seg2(i) = ibits(iq(ldtrk+1),18,4)
        seg3(i) = ibits(iq(ldtrk+1),25,4)
        nwires(i) = iq(ldtrk+2)
        xybit(i) = iq(ldtrk+3)
        rzbit(i) = iq(ldtrk+4)
        nz(i) = iq(ldtrk+5)
        phi(i) = q(ldtrk+6)
        x0(i) = q(ldtrk+7)
        y0(i) = q(ldtrk+8)
        theta(i) = q(ldtrk+9)
        r0(i) = q(ldtrk+10)
        z0(i) = q(ldtrk+11)
        chixy2(i) = q(ldtrk+12)
        chirz2(i) = q(ldtrk+13)
        ndof(i) = iq(ldtrk+14)
        errphi(i) = q(ldtrk+16)
        errxy(i) = q(ldtrk+17)
        errtheta(i) = q(ldtrk+18)
        errz0(i) = q(ldtrk+19)
        ion(i) = q(ldtrk+20)
        errion(i) = q(ldtrk+21)
        thetaz0cov(i) = q(ldtrk+22)
c
        ldtrk = lq(ldtrk)
      enddo
c
      return
      END
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine numcdtks(okok,num)
c
c     returns number of cd tracks
c
      implicit none
c
      include 'd0$inc:zebcom.inc'
c
      integer okok,num
c
c
      integer ldtrh,gzdtrh
c
      ldtrh = gzdtrh(0)
      num = 0
      okok = 0
      if (ldtrh.lt.1) return
      num = iq(ldtrh+2)
      okok = 1
      return
      end
