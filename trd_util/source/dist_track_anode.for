      SUBROUTINE DIST_TRACK_ANODE(VIN,DPHITC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute angular distance TRACK-closest anode
C-
C-   Inputs  : VIN= parameters of the track
C-   Outputs :
C-   Controls:
C-
C-   Created 13-MAR-1994   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FIRSHT.INC'
      INCLUDE 'D0$INC:GEOMTR.INC/LIST'
      INCLUDE 'D0$PARAMS:TRD_NB_OF_WIRES.PARAMS'
      INCLUDE 'D0$INC:TRD_NB_OF_WIRES.inc'
      INCLUDE 'D0$INC:TRWCOD_512.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL DPHITC(3),DPHIM,dang(3)
      INTEGER I,IAIN,IMI,IMS,JAIN,JAOUT,WIRE
      INTEGER LOUT,TRUNIT,TCHNB
      INTEGER ICH,IGTRAK
      REAL VIN(6),VOUT(6),RIN,PHITR,PHICAT,DPHIST,X,Y,ZIN,PI
      REAL CPHOF(3),SPHOF(3),DPHI,PHIR,PHIT,ZG
      LOGICAL FIRST,DOPRINT,TRD_DO_PRINT,run1a
C----------------------------------------------------------------------
C
      DATA FIRST/.TRUE./
      CALL HCDIR('//PAWC/TRD',' ')  ! go to TRD directory
      IF(FIRST)THEN
        CALL HBOOK1(FIRSHT+1441,
     &        'angular distance track-closest wire layer 1',
     &        50,-10.,10.,0.)
        CALL HBOOK1(FIRSHT+1442,
     &        'angular distance track-closest wire layer 2',
     &        50,-10.,10.,0.)
        CALL HBOOK1(FIRSHT+1443,
     &        'angular distance track-closest wire layer 3',
     &        50,-10.,10.,0.)
        LOUT=TRUNIT()
        FIRST=.FALSE.
        PI=ACOS(-1.)
C  NOT ELEGANT:SHOULD BE DONE IN INITRD AND TRANSMITTED
C                                             THROUGH COMMON BLOCK
        DO 31 ICH = 1,  3
c          PRINT*,' in trcell,layer',ICH,' ofsdph',OFSDPH(ICH),
c     &      ' dphian',DPHIAN
          CPHOF(ICH)=COS(OFSDPH(ICH))
          SPHOF(ICH)=SIN(OFSDPH(ICH))
          dang(ich)=dphian(ich)
          if(ich.eq. 3 .and. run1a())dang(ich)=2.*dphian(ich)
   31   CONTINUE
      END IF
C      print*,' entree dist_track_anode'
      DOPRINT=TRD_DO_PRINT()
      DO 100 ICH=1,3
        DPHIM=1000.
        RIN=RADAN(ICH)
        CALL EXTCYL(VIN,VOUT,RIN,IGTRAK)
        IF(IGTRAK.NE.0)GO TO 60
        IF(ABS(VOUT(3)).GT.83.5)GO TO 60
C  impact point of the track in the anode plane in the TRD frame
        X=CPHOF(ICH)*VOUT(1)+SPHOF(ICH)*VOUT(2)
        Y=-SPHOF(ICH)*VOUT(1)+CPHOF(ICH)*VOUT(2)
        PHITR=ATAN2(-Y,-X)+PI
        IAIN=PHITR/DANG(ICH)+1! cell crossed by the track
        IMI=IAIN-10
        IMS=IAIN+10
c        if(doprint)write(lout,*)' in dist_track_anode,iain',iain
        DO  44 WIRE =IMI,IMS
          JAIN=WIRE
          IF(JAIN.LE.0)JAIN= NWIRE_PER_LAYER(ICH)+JAIN
          IF(JAIN.GT.NWIRE(ICH))JAIN=JAIN-NWIRE(ICH)
c          IF(JAIN.NE.WIRE)PRINT*, 'imi,ims',IMI,IMS,
c     &      ' wire',WIRE,'jain',JAIN
c          if(doprint)write(lout,*)' in dst_track_anode,jain',jain,
c     &      ' twcod ',twcod(tchnb(jain,ich))
          IF(.NOT.TWCOD(TCHNB(JAIN,ICH)))GO TO 44
          DPHI=PHITR-(FLOAT(JAIN)-.5)*DANG(ICH)!ANG. DIST. TO THE WIRE
          IF(ABS(DPHI).GT.ABS(DPHIM))GO TO 60
          DPHIM=DPHI
   44   CONTINUE
   60   CONTINUE
        DPHITC(ICH)=DPHIM/DANG(ICH)
        if(doprint)        write(lout,*)
     +   ' layer',ICH,' cell track',IAIN,' cell TRd',JAIN,
     &    'phitr-phi cell (in cell unit)',DPHITC(ICH)
        CALL HF1(FIRSHT+1440+ICH,DPHITC(ICH),1.)
  100 CONTINUE
  999 RETURN
      END
