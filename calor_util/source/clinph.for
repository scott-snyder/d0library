      SUBROUTINE CLINPH(VTX,DIR,NCLMAX,NCELL,IETAC,IPHIC,LAYERC,ARGSOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds all calor cells hit by a line
C-
C-   Inputs  : VTX(3)         Point origin of the line
C-             DIR(3)         Direction cosines of the line
C-             NCLMAX         Max number of cells to be returned
C-   Outputs : NCELL          Actual number of hit cells returned
C-             IETAC(NCLMAX)  List of etas of hit cells
C-             IPHIC(NCLMAX)  List of phis of hit cells
C-             LAYERC(NCLMAX) List of layerc's of hit cells
C-             ARGSOK         Flag for arg errors
C-   Controls:
C-
C-   Created  14-DEC-1988   Michael W. Peters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER NSLMAX
      PARAMETER (NSLMAX=450)
C      COMMON/CTEST/NSL,SL(NSLMAX),INDEX(NSLMAX)
      REAL VTX(3),DIR(3)
      INTEGER NCLMAX,NCELL,IETAC(NCLMAX),IPHIC(NCLMAX),LAYERC(NCLMAX)
      INTEGER ARGSOK
      INTEGER IETA,IPHI,LAYER,AROKIN
      INTEGER IEDGE,IEND,JETA,KETA,JETAL(17),IETAL(3,6)
      INTEGER INDEX(NSLMAX),I,NSL,IS,NS,NSLP,MAXNSL
      REAL CENPHI,DPHI,CENTH,DTH,TH1,TH2
      REAL CENRAD,DRAD,CENZED,DZED,ZED,S(2),X,Y,Z,SL(NSLMAX)
      REAL TILT,RT,ZT
      REAL RLOSQ,RHISQ,RSQ,ZCC,ZLO,ZHI,SPT
      REAL RDLIST(45),ZDLIST(80),PHLIST(70),THLIST(110),TZLIST(110)
      INTEGER NRD,NZD,NPH,NTH
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA IETAL/21, 0,0,  11, 0, 0,   9,12,0,       !ETA LISTS
     &           16,21,0,  11,16,21,  11, 0,0/
C                 ECEM        CCMG    ICD,ECMG
C               ECMH,IH   ECOH,MH,IH    ECOH
      DATA JETAL/7*1,2,3,3,4*4,5,6,6/       !LIST OF ETA LISTS BY LAYER
      DATA MAXNSL/0/
C
      IF(FIRST) THEN
        NPH=0
        NRD=0
        NTH=0
        NZD=0
        IETA=1
C
C ****  List phi planes
C
        DO 100 IPHI = 1 ,  NPHIL/2
          CALL CALPHI(IPHI,IETA,CENPHI,DPHI,AROKIN)
          NPH=NPH+1
          PHLIST(NPH)=CENPHI-DPHI/2.
          NPH=NPH+1
          PHLIST(NPH)=CENPHI
  100   CONTINUE
C
C ****  List eta cones
C
        DO 200 IETA = 1 ,  NETAL+1
          IF(IETA.EQ.0) GO TO 200
          IF(IETA.EQ.NETAL+1) THEN
            CALL CALTH(NETAL,CENTH,DTH,AROKIN)
            IF(AROKIN.NE.0) GO TO 200
            NTH=NTH+1
            THLIST(NTH)=CENTH-DTH/2.
            TZLIST(NTH)=0.
          ELSE
            CALL CALTH(IETA,CENTH,DTH,AROKIN)
            IF(AROKIN.NE.0) GO TO 200
            TH1=CENTH+DTH/2.
            TH2=CENTH-DTH/2.
            NTH=NTH+1
            THLIST(NTH)=TH1
            TZLIST(NTH)=0.
            NTH=NTH+1
            THLIST(NTH)=2.*ATAN(SQRT(TAN(TH1/2.)*TAN(TH2/2.)))
            IF(THLIST(NTH).LT.0.) THLIST(NTH)=PI+THLIST(NTH)
            TZLIST(NTH)=0.
          ENDIF
  200   CONTINUE
        DO 400 IEDGE=-1,1,2           !LOOP ON EDGES OF THE CELLS
C
C ****  List CC cylinders
C
          DO 300 LAYER=1,MNLYCH          !LOOP ON CC LAYERS
            IF(LAYER.GT.3.AND.LAYER.LT.7) GO TO 300
            IF(LAYER.GT.7.AND.LAYER.LT.11) GO TO 300
            IF(LAYER.EQ.14) GO TO 300
            IETA=1
            CALL CALRAD(IETA,LAYER,CENRAD,DRAD,CENZED,DZED,AROKIN)
            IF(AROKIN.NE.0) GO TO 300
            NRD=NRD+1
            RDLIST(NRD)=CENRAD+IEDGE*DRAD/2.
  300     CONTINUE
C
C ****  List EC layers
C
          DO 390 LAYER=1,MXLYCH         !LOOP ON EC LAYERS
            IF(LAYER.GT.3.AND.LAYER.LT.7) GO TO 390
            JETA=JETAL(LAYER)           !SELECT ETA LIST FOR THIS LAYER
            DO 380 IEND=1,2             !LOOP ON N/S END OF CALOR
              DO 370 KETA=1,3           !LOOP ON ETA'S IN THIS ETA LIST
                IETA=IETAL(KETA,JETA)
                IF(IETA.EQ.0) GO TO 380
                IF(IEND.EQ.2) IETA=-IETA
                CALL CALZED(IETA,LAYER,CENZED,DZED,CENRAD,DRAD,
     &          TILT,AROKIN)
                IF(AROKIN.NE.0) GO TO 370
                RT=CENRAD-DRAD/2.
                CALL CALZED(IETA,LAYER,CENZED,DZED,CENRAD,DRAD,
     &          TILT,AROKIN)
                ZED=CENZED+IEDGE*DZED/2.
                IF(TILT.NE.0.) THEN
                  ZT=ZED+RT*TAN(TILT)
                  NTH=NTH+1
                  THLIST(NTH)=HALFPI+TILT
                  TZLIST(NTH)=ZT
                ELSE
                  NZD=NZD+1
                  ZDLIST(NZD)=ZED
                ENDIF
  370         CONTINUE
  380       CONTINUE
  390     CONTINUE
C
C ****  List ECEM/ECIH/ECMH/ECOH boundary cylinders
C
          CALL CALZED(21,1,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN)  !EM
          NRD=NRD+1
          RDLIST(NRD)=CENRAD+IEDGE*DRAD/2.
          CALL CALZED(21,11,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN)  !IH
          NRD=NRD+1
          RDLIST(NRD)=CENRAD+IEDGE*DRAD/2.
          CALL CALZED(15,11,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN)  !MH
          NRD=NRD+1
          RDLIST(NRD)=CENRAD+IEDGE*DRAD/2.
          CALL CALZED(11,15,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN)  !OH
          NRD=NRD+1
          RDLIST(NRD)=CENRAD+IEDGE*DRAD/2.
C
C ****  List ICD/MG boundary cylinders
C
          CALL CALZED(8,8,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN)  !CCMG
          NRD=NRD+1
          RDLIST(NRD)=CENRAD+IEDGE*DRAD/2.
          CALL CALZED(9,9,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN)  !ICD
          NRD=NRD+1
          RDLIST(NRD)=CENRAD+IEDGE*DRAD/2.
          CALL CALZED(12,9,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN) !ICD
          NRD=NRD+1
          RDLIST(NRD)=CENRAD+IEDGE*DRAD/2.
          CALL CALZED(8,10,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN) !ECMG
          NRD=NRD+1
          RDLIST(NRD)=CENRAD+IEDGE*DRAD/2.
          CALL CALZED(11,10,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN)!ECMG
          NRD=NRD+1
          RDLIST(NRD)=CENRAD+IEDGE*DRAD/2.
C
C ****  List end planes of the CC
C
          CALL CALRAD(1,1,CENRAD,DRAD,CENZED,DZED,AROKIN)
          NZD=NZD+1
          ZDLIST(NZD)=CENZED+IEDGE*DZED/2.
          CALL CALRAD(1,11,CENRAD,DRAD,CENZED,DZED,AROKIN)
          NZD=NZD+1
          ZDLIST(NZD)=CENZED+IEDGE*DZED/2.
          CALL CALRAD(1,15,CENRAD,DRAD,CENZED,DZED,AROKIN)
          NZD=NZD+1
          ZDLIST(NZD)=CENZED+IEDGE*DZED/2.
  400   CONTINUE          !FINISH LOOP ON IEDGE
C
C ****  List outer radii of remaining ECEM
C
        CALL CALZED(21,2,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN)
        NRD=NRD+1
        RDLIST(NRD)=CENRAD+DRAD/2.
        CALL CALZED(21,3,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN)
        NRD=NRD+1
        RDLIST(NRD)=CENRAD+DRAD/2.
        CALL CALZED(21,7,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN)
        NRD=NRD+1
        RDLIST(NRD)=CENRAD+DRAD/2.
C
C ****  List special cases at back of ECMH
C
        CALL CALZED(15,15,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN)
        ZED=CENZED+DZED/2.
        NZD=NZD+1
        ZDLIST(NZD)=ZED
        NZD=NZD+1
        ZDLIST(NZD)=-ZED
        CALL CALZED(14,15,CENZED,DZED,CENRAD,DRAD,TILT,AROKIN)
        ZED=CENZED+DZED/2.
        NZD=NZD+1
        ZDLIST(NZD)=ZED
        NZD=NZD+1
        ZDLIST(NZD)=-ZED
        FIRST=.FALSE.
      ENDIF
C
C ****  End of initialization
C
      ARGSOK=1
      NSL=0
      IETA=1
C
C ****  Find intersections of line with all phi planes
C
      DO 500 I=1,NPH
        CALL CLNPH(VTX,DIR,PHLIST(I),NS,S)
        IF(NS.GT.0) THEN
          IF(NSL.GE.NSLMAX) GO TO 999
          NSL=NSL+1
          SL(NSL)=S(1)
        ENDIF
  500 CONTINUE
C
C ****  Find intersections of line with all eta cones
C
      DO 600 I=1,NTH
        CALL CLNTH(VTX,DIR,TZLIST(I),THLIST(I),NS,S)
        DO 550 IS=1,NS
          IF(NSL.GE.NSLMAX) GO TO 999
          NSL=NSL+1
          SL(NSL)=S(IS)
  550   CONTINUE
  600 CONTINUE
C
C ****  Find intersections of line with all radial cylinders
C
      DO 700 I=1,NRD
        CALL CLNRD(VTX,DIR,RDLIST(I),NS,S)
        DO 620 IS=1,NS
          IF(NSL.GE.NSLMAX) GO TO 999
          NSL=NSL+1
          SL(NSL)=S(IS)
  620   CONTINUE
  700 CONTINUE
C
C ****  Find intersections of line with all z-planes
C
      DO 800 I=1,NZD
        CALL CLNZD(VTX,DIR,ZDLIST(I),NS,S)
        DO 760 IS=1,NS
          IF(NSL.GE.NSLMAX) GO TO 999
          NSL=NSL+1
          SL(NSL)=S(IS)
  760   CONTINUE
  800 CONTINUE
      IF(NSL.GT.0) THEN
        DO 900 I=1,NSL
          INDEX(I)=I
  900   CONTINUE
        CALL SORTTF(SL,INDEX,NSL)
        ARGSOK=2
        IF(NSL.GT.MAXNSL) THEN
          MAXNSL=NSL
        ENDIF
        NCELL=0
        DO 950 I=2,NSL
          S(1)=SL(INDEX(I))
          S(2)=SL(INDEX(I-1))
          IF(S(1).LT.0.) GO TO 950
          IF(ABS(S(1)-S(2)).LT..001) GO TO 950
          SPT=.5*(S(1)+S(2))
          X=VTX(1)+DIR(1)*SPT
          Y=VTX(2)+DIR(2)*SPT
          Z=VTX(3)+DIR(3)*SPT
          CALL CPOSPH(X,Y,Z,IETA,IPHI,LAYER,AROKIN)
          IF(AROKIN.NE.0) GO TO 950
          IF(NCELL.GT.0) THEN
            IF(IETA.EQ.IETAC(NCELL).AND.IPHI.EQ.IPHIC(NCELL).AND.
     &      LAYER.EQ.LAYERC(NCELL)) GO TO 950
          ENDIF
          IF(NCELL.GE.NCLMAX) GO TO 999
          NCELL=NCELL+1
          IETAC(NCELL)=IETA
          IPHIC(NCELL)=IPHI
          LAYERC(NCELL)=LAYER
  950   CONTINUE
      ENDIF
      ARGSOK=0
  999 RETURN
      END
