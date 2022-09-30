      SUBROUTINE CLUPOS(ISTACK,VINT,VOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute interaction point coordinates of
C-          X rays and Delta rays  in the general system .
C-           The behaviour of high energy (>5 KeV) delta rays is taken
C-          into account and split in several "clusters".
C-           Drift distances , hit wires  and drift times are
C-          calculated.
C-
C-    INPUT: ISTACK =ISTACK: TRD stack number
C-           VINT   =7 vector defining the entrance point
C-           VOUT   =7 vector    ""     "  exit      "
C-
C-   Outputs : ==> COMMON /CLUSM/
C-             XCLES(I)   =POSITION ALONG    THE RADIUS FOR CLUSTER I
C-             YCLES(I)   =   ""    PERP. TO THE   ""         ""
C-             CLUDIS(I,1)=DRIFT DISTANCE IN DRIFT ZONE
C-             CLUDIS(I,2)=  ""    ""        AMPL   "
C-             -IWIRE(I)=HIT WIRE FOR CLUSTER #I
C-             -TIMEC(I)=DRIFT TIME
C-
C-             ==> COMMON /CLUSTR/
C-             ISTRIP(I)=CATHODE STRIP FOR CLUSTER #I
C-             DSTRIP(I)=FRACTION OF CATHODE STRIP FROM CENTER
C-
C-
C-   Created                A. ZYLBERSTEJN
C-   Updated  22-DEC-1987   A. ZYLBERSTEJN
C-   Updated  20-SEP-1988   J.R. HUBBARD/A. ZYLBERSTEJN
C-   Updated  11-OCT-1988   J.R. HUBBARD      Include Cathodes
C-   Updated  10-JUL-1989   A. Zylberstejn  Done corrections to handle
C-                                          correctly high energy clusters
C-   Updated  25-OCT-1989   A. Zylberstejn  :Make a correction for clusters
C-                                           outside the TEC
C-   Updated  26-MAR-1990   A. Zylberstejn  :change the treatment of the case
C-                                           of too many clusters. Remove call
C-                                           to CLUCLE
C-   Updated  13-MAR-1991   A. Zylberstejn  : split clusters on 2 wires
C-   Updated   9-AUG-1993   J.P. Cussonneau : Modified call to trhigh in order
C-                                            to generate high energy clusters
C-                                            forward.   
C-                                            Modified transverse dispersion of
C-                                            clusters in drift area.
C----------------------------------------------------------------------
C
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:CLUSM.INC/LIST'
      INCLUDE 'D0$INC:CLUSTR.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:GCONST.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:GEOMTC.INC/LIST'
      INCLUDE 'D0$INC:GEOMTR.INC/LIST'
      INCLUDE 'D0$INC:INFO.INC/LIST'
      INCLUDE 'D0$INC:POSIT.INC/LIST'
      INCLUDE 'D0$INC:WORKSP.INC'
      REAL XIN0,YIN0,PHIN0
      REAL AUX(1000),IAUX(300),ECLIN
      EQUIVALENCE(WS(1),AUX(1)),(IWS(1001),IAUX)
      INTEGER I,IWI,K,ISTACK,NNEW,NPRNT,IND,LVMIN
      INTEGER INEW  
      REAL DRIG,PHICL,RR,CT,ST,CPH,SPH
      REAL CPHOF(3),SPHOF(3),PHIW(512,3)
      REAL ECL,ENEW(20),XNEW(20),YNEW(20),ZNEW(20)
      REAL PHICAT,DPHIST,RCDP,SNEW
      REAL RX,RY,RZ,RT,CX,CY,CZ,CLSMEN,DISPTR
      REAL DSGA,R2,TRDTIM,XAV,XP,YP,ZP,YL,VECTL
      REAL CDPHI,SDPHI,DPHI,VINT(7),VOUT(7),SIGT
      REAL X1,Y1,Z1,X2,Y2,Z2
      LOGICAL FIRST
C
      DATA NPRNT/0/
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST=.FALSE.
        DISPTR=.12  ! transverse dispersion (old value 0.03)
C  NOT ELEGANT:SHOULD BE DONE IN INITRD AND TRANSMITTED
C                                             THROUGH COMMON BLOCK
        DO 31 I =  1,  3
          CPHOF(I)=COS(OFSDPH(I))
          SPHOF(I)=SIN(OFSDPH(I))
C  DEFINE PHI FOR EACH WIRE IN THE TRD REFERENCE FRAME
          DO 41 K =  1,NWIRE(I)
            PHIW(K,I)=(K-.5)*DPHIAN(I)
   41     CONTINUE
   31   CONTINUE
      END IF
      K = 0
C  DETERMINE THE DIRECTION COSINES
      RX=VOUT(1)-VINT(1)
      RY=VOUT(2)-VINT(2)
      RZ=VOUT(3)-VINT(3)
      RT=SQRT(RX**2+RY**2+RZ**2)
      CX=RX/RT
      CY=RY/RT
      CZ=RZ/RT
      IF ( PTRD.GE.8) THEN
        WRITE(LOUT,*)' VOUT',VOUT(1),VOUT(2),VOUT(3),' ROUT',
     +       SQRT(VOUT(1)**2+VOUT(2)**2)
        WRITE(LOUT,*)' VINT',VINT(1),VINT(2),VINT(3),' RINT',
     +       SQRT(VINT(1)**2+VINT(2)**2)
      END IF
      CALL UCOPY(XCLES,AUX(1),NSMEAR)
      CALL UCOPY(ECLES,AUX(501),NSMEAR)
      CALL UCOPY_i(IESCAP,IAUX(1),NSMEAR)
      DO 100 I = 1,NSMEAR
C  XCLES is referenced with respect to the 'MIDDLE' window (TEC entrance
C  along the path of the particle).
C---
C COMPUTE COORDINATES IN THE GENERAL SYSTEM (ASSUMING X RAYS AND DELTA
C        RAYS ARE COLINEAR WITH THE TRACK IN THE TEC)
C---
        ECLIN=AUX(500+I)
        IF(ECLIN.LT.ECLMIN)GO TO 100
        X   = VINT(1) + AUX(I)*CX
        Y   = VINT(2) + AUX(I)*CY
        Z   = VINT(3) + AUX(I)*CZ
        XP=X
        YP=Y
        ZP=Z
        XAV = AUX(I)
        R2 = X**2 + Y**2 !RADIUS IN THE GENERAL SYSTEM
        VECTL=SQRT(R2+Z**2)
        R=SQRT(R2)
        CT = Z/VECTL
        ST = SQRT(1. - CT**2)
        CPH = X/R
        SPH = Y/R
        NNEW=0
        SNEW=0.
C Check that the cluster is in the TEC
        IF (R.GT.RADEXT(ISTACK).OR.R.LT.RADWIN(ISTACK))GO TO 90
   30   CONTINUE
        IF (IAUX(I).NE.0)THEN!  Analyse the X ray resulting from
C                              ! luminescence
          CALL XLUM(X,Y,Z,CT,ST,CPH,SPH,ECLIN,XP,YP,ZP)
C        Check if the X ray interacts in the T.E.C.
          R = SQRT(XP**2 + YP**2)
          IF (R.GT.RADEXT(ISTACK).OR.R.LT.RADWIN(ISTACK))GO TO 100
          X=XP
          Y=YP
          Z=ZP
          ECLIN=CLSMEN(ECLIN)!SMEAR THE CLUSTER ENERGY
        END IF
        ECL=ECLIN
C
C    ANALYSE HIGH ENERGY CLUSTER
        IF( ECL.GT.5.)THEN
          IF(PTRD.GE.10)WRITE(LOUT,*)' CALL  TRHIGH WITH ECL=',ECL
          CALL TRHIGH(ECL,0.,0.,0.,NNEW,ENEW,XNEW,YNEW,ZNEW)
          SNEW=0.
          DO INEW = 1,NNEW  
            X1 = XNEW(INEW) 
            Y1 = YNEW(INEW) 
            Z1 = ZNEW(INEW) 
            IF ( PTRD.GE.8) THEN
              WRITE(LOUT,*)' IN CLUPOS BEFORE TRAFRA' 
              WRITE(LOUT,*)' X,Y,Z=',X,Y,Z
              WRITE(LOUT,*)' X1,Y1,Z1=',X1,Y1,Z1
              WRITE(LOUT,*)' CT,ST,CPH,SPH=',CT,ST,CPH,SPH
            ENDIF 
C-- Change coord. from particle frame to mother frame 
            CALL TRAFRA(X1,Y1,Z1,CT,ST,CPH,SPH,X,Y,Z,X2,Y2,Z2)
C--
            IF ( PTRD.GE.8) THEN
              WRITE(LOUT,*)' IN CLUPOS AFTER TRAFRA' 
              WRITE(LOUT,*)' X2,Y2,Z2=',X2,Y2,Z2
            ENDIF 
            XNEW(INEW) = X2
            YNEW(INEW) = Y2
            ZNEW(INEW) = Z2
          ENDDO
        END IF
C
   40   CONTINUE
        IF(NNEW.NE.0)THEN
          IF(ENEW(NNEW).LT.ECLMIN)THEN
            NNEW=NNEW-1
            GO TO 40
          END IF
          XP=XNEW(NNEW)
          YP=YNEW(NNEW)
          ZP=ZNEW(NNEW)
          ECL=ENEW(NNEW)
          SNEW=SNEW+ECL
          R=SQRT(X**2+Y**2)
          NNEW=NNEW-1
          IF (R.GT.RADEXT(ISTACK).OR.R.LT.RADWIN(ISTACK))GO TO 40
          IF(K.GE.LENGS-10) THEN
            IND=LVMIN(ECLES,K)
            IF(ECL.LE.ECLES(IND))GO TO 40
          ELSE
            K=K+1!INCREMENT NUMBER OF CLUSTERS IF EVERYTHING OK
            IND=K
          END IF
        ELSE
          IF(K.GE.LENGS-10)THEN
            IND=LVMIN(ECLES,K)
          ELSE
            K=K+1!INCREMENT NUMBER OF CLUSTERS IF EVERYTHING OK
            IND=K
          END IF
        END IF ! end of new clusters
        DRIG = RADGRI(ISTACK) - R ! distance GRID-INTERACTION POINT
C  DEFINE THE CLUSTER COORDINATES IN THE TRD FRAME
        X= CPHOF(ISTACK)*XP+SPHOF(ISTACK)*YP
        Y=-SPHOF(ISTACK)*XP+CPHOF(ISTACK)*YP
        Z=ZP
        PHICL = ATAN2(-Y,-X) + PI
        IWIRE(IND) = PHICL /DPHIAN(ISTACK)+1
C  Introduce lateral smearing and splitting on 2 wires
        IF (DRIG.GT.0.)THEN!  Check that interaction point is in drift area
          XIN0=X
          YIN0=Y
          PHIN0 = PHICL
          IWI = IWIRE(IND)
          SIGT=DISPTR*DRIG
          CALL NORRAN(DPHI)
          DPHI=DPHI*SIGT/R
C  Smear X and Y
          X=XIN0-DPHI*YIN0
          Y=YIN0+DPHI*XIN0
          PHICL = ATAN2(-Y,-X) + PI
          IWIRE(IND) = PHICL /DPHIAN(ISTACK)+1
C          IF( IWIRE(IND).NE.IWI)THEN
          IF(PTRD.GE.8 .AND. IWIRE(IND).NE.IWI)THEN
            WRITE(LOUT,*)' wire before',IWI,' wire after', IWIRE(IND),
     + ' dphi', DPHI,' drift',DRIG
            WRITE(LOUT,*)' phi part-phi wire before',
     &        PHIN0 - PHIW(IWI,ISTACK)
            WRITE(LOUT,*)' phi part-phi wire after',
     &        PHICL - PHIW(IWI,ISTACK)
          END IF
        END IF
        IF(IWIRE(IND).EQ.NWIRE(ISTACK)+1)IWIRE(IND)=1
        ECLES(IND)=ECL
        IESCAP(IND)=0
        IF(IWIRE(IND).GT.NWIRE(ISTACK) .OR. IWIRE(IND).LE.0)THEN
          WRITE(LOUT,*)' PROBLEM_TRD IN CLUPOS'
          WRITE(LOUT,*)'XP,YP,ZP',XP,YP,ZP
          WRITE(LOUT,*)' X,Y,Z',X,Y,Z
          WRITE(LOUT,*)'IND,IWIRE,PHI',IND,IWIRE(IND),PHICL
          WRITE(LOUT,*)' PHIW',PHIW(IWIRE(IND),ISTACK),' ISTACK',ISTACK
          WRITE(LOUT,*)' IWIRE(',IND,')=',IWIRE(IND),'>',NWIRE(ISTACK)
          WRITE(LOUT,*)'X,Y,Z',X,Y,Z,' R',R,' PHICL',PHICL
          WRITE(LOUT,*)' IESCAP',IAUX(I),' OFSDPH,DPHIAN',
     &     OFSDPH(ISTACK),DPHIAN(ISTACK)
          K=K-1
          GO TO 90
        END IF
        DPHI = PHICL - PHIW(IWIRE(IND),ISTACK)
        CDPHI = COS(DPHI)
        SDPHI = SIN(DPHI)
C  Define drift distances in ampl. zone and in drift zone
        IF (DRIG.GT.0.) THEN
C  .......INTERACTION POINT IN DRIFT ZONE
          RR = RADGRI(ISTACK)
        ELSE
C  ....... INTERACTION POINT IN AMPL. ZONE
          DRIG = 0.
          RR = R
        END IF
        DSGA = SQRT(RR**2 + RADAN(ISTACK)**2
     +                      - 2.*RR*RADAN(ISTACK)*CDPHI)
        CLUDIS(IND,1) = DRIG
        CLUDIS(IND,2) = DSGA
C  CLUSTER REFERENCED WITH RESPECT TO THE ANODE WIRE: X ALONG THE RADIUS
C                                                     Y PERPENDICULAR
        RCDP=AMAX1(R*CDPHI,RADWIN(ISTACK))
        XCLES(IND) = -RADAN(ISTACK) + RCDP
        YCLES(IND) = R*SDPHI
        ZCLES(IND)=Z
        IESCAP(IND)=IESCAP(I)
C  COMPUTE DRIFT TIME (FORMERLY DONE IN CLUTIM)
        YL=ABS(YCLES(IND))
        TIMEC(IND)=TRDTIM(YL,XCLES(IND))
C +--------------------------------------+
C  COMPUTE CATHODE STRIP FOR THIS CLUSTER|
C +--------------------------------------+
        PHICAT = PHICL - OFSDPH(ISTACK) - DPHIDZ(ISTACK)*Z + TWOPI
        DPHIST = AMOD(PHICAT-OFSCAT(ISTACK),TWOPI)
        ISTRIP(IND) = DPHIST/DPHICA(ISTACK) + 1.0
        ISTRIP(IND) = MAX0(ISTRIP(IND),1)
        ISTRIP(IND) = MIN0(ISTRIP(IND),NSTRIP(ISTACK))
        DSTRIP(IND) = DPHIST/DPHICA(ISTACK) - FLOAT(ISTRIP(IND)) + 0.5
   90   CONTINUE
        IF(NNEW.NE.0)GO TO 40
        IF (NPRNT.LE.10. AND. PTRD.GE.10) THEN
          WRITE (LOUT,*) ' VOUT',VOUT(1),VOUT(2),VOUT(3)
          WRITE(LOUT,*)' VIN',VINT(1),VINT(2),VINT(3)
          WRITE (LOUT,*) 'XAV',XAV,'WIRE',IWIRE(IND),' TIME',TIMEC(IND)
          WRITE (LOUT,*)'X,Y,Z',X,Y,Z,' R ',R
          WRITE (LOUT,*)'DRIG',DRIG,'DSGA',DSGA,'PHICL',PHICL
          WRITE (LOUT,*)'XCLES,YCLES',XCLES(IND),YCLES(IND)
          WRITE(LOUT,*)'XP,YP,ZP',XP,YP,ZP
          WRITE(LOUT,*)'IND,IWIRE,PHI',IND,IWIRE(IND),PHICL
          WRITE(LOUT,*)' PHIW',PHIW(IWIRE(IND),ISTACK),' ISTACK',ISTACK
          WRITE(LOUT,*)' ISTRIP,DSTRIP',ISTRIP(IND),DSTRIP(IND)
          NPRNT = NPRNT + 1
        END IF
  100 CONTINUE
      NSMEAR=K
      RETURN
      END
