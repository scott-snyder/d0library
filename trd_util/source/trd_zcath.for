      REAL FUNCTION TRD_ZCATH(LAYER,NUM_CATH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C- Z=((Nc-1)*w-r*(phi-phi0))/sin(alpha)+Z0
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-JUL-1993   A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
C      IMPLICIT NONE
      REAL ALPHA(3),RC,L0,SALPHA(3),CALPHA(3),WID(3)
      REAL PHI_TRACK,PHICAT,DPHIST,DPHI_SUR_DZ(3),Z_R,OFSC(3)
      REAL RADIN(3),RADGRI(3),DRGRAN(3),RADAN(3)
     +            ,RADEXT(3),DISTAN(3),OFSDPH(3),DPHIAN(3)
     +            ,DRWINA(3),DRWING(3)
      INTEGER NWIRE(3)
      REAL  WIDCAT(3),DPHICA(3),DISTCA(3),DPHIDZ(3),
     1                OFSCAT(3)
      INTEGER NSTRIP(3)
      INCLUDE 'D0$INC:TRD_PHI_Z.INC'
      REAL ALPHA_CATH(3),R_EFFECT(3),WID_CATH(3),DZ(3)
      REAL PI,TWOPI,DDR(3),DR(3),DWID(3),DALPHA(3)
      INTEGER I,LAYER,N0,NUM_CATH,NTRY,LOUT,TRUNIT,IFOIS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA ALPHA/23.735,-28.965,47.013/
      DATA WID/.6,.8,.79939/
      DATA L0/-84.8/
      DATA N0/1/
C      WRITE(73,*)' enter z1,first ',FIRST
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
        PI=ACOS(-1.)
        TWOPI=2.*PI
        RADAN(1)=26.30000
        RADEXT(1)=  26.705
        RADAN(2)=36.85
        RADEXT (2)=  37.255
        RADAN(3) = 47.4
        RADEXT (3)=47.805
        PI =ACOS(-1.)
        DO ICH=1,3
          DZ(ICH)=0.
          DDR(ICH)=0.
          DWID(ICH)=0.
          DALPHA(ICH)=0.0
C          ALPHA_cath(ich)=ALPHA(ICH)
          WID_CATH(ICH)=WID(ICH)
          R_EFFECT(ICH)=RADEXT(ICH)
C        WRITE(72,*)' wid_cath',WID_CATH(ICH)
          ALPHA(ICH)=ALPHA(ICH)*TWOPI/360.+DALPHA(ICH)
          CALPHA(ICH)=COS(ALPHA(ICH))
          SALPHA(ICH)=SIN(ALPHA(ICH))
        END DO
 3567   FORMAT('in z1 ich',I2,' r anode',G10.4,' radext', G10.4)
C        RC=RADEXT(LAYER)+ddr(layer)
        IFOIS=0
        DPHI=0.
        PHI0=DPHI
C        phi0=0.
      END IF
      IFOIS=IFOIS+1
      PHICAT=PHI_TRD(LAYER)
      Z1=-1000.
      NTRY=0
      DPHIST=WID(LAYER)*(NUM_CATH-.5)
      Z0=L0+DZ(LAYER)
   10 Z1=-(DPHIST-R_EFFECT(LAYER)*PHICAT*CALPHA(LAYER))/SALPHA(LAYER)+Z0
C      IF(IFOIS.LE.10)THEN
C        WRITE(LOUT,*)' ntry',NTRY,' z1',Z1
C      END IF
      IF(ABS(Z1).GT.84..AND.NTRY.EQ.0)THEN
        NTRY=1
        PHICAT=PHICAT+TWOPI
        GO TO 10
      END IF
      IF(ABS(Z1).GT.84..AND.NTRY.EQ.1)THEN
        NTRY=2
        PHICAT=PHICAT-2*TWOPI
        GO TO 10
      END IF
C----------------------------------------------------------------------
      TRD_ZCATH=Z1
  999 RETURN
      END
