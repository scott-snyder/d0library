      SUBROUTINE Z_CATHODE (LZTRK,ZTRACK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute Z track in the 3 TRD layers using cathode
C-                         informations. The geometry of the TRD must have been
C-                         defined elsewhere. If used at DST level care must be
C-                         taken to define the anode plane radius
C-
C-   Inputs  : LZTRK  track link
C-   Outputs : ZTRACK(1,2,3)= Z of the track in TRD layer 1,2,3
C-   Controls:
C-
C-   Created   1-SEP-1993   A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ALPHA(3),RC(3),L0,SALPHA(3),CALPHA(3),WID(3)
      REAL PHI_TRACK,PHICAT,DPHIST,DPHI_SUR_DZ(3)
      INCLUDE 'D0$INC:GEOMTR.INC'
      INCLUDE 'D0$INC:GEOMTC.INC'
      INCLUDE 'D0$LINKS:IZZTRK.LINK/LIST'
      REAL ZCALC,ZC
      REAL DPHI,VIN(6),VOUT(6),PHI_CYL,ZTRACK(3)
      INTEGER I,IW,K,NAD,NC,NV,IFOIS,NA,N0,NTRY
      INTEGER J,JC,LOUT,TRUNIT,NCLA
      INTEGER IGT
      INTEGER LTHIT,GZTHIT,NTHIT,IAHT,IPHT,IWHT,IER
      INTEGER LDTRH,GZDTRH,NDTRH,NID,NUMCAT
      REAL VERSION,ETOT_AN,ETOT_CAT,SEC_UR(16,3),ECAT,TRD_ZPOS
      REAL SCAT,Z1,Z2
      REAL RW(100)
      INTEGER INTW(100)
      INTEGER IBIN
      REAL PI,TWOPI
      INTEGER LVMIN,LVMAX,DENSITY_OF_TRD_HITS,NHIT_LOCAL,WMIN,WMAX
      COMMON/CATH_FADC/FADC_CAT(128,256),CATH_HIT(256)
      REAL FADC_CAT
      LOGICAL CATH_HIT
      INTEGER N_AFTER_CUT,DWIRE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:worksp.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER ICH,LTRDT,LTPRL,JTRDT,EVENTI,EVENTO,NUMTR
      INTEGERLZTRK,LZFIT, LZTRH,GZFTRH,ICD,NCLH
      REAL ET,LIET,LIETN
      LOGICAL FIRST,MC_DATA,DOPRINT
      REAL S,ENERGY
      DATA FIRST/.TRUE./
      DATA ALPHA/.4143,-.5055,.8212/
      DATA RC/26.70,37.25,47.80/
      DATA WID/.6,.8,.79939/
      DATA L0/-84.8/
      DATA N0/1/
C----------------------------------------------------------------------
C
C
      IF(FIRST) THEN
        NID=0
        MC_DATA=.FALSE.
        PI =ACOS(-1.)
        TWOPI=2.*PI
        LOUT=TRUNIT()
        IF (IQ(LHEAD+1) .GT. 1000) MC_DATA = .TRUE.
        IFOIS=0
        EVENTI=0
        EVENTO=0
c        DOPRINT=.TRUE.
        DO I=1,3
C          CALPHA(I)=COS(ALPHA(I))
C          SALPHA(I)=SIN(ALPHA(I))
          CALPHA(I)=256.*WID(I)/(TWOPI*RC(I))
          SALPHA(I)=SQRT(1.-CALPHA(I)**2)
          IF(I.EQ.2)SALPHA(I)=-SALPHA(I)
C          WID(I)=TWOPI*RC(I)*CALPHA(I)/256.
          DPHI_SUR_DZ(I)=SALPHA(I)/(RC(I)*CALPHA(I))
        END DO
        FIRST=.FALSE.
      ENDIF
C  default values for Z TRD
      ZTRACK(1)=-1000.
      ZTRACK(2)=-1000.
      ZTRACK(3)=-1000.
      IF(LZTRK.LE.0)GO TO 999 ! request a track
      LTRDT=LQ(LZTRK-9)
      IF(LTRDT.LE.0)GO TO 999 ! request TRD on the track
      LZFIT=LQ(LZTRK-1)
C  Define track parameters
      VIN(1)   =Q(LZFIT+11)
      VIN(2)   =Q(LZFIT+12)
      VIN(3)   =Q(LZFIT+15)
      VIN(4)   =  Q(LZFIT+20) !cx
      VIN(5)   =  Q(LZFIT+22) !cy
      VIN(6)   =  Q(LZFIT+24) !cz
      DO 442 ICH=1,3
        LTPRL=LQ(LTRDT-ICH)
        IF(LTPRL.LE.0) GO TO  442 ! request a hit in the layer
        CALL EXTCYL(VIN,VOUT,RADAN(ICH),IGT)
        PHI_CYL=ATAN2(-VOUT(2),-VOUT(1))+PI
        CALL UNPACK_TPRL(LTPRL,VERSION,RW,INTW,IER)
        NA=INTW(4)
        NC=INTW(5)
        IF(NA*NC.LE.0)GO TO 442 ! Request anodes AND cathodes
        NCLA=INTW(6)
C   Compute wire with E max
        ZTRACK(ICH)=-1000.
        SCAT=0.
        ZTRACK(ICH)=0.
        DO  JC=1,NC
          J=INTW(50+NA+JC)
          ECAT=RW(50+NA+JC)
c          Z1=ZCALC(ICH,J,PHI_CYL,RADEXT(ICH))
C          IF(ABS(Z1).LE.l0)THEN ! Check if Z in TRD chamber
          PHICAT=PHI_CYL
          NTRY=0
          DPHIST=WID(ICH)*FLOAT(J-N0)
   10     ZC=-(DPHIST-RADEXT(ICH)*PHICAT*CALPHA(ICH))/SALPHA(ICH)+L0
          IF(ABS(ZC).GT.84.)THEN
            NTRY=NTRY+1
            PHICAT=PHICAT+TWOPI
            IF(NTRY.LT.2)GO TO 10
          END IF
c          PRINT*,'ZC',ZC,'Z1',Z1,' L0',L0,' ECAT',ECAT
          SCAT=SCAT+ECAT
          ZTRACK(ICH)=ZTRACK(ICH)+ZC*ECAT
C          END IF
        END DO
        ZTRACK(ICH)=ZTRACK(ICH)/SCAT
c        PRINT*,' ich',ICH,'zc',ZTRACK(ICH)
  442 CONTINUE
  999 RETURN
      END
