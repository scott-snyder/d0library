      SUBROUTINE MUTFLT(IMUOT,DELT,EDELT,CHISQ,NDOF,IRC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : float the tzero and find best track fit
C-
C-   Inputs  : IMUOT = index of muot bank for track of interest
C-   Outputs : DELT = time shift (ns) of preferred fit
C-             EDELT = error on DELT
C-             CHISQ = chisquared of floated fit 
C-             NDOF  = degrees-of-freedom of floated fit 
C-             IRC = return code (0=good) 
C-   Controls: 
C-
C-   Created  28-JUL-1992   Darien R. Wood
C-   Modified 12-Apr-1993   DRW, add second pass with finer time steps
C-   Updated  17-APR-1993   Daria Zieminska  CALL EZRSET 
C-   Modified 24-May-1993   DRW, use only majority orientation for
C-                          mixed orientation tracks    
C-    DH 4/94 add quad to MUFTBD call
C     DH 8/94 allow for NTIME=0
C-    DW 7/95 fill initial values of DELT, etc. even when IRC=-1
C-    DW 8/95 check on failure of MUFTBD
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NHMAX,NTMAX
      REAL TBIN_COARSE,TBIN_FINE
      PARAMETER(NHMAX=40)
      PARAMETER(NTMAX=30)
      PARAMETER(TBIN_COARSE=50.)
      PARAMETER(TBIN_FINE=20.)
      INTEGER IMUOT,NDOF,IRC
      REAL DELT,EDELT,CHISQ
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZMUOT,MUQUAD
      REAL XYZLO(3),XYZLI(3),XYZGO(3),XYZGI(3),DCI(3),DCO(3)
      INTEGER IQUAD
      REAL SLBI,SLBO,SLNBI,SLNBO
      REAL XGBC,ZG,XGA,YG,YGA
      REAL TFLOAT,WLEN,TCOR
      INTEGER IT0,IHIT,NHITS,LP,IADD,NMOD,NPLN,NWIR,IERR_MUADD,IERR_MTP
      INTEGER LMUOT,LMHTT,LMUOH,GZMUOH,JHIT
      INTEGER IFLAG(NHMAX),JADD(NHMAX),IOUT(NHMAX)
      REAL XDR(2,NHMAX),ZWIR(NHMAX),XWIR(NHMAX)
      REAL TIME_HIT(2,NHMAX),ANGLE_HIT(NHMAX)
      INTEGER IFL_HIT(2,NHMAX),NMOD_HIT(NHMAX),IVER,NV,NH_GOOD
      REAL TSVAL(NTMAX),CSVAL(NTMAX),TBEST,ETBEST,CSBEST
      INTEGER NDVAL(NTMAX),NTIMES,NDBEST,IRC_TFB
      REAL XFI,ZFI,SLFI,XFO,ZFO,SLFO    ! output fit parameters
      REAL QFI,QFO,QFIO                 ! fit quality factors
C                                       ! (sqrt(chisq/dof))
      REAL XYZW(3),XYP(2),VECT(3),VERTEX(3),VA,VB,VC
      REAL COST,ANGLE,TIME,MUDRFT
      INTEGER LAYER,MULAYR,K,IOR,IODD,NTIME
      INTEGER MUVERT_OLD,MUVERT_NEW,IER,IVTX
      REAL XTD,ZBEND
      REAL T0_CENT,TBIN
      INTEGER IT0_MAX,IPASS,MOD_QUAD
C
      EXTERNAL GZMUOT,MULAYR,MUDRFT,GZMUOH,MUQUAD
C
C initail values of subroutine output variables
      IRC = -1
      DELT = -9999.
      EDELT = 0.
      CHISQ = 0.
      NDOF = 0
C special treatment for cosmics so that the TOF correction is not
C biased by previously found track parameters
      CALL EZPICK('MURECO_RCP')
      CALL EZGET('MUVERT',MUVERT_OLD,IER)
      MUVERT_NEW = MUVERT_OLD
      IF(MUVERT_OLD.EQ.1) THEN
        MUVERT_NEW = 4
        CALL EZSET('MUVERT',MUVERT_NEW,IER)
      ENDIF  
      CALL EZRSET
      LMUOT = GZMUOT(IMUOT)
      IF(LMUOT.LE.0) GOTO 999
      NHITS = IQ(LMUOT+1)
      IF(NHITS.LE.3) GOTO 999
      LMHTT = LQ(LMUOT-1)
      IF(LMHTT.LE.0) GOTO 999
      LMUOH = GZMUOH(0)
      IF(LMUOH.LE.0) GOTO 999
C get local parameters of track
      IQUAD = IQ(LMUOT+3)
      CALL UCOPY(Q(LMUOT+8),XYZGI,3)
      CALL UCOPY(Q(LMUOT+14),DCI,3)
      CALL UCOPY(Q(LMUOT+11),XYZGO,3)
      CALL UCOPY(Q(LMUOT+17),DCO,3)
C convert to local parameters
      CALL MUUNRT(IQUAD,DCI,DCO,XYZGI,XYZGO,
     &  SLBI,SLNBI,SLBO,SLNBO,XYZLI,XYZLO)
      XGBC = XYZLO(1)
      ZG = XYZLO(3)
      XGA = XYZLI(1)
      YG = XYZLO(2)
      YGA = XYZLI(2)
C
C   LOOP OVER HITS on track and fill data for fitting
      CALL VZERO(XWIR,NHMAX)
      CALL VZERO(ZWIR,NHMAX)
      CALL VZERO_i(IOUT,NHMAX)
      CALL VZERO_i(IFL_HIT,2*NHMAX)
      CALL VZERO(TIME_HIT,2*NHMAX)
      CALL VZERO(ANGLE_HIT,NHMAX)
      CALL VZERO_i(NMOD_HIT,NHMAX)
      NH_GOOD = 0
      DO IHIT=1,NHITS
        JHIT=IQ(LMHTT+5*(IHIT-1)+2)
        LP = LMUOH + 28*(JHIT-1)
        IADD=IQ(LP+1)       ! WIRE ADDRESS
        CALL MUADD(IADD,NMOD,NPLN,NWIR,IERR_MUADD)
        IF(IERR_MUADD.EQ.0) THEN
C use only modules in the majority (track) orientation
          MOD_QUAD = MUQUAD(NMOD)
          IF(MOD_QUAD.EQ.IQUAD) THEN
            LAYER=MULAYR(NMOD)
            IF(LAYER.EQ.1) THEN      ! A LAYER
              COST=1./SQRT(1.+SLBI**2)
            ELSE    ! BC LAYER
              COST=1./SQRT(1.+SLBO**2)
            ENDIF
            ANGLE=ACOS(COST)
            WLEN=Q(LP+24)
C do projection
            CALL MUTPRO(JHIT,SLBO,SLBI,SLNBO,SLNBI,ZG,XGBC,
     &          XGA,YG,YGA,XYZW,XYP,IERR_MTP)
              NTIME=IQ(LP+6)
            IF(IERR_MTP.EQ.0.AND.NTIME.GE.1) THEN
              NH_GOOD = NH_GOOD + 1
              XWIR(NH_GOOD) = XYZW(1)
              ZWIR(NH_GOOD) = XYZW(3)
C look at bend view coordinates
              VECT(1)=Q(LP+21)
              VECT(2)=Q(LP+22)
              VECT(3)=Q(LP+23)
              XTD=XYP(2)-XYZW(2)
              IOR=IQ(LP+5)           ! ORIENTATION
              IODD=1                   ! ODD/EVEN CELL
              IF(MOD(NWIR,2).EQ.0) IODD=0
              CALL MUTCOR(IOR,IODD,VECT,XTD,WLEN,TCOR)   ! TIME OF FLIGHT
              DO K=1,NTIME
                TIME=Q(LP+8+K)-TCOR                ! DRIFT TIME PLUS TOF
                TIME_HIT(K,NH_GOOD) = TIME
                IFL_HIT(K,NH_GOOD) = 1
              ENDDO                                 ! END OF T1,T2 LOOP
              JADD(NH_GOOD) = IADD
              ANGLE_HIT(NH_GOOD) = ANGLE
              NMOD_HIT(NH_GOOD) = NMOD
            ENDIF
          ENDIF
        ENDIF
      ENDDO     ! END OF POINTS LOOP  
C
C if enough good points, try various t0's
      IF(NH_GOOD.GE.3) THEN
C set up for bend-view fit
        CALL MUZBND(IQUAD,ZBEND)       ! MAGNET BEND POINT
        CALL VERXYZ(IVER,VERTEX,NV)
        IF(IQUAD.LE.4) THEN        ! CENTRAL
          VA=VERTEX(3)
          VB=0.
          VC=0.
        ELSE
          VA=0.
          VB=VERTEX(3)
          VC=0.
        ENDIF
C loop over passes, coarse then fine
        DO IPASS = 1,2
          IF(IPASS.EQ.1) THEN
            T0_CENT = 0.
            IT0_MAX = 10
            TBIN = TBIN_COARSE
          ELSE  
            IT0_MAX = 5
            TBIN = TBIN_FINE
          ENDIF  
C loop over floating t0's
          NTIMES = 0
          DO IT0=-IT0_MAX,IT0_MAX
            TFLOAT = TBIN*FLOAT(IT0) + T0_CENT
            CALL VZERO(XDR,2*NHMAX)
            DO IHIT=1,NH_GOOD
              ANGLE = ANGLE_HIT(IHIT)
              NMOD = NMOD_HIT(IHIT)
              IFLAG(IHIT) = 1
              DO K=1,2
                IF(IFL_HIT(K,IHIT).GT.0) THEN
C add float time
                  TIME = TIME_HIT(K,IHIT) + TFLOAT
                  IF(TIME.GT.-50. .AND. TIME.LT.900.) THEN
                    IFLAG(IHIT) = 0           ! use point in fit
                  ENDIF  
                  XDR(K,IHIT)=MUDRFT(TIME,ANGLE,NMOD)    ! DRIFT distance
                ELSE  
                  XDR(K,IHIT) = 999999.
                ENDIF
              ENDDO
            ENDDO  
C
C do bend-view fit
            CALL MUFTBD(NH_GOOD,IQUAD,IFLAG,ZBEND,VA,VB,JADD,ZWIR,XWIR,
     &      XDR,1.,XFO,ZFO,SLFO,XFI,ZFI,SLFI,QFO,QFI,QFIO,IOUT,IVTX)
            NDOF = -2
            DO IHIT=1,NH_GOOD
              IF(IOUT(IHIT).NE.0) NDOF = NDOF + 1
            ENDDO
C check if fit failed
            IF(QFIO.GT.9000.) THEN
              NDOF = -2
            ENDIF
C look for best chisquared
            NTIMES = NTIMES + 1
            TSVAL(NTIMES) = TFLOAT
            CSVAL(NTIMES) = FLOAT(NDOF)*(QFIO**2)
            NDVAL(NTIMES) = NDOF
          ENDDO ! END OF FLOAT TIME LOOP  
          IF(IPASS.EQ.1) THEN
C choose range for finer T0 search
            IF(NTIMES.GE.2) THEN
              CALL TFBEST(NTIMES,TSVAL,CSVAL,NDVAL,TBEST,ETBEST,
     &         CSBEST,NDBEST,IRC_TFB)
              IF(IRC_TFB.GE.0) THEN
                T0_CENT = TBEST
              ENDIF
            ENDIF  
C second pass, get final floating time
          ELSE
            DELT = 0.
            EDELT = 0.
            IF(NTIMES.GE.2) THEN
              CALL TFBEST(NTIMES,TSVAL,CSVAL,NDVAL,TBEST,ETBEST,
     &      CSBEST,NDBEST,IRC_TFB)
              IRC = IRC_TFB
              DELT = TBEST
              EDELT = ETBEST
              CHISQ = CSBEST
              NDOF = NDBEST
            ENDIF
          ENDIF
        ENDDO                           ! end of pass loop
      ENDIF
C----------------------------------------------------------------------
  999 CONTINUE
      IF(MUVERT_NEW.NE.MUVERT_OLD) THEN
        CALL EZPICK('MURECO_RCP')
        CALL EZSET('MUVERT',MUVERT_OLD,IER)
        CALL EZRSET
      ENDIF  
      RETURN
      END
