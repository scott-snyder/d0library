      SUBROUTINE CM3POS_PV(LCASH,WEIGHT_CUT,XBAR3,DBAR3,ETAPHI,DETAPHI,
     &  PV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates log-weighted center of gravity
C-                         position and dispersion for EM layer 3
C-                         working from CASH bank
C-
C-   Inputs  :  LCASH           link to CASH bank
C-              WEIGHT_CUT      Weighting factor (NIM A311(1992) p.130)
C-              PV              z of primary vertex
C-
C-   Outputs :  XBAR3(3)        position (x,y,z) of EM 3 shower center
C-              DBAR3(3)        dispersion (x,y,z) of EM3 shower
C-              ETAPHI(3)       position (eta,phi,theta) of EM3 shower
C-              DETAPHI(3)      dispersion (eta,phi,theta) of EM3 shower
C-
C-              Note that one could also use r,phi,z coordinates
C-
C-   Controls: none
C-
C-   Created   6-FEB-1992   Norman A. Graf
C-   Updated  24-MAR-1992   Norman A. Graf  ADDED ETAPHI,DETAPHI 
C-   Updated  20-SEP-1992   Rajendran Raja  REMOVED ZLINKC.INC(LCASH) 
C-   Updated  30-SEP-1992   Natalie  Roe    Use tuned parameters for log weight
C-   Updated  10-JUN-1993   Orin Dahl       Correct for radial offset in ECEM
C-   Updated   8-JUL-1993   Meenakshi Narain   Turn off radial offset in ECEM
C-                                          for MCDATA
C-   Updated   4-OCT-1993   Lupe Howell     Moved EZRSET so it willonly be done
C-                                          if RCP file defined.
C-   Updated  15-DEC-1993   Orin Dahl       Fix bug in wrapping phi at tower 10
C-   Updated  23-FEB-1994   Azriel Goldschmidt USE CM3POS,  adding input primary
C-                                          vertex to make Theta correction in
C-                                          CC. Change implementation of
C-                                          algorithm (accumulated cell).
C-   Updated  10-JUN-1995   Norman A. Graf  Added LAYER_HOT to CEMDPTH call
C-                                          and modified call to CELXYZ when
C-                                          recalculating X and Y.
C-                                     
C-   Updated   5-AUG-1995   Meenakshi Narain   Call CEMDPTH_LAYER
C-                                          to fill LAYER_HOT array
C-   Updated  11-FEB-1995   sss - fix division-by-zero error for clusters
C-                                  with z=0.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CEMPRF.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      REAL PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC'
      REAL XBAR3(3),DBAR3(3),SUMEWT(3),ECLUS
      REAL ETAPHI(3),DETAPHI(3),E(4),PHI_CELL,THETA_CELL,ETA_CELL
      INTEGER IETA_HOT(5),IPHI_HOT(5),LAYER_HOT(5)
      REAL EDPTH(5),PDPTH(5),ENERGY_HOT(5)
      REAL WT,WEIGHT_CUT,ENERGY
      REAL    W0_ECEM_PHI(6),W0_ECEM_ETA(6),W0_CCEM_PHI(3,5),
     &        W0_CCEM_ETA(3,5),W0(3)
      REAL    ECEM_OFFSET_THETA, ECEM_OFFSET_LN_E0
      REAL    CC_THETA_A1,CC_THETA_A3,CC_THETA_A5,CC_THETA_A7
      INTEGER INDEX1,INDEX2,IETA_3
      REAL    XX,YY,ZZ,EM3WTOT
      REAL   R_CCEM,Z_ECEM 
      REAL    TAN_TH, DEL_TANTH
      REAL    PV
      REAL    PHI_WRAP
      REAL    Z_NEW,PHI_NEW,SUM_WEI,SUM_PHI,WT_TEMP,SUM_Z
      REAL    SUM_OVER_ETA(256),SUM_OVER_PHI(160)
      REAL    Z_CELLS(160),PHI_CELLS(256)
      REAL    X_NEW(3),DZ,THETA
      INTEGER N_PHI_C(256),N_Z_C(160),IETALY3,IPHILY3
      INTEGER I,NCH,POINTER,IOK,IER
      INTEGER  ETAI,PHII,ILYR
      INTEGER PACKED_WORD,LCASH,RECORD_TYPE
      LOGICAL MCDATA
      LOGICAL FIRST/.TRUE./
      DATA W0_ECEM_PHI/ 5.3,4.9,4.5,4.3,4.9,4.5/
      DATA W0_ECEM_ETA/ 4.0,3.6,3.4,3.2,3.2,3.2/
      DATA W0_CCEM_PHI/ 4.2,4.2,4.4,4.3,4.4,4.0,4.5,4.6,4.0,
     &                  4.7,4.6,5.0,4.7,4.6,5.2/
      DATA W0_CCEM_ETA/ 4.0,3.2,3.0,4.3,3.4,2.8,4.5,3.4,2.8,
     &                  4.7,3.4,2.6,4.7,3.4,2.4/
      DATA ECEM_OFFSET_THETA/ 4.28E-4 /
      DATA ECEM_OFFSET_LN_E0/ -11.064 /
      DATA CC_THETA_A1/-1.2525/
      DATA CC_THETA_A3/2.45835/
      DATA CC_THETA_A5/-3.08253/
      DATA CC_THETA_A7/3.18251/
C
C----------------------------------------------------------------------
C
C ****  TRY NEW EM LAYER 3 X,Y POSITION FINDING HERE...
C
C
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('CAPHEL_RCP')
        CALL EZERR(IER)
        IF (IER.EQ.0) THEN
          CALL EZGET('W0_ECEM_PHI',W0_ECEM_PHI,IER)
          CALL EZGET('W0_ECEM_ETA',W0_ECEM_ETA,IER)
          CALL EZGET('W0_CCEM_PHI',W0_CCEM_PHI,IER)
          CALL EZGET('W0_CCEM_ETA',W0_CCEM_ETA,IER)
          CALL EZGET('ECEM_OFFSET_THETA',ECEM_OFFSET_THETA,IER)
          CALL EZGET('ECEM_OFFSET_LN_E0',ECEM_OFFSET_LN_E0,IER)
          CALL EZGET('CC_THETA_A1',CC_THETA_A1,IER)
          CALL EZGET('CC_THETA_A3',CC_THETA_A3,IER)
          CALL EZGET('CC_THETA_A5',CC_THETA_A5,IER)
          CALL EZGET('CC_THETA_A7',CC_THETA_A7,IER)
          CALL EZRSET
        ELSE
          CALL ERRMSG(' NO_CAPHEL_RCP','CM3POS',
     &        ' USING DEFAULT PARAMETERS FOR WEIGHTS','W')
        ENDIF
      ENDIF

      W0(1)=WEIGHT_CUT
C
c  Use ETOT of shower and etamax in layer 3 to determine weight
c  parameter in log position finding
C
      CALL CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,
     &  ENERGY_HOT)
      CALL CEMDPTH_LAYER(LAYER_HOT)   ! entry point in CEMDPTH
C
C IF NO ENERGY IN EM3, QUIT NOW
C
      EM3WTOT = EDPTH(3)
      IF(EM3WTOT .LE. 0.) THEN
       DO I = 1,3
        XBAR3(I) = -999.
        DBAR3(I) = -999.
        ETAPHI(I)  = -999.
        DETAPHI(I) = -999.
        SUMEWT(I) = -999.
       ENDDO
       GOTO 999
      ENDIF

        ECLUS = 0.
        DO I = 1,5
          ECLUS = ECLUS + EDPTH(I)
        ENDDO
C
        IETA_3=ABS(IETA_HOT(3))
        IF(IETA_3.GE.14)THEN ! ECEM
         INDEX1= (IETA_3-12)/3
         IF(INDEX1.LT.1)INDEX1=1
         IF(INDEX1.GT.6)INDEX1=6
         W0(2)=W0_ECEM_PHI(INDEX1)
         W0(3)=W0_ECEM_ETA(INDEX1)
        ELSE  ! CCEM
         IF(IETA_3.LE.2)THEN
          INDEX1=1
         ELSEIF(IETA_3.LE.6)THEN
          INDEX1=2
         ELSE
          INDEX1=3
         ENDIF
         IF(ECLUS.LE.15.)THEN
          INDEX2=1
         ELSEIF(ECLUS.LE.25.)THEN
          INDEX2=2
         ELSEIF(ECLUS.LE.40.)THEN
          INDEX2=3
         ELSEIF(ECLUS.LE.75.)THEN
          INDEX2=4
         ELSE
          INDEX2=5
         ENDIF
         W0(2)=W0_CCEM_PHI(INDEX1,INDEX2)
         W0(3)=W0_CCEM_ETA(INDEX1,INDEX2)
        ENDIF
        PHI_WRAP = (TWOPI/64.) * REAL( MOD(IPHI_HOT(3)+32,64) )
C
      NCH    = IQ(LCASH+2)
      POINTER=1
      DO I = 1,3
        XBAR3(I) = 0.
        DBAR3(I) = 0.
        ETAPHI(I)  = 0.
        DETAPHI(I) = 0.
        SUMEWT(I) = 0.
      ENDDO
      DO I=1,256
        SUM_OVER_ETA(I) = 0.
        N_PHI_C(I) = 0
        PHI_CELLS(I) = 0.
      ENDDO
      DO I=1,160
        SUM_OVER_PHI(I) = 0.
        N_Z_C(I) = 0
        Z_CELLS(I) = 0. 
      ENDDO
      POINTER = 1

      DO I = 1,NCH
        POINTER = POINTER+2
        PACKED_WORD = IQ(LCASH+POINTER)
        ENERGY = Q(LCASH+POINTER+1)
        CALL CAEP_INDICES(PACKED_WORD,ETAI,PHII,ILYR)
          IF(ILYR.GE.LYEM3A .AND. ILYR.LE.LYEM3D) THEN
            CALL CELXYZ(ETAI,PHII,ILYR,XX,YY,ZZ,IOK)
C
            E(1) = XX
            E(2) = YY
            E(3) = ZZ
            E(4) = +1.
            CALL ETOETA(E,PHI_CELL,THETA_CELL,ETA_CELL)
            IF ( PHI_CELL.LE.PHI_WRAP )  PHI_CELL = TWOPI + PHI_CELL
C going to do CC special stuff
C IETALY3 is 80 at the first EM cell on the negative side 81 on the positive
            IETALY3 = INT((ETA_CELL + 4.) / 0.049) + 1
C IPHILY3 is 1 at the first, phi 0 < 3 pi, 256
            IPHILY3 = INT(PHI_CELL / 0.049) + 1
            SUM_OVER_ETA(IPHILY3) = SUM_OVER_ETA(IPHILY3) + ENERGY
            SUM_OVER_PHI(IETALY3) = SUM_OVER_PHI(IETALY3) + ENERGY

C Need to average over phi and eta ( because of geometry imperfections)    
            N_PHI_C(IPHILY3) = N_PHI_C(IPHILY3) + 1
            PHI_CELLS(IPHILY3) = PHI_CELL + PHI_CELLS(IPHILY3) 
            N_Z_C(IETALY3) = N_Z_C(IETALY3) + 1 
            Z_CELLS(IETALY3)   = ZZ + Z_CELLS(IETALY3)
C
            IF(ENERGY .GT. 0) THEN
              WT = W0(1) + LOG(ENERGY/EM3WTOT)
            ELSE
              WT = 0.
            ENDIF
            IF (WT.LT.0.) WT = 0.
            SUMEWT(1) = SUMEWT(1) + WT
            XBAR3(1) = XBAR3(1) + WT*XX
            XBAR3(2) = XBAR3(2) + WT*YY
            XBAR3(3) = XBAR3(3) + WT*ZZ
            DBAR3(1) = DBAR3(1) + WT*(XX*XX)
            DBAR3(2) = DBAR3(2) + WT*(YY*YY)
            DBAR3(3) = DBAR3(3) + WT*(ZZ*ZZ)
C
            IF(ENERGY .GT. 0) THEN
              WT = W0(2) + LOG(ENERGY/EM3WTOT)
            ELSE
              WT = 0.
            ENDIF
            IF (WT.LT.0.) WT = 0.
            SUMEWT(2) = SUMEWT(2) + WT
            ETAPHI(2) = ETAPHI(2) + WT*PHI_CELL
            DETAPHI(2) = DETAPHI(2) + WT*PHI_CELL*PHI_CELL

            IF(ENERGY .GT. 0) THEN
              WT = W0(3) + LOG(ENERGY/EM3WTOT)
            ELSE
              WT = 0.
            ENDIF
            IF (WT.LT.0.) WT = 0.
            SUMEWT(3) = SUMEWT(3) + WT
            ETAPHI(1) = ETAPHI(1) + WT*ETA_CELL
            ETAPHI(3) = ETAPHI(3) + WT*THETA_CELL
            DETAPHI(1) = DETAPHI(1) + WT*ETA_CELL*ETA_CELL
            DETAPHI(3) = DETAPHI(3) + WT*THETA_CELL*THETA_CELL
C
          ENDIF
        ENDDO  ! loop over channels

        IF(SUMEWT(1).GT.0) THEN
          DO I = 1,3
            XBAR3(I) = XBAR3(I)/SUMEWT(1)
            DBAR3(I) = DBAR3(I)/SUMEWT(1)
            DBAR3(I) = DBAR3(I) - XBAR3(I)**2
          ENDDO
        ELSE
          DO I = 1,3
            XBAR3(I) = -999.
            DBAR3(I) = -999.
          ENDDO
        ENDIF
C
        IF(SUMEWT(2).GT.0.) THEN
            ETAPHI(2)  = ETAPHI(2)/SUMEWT(2)
            IF(ETAPHI(2).GT.TWOPI)ETAPHI(2)=ETAPHI(2)-TWOPI
            DETAPHI(2) = DETAPHI(2)/SUMEWT(2)
            DETAPHI(2) = DETAPHI(2) - ETAPHI(2)**2
        ELSE
            ETAPHI(2)=-999.
            DETAPHI(2)=-999.
        ENDIF
C
        IF(SUMEWT(3).GT.0.) THEN
            ETAPHI(1)  = ETAPHI(1)/SUMEWT(3)
            DETAPHI(1) = DETAPHI(1)/SUMEWT(3)
            DETAPHI(1) = DETAPHI(1) - ETAPHI(1)**2
            ETAPHI(3)  = ETAPHI(3)/SUMEWT(3)
            DETAPHI(3) = DETAPHI(3)/SUMEWT(3)
            DETAPHI(3) = DETAPHI(3) - ETAPHI(1)**2
        ELSE
            ETAPHI(1)=-999.
            DETAPHI(1)=-999.
            ETAPHI(3)=-999.
            DETAPHI(3)=-999.
        ENDIF
C calculate the position using accumulated cells, first get average
C phi and z for a given row,column FOR CC ONLY
        Z_NEW = -9999.
        PHI_NEW = -9999.
        X_NEW(1) = -9999.
        X_NEW(2) = -9999.
        X_NEW(3) = -9999.
        IF ( ABS(ETAPHI(1)).LT.1.4 )  THEN
         PHI_NEW = 0.
         Z_NEW   = 0.
         SUM_WEI = 0.
         SUM_PHI = 0.
         DO I=1,256
            IF(N_PHI_C(I).GE.1.AND.SUM_OVER_ETA(I).GT.0)THEN
             PHI_CELLS(I) = PHI_CELLS(I) / N_PHI_C(I)
             WT_TEMP = MAX(0.,W0(1)+LOG(SUM_OVER_ETA(I)/EM3WTOT))
             SUM_WEI = SUM_WEI + WT_TEMP
             SUM_PHI = SUM_PHI + WT_TEMP * PHI_CELLS(I)
           ENDIF
         ENDDO
         IF(SUM_WEI.GT.0) PHI_NEW = SUM_PHI / SUM_WEI
         IF ( PHI_NEW .GE. TWOPI) PHI_NEW = PHI_NEW - TWOPI
         SUM_WEI = 0.
         SUM_Z = 0.
         DO I=1,160
           IF(N_Z_C(I).GE.1.AND.SUM_OVER_PHI(I).GT.0)THEN
             Z_CELLS(I) = Z_CELLS(I) / N_Z_C(I)
             WT_TEMP = MAX(0.,W0(1)+LOG(SUM_OVER_PHI(I)/EM3WTOT))
             SUM_WEI = SUM_WEI + WT_TEMP
             SUM_Z = SUM_Z + WT_TEMP * Z_CELLS(I)
           ENDIF
         ENDDO
         IF(SUM_WEI.GT.0) Z_NEW = SUM_Z / SUM_WEI
         CALL CELXYZ(IETA_HOT(3),IPHI_HOT(3),LAYER_HOT(3),XX,YY,ZZ,IOK)
         IF(IOK.EQ.0)THEN
           R_CCEM=SQRT(XX**2+YY**2)
           X_NEW(1)=R_CCEM*COS(PHI_NEW)
           X_NEW(2)=R_CCEM*SIN(PHI_NEW)
           X_NEW(3)=Z_NEW
         ELSE
           DO I=1,3
             X_NEW(I)=-9999.
           ENDDO
         ENDIF
C Now add theta correction to the CC USING THE PV
C This has to be applied to MC as well since that's were the correction came
C from
         THETA = ATAN2 (R_CCEM,(Z_NEW-PV))
         IF (THETA.LT.0.) THETA = THETA + PI
         DZ = (THETA - HALFPI)*CC_THETA_A1 + ((THETA - HALFPI)**3)
     &     *CC_THETA_A3 + ((THETA - HALFPI)**5)*CC_THETA_A5
     &     + ((THETA - HALFPI)**7)*CC_THETA_A7
         Z_NEW    = Z_NEW - DZ
         X_NEW(3) = Z_NEW
        ENDIF
C CORRECT FOR RADIAL OFFSET IN ECEM
        MCDATA = .FALSE.
        RECORD_TYPE = IQ(LHEAD+1)
        IF (RECORD_TYPE.GE.1005) MCDATA = .TRUE.
        IF (.NOT.MCDATA) THEN
          IF ( ABS(ETAPHI(1)).GT.1.4 )  THEN
            TAN_TH    = TAN(ETAPHI(3))
            DEL_TANTH = TAN_TH * ECEM_OFFSET_THETA
     &                * ( LOG(ECLUS) - ECEM_OFFSET_LN_E0 )
            TAN_TH    = TAN_TH - DEL_TANTH
            ETAPHI(3) = ATAN(TAN_TH)
            IF ( TAN_TH.LT.0. )  ETAPHI(3) = ETAPHI(3) + PI
            ETAPHI(1) = - LOG( TAN( ETAPHI(3)/2. ) )
          ENDIF
        ENDIF

        IF(SUMEWT(2)*SUMEWT(3).GT.0.)THEN
         IF(IETA_3.LT.14)THEN ! CCEM
C RECALCULATE XBAR USING THETA AND PHI AT RADIUS R_CCEM
          CALL CELXYZ(IETA_HOT(3),IPHI_HOT(3),LAYER_HOT(3),XX,YY,ZZ,IOK)
          IF(IOK.EQ.0)THEN
           R_CCEM=SQRT(XX**2+YY**2)
           XBAR3(1)=R_CCEM*COS(ETAPHI(2))
           XBAR3(2)=R_CCEM*SIN(ETAPHI(2))
           XBAR3(3)=R_CCEM/TAN(ETAPHI(3))
          ELSE
           DO I=1,3
            XBAR3(I)=-999.
           ENDDO
          ENDIF
         ELSE  ! ECEM
C RECALCULATE XBAR USING THETA AND PHI AT Z
          CALL CELXYZ(IETA_HOT(3),IPHI_HOT(3),LAYER_HOT(3),XX,YY,ZZ,IOK)
          IF(IOK.EQ.0)THEN
           Z_ECEM= ZZ
           XBAR3(1)=Z_ECEM*TAN(ETAPHI(3))*COS(ETAPHI(2))
           XBAR3(2)=Z_ECEM*TAN(ETAPHI(3))*SIN(ETAPHI(2))
          ELSE
           DO I=1,3
            XBAR3(I)=-999.
           ENDDO
          ENDIF
         ENDIF
        ELSE ! at least one of wts=0.
         DO I=1,3
          XBAR3(I)=-999.
         ENDDO
        ENDIF
CCC temporary replace xbar3 array with new values for CC
        IF ( ABS(ETAPHI(1)).LT.1.4 )  THEN
CC phi resolution is better with previous method in plate MC
CC         XBAR3(1)=X_NEW(1)
CC         XBAR3(2)=X_NEW(2)
         XBAR3(3)=X_NEW(3)
        ENDIF
  999 RETURN
      END
