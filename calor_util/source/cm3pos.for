      SUBROUTINE CM3POS(LCASH,WEIGHT_CUT,XBAR3,DBAR3,ETAPHI,DETAPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates log-weighted center of gravity
C-                         position and dispersion for EM layer 3
C-                         working from CASH bank
C-
C-   Inputs  :  LCASH           link to CASH bank
C-              WEIGHT_CUT      Weighting factor (NIM A311(1992) p.130)
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
C-   Updated  10-JUN-1995   Norman A. Graf  Added LAYER_HOT to CEMDPTH call
C-                                          and modified call to CELXYZ when
C-                                          recalculating X and Y.
C-   Updated   5-AUG-1995   Meenakshi Narain   Call CEMDPTH_LAYER
C-                                          to fill LAYER_HOT array
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
      REAL WT,WEIGHT_CUT,ENERGY,R
      REAL    W0_ECEM_PHI(6),W0_ECEM_ETA(6),W0_CCEM_PHI(3,5),
     &        W0_CCEM_ETA(3,5),W0(3)
      REAL    ECEM_OFFSET_THETA, ECEM_OFFSET_LN_E0
      INTEGER INDEX1,INDEX2,IETA_3
      REAL    XX,YY,ZZ,EM3WTOT
      REAL     CENRAD, DELRAD, CENZED, DELZED, TILT
      REAL   R_CCEM,Z_ECEM
      REAL    TAN_TH, DEL_TANTH
      INTEGER ARGSOK
      INTEGER I,NCH,POINTER,IOK,IER
      INTEGER  ETAI,PHII,ILYR
      INTEGER PACKED_WORD,LCASH,RECORD_TYPE
      LOGICAL PHI_WRAP,MCDATA
      LOGICAL FIRST/.TRUE./
      DATA W0_ECEM_PHI/ 5.3,4.9,4.5,4.3,4.9,4.5/
      DATA W0_ECEM_ETA/ 4.0,3.6,3.4,3.2,3.2,3.2/
      DATA W0_CCEM_PHI/ 4.2,4.2,4.4,4.3,4.4,4.0,4.5,4.6,4.0,
     &                  4.7,4.6,5.0,4.7,4.6,5.2/
      DATA W0_CCEM_ETA/ 4.0,3.2,3.0,4.3,3.4,2.8,4.5,3.4,2.8,
     &                  4.7,3.4,2.6,4.7,3.4,2.4/
      DATA ECEM_OFFSET_THETA/ 4.28E-4 /
      DATA ECEM_OFFSET_LN_E0/ -11.064 /
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
          CALL EZGET_rarr('W0_ECEM_PHI',W0_ECEM_PHI,IER)
          CALL EZGET_rarr('W0_ECEM_ETA',W0_ECEM_ETA,IER)
          CALL EZGET_rarr('W0_CCEM_PHI',W0_CCEM_PHI,IER)
          CALL EZGET_rarr('W0_CCEM_ETA',W0_CCEM_ETA,IER)
          CALL EZGET('ECEM_OFFSET_THETA',ECEM_OFFSET_THETA,IER)
          CALL EZGET('ECEM_OFFSET_LN_E0',ECEM_OFFSET_LN_E0,IER)
        ELSE
          CALL ERRMSG(' NO_CAPHEL_RCP','CM3POS',
     &        ' USING DEFAULT PARAMETERS FOR WEIGHTS','W')
        ENDIF
        CALL EZRSET
      ENDIF

      W0(1)=WEIGHT_CUT
C
c  Use ETOT of shower and etamax in layer 3 to determine weight
c  parameter in log position finding
C
      CALL CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
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
        PHI_WRAP=(IPHI_HOT(3).GE.54.OR.IPHI_HOT(3).LE.10)
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
            CALL ETOETA(E,PHI_CELL,THETA_CELL,ETA_CELL)
            IF(PHI_WRAP.AND.PHI_CELL.LE.TWOPI*10/64)
     &        PHI_CELL=TWOPI+PHI_CELL
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

  999 RETURN
      END
