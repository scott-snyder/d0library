      SUBROUTINE JET_ET_MCCORR(ETA,ETIN,CONE,ETOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Corrects Jet Et as a function of Eta, Et and
C-   cone size.  This correction is VALID FOR THE DOUBLE BLIND EVENTS
C-   ONLY.  The correction function was derived by comparing Jet Pt
C-   to PJet Et=Esin(theta).  Only events with equal numbers of Jets
C-   and PJets were used for the comparison.
C-
C-   Inputs  :  ETA     -   Measured Eta.
C-              ETIN    -   Measured Et.
C-              CONE    -   Cone size used to find the jet.
C-                          (Pjet cone EQUALS Jet cone.)
C-   Outputs :  ETOUT   -   Corrected Et. Zero if no correction made.
C-
C-   Created  7-DEC-1990   Gerald C. Blazey    V1.0
C-   Updated  3-JAN-1991   W.G.D.Dharmaratna   V1.2 Added correction
C-                         factors for cone=0.3 & cone=0.5, tested for
C-                         20 < Et <200.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ICONE,IETA_L,IETA_H
      LOGICAL FIRST
      REAL ETA,ETIN,CONE,ETOUT,AETA,ETIN_ET
      REAL CORR_ETA,CORR_ET,SLOPE
      REAL CETA(8,3),FPARAM_R1,FPARAM_R2
      REAL CET(5,3)
C
      DATA FIRST /.TRUE./
      DATA CETA/
     &      1.479,1.472,1.414,1.408,1.405,1.517,1.584,1.60, ! Cone = .3
     &      1.373,1.355,1.310,1.291,1.228,1.327,1.373,1.398,! Cone = .5
     &      1.32,1.32,1.25,1.23,1.15,1.20,1.22,1.27/        ! Cone = .7
      DATA CET/
     &      1.391,-1.383E-02,1.402E-04,-5.867E-07,8.387E-10,! Cone = .3
     &      1.262,-1.008E-02,1.140E-04,-5.197E-07,7.951E-10,!      = .5
     &      1.012,-.00035,0.0,0.0,0.0/                      !      = .7
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL ERRMSG('Using Jet Et correction V1.2',
     &    'JET_ET_MCCORR',' ','S')
        FIRST=.FALSE.
      END IF
C
      ETOUT=0
C
      ICONE=0                           ! GET CONE INDEX
      IF(CONE .EQ. 0.3) ICONE=1
      IF(CONE .EQ. 0.5) ICONE=2
      IF(CONE .EQ. 0.7) ICONE=3
      IF(ICONE.EQ.   0) THEN
        CALL ERRMSG('No corrections, returns Et=0',
     &    'JET_ET_MCCORR',' ','W')
	GOTO 999
      ENDIF
C
      AETA    = ABS(ETA)                ! ETA CORRECTION
      IETA_L  = AETA / .25 + 1          ! GET ETA INDICES
      IETA_L  = IETA_L / 2
      IETA_H  = IETA_L + 1
      IF( AETA .LT. 0.25) IETA_L=1
      IF( AETA .GT. 3.75) THEN
        IETA_L=8
        IETA_H=8
      END IF
      SLOPE   = (CETA(IETA_H,ICONE)-CETA(IETA_L,ICONE))/.50
      CORR_ETA= CETA(IETA_L,ICONE) + SLOPE*(AETA-(IETA_L*.50-.25))
      ETOUT   = ETIN * CORR_ETA

      ETIN_ET = ETOUT
      IF (ICONE .LE. 2 .AND. ETOUT .GT. 280.) ETOUT = 280.    
        CORR_ET=(CET(1,ICONE)+CET(2,ICONE)*ETOUT+CET(3,ICONE)*ETOUT**2
     &   +CET(4,ICONE)*ETOUT**3+CET(5,ICONE)*ETOUT**4) 
      ETOUT =ETIN_ET*CORR_ET 
C
  999 RETURN
      END
