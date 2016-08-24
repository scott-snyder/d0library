      SUBROUTINE ECEMCR(LPTR_IN,ZV1,DZ1,DE,ERR_DE,CORR_APPLIED,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Control routine for ecem corrections
C-                         See ECEMCR.DOC for details.
C-
C-   Inputs  : LPTR_IN      Pointer to input bank. Can be LCACL,LPELC, or LPPHO
C-             ZV1          Z vertex of event
C-             DZ1          Error in ZV1
C-   Outputs : DE           Net Energy correction to be ADDED to cluster energy
C-             ERR_DE       Estimate of error in DE
C-             CORR_APPLIED Flag indicating which corrections contribute to DE
C-                          Bit 1 : DELTA
C-                          Bit 2 : OUTER RADIUS
C-                          Bit 3 : TOWER 14 (outer radius) No correction
C-                          Bit 4 : CRACK
C-                          Bit 5 : BOLT
C-             IER          0  OK
C-                          -1 Called with wrong type of Bank
C-                          -2 Cluster is not in ECEM
C-                          -3 Inconsistency in RCP
C-
C-   Created   6-NOV-1992   Anthony L. Spadafora
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      LOGICAL CACL,PELC,PPHO
      LOGICAL ECEM_DO_DELTA,ECEM_DO_CRACK,ECEM_DO_BOLT,
     &  ECEM_DO_OUT_RAD,ECEM_DO_NEW_CELL_E,ECEM_MIN_CORR
C
      INTEGER LPTR_IN,LCACL_IN,IER,IER2
      INTEGER CORR_APPLIED
      INTEGER FLAG_BIT_DELTA /1/
      INTEGER FLAG_BIT_OUTER_RADIUS /2/
      INTEGER FLAG_BIT_TWR14 /4/
      INTEGER FLAG_BIT_CRACK /8/
      INTEGER FLAG_BIT_BOLT /16/
      INTEGER ARGSOK
      INTEGER IETA_MIN
      INTEGER LCASH_PTR,IETA_HOT(5),IPHI_HOT(5)
C
      REAL    E,ZV1,DZ1,DE,ERR_DE
      REAL    DE_DELTA,ERR_DE_DELTA,
     &  DE_OUTER_RADIUS, ERR_DE_OUTER_RADIUS,
     &  DE_CRACK, ERR_DE_CRACK,
     &  DE_BOLT, ERR_DE_BOLT,
     &  THRESH_DE
      REAL    X_EM3,Y_EM3,THETA,PHI
      REAL    Z_R
      REAL    DZED,CENRAD,DRAD,TILT
      REAL    Z_EM3_MOD_N,Z_EM3_MOD_S,Z_EM3_MOD
      REAL    EDPTH(5),PDPTH(5),ENERGY_HOT(5)

C..   for TESTING
      LOGICAL ECEMCR_DEBUG
      LOGICAL ECEM_DO_POS_CORR
      LOGICAL USE_ISA_TRK
      LOGICAL USE_PWC_TRK

      INTEGER ICLUS
      INTEGER OUTUNIT/11/
      integer ihcacl/4hCACL/
      integer ihpelc/4hPELC/
      integer ihppho/4hPPHO/
C
C----------------------------------------------------------------------
C
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
        CALL EZPICK('ECEMCR_RCP')
        CALL EZGET('ECEM_DO_DELTA',ECEM_DO_DELTA,IER)
        CALL EZGET('ECEM_DO_CRACK',ECEM_DO_CRACK,IER)
        CALL EZGET('ECEM_DO_BOLT',ECEM_DO_BOLT,IER)
        CALL EZGET('ECEM_DO_OUT_RAD',ECEM_DO_OUT_RAD,IER)
        CALL EZGET('ECEM_DO_NEW_CELL_E',ECEM_DO_NEW_CELL_E,IER)
        CALL EZGET('ECEM_MIN_CORR',ECEM_MIN_CORR,IER)
        CALL EZGET('IETA_MIN',IETA_MIN, IER)
        CALL EZGET('THRESH_DE',THRESH_DE, IER)
        CALL EZGET('ECEMCR_DEBUG',ECEMCR_DEBUG,IER)
        CALL EZGET('USE_ISA_TRK',USE_ISA_TRK,IER)
        CALL EZGET('USE_PWC_TRK',USE_PWC_TRK,IER)
        CALL EZRSET
C
C...    Get Z of EM3 layer
        CALL CALZED( 16,3,Z_EM3_MOD_S,DZED,CENRAD,DRAD,TILT,ARGSOK)
        CALL CALZED(-16,3,Z_EM3_MOD_N,DZED,CENRAD,DRAD,TILT,ARGSOK)
      ENDIF
C
      IER = -1
      DE = 0.
      ERR_DE = 0.
      CORR_APPLIED = 0
      CACL = .FALSE.
      PELC = .FALSE.
      PPHO = .FALSE.
C
      IF(IQ(LPTR_IN-4) .EQ. ihCACL) THEN
        CACL = .TRUE.
        LCACL_IN = LPTR_IN
      ELSEIF(IQ(LPTR_IN-4) .EQ. iHPELC) THEN
        PELC = .TRUE.
        LCACL_IN = LQ(LPTR_IN-2)
      ELSEIF(IQ(LPTR_IN-4) .EQ. iHPPHO) THEN
        PPHO = .TRUE.
        LCACL_IN = LQ(LPTR_IN-2)
      ELSE
        CALL ERRMSG('CALORIMETER','ECEMCR',
     &      'Called with bank not a CACL, PELC, PPHO ','W')
        RETURN
      ENDIF
C
C..   Test if cluster is in EC by checking IETA(3)
C..
      LCASH_PTR = LQ(LCACL_IN-2)
      CALL CEMDPTH(LCASH_PTR,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
      IF(IABS(IETA_HOT(3)).LT.IETA_MIN) THEN
        IER = -2
        RETURN ! not in ECEM
      ENDIF
C
      Z_EM3_MOD = Z_EM3_MOD_S
      IF(IETA_HOT(3).LT.0) Z_EM3_MOD = Z_EM3_MOD_N
C
C...  Get position of cluster or track in midplane of EM3
      E = Q(LCACL_IN+7)         !Use CACL E so can rerun on DST
      IF(CACL) THEN
        THETA = Q(LPTR_IN+11)
        PHI   = Q(LPTR_IN+12)
      ELSEIF(PELC) THEN
        THETA = Q(LPTR_IN+8)
        PHI   = Q(LPTR_IN+10)
      ELSEIF(PPHO) THEN
        THETA = Q(LPTR_IN+8)
        PHI   = Q(LPTR_IN+10)
      ENDIF

C...  For testing, overwrite angles with isajet or PWC coordinates
ccc      IF(USE_ISA_TRK)  CALL GET_ISA_ANGLES(THETA,PHI)
ccc      IF(USE_PWC_TRK)  CALL GET_PWC_ANGLES(THETA,PHI)

      X_EM3 = TAN(THETA)*COS(PHI)*(Z_EM3_MOD - ZV1)
      Y_EM3 = TAN(THETA)*SIN(PHI)*(Z_EM3_MOD - ZV1)
C
C..  For Debugging
      IF(ECEMCR_DEBUG) THEN
        ICLUS = IQ(LCACL_IN+2)
        WRITE(OUTUNIT,10) ICLUS,X_EM3,Y_EM3,Z_EM3_MOD,ZV1
   10   FORMAT(' ECEMCR: iclus ',I6,' x/y/z/ZV1 ',4F10.3)
      ENDIF
C
      IF(ECEM_DO_DELTA) THEN
        CALL ECEMCR_DELTA(IABS(IETA_HOT(3)), DE_DELTA, ERR_DE_DELTA,
     &    IER2)
        IF(IER2.NE.0) THEN
          IER = -3
          DE = 0.
          ERR_DE = 0.
          CORR_APPLIED = -1
          RETURN
        ENDIF
        ERR_DE = SQRT(ERR_DE**2 + ERR_DE_DELTA**2)
        IF ( ABS(DE_DELTA).GE.THRESH_DE ) THEN
          CORR_APPLIED = IOR(CORR_APPLIED,FLAG_BIT_DELTA)
          DE = DE + DE_DELTA
        ENDIF
      ENDIF
C
      IF(ECEM_DO_OUT_RAD) THEN
        CALL ECEMCR_OUTER_RADIUS(E,ZV1,IABS(IETA_HOT(3)), X_EM3, Y_EM3,
     &    Z_EM3_MOD, DE_OUTER_RADIUS, ERR_DE_OUTER_RADIUS,IER2)
        IF(IER2.LT.0) THEN
          IER = -3
          DE = 0.
          ERR_DE = 0.
          CORR_APPLIED = -1
          RETURN
        ELSE 
          ERR_DE = SQRT(ERR_DE**2 + ERR_DE_OUTER_RADIUS**2)
          IF(IER2.EQ.1) THEN    !TOWER 14
            CORR_APPLIED = IOR(CORR_APPLIED,FLAG_BIT_TWR14)
          ELSE                  !Normal correction    
              IF ( ABS(DE_OUTER_RADIUS).GE.THRESH_DE ) THEN
                CORR_APPLIED = IOR(CORR_APPLIED,FLAG_BIT_OUTER_RADIUS)
                DE = DE + DE_OUTER_RADIUS
              ENDIF
          ENDIF
        ENDIF
      ENDIF   !ECEM_DO_OUT_RAD
C
C
      DE_BOLT = 0.
      IF(ECEM_DO_BOLT) THEN
        CALL ECEMCR_BOLT(E,ZV1, X_EM3, Y_EM3, Z_EM3_MOD,
     &    DE_BOLT, ERR_DE_BOLT,IER2)
        IF(IER2.NE.0) THEN
          IER = -3
          DE = 0.
          ERR_DE = 0.
          CORR_APPLIED = -1
          RETURN
        ENDIF
        ERR_DE = SQRT(ERR_DE**2 + ERR_DE_BOLT**2)
        IF ( ABS(DE_BOLT).GE.THRESH_DE ) THEN
          CORR_APPLIED = IOR(CORR_APPLIED,FLAG_BIT_BOLT)
          DE = DE + DE_BOLT
        ENDIF
      ENDIF
C
C...  Do crack correction only if no bolt correction

      IF(ECEM_DO_CRACK .AND.
     &  (IAND(CORR_APPLIED,FLAG_BIT_BOLT).EQ.0) ) THEN
        CALL ECEMCR_CRACK(E,ZV1, X_EM3, Y_EM3, Z_EM3_MOD,
     &      DE_CRACK, ERR_DE_CRACK,IER2)
        IF(IER2.NE.0) THEN
          IER = -3
          DE = 0.
          ERR_DE = 0.
          CORR_APPLIED = -1
          RETURN
        ENDIF
        ERR_DE = SQRT(ERR_DE**2 + ERR_DE_CRACK**2)
        IF ( ABS(DE_CRACK).GE.THRESH_DE ) THEN
          CORR_APPLIED = IOR(CORR_APPLIED,FLAG_BIT_CRACK)
          DE = DE + DE_CRACK
        ENDIF
      ENDIF
C
C...
      IF(ECEM_DO_NEW_CELL_E) CALL ECEMCR_NEW_CELL_E(LPTR_IN,IER2)
C
      IER = 0
C
  999 RETURN
      END
