      SUBROUTINE CHMANL_NEW(ECLUS,NEWECLUS,ABORT_PROC,LEVEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyzes EM calorimeter clusters (CACL)
C-                         using the H matrix method.
C-
C-   Inputs  : ECLUS = ENERGY OF CLUSTER
C-             CACL and CACH banks
C-   Outputs : NEWECLUS = New energy of cluster
C-             corrected for crack etc. This should be passed to
C-             CAFIX package
C-             ABORT_PROC = .TRUE. WILL not do rest of CAPHEL
C-             LEVEL = 0 IF HMATRIX ANALYSIS OVERRIDDEN
C-   Controls: CAPHEL_RCP
C-
C-   Called by: CAPHEL
C-
C-   Updated  17-MAR-1992   Rajendran Raja  with new HMATRIX package and
C-                                          no interpolation scheme
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
C
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:CHMATR_NEW.INC'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:CIMPACT.INC'
      INCLUDE 'D0$INC:CTRAK.INC'
C
      REAL ECLUS,NEWECLUS
      INTEGER LEVEL
C
      INTEGER HMATRIX_DUMP_EVENTS
      INTEGER IER
C
      LOGICAL OK,HMATRIX_EVENT,ABORT_PROC
C
      INTEGER ISIGN
      INTEGER IOFF,IOFF2,I
C
      INTEGER NEIGEN_MAX
      PARAMETER( NEIGEN_MAX = 37 )
      REAL    EIGEN_VALUE_MAX_MAP(NEIGEN_MAX)
C
      INTEGER NRAP
      LOGICAL DO_HMATRIX_ANAL, DO_NTUPLE
C
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C
      IF(FIRST)THEN                     ! LOCAL INIT
        FIRST = .FALSE.
C
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET('ZETA_CUT',ZETA_CUT,IER)
        CALL EZGET('EIGEN_VALUE_MAX_MAP',EIGEN_VALUE_MAX_MAP,IER)
        CALL EZGET('MAXIMUM_VISIBLE_ET',ET_MAX,IER)
        CALL EZGET('HMATRIX_DUMP_EVENTS',HMATRIX_DUMP_EVENTS,IER)
        CALL EZGET('ADD_CRYOSTAT_ENERGY',ADD_CRYO,IER)
        CALL EZGET('ADD_CRACK_ENERGY',ADD_CRACK,IER)
        CALL EZ_GET_CHARS('RAPIDITY_MAP',NRAP,RAPIDITY_MAP,IER)
        CALL EZRSET
        CALL EZPICK('CAPHEL_RCP')
        CALL EZGET('DO_HMATRIX_ANALYSIS',DO_HMATRIX_ANAL,IER)
        CALL EZGET('DO_HMATRIX_NTUPLE',DO_NTUPLE,IER)
        CALL EZRSET
      ENDIF
C
C ****  FOLLOWING ROUTINE FILLS QUAN VECTOR DEPENDING ON H MATRIX TYPE
C ****  READ IN FROM HMATRIX_RCP
C
      CALL C_SETUP_ETAC_PHIC              ! setup etac and phic of cluster
C
      IF ( .NOT.ACCUMULATE ) THEN
        CALL HMATRIX_SET(RAPIDITY_MAP(IABS(ETAC)),IER)
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('CAPHEL','CHMANL_NEW',
     &        ' Error selecting H matrix for rapidity ','W')
        ENDIF
      ENDIF
C
C
C ****  NEED TO ACCESS HMATRIX_RZ_RCP AFTER SETTING THE NEW MATRIX
C
      IF(ACCUMULATE)THEN
        CALL EZPICK('HMATRIX_RCP')
      ELSE
        CALL EZPICK('HMATRIX_RZ_RCP')
      ENDIF
C
      CALL EZGET('USE_DEAD_ENERGY',USE_DEAD_ENERGY,IER)
      CALL EZGET('USE_POSITION_INFO',USE_POSITION_INFO,IER)
      CALL EZGET('USE_ENERGY_RATIO',USE_ENERGY_RATIO,IER)
      CALL EZGET('Z_VERTEX_SPREAD',Z_VERTEX_SPREAD,IER)
C
      CALL EZRSET
C
      NEWECLUS = ECLUS
      IOFF = 2
C
C ****    assume that if dead energy used then they are the
C ****    first 2 entries in the invisibles list
C
      IF (VIS_DIM.EQ.72) IOFF = 9 * IOFF
      IOFF2 = IOFF/2
      CALL CHMATRIX_FILL_QUAN(EVIS,EDEAD,ENER_CRACK,ENER_CRYO,IER)
C
      IF ( IER.EQ.0 ) THEN
        CALL CHMATRIX_DUMP(HMATRIX_DUMP_EVENTS) !DUMP STUFF
C
        CALL EZPICK('HMATRIX_RCP')
C
C ****  SETTING IT SO THAT HMATRIX_DIAG_TRAN PICKS IT UP.
C
        CALL EZSET('EIGEN_VALUE_MAX',
     &    EIGEN_VALUE_MAX_MAP(IABS(ETAC)),IER)
C
        CALL EZRSET
C
        OK = HMATRIX_EVENT()  !QUAN BANK ALREADY FILLED
C
        ABORT_PROC = .FALSE.
C
        IF ( ACCUMULATE ) THEN
          ABORT_PROC = .TRUE.    !DO NOT DO REST OF CAPHEL
          LEVEL = 0
          IF ( DO_HMATRIX_ANAL ) THEN
            CALL CAL_HMATRIX_ANAL(DO_NTUPLE)
          ENDIF
        ELSE
C
C ****  STORE CHSQF, PROBF AWAY
C
          LEVEL = 1
          CHSQF = C(LHMTR+6)  !Truncated chisquared
          PROBF  = C(LHMTR+8)
C
C ****  comment out for recov12.11 and above
C
c          IF ( CHSQF.GT.ZETA_CUT(IABS(ETAC))) THEN
c            ABORT_PROC = .TRUE.  !REJECT FURTHER PROCESSING
c          ENDIF
c          IF ( Q(LCACL+8).GT.ET_MAX ) THEN
c            ABORT_PROC = .FALSE.  !Accept event any way
c            LEVEL = 0
c          ENDIF
C
C ****  TRANSFORM PREDICTED QUANTITIES TO PROPER UNITS.
C
          CALL UZERO(PRED_CENTER,1,3)
          IF ( USE_POSITION_INFO ) THEN
            EM3AV(1) = Q(LCACL+14)
            EM3AV(2) = Q(LCACL+15)
            EM3AV(3) = Q(LCACL+16)            ! CENTER OF SHOWER.
C
            PHI_CENTROID = ATAN2(EM3AV(2),EM3AV(1))
            IF(PHI_CENTROID.LT.0.0)PHI_CENTROID = PHI_CENTROID + TWOPI
C
            Z_CENTROID = EM3AV(3)
C
            R_CENTROID = SQRT(EM3AV(1)**2 + EM3AV(2)**2)
C
            DELTA_PHI = C(LQUAN+TOT_DIM-2)
            DELTA_Z = C(LQUAN+TOT_DIM-1)
            DELTA_R = C(LQUAN+TOT_DIM)
C
            PHI_CENTROID = PHI_CENTROID + DELTA_PHI
            R_CENTROID = R_CENTROID + DELTA_R
            Z_CENTROID = Z_CENTROID + SIGN_ETA*DELTA_Z
C
            PRED_CENTER(1) = R_CENTROID*COS(PHI_CENTROID)
            PRED_CENTER(2) = R_CENTROID*SIN(PHI_CENTROID)
            PRED_CENTER(3) = Z_CENTROID
C
            ZVPRED  = 0.0   !Not predicting this anymore<
          ENDIF
C
          EDEAD_PRED = 0.0
          ENER_CRYO_PRED = 0.0
          ENER_CRACK_PRED = 0.0
          IF(USE_DEAD_ENERGY)THEN
            DO I = VIS_DIM+1, VIS_DIM+IOFF
              IF(I.LE.VIS_DIM+IOFF2)THEN
                ENER_CRACK_PRED = ENER_CRACK_PRED + C(LQUAN+I)
              ELSE
                ENER_CRYO_PRED = ENER_CRYO_PRED + C(LQUAN+I)
              ENDIF
C
              EDEAD_PRED = EDEAD_PRED + C(LQUAN+I)
C
            ENDDO
            IF ( USE_ENERGY_RATIO ) THEN
              EDEAD_PRED = EDEAD_PRED*EVIS    !PREDICTED DEAD ENERGY
              ENER_CRACK_PRED = ENER_CRACK_PRED*EVIS  !PREDICTED CRACK ENERGY
              ENER_CRYO_PRED = ENER_CRYO_PRED*EVIS    !PREDICTED CRYO ENERGY
            ENDIF
            IF ( ADD_CRYO ) THEN
              NEWECLUS = NEWECLUS + ENER_CRYO_PRED
            ENDIF
            IF ( ADD_CRACK ) THEN
              NEWECLUS = NEWECLUS + ENER_CRACK_PRED
            ENDIF
          ENDIF
        ENDIF
C
      ENDIF
C
  999 RETURN
      ENTRY CHM_SET_ABORT
C
C ****  TO BE CALLED IF ZTRAKS PROCESSING NOT DESIRED E.G. WHEN DOING TEST BEAM
C       STUFF
C
      ABORT_PROC = .TRUE.
      RETURN
      END
