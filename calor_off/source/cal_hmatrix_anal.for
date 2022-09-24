      SUBROUTINE CAL_HMATRIX_ANAL(DO_NTUPLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Analyze hmatrix for caphel package
C-
C-   Inputs  :IF DO_NTUPLE, CALLS HMATRIX_MAKE_NTUPLE
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990   Rajendran Raja
C-   Updated  17-MAR-1992   Rajendran Raja
C-   Updated  12-OCT-1992   Meenakshi Narain
C-                          add protection to compute distance of closest
C-                          approach from tracks only for electrons
C-   Updated  12-JAN-1994   Alexander Peryshkin swich to CASH bank for DST
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CHMATR_NEW.INC'
      INCLUDE 'D0$INC:CIMPACT.INC'
      INCLUDE 'D0$INC:CTRAK.INC'
      INCLUDE 'D0$INC:CEMPRF.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZCACH.LINK'
      INCLUDE 'D0$INC:MORE_NTUPLE.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER IER, I, J, MATRIX_TYPE
      REAL ECLUSTER
      INTEGER MAX_SCATTER_DIAG,ITWO
      REAL    DCL_HMATRIX,DCL_CENTROID,DCL_LOG
      REAL    DCLOSE_HMATRIX(3),DCLOSE_CENTROID(3),DCLOSE_LOG(3)
      REAL    PHI_HMATRIX,PHI_LOG
      REAL    PHI_ISAJET
      REAL    R_HMATRIX,R_LOG
      REAL    R_ISAJET
      REAL    R_CENTROID_PHI,R_HMATRIX_PHI,R_LOG_PHI
      REAL    R_ISAJET_PHI
      REAL    Z_HMATRIX,Z_LOG,Z_ISAJET
      REAL    AVERAGE_PHI,AVERAGE_RAD
      REAL    DELTA_OFF
      LOGICAL DIAGONALIZE
      LOGICAL DO_NTUPLE
C
      INTEGER CELLS
      INTEGER TRULEN
      CHARACTER*8 CHR
      INTEGER ELECTRON
C----------------------------------------------------------------------
C
      CALL DHDIR('HMATRIX_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','CAL_HMATRIX_ANAL',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF(FIRST)THEN
        FIRST = .FALSE.
C
C ****  BOOK HISTOGRAMS HERE
C
        CALL EZPICK('HMATRIX_RCP')
C
C ****  BOOK HISTOGRAMS HERE
C
        CALL EZGET_l('DIAGONALIZE',DIAGONALIZE,IER)
        IF(.NOT.ACCUMULATE)THEN
          CALL DO_HBOOK('USAGE_PHASE_HISTOGRAMS')     ! BOOK THEM
        ENDIF
C
        CALL EZGET_i('MAX_SCATTER_DIAG',MAX_SCATTER_DIAG,IER)
        CALL EZGET('AVERAGE_RAD',AVERAGE_RAD,IER)
        CALL EZGET('AVERAGE_PHI',AVERAGE_PHI,IER)
        AVERAGE_PHI = AVERAGE_PHI*RADIAN  !AVERAGE PHI OF BEAM IN RADIANS
        DELTA_OFF = AVERAGE_RAD*AVERAGE_PHI
C
        CALL EZRSET
C
      ENDIF
C
      LCACH = LQ(LCACL-IZCACH)
C if no CACH bank to look at CASH but used LCACH name for bank pointer
      IF ( LCACH.EQ.0 ) LCACH = LQ(LCACL-2)
      IF ( LCACH.GT.0 ) THEN
        CELLS = IQ(LCACH+2)   !NUMBER OF CELLS IN CLUSTER
      ELSE
        CELLS = 0
      ENDIF
C
C ****  USAGE HISTOGRAMS NEXT. USER WILL NEED TO CHANGE CODE HERE.
C
      IF ( .NOT.ACCUMULATE ) THEN
        CALL HFILL(301,C(LHMTR+3),0.0,1.0)    ! Chisquared
        CALL HFILL(302,C(LHMTR+5),0.0,1.0)    ! Probability
        CALL HFILL(303,C(LHMTR+6),0.0,1.0)    ! Chisquared TRUNCATED
        CALL HFILL(304,C(LHMTR+8),0.0,1.0)    ! Probability
C
C ****  now to histogram diagonalized quantities
C
        IF ( DIAGONALIZE ) THEN
          DO I = 1 , VIS_DIM
            CALL HFILL(350+I,C(LDIAG+I),0.,1.0)
          ENDDO
C
C ****  now to scattergram diaganalized normalised quantities.
C
          IF ( VIS_DIM.GT.1 ) THEN
            ITWO = 0
            DO I = 1 , VIS_DIM-1
              DO J = I+1 , VIS_DIM
                ITWO = ITWO + 1
                IF ( ITWO.LE.MAX_SCATTER_DIAG ) THEN
                  CALL HFILL(400+ITWO,C(LDIAG+J),C(LDIAG+I),1.0)
                ELSE
                  GO TO 950               ! NO MORE
                ENDIF
              ENDDO
            ENDDO
          ENDIF
        ENDIF
C
  950   CONTINUE
C
C ****  add here electron specific histograms
C
        IF ( USE_DEAD_ENERGY ) THEN
          ACCUMULATE = .TRUE.
          CALL CHMATRIX_FILL_QUAN(EVIS,EDEAD,ENER_CRACK,ENER_CRYO,IER)
          ACCUMULATE = .FALSE.
          CALL HFILL(501,ENER_CRYO_PRED,0.0,1.0)
          CALL HFILL(502,ENER_CRACK_PRED,0.0,1.0)
          CALL HFILL(503,ENER_CRYO,0.0,1.0)
          CALL HFILL(504,ENER_CRACK,0.0,1.0)
          CALL HFILL(510,ENER_CRYO_PRED,ENER_CRYO,1.0)
          CALL HFILL(511,ENER_CRACK_PRED,ENER_CRACK,1.0)
          CALL HFILL(513,ENER_CRYO-ENER_CRYO_PRED,0.0,1.0)
          CALL HFILL(514,ENER_CRACK-ENER_CRACK_PRED,0.0,1.0)
        ENDIF
C
        CALL HFILL(506,ZVPRED,0.0,1.0)
        CALL HFILL(507,VERT(3),0.0,1.0)
        CALL HFILL(512,ZVPRED,VERT(3),1.0)
C
        CALL HFILL(515,VERT(3)-ZVPRED,0.0,1.0)
C
C ****  SET UP CHOSEN TRACK
C
        CALL C_SETUP_TRACK(IER)
        IF ( IER.EQ.0 ) THEN
          ELECTRON = 1
        ELSE
          ELECTRON = 0
        ENDIF
C
C ****  WORK OUT DISTANCE OF CLOSEST APPROACH
C
        DCL_HMATRIX = 0
        DCL_LOG = 0
        DCL_CENTROID = 0
C
        IF ( USE_POSITION_INFO ) THEN
C
          IF (ELECTRON.EQ.1) THEN
            CALL CAL_EM_IMPACT
            CALL CLOSE_DIST1(PRED_CENTER,VERT,UVEC,DCL_HMATRIX,
     &        DCLOSE_HMATRIX)
            CALL CLOSE_DIST1(XBAR3,VERT,UVEC,DCL_LOG,
     &        DCLOSE_LOG)
            CALL CLOSE_DIST1(EM3AV,VERT,UVEC,DCL_CENTROID,
     &        DCLOSE_CENTROID)
C
            CALL HFILL(516,DCL_HMATRIX,0.0,1.0)
            CALL HFILL(517,DCL_LOG,0.0,1.0)
            CALL HFILL(518,DCL_CENTROID,0.0,1.0)
          ELSE
            DO I = 1, 3
              RIMPACT(I) = 0.
              DCLOSE_LOG(I) = 999.
              DCLOSE_HMATRIX(I) = 999.
              DCLOSE_CENTROID(I) = 999.
            END DO
            DCL_LOG = 999.
            DCL_HMATRIX = 999.
            DCL_CENTROID = 999.
          ENDIF
C
          PHI_ISAJET = PHI(1)*RADIAN
          PHI_CENTROID = ATAN2(EM3AV(2)-VERT(2),
     &        EM3AV(1)-VERT(1))
C
          PHI_LOG = ATAN2(XBAR3(2)-VERT(2),
     &        XBAR3(1)-VERT(1))
C
          PHI_HMATRIX = ATAN2(PRED_CENTER(2)-VERT(2),
     &        PRED_CENTER(1)-VERT(1))
C
          Z_LOG = XBAR3(3)
          Z_CENTROID = EM3AV(3)
          Z_HMATRIX = PRED_CENTER(3)
          Z_ISAJET = RIMPACT(3)
C
          CALL HFILL(519,Z_HMATRIX,Z_ISAJET,1.0)
          CALL HFILL(520,Z_CENTROID,Z_ISAJET,1.0)
          CALL HFILL(521,Z_LOG,Z_ISAJET,1.0)
C
          R_CENTROID = SQRT(EM3AV(2)**2+EM3AV(1)**2)
          R_HMATRIX = SQRT(PRED_CENTER(2)**2+PRED_CENTER(1)**2)
          R_LOG = SQRT(XBAR3(1)**2+XBAR3(2)**2)
          R_ISAJET = SQRT(RIMPACT(1)**2+RIMPACT(2)**2)
C
          R_CENTROID_PHI = R_CENTROID*PHI_CENTROID
          R_LOG_PHI = R_LOG*PHI_LOG
          R_HMATRIX_PHI = R_HMATRIX*PHI_HMATRIX
          R_ISAJET_PHI = R_ISAJET*PHI_ISAJET
C
          CALL HFILL(522,R_HMATRIX_PHI-DELTA_OFF,
     &        R_ISAJET_PHI-DELTA_OFF
     &        ,1.0)
          CALL HFILL(523,R_CENTROID_PHI-DELTA_OFF,
     &        R_ISAJET_PHI-DELTA_OFF
     &        ,1.0)
          CALL HFILL(524,R_LOG_PHI-DELTA_OFF,
     &        R_ISAJET_PHI-DELTA_OFF
     &        ,1.0)
C
          CALL HFILL(525,R_ISAJET_PHI-DELTA_OFF,EDEAD_PRED,1.0)
C
          CALL HFILL(526,R_ISAJET_PHI-DELTA_OFF,DCL_HMATRIX,1.0)
          CALL HFILL(527,R_ISAJET_PHI-DELTA_OFF,DCL_CENTROID,1.0)
          CALL HFILL(528,R_ISAJET_PHI-DELTA_OFF,DCL_LOG,1.0)
C
          CALL HFILL(529,DCLOSE_HMATRIX(3),0.0,1.0)
          CALL HFILL(530,DCLOSE_CENTROID(3),0.0,1.0)
          CALL HFILL(531,DCLOSE_LOG(3),0.0,1.0)
C
          CALL HFILL(532,DCLOSE_HMATRIX(2),0.0,1.0)
          CALL HFILL(533,DCLOSE_CENTROID(2),0.0,1.0)
          CALL HFILL(534,DCLOSE_LOG(2),0.0,1.0)
C
          CALL HFILL(535,DCLOSE_HMATRIX(1),0.0,1.0)
          CALL HFILL(536,DCLOSE_CENTROID(1),0.0,1.0)
          CALL HFILL(537,DCLOSE_LOG(1),0.0,1.0)
C
C ****  ADD QUANTITES TO NTUPLE
C
          MORE_NAMES(1) = 'CHISQ'
          MORE_NAMES(2) = 'PROB'
          MORE_NAMES(3) = 'CHISQ_TR'
          MORE_NAMES(4) = 'PROB_TR'
          MORE_NAMES(5) = 'DCL_HMTR'
          MORE_NAMES(6) = 'DCL_CENTR'
          MORE_NAMES(7) = 'DCL_LOG'
          MORE_NAMES(8) = 'VERTZ'
          MORE_NAMES(9) = 'ZVPRED'
          MORE_NAMES(10) = 'CELLS'
          NUM_MORE = 10
          MORE_QUANS(1) = C(LHMTR+3)
          MORE_QUANS(2) = C(LHMTR+5)
          MORE_QUANS(3) = C(LHMTR+6)
          MORE_QUANS(4) = C(LHMTR+8)
          MORE_QUANS(5) = DCL_HMATRIX
          MORE_QUANS(6) = DCL_CENTROID
          MORE_QUANS(7) = DCL_LOG
          MORE_QUANS(8) = VERT(3)
          MORE_QUANS(9) = ZVPRED
          MORE_QUANS(10) = CELLS
        ELSE   !NO POSITION INFO
          MORE_NAMES(1) = 'CHISQ'
          MORE_NAMES(2) = 'PROB'
          MORE_NAMES(3) = 'CHISQ_TR'
          MORE_NAMES(4) = 'PROB_TR'
          MORE_NAMES(5) = 'CELLS'
          NUM_MORE = 5
          MORE_QUANS(1) = C(LHMTR+3)
          MORE_QUANS(2) = C(LHMTR+5)
          MORE_QUANS(3) = C(LHMTR+6)
          MORE_QUANS(4) = C(LHMTR+8)
          MORE_QUANS(5) = CELLS
        ENDIF
      ELSE        !NOW IN ACCUMULATE MODE
        MORE_NAMES(1) = 'CELLS'
        NUM_MORE = 1
        MORE_QUANS(1) = CELLS
      ENDIF
      DO I = 1 , NDPTH
        MORE_QUANS(I+NUM_MORE) = ENDPTH(I)
        WRITE(CHR,20)I
   20   FORMAT('DEPTH',I3.3)
        MORE_NAMES(I+NUM_MORE) = CHR
      ENDDO
C
      NUM_MORE = NUM_MORE + NDPTH
      MORE_QUANS(1+NUM_MORE) = ETOT
      MORE_QUANS(2+NUM_MORE) = ET
      MORE_QUANS(3+NUM_MORE) = ETRANS
      MORE_QUANS(4+NUM_MORE) = ETAC
      MORE_QUANS(5+NUM_MORE) = PHIC
C
      MORE_QUANS(6+NUM_MORE) = XBAR3(1)
      MORE_QUANS(7+NUM_MORE) = XBAR3(2)
      MORE_QUANS(8+NUM_MORE) = XBAR3(3)
C
      MORE_QUANS(9+NUM_MORE) =  Q(LCACL+14)
      MORE_QUANS(10+NUM_MORE) = Q(LCACL+15)
      MORE_QUANS(11+NUM_MORE) = Q(LCACL+16)
      MORE_QUANS(12+NUM_MORE) = ELECTRON
C
      MORE_NAMES(1+NUM_MORE) = 'ETOT'
      MORE_NAMES(2+NUM_MORE) = 'ET'
      MORE_NAMES(3+NUM_MORE) = 'ETRANS'
      MORE_NAMES(4+NUM_MORE) = 'ETAC'
      MORE_NAMES(5+NUM_MORE) = 'PHIC'
C
      MORE_NAMES(6+NUM_MORE) = 'XBAR3(1)'
      MORE_NAMES(7+NUM_MORE) = 'XBAR3(2)'
      MORE_NAMES(8+NUM_MORE) = 'XBAR3(3)'
C
      MORE_NAMES(9+NUM_MORE) = 'CENTROID_X'
      MORE_NAMES(10+NUM_MORE) = 'CENTROID_Y'
      MORE_NAMES(11+NUM_MORE) = 'CENTROID_Z'
      MORE_NAMES(12+NUM_MORE) = 'ELECTRON'
      NUM_MORE = NUM_MORE + 12
C
      IF ( DO_NTUPLE ) THEN
        CALL HMATRIX_MAKE_NTUPLE(MORE_NAMES,MORE_QUANS,NUM_MORE)
      ENDIF
C
  999 RETURN
      END
