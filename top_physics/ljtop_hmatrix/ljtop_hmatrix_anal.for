      SUBROUTINE LJTOP_HMATRIX_ANAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-OCT-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$LINKS:IZQUAN.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:CTRAK.INC'
      INCLUDE 'D0$INC:EVENT_QUAN.INC'
      INCLUDE 'D0$INC:MORE_NTUPLE.INC'
      INCLUDE 'D0$INC:FILE_WT.INC'
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER IER
      INTEGER NJETSO,I,IND,STATUS
      CHARACTER*32 USE_HMATRIX(10)
      INTEGER NUSE
      REAL    WT
      INTEGER TRG1,TRG2
      INTEGER LQUAN_SAVE
      EQUIVALENCE (LQUAN_SAVE,HSTLNK(HST_LNKMX))
      INTEGER NDATAQ
      CHARACTER*32 NTUPLE_FILE
      INTEGER NCHR
C
      INTEGER LISV1,LISP1
      EQUIVALENCE (LISV1,CSTLNK(LNKMX-2)),(LISP1,CSTLNK(LNKMX-3))
      INTEGER GZISV1
      REAL    ISOL
C----------------------------------------------------------------------
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL EZPICK('LJTOP_HMATRIX_RCP')
C
C ****  BOOK HISTOGRAMS HERE
C
C        CALL DO_HBOOK('HISTOGRAMS')     ! BOOK THEM
        CALL EZRSET
        CALL EZPICK('HMATRIX_RCP')
        CALL EZ_GET_CHARS('USE_HMATRIX',NUSE,USE_HMATRIX,IER)
        CALL EZ_GET_CHARS('NTUPLE_FILE',NCHR,NTUPLE_FILE,IER)
        CALL EZRSET
C
      ENDIF
      CALL DHDIR_SAVE_FILE            !SAVE PREVIOUS TOPDIR
      CALL DHDIR_DECLARE_FILE(NTUPLE_FILE)
      CALL DHSETDIR('//PAWC',STATUS)
      CALL DHDIR('LJTOP_HMATRIX_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('CALORIMETER','CPHANL',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      MORE_NAMES(1) = 'NVERTEX'
      MORE_NAMES(2) = 'Z_VERT1'
      MORE_NAMES(3) = 'Z_VERT2'
      MORE_NAMES(4) = 'Z_VERT3'
      MORE_NAMES(5) = 'WEIGHT'
C
      MORE_NAMES(6) = 'ELE_HIGH'
      MORE_NAMES(7) = 'CHSQ_EM'  !ELECTRON/PHOTON H MATRIX CHISQUARED
      MORE_NAMES(8) = 'TRD_EM'   !TRD probability for EM
      MORE_NAMES(9) = 'ISOL_EM'  !isolation for EM
C
      MORE_NAMES(10) = 'CHSQ_DTA'   !USING H MATRIX FROM DATA
      MORE_NAMES(11) = 'LIKE_DTA'  !Diagonalized likelihood from data
      MORE_NAMES(12) = 'DTA_001'   !Diagonalized component  from data
      MORE_NAMES(13) = 'DTA_002'   !Diagonalized component  from data
      MORE_NAMES(14) = 'DTA_003'   !Diagonalized component  from data
      MORE_NAMES(15) = 'DTA_004'   !Diagonalized component  from data
      MORE_NAMES(16) = 'DTA_005'   !Diagonalized component  from data
C
      MORE_NAMES(17) = 'CHSQ_VC3'   !USING H MATRIX FROM  VC3
      MORE_NAMES(18) = 'LIKE_VC3'  !Diagonalized likelihood from vc3
      MORE_NAMES(19) = 'VC3_001'   !Diagonalized component  from vc3
      MORE_NAMES(20) = 'VC3_002'   !Diagonalized component  from vc3
      MORE_NAMES(21) = 'VC3_003'   !Diagonalized component  from vc3
      MORE_NAMES(22) = 'VC3_004'   !Diagonalized component  from vc3
      MORE_NAMES(23) = 'VC3_005'   !Diagonalized component  from vc3
C
      MORE_NAMES(24) = 'CHSQ_VC4'   !USING H MATRIX FROM  VC4
      MORE_NAMES(25) = 'LIKE_VC4'  !Diagonalized likelihood from vc4
      MORE_NAMES(26) = 'VC4_001'   !Diagonalized component  from vc4
      MORE_NAMES(27) = 'VC4_002'   !Diagonalized component  from vc4
      MORE_NAMES(28) = 'VC4_003'   !Diagonalized component  from vc4
      MORE_NAMES(29) = 'VC4_004'   !Diagonalized component  from vc4
      MORE_NAMES(30) = 'VC4_005'   !Diagonalized component  from vc4
C
      MORE_NAMES(31) = 'CHSQ_QCD'   !USING H MATRIX FROM  QCD
      MORE_NAMES(32) = 'LIKE_QCD'  !Diagonalized likelihood from qcd
      MORE_NAMES(33) = 'QCD_001'   !Diagonalized component  from qcd
      MORE_NAMES(34) = 'QCD_002'   !Diagonalized component  from qcd
      MORE_NAMES(35) = 'QCD_003'   !Diagonalized component  from qcd
      MORE_NAMES(36) = 'QCD_004'   !Diagonalized component  from qcd
      MORE_NAMES(37) = 'QCD_005'   !Diagonalized component  from qcd
C
      MORE_NAMES(38) = 'CHSQ_140'   !USING H MATRIX FROM  140
      MORE_NAMES(39) = 'LIKE_140'  !Diagonalized likelihood from 140
      MORE_NAMES(40) = 'T140_001'  !Diagonalized component  from 140
      MORE_NAMES(41) = 'T140_002'  !Diagonalized component  from 140
      MORE_NAMES(42) = 'T140_003'  !Diagonalized component  from 140
      MORE_NAMES(43) = 'T140_004'  !Diagonalized component  from 140
      MORE_NAMES(44) = 'T140_005'  !Diagonalized component  from 140
C
      MORE_NAMES(45) = 'CHSQ_160'   !USING H MATRIX FROM  160
      MORE_NAMES(46) = 'LIKE_160'  !Diagonalized likelihood from 160
      MORE_NAMES(47) = 'T160_001'  !Diagonalized component  from 160
      MORE_NAMES(48) = 'T160_002'  !Diagonalized component  from 160
      MORE_NAMES(49) = 'T160_003'  !Diagonalized component  from 160
      MORE_NAMES(50) = 'T160_004'  !Diagonalized component  from 160
      MORE_NAMES(51) = 'T160_005'  !Diagonalized component  from 160
C
      MORE_NAMES(52) = 'CHSQ_180'   !USING H MATRIX FROM  180
      MORE_NAMES(53) = 'LIKE_180'  !Diagonalized likelihood from 180
      MORE_NAMES(54) = 'T180_001'  !Diagonalized component  from 180
      MORE_NAMES(55) = 'T180_002'  !Diagonalized component  from 180
      MORE_NAMES(56) = 'T180_003'  !Diagonalized component  from 180
      MORE_NAMES(57) = 'T180_004'  !Diagonalized component  from 180
      MORE_NAMES(58) = 'T180_005'  !Diagonalized component  from 180
C
      MORE_NAMES(59) = 'CHSQ_200'   !USING H MATRIX FROM  200
      MORE_NAMES(60) = 'LIKE_200'  !Diagonalized likelihood from 200
      MORE_NAMES(61) = 'T200_001'  !Diagonalized component  from 200
      MORE_NAMES(62) = 'T200_002'  !Diagonalized component  from 200
      MORE_NAMES(63) = 'T200_003'  !Diagonalized component  from 200
      MORE_NAMES(64) = 'T200_004'  !Diagonalized component  from 200
      MORE_NAMES(65) = 'T200_005'  !Diagonalized component  from 200
C
C- PX, PY, PZ, E, ET, ETA, PHI, DET_ETA, QUALITY
C- FOR LEPTON
C-
      IND = 65
      MORE_NAMES(IND+1) = 'PX_LP'
      MORE_NAMES(IND+2) = 'PY_LP'
      MORE_NAMES(IND+3) = 'PZ_LP'
      MORE_NAMES(IND+4) = 'EN_LP'
      MORE_NAMES(IND+5) = 'ET_LP'
      MORE_NAMES(IND+6) = 'ETA_LP'
      MORE_NAMES(IND+7) = 'PHI_LP'
      MORE_NAMES(IND+8) = 'DETA_LP'
      MORE_NAMES(IND+9) = 'QUAL_LP'
C
      MORE_NAMES(IND+10) = 'TIGHT_EL'
      MORE_NAMES(IND+11) = 'LOOSE_EL'
      MORE_NAMES(IND+12) = 'TIGHT_PH'
      MORE_NAMES(IND+13) = 'LOOSE_PH'
      MORE_NAMES(IND+14) = 'DEDX'
C
      MORE_NAMES(IND+15) = 'NEUTX'
      MORE_NAMES(IND+16) = 'NEUTY'
      MORE_NAMES(IND+17) = 'MET'
C
      MORE_NAMES(IND+18) = 'TRMASS'
      MORE_NAMES(IND+19) = 'SCAL_ET'
      MORE_NAMES(IND+20) = 'NJETS'
C
      MORE_NAMES(IND+21) = 'JET1_X'
      MORE_NAMES(IND+22) = 'JET1_Y'
      MORE_NAMES(IND+23) = 'JET1_Z'
      MORE_NAMES(IND+24) = 'JET1_E'
      MORE_NAMES(IND+25) = 'JET1_ET'
C
      MORE_NAMES(IND+26) = 'JET2_X'
      MORE_NAMES(IND+27) = 'JET2_Y'
      MORE_NAMES(IND+28) = 'JET2_Z'
      MORE_NAMES(IND+29) = 'JET2_E'
      MORE_NAMES(IND+30) = 'JET2_ET'
C
      MORE_NAMES(IND+31) = 'JET3_X'
      MORE_NAMES(IND+32) = 'JET3_Y'
      MORE_NAMES(IND+33) = 'JET3_Z'
      MORE_NAMES(IND+34) = 'JET3_E'
      MORE_NAMES(IND+35) = 'JET3_ET'
C
      MORE_NAMES(IND+36) = 'JET4_X'
      MORE_NAMES(IND+37) = 'JET4_Y'
      MORE_NAMES(IND+38) = 'JET4_Z'
      MORE_NAMES(IND+39) = 'JET4_E'
      MORE_NAMES(IND+40) = 'JET4_ET'
C
      MORE_NAMES(IND+41) = 'JET5_X'
      MORE_NAMES(IND+42) = 'JET5_Y'
      MORE_NAMES(IND+43) = 'JET5_Z'
      MORE_NAMES(IND+44) = 'JET5_E'
      MORE_NAMES(IND+45) = 'JET5_ET'
C
      MORE_NAMES(IND+46) = 'NTAGS'
      MORE_NAMES(IND+47) = 'TAG1_X'
      MORE_NAMES(IND+48) = 'TAG1_Y'
      MORE_NAMES(IND+49) = 'TAG1_Z'
      MORE_NAMES(IND+50) = 'TAG1_E'
      MORE_NAMES(IND+51) = 'TAG1_ET'
      MORE_NAMES(IND+52) = 'DELTA_R'
      MORE_NAMES(IND+53) = 'PTREL'
C
      MORE_NAMES(IND+54) = 'SPHERIC'
      MORE_NAMES(IND+55) = 'YPLANAR'
      MORE_NAMES(IND+56) = 'APLANAR'
C THIS INCLUDES BEAM ORIENTATION
      MORE_NAMES(IND+57) = 'SPHERI11'
      MORE_NAMES(IND+58) = 'YPLANA11'
      MORE_NAMES(IND+59) = 'APLANA11'
C
      NUM_MORE = IND+59
C
      CALL UZERO(MORE_QUANS,1,NMORE)
      MORE_QUANS(1) = NVER
      MORE_QUANS(2) = P9_VERTEX(3,1)
      MORE_QUANS(3) = P9_VERTEX(3,2)
      MORE_QUANS(4) = P9_VERTEX(3,3)
      MORE_QUANS(5) = WEIGHTS(IFILE)
      MORE_QUANS(6) = ELE_HIGH
      MORE_QUANS(7) = P24_ELECTRON(18,1)  !Truncated Chisquared
      MORE_QUANS(8) = P24_ELECTRON(24,1)  !TRD anode likelihood prob
C
      ISOL = 0.0
      IF ( P24_ELECTRON(15,1).gt.0.0 ) THEN
        ISOL = (P24_ELECTRON(14,1)-P24_ELECTRON(15,1))
     &    /P24_ELECTRON(15,1)
      ENDIF
C
      MORE_QUANS(9) = ISOL
C
      IF ( .NOT.ACCUMULATE ) THEN
C
        LQUAN_SAVE = LQUAN   !SAVE QUAN BANK
        IND = 10
        DO I = 1 , NUSE
          CALL HMATRIX_SET(USE_HMATRIX(I),IER)
          IF ( IER.NE.0 ) THEN
            CALL ERRMSG('LJTOP_HMATRIX','LJTOP_HMATRIX_ANAL',
     &        'ERROR SETTING TO H MATRIX ','W')
          ELSE
            IF ( LQUAN.NE.LQUAN_SAVE ) THEN
C COPY SAVED QUAN BANK TO QUAN
              NDATAQ = IC(LQUAN_SAVE-1)
              CALL UCOPY(C(LQUAN_SAVE+1),C(LQUAN+1),NDATAQ)
            ENDIF
            CALL HMATRIX_CHISQUARED
            CALL HMATRIX_DIAG_TRAN          !OBTAIN DIAGONALIZED VARIABLES
            MORE_QUANS(IND) = C(LHMTR+3)    !Chisq
            MORE_QUANS(IND+1) = 0.0         !LIKELIHOOD TO BE FILLED IN LATER
            CALL UCOPY(C(LDIAG+1),MORE_QUANS(IND+2),VIS_DIM) !DIAG QUANS
            CALL HMATRIX_RESET
          ENDIF
          IND = IND + 7  !Increment pointer anyway
        ENDDO
      ENDIF
C
      IND = 66
      IF ( ELECTRON )THEN
        CALL UCOPY(P24_ELECTRON(1,1),MORE_QUANS(IND),9)
      ELSEIF ( PHOTON ) THEN
        CALL UCOPY(P18_PHOTON(1,1),MORE_QUANS(IND),9)
      ENDIF
      IND = IND + 9
C
      CALL UZERO(MORE_QUANS(IND),1,4)
      IF ( TIGHT_ELECTRON ) THEN
        MORE_QUANS(IND) = 1
      ELSEIF ( LOOSE_ELECTRON ) THEN
        MORE_QUANS(IND+1) = 1
      ELSEIF ( TIGHT_PHOTON ) THEN
        MORE_QUANS(IND+2) = 1
      ELSEIF ( LOOSE_PHOTON ) THEN
        MORE_QUANS(IND+3) = 1
      ENDIF
C
      IND = IND + 4
C
      MORE_QUANS(IND) = RDEDX
      IND = IND + 1
C
      CALL UCOPY(P2_NEUT,MORE_QUANS(IND),2)
      IND = IND + 2
      MORE_QUANS(IND) = MET_D0
      IND = IND+ 1
      MORE_QUANS(IND)=TRMASS
      MORE_QUANS(IND+1)=SCALAR_ET
      IND = IND + 2
      NJETSO = MIN(NJETS,5)
      MORE_QUANS(IND) = NJETSO
      IND = IND + 1
C
      DO I = 1 , 5
C MAX 5 JETS ALLOWED IN NTUPLE
        IF ( I.LE.NJETSO ) THEN
          CALL UCOPY(P25_JETS(1,I),MORE_QUANS(IND),5)
        ENDIF
        IND = IND + 5
      ENDDO
C
      MORE_QUANS(IND) = NTAG
      IND = IND + 1
      CALL UCOPY(P25_MUON_TAGGED(1,1),MORE_QUANS(IND),5)
      IND = IND + 5
      MORE_QUANS(IND) = P25_MUON_TAGGED(24,1)
      IND=IND+1
      MORE_QUANS(IND) = P25_MUON_TAGGED(25,1)
      IND=IND+1
C
      MORE_QUANS(IND) = SPHERIC
      IND = IND+1
      MORE_QUANS(IND) = YPLANAR
      IND = IND+1
      MORE_QUANS(IND) = APLANAR
      IND = IND+1
C
      MORE_QUANS(IND) = SPHERIC1
      IND = IND+1
      MORE_QUANS(IND) = YPLANAR1
      IND = IND+1
      MORE_QUANS(IND) = APLANAR1
      IND = IND+1
C
      CALL HMATRIX_MAKE_NTUPLE(MORE_NAMES,MORE_QUANS,NUM_MORE)
C
      LISV1 = GZISV1()
      DO WHILE (LISV1.NE.0)
        CALL MZDROP(IXMAIN,LISV1,'V')   !DROP STRUCTUREs BELOW
        LISV1 = LQ(LISV1)
      ENDDO
C
      CALL DHDIR_RESTORE_FILE        !RESTORE PREVIOUS TOPDIR
C
  999 RETURN
      END
