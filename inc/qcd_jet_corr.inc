C----------------------------------------------------------------------
C-
C-   Created   5-JUL-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
C: MPF
      REAL A1_R(8, 3 )
      REAL A0_R(8, 3 ), A0ERR_R(8,3), MPF_MID(8), MPF_HIGH(8)
      SAVE A1_R, A0_R, A0ERR_R
C: DIJET ASSYM
      INTEGER NDIJ_ET_BINS
      PARAMETER( NDIJ_ET_BINS = 5 )
      REAL DIJ_ETAVE( NDIJ_ET_BINS, 3 )
      REAL DIJPAR( 6*6, NDIJ_ET_BINS, 3 )
C: Others
      REAL UNDER_DENSITY, ZEROZSP_DENSITY, UNDER_ERROR, ZEROZSP_ERROR
      REAL UNDER_ICD_DENSITY
      REAL OOC_NOM_R(3)
      REAL OOC_ERR_R(3)
      REAL EMSCALE_R, EMSCALE_ER1_R, EMSCALE_ER2_R,HVSCALE_R
C: Local variables to JET_CAL routine
      REAL A1(8)
      REAL A0(8), A0ERR(8), ETX(8), EMSCALE, EMS_ER1, EMS_ER2, HVSCALE
      REAL MPF_MED, MPF_MAX, E_MED, E_MAX
      REAL OOC_NOM, UNDER, ZEROS, UNDERERR, ZEROERR
      REAL OOC_ERR
      LOGICAL OOC
      INTEGER ICONE
      COMMON /QCD_JET_CORR/ DIJ_ETAVE, DIJPAR, A0,
     &  A0ERR, A1,
     &  EMSCALE, EMS_ER1, EMS_ER2, OOC_NOM, UNDER, ZEROS, UNDERERR,
     &  ZEROERR, OOC_ERR, ICONE, MPF_MED, MPF_MAX, E_MED, E_MAX, OOC
