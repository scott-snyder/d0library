      SUBROUTINE L15_LOCAL_DSP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simulate the local DSPs for L15 EM trigger
C-
C-   Inputs  : Thresholds from L15C_REFSET_THRESHOLDS.INC
C-
C-   Outputs :    L15CAL_LOCAL_DSP_BLOCK
C-
C-   Controls:
C-
C-   Created  30-JUN-1993   sFahey
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L15C_REFSET_THRESHOLDS.INC'
      INCLUDE 'D0$INC:L15C_TOOL_INIT.INC'
      INCLUDE 'D0$PARAMS:L15_LOCAL_DSP.PARAMS'
      INCLUDE 'D0$INC:L15_LOCAL_DSP.INC'
C
      INTEGER NUMCANDS
C
      INTEGER L1ETAC,L1PHIC
C
      INTEGER DSP
      INTEGER ETA_SIGN
      REAL EM_TT_THRESH,TOT_TT_THRESH
      INTEGER OBJ_INFO1,OBJ_INFO2,OBJ_INFO3
      INTEGER OBJ_WORD1(MAX_PER_DSP)
      INTEGER OBJ_WORD2(MAX_PER_DSP)
      INTEGER OBJ_WORD3(MAX_PER_DSP)
      LOGICAL PASSED
C
C----------------------------------------------------------------------
C
C   Get trigger tower info and put into arrays EMET, TOTET
C     (in common block L15_LOCAL_DSP)
C
      CALL L15C_GET_ENERGIES
C
C   Loop over all DSPs
C
      DO DSP = 1, NUM_DSPS
C
        NUMCANDS = 0
        DO L1PHIC = PHI_MIN,PHI_MAX
          DO L1ETAC = DSP_ETA_LOW(DSP),DSP_ETA_HIGH(DSP)
C
            IF (L1ETAC.NE.0) THEN
              IF (L1ETAC.GT.0) ETA_SIGN = POS_ETA
              IF (L1ETAC.LT.0) ETA_SIGN = NEG_ETA
C
              EM_TT_THRESH       = 0.001*(L15_EMT_THRSHLD
     &              (L15CT_NUM,L15TM_NUM,ETA_SIGN,ABS(L1ETAC),L1PHIC))
C
              TOT_TT_THRESH      = 0.001*(L15_TOT_THRSHLD
     &              (L15CT_NUM,L15TM_NUM,ETA_SIGN,ABS(L1ETAC),L1PHIC))
C
              IF (EMET(L1ETAC,L1PHIC).GT.EM_TT_THRESH) THEN
C
                IF (TOTET(L1ETAC,L1PHIC).GT.TOT_TT_THRESH) THEN
C
C   Call local DSP tool to see if this candidate passed.
C       At this time tool numbers are hard coded!  This might be
C       changed in future versions.
C   
C
                  IF (L15TL_LOC_NUM.EQ.1) THEN
                    CALL LDSP_TOOL_3X3(L1ETAC,L1PHIC,
     &                         PASSED,OBJ_INFO1,OBJ_INFO2,OBJ_INFO3)
                  ELSEIF (L15TL_LOC_NUM.EQ.2) THEN
                    CALL LDSP_TOOL_EM_ISO(L1ETAC,L1PHIC,
     &                         PASSED,OBJ_INFO1,OBJ_INFO2,OBJ_INFO3)
                  ELSE
                    CALL ERRMSG('L15_LOCAL_DSP','L15_CAL_SIM',
     &                'INVALID LOCAL DSP TOOL NUMBER','E')
                  ENDIF
C
                  IF (PASSED) THEN
                    NUMCANDS = NUMCANDS + 1
                    IF (NUMCANDS.LE.MAX_PER_DSP) THEN
                      OBJ_WORD1(NUMCANDS) = OBJ_INFO1
                      OBJ_WORD2(NUMCANDS) = OBJ_INFO2
                      OBJ_WORD3(NUMCANDS) = OBJ_INFO3
                    ENDIF
                  ENDIF
C
                ENDIF
C
              ENDIF
C
            ENDIF
C
          ENDDO
        ENDDO
C
C   Now put local DSP parameters into TRGR block
C
        CALL LDSP_FILL_DATA_BLOCK(DSP,NUMCANDS,
     &                        OBJ_WORD1,OBJ_WORD2,OBJ_WORD3)
C
C
      ENDDO  ! End of loop over DSPs
C
  999 RETURN
      END
