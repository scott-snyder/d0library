      LOGICAL FUNCTION L15CAL_EM_SIM(L15CAL_TERM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : L1.5 EM SIMULATOR
C-
C-   Returned value  : 
C-   Inputs  : L15CAL_TERM  -- L15 CAL TERM NUMBER
C-   Outputs : 
C-   Controls: 
C-
C-   Created  14-SEP-1993   sFahey
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L15COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L15C_TOOL_INIT.INC'
C
      INTEGER L15CAL_TERM,NUM_TOOLS,I
      LOGICAL PASSED
C----------------------------------------------------------------------
C
C
      L15CAL_EM_SIM = .FALSE.
C
      L15TM_NUM = L15CAL_TERM
C
      L15CT_NUM = L15CT_NUM_MIN     ! Only one crate for now
C
C   Get LOCAL tool number from L15C_TOOL_INIT
C
      NUM_TOOLS = 0
      DO I = L15TL_NUM_MIN , L15TL_NUM_MAX
        IF (L15_TYPE_PARAMS(L15CT_NUM,L15TM_NUM,L15TL_LOC_DSP,I,1)
     &        .NE.L15TL_PARAM_UNUSED) THEN
          L15TL_LOC_NUM = I
          NUM_TOOLS = NUM_TOOLS + 1
        ENDIF
      ENDDO
C
      IF (NUM_TOOLS.NE.1) THEN
        CALL ERRMSG('L15CAL_EM_SIM','L15_CAL_SIM',
     &    'INVALID NUMBER OF LOCAL TOOLS FOR THIS TERM','E')
      ENDIF
C
C   Get GLOBAL tool number from L15C_TOOL_INIT
C
      NUM_TOOLS = 0
      DO I = L15TL_NUM_MIN , L15TL_NUM_MAX
        IF (L15_TYPE_PARAMS(L15CT_NUM,L15TM_NUM,L15TL_GLB_DSP,I,1)
     &        .NE.L15TL_PARAM_UNUSED) THEN
          L15TL_GLB_NUM = I
          NUM_TOOLS = NUM_TOOLS + 1
        ENDIF
      ENDDO
C
      IF (NUM_TOOLS.NE.1) THEN
        CALL ERRMSG('L15CAL_EM_SIM','L15_CAL_SIM',
     &    'INVALID NUMBER OF GLOBAL TOOLS FOR THIS TERM','E')
      ENDIF
C
C
      CALL L15_LOCAL_DSP
C
C
      CALL L15_GLOBAL_DSP(PASSED)
C
C
      IF (PASSED) THEN
        L15CAL_EM_SIM = .TRUE.
      ENDIF
      GOTO 999
C
C
  999 RETURN
      END
