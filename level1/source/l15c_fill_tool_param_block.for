      SUBROUTINE L15C_FILL_TOOL_PARAM_BLOCK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the tool parameter section of the 
C-                         L15 calorimeter block.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-MAY-1994   sFahey
C-
C-        *NOTE* -- NOT FULLY FILLED
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L15CALDBB_DATA_BLOCK.INC'
      INCLUDE 'D0$PARAMS:L15COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L15C_TERM_INIT.INC'
      INCLUDE 'D0$INC:L15C_TOOL_INIT.INC'
C
      INTEGER BYTE_LENGTH
      PARAMETER (BYTE_LENGTH = 8)
C
      INTEGER INDEX,NUM_WORDS,I
      INTEGER NTERMS
      INTEGER LIST_LNGTH,HEADER
C----------------------------------------------------------------------
C
C   Find number of L15 Cal terms defined
      NTERMS = 0
      DO L15TM_NUM = L15TM_NUM_MIN,L15TM_NUM_MAX
        IF (L15C_PASS(L15CT_NUM,L15TM_NUM).NE.-1)
     &        NTERMS = NTERMS + 1
      ENDDO
C
      CALL SBYT(NTERMS,LIST_LNGTH,(BYTE1-1)*BYTE_LENGTH + 1,
     &    BYTE_LENGTH)
      CALL SBYT(LOC_NTERM,LIST_LNGTH,(BYTE2-1)*BYTE_LENGTH + 1,
     &    BYTE_LENGTH)
      CALL SBYT(NTERMS,LIST_LNGTH,(BYTE3-1)*BYTE_LENGTH + 1,
     &    BYTE_LENGTH)
      CALL SBYT(GLB_NTERM,LIST_LNGTH,(BYTE4-1)*BYTE_LENGTH + 1,
     &    BYTE_LENGTH)
      L15CAL_TOOL_PARAM_BLOCK(2) = LIST_LNGTH
C
C   Fill local parameter section for each term
C
      INDEX = 3
      DO L15TM_NUM = L15TM_NUM_MIN,L15TM_NUM_MAX
        IF (L15C_PASS(L15CT_NUM,L15TM_NUM).NE.-1) THEN
C
          CALL SBYT(L15TM_NUM,HEADER,(BYTE1-1)*BYTE_LENGTH + 1,
     &      BYTE_LENGTH)
          CALL SBYT(1,HEADER,(BYTE2-1)*BYTE_LENGTH + 1,
     &      BYTE_LENGTH)
          CALL SBYT(111,HEADER,(BYTE3-1)*BYTE_LENGTH + 1,  
     &      BYTE_LENGTH)                                   ! NOT FILLED
          CALL SBYT(111,HEADER,(BYTE4-1)*BYTE_LENGTH + 1,  
     &      BYTE_LENGTH)                                   ! NOT FILLED
          L15CAL_TOOL_PARAM_BLOCK(INDEX) = HEADER
          INDEX = INDEX + 1
C
          DO I = 1,(LOC_NLW-1)
            L15CAL_TOOL_PARAM_BLOCK(INDEX) = 0             ! NOT FILLED
            INDEX = INDEX + 1
          ENDDO
C
        ENDIF
C
      ENDDO
C
C   Fill global parameter section for each term
C
      DO L15TM_NUM = L15TM_NUM_MIN,L15TM_NUM_MAX
        IF (L15C_PASS(L15CT_NUM,L15TM_NUM).NE.-1) THEN
C
          CALL SBYT(L15TM_NUM,HEADER,(BYTE1-1)*BYTE_LENGTH + 1,
     &      BYTE_LENGTH)
          CALL SBYT(2,HEADER,(BYTE2-1)*BYTE_LENGTH + 1,
     &      BYTE_LENGTH)
          L15CAL_TOOL_PARAM_BLOCK(INDEX) = HEADER
          INDEX = INDEX + 1
C
          DO I = 1,(GLB_NLW-1)
            L15CAL_TOOL_PARAM_BLOCK(INDEX) = 0             ! NOT FILLED
            INDEX = INDEX + 1
          ENDDO
C
        ENDIF
C
      ENDDO
C
C   Fill number of longwords to follow
C
        NUM_WORDS = 2 + NTERMS*LOC_NLW + NTERMS*GLB_NLW
        L15CAL_TOOL_PARAM_BLOCK(1) = NUM_WORDS - 1
  999 RETURN
      END
