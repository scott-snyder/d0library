      SUBROUTINE L15COOR_TOOL_INIT(TYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   1-APR-1994   Zhengzhi Zhang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER TYPE
      INCLUDE 'D0$PARAMS:L15COOR_PARSER.PARAMS'
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L15C_TOOL_INIT.INC'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'

      INTEGER CRATE, TERM, TOOL,PARAM
      INTEGER I,J,K,M,N,COUNT 
C----------------------------------------------------------------------
      IF(TYPE.EQ.L15COOR_LOCDSP)M = L15TL_LOC_DSP
      IF(TYPE.EQ.L15COOR_GLBDSP)M = L15TL_GLB_DSP
      DO I = L15CT_NUM_MIN, L15CT_NUM_MAX
        DO J = L15TM_NUM_MIN,L15TM_NUM_MAX
          DO K = L15TL_NUM_MIN,L15TL_NUM_MAX
            DO N = L15PM_NUM_MIN,L15PM_NUM_MAX
              L15_TYPE_PARAMS(I,J,M,K,N) = L15TL_PARAM_UNUSED
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      PARAM = L15PM_NUM_MIN
      DO COUNT = 1,LIST_TOP
C
CZZZ  Parsing parameters
        IF(LIST(COUNT,OBJ_INDEX).EQ.KEY_WITH_PARAMS)THEN
          IF(LIST(COUNT,ITEM_T_INDEX).EQ.PARSE_ASSERTED.OR.
     &       LIST(COUNT,ITEM_T_INDEX).EQ.PARSE_NEGATED)THEN
            PARAM = PARAM + 1
            IF(PARAM.GT.L15PM_NUM_MAX)THEN
              PARSE_STATUS = PARSE_BAD_PARAM
              GOTO 999  
            ENDIF         
            L15_TYPE_PARAMS(CRATE,TERM,M,TOOL,PARAM) 
     &          = L15TL_PARAM_INT
            L15_INTG_PARAMS(CRATE,TERM,M,TOOL,PARAM) 
     &          = LIST(COUNT,ITEM_INDEX)
c            WRITE(*,*)L15_INTG_PARAMS(CRATE,TERM,M,TOOL,PARAM)
          ELSE IF(LIST(COUNT,ITEM_T_INDEX).EQ.PARSE_FLOAT_INT)THEN
            PARAM = PARAM + 1
            IF(PARAM.GT.L15PM_NUM_MAX)THEN
              PARSE_STATUS = PARSE_BAD_PARAM
              GOTO 999  
            ENDIF         
            L15_TYPE_PARAMS(CRATE,TERM,M,TOOL,PARAM) 
     &          = L15TL_PARAM_FLT
            I = 2
            DO WHILE(I.LT.LIST(COUNT+1,ITEM_INDEX))
              I = I*10
            ENDDO
            I = I/2
            L15_FLOAT_PARAMS(CRATE,TERM,M,TOOL,PARAM) 
     &          = FLOAT(LIST(COUNT,ITEM_INDEX)) +  
     &            FLOAT(LIST(COUNT+1,ITEM_INDEX)-I)/FLOAT(I) 
c            WRITE(*,*)L15_FLOAT_PARAMS(CRATE,TERM,M,TOOL,PARAM)
          ENDIF
C
CZZZ    Handle other key words
        ELSE IF(LIST(COUNT,OBJ_INDEX).EQ.KEY_CRATE)THEN
          CRATE = LIST(COUNT,ITEM_INDEX)
          IF(CRATE.GT.L15CT_NUM_MAX.OR.CRATE.LT.L15CT_NUM_MIN)THEN
            PARSE_STATUS = PARSE_BAD_PARAM
            GOTO 999
          ENDIF
        ELSE IF(LIST(COUNT,OBJ_INDEX).EQ.KEY_TERM)THEN
          TERM = LIST(COUNT,ITEM_INDEX)
          IF(TERM.GT.L15TM_NUM_MAX.OR.TERM.LT.L15TM_NUM_MIN)THEN
            PARSE_STATUS = PARSE_BAD_PARAM
            GOTO 999
          ENDIF
        ELSE IF(LIST(COUNT,OBJ_INDEX).EQ.KEY_USE_TOOL)THEN
          TOOL = LIST(COUNT,ITEM_INDEX)
          IF(TOOL.GT.L15TL_NUM_MAX.OR.TOOL.LT.L15TL_NUM_MIN)THEN
            PARSE_STATUS = PARSE_BAD_PARAM
            GOTO 999
          ENDIF
        ELSE
          PARSE_STATUS = PARSE_BAD_PARAM
          GOTO 999
        ENDIF
      ENDDO

  999 RETURN
      END
