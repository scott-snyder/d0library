      SUBROUTINE L15C_FILL_FRAME_PARAM_BLOCK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the frame parameter section of the
C-                         L15 calorimeter block
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-MAY-1994   sFahey
C-
C-      *NOTE* NOT FULLY FILLED YET
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
      INTEGER NUM_WORDS,NTERMS,HEADER,INDEX,I
      INTEGER UNI_1ST_WRD
      PARAMETER (UNI_1ST_WRD = 3)
      INTEGER L15CT_ID
      PARAMETER (L15CT_ID=81)
      INTEGER MAX_TOOL_PARAMS
      PARAMETER (MAX_TOOL_PARAMS = 12)
C----------------------------------------------------------------------
C
C   Find number of L15 Cal terms defined
      NTERMS = 0
      DO L15TM_NUM = L15TM_NUM_MIN,L15TM_NUM_MAX
        IF (L15C_PASS(L15CT_NUM,L15TM_NUM).NE.-1)
     &        NTERMS = NTERMS + 1
      ENDDO
C
C   Fill Universal Parameter section
C
      CALL SBYT(L15CT_ID,HEADER,(BYTE4-1)*BYTE_LENGTH + 1,
     &    BYTE_LENGTH)
      INDEX = UNI_1ST_WRD
      L15CAL_FRAME_PARAM_BLOCK(INDEX) = HEADER
      INDEX = INDEX + 1
      L15CAL_FRAME_PARAM_BLOCK(INDEX) = NTERMS  ! Num terms for this crate
      INDEX = INDEX + 1
      L15CAL_FRAME_PARAM_BLOCK(INDEX) = 0       ! Num Universal params
      INDEX = INDEX + 1
C
C   Fill Frame Parameter section
C
      INDEX = UNI_1ST_WRD + FRAME_UNI_NLW
      DO L15TM_NUM = L15TM_NUM_MIN,L15TM_NUM_MAX
        IF (L15C_PASS(L15CT_NUM,L15TM_NUM).NE.-1) THEN
C
          CALL SBYT(L15TM_NUM,HEADER,(BYTE1-1)*BYTE_LENGTH + 1,
     &      BYTE_LENGTH)
          CALL SBYT(0,HEADER,(BYTE2-1)*BYTE_LENGTH + 1,
     &      BYTE_LENGTH)
          L15CAL_FRAME_PARAM_BLOCK(INDEX) = HEADER
          INDEX = INDEX + 1
          L15CAL_FRAME_PARAM_BLOCK(INDEX) = L15C_PASS(L15CT_NUM,
     &      L15TM_NUM)
          INDEX = INDEX + 1
          L15CAL_FRAME_PARAM_BLOCK(INDEX) = 0 ! MASK OF SPEC TRIGS
          INDEX = INDEX + 1
          L15CAL_FRAME_PARAM_BLOCK(INDEX) = 0 ! NUM PARAMS
          INDEX = INDEX + 1
          DO I = 1,MAX_TOOL_PARAMS
            L15CAL_FRAME_PARAM_BLOCK(INDEX) = 0 ! Ith param
            INDEX = INDEX + 1
          ENDDO
C
        ENDIF
      ENDDO
C
C   Fill number of longwords to follow
C
      NUM_WORDS = 2 + FRAME_UNI_NLW + NTERMS*FRAME_TERM_NLW
      L15CAL_FRAME_PARAM_BLOCK(1) = NUM_WORDS - 1
  999 RETURN
      END
