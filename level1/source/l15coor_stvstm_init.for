      SUBROUTINE L15COOR_STVSTM_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   4-APR-1994   Zhengzhi Zhang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L15COOR_PARSER.PARAMS'
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L15C_TERM_INIT.INC'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
 
      INTEGER CRATE,TERM
      INTEGER I,J,K,COUNT
C----------------------------------------------------------------------
      DO I = L15CT_NUM_MIN, L15CT_NUM_MAX
        DO J = L15TM_NUM_MIN,L15TM_NUM_MAX
          DO K = L15ST_NUM_MIN,L15ST_NUM_MAX
            L15C_SPTRG(I,J,K) = 0
          ENDDO
        ENDDO
      ENDDO

      DO COUNT = 1,LIST_TOP
C
C       GET pass number
c
        IF(LIST(COUNT,OBJ_INDEX).EQ.KEY_SPTRG)THEN
          IF((LIST(COUNT,ITEM_T_INDEX).EQ.PARSE_ASSERTED).AND.
     +       (LIST(COUNT,ITEM_INDEX).GE.L15ST_NUM_MIN).AND.
     +       (LIST(COUNT,ITEM_INDEX).LE.L15ST_NUM_MAX))THEN
            L15C_SPTRG(CRATE,TERM,LIST(COUNT,ITEM_INDEX)) = 1
c            WRITE(*,*)CRATE,TERM,
c     +          L15C_SPTRG(CRATE,TERM,LIST(COUNT,ITEM_INDEX))
          ELSE
            PARSE_STATUS = PARSE_BAD_PARAM
            GOTO 999
          ENDIF
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
        ELSE
          PARSE_STATUS = PARSE_BAD_PARAM
          GOTO 999
        ENDIF
      ENDDO

  999 RETURN
      END
