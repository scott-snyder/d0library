      SUBROUTINE L15C_FILL_DEBUG_SECTION(EVENT_TYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill common block L15CAL_DEBUG_BLOCK
C-
C-   Inputs  : EVENT_TYPE = 0 for normal events; = 1 for Mark & Pass events
C-   Outputs :
C-   Controls:
C-
C-   Created  11-MAY-1994   Dan Owen
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L15C_TERM_INIT.INC'
      INCLUDE 'D0$INC:L15CALDBB_DATA_BLOCK.INC'
      INTEGER  EVENT_TYPE, INDEX, WDS_T_FLW, TERM(8),
     &  TRM_INDX
      INTEGER L15TM_NUM, NTERMS, FIRST, LDSP
      INTEGER L15CT_NUM
      DATA FIRST/0/, L15CT_NUM/0/
C-  note that routine presently setup to only write out local crate 0's data
C-  (corresponds to crate ID = 81)
C----------------------------------------------------------------------
C-  first call initalize term array with numbers of active terms for this run
      IF ( FIRST .EQ. 0 ) THEN
        NTERMS = 0
        DO L15TM_NUM = L15TM_NUM_MIN,L15TM_NUM_MAX
          IF (L15C_PASS(L15CT_NUM,L15TM_NUM).NE.-1) THEN
            NTERMS = NTERMS + 1
            TERM(NTERMS)=L15TM_NUM
            FIRST = FIRST + 1
          ENDIF
        ENDDO
      ENDIF
C
C-  normal events only get type 0 and type 4 blocks filled
C-  Mark & Pass events get all types written
      INDEX = 2
      CALL L15C_FILL_DEBUG_T0(INDEX,EVENT_TYPE,WDS_T_FLW)
      INDEX = INDEX + WDS_T_FLW + 1
      IF ( EVENT_TYPE .NE. 0 ) THEN
C-  Loop over local DSPs
        DO LDSP = 1 , NUM_DSPS
          CALL L15C_FILL_DEBUG_T1(INDEX,LDSP,WDS_T_FLW)
          INDEX = INDEX + WDS_T_FLW + 1
C-  Loop over terms making type 2 blocks
          DO TRM_INDX = 1 , NTERMS
            CALL L15C_FILL_DEBUG_T2(INDEX,L15CT_NUM,TERM(TRM_INDX),LDSP,
     &        WDS_T_FLW)
            INDEX = INDEX + WDS_T_FLW + 1
          ENDDO
C-  Loop over terms making type 3 blocks
          DO TRM_INDX = 1 , NTERMS
            CALL L15C_FILL_DEBUG_T3(INDEX,L15CT_NUM,TERM(TRM_INDX),LDSP,
     &        WDS_T_FLW)
            INDEX = INDEX + WDS_T_FLW + 1
          ENDDO
        ENDDO
      ENDIF
C-  write last block (type 4)
      CALL L15C_FILL_DEBUG_T4(INDEX,WDS_T_FLW)
      INDEX = INDEX + WDS_T_FLW
C-  fill in first word (words to follow) of DEBUG section
      L15CAL_DEBUG_BLOCK(1)=INDEX-1
  999 RETURN
      END
