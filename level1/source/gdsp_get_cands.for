      SUBROUTINE GDSP_GET_CANDS(TTETA,TTPHI,OBJ_TYPE,OBJ_NRG,NUMCANDS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get local DSP candidates from the
C-                         L15cal_local_dsp data block for the
C-                         global DSP
C-
C-   Inputs  :
C-   Outputs :    TTETA    - INTEGER array of Candidate Trigger Tower ETAs
C-                TTPHI    -                     "                    PHIs
C-                OBJ_TYPE - INTEGER array of Candidate Type code
C-                OBJ_NRG  - REAL array of Candidate Energy
C-                NUMCANDS - INTEGER number of candidates
C-   Controls: 
C-
C-   Created  29-NOV-1993   sFahey
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L15COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L15C_TOOL_INIT.INC'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L15CALDBB_DATA_BLOCK.INC'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
C
      INTEGER FF
      PARAMETER (FF = 255)
      INTEGER WRDS_PER_ENTRY,HEADWRD
      PARAMETER (HEADWRD = 1)
      INTEGER NUMCANDS,MAXCANDS,DSP
      PARAMETER(MAXCANDS=MAX_PER_DSP*NUM_DSPS)
      INTEGER TTETA(MAXCANDS),TTPHI(MAXCANDS)
      INTEGER OBJ_TYPE(MAXCANDS)
      REAL OBJ_NRG(MAXCANDS)
C
      INTEGER INDEX,NUM_ENTRIES,ENTRY,TERM,TOOL
      INTEGER NRG_INT,REAL_OR_MP,JBYT
C
      INTEGER READIN
      BYTE BYTES(4)
      EQUIVALENCE (BYTES,READIN)
C----------------------------------------------------------------------
C
      NUMCANDS = 0
      DO DSP = 1,NUM_DSPS
        INDEX = (DSP-1)*(MAX_PER_DSP*WRDS_PER_ENTRY+HEADWRD) + 1
        INDEX = INDEX + HEADWRD
        READIN = L15CAL_LOCAL_DSP_BLOCK(INDEX)
        NUM_ENTRIES = BYTES(BYTE2)
        IF (NUM_ENTRIES .EQ. FF) THEN
C           OVERFLOW OF ENTRIES
          NUM_ENTRIES = 8
        ENDIF
        WRDS_PER_ENTRY = BYTES(BYTE4)
        DO ENTRY = 1, NUM_ENTRIES
          INDEX = INDEX + 1
          READIN = L15CAL_LOCAL_DSP_BLOCK(INDEX)
          TERM = BYTES(BYTE1)
          TOOL = BYTES(BYTE2)
C
          IF ((TOOL.EQ.L15TL_LOC_NUM).AND.
     &        (TERM.EQ.L15TM_NUM)) THEN
            NUMCANDS = NUMCANDS + 1
            TTETA(NUMCANDS) = BYTES(BYTE3)
            TTPHI(NUMCANDS) = BYTES(BYTE4)
            INDEX = INDEX + 1
            READIN = L15CAL_LOCAL_DSP_BLOCK(INDEX)
            OBJ_TYPE(NUMCANDS) = BYTES(BYTE1)
            REAL_OR_MP = BYTES(BYTE2)
            NRG_INT = JBYT(READIN,17,16)
            OBJ_NRG(NUMCANDS) = FLOAT(NRG_INT)/4
          ENDIF
C
        ENDDO
      ENDDO
C
  999 RETURN
      END
