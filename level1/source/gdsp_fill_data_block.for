      SUBROUTINE GDSP_FILL_DATA_BLOCK(PASSED,TTETA,TTPHI,OBJ_NRG,
     &                                   OBJ_TYPE,NUMCANDS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FILL L15CAL_GLOBAL_DSP_DATA_BLOCK
C-
C-   Inputs  :  PASSED     - LOGICAL
C-              TTETA      - INTEGER array of Trigger Tower ETA coordinates
C-              TTPHI      - INTEGER array of Trigger Tower PHI coordinates
C-              OBJ_NRG    - INTEGER array of candidate Et*4
C-              OBJ_TYPE   - INTEGER array of candidate type code
C-              NUMCANDS   - INTEGER number of candidates
C-   Outputs :
C-   Controls:
C-
C-   Created  30-NOV-1993   sFahey
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L15C_TOOL_INIT.INC'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L15CALDBB_DATA_BLOCK.INC'
      INCLUDE 'D0$PARAMS:L15_LOCAL_DSP.PARAMS'
C
      INTEGER FF
      PARAMETER (FF = 255)
      INTEGER BYTE_LENGTH
      PARAMETER (BYTE_LENGTH = 8)
      INTEGER WRDS_PER_ENTRY,MAX_ENTRIES
      PARAMETER (WRDS_PER_ENTRY = 4, MAX_ENTRIES = 16)
C
      LOGICAL PASSED
      INTEGER NUMCANDS,MAXCANDS
      PARAMETER(MAXCANDS=MAX_PER_DSP*NUM_DSPS)
      INTEGER TTETA(MAXCANDS),TTPHI(MAXCANDS)
      INTEGER OBJ_NRG(MAXCANDS),OBJ_TYPE(MAXCANDS)
C
      INTEGER INDEX,STATUS
      INTEGER NENTRIES,ENTRY,TERM,TOOL
      INTEGER IETA,IPHI,REAL_OR_MP
      INTEGER HEADWORD,GDSP_WORD1,GDSP_WORD2
      INTEGER GDSP_WORD3,GDSP_WORD4
C----------------------------------------------------------------------
C
      INDEX = 2
      CALL SBYT(GDSP_ID, HEADWORD, (BYTE1-1)*BYTE_LENGTH + 1,
     &    BYTE_LENGTH)
C
      IF (NUMCANDS .GT. MAX_ENTRIES) THEN
        STATUS = FF
        NENTRIES = MAX_ENTRIES
      ELSE
        STATUS = NUMCANDS
        NENTRIES = NUMCANDS
      ENDIF
C
      CALL SBYT(STATUS, HEADWORD, (BYTE2-1)*BYTE_LENGTH + 1,
     &    BYTE_LENGTH)
      CALL SBYT(MAX_ENTRIES, HEADWORD, (BYTE3-1)*BYTE_LENGTH + 1,
     &    BYTE_LENGTH)
      CALL SBYT(WRDS_PER_ENTRY, HEADWORD, (BYTE4-1)*BYTE_LENGTH + 1,
     &    BYTE_LENGTH)
C
      L15CAL_GLOBAL_DSP_BLOCK(INDEX) = HEADWORD
      INDEX = INDEX + 1
C
C  Next add objects to global list but only if event passed...
C
      IF (PASSED) THEN
C
        DO ENTRY = 1,NENTRIES
          TERM = L15TM_NUM
          TOOL = L15TL_GLB_NUM
          IETA = TTETA(ENTRY)
          IPHI = TTPHI(ENTRY)
          CALL SBYT(TERM, GDSP_WORD1, (BYTE1-1)*BYTE_LENGTH + 1,
     &      BYTE_LENGTH)
          CALL SBYT(TOOL, GDSP_WORD1, (BYTE2-1)*BYTE_LENGTH + 1,
     &      BYTE_LENGTH)
          CALL SBYT(IETA, GDSP_WORD1, (BYTE3-1)*BYTE_LENGTH + 1,
     &      BYTE_LENGTH)
          CALL SBYT(IPHI, GDSP_WORD1, (BYTE4-1)*BYTE_LENGTH + 1,
     &      BYTE_LENGTH)
C
          REAL_OR_MP = 0                  ! DUMMIED FOR NOW
          CALL SBYT(OBJ_TYPE(ENTRY), GDSP_WORD2,
     &      (BYTE1-1)*BYTE_LENGTH + 1, BYTE_LENGTH)
          CALL SBYT(REAL_OR_MP, GDSP_WORD2, (BYTE2-1)*BYTE_LENGTH + 1,
     &      BYTE_LENGTH)
          CALL SBYT(OBJ_NRG(ENTRY), GDSP_WORD2,
     &      (BYTE3-1)*BYTE_LENGTH + 1, 2*BYTE_LENGTH)
C
          GDSP_WORD3 = 0
          GDSP_WORD4 = 0
          L15CAL_GLOBAL_DSP_BLOCK(INDEX) = GDSP_WORD1
          INDEX = INDEX + 1
          L15CAL_GLOBAL_DSP_BLOCK(INDEX) = GDSP_WORD2
          INDEX = INDEX + 1
          L15CAL_GLOBAL_DSP_BLOCK(INDEX) = GDSP_WORD3
          INDEX = INDEX + 1
          L15CAL_GLOBAL_DSP_BLOCK(INDEX) = GDSP_WORD4
          INDEX = INDEX + 1
C
        ENDDO
C
      ENDIF ! PASSED
C
  999 RETURN
      END