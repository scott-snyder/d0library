      SUBROUTINE L15_GLOBAL_DSP(PASSED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simulate Global DSP.
C-
C-   Inputs  :    Parameter Set from  L15C_TOOL_INIT.INC
C-
C-   Outputs :    L15CAL_GLOBAL_DSP_BLOCK
C-                PASSED       : LOGICAL
C-   Controls:
C-
C-   Created  14-SEP-1993   sFahey
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L15C_TOOL_INIT.INC'
C
      INTEGER NUMCANDS,MAXCANDS
      PARAMETER(MAXCANDS=MAX_PER_DSP*NUM_DSPS)
      INTEGER TTETA(MAXCANDS),TTPHI(MAXCANDS)
      INTEGER IFAILED(MAXCANDS),OBJ_TYPE(MAXCANDS)
      INTEGER OBJ_INRG(MAXCANDS)
C
      LOGICAL PASSED
C
C----------------------------------------------------------------------
      CALL VZERO(IFAILED,MAXCANDS)
      PASSED = .FALSE.
C
C   Call Global DSP tool to see if event passed.
C       At this time tools are hard coded!  This might need to be
C       changed in future versions.
C
      IF (L15TL_GLB_NUM.EQ.2) THEN
        CALL GDSP_TOOL_NCAND_CUT(PASSED,TTETA,TTPHI,OBJ_INRG,OBJ_TYPE)
      ELSE
        CALL ERRMSG('L15_LOCAL_DSP','L15_CAL_SIM',
     &              'INVALID LOCAL DSP TOOL NUMBER','E')  
      ENDIF
C
C
C   PUT GLOBAL DSP CANDIDATES INTO L15CAL BLOCK
C
      CALL GDSP_FILL_DATA_BLOCK(PASSED,TTETA,TTPHI,
     &                          OBJ_INRG,OBJ_TYPE,NUMCANDS)
C
  999 RETURN
      END
