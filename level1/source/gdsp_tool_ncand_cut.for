      SUBROUTINE GDSP_TOOL_NCAND_CUT(PASSED,TTETA,TTPHI,
     &                               OBJ_INRG,OBJ_TYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GLOBAL DSP TOOL.   MAKES A CUT ON THE NUMBER
C-                         OF CANDIDATES NEEDED FOR EVENT TO PASS. ALSO
C-                         ELIMINATES SIDE BY SIDE CANDIDATES.
C-
C-   Inputs  :
C-   Outputs :  PASSED -- LOGICAL
C-              TTETA  -- INTEGER ARRAY (1:MAXCANDS) OF CANDIDATE TT ETA
C-              TTPHI  -- INTEGER ARRAY (1:MAXCANDS) OF CANDIDATE TT PHI
C-              OBJ_INRG -- INT ARRAY (1:MAXCANDS) OF ENERGY IN 1/4 GEV
C-              OBJ_TYPE -- INT ARRAY (1:MAXCANDS) OF TYPE FROM LOCAL DSP
C-
C-   Controls:
C-
C-   Created  13-APR-1994   sFahey
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
      REAL OBJ_NRG(MAXCANDS)
      INTEGER OBJ_INRG(MAXCANDS)
      LOGICAL PASSED
      INTEGER I,J,DEL_IETA,DEL_IPHI,NEWNUM
C----------------------------------------------------------------------
C
C
C   Read in candidates from l15cal_local_dsp data block
C
      CALL GDSP_GET_CANDS(TTETA,TTPHI,OBJ_TYPE,OBJ_NRG,NUMCANDS)
C
C
C   FIND SIDE BY SIDE CANDIDATES
C
C      DO I = 1,NUMCANDS
C        DO J = 1,NUMCANDS
C          IF (I.NE.J) THEN
C
C            DEL_IETA = ABS(TTETA(I) - TTETA(J))
C            IF (((TTETA(I).GT.0).AND.(TTETA(J).LT.0)).OR.       ! CROSS
C     &          ((TTETA(I).LT.0).AND.(TTETA(J).GT.0))) THEN     ! TTETA = 0
C              DEL_IETA = DEL_IETA - 1
C            ENDIF
C
C            DEL_IPHI = ABS(TTPHI(I) - TTPHI(J))
C            IF ((32-DEL_IPHI).LT.DEL_IPHI) THEN       ! PHI WRAP
C              DEL_IPHI = 32 - DEL_IPHI
C            ENDIF
C
C   NEXT, IF CANDIDATES SIDE BY SIDE ELIMINATE SMALLER OF THE TWO
C
C            IF ((DEL_IETA.LE.1).AND.(DEL_IPHI.LE.1)) THEN
C              IF (OBJ_NRG(I).GT.OBJ_NRG(J)) THEN
C                IFAILED(J) = 1
C              ELSE
C                IFAILED(I) = 1
C              ENDIF
C            ENDIF
C          ENDIF
C        ENDDO
C      ENDDO
C
C   MAKE UPDATED LIST OF CANDIDATES
C
C      NEWNUM = 0
C      DO I = 1,NUMCANDS
C        IF (IFAILED(I).EQ.0) THEN
C          NEWNUM = NEWNUM + 1
C          TTETA(NEWNUM) = TTETA(I)
C          TTPHI(NEWNUM) = TTPHI(I)
C          OBJ_NRG(NEWNUM) = OBJ_NRG(I)
C          OBJ_INRG(NEWNUM) = INT(4*OBJ_NRG(I))
C          OBJ_TYPE(NEWNUM) = OBJ_TYPE(I)
C        ENDIF
C      ENDDO
C      NUMCANDS = NEWNUM
C
C   NOW MAKE SURE WE HAVE ENOUGH CANDIDATES IN THE EVENT
C
      IF (NUMCANDS.LT.L15_INTG_PARAMS(L15CT_NUM,L15TM_NUM,
     &                      L15TL_GLB_DSP,L15TL_GLB_NUM,1)) THEN
        GOTO 999
      ELSE
        PASSED = .TRUE.
      ENDIF
C
C
  999 RETURN
      END
