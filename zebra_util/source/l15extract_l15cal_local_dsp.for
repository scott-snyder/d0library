	SUBROUTINE L15EXTRACT_L15CAL_LOCAL_DSP(NBANK,
     &  LDSP_NUM,L15_LDSP_PASS,
     &  NUM_VALID_ENT,ETA_OF_TOWER,PHI_OF_TOWER,EM_ET_1X2,TOTAL_ET_3X3,
     &  DELTA_ETA,DELTA_PHI,PASS_FAIL_FLAG)
C----------------------------------------------------------------------
C-
C-	Purpose and Methods: Extract entries for one local DSP 
C-	from calorimeter L1.5 section of TRGR bank
C-
C-	Inputs:  LDSP_NUM           number of the local dsp desired
C-                                  LDSP_NUM = 1, 2, ..., 11
C-	         NBANK to chose which bank to extract
C-              1 = most recent bank, 2 = older bank
C-
C-	Outputs: L15_LDSP_PASS      copy of L15CAL_LOCAL_DSP_BLOCK
C-                                  for the ldsp number LDSP_NUM
C-               NUM_VALID_ENT      number of valid entries for this ldsp
C-                                    = 0, 1, ...., 8      or
C-                                    = FF(hex),255 for list overflow
C-               ETA_OF_TOWER       Eta coordinate of trigger tower
C-               PHI_OF_TOWER       Phi coordinate of trigger tower
C-               EM_ET_1X2          Object core EM energy
C-               TOTAL_ET_3X3       Object total 3x3 energy
C-		 DELTA_ETA          of neighbor tower used in 1x2 calc.
C-		 DELTA_PHI          of neighbor tower used in 1x2 calc.
C- 	         PASS_FAIL_FLAG     0 = real entry that passed local algor.
C-	                            FF = failed, but saved for mark/pass
C-
C-	Controls:
C-
C-	Created 14-JAN-1994	Gregory Snow
C-      Modified 4-AUG-1994     G. Snow -- now pass 8 entries in LDSP
C-                              list even if list overflows
C-      Modified 17_OCT-1994    G. Snow -- suppress error message calls
C-
C----------------------------------------------------------------------
	IMPLICIT NONE
	INCLUDE 'D0$INC:ZEBCOM.INC'
	INCLUDE 'D0$LINKS:IZTRGR.LINK'
	INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
	INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
C-
	INTEGER PACKEDWORD		! for unpacking longwords
	BYTE BYTES(4)			! for unpacking longwords
	EQUIVALENCE (PACKEDWORD,BYTES)	! for unpacking longwords
        INTEGER PACKEDWORD1             ! for unpacking longwords
        INTEGER*2 WORDS(2)              ! for unpacking longwords
        EQUIVALENCE (PACKEDWORD1,WORDS) ! for unpacking longwords
C-
	INTEGER NBANK
	INTEGER LTRGR,GZTRGR		! pointers to TRGR bank	
	INTEGER GZFIND_CRATE		! pointers to TRGR bank
	INTEGER L15START,PNTR		! pointers to TRGR bank
        INTEGER L15CAL_FRAME_START      ! pointers to TRGR bank
        INTEGER L15CAL_FRAME_PARAM_START   ! pointers to TRGR bank
        INTEGER L15CAL_TOOL_PARAM_START    ! pointers to TRGR bank
	INTEGER L15CAL_LOCAL_DSP_START     ! pointers to TRGR bank
        INTEGER HEADER_LWFOLLOW, FRAME_LWFOLLOW
        INTEGER FRAME_PARAM_LWFOLLOW, TOOL_PARAM_LWFOLLOW
	INTEGER LOCAL_DSP_LWFOLLOW
C
C-
	INTEGER I			! do loop indices
        INTEGER LDSP_NUM                ! input, local DSP sought
	INTEGER L15_LDSP_PASS(1+LOC_NUM_ENT*LOC_NLW_ENT)
	INTEGER NUM_VALID_ENT, ETA_OF_TOWER(LOC_NUM_ENT)
        INTEGER NUM_ENT_EXTRACT 
	INTEGER PHI_OF_TOWER(LOC_NUM_ENT), EM_ET_1X2(LOC_NUM_ENT)
        INTEGER TOTAL_ET_3X3(LOC_NUM_ENT)
	INTEGER DELTA_ETA(LOC_NUM_ENT), DELTA_PHI(LOC_NUM_ENT)
	INTEGER PASS_FAIL_FLAG(LOC_NUM_ENT)
C-
C- Initialize outputs first
C-
	CALL VZERO(L15_LDSP_PASS,1+LOC_NUM_ENT*LOC_NLW_ENT)
	NUM_VALID_ENT = 0
	CALL VZERO(ETA_OF_TOWER,LOC_NUM_ENT)
	CALL VZERO(PHI_OF_TOWER,LOC_NUM_ENT)
	CALL VZERO(EM_ET_1X2,LOC_NUM_ENT)
	CALL VZERO(TOTAL_ET_3X3,LOC_NUM_ENT)
	CALL VZERO(DELTA_ETA,LOC_NUM_ENT)
	CALL VZERO(DELTA_PHI,LOC_NUM_ENT)
	CALL VZERO(PASS_FAIL_FLAG,LOC_NUM_ENT)	
C-
	LTRGR = GZTRGR()
	IF(NBANK .EQ. 2)LTRGR = LQ(LTRGR)
	L15START = GZFIND_CRATE('TRGR',LTRGR,L15CAL_CRATE_ID)
C-	
	IF(L15START. LE. 0) THEN
C-          CALL ERRMSG('GZFIND_CRATE CANT FIND CRATE 81',
C-     &    'L15EXTRACT_L15CAL_LOCAL_DSP',
C-     &    'GZFIND_CRATE CANT FIND CRATE 81','W')
	  RETURN
	ENDIF
C-
C-  read through TRGR to find start of LOCAL_DSP section
C-
        HEADER_LWFOLLOW = IQ(L15START)
        L15CAL_FRAME_START = L15START + HEADER_LWFOLLOW + 1
        FRAME_LWFOLLOW = IQ(L15CAL_FRAME_START)
        L15CAL_FRAME_PARAM_START = L15CAL_FRAME_START +
     &                             FRAME_LWFOLLOW + 1
        FRAME_PARAM_LWFOLLOW = IQ(L15CAL_FRAME_PARAM_START)
        L15CAL_TOOL_PARAM_START = L15CAL_FRAME_PARAM_START +
     &                            FRAME_PARAM_LWFOLLOW + 1
        TOOL_PARAM_LWFOLLOW = IQ(L15CAL_TOOL_PARAM_START)
	L15CAL_LOCAL_DSP_START = L15CAL_TOOL_PARAM_START +
     &                           TOOL_PARAM_LWFOLLOW + 1
	LOCAL_DSP_LWFOLLOW = IQ(L15CAL_LOCAL_DSP_START)
C-
C-        IF(LOCAL_DSP_LWFOLLOW+1 .NE. L15CAL_LOCAL_DSP_LENGTH)THEN
C-          CALL ERRMSG('UNEXPECTED L15CAL LOCAL DSP LENGTH',
C-     &    'L15EXTRACT_L15CAL_LOCAL_DSP',
C-     &    'UNEXPECTED L15CAL LOCAL DSP LENGTH','W')
C-        ENDIF
C-
C-
C-  pointer to start of particular LDSP information
C-
	PNTR = L15CAL_LOCAL_DSP_START
	PNTR=PNTR+1+(LDSP_NUM-1)*(1+LOC_NUM_ENT*LOC_NLW_ENT)
C-
C-  fill in L15_LDSP_PASS for output
C-
	DO I=1,1+LOC_NUM_ENT*LOC_NLW_ENT
           L15_LDSP_PASS(I)=IQ(PNTR)
           PNTR=PNTR+1
        ENDDO
C-
C-  fill other output arrays
C-
        PACKEDWORD=L15_LDSP_PASS(1)
        NUM_VALID_ENT = BYTES(BYTE2)
	NUM_ENT_EXTRACT = NUM_VALID_ENT
C-- deal with overflows  == FF(hex)
C-- if list overflows, then extract 8 entries that are there
C--
        IF(NUM_VALID_ENT .EQ. 255) NUM_ENT_EXTRACT = LOC_NUM_ENT
	IF(NUM_ENT_EXTRACT.GE.1 .AND. NUM_ENT_EXTRACT.LE.LOC_NUM_ENT)THEN
        DO I=1,NUM_ENT_EXTRACT
C-  from 1st longword of object entry
           PACKEDWORD=L15_LDSP_PASS(3*I-1)
           ETA_OF_TOWER(I)=BYTES(BYTE3)
           PHI_OF_TOWER(I)=BYTES(BYTE4)
C-  from 2nd longword of object entry
C-  EM_ET_1X2 is second half of 2nd longword
           PACKEDWORD1 = L15_LDSP_PASS(3*I)
           EM_ET_1X2(I)= WORDS(WORD2)
C-  pass/fail flag is 2nd byte of 2nd longword
	   PACKEDWORD = L15_LDSP_PASS(3*I)
	   PASS_FAIL_FLAG(I) = BYTES(BYTE2)
C-  from 3rd longword of object entry           
C-  TOTAL_ET_3X3 is first half of 3rd longword
           PACKEDWORD1 = L15_LDSP_PASS(3*I+1)
           TOTAL_ET_3X3(I)= WORDS(WORD1)
C-  DELTA_ETA is 3rd byte of 3rd longword
C-  DELTA_PHI is 4th byte of 3rd longword
	   PACKEDWORD = L15_LDSP_PASS(3*I+1)
	   DELTA_ETA(I) = BYTES(BYTE3)
	   DELTA_PHI(I) = BYTES(BYTE4)
        ENDDO
	ENDIF
C-
999	RETURN
	END





