      SUBROUTINE L15CAL_ESUM_FILL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills the TR15 ESUM summary bank from the
C-                         Cal Level 1.5 part of TRGR bank
C-
C-	Explanation of information contained in QUAL_BITS entry in ESUM
C-	bank, i.e. the last argument of ESUMFL, filled with FLAG_WORD here
C-
C     Bits 16 to 32 contain 3x3 ET, IN 1/4 GEV COUNTS!!!
C     Bit 1:  0 = Normal Event, 1 = Mark and Forced Pass Event
C     BIT 2:  0 = Event Passed, 1 = Failed Local Tool cuts
C     BITS 3,4: DELTA ETA value   0,0 means DELTA_ETA = -1
C                                 0,1 means DELTA_ETA = 0
C                                 1,0 means DELTA_ETA = 1
C     BITS 5,6: DELTA PHI value   0,0 means DELTA_PHI = -1
C                                 0,1 means DELTA_PHI = 0
C                                 1,0 means DELTA_PHI = 1
C *** BIT 7: 0 means Crate 81, 1 means Crate 91, still to be built
C *** Leave at 0 until new crate is built
C
C     BITS 8,9,10: Term Number that generated this entry
C
C *** BIT 11: 0 means LDSP entry list is complete (i.e. not overflowed),
C ***         1 means LDSP entry list is overflowed
C-
C-
C-   Controls: None.
C-
C-   Created MAY-15-1994  G. Snow -- fashioned after Amber's L1ESUM_TRGR
C-   Modified AUG-2-1994  G. Snow -- added bit 11 to FLAG_WORD for LDSP
C-                        list overflows, also overflowed lists are now
C-                        written to ESUM 
C-   Modified DEC-22-1995 G. Snow -- added separate ESUM filling for
C-                        electron and jet objects
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS/LIST'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:DBITT.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
C
      INTEGER PACKEDWORD              ! for unpacking longwords
      INTEGER*2 WORDS(2)              ! for unpacking longwords
      EQUIVALENCE (PACKEDWORD,WORDS)  ! for unpacking longwords
C
      INTEGER UNPACKWORD              ! for unpacking longwords
      BYTE BYTES(4)                   ! for unpacking longwords
      EQUIVALENCE (UNPACKWORD,BYTES)  ! for unpacking longwords
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER NETA_OFF_HI, NPHI_OFF_HI
      INTEGER LTRGR_CAL15,GZTRGR
      INTEGER GZFIND_CRATE
      INTEGER I, NUMENT
      INTEGER LDSP, OBJ_TYP
C
C- for HEADER extract
        LOGICAL DATA_VALID,CYCLE_PERFORMED,FW_TERM_EARLY
        INTEGER MARK_PASS_MASK, MARK_PASS_INTEGER
        INTEGER L15_HEADER_PASS(L15CAL_HEADER_LENGTH)
C- for LDSP extract
        INTEGER L15_LDSP_PASS(1+LOC_NUM_ENT*LOC_NLW_ENT)
        INTEGER NUM_VALID_ENT, ETA_OF_TOWER(LOC_NUM_ENT)
        INTEGER PHI_OF_TOWER(LOC_NUM_ENT), EM_ET_1X2(LOC_NUM_ENT)
        INTEGER TOTAL_ET_3X3(LOC_NUM_ENT)
        INTEGER DELTA_ETA(LOC_NUM_ENT), DELTA_PHI(LOC_NUM_ENT)
        INTEGER PASS_FAIL_FLAG(LOC_NUM_ENT)
C        REAL RATIO
	REAL CAL_ETA, CAL_PHI, EM_ET_1X2_GEV
	INTEGER FLAG_WORD
	INTEGER DELTA_ETA_PASS, DELTA_PHI_PASS, TERM_NUM_PASS
	INTEGER LIST_OVERFLOW
C
C
      IF (FIRST) THEN
        CALL CL2_RING_INIT    ! FILL common block for conversion
                              ! eta and phi to offline phi.
        FIRST = .FALSE.
        NETA_OFF_HI = NTTLTETA-1
        NPHI_OFF_HI = NTTLTPHI-1
      END IF
C
C
C **** See whether there is a crate 81, if not, return
C
      LTRGR_CAL15 = GZFIND_CRATE( 'TRGR', GZTRGR(), L15CAL_CRATE_ID)
      IF(LTRGR_CAL15.LE.0) THEN
	RETURN
      ENDIF
C
C **** Check data valid flag in header, if data not valid, return
C
	CALL L15EXTRACT_L15CAL_HEADER(1,L15_HEADER_PASS,DATA_VALID,
     &              CYCLE_PERFORMED,FW_TERM_EARLY,MARK_PASS_MASK)
	IF(.NOT. DATA_VALID) THEN
	  RETURN
	ENDIF
C
C *** Get Mark/Forced Pass flag ready for FLAG_WORD
C
	MARK_PASS_INTEGER = 0
	IF(MARK_PASS_MASK .NE. 0) MARK_PASS_INTEGER = 1
C
C *** Loop over 11 local DSP'S
C
	DO I=1,11
        LDSP=I
        CALL L15EXTRACT_L15CAL_LOCAL_DSP(1,LDSP,L15_LDSP_PASS,
     &  NUM_VALID_ENT,ETA_OF_TOWER,PHI_OF_TOWER,EM_ET_1X2,TOTAL_ET_3X3,
     &  DELTA_ETA,DELTA_PHI,PASS_FAIL_FLAG)
C
C *** Skip if there are no valid entries, or more than eight
C *** Deal with overflowed list separately
C
	IF(NUM_VALID_ENT .LT. 1) GO TO 1001
	IF(NUM_VALID_ENT .GT. 8 .AND. NUM_VALID_ENT .NE. 255) THEN
	  GO TO 1001
	ENDIF
C
	LIST_OVERFLOW = 0
	IF(NUM_VALID_ENT .EQ. 255) THEN
	  LIST_OVERFLOW = 1
	  NUM_VALID_ENT = 8
	ENDIF
C
C *** Loop over found objects
C
	DO NUMENT=1,NUM_VALID_ENT	
C *** First see if object is electron(1) or jet(2) object
           UNPACKWORD = L15_LDSP_PASS(LOC_NLW_ENT * NUMENT)
           OBJ_TYP = BYTES(BYTE1)	
C *** Prepare FLAG_WORD
	   PACKEDWORD = 0
C *** Bits 16 to 32 contain 3x3 ET, IN 1/4 GEV COUNTS!!!
	   WORDS(WORD2) = TOTAL_ET_3X3(NUMENT)
C *** Bit 1:  0 = Normal Event, 1 = Mark and Forced Pass Event
	   IF(MARK_PASS_INTEGER .EQ. 1)CALL SBIT1(PACKEDWORD,1)
C *** BIT 2:  0 = Pass, 1 = Fail Local Tool cuts
	   IF(PASS_FAIL_FLAG(NUMENT) .NE. 0)CALL SBIT1(PACKEDWORD,2)
C *** BITS 3,4: DELTA ETA value   0,0 means DELTA_ETA = -1
C ***                             0,1 means DELTA_ETA = 0
C ***                             1,0 means DELTA_ETA = 1
	   DELTA_ETA_PASS = DELTA_ETA(NUMENT) + 1
	   CALL SBYT(DELTA_ETA_PASS,PACKEDWORD,3,2)
C *** BITS 5,6: DELTA PHI value   0,0 means DELTA_PHI = -1
C ***                             0,1 means DELTA_PHI = 0
C ***                             1,0 means DELTA_PHI = 1
	   DELTA_PHI_PASS = DELTA_PHI(NUMENT) + 1
	   CALL SBYT(DELTA_PHI_PASS,PACKEDWORD,5,2)
C *** BIT 7: 0 means Crate 81, 1 means Crate 91, still to be built
C *** Leave at 0 until new crate is built
C
C *** BITS 8,9,10: Local Tool Number that generated this entry
C
	   UNPACKWORD = L15_LDSP_PASS(LOC_NLW_ENT*NUMENT-1)
	   TERM_NUM_PASS = BYTES(BYTE1)
	   CALL SBYT(TERM_NUM_PASS,PACKEDWORD,8,3)
C
C *** BIT 11: 0 means entry list is complete (i.e. not overflowed),
C ***         1 means entry list is overflowed
C
	   IF(LIST_OVERFLOW .EQ. 1) CALL SBIT1(PACKEDWORD,11)
C
	   FLAG_WORD = PACKEDWORD
C
C *** Convert L1 tower coordinates to detector coordinates
C
	   CALL PHYS_COR(PHI_OF_TOWER(NUMENT),ETA_OF_TOWER(NUMENT),
     &                   CAL_PHI,CAL_ETA)
C *** Convert energy counts to GeV
	   EM_ET_1X2_GEV = EM_ET_1X2(NUMENT) * ESCALE
C *** Fill TR15 ESUM bank with this entry, both electron and photon,
C *** when OBJ_TYP = 1
           IF(OBJ_TYP .EQ. 1)THEN
	   CALL ESUMFL('TR15',ID_ELECTRON,EM_ET_1X2_GEV,CAL_ETA,
     &                 CAL_ETA,CAL_PHI,FLAG_WORD)
	   CALL ESUMFL('TR15',ID_PHOTON,EM_ET_1X2_GEV,CAL_ETA,
     &                 CAL_ETA,CAL_PHI,FLAG_WORD)
           ENDIF
C *** Fill TR15 ESUM bank with this entry as jet when OBJ_TYP = 2
           IF(OBJ_TYP .EQ. 2)THEN
           CALL ESUMFL('TR15',ID_JET,EM_ET_1X2_GEV,CAL_ETA,
     &                 CAL_ETA,CAL_PHI,FLAG_WORD)
           ENDIF
C
	ENDDO  !End of loop over found objects in given local DSP
1001	CONTINUE
	ENDDO  !End of loop over local DSP's
C
C
      CALL ESUM_PUSH  !compact the banks
999   RETURN
      END
