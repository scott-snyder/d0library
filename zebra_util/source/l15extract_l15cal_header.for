	SUBROUTINE L15EXTRACT_L15CAL_HEADER(NBANK,L15_HEADER_PASS,
     &       DATA_VALID,CYCLE_PERFORMED,FW_TERM_EARLY,MARK_PASS_MASK)
C----------------------------------------------------------------------
C-
C-	Purpose and Methods: Extract header from calorimeter L1.5
C-	section of TRGR bank
C-
C-	Inputs: NBANK to chose which version of TRGR bank to extract
C-	        1 = most recent bank, 2 = older bank
C-
C-	Outputs: L15_HEADER_PASS    copy of L15CAL_CRATE_HEADER
C-	         DATA_VALID         logical, true = data valid
C-	         CYCLE_PERFORMED    logical, true = L1.5 decision
C-	                            cycle performed
C-	         FW_TERM_EARLY	    logical, true = early termination
C-	         MARK_PASS_MASK     mark and pass mask	
C-	Controls:
C-
C-	Created 14-JAN-1994	Gregory Snow
C-      17 Oct. 94: Commented out error message writing
C-
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
C-
	INTEGER LTRGR,GZTRGR		! pointers to TRGR bank	
	INTEGER GZFIND_CRATE		! pointers to TRGR bank
	INTEGER L15START,PNTR		! pointers to TRGR bank
	INTEGER HEADER_LWFOLLOW         ! TRGR's header longwords to follow
C-
	INTEGER NBANK
	INTEGER I			! do loop indices
	LOGICAL DATA_VALID,CYCLE_PERFORMED,FW_TERM_EARLY		
	INTEGER MARK_PASS_MASK
	INTEGER L15_HEADER_PASS(L15CAL_HEADER_LENGTH)
C-
C-- Initialize outputs first
C-
	CALL VZERO(L15_HEADER_PASS,L15CAL_HEADER_LENGTH)
	MARK_PASS_MASK = 0
	DATA_VALID = .FALSE.
	CYCLE_PERFORMED = .TRUE.
	FW_TERM_EARLY = .TRUE.
C-
	LTRGR = GZTRGR()
	IF(NBANK .EQ. 2)LTRGR = LQ(LTRGR)
	L15START = GZFIND_CRATE('TRGR',LTRGR,L15CAL_CRATE_ID)
C-	
	IF(L15START. LE. 0) THEN
C-	  CALL ERRMSG('GZFIND_CRATE CANT FIND CRATE 81',
C-     &    'L15EXTRACT_L15CAL_HEADER',
C-     &    'GZFIND_CRATE CANT FIND CRATE 81','W')
	  RETURN
	ENDIF
C-
C-
C-  FIND NUMBER OF LONGWORDS IN HEADER
C-
	HEADER_LWFOLLOW = IQ(L15START)
	IF(HEADER_LWFOLLOW+1 .NE. L15CAL_HEADER_LENGTH) THEN
C-	  CALL ERRMSG('UNEXPECTED L15CAL HEADER LENGTH',
C-     &    'L15EXTRACT_L15CAL_HEADER',
C-     &    'UNEXPECTED L15CAL HEADER LENGTH','W')
	  RETURN
	ENDIF
C-
C-  FILL L15_HEADER_PASS ARRAY FOR OUTPUT
C-
	PNTR = L15START
	DO I = 1, HEADER_LWFOLLOW+1
	  L15_HEADER_PASS(I) = IQ(PNTR)
	  PNTR = PNTR + 1
	ENDDO
C-
C-  UNPACK SOME OF THE HEADER ENTRIES FOR OUTPUT
C-
	PACKEDWORD = L15_HEADER_PASS(6)
	IF(BYTES(BYTE1).EQ.0)DATA_VALID = .TRUE.
	IF(BYTES(BYTE2).NE.0)CYCLE_PERFORMED = .FALSE.
	IF(BYTES(BYTE3).NE.0)FW_TERM_EARLY = .FALSE.
C-
	PACKEDWORD = L15_HEADER_PASS(7)
	MARK_PASS_MASK = BYTES(BYTE1)
C-	
C-
999	RETURN
	END

