C VAX/DEC CMS REPLACEMENT HISTORY, Element L15EXTRACT_L15CAL_FRAME.FOR
C *2    14-JUL-1994 17:04:39 NIKOS "Fix typo bug"
C *1    14-JUL-1994 16:24:25 NIKOS "Extract FRAME Code section from calorimeter L1.5 section of TRGR bank"
C VAX/DEC CMS REPLACEMENT HISTORY, Element L15EXTRACT_L15CAL_FRAME.FOR
	SUBROUTINE L15EXTRACT_L15CAL_FRAME(NBANK,L15_FRAME_PASS)
C----------------------------------------------------------------------
C-
C-	Purpose and Methods: Extract FRAME Code section 
C-	from calorimeter L1.5 section of TRGR bank
C-
C-	Inputs: NBANK to chose which bank to extract
C-              1 = most recent bank, 2 = older bank
C-
C-	Outputs: L15_FRAME_PASS     copy of FRAME section
C-
C-	Controls:
C-
C-	Created 2-may-1994	Gregory Snow
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
	INTEGER HEADER_LWFOLLOW, FRAME_LWFOLLOW
C-
	INTEGER I			! do loop indices
	INTEGER L15_FRAME_PASS(L15CAL_FRAME_LENGTH)   ! output array
C-
C- Initialize outputs first
C-
	CALL VZERO(L15_FRAME_PASS,L15CAL_FRAME_LENGTH)
C-
	LTRGR = GZTRGR()
	IF(NBANK .EQ. 2)LTRGR = LQ(LTRGR)
	L15START = GZFIND_CRATE('TRGR',LTRGR,L15CAL_CRATE_ID)
C-	
	IF(L15START. LE. 0) THEN
          CALL ERRMSG('GZFIND_CRATE CANT FIND CRATE 81',
     &    'L15EXTRACT_L15CAL_FRAME',
     &    'GZFIND_CRATE CANT FIND CRATE 81','W')
	  RETURN
	ENDIF
C-
C-  read through TRGR to find start of FRAME section
C-  
        HEADER_LWFOLLOW = IQ(L15START)
	L15CAL_FRAME_START = L15START + HEADER_LWFOLLOW + 1
	FRAME_LWFOLLOW = IQ(L15CAL_FRAME_START)
        IF(FRAME_LWFOLLOW+1 .NE. L15CAL_FRAME_LENGTH) THEN
          CALL ERRMSG('UNEXPECTED L15CAL FRAME LENGTH',
     &    'L15EXTRACT_L15CAL_FRAME',
     &    'UNEXPECTED L15CAL FRAME LENGTH','W')
        ENDIF
C-
C-  fill in L15_FRAME_PASS for output
C-

        PNTR=L15CAL_FRAME_START
	IF(FRAME_LWFOLLOW.GE.0 .AND. 
     &	   FRAME_LWFOLLOW+1.LE.L15CAL_FRAME_LENGTH)THEN
	DO I=1,FRAME_LWFOLLOW+1
           L15_FRAME_PASS(I)=IQ(PNTR)
           PNTR=PNTR+1
        ENDDO
	ENDIF
C-
999	RETURN
	END





