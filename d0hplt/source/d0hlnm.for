C========================================================================
	INTEGER*4 FUNCTION D0HLNM(ITABLE,NAMIN,NAMOUT)
C========================================================================
C	
C   Description: Integer function which translates a logical name from
C   ============ the LNM$PROCESS_TABLE or LNM$SYSTEM_TABLE.
C                 This is a Vax specific routine!!!
C
C   Author:
C   ========
C   Tami Kramer
C
C   Argument Declarations:
C   =======================
C   Input: ITABLE - 1 for PROCESS_TABLE  2 for SYSTEM_TABLE
C   Input: NAMIN - the logical name to be translated
C   Output: NAMOUT - the translated name
C
C   Revision History:
C   =================
C   Original Creation - August 24, 1988
C   Updated 26-Feb-1992  Herbert Greenlee
C     Added machine blocks to turn this routine into a dummy on UNIX.
C     Always return value 0 (failure) and NAMOUT = ' '
C
C=============================================================================
C
      IMPLICIT NONE
C&IF VAXVMS
      INCLUDE '($LNMDEF)/NOLIST'
C&ELSE
C&ENDIF
C
C  Local Declarations:
C  ====================
C
      CHARACTER*(*) NAMIN,NAMOUT
      INTEGER ITABLE
C&IF VAXVMS
      STRUCTURE /ITMLST/
	    INTEGER*2 BUFFER_LENGTH
	    INTEGER*2 ITEM_CODE
	    INTEGER*4 BUFFER_ADDRESS
	    INTEGER*4 BUFFER_RETURN_LENGTH
	    INTEGER*4 END_LIST /0/
      END STRUCTURE    !ITMLST
      INTEGER*4 STATUS
      RECORD /ITMLST/ TRNLNM_ITMLST
      INTEGER*4 SYS$TRNLNM
C
C  Executable Code:
C  ==================	 
C
      IF (ITABLE .NE. 1 .AND. ITABLE .NE. 2) THEN
         CALL INTMSG(' ERROR IN D0HLNM - SPECIFY TABLE')
         RETURN
      ENDIF
      NAMOUT = ' '
      TRNLNM_ITMLST.ITEM_CODE = LNM$_STRING
      TRNLNM_ITMLST.BUFFER_ADDRESS = %LOC(NAMOUT)
      TRNLNM_ITMLST.BUFFER_LENGTH = LEN(NAMOUT)
      IF (ITABLE .EQ. 1) THEN
         D0HLNM = SYS$TRNLNM(LNM$M_CASE_BLIND,'LNM$PROCESS_TABLE',
     &    NAMIN,,TRNLNM_ITMLST)
      ELSEIF (ITABLE .EQ. 2) THEN
         D0HLNM = SYS$TRNLNM(LNM$M_CASE_BLIND,'LNM$SYSTEM_TABLE',
     &    NAMIN,,TRNLNM_ITMLST)
      ENDIF
C&ELSE
C&      NAMOUT = ' '
C&      D0HLNM = 0
C&ENDIF
      RETURN
      END
