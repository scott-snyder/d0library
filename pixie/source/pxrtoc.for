      SUBROUTINE PXRTOC(REXP,CEXP)
C====================================================================
C
C  Description:  Converts a real expression into a character
C  ============  expression.
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - September 14,1986
C
C=====================================================================
C
C  
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
C
      REAL REXP
      CHARACTER*(*)CEXP
      CHARACTER*13 CX
C
C  Executable Code:
C  ================
C
      IF (REXP .EQ. 0.) THEN
         CEXP = '0.'
         GO TO 999
      ENDIF
      WRITE(CX,110) REXP
  110 FORMAT(E10.3)
      READ(CX,111) CEXP
  111 FORMAT(A13)
  999 CONTINUE
C
      RETURN
      END
