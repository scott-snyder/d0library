      SUBROUTINE PXFTOC(REXP,CEXP)
C====================================================================
C
C  Description:  Converts an floating point (F FORMAT)number 
C  ============  into a character expression.
C                (Maximum size F8.1)
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - August 4,1986
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
      CHARACTER*(8) RCX
C
C  Executable Code:
C  ================
C
      WRITE(RCX,108) REXP
  108 FORMAT(F8.1)
      READ(RCX,109) CEXP
  109 FORMAT(A8)
C
      RETURN
      END
C===================================================================
