      SUBROUTINE PXCTOI(CEXP,NCHAR,IEXP)
C====================================================================
C
C  Description:  Converts a character expression into an integer
C  ============  expression.
C
C  Argument Declarations:
C  ======================
C  CEXP - input - character string to be converted
C  NCHAR - input - number of characters in string
C  IEXP - output - integer returned
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - December 29,1986
C-   Updated  23-MAR-2004   compile with g77.
C
C=====================================================================
C
C  
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
C
      INTEGER IEXP,NCHAR
      CHARACTER*(*)CEXP
      CHARACTER*10 ICX
C
C  Executable Code:
C  ================
C
      IF (CEXP .EQ. '0') THEN
         IEXP = 0
         GO TO 999
       ENDIF
C&IF LINUX
C&      READ(CEXP,*) IEXP
C&ELSE
      READ(CEXP,108) IEXP
  108 FORMAT(I<NCHAR>)
C&ENDIF
  999 CONTINUE
C
      RETURN
      END
