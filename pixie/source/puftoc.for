      SUBROUTINE PUFTOC(REXP, FMT, RCX, CEXP)
C====================================================================
C
C  Description:  Converts an floating point (F FORMAT)number 
C  ============  into a character expression.
C                
C  Input:
C  ======  REXP - floating point number 
C          FMT  - character variable with the format of the floating pnt num 
C                 'Fxx.xx'
C          RCX  - dummy variable necessary for convertion withethe same
C                 character size as the floating number.
C  Output:
C  =======
C          CEXP - character expression withe  the floating point number value.
C
C  Author:
C  =======
C  Tami Kramer   
C
C  Revision History:
C  ==================
C  Original Creation - August 4,1986
C  Modifyed: May 8, 1989 Lupe Rosas
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
      CHARACTER*(*) RCX
      CHARACTER*(*) FMT   ! Format 
      CHARACTER*(*)CEXP
      CHARACTER*8 FORCHR, RPAR*1
C-----------------------------------------------------------------------
      DATA FORCHR(1:1),RPAR /'(', ')'/
C
C  Executable Code:
C  ================
C
      FORCHR(2:7) = FMT
      FORCHR(8:8) = RPAR

      WRITE(RCX,FORCHR) REXP       
      READ(RCX,109) CEXP 
  109 FORMAT(A)
C
      RETURN
      END
