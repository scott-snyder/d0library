C====================================================================
      SUBROUTINE ISZTRK(PX,PY,PZ,PSCAL)
C====================================================================
C
C  Description:  Plots the tracks
C  ============
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - May 13,1987
C
C===================================================================
C
      IMPLICIT NONE
C
C  Argument Declarations:
C  ======================
C
      REAL PX,PY,PZ,PSCAL
C
C  Local Declarations:
C  ===================
C
      REAL PSX,PSY,PSZ
C
C  Executable Code:
C  ================
C
      CALL J3MOVE(0.,0.,0.)
      PSX = PX*PSCAL
      PSY = PY*PSCAL
      PSZ = PZ*PSCAL
      CALL J3DRAW(PSX,PSY,PSZ)
      RETURN
      END
     
