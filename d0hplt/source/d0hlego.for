      SUBROUTINE D0HLEGO(IDNUM)
C========================================================================
C
C  Description:  Makes LEGO plot of 2-D hist number IDNUM 
C  ============  
C
C  Author:
C  ========
C  Sharon Hagopian
C
C  Revision History:
C  =================
C  Original Creation - May 7, 1992
C
C==========================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INTEGER IDNUM
      REAL THETA,PHI
      LOGICAL LEXIST,HEXIST
      DATA THETA,PHI/30,30/
C
C  Executable Code:
C  ================
C
      LEXIST = HEXIST(IDNUM)
      IF (LEXIST) THEN
        CALL HPLEGO(IDNUM,THETA,PHI)
      ELSE
        CALL INTMSG(' HISTOGRAM DOES NOT EXIST')
      ENDIF
      RETURN
      END
