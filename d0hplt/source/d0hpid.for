      SUBROUTINE D0HPID(IDNUM)
C========================================================================
C
C  Description:  Plots histogram IDNUM and it's ID number and number of
C  ============  entries.
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - August 13, 1988
C
C==========================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INTEGER IDNUM
      LOGICAL LEXIST,HEXIST
C
C  Executable Code:
C  ================
C
      LEXIST = HEXIST(IDNUM)
      IF (LEXIST) THEN
        CALL HPLOT(IDNUM,' ','HIST',0)
      ELSE
        CALL INTMSG(' HISTOGRAM DOES NOT EXIST')
      ENDIF
      RETURN
      END
