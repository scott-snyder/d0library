C=======================================================================
      SUBROUTINE D0HCLR(IDNUM)
C=======================================================================
C
C  Description:  Resets one or all histograms...
C  ============
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - August 27,1988
C
C========================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INTEGER IDNUM
      CHARACTER*6 CNUM
      CHARACTER*40 OSTRG
      CHARACTER*10 ICX
      LOGICAL LEXIST,HEXIST
      LOGICAL FLGVAL
      EXTERNAL FLGVAL
C
C  Executable Code:
C  ================
C
      LEXIST = .TRUE.
      IF (IDNUM .NE. 0) LEXIST = HEXIST(IDNUM)
      IF (LEXIST) THEN
        CALL HRESET(IDNUM,'  ')
        IF (IDNUM .EQ. 0) THEN
          CALL INTMSG(' CLEARED ALL HISTOGRAMS')
        ELSE
          WRITE(ICX,100) IDNUM
  100     FORMAT(I6)
          READ(ICX,101) CNUM
  101     FORMAT(A6)
          OSTRG = ' CLEARED HISTOGRAM # '//CNUM
          CALL INTMSG(OSTRG)
        ENDIF
      ELSE
        CALL INTMSG(' HISTOGRAM DOES NOT EXIST')
      ENDIF
      RETURN
      END
