C=======================================================================
      SUBROUTINE D0HTYP(IDNUM)
C=======================================================================
C
C  Description:  Types histogram IDNUM or if IDNUM=0 types all hists.
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
      INTEGER IUNIT
      INTEGER I, LIBREP
      LOGICAL LEXIST,HEXIST
      DATA IUNIT/6/
C
C  Executable Code:
C  ================
C 
      LEXIST = .TRUE.
      IF (IDNUM .NE. 0) LEXIST = HEXIST(IDNUM)
      IF (LEXIST) THEN
         CALL ENDSPL
         I=LIBREP()
         CALL OUTSAV
         CALL SETFLG
         CALL HOUTPU(IUNIT)
         CALL HPRINT(IDNUM)
         CALL REASAV
      ELSE
         CALL INTMSG(' HISTOGRAM DOES NOT EXIST')
      ENDIF
      RETURN
      END
