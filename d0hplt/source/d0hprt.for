C=======================================================================
      SUBROUTINE D0HPRT(IDNUM)
C=======================================================================
C
C  Description:  Prints histogram IDNUM or if IDNUM=0 prints all hists.
C  ============
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - August 27,1988
C  Updated  26-Feb-1992  Herbert Greenlee
C    Replaced OPEN statement with call to D0OPEN
C
C========================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INTEGER IDNUM
      INTEGER IUSER,IUNIT,IERR
      CHARACTER*6 CNUM
      CHARACTER*40 OSTRG
      CHARACTER*12 FILNAM
      CHARACTER*10 ICX
      LOGICAL LEXIST,HEXIST,OK
      DATA IUSER/8/
      DATA FILNAM/'HISPRT.DAT'/
C
C  Executable Code:
C  ================
C 
      LEXIST = .TRUE.
      IF (IDNUM .NE. 0) LEXIST = HEXIST(IDNUM)
      IF (LEXIST) THEN
         CALL GTUNIT(IUSER,IUNIT,IERR)
         CALL D0OPEN(IUNIT,FILNAM,'OF',OK)
         IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed in D0HPRT')
         CALL HOUTPU(IUNIT)
         CALL HPRINT(IDNUM)
         CLOSE(IUNIT)
         CALL QPRINT(FILNAM,'SYS$PRINT','DEFAULT',.TRUE.)
         IF (IDNUM .EQ. 0 ) THEN
            CALL INTMSG(' PRINTED ALL HISTOGRAMS ON QUEUE SYS$PRINT')
         ELSE
            WRITE(ICX,100) IDNUM
  100       FORMAT(I6)
            READ(ICX,101) CNUM
  101       FORMAT(A6)
            OSTRG = ' PRINTED HISTOGRAM # '//CNUM
            CALL INTMSG(OSTRG)
         ENDIF
      ELSE
         CALL INTMSG(' HISTOGRAM DOES NOT EXIST')
      ENDIF
      RETURN
      END
