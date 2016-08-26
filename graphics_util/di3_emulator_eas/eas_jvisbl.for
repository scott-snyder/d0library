      SUBROUTINE JVISBL(NAME, VISFLG)
C
C    Purpose:
CD   This module changes the visibility of the named segment. The
CD   parameters passed are the segment name (NAME) and the new 
CD   visibility (VISFLG).  The valid values for visibility are 0 or 1.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 13-Oct-1988
CH   History:
CH      13-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-W, GRFPAR-R
C
C    Calls:
CC      ERROR, KBLDN, PREMFR, PINCL
C
      EXTERNAL ERRHND
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER NAME, VISFLG
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I, LOCAT, CVIS
      CHARACTER*4 TSEG
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (VISFLG .LT. 0 .OR. VISFLG .GT. 1) THEN
         CALL ERROR('JVISBL: VISFLG OUT OF RANGE (0..1)')
      ENDIF
      IF (SEGOPN) THEN
         CALL ERROR('JVISBL: A SEGMENT IS OPEN')
      ENDIF
      LOCAT = 0
      DO 10 I=1,NSEGS
         IF (SEGINF(1,I) .EQ. NAME) LOCAT = I
   10 CONTINUE
      IF (LOCAT .EQ. 0) THEN
         CALL ERROR('JVISBL: SEGMENT DOES NOT EXIST')
      ENDIF
      CVIS = iand((SEGINF(2,LOCAT) / 8), 1)
      IF (CVIS .NE. VISFLG) THEN
         CVIS = VISFLG * 8
         SEGINF(2,LOCAT) = iand((NOT(8)), SEGINF(2,LOCAT)) + CVIS
         CALL KBLDN(SEGINF(6,LOCAT), TSEG)
         TSEG = 'D'//TSEG
         IF (VISFLG .EQ. 1) THEN
            CALL PINCL(TSEG, EMDISP, ERRHND)
         ELSE
            CALL PREMFR(TSEG, EMDISP, ERRHND)
         ENDIF
      ENDIF
      RETURN
      END
