      SUBROUTINE JVSALL(VISFLG)
C
C    Purpose:
CD   This module changes the visibility of the all segments. The
CD   parameter passed is the new visibility (VISFLG).  The valid values
CD   for visibility are 0 or 1. 
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 26-Oct-1988
CH   History:
CH      26-OCT-88  ATV  Initial entry.
C
C    Common blocks:
CB      SEGINF-W, GRFPAR-R
C
C    Calls:
CC      ERROR, KBLDN, PREMFR, PINCL
C
      IMPLICIT NONE
      EXTERNAL ERRHND
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER VISFLG
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I, LOCAT, CVIS, NAME
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
         CALL ERROR('JVSALL: VISFLG OUT OF RANGE (0..1)')
      ENDIF
      IF (SEGOPN) THEN
         CALL ERROR('JVSALL: A SEGMENT IS OPEN')
      ENDIF
      DO 10 I=1,NSEGS
         LOCAT = I
         CVIS = (SEGINF(2,LOCAT) / 8) .AND. 1
         IF (CVIS .NE. VISFLG) THEN
            CVIS = VISFLG * 8
            SEGINF(2,LOCAT) = ((.NOT. 8) .AND. SEGINF(2,LOCAT)) + CVIS
            NAME = SEGINF(6,LOCAT)
            CALL KBLDN(NAME, TSEG)
            TSEG = 'D'//TSEG
            IF (VISFLG .EQ. 1) THEN
               CALL PINCL(TSEG//'"', EMDISP, ERRHND)
            ELSE
               CALL PREMFR(TSEG//'"', EMDISP, ERRHND)
            ENDIF
         ENDIF
   10 CONTINUE
      RETURN
      END
