      SUBROUTINE JPIDEX(CCOL, CINT)
C
C    Purpose:
CD   This module sets the current polygon interior color and intensity.
CD   The parameters passed are both integers and designate the color and 
CD   intensity.  Valid values for both are (0..32767)
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 20-Oct-1988
CH   History:
CH      20-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-R, PLGATT-W
C
C    Calls:
CC      ERROR.
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER CCOL, CINT 
C
C    Then local declarations of variables (non-common variables).
C
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PLGATT.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         IF (CCOL .LT. 0 .OR. CCOL .GT. 32767) THEN
            CALL ERROR('JPIDEX: CCOLOR IS OUT OF RANGE (0..32767)')
         ENDIF
         IF (CINT .LT. 0 .OR. CINT .GT. 32767) THEN
            CALL ERROR('JPIDEX: CINTEN IS OUT OF RANGE (0..32767)')
         ENDIF
         CPIDCO = CCOL
         CPIDIN = CINT
      ELSE
         CALL ERROR('JPIDEX: NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END
