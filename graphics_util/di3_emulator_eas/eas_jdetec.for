      SUBROUTINE JDETEC(NAME, DETVAL)
C
C    Purpose:
CD   This module sets the detectability of a retained segment specified
CD   by NAME and assigns a detectability of DETVAL.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 14-Nov-1988
CH   History:
CH      14-NOV-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-W
C
C    Calls:
CC      PSNBOO, ERROR.
C
      EXTERNAL ERRHND
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER NAME, DETVAL
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I, LOCATE, TEMP
      LOGICAL DETEC
      CHARACTER*3  SSTR
      CHARACTER*4  DET, TEMPS
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (DETVAL .LT. 0 .OR. DETVAL .GT. 32767) THEN
         CALL ERROR('JDETEC: DETVAL IS NOT IN RANGE (0..32767)')
      ENDIF
      LOCATE = 0
      DO 10 I=1,NSEGS
         IF (NAME .EQ. SEGINF(1,I)) LOCATE = I
   10 CONTINUE
      IF (LOCATE .EQ. 0) THEN
         CALL ERROR('JDETEC: NAME DOES NOT EXIST')
      ENDIF
      CALL KBLDN(SEGINF(6,LOCATE), SSTR)
      DET   = 'D' // SSTR
      DETEC = (DETVAL .GT. 0)
      CALL PSNBOO(DETEC, 1, DET, ERRHND)
      TEMP = SEGINF(3,LOCATE) / 65536
      SEGINF(3, LOCATE) = TEMP * 65536 + DETVAL
      RETURN
      END
