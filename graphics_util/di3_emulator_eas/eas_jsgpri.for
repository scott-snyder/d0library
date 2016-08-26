      SUBROUTINE JSGPRI(NAME, SGPRI)
C
C    Purpose:
CD   This module sets the segment priority. The segment name is passed 
CD   as NAME, and the new priority is SGPRI.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 08-Nov-1988
CH   History:
CH      08-NOV-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-W
C
C    Calls:
CC      ERROR.
C
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      INTEGER NAME, SGPRI
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I, LOCATE, TEMP
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         CALL ERROR('JSGPRI: A SEGMENT IS OPEN')
      ENDIF
      IF (SGPRI .LT. 0 .OR. SGPRI .GT. 32767) THEN
         CALL ERROR('JSGPRI: SEGPRI NOT IN RANGE (0..32767)')
      ENDIF
      LOCATE = 0
      DO 10 I=1,NSEGS
         IF (NAME .EQ. SEGINF(1,I)) LOCATE = I
   10 CONTINUE
      IF (LOCATE .EQ. 0) THEN
         CALL ERROR('JSGPRI: NAME DOES NOT EXIST')
      ENDIF
      TEMP = iand(SEGINF(3,LOCATE), 65535)
      SEGINF(3,LOCATE) = TEMP + SGPRI + 65536
      RETURN
      END
