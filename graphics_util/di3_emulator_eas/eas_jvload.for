      SUBROUTINE JVLOAD(ARRAY)
C
C    Purpose:
CD   This module loads in the viewing transformation information stored 
CD   in area ARRAY. No checking is done to make sure data is valid.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 16-Jan-1989
CH   History:
CH      16-JAN-89  ATV  Added perspective stuff.
CH      09-NOV-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-W
C
C    Calls:
CC      NONE.
C
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      REAL ARRAY(85)
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I, NSEL
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
C
C    Then executable code follows
C
C
      NSEL = 1
      DO 10 I=1,6
         UWIND(I) = ARRAY(NSEL)
         UVIEW(I) = ARRAY(NSEL+6)
         NSEL = NSEL + 1
   10 CONTINUE
      NSEL = NSEL + 6
      DO 20 I=1,3
         VUPNT(I) = ARRAY(NSEL)
         NORML(I) = ARRAY(NSEL+3)
         UPVEC(I) = ARRAY(NSEL+6)
         NSEL = NSEL + 1
   20 CONTINUE
      NSEL = NSEL + 6
      IF (ARRAY(NSEL) .EQ. 1.0) THEN
         WCLIP = .TRUE.
      ELSE
         WCLIP = .TRUE.
      ENDIF
      NSEL = NSEL + 1
      IF (ARRAY(NSEL) .EQ. 1.0) THEN
         HCLIP = .TRUE.
      ELSE
         HCLIP = .FALSE.
      ENDIF
      NSEL = NSEL + 1
      IF (ARRAY(NSEL) .EQ. 1.0) THEN
         YCLIP = .TRUE.
      ELSE
         YCLIP = .FALSE.
      ENDIF
      NSEL = NSEL + 1
      RIGHT = ARRAY(NSEL)
      NSEL = NSEL + 1
      PRJTYP = INT(ARRAY(NSEL))
      NSEL = NSEL + 1
      PAROBX = ARRAY(NSEL)
      NSEL = NSEL + 1
      PAROBY = ARRAY(NSEL)
      NSEL = NSEL + 1
      PAROBZ = ARRAY(NSEL)
      NSEL = NSEL + 1
      PEROBX = ARRAY(NSEL)
      NSEL = NSEL + 1
      PAROBY = ARRAY(NSEL)
      NSEL = NSEL + 1
      PAROBZ = ARRAY(NSEL)
      NSEL = NSEL + 1
      PERSP = ARRAY(NSEL)
      RETURN
      END
