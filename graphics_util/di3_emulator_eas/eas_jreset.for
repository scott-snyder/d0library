      SUBROUTINE JRESET
C
C    Purpose:
CD   This module resets the viewing transformation back to the initial 
CD   values established in JBEGIN.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 16-Jan-1989
CH   History:
CH      10-MAR-89  SA   UVIEW(5) and UVIEW(6) correctly set. LINATT added.
CH      10-MAR-89  SA   Minimum intesity UVIEW(5) was changed from 0.5 to 0.1.
CH      16-JAN-89  ATV  Added initialization of perspective.
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
C
C    Then local declarations of variables (non-common variables).
C
      INTEGER I
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
C
C    Then executable code follows
C
      DO 10 I=1,6
         IF (MOD(I,2) .EQ. 0) THEN
            UWIND(I) = 1.0
            UVIEW(I) = 1.0
         ELSE
            UWIND(I) = -1.0
            UVIEW(I) = -1.0
         ENDIF
   10 CONTINUE
      UVIEW(5) = MINTEN / 32767.0
      UVIEW(6) = DINTEN / 32767.0
      DO 20 I=1,3
         VUPNT(I) = 0.0
         NORML(I) = 0.0
         UPVEC(I) = 0.0
   20 CONTINUE
      NORML(3) = 1.0
      UPVEC(2) = 1.0
      WCLIP = .FALSE.
      HCLIP = .FALSE.
      YCLIP = .FALSE.
      RIGHT = 1.0
      PRJTYP = PTPARA
      RETURN
      END
