      SUBROUTINE JEND
C
C    Purpose:
CD   Subroutine JEND sets internal variables to a null state.
CD   There are NO parameters to this subroutine.
C
C    A. Virgo, R. Heckelsberg
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 18-May-1988
CH   History:
CH        14-JUN-88  ATV  Added the include statement for SEGINF
CH        18-May-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB        GENRL-W, GRFPAR-W, DEVSTS-W, SEGINF-W
C
C    Calls:
CC        NONE.
C
C
C    Then local declarations of variables (non-common variables).
C

C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GENRL.INC/LIST'
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:DEVSTS.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         CALL ERROR ('A SEGMENT IS OPEN')
      ENDIF
C
C    DE-INITIALIZE DEVSTS COMMON BLOCK
C
      IF (DEVON) CALL JDEVOF(DSPDEV)
C
C    DE-INITIALIZE GRFPAR COMMON BLOCK
C
      IF (DINIT) CALL JDEND(DSPDEV)
C
C    DE-INITIALIZE GENRL COMMON BLOCK
C
      BEGIN = .FALSE.
      EMVERS = 0.1
      RETURN
      END
