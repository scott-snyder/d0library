      SUBROUTINE JPARAL
C
C    Purpose:
CD   This module sets the projection type to be Parallel orthographic. 
CD   There are no parameters.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 16-JAN-1989
CH   History:
CH      16-JAN-89  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-W, SEGINF-R
C
C    Calls:
CC      ERROR.
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
C
C    Then local declarations of variables (non-common variables).
C
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         CALL ERROR('JPARAL: A SEGMENT IS OPEN')
      ENDIF
      PRJTYP = PTPARA
      RETURN
      END
