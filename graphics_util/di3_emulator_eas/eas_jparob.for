      SUBROUTINE JPAROB(DU, DV, DN)
C
C    Purpose:
CD   This module sets the projection type to Parallel Oblique.  The 
CD   parameters passed are all real displacements from the view 
CD   reference point.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 16-Jan-1989
CH   History:
CH      16JAN-89  ATV  Initial entry.
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
      REAL DU, DV, DN
C
C    Then local declarations of variables (non-common variables).
C
      REAL DELTA, EPS
      DATA EPS /1E-9/
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
         CALL ERROR('JPAROB: A SEGMENT IS OPEN.')
      ENDIF
      DELTA = SQRT(DU * DU + DV * DV + DN * DN)
      IF (DELTA .LT. EPS) THEN
         CALL ERROR('JPAROB: THE PARAMETERS DO NOT DEFINE A VECTOR.')
      ENDIF
      DELTA = SQRT(DU * DU + DV * DV)
      IF (DELTA .LT. EPS) THEN
         PRJTYP = PTPARA
      ELSE
         PRJTYP = PTPARO
         PAROBX = DU
         PAROBY = DV
         PAROBZ = DN
      ENDIF
      RETURN
      END
