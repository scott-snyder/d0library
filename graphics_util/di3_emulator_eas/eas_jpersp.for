      SUBROUTINE JPERSP(DN)
C
C    Purpose:
CD   This module sets the projection type to Perspective Orthographic. The 
CD   parameter passed is real displacement from the view reference
CD   point along the N-axix.
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
      REAL DN
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
         CALL ERROR('JPERSP: A SEGMENT IS OPEN.')
      ENDIF
      PRJTYP = PTPERS
      PERSP  = DN
      RETURN
      END
