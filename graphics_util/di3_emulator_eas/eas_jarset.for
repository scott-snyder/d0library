      SUBROUTINE JARSET
C
C    Purpose:
CD   This module resets the primitive attributes to their default values.
C
C    A. Virgo
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 05-Oct-1988
CH   History:
CH      05-OCT-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      SEGINF-W, LINATT-W
C
C    Calls:
CC      ERROR
C
C    Next is the declaration of parameters passed to the subroutine/function. 
C
C    Then local declarations of variables (non-common variables).
C
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
C
C    Then executable code follows
C
      IF (SEGOPN) THEN
C
C   Initialize current pick id to the Default
C
         CPIKID = DPIKID
C
C   Initialize drawing primitive attributes.
C
         CURCOL = DEFCOL
         CINTEN = DINTEN
         CLSTYL = DLSTYL
         CMARKR = DMARKR
         UVIEW(5) = MINTEN / 32767.0
         UVIEW(6) = CINTEN / 32767.0
      ELSE
         CALL ERROR('JARSET: NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END
