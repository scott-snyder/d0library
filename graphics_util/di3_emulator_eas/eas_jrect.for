        SUBROUTINE JRECT(X0, Y0, X1, Y1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
CD   The purpose of this module is to produce a rectangle defined by the
CD   two vertex coordinates specified. The parameters are:
CD      X0, Y0 -->  the first vertex definition in world coordinates.
CD      X1, Y1 -->  the second vertex definition in world coordinates.
C-
C-   Inputs  : X0, Y0, X1, Y1
C-   Outputs : 
C-   Controls: 
C-
C-   Created  09-JAN-1989   A. VIRGO
C-   UPDATED  19-JUN-1990   S. ABACHI     Real polygons were used.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL X0, Y0, X1, Y1
      REAL INTENS
      INTEGER NVERP(1000)
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      INCLUDE 'D0$INC:LINATT.INC/LIST'
      INCLUDE 'D0$INC:PLGATT.INC/LIST'
C
      IF (SEGOPN) THEN
         INTENS = FLOAT(CINTEN) / 32767.0
         VERTIC(1,1) = X0
         VERTIC(2,1) = Y0
         VERTIC(3,1) = CPZ
         VERTIC(4,1) = INTENS
         VERTIC(1,2) = X0
         VERTIC(2,2) = Y1
         VERTIC(3,2) = CPZ
         VERTIC(4,2) = INTENS
         VERTIC(1,3) = X1
         VERTIC(2,3) = Y1
         VERTIC(3,3) = CPZ
         VERTIC(4,3) = INTENS
         VERTIC(1,4) = X1
         VERTIC(2,4) = Y0
         VERTIC(3,4) = CPZ
         VERTIC(4,4) = INTENS
         VERTIC(1,5) = X0
         VERTIC(2,5) = Y0
         VERTIC(3,5) = CPZ
         VERTIC(4,5) = INTENS
CC         NVERT       = 5
         NVERT       = 4
         NVERP(1) = NVERT
         CALL KPLGN(1,NVERP)
         CPX = X0
         CPY = Y0
      ELSE
         CALL ERROR('JRECT: NO SEGMENT IS OPEN')
      ENDIF
      RETURN
      END
