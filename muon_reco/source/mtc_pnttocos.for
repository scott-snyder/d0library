      SUBROUTINE MTC_PNTTOCOS(PNT1,PNT2, dir)
C----------------------------------------------------------------------
C- MTC_PNTTOCOS: part of the MTC package
C-
C-   Purpose and Methods : Get the direction cosines for a line
C-      directed from pnt1(3) through pnt2(3).
C-
C-   Inputs  : pnt1(3)
C-             pnt2(3)
C-   Outputs : dir - direction cosines of the line from PNT1 to PNT2
C-
C-   Created  20-FEB-1994   Elizabeth Gallas
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input
      REAL pnt1(3), pnt2(3)
C- output
      REAL dir(3)
C- local
      REAL dx,dy,dz, d
C----------------------------------------------------------------------
      IF(pnt1(1).EQ.pnt2(1) .AND.
     &   pnt1(2).EQ.pnt2(2) .AND.
     &   pnt1(3).EQ.pnt2(3) ) go to 666

      dx = pnt2(1) - pnt1(1)
      dy = pnt2(2) - pnt1(2)
      dz = pnt2(3) - pnt1(3)
      d = sqrt(dx**2 + dy**2 + dz**2)

      dir(1) = dx / d
      dir(2) = dy / d
      dir(3) = dz / d

      go to 999
C----------------------------------------------------------------------
  666 CONTINUE
      dir(1) = -50.
      dir(2) = -50.
      dir(3) = -50.
C----------------------------------------------------------------------
  999 RETURN
      END
