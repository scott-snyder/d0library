C DEC/CMS REPLACEMENT HISTORY, Element JMOVE.FOR
C *2    17-MAY-1989 17:36:45 ABACHI " "
C *1     6-DEC-1988 13:42:06 ABACHI "EAS EMULATOR ROUTINES"
C DEC/CMS REPLACEMENT HISTORY, Element JMOVE.FOR
        SUBROUTINE JRMOVE(XPT, YPT)
C
C    Purpose:
CD   The purpose of this routine is four-fold, in that there are four
CD   entry points.  The main entry is JRMOVE and the predefined sub-entry
CD   points are in order JMOVE, JR3MOV, and J3MOVE. They execute the
CD   same code to queue a vector into a list with move status. The two-
CD   dimensional parts assume that the z-dimension is CPZ (co-planar).
CD   The arguments are as follows:
CD      XPT --> X offset/position in world coordinates.
CD      YPT --> Y offset/position in world coordinates.
CD      ZPT --> Z offset/position in world coordinates.
C
C    A. Virgo, R. Heckelsberg
C    Fermi National Accelerator Laboratory
C    RD/Computing section -- Graphics Support Group
C    Batavia, IL  60510
C
CM   Module Date: 08-Nov-1988
CH   History:
CH      08-NOV-88  ATV  Add right handed stuff.
CH      18-JUN-88  ATV  Initial entry.
C
      IMPLICIT NONE
C
C    Common blocks:
CB      GRFPAR-R, SEGINF-R, PRIMVR-W
C
C    Calls:
CC      ERROR, KQUEV
      EXTERNAL ERRHND
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      REAL XPT, YPT, ZPT
C
C    Then local declarations of variables (non-common variables).
C
      REAL X, Y, Z
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:SEGINF.INC/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
C
C    Then executable code follows
C
C    Relative move from Current Position.
C
      CPX = CPX + XPT
      CPY = CPY + YPT
      GOTO 1
C
C    Absolute move.
C
      ENTRY JMOVE(XPT, YPT)
      CPX = XPT
      CPY = YPT
      GOTO 1
C
C    Relative 3-D move for Current Position.
C
      ENTRY JR3MOV(XPT, YPT, ZPT)
      CPX = CPX + XPT
      CPY = CPY + YPT
      CPZ = CPZ + (ZPT * RIGHT)
      GOTO 1
C
C    Absolute 3-D move.
C
      ENTRY J3MOVE(XPT, YPT, ZPT)
      CPX = XPT
      CPY = YPT
      CPZ = ZPT * RIGHT
C
C    Perform the move.
C
    1 CONTINUE
      CALL KQUEV(CPX, CPY, CPZ, 'MOVE')
      RETURN
      END
