      SUBROUTINE JCONWV(WX, WY, WZ, VX, VY)
C----------------------------------------------------------------------
C-
C   Purpose and Methods : This module converts a world coordinate to a
C                         virtual device coordinate (VX, VY) value using the
C                         current viewing transformation.
C-
C-   Inputs  : WX, WY, WZ Virtual coordinate
C-   Outputs : VX, VY World coordinate
C-   Controls: None
C-
C-   Created    02-MAY-1989   SHAHRIAR ABACHI
C-   Modified   08-MAY-1989   Nobuaki Oshima   Generalized for all coordinates.
C-   Modified   09-OCT-1991   SHAHRIAR ABACHI  Effect of 3D rotations added
C-   Modified   11-OCT-1991   SHAHRIAR ABACHI  Effect of translations added
C-   Modified 04-SEP-1992   N. OSHIMA      remove 3D part, add VUPNT trans.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL WX, WY, WZ, VX, VY
      REAL NUX(3), NUY(3), NUZ(3), CC, VLENY, VLENZ, MXVW(3,3)
      REAL VV(3), VW(3), NORML2(3), VLENX
      REAL WX2, WY2, WZ2
      INTEGER I, J, IM, IC, IJK
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
      INCLUDE 'D0$PARAMS:ESCAPE_CODE_NAMES.DEF/LIST'
      INCLUDE 'D0$INC:PRIMVR.INC/LIST'
      REAL SCALX,SCALY,WR(3)
      INTEGER IFAIL
C----------------------------------------------------------------------
C
      SCALX = (UVIEW(2) - UVIEW(1)) / (UWIND(2) - UWIND(1))
      SCALY = (UVIEW(4) - UVIEW(3)) / (UWIND(4) - UWIND(3))
C
      DO I=1,3
        NORML2(I) = - NORML(I)
      ENDDO
C
      CALL CROSS(UPVEC, NORML2, NUX)
      CALL CROSS(NORML2, NUX, NUY)
C
      VLENX = 0.0
      VLENY = 0.0
      VLENZ = 0.0
      DO I=1,3
        VLENX = VLENX + NUX(I)**2
        VLENY = VLENY + NUY(I)**2
        VLENZ = VLENZ + NORML2(I)**2
      ENDDO
      VLENX = SQRT(VLENX)
      VLENY = SQRT(VLENY)
      VLENZ = SQRT(VLENZ)
      DO I=1,3
        NUX(I) = NUX(I) / VLENX
        NUY(I) = NUY(I) / VLENY
        NUZ(I) = NORML2(I) / VLENZ
      ENDDO
C
      DO J=1,3
        MXVW(J,1) = NUX(J)
        MXVW(J,2) = NUY(J)
        MXVW(J,3) = NUZ(J)
      ENDDO
C
C - Inverse matrix.
C
      CALL RINV(3,MXVW,3,WR,IFAIL)
C
      VV(1) = WX - VUPNT(1)
      VV(2) = WY - VUPNT(2)
      VV(3) = WZ - VUPNT(3) * RIGHT
      CALL VMATR(VV, MXVW, VW, 3, 3)
C
      VX = UVIEW(1) + (VW(1) - UWIND(1)) * SCALX
      VY = UVIEW(3) + (VW(2) - UWIND(3)) * SCALY
C
  999 RETURN
      END
