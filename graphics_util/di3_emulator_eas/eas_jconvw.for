      SUBROUTINE JCONVW(VX, VY, WX, WY, WZ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
CD   This module converts a virtual device coordinate (VX, VY) to a
CD   world coordinate value using the current viewing transformation.
C-
C-   Inputs  : VX, VY
C-   Outputs : WX, WY, WZ
C-   Controls:
C-
C-   Created  26-OCT-1988   A. VIRGO, S. ABACHI
C-   UPDATED  08-MAY-1989   N. OSHIMA      generalized for all coordinates
C-   UPDATED  06-JUL-1990   S. ABACHI      Completely restructred
C-   UPDATED  12-JUL-1990   S. ABACHI      generalized to include 3D matrices
C-   Modified 04-SEP-1992   N. OSHIMA      remove 3D part, add VUPNT trans.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL VX, VY, WX, WY, WZ
      REAL NUX(3), NUY(3), NUZ(3), CC, VLENY, VLENZ, MXVW(3,3)
      REAL VV(3), VW(3), NORML2(3), VLENX
      REAL WX2, WY2, WZ2
      INTEGER I, J, IM, IC, IJK
      REAL    SCALX, SCALY
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      INCLUDE 'D0$INC:NEWDI3.INC/LIST'
      INCLUDE 'D0$PARAMS:ESCAPE_CODE_NAMES.DEF/LIST'
C
C----------------------------------------------------------------------
C
      SCALX = (UWIND(2) - UWIND(1)) / (UVIEW(2) - UVIEW(1))
      SCALY = (UWIND(4) - UWIND(3)) / (UVIEW(4) - UVIEW(3))
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
      WX = UWIND(1) + (VX - UVIEW(1)) * SCALX
      WY = UWIND(3) + (VY - UVIEW(3)) * SCALY
      WZ = 0.0
C
      VV(1) = WX
      VV(2) = WY
      VV(3) = WZ
      CALL VMATR(VV, MXVW, VW, 3, 3)
      WX = VW(1) + VUPNT(1)
      WY = VW(2) + VUPNT(2)
      WZ = VW(3) + VUPNT(3) * RIGHT
C
      RETURN
      END
