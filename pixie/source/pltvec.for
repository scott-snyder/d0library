      SUBROUTINE PLTVEC(XS, YS, ZS, NS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw a 3D Figure Bounded by NS edges.
C-          The vectors XS, etc are dimensioned (2, NS) and could
C-          be (but need not be) generated by CELVEC to describe a
C-          cell shape.
C-
C-   Inputs  :     XS      2 x NS X-vector  describing the begin and end
C-                         points of an edge
C-                 YS      Y-vector
C-                 ZS      Z-vector
C-                 NS      number of edges
C-   Outputs :     NONE
C-   Controls:     NONE
C-
C-   Modified 01-FEB-1990   Nobu Oshima(Use J3PLGN, when NS=12)
C-   Created  24-APR-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER  NS, I, J
      REAL XS(2,NS), YS(2,NS), ZS(2,NS)
      REAL XPF(4), YPF(4), ZPF(4), XPB(4), YPB(4), ZPB(4)
C
C---
C-
      IF (NS .EQ. 12) THEN
        XPF(1) = XS(1,1)
        YPF(1) = YS(1,1)
        ZPF(1) = ZS(1,1)
C
        XPF(2) = XS(2,1)
        YPF(2) = YS(2,1)
        ZPF(2) = ZS(2,1)
C
        XPF(3) = XS(2,2)
        YPF(3) = YS(2,2)
        ZPF(3) = ZS(2,2)
C
        XPF(4) = XS(1,3)
        YPF(4) = YS(1,3)
        ZPF(4) = ZS(1,3)
C
        CALL J3PLGN(XPF,YPF,ZPF,4)
C-
        XPB(1) = XS(1,5)
        YPB(1) = YS(1,5)
        ZPB(1) = ZS(1,5)
C
        XPB(2) = XS(2,5)
        YPB(2) = YS(2,5)
        ZPB(2) = ZS(2,5)
C
        XPB(3) = XS(2,6)
        YPB(3) = YS(2,6)
        ZPB(3) = ZS(2,6)
C
        XPB(4) = XS(1,7)
        YPB(4) = YS(1,7)
        ZPB(4) = ZS(1,7)
C
        CALL J3PLGN(XPB,YPB,ZPB,4)
C-
        DO 110 J = 1,4
          CALL J3MOVE( XPF(J), YPF(J), ZPF(J))
          CALL JR3DRA( XPB(J)-XPF(J), YPB(J)-YPF(J), ZPB(J)-ZPF(J))
  110   CONTINUE
C-
      ELSE
        DO 200 I = 1, NS
          CALL J3MOVE(XS(1,I), YS(1,I), ZS(1,I))
          CALL JR3DRA(XS(2,I)-XS(1,I),YS(2,I)-YS(1,I),ZS(2,I)-ZS(1,I))
  200   CONTINUE
      ENDIF
C-
  999 RETURN
      END