      SUBROUTINE PL2VEC(YS, ZS, NS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw a 2D cells bounded by NS edges.
C-                         Auxiliary routine for PCSVEN.
C-
C-   Inputs  :     YS      2 x NS Y-vector  describing the begin and end
C-                         points of an edge
C-                 ZS      Z-vector
C-                 NS      number of edges
C-
C-   Warning!:     This routine strongly depends on CELVEC output YS & ZS.
C-
C-   Created  11-APR-1990   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER  NS
      REAL YS(2,NS), ZS(2,NS)
      REAL XPF(5), YPF(5)
C----------------------------------------------------------------------
C--- (NS=12)
      IF (NS .EQ. 12) THEN
        YPF(1) = YS(1,1)
        XPF(1) = ZS(1,1)
        YPF(2) = YS(2,2)
        XPF(2) = ZS(2,2)
        YPF(3) = YS(2,6)
        XPF(3) = ZS(2,6)
        YPF(4) = YS(1,5)
        XPF(4) = ZS(1,5)
C
        CALL JPOLGN(XPF, YPF, 4)
C-
C--- (NS=15)
      ELSEIF (NS .EQ. 15) THEN
        YPF(1) = YS(1,1)
        XPF(1) = ZS(1,1)
        YPF(2) = YS(1,2)
        XPF(2) = ZS(1,2)
        YPF(3) = YS(1,3)
        XPF(3) = ZS(1,3)
        YPF(4) = YS(1,4)
        XPF(4) = ZS(1,4)
        YPF(5) = YS(1,5)
        XPF(5) = ZS(1,5)
C
        CALL JPOLGN(XPF, YPF, 5)
C-
C--- (NS= 9)
      ELSEIF (NS .EQ. 9) THEN
        YPF(1) = YS(1,1)
        XPF(1) = ZS(1,1)
        YPF(2) = YS(1,2)
        XPF(2) = ZS(1,2)
        YPF(3) = YS(1,3)
        XPF(3) = ZS(1,3)
C
        CALL JPOLGN(XPF, YPF, 3)
C-
C--- (NS=21)
      ELSEIF (NS .EQ. 21) THEN
        YPF(1) = YS(1,2)
        XPF(1) = ZS(1,2)
        YPF(2) = YS(1,6)
        XPF(2) = ZS(1,6)
        YPF(3) = YS(2,6)
        XPF(3) = ZS(2,6)
        YPF(4) = YS(1,12)
        XPF(4) = ZS(1,12)
C
        CALL JPOLGN(XPF, YPF, 4)
C
        YPF(1) = YS(1,7)
        XPF(1) = ZS(1,7)
        YPF(2) = YS(1,16)
        XPF(2) = ZS(1,16)
        YPF(3) = YS(1,17)
        XPF(3) = ZS(1,17)
C
        CALL JPOLGN(XPF, YPF, 3)
C-
      ENDIF
C-
  999 RETURN
      END
