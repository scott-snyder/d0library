      SUBROUTINE PTRD3D_GEO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : make 3D display for TRD geometry
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created   3-FEB-1991   Qizhong Li-Demarteau   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
      CHARACTER*3  PARCOL
      INTEGER ILAY, IFTRDL
      REAL R(4) ! RADIOUS OF TRD LAYERS
      REAL    ZLEN , Z1, Z2
      DATA PARCOL/'GRE'/
      DATA R/17.50,28.05,38.60,49.15/
      DATA ZLEN/187.0/
C----------------------------------------------------------------------
C
      CALL PUGETV('TRD DRAW 3D LAYER', IFTRDL)
      CALL PUOPEN
C
C draw layer boundaries
C
      CALL PXCOLR(PARCOL)         
      Z1 = ZLEN / 2
      Z2 = -Z1
      IF (IFTRDL .GT. 0) THEN
        DO 100 ILAY=1,4
          CALL JCIRCL(0.,0.,Z1,R(ILAY),0)
          CALL JCIRCL(0.,0.,Z2,R(ILAY),0)
          CALL J3MOVE(0., R(ILAY), Z1)
          CALL J3DRAW(0., R(ILAY), Z2)
          CALL J3MOVE(0.,-R(ILAY), Z1)
          CALL J3DRAW(0.,-R(ILAY), Z2)
  100   CONTINUE
      ELSE
        CALL JCIRCL(0.,0.,Z2,R(4),0)
        CALL J3MOVE(0., R(4), Z1)
        CALL J3DRAW(0., R(4), Z2)
        CALL J3MOVE(0.,-R(4), Z1)
        CALL J3DRAW(0.,-R(4), Z2)
      ENDIF
      CALL JRCLOS
C
  999 RETURN
      END
