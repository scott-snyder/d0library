      SUBROUTINE GET_PROJ_QUAN(NEW_SYS,W3_VEC,JET3_VEC,JET_PROJ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the Dot product of the
C-   Jet 3 Vector Jet3_VEC along the W direction, Beam direction
C-   and along the normal to the plane containing the W and the Beam
C-
C-   Inputs  : NEW_SYS if true, will calculate the normal and W directions.
C-             W3_VEC = Px,py,pz of W.
C-             JET3_VEC = px,py,pz of Jet
C-
C-   Outputs : JET_PROJ(1) = Projection of JET3_VEC along W direction.
C-             JET_PROJ(2) = Projection of JET3_VEC along beam direction.
C-             JET_PROJ(3) = Projection of JET3_VEC along normal
C-   Controls:
C-
C-   Created   9-JAN-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    W3_VEC(*),JET3_VEC(*),JET_PROJ(*)
      REAL    W_DIR(3),BEAM_DIR(3),NORMAL(3)
      REAL    WMOM
      DATA BEAM_DIR /0.,0.,1.0/
      INTEGER I
      LOGICAL NEW_SYS
C----------------------------------------------------------------------
      IF ( NEW_SYS ) THEN
        WMOM = 0.
        DO I = 1 ,3
          WMOM = WMOM + W3_VEC(I)**2
        ENDDO
        WMOM = SQRT(WMOM)
        DO I = 1 ,3
          W_DIR(I) = W3_VEC(I)/WMOM
        ENDDO
C
        CALL CROSS(W_DIR,BEAM_DIR,NORMAL)
      ENDIF
C
      CALL DOT(JET3_VEC,W_DIR,3,JET_PROJ(1))
      CALL DOT(JET3_VEC,BEAM_DIR,3,JET_PROJ(2))
      CALL DOT(JET3_VEC,NORMAL,3,JET_PROJ(3))
C
  999 RETURN
      END
