      SUBROUTINE EMULMV(IC, IM, RLIST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Transforms a vector by a matrix (Called by JESCAP
C-                         function)
C-
C-   Inputs  :
C-             IC             : 0 = Transform in world coordinate system
C-                              1 = Transform in virtual coordinate system
C-                             -1 = Transform with translation taken out
C-             IM             : Matrix number
C-             RLIST(1:96)    : Array elements to choose a 4X4 matrix from
C-             RLIST(97:99)   : The 3-D input vector
C-
C-   Outputs :
C-          RLIST(100:102) : The 3-D output vector.
C-
C-   Controls:
C-
C-   Created   11-JUN-1990   SHAHRIAR ABACHI
C-   Modified  07-NOV-1990   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL  RLIST(*)
      INTEGER  IM, IC
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      REAL  MATX(96), XV, YV, ZV, XV2, YV2, ZV2
      REAL  MAT(4,4), MAT2(4,4), SX, SY, SZ
      REAL  V0(4), V1(4), SC(3)
      INTEGER  I, J, K, IV, IJK
C
      IJK = 96
C
      DO I=1,3
        V0(I) = RLIST(IJK+I)
      ENDDO
      V0(3) = V0(3) * RIGHT             ! Change it to left handed coordinate
C
      V0(4) = 1.0
      IF(IC .EQ. -1) V0(4) = 0.0        ! no translations
C
      DO I=1,4
        V1(I) = 0.0
      ENDDO

C
      DO I=1,IJK
        MATX(I) = RLIST(I)
      ENDDO
C
      K = (IM - 1) * 16
C
      DO I = 1,4
        DO J = 1,4
          K = K + 1
          MAT(I,J) = MATX(K)
        ENDDO
      ENDDO
C
      CALL MXTRP(MAT, MAT2, 4, 4)
C
      IF(IC .EQ. 1) THEN        ! Changed to apply to virtual coordinate
        SC(1) = ( UWIND(2) - UWIND(1) ) / ( UVIEW(2) - UVIEW(1) )
        SC(2) = ( UWIND(4) - UWIND(3) ) / ( UVIEW(4) - UVIEW(3) )
        SC(3) = SX
        DO I=1,3
          MAT2(I,4) = MAT2(I,4) / SC(I)
        ENDDO
      ENDIF
C
      CALL VMATR(V0, MAT2, V1, 4, 4)
      V1(3) = V1(3) * RIGHT   ! Back to original handedness
C
      IJK = 99
      DO I=1,3
        RLIST(IJK + I) = V1(I)
      ENDDO
C
  999 RETURN
      END
