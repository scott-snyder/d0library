      SUBROUTINE HMATRIX_PREDICT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Predict the invisible quantities during
C-   the non-accumulate phase.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INTEGER I,J,K,HMINDEX
      REAL    CHISQ,DI,DJ
      INTEGER II,JJ,KK
      DOUBLE PRECISION DELJ,HMATRIX_DARRAY,HMATRIX_DVECT,DHINV,DHMAT
C----------------------------------------------------------------------
      IF ( .NOT.ACCUMULATE ) THEN
        IF ( INVIS_DIM.LE.0 ) THEN
C          CALL ERRMSG('HMATRIX','HMATRIX_PREDICT',
C     &      'NO INVISIBLE QUANTITIES TO PREDICT','W')
        ELSE
          DO K = VIS_DIM+1 , TOT_DIM
            KK = K-VIS_DIM
            C(LQUAN+K) = HMATRIX_DVECT(LAVER,K)
            DO I = VIS_DIM+1 , TOT_DIM
              II = I-VIS_DIM
              DHINV = HMATRIX_DARRAY(LHINV,II,KK,INVIS_DIM,INVIS_DIM)
              DO J = 1 , VIS_DIM
                DELJ = C(LQUAN+J) - HMATRIX_DVECT(LAVER,J)
                DHMAT = HMATRIX_DARRAY(LHMAT,I,J,TOT_DIM,TOT_DIM)
                C(LQUAN+K) = C(LQUAN+K) - DHINV*DHMAT*DELJ
              ENDDO
            ENDDO
          ENDDO
        ENDIF
      ELSE
        CALL ERRMSG('HMATRIX','HMATRIX_PREDICT',
     &    'CANNOT BE CALLED DURING ACCUMULATE PHASE ','W')
      ENDIF
  999 RETURN
      END
