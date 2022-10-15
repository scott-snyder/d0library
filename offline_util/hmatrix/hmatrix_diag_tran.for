      SUBROUTINE HMATRIX_DIAG_TRAN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Transform the vector QUAN(1:VIS_DIM)
C-   to the co-ordinate system in which HVIS is diagonal
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-DEC-1990   Rajendran Raja
C-   Updated  28-JAN-1993   Meenakshi Narain  
C-                          fetch the value of eigen_value_max for each cluster
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      INCLUDE 'D0$INC:HMATRIX_PARS.INC'
      INTEGER I,J,K,IER
      DOUBLE PRECISION HMATRIX_DARRAY,HMATRIX_DVECT
      INTEGER HMINDEX,IND
      REAL    HMATRIX_ARRAY
      INTEGER PRUNIT,DMPUNI
      INTEGER NEIGEN(100),LEIGEN,LEIGEN_DIM,SSUNIT
      LOGICAL FIRST,TEST_UMAT
      DATA FIRST/.TRUE./
      INTEGER LOW_DIM,HIGH_DIM
      REAL    CHISQ,PROB
      REAL    EIGEN_VALUE_MAX ,EIG
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET_l('TEST_UMAT',TEST_UMAT,IER)
        CALL EZGETA_i('USE_EIGEN_VECTORS',0,0,0,LEIGEN,IER)
        CALL EZGETA_iarr('USE_EIGEN_VECTORS',1,LEIGEN,1,NEIGEN,IER)
        IF ( LEIGEN.NE.0 ) THEN
          WRITE(SSUNIT(),1)LEIGEN,NEIGEN
    1     FORMAT(' THE FOLLOWING ',I5,' EIGEN VECTORS WILL BE USED',
     &    ' CALCULATING TRUNCATED CHISQUARED ',/,
     &    (8I5))
        ELSEIF ( LEIGEN.EQ.0 ) THEN
          WRITE(SSUNIT(),2)
    2     FORMAT(
     &    ' NO SELECTION BEING MADE IN CALCULATING TRUNCATED',
     &    ' CHISQUARED')
        ELSE
          CALL ERRMSG('HMATRIX','HMATRIX_DIAG_TRAN',
     &      'BAD VALUE OF LEIGEN','W')
        ENDIF
C
        CALL EZRSET
C
        IF ( TEST_UMAT ) THEN
          DO I = 1 , VIS_DIM
            DO J = 1 , VIS_DIM
              IND = HMINDEX(I,J,VIS_DIM,VIS_DIM)
              C(LWORK+IND) = 0.0
              DO K = 1 , VIS_DIM
                C(LWORK+IND) = C(LWORK+IND) +
     &        HMATRIX_DARRAY(LHVIS,I,K,VIS_DIM,VIS_DIM)*
     &        HMATRIX_ARRAY(LUMAT,K,J,VIS_DIM,VIS_DIM)
              ENDDO
            ENDDO
          ENDDO
          PRUNIT = DMPUNI()
          CALL MXPRNT(PRUNIT,'DUMP OF WORK',C(LWORK+1),VIS_DIM,
     &  VIS_DIM,VIS_DIM,VIS_DIM,7,'(F12.5)')
        ENDIF
      ENDIF

      CALL EZPICK('HMATRIX_RCP')
      EIGEN_VALUE_MAX = 0.0
      CALL EZGET('EIGEN_VALUE_MAX',EIGEN_VALUE_MAX,IER)
      CALL EZRSET
C
C ****  NOW TO PRODUCE DIAG BANK BY MULTIPLYING UMAT (TRANSVERSE) INTO
C ****  QUAN-AVER
C
      DO I = 1 , VIS_DIM
        C(LDIAG+I) = 0.
        DO J = 1 , VIS_DIM
          C(LDIAG+I) = C(LDIAG+I) +
     &      HMATRIX_ARRAY(LUMAT,J,I,VIS_DIM,VIS_DIM)*
     &      (C(LQUAN+J)-HMATRIX_DVECT(LAVER,J))
        ENDDO
        EIG = C(LEIGN+I)
        IF ( EIGEN_VALUE_MAX.NE.0.0 ) THEN
C
C ****  IF EIGEN_VALUE_MAX IS NON ZERO, IT IS USED TO SET A MAXIMUM EIGEN VALUE
C ****  THIS IS A WAY OF ADDING SYSTEMATIC ERRORS TO THE ERROR MATRIX . THE
C ****  EIGEN VALUES HERE ARE THE INVERSE OF THE ERROR MATRIX EIGEN VALUES
C
          IF(EIG.GT.EIGEN_VALUE_MAX)THEN
            EIG = EIGEN_VALUE_MAX 
          ENDIF
        ENDIF
        C(LDIAG+I) = C(LDIAG+I)*SQRT(EIG)        ! NORMALIZING IT
        ! TO UNIT GAUSSIAN
      ENDDO
C
      CHISQ = 0.
      LEIGEN_DIM = 0
      DO I = 1,VIS_DIM
        IF ( LEIGEN.EQ.0 ) THEN
          CHISQ = CHISQ + C(LDIAG+I)**2
          LEIGEN_DIM = LEIGEN_DIM + 1
        ELSE
          DO J = 1 , LEIGEN
            IF(NEIGEN(J).EQ.I)THEN
              CHISQ = CHISQ + C(LDIAG+I)**2
              LEIGEN_DIM = LEIGEN_DIM + 1
              GO TO 123
            ENDIF
          ENDDO
        ENDIF
  123   CONTINUE
      ENDDO
C
C ****  NOW TO FILL HMTR
C
      C(LHMTR+6) = CHISQ
      C(LHMTR+7) = LEIGEN_DIM
      C(LHMTR+8) = PROB(CHISQ,IABS(LEIGEN_DIM))
C
  999 RETURN
      END
