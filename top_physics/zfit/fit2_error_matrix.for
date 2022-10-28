      SUBROUTINE FIT2_ERROR_MATRIX(XM,BLEP_ERR,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate 3x3 error matrix for electron/muon 
C-
C-   Returned value  :
C-   Inputs  :    XM        (R)  Measured 3-momenta of the lepton   
C-                          
C-   Outputs :    BLEP_ERR  (R)  3x3 Error matrix FOR THE TWO LEPTONS
C-                IER       (I)  Error code
C-   Controls:
C-
C-   Created  4-SEP-1993   Pushpa C. Bhat  Separate error calculation
C-                                         from FIT_TWO_EVENT
C-
C----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION XM(8)
      DIMENSION ELEC_MAT(3,3),EIG(3),AMAT(3,3),BMAT(3,3)
      DIMENSION BLEP_ERR(2,3,3)
      REAL    SIGTHSQR,SIGPHISQR,SIGMA_E
      LOGICAL MUMU,EE,FIRST,EZERROR,DO_ELECTRONS,DO_MUONS
C----------------------------------------------------------------------
      INTEGER IER,I,J,K
C----------------------------------------------------------------------
      DATA FIRST /.TRUE./
      SAVE FIRST,SIGTHSQ,SIGPHISQ,SIGMA_E
C----------------------------------------------------------------------
C
C ****  Read rcp file for the first event.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('FIT_TWO_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('FIT_TWO','FIT2_ERROR_MATRIX',
     &      'Cannot find bank FIT_TWO_RCP','W')
          GOTO 999
        ENDIF
C
        CALL EZGET_l('DO_ELECTRONS',DO_ELECTRONS,IER)
        CALL EZGET_l('DO_MUONS',DO_MUONS,IER)
        IF(.NOT.DO_ELECTRONS)MUMU=.TRUE.
        CALL EZGET ('ELEC_SAMPLING_TERM',SIGMA_E,IER)
        IF(IER.NE.0)SIGMA_E=.15
        CALL EZGET ('SIGMA_THETA_SQ',SIGTHSQR,IER)
        CALL EZGET ('SIGMA_PHI_SQ',SIGPHISQR,IER)
        SIGTHSQ=SIGTHSQR
        SIGPHISQ=SIGPHISQR
      ENDIF
C
C ****  calculate error_matrix
C
      DO i=1,3
        DO j=1,3
          amat(i,j)=0.0
          bmat(i,j)=0.0
          do k=1,2
            blep_err(k,i,j)=0.
          enddo
        ENDDO
      ENDDO
C
C ****  FIRST MUONS
C
      IF(DO_MUONS)THEN
        SIG1BYP1=.20*XM(1)
        BLEP_ERR(1,1,1)=SIG1BYP1**2.+.0001
        BLEP_ERR(1,2,2)=SIGTHSQ
        BLEP_ERR(1,3,3)=SIGPHISQ
        SIG1BYP2=.20*XM(4)
        BLEP_ERR(2,1,1)=SIG1BYP2**2.+.0001
        BLEP_ERR(2,2,2)=SIGTHSQ
        BLEP_ERR(2,3,3)=SIGPHISQ
        GOTO 900
      ENDIF
      ener=sqrt(xm(1)**2.+xm(2)**2.+xm(3)**2.)
C      sigesq=.15*.15*ener
      sigesq=SIGMA_E*SIGMA_E*ener
C      IF(MUMU) SIGESQ = (.01*ENER*ENER)**2.
      theta=acos(xm(3)/ener)
      phi=atan2(xm(2),xm(1))
      et=sqrt(xm(1)**2+xm(2)**2)
      amat(1,1)=sigesq
      amat(2,2)=sigthsq
      amat(3,3)=sigphisq
      bmat(1,1)=xm(1)/ener
      bmat(2,1)=xm(2)/ener
      bmat(3,1)=xm(3)/ener
      bmat(1,2)=xm(3)*xm(1)/et
      bmat(1,3)=-xm(2)
      bmat(2,2)=xm(2)*xm(3)/et
      bmat(2,3)=xm(1)
      bmat(3,2)=-et
      bmat(3,3)=0.
      CALL lsq_matrix_fill('B_MAT1',BMAT,3,3,2,IER)
      CALL lsq_matrix_fill('A_MAT',AMAT,3,3,2,IER)
      CALL LSQ_MATRIX_TRANSPOSE('B_MAT1','B_MAT2',IER)
      CALL LSQ_MATRIX_MULTIPLY('A_MAT','B_MAT2','C_MAT',IER)
      CALL LSQ_MATRIX_MULTIPLY('B_MAT1','C_MAT','ELEC_MATR',IER)
      CALL LSQ_MATRIX_GET('ELEC_MATR',ELEC_MAT,3,3,2,IER)
C
      CALL LSQ_MATRIX_DIAG('ELEC_MATR','UNI_MAT','EIG_MAT',IER)
      IF(IER.GT.0)WRITE(6,*)' ERROR IN DIAG'
      CALL LSQ_MATRIX_GET('EIG_MAT',EIG,3,1,2,IER)
      DO I=1,3
        IF(EIG(I).LT.0)WRITE(6,*)'NEGATIVE EIGEN VALUES FOR LEPTON1'
        IF(EIG(I).EQ.0)WRITE(6,*)'ZERO EIGEN VALUES FOR LEPTON1'
        IF(EIG(I).LE.0)WRITE(6,*)'RUN ',RUN,'EVENT  ',EVENT,EIG(I)
      ENDDO
C
      DO I=1,3
        DO J=1,3
          BLEP_ERR(1,I,J)=ELEC_MAT(I,J)
        ENDDO
      ENDDO
C
C ****  SECOND LEPTON
C
      ener=sqrt(xm(4)**2.+xm(5)**2.+xm(6)**2.)
C      sigesq=.15*.15*ener
      sigesq=SIGMA_E*SIGMA_E*ener
C      IF(MUMU) SIGESQ = (.01*ENER*ENER)**2.
      theta=acos(xm(6)/ener)
      phi=atan2(xm(5),xm(4))
      et=sqrt(xm(4)**2+xm(5)**2)
      DO i=1,3
        DO j=1,3
          amat(i,j)=0.0
          bmat(i,j)=0.0
        ENDDO
      ENDDO
      amat(1,1)=sigesq
      amat(2,2)=sigthsq
      amat(3,3)=sigphisq
      bmat(1,1)=xm(4)/ener
      bmat(2,1)=xm(5)/ener
      bmat(3,1)=xm(6)/ener
      bmat(1,2)=xm(6)*xm(4)/et
      bmat(1,3)=-xm(5)
      bmat(2,2)=xm(5)*xm(6)/et
      bmat(2,3)=xm(4)
      bmat(3,2)=-et
      bmat(3,3)=0.
      CALL lsq_matrix_fill('B_MAT1',BMAT,3,3,2,IER)
      CALL lsq_matrix_fill('A_MAT',AMAT,3,3,2,IER)
      CALL LSQ_MATRIX_TRANSPOSE('B_MAT1','B_MAT2',IER)
      CALL LSQ_MATRIX_MULTIPLY('A_MAT','B_MAT2','C_MAT',IER)
      CALL LSQ_MATRIX_MULTIPLY('B_MAT1','C_MAT','ELEC_MATR',IER)
      CALL LSQ_MATRIX_GET('ELEC_MATR',ELEC_MAT,3,3,2,IER)
C
      CALL LSQ_MATRIX_DIAG('ELEC_MATR','UNI_MAT','EIG_MAT',IER)
      IF(IER.GT.0)WRITE(6,*)' ERROR IN DIAG'
      CALL LSQ_MATRIX_GET('EIG_MAT',EIG,3,1,2,IER)
      DO I=1,3
        IF(EIG(I).LT.0)WRITE(6,*)'NEGATIVE EIGEN VALUES FOR LEPTON2'
        IF(EIG(I).EQ.0)WRITE(6,*)'ZERO EIGEN VALUES FOR LEPTON2'
        IF(EIG(I).LE.0)WRITE(6,*)'RUN ',RUN,'EVENT  ',EVENT,EIG(I)
      ENDDO
C
      DO I=1,3
        DO J=1,3
          BLEP_ERR(2,I,J)=ELEC_MAT(I,J)
        ENDDO
      ENDDO
C
  900 CONTINUE
  999 RETURN
      END
