      SUBROUTINE FCN(NPAR,GRAD,CHISQ,XP,IFLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-FEB-1993   Pushpa C. Bhat
C-
C----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:FIT_TWO.INC'
C----------------------------------------------------------------------
      DIMENSION  XP(*),GRAD(*),XVAL(8)
C----------------------------------------------------------------------
C      INTEGER NEV,I,J,IFLAG,IER
C----------------------------------------------------------------------
      REAL*8       LH_MAT(1,NOBS),RH_MAT(NOBS,1)
      DIMENSION    PR1_MAT(NOBS,1),DERMAT(8,6)
      DIMENSION    PZLAB(4),P11(4),P22(4),P1(4),P2(4)
      DIMENSION    PAR_STEP(6),XP_SAVE(6)
      REAL    CHISQ_CL
      REAL    XVAL1,XVAL2
C      REAL    ZMASS,PI,EMASS,PDEC,TH2,PHI2,PT,PR2_MAT
C----------------------------------------------------------------------
      DATA PI/3.14285/
      DATA ZMASS/91.17/
      DATA EMASS/.000511/
C----------------------------------------------------------------------
C
C ****  CALCULATE THE PARAMETERS
C
      IF(IFLAG.EQ.3)GOTO 900
C
C ****  For IFLAG=10, calculate der. matrix
C
      IPAR=0
  110 CONTINUE
      IF (IFLAG.EQ.10) THEN
        IPAR=IPAR+1
        DO I=1,6
          XP(I)=XP_SAVE(I)
          PAR_STEP(I)=XP(I)*.0000001D0
        ENDDO
          XP(IPAR)=XP(IPAR)+PAR_STEP(IPAR)
      ENDIF
C 
      DO I=1,3
        PZLAB(I)=-XP(I)
      ENDDO
C      PZLAB(3)=-XP(4)
      PZLAB(4)=SQRT(XP(1)**2+XP(2)**2+XP(3)**2+XP(6)**2)
      PDEC=XP(6)/2.
      P11(3)=PDEC*COS(XP(4))
      P11(2)=PDEC*SIN(XP(4))*SIN(XP(5))
      P11(1)=PDEC*SIN(XP(4))*COS(XP(5))
      P11(4)=0.
      DO I=1,3
        P11(4)=P11(4)+P11(I)**2
      ENDDO
      P11(4)=P11(4)+EMASS**2
      P11(4)=SQRT(P11(4))
      P22(3)=-P11(3)
      P22(2)=-P11(2)
      P22(1)=-P11(1)
      P22(4)= P11(4)
      CALL LORENTZ4(PZLAB,P11,P1)
      CALL LORENTZ4(PZLAB,P22,P2)
      DO I=1,3
        XVAL(I)=P1(I)
        XVAL(I+3)=P2(I)
      ENDDO
      IF(DO_MUONS)THEN
        XVAL(1)=1./SQRT(P1(1)**2.+P1(2)**2.+P1(3)**2.)
        XVAL(2)=ACOS(P1(3)*XVAL(1))
        XVAL(3)=ATAN2(P1(2),P1(1))
        IF(XVAL(3) .LT. 0)XVAL(3)= 2.*PI + XVAL(3)
        XVAL(4)=1./SQRT(P2(1)**2.+P2(2)**2.+P2(3)**2.)
        XVAL(5)=ACOS(P2(3)*XVAL(4))
        XVAL(6)=ATAN2(P2(2),P2(1))
        IF(XVAL(6) .LT. 0)XVAL(6)= 2.*PI + XVAL(6)
      ENDIF
      XVAL(7)=XP(1)
      XVAL(8)=XP(2)
C
C ****  Calculate the following to keep the stretch functions
C
      IF(IFLAG.EQ.10)THEN
        DO I=1,8
          XVAL1=XVAL(I)
          XVAL2=XVAL_SAVE(I)
          DERMAT(I,IPAR)= XVAL(I) - XVAL_SAVE(I)
          IF(PAR_STEP(IPAR).NE.0)THEN
            DERMAT(I,IPAR)=DERMAT(I,IPAR)/PAR_STEP(IPAR)
          ELSE
            DERMAT(I,IPAR)=0.
          ENDIF
        ENDDO
        IF(IPAR.LT.6)THEN
          GOTO 110
        ELSE
C          DO I=1,8
C            WRITE(6,*)(DERMAT(I,J),J=1,6)
C          ENDDO
          CALL LSQ_MATRIX_FILL('DER_MAT',DERMAT,8,6,2,IER)
          RETURN
        ENDIF
      ENDIF
C
C ****  DEFINE CHISQ TO BE CALCULATED
C
      DO I=1,NOBS
        LH_MAT(1,I)=XM(I)-XVAL(I)
      ENDDO
      CALL LSQ_MATRIX_FILL('LH_MATRIX',LH_MAT,1,NOBS,2,IER)
      CALL LSQ_MATRIX_TRANSPOSE('LH_MATRIX','RH_MATRIX',IER)
      CALL LSQ_MATRIX_MULTIPLY('INV_MATRIX','RH_MATRIX','PR1_MATRIX'
     &,IER)
      CALL LSQ_MATRIX_MULTIPLY('LH_MATRIX','PR1_MATRIX','PR2_MATRIX',
     &  IER)
      CALL LSQ_MATRIX_GET('PR2_MATRIX',PR2_MAT,1,1,2,IER)
C      CHISQ=PR2_MAT+((XP(6)-ZMASS_OBS)**2./ZWID_OBS**2.)
C      BW = XP(6)**2./((XP(6)**2.-ZMASS_OBS**2.)**2. + 
C     &  (ZMASS_OBS**4. * (2.*ZWID_OBS)**2.)/ZMASS_OBS**2.)
      IF(BW_CONSTR_RES)THEN
      BWCHISQ=0.
      ZMBW=XP(6) -10.*SIG_MASS
      DELM=SIG_MASS/20.
C      DO I=1,400
C        BWCHISQ=BWCHISQ+
C     1          ZWID_OBS**2./((ZMBW-ZMASS_OBS)**2.+ZWID_OBS**2./4.)
C     2         *EXP(-(XP(6)-ZMBW)**2./2.*SIG_MASS**2.)
C     3         *DELM
C        IF(I.EQ.1)WRITE(LUINT,*)' INTEG_LOOP, I=1:',ZMBW
C        IF(I.EQ.400)WRITE(LUINT,*)' INTEG_LOOP, I=400:',ZMBW
C        ZMBW=ZMBW+DELM
C      ENDDO
C      BWMAX=2.*SQRT(2.*PI)*SIG_MASS/(PI)
C      BWCHISQ=-2.*(LOG(BWCHISQ-BWMAX))
      ENDIF
C
      BW = ZWID_OBS/(2.*PI*((XP(6)-ZMASS_OBS)**2.+
     & (ZWID_OBS/2.)**2.))
      BWMAX = 2./(ZWID_OBS*PI)
      IF(BW_CONSTR)THEN
        CHISQ=PR2_MAT-2*(LOG(BW)-LOG(BWMAX))
      ELSEIF(BW_CONSTR_RES)THEN
C        CHISQ=PR2_MAT+BWCHISQ
        CHISQ=PR2_MAT-2.*(LOG((ZMASS_OBS**2.)*((ZWID_OBS+SIG_MASS)**2.))
     & -LOG((XP(6)**2.-ZMASS_OBS**2.)**2.+(ZMASS_OBS**2.)*
     & ((ZWID_OBS+SIG_MASS)**2.)))
      ELSE
        CHISQ=PR2_MAT
      ENDIF
      CHISQ_SAVE=CHISQ
C
  900 CONTINUE
      IF(IFLAG .EQ. 3)THEN
C
C ****  Save the parameters and steps
C
        DO I=1,6
          XP_SAVE(I)=XP(I)
        ENDDO
        DO I=1,8
          XVAL_SAVE(I)=XVAL(I)
        ENDDO
C        IF(DO_MUONS)THEN
C          DO I=1,4,3
C            PTEMP=1/XVAL(I)
C            XVAL_SAVE(I)=PTEMP*SIN(XVAL(I+1))*COS(XVAL(I+2))
C            XVAL_SAVE(I+1)=PTEMP*SIN(XVAL(I+1))*SIN(XVAL(I+2))
C            XVAL_SAVE(I+2)=PTEMP*COS(XVAL(I+1))
C          ENDDO
C        ENDIF
C        
        CALL LSQ_MATRIX_DELETE('LH_MATRIX',IER)
        CALL LSQ_MATRIX_DELETE('RH_MATRIX',IER)
        CALL LSQ_MATRIX_DELETE('PR1_MATRIX',IER)
        CALL LSQ_MATRIX_DELETE('PR2_MATRIX',IER)
      ENDIF
  999 RETURN
      END
