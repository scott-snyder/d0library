      SUBROUTINE CEMATPRDCTL(READIN,IETA,ENERGY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INTERPOLATES LONGITUDINAL COVARIANCE
C-                         MATRIX ELEMENTS FOR IETA 1 BIN (SO FAR)
C-
C-                         ALSO PREDICTS AVERAGE QUANTITIES IN
C-                         AVERL.
C-
C-                         Functional form of fit is a quadratic
C-        viz.
C-
C-        EMAT(I,J) = A(I,J) + B(I,J)*ENERGY + C(I, J)*ENERGY**2
C-                        
C-   Inputs  : EM and FH visible energy for event
C-   Outputs : HMATRL, HMATRL_VIS, HMATRL_INV and AVERL in CHMATRL.INC
C-   Controls: NONE
C-
C-   Created   4-JAN-1990   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CHMATR.INC'
      DOUBLE PRECISION EMATRLP(NDIML,NDIML,4)
      DOUBLE PRECISION APARAMS(NDIML,4)
      DOUBLE PRECISION EMATRL_VIS(NDIMVL,NDIMVL)
      DOUBLE PRECISION HMATRPL(NDIMP,NDIMP)
      REAL ENERGY,XP,YP
      REAL    WORK(NDIMH)
      INTEGER I,II,J,JJ,K,NLOPS,NTMS
      INTEGER NERROR,IETA
      INTEGER IHMUN,IERR
      CHARACTER*80 FILNAM
      LOGICAL READIN,OK
      DATA NTMS /4/
C----------------------------------------------------------------------
C
C:::  Read in parameters for now
C
      IF(READIN) THEN
       WRITE(FILNAM,101) IETA        
       CALL GTUNIT(501,IHMUN,IERR)         
       CALL D0OPEN (IHMUN,FILNAM,'IF',OK)   
       READ(IHMUN,*)EMATRLP,APARAMS
       CLOSE(UNIT=IHMUN)               
       CALL RLUNIT(501,IHMUN,IERR)     
      ENDIF
C
  101 FORMAT('USR$FITS:HMATRIX_ETA',I2.2,'_PARAMS.DAT')             
C
C  
C:::  Recreate EMATRL and create EMATRL_VIS
C
      DO 10 I = 1,NDIML
        DO 20 J = 1,I
          XP = ENERGY
          YP = EMATRLP(J,I,ntms)*XP
          NLOPS = NTMS-1
          DO 30 WHILE(NLOPS.GT.1)
          YP = XP*(EMATRLP(J,I,NLOPS)+YP)
          NLOPS = NLOPS-1
   30   CONTINUE
        YP = YP+EMATRLP(J,I,1)
        EMATRL(J,I) = YP
        EMATRL(I,J) = EMATRL(J,I)
          IF(I.LE.NDIMVL)THEN
            EMATRL_VIS(J,I) = EMATRL(J,I)
            EMATRL_VIS(I,J) = EMATRL(J,I)
          ENDIF
   20 CONTINUE
   10 CONTINUE
C
C:::  Now to invert EMATRL longitudinal matrix
C
      CALL MATRIX_INVERT_2('EMATRL',EMATRL,NDIML,WORK,HMATRL,NERROR)
C
      IF(NERROR.NE.0)THEN
        CALL ERRMSG('CALORIMETER','CEMATPRDCTL',
     &    'ERROR INVERTING EMATRL MATRIX','W')
      ENDIF
C
C
C::: Now to get HMATRL_VIS, the visible portion of the longitudinal H matrix.
C
      CALL MATRIX_INVERT_2('EMATRL_VIS',EMATRL_VIS,NDIMVL,WORK,
     &  HMATRL_VIS,NERROR)
C
      IF(NERROR.NE.0)THEN
        CALL ERRMSG('CALORIMETER','CEMATPRDCTL',
     &    'ERROR INVERTING EMATRL_VIS MATRIX','W')
      ENDIF
C
C
C:::  Now to invert the invisible portion of HMATRL
C
      II = 0
      DO 1600 I = IPREDL , NDIML
        II = II + 1
        JJ = 0
        DO 1600 J = IPREDL , NDIML
          JJ = JJ+1
          HMATRPL(II,JJ) = HMATRL(I,J)
 1600 CONTINUE
C
      CALL MATRIX_INVERT_2                                
     &  ('HMATRPL',HMATRPL,NDIMP,WORK,HMATRL_INV,NERROR)  
C                                                         
      IF(NERROR.NE.0)THEN
        CALL ERRMSG('CALORIMETER','CEMATPRDCTL',
     &    'ERROR INVERTING HMATRL_INV','W')
      ENDIF
C
C::: Recreate AVERL                                        
C                                                          
        DO 210 J = 1,NDIML                                 
          XP = ENERGY                                     
          YP = APARAMS(J,ntms)*XP                             
          NLOPS = NTMS-1                                   
          DO 230 WHILE(NLOPS.GT.1)                         
            YP = XP*(APARAMS(J,NLOPS)+YP)                  
            NLOPS = NLOPS-1                                
  230     CONTINUE                                         
          AVERL(J) = YP+APARAMS(J,1)                             
  210   CONTINUE                                           
C
  999 RETURN
      END
