      SUBROUTINE CEMATPRDCT(ENERGY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INTERPOLATES TRANSVERSE COVARIANCE
C-                         MATRIX ELEMENTS FOR IETA 1 BIN (SO FAR)
C-
C-                         ALSO PREDICTS AVERAGE QUANTITIES IN
C-                         AVR.
C-
C-                         Functional form of fit is a quadratic
C-        viz.
C-
C-        EMAT(I,J) = A(I,J) + B(I,J)*ENERGY + C(I, J)*ENERGY**2
C-                        
C-   Inputs  : EM and FH visible energy for event
C-   Outputs : HMATR, HMATR_VIS, HMATR_INV and AVR in CHMAT.INC
C-   Controls: NONE
C-
C-   Created   10-FEB-1990   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CHMATR.INC'
      DOUBLE PRECISION EMATP(NDIMH,NDIMH,4)
      DOUBLE PRECISION APARAMT(NDIMH,4)
      DOUBLE PRECISION EMAT_VIS(NDIMVFR,NDIMVFR)
      DOUBLE PRECISION HMATRPL(NDIMVFR,NDIMVFR)
      REAL ENERGY,XP,YP
      REAL    WORK(NDIMH)
      INTEGER I,II,J,JJ,K,NLOPS,NTMS
      INTEGER NERROR
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      DATA NTMS /4/
C----------------------------------------------------------------------
C
C:::  Read in parameters for now
C
      IF(FIRST) THEN
       READ(36,*)EMATP,APARAMT
       FIRST = .FALSE.
      ENDIF
C  
C:::  Recreate EMAT and create EMAT_VIS
C
      DO 10 I = 1,NDIMH
        DO 20 J = 1,I
          XP = ENERGY
          YP = EMATP(J,I,ntms)*XP
          NLOPS = NTMS-1
          DO 30 WHILE(NLOPS.GT.1)
          YP = XP*(EMATP(J,I,NLOPS)+YP)
          NLOPS = NLOPS-1
   30   CONTINUE
        YP = YP+EMATP(J,I,1)
        EMAT(J,I) = YP
        EMAT(I,J) = EMAT(J,I)
          IF(I.LE.NDIMVFR)THEN
            EMAT_VIS(J,I) = EMAT(J,I)
            EMAT_VIS(I,J) = EMAT(J,I)
          ENDIF
   20 CONTINUE
   10 CONTINUE
C
C:::  Now to invert EMAT longitudinal matrix
C
      CALL MATRIX_INVERT_2('EMAT',EMAT,NDIMH,WORK,HMAT,NERROR)
C
      IF(NERROR.NE.0)THEN
        CALL ERRMSG('CALORIMETER','CEMATPRDCT',
     &    'ERROR INVERTING EMAT MATRIX','W')
      ENDIF
C
C
C::: Now to get HMAT_VIS, the visible portion of the longitudinal H matrix.
C
      CALL MATRIX_INVERT_2('EMAT_VIS',EMAT_VIS,NDIMVFR,WORK,
     &  HMAT_VIS,NERROR)
C
      IF(NERROR.NE.0)THEN
        CALL ERRMSG('CALORIMETER','CEMATPRDCT',
     &    'ERROR INVERTING EMAT_VIS MATRIX','W')
      ENDIF
C
C
C:::  Now to invert the invisible portion of HMAT
C
      II = 0
      DO 1600 I = IPREDF , NDIMH
        II = II + 1
        JJ = 0
        DO 1600 J = IPREDF , NDIMH
          JJ = JJ+1
          HMATRPL(II,JJ) = HMAT(I,J)
 1600 CONTINUE
C
      CALL MATRIX_INVERT_2                                
     &  ('HMATRPL',HMATRPL,NDIMP,WORK,HMAT_INV,NERROR)  
C                                                         
      IF(NERROR.NE.0)THEN
        CALL ERRMSG('CALORIMETER','CEMATPRDCT',
     &    'ERROR INVERTING HMAT_INV','W')
      ENDIF
C
C::: Recreate AVR                                        
C                                                          
        DO 210 J = 1,NDIMH                                 
          XP = ENERGY                                     
          YP = APARAMT(J,ntms)*XP                             
          NLOPS = NTMS-1                                   
          DO 230 WHILE(NLOPS.GT.1)                         
            YP = XP*(APARAMT(J,NLOPS)+YP)                  
            NLOPS = NLOPS-1                                
  230     CONTINUE                                         
          AVR(J) = YP+APARAMT(J,1)                             
  210   CONTINUE                                           
C
  999 RETURN
      END
