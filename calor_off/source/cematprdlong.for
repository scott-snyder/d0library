      SUBROUTINE CEMATPRDLONG(READIN,IETA,ENERGY)
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
      DOUBLE PRECISION EMATRLP_ALL(37,NDIML,NDIML,4)
      DOUBLE PRECISION APARAMS(NDIML,4),APARAMS_ALL(37,NDIML,4)
      DOUBLE PRECISION EMATRL_VIS(NDIMVL,NDIMVL)
      DOUBLE PRECISION HMATRPL(NDIMP,NDIMP)
      REAL ENERGY,XP,YP
      REAL    WORK(NDIMH)
      INTEGER I,II,J,JJ,K,L,NLOPS,NTMS
      INTEGER NERROR,IETA
      INTEGER IHMUN,IERR
      CHARACTER*80 FILNAM,INFIL
      LOGICAL READIN,OK,FIRST
      DATA FIRST /.TRUE./
      DATA NTMS /4/
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('CLEANEM_RCP')
C
C:::  Read in parameters for now
C
C      IF(READIN) THEN

        DO 100 I = 1,35
          IF(I .EQ. 13 .OR. I .EQ. 14) GOTO 100
          WRITE(FILNAM,101) I
          CALL EZ_FILE_OPEN(501,FILNAM,'IF',IHMUN,INFIL,IERR)
          IF(IERR .NE. 0) THEN
            CALL ERRMSG('CLEANEM','CEMATPRDLONG',
     &      'UNABLE TO READ IN HMATRIX PARAMETERS','F')
          ENDIF
          READ(IHMUN,*)EMATRLP,APARAMS
          DO J = 1,NDIML
            DO L = 1,4
              DO K = 1,NDIML
                EMATRLP_ALL(I,K,J,L) = EMATRLP(K,J,L)
              ENDDO
              APARAMS_ALL(I,J,L) = APARAMS(J,L)
            ENDDO
          ENDDO
          CLOSE(UNIT=IHMUN)
          CALL RLUNIT(501,IHMUN,IERR)
  100   CONTINUE
        CALL EZRSET
C      ENDIF
      ENDIF
C
  101 FORMAT('HMATRIX_ETA',I2.2,'_PARAMETERS')
C
C
C:::  Recreate EMATRL and create EMATRL_VIS
C
      DO 10 I = 1,NDIML
        DO 20 J = 1,I
          XP = ENERGY
          YP = EMATRLP_ALL(IETA,J,I,ntms)*XP
          NLOPS = NTMS-1
          DO 30 WHILE(NLOPS.GT.1)
          YP = XP*(EMATRLP_ALL(IETA,J,I,NLOPS)+YP)
          NLOPS = NLOPS-1
   30   CONTINUE
        YP = YP+EMATRLP_ALL(IETA,J,I,1)
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
C      CALL MATRIX_INVERT_2('EMATRL',EMATRL,NDIML,WORK,HMATRL,NERROR)
C
C      IF(NERROR.NE.0)THEN
C        CALL ERRMSG('CAPHEL','CEMATPRDLONG',
C     &    'ERROR INVERTING EMATRL MATRIX','W')
C      ENDIF
C
C
C::: Now to get HMATRL_VIS, the visible portion of the longitudinal H matrix.
C
      CALL MATRIX_INVERT_2('EMATRL_VIS',EMATRL_VIS,NDIMVL,WORK,
     &  HMATRL_VIS,NERROR)
C
      IF(NERROR.NE.0)THEN
        CALL ERRMSG('CAPHEL','CEMATPRDLONG',
     &    'ERROR INVERTING EMATRL_VIS MATRIX','W')
      ENDIF
C
C
C:::  Now to invert the invisible portion of HMATRL
C
C      II = 0
C      DO 1600 I = IPREDL , NDIML
C        II = II + 1
C        JJ = 0
C        DO 1600 J = IPREDL , NDIML
C          JJ = JJ+1
C          HMATRPL(II,JJ) = HMATRL(I,J)
C 1600 CONTINUE
C
C      CALL MATRIX_INVERT_2
C     &  ('HMATRPL',HMATRPL,NDIMP,WORK,HMATRL_INV,NERROR)
C
C      IF(NERROR.NE.0)THEN
C        CALL ERRMSG('CAPHEL','CEMATPRDLONG',
C     &    'ERROR INVERTING HMATRL_INV','W')
C      ENDIF
C
C::: Recreate AVERL
C
      DO 210 J = 1,NDIML
        XP = ENERGY
        YP = APARAMS_ALL(IETA,J,ntms)*XP
        NLOPS = NTMS-1
        DO 230 WHILE(NLOPS.GT.1)
        YP = XP*(APARAMS_ALL(IETA,J,NLOPS)+YP)
        NLOPS = NLOPS-1
  230 CONTINUE
      AVERL(J) = YP+APARAMS_ALL(IETA,J,1)
  210 CONTINUE
C
  999 RETURN
      END
