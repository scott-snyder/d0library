      REAL FUNCTION TRD_ANAL_CATH(LTRDT,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Analysis of the TRD informations using TRD banks created
C-   Updated  28-APR-1994   A. Zylberstejn
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRD_PHI_Z.INC'
      INTEGER LTDST,LTANA,GZTDST
      INTEGER IFOIS,NA,NC
      INTEGER J,JC,LOUT,TRUNIT,ICL,NCLA
      REAL ECAT,SCAT,Z2,PHIT,TRD_ZCATH,ZC,WGHT,EMAX
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER ICH,LAYER,LTRDT,LTPRL
      LOGICAL DOPRINT,TRD_DO_PRINT,FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C
      IF(FIRST) THEN
        LOUT=TRUNIT()
        IFOIS=0
        FIRST=.FALSE.
      ENDIF
      LTDST=GZTDST()
      IFOIS=IFOIS+1
C      DOPRINT=IFOIS.LE.10
      DOPRINT=TRD_DO_PRINT().AND.LOUT.NE.0
C      IF(DOPRINT)WRITE(LOUT,*)' in anal_cath,ltdst',LTDST
      TRD_ANAL_CATH=-1000.
      IF(LTRDT.LE.0 .OR. LTDST.LE.0)GO TO 999
      IF(DOPRINT)
     +  WRITE(LOUT,*)' number of cathodes planes',Q(LTRDT+23)
      ICH=LAYER
      LTPRL=LQ(LTRDT-ICH)
      IF(LTPRL.LE.0) GO TO  999
      LTANA=LQ(LTDST-ICH)
      IF(DOPRINT)WRITE(LOUT,*)' in anal_cath,ltdst,ltana',LTDST,LTANA
      IF(LTANA.LE.0)GO TO 999
      NA=IQ(LTANA+300+4)
      NC=IQ(LTANA+300+5)
      IF(DOPRINT)WRITE(LOUT,*)' in anal_cath,layer',ICH,'na',NA,' nc',
     &    NC
      IF(NA.LE.0 .OR. NC.LE.0)GO TO 999
      PHIT=PHI_TRD(ICH)
      ECAT=0.
      SCAT=0.
      ZC=0.
      EMAX=.6*Q(LTDST+ICH+1)
      DO  JC=1,NC
        J=IQ(LTANA+300+50+NA+JC)
        ECAT=Q(LTANA+50+NA+JC)
        IF(ECAT.GE.0.2 .AND. ECAT.LE.EMAX)THEN
          WGHT=ECAT**2
          Z2=TRD_ZCATH(ICH,J)
          IF(ABS(Z2).LT.100.)THEN
            SCAT=SCAT+WGHT
            ZC=ZC+Z2*WGHT
          END IF
C          IF(DOPRINT)
C     +            WRITE(LOUT,*)' in TRD_anal_cath,ecat',ECAT,
C     &            'z2',Z2,' <z calc>',ZC/SCAT,' Z track',Z_TRD(ICH)
        END IF
      END DO
      IF(SCAT.LE.0.)GO TO 999
      TRD_ANAL_CATH=ZC/SCAT
  999 CONTINUE
      RETURN
      END
