      SUBROUTINE GET_DILEP_QUAN(I,I1,I2,DPHI,DPHIMET,EEMASS,MT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-NOV-1993   Balamurali V from Meena's EESEL5.FOR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ELEC.INC'
      INCLUDE 'D0$INC:PHOT.INC'
      INCLUDE 'D0$INC:MET.INC'
      REAL     PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC'
C
      INTEGER I,I1,I2
      REAL    ET1,PHI1,ETA1,E1,THE1,CE1
      REAL    ET2,PHI2,ETA2,E2,THE2,CE2
      REAL    COSX,DPHI,DPHIMET(2),EEMASS(2),MT(2)
      REAL    METX,METPHIX
C----------------------------------------------------------------------
C
      ET1  = RELEC(1,I1)*RELEC(10,I1)
      PHI1 = RELEC(2,I1)
      ETA1 = RELEC(3,I1)
      E1   = RELEC(22,I1)
      THE1 = 2*ATAN(EXP(-ETA1))
      CE1  = E1*RELEC(10,I1)
C
      IF(I .EQ. 1)THEN
        ET2  = RELEC(1,I2)*RELEC(10,I2)
        PHI2 = RELEC(2,I2)
        ETA2 = RELEC(3,I2)
        E2   = RELEC(22,I2)
        THE2 = 2*ATAN(EXP(-ETA2))
        CE2  = E2*RELEC(10,I2)
      ELSEIF(I .EQ. 2)THEN
        ET2  = RPHOT(1,I2)*RPHOT(10,I2)
        PHI2 = RPHOT(2,I2)
        ETA2 = RPHOT(3,I2)
        E2   = RPHOT(16,I2)
        THE2 = 2*ATAN(EXP(-ETA2))
        CE2  = E2*RPHOT(10,I2)
      ELSEIF(I .EQ. 3)THEN
        ET1  = RPHOT(1,I1)*RPHOT(10,I1)
        PHI1 = RPHOT(2,I1)
        ETA1 = RPHOT(3,I1)
        E1   = RPHOT(16,I1)
        THE1 = 2*ATAN(EXP(-ETA1))
        CE1  = E1*RPHOT(10,I1)
C
        ET2  = RELEC(1,I2)*RELEC(10,I2)
        PHI2 = RELEC(2,I2)
        ETA2 = RELEC(3,I2)
        E2   = RELEC(22,I2)
        THE2 = 2*ATAN(EXP(-ETA2))
        CE2  = E2*RELEC(10,I2)
      ELSEIF(I .EQ. 4)THEN
        ET1  = RPHOT(1,I1)*RPHOT(10,I1)
        PHI1 = RPHOT(2,I1)
        ETA1 = RPHOT(3,I1)
        E1   = RPHOT(16,I1)
        THE1 = 2*ATAN(EXP(-ETA1))
        CE1  = E1*RPHOT(10,I1)
C
        ET2  = RPHOT(1,I2)*RPHOT(10,I2)
        PHI2 = RPHOT(2,I2)
        ETA2 = RPHOT(3,I2)
        E2   = RPHOT(16,I2)
        THE2 = 2*ATAN(EXP(-ETA2))
        CE2  = E2*RPHOT(10,I2)
      ELSE
        CALL ERRMSG('GET_DILEP_QUAN',' ','Unknown flag value','F')
      ENDIF
C
C ** Now compute various Dilepton quantities
C
      COSX=COS(THE1)*COS(THE2)+SIN(THE1)*SIN(THE2)*COS(PHI1-PHI2)

      EEMASS(1)=SQRT(2*E1*E2*(1-COSX))
      EEMASS(2)=SQRT(2*CE1*CE2*(1-COSX))

      DPHI=ABS(PHI1-PHI2)
      IF(DPHI.GT.PI)DPHI=2*PI-DPHI
      DPHI=DPHI/PI*180

      METX = META(1,2)
      METPHIX = META(2,2)
      MT(1)=SQRT(2*ET1*METX*(1-COS(PHI1-METPHIX)))
      MT(2)=SQRT(2*ET2*METX*(1-COS(PHI2-METPHIX)))
C
      DPHIMET(1)=ABS(PHI1-METPHIX)
      IF(DPHIMET(1).GT.PI)DPHIMET(1)=2*PI-DPHIMET(1)
      DPHIMET(1)=DPHIMET(1)/PI*180
      DPHIMET(2)=ABS(PHI2-METPHIX)
      IF(DPHIMET(2).GT.PI)DPHIMET(2)=2*PI-DPHIMET(2)
      DPHIMET(2)=DPHIMET(2)/PI*180
C
  999 RETURN
      END
