      SUBROUTINE      HV_COR_JETS(NUMRUN,LPOINT,JETS)
C------------------------------------------------------------------
C
C Purpose and Methods: Returns HV-gain corrected JETS for runs taken at
C                      Calorimeter HV  of 2000V.
C
C INPUTS: NUMRUN(I*4)  = RUN NUMBER
C         LPOINT(I*4)   = Link to JETS bank.
C         JETS bank.
C
C OUTPUT: JETS(1)     = Px
C         JETS(2)     = Py
C         JETS(3)     = Pz
C         JETS(4)     = E
C         JETS(5)     = Et
C         JETS(6)     = Theta
C         JETS(7)     = Phi
C         JETS(8)     = Eta
C         JETS(9)     = Fraction of EM Et = EM Et/Total_ET
C         JETS(10)    = Fraction of ICD/MG Et
C
C
C Controls : HV_COR_RCP
C
C Created 20-OCT-1992 Hiro Aihara
C
C-------------------------------------------------------------------

      IMPLICIT        NONE

      INCLUDE         'D0$INC:ZEBCOM.INC'
      INCLUDE         'D0$INC:ZLINKC.INC'

      INTEGER*4       LPOINT
      INTEGER*4       GZJETS

      INTEGER*4       NUMRUN

      REAL*4          JETS(10)

      REAL*4          Px, Py, Pz, E, Et, Theta, Phi, Eta

      INTEGER*4       FIRST_RUN_TO_CORRECT/52470/
      REAL*4          CC_EM /1.015/
      REAL*4          CC_HAD/1.023/
      REAL*4          EC_EM /1.016/
      REAL*4          EC_HAD/1.019/

      LOGICAL         FIRST
      DATA            FIRST/.TRUE./

      INTEGER*4       IER

      REAL*4          SMALL
      PARAMETER       (SMALL=1.0E-5)
      REAL*4          TWOPI
      PARAMETER       (TWOPI=6.283185307)

      REAL*4          EM_FRAC, ICD_FRAC
      REAL*4          Eta_JET
      REAL*4          EM_Ex, EM_Ey, EM_ET, ICD_ET
      REAL*4          EZOE

      INTEGER*4       i, j


      IF(FIRST) THEN

        FIRST = .FALSE.

        IER = 0
C          call INRCP('HV_COR_RCP',IER)

        IF(IER.EQ.0) THEN
          CALL EZPICK('HV_COR_RCP')
          CALL EZGET_i('FIRST_RUN_TO_CORRECT',
     &                   FIRST_RUN_TO_CORRECT,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_JETS',
     &                      'Fail to read FIRST_RUN/Use Default','W')
          END IF
          CALL EZGET('CC_EM',CC_EM,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_JETS',
     &                      'Fail to read CC_EM/Use Default','W')
          END IF
          CALL EZGET('EC_EM',EC_EM,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_JETS',
     &                      'Fail to read EC_EM/Use Default','W')
          END IF
          CALL EZGET('CC_HAD',CC_HAD,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_JETS',
     &                      'Fail to read CC_HAD/Use Default','W')
          END IF
          CALL EZGET('EC_HAD',EC_HAD,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_JETS',
     &                      'Fail to read EC_HAD/Use Default','W')
          END IF

          CALL EZRSET

        ELSE
          CALL ERRMSG('CAL','HV_COR',
     &                  'FAIL to read HV_COR_RCP/Use Default','W')
        END IF

      END IF

      IF(LPOINT.EQ.0) THEN
        CALL ERRMSG('CAL','HV_COR_JETS','LPOINT=0','W')
        JETS(5)=-999.
        RETURN
      END IF

      JETS( 1) = Q(LPOINT+2)
      JETS( 2) = Q(LPOINT+3)
      JETS( 3) = Q(LPOINT+4)
      JETS( 4) = Q(LPOINT+5)
      JETS( 5) = Q(LPOINT+6)
      JETS( 6) = Q(LPOINT+7)
      JETS( 7) = Q(LPOINT+8)
      JETS( 8) = Q(LPOINT+9)
      JETS( 9) = Q(LPOINT+14)
      JETS(10) = Q(LPOINT+17)

      IF(NUMRUN.LT.FIRST_RUN_TO_CORRECT) RETURN  ! No correction necessary.

      Eta_JET = Q(LPOINT+9)
      EM_FRAC = Q(LPOINT+14)
      ICD_FRAC = Q(LPOINT+17)

      DO i = 1, 4  ! Ex, Ey, Ez, E

        IF(abs(Eta_JET).LE.1.3) THEN ! approx. CC jet
          JETS(i) = Q(LPOINT+1+i)*EM_FRAC*CC_EM
     &               + Q(LPOINT+1+i)*(1.-EM_FRAC-ICD_FRAC)*CC_HAD
     &               + Q(LPOINT+1+i)*ICD_FRAC
        ELSE                         ! approx. EC jet
          JETS(i) = Q(LPOINT+1+i)*EM_FRAC*EC_EM
     &               + Q(LPOINT+1+i)*(1.-EM_FRAC-ICD_FRAC)*EC_HAD
     &               + Q(LPOINT+1+i)*ICD_FRAC
        END IF

      END DO

      IF(abs(Eta_JET).LE.1.3) THEN    ! approx. CC jet
        EM_Ex   = Q(LPOINT+2)*EM_FRAC*CC_EM
        EM_Ey   = Q(LPOINT+3)*EM_FRAC*CC_EM
      ELSE
        EM_Ex   = Q(LPOINT+2)*EM_FRAC*EC_EM
        EM_Ey   = Q(LPOINT+3)*EM_FRAC*EC_EM
      END IF

      EM_ET = sqrt(EM_Ex**2+EM_Ey**2)
      ICD_ET = Q(LPOINT+6)*ICD_FRAC

      JETS(5) = sqrt(JETS(1)**2+JETS(2)**2)

      EZOE = (JETS(3)+SMALL)/(JETS(4)+SMALL)
      JETS(6) = acos(EZOE)
      JETS(8) = -ALOG(TAN(JETS(6)/2.)+SMALL)

      JETS(7) = atan2(JETS(2),JETS(1)+SMALL)
      IF(JETS(7).LT.0.) JETS(7) = JETS(7)+TWOPI

      IF(JETS(5).GT.0.) THEN
        JETS( 9) = EM_ET/JETS(5)
        JETS(10) = ICD_ET/JETS(5)
      END IF

      RETURN


      END
