      SUBROUTINE      HV_COR_PNUT(NUMRUN,NUTR)
C------------------------------------------------------------------
C
C Purpose and Methods: Returns HV-gain corrected missing Et for runs taken at
C                      Calorimeter HV  of 2000V.
C                      Returns non-zero NUTR(1:4,i) if and only if
C                      PNUTi bank exists.
C                      Correction is based on JETS bank.
C
C INPUTS: NUMRUN(I*4)  = RUN NUMBER
C         PNUT bank.
C         JETS bank.
C
C OUTPUT: NUTR(1,i)     = Ex
C         NUTR(2,i)     = Ey
C         NUTR(3,i)     = Et = sqrt(Ex**2+Ey**2)
C         NUTR(4,i)     = Phi (radian)
C         i runs from 1 through 3 corresponding to PNUT1, PNUT2 and PNUT3.
C
C Controls : HV_COR_RCP
C
C Created 16-OCT-1992 Hiro Aihara
C
C-------------------------------------------------------------------

      IMPLICIT        NONE

      INCLUDE         'D0$INC:ZEBCOM.INC'
      INCLUDE         'D0$INC:ZLINKC.INC'

      INTEGER*4       GZPNUT, GZJETS

      INTEGER*4       NUMRUN

      REAL*4          NUTR(4,3)

      REAL*4          Px, Py, Phi

      INTEGER*4       FIRST_RUN_TO_CORRECT/52470/
      REAL*4          CC_EM /1.015/
      REAL*4          CC_HAD/1.023/
      REAL*4          EC_EM /1.016/
      REAL*4          EC_HAD/1.019/

      LOGICAL         FIRST
      DATA            FIRST/.TRUE./

      INTEGER*4       IER

      REAL*4          COR_MIX/1.01825/
      REAL*4          HAD_COR/1.021/

      REAL*4          SMALL
      PARAMETER       (SMALL=1.0E-5)
      REAL*4          TWOPI
      PARAMETER       (TWOPI=6.283185307)

      INTEGER*4       ICHOICE_HAD/1/
      REAL*4          TEMPLATE(5,4)
      DATA            TEMPLATE/
     &                 1.,6.,0.7,0.,0.,        ! CONE R=0.7
     &                 1.,6.,0.5,0.,0.,        ! CONE R=0.5
     &                 1.,6.,0.3,0.,0.,        ! CONE R=0.3
     &                 2.,7.,2.0,8.,2./        ! NN   2x2

      REAL*4          EM_FRAC, ICD_FRAC
      REAL*4          Px_ICD, Py_ICD
      REAL*4          Px_JETS_OLD, Py_JETS_OLD
      REAL*4          Px_JETS_NEW, Py_JETS_NEW
      REAL*4          Px_EM_OLD, Py_EM_OLD
      REAL*4          Px_EM_NEW, Py_EM_NEW
      REAL*4          Px_HAD_OLD, Py_HAD_OLD
      REAL*4          Px_MUON, Py_MUON
      REAL*4          Eta_JET

      INTEGER*4       i, j

      REAL*4          Temp


      IF(FIRST) THEN

        FIRST = .FALSE.

        IER = 0
!          call INRCP('HV_COR_RCP',IER)

        IF(IER.EQ.0) THEN
          CALL EZPICK('HV_COR_RCP')
          CALL EZGET('FIRST_RUN_TO_CORRECT',
     &                   FIRST_RUN_TO_CORRECT,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_PNUT',
     &                      'Fail to read FIRST_RUN/Use Default','W')
          END IF
          CALL EZGET('HAD_JETS_ALGORITHM',ICHOICE_HAD,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_PNUT',
     &                      'Fail to read HAD_JETS_ALGORITHM','W')
          END IF
          CALL EZGET('CC_EM',CC_EM,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_PNUT',
     &                      'Fail to read CC_EM/Use Default','W')
          END IF
          CALL EZGET('EC_EM',EC_EM,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_PNUT',
     &                      'Fail to read EC_EM/Use Default','W')
          END IF
          CALL EZGET('CC_HAD',CC_HAD,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_PNUT',
     &                      'Fail to read CC_HAD/Use Default','W')
          END IF
          CALL EZGET('EC_HAD',EC_HAD,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_PNUT',
     &                      'Fail to read EC_HAD/Use Default','W')
          END IF

          CALL EZRSET

          COR_MIX = (CC_EM+EC_EM+CC_HAD+EC_HAD)/4.
          HAD_COR = (CC_HAD+EC_HAD)/2.

        ELSE
          CALL ERRMSG('CAL','HV_COR',
     &                  'FAIL to read HV_COR_RCP/Use Default','W')
        END IF

      END IF


      DO i = 1, 3
        DO j = 1, 4
          NUTR(j,i) =0.
        END DO
      END DO

      LPNUT1 = GZPNUT(1)
      IF(LPNUT1.NE.0) THEN
        NUTR(1,1) = Q(LPNUT1+3)
        NUTR(2,1) = Q(LPNUT1+4)
        NUTR(3,1) = Q(LPNUT1+7)
        NUTR(4,1) = Q(LPNUT1+10)
      ELSE
        RETURN
      END IF

      LPNUT2 = GZPNUT(2)
      IF(LPNUT2.NE.0) THEN
        NUTR(1,2) = Q(LPNUT2+3)
        NUTR(2,2) = Q(LPNUT2+4)
        NUTR(3,2) = Q(LPNUT2+7)
        NUTR(4,2) = Q(LPNUT2+10)
      END IF

      LPNUT3 = GZPNUT(3)
      IF(LPNUT3.NE.0) THEN
        NUTR(1,3) = Q(LPNUT3+3)
        NUTR(2,3) = Q(LPNUT3+4)
        NUTR(3,3) = Q(LPNUT3+7)
        NUTR(4,3) = Q(LPNUT3+10)
      END IF

      IF(NUMRUN.LT.FIRST_RUN_TO_CORRECT) RETURN  ! No correction necessary.

      IF(ICHOICE_HAD.EQ.1) CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
      IF(ICHOICE_HAD.EQ.2) CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
      IF(ICHOICE_HAD.EQ.3) CALL SET_CAPH('CONE_JET',TEMPLATE(1,3),IER)
      IF(ICHOICE_HAD.EQ.4) CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)

      LJETS = GZJETS()

      IF(LJETS.EQ.0) THEN     ! If no jets, missing Et is due to underlying
                              ! event particles.
        DO i = 1, 3
          NUTR(i,1) = NUTR(i,1)*COR_MIX  ! PNUT1
        END DO

        IF(LPNUT2.NE.0) THEN

          Px_ICD = Q(LPNUT1+3)-Q(LPNUT2+3)
          Py_ICD = Q(LPNUT1+4)-Q(LPNUT2+4)
          NUTR(1,2) = NUTR(1,1) - Px_ICD
          NUTR(2,2) = NUTR(2,1) - Py_ICD
          NUTR(3,2) = sqrt(NUTR(1,2)**2+NUTR(2,2)**2)
          NUTR(4,2) = atan2(NUTR(2,2),NUTR(1,2)+SMALL)
          IF(NUTR(4,2).lt.0.) NUTR(4,2)=NUTR(4,2)+TWOPI

          IF(LPNUT3.NE.0) THEN
            Px_MUON = Q(LPNUT2+3)-Q(LPNUT3+3)
            Py_MUON = Q(LPNUT2+4)-Q(LPNUT3+4)
            NUTR(1,3) = NUTR(1,2) - Px_MUON
            NUTR(2,3) = NUTR(2,2) - Py_MUON
            NUTR(3,3) = sqrt(NUTR(1,3)**2+NUTR(2,3)**2)
            NUTR(4,3) = atan2(NUTR(2,3),NUTR(1,3)+SMALL)
            IF(NUTR(4,3).lt.0.) NUTR(4,3)=NUTR(4,3)+TWOPI
          END IF

        END IF

      ELSE

        IF(LPNUT2.NE.0) THEN

          Px_ICD = Q(LPNUT1+3)-Q(LPNUT2+3)
          Py_ICD = Q(LPNUT1+4)-Q(LPNUT2+4)

          Px_JETS_OLD = 0.
          Py_JETS_OLD = 0.
          Px_HAD_OLD = 0.
          Py_HAD_OLD = 0.
          Px_EM_NEW   = 0.
          Py_EM_NEW   = 0.
          Px_JETS_NEW  = 0.
          Py_JETS_NEW  = 0.

          DO WHILE (LJETS.GT.0)

            Eta_JET = Q(LJETS+9)
            EM_FRAC  = Q(LJETS+14)

            Px_JETS_OLD = Px_JETS_OLD + Q(LJETS+2)
            Py_JETS_OLD = Py_JETS_OLD + Q(LJETS+3)
            Px_HAD_OLD = Px_HAD_OLD + Q(LJETS+2)*(1.-EM_FRAC)
            Py_HAD_OLD = Py_HAD_OLD + Q(LJETS+3)*(1.-EM_FRAC)

            IF(abs(Eta_JET).LE.1.3) THEN   ! approx. CC jet
              Px_EM_NEW = Px_EM_NEW
     &                       + Q(LJETS+2)*EM_FRAC*CC_EM
              Py_EM_NEW = Py_EM_NEW
     &                       + Q(LJETS+3)*EM_FRAC*CC_EM
            ELSE                           ! approx. EC jet
              Px_EM_NEW = Px_EM_NEW
     &                       + Q(LJETS+2)*EM_FRAC*EC_EM
              Py_EM_NEW = Py_EM_NEW
     &                       + Q(LJETS+3)*EM_FRAC*EC_EM
            END IF

            LJETS = LQ(LJETS)

          END DO

          Px_JETS_OLD = Px_JETS_OLD - Px_ICD
          Py_JETS_OLD = Py_JETS_OLD - Py_ICD
          Px_HAD_OLD  = Px_HAD_OLD -  Px_ICD
          Py_HAD_OLD  = Py_HAD_OLD -  Py_ICD

          Px_JETS_NEW = Px_HAD_OLD*HAD_COR + Px_EM_NEW
          Py_JETS_NEW = Py_HAD_OLD*HAD_COR + Py_EM_NEW

          NUTR(1,1) = NUTR(1,1) + Px_JETS_OLD ! underlying event -px
          NUTR(2,1) = NUTR(2,1) + Py_JETS_OLD !                  -py

          NUTR(1,1) = NUTR(1,1)*COR_MIX - Px_JETS_NEW
          NUTR(2,1) = NUTR(2,1)*COR_MIX - Py_JETS_NEW

          NUTR(3,1) = sqrt(NUTR(1,1)**2+NUTR(2,1)**2)
          NUTR(4,1) = atan2(NUTR(2,1),NUTR(1,1)+SMALL)
          IF(NUTR(4,1).lt.0.) NUTR(4,1)=NUTR(4,1)+TWOPI

          NUTR(1,2) = NUTR(1,1) - Px_ICD
          NUTR(2,2) = NUTR(2,1) - Py_ICD

          NUTR(3,2) = sqrt(NUTR(1,2)**2+NUTR(2,2)**2)
          NUTR(4,2) = atan2(NUTR(2,2),NUTR(1,2)+SMALL)
          IF(NUTR(4,2).lt.0.) NUTR(4,2)=NUTR(4,2)+TWOPI

          IF(LPNUT3.NE.0) THEN
            Px_MUON = Q(LPNUT2+3)-Q(LPNUT3+3)
            Py_MUON = Q(LPNUT2+4)-Q(LPNUT3+4)
            NUTR(1,3) = NUTR(1,2) - Px_MUON
            NUTR(2,3) = NUTR(2,2) - Py_MUON
            NUTR(3,3) = sqrt(NUTR(1,3)**2+NUTR(2,3)**2)
            NUTR(4,3) = atan2(NUTR(2,3),NUTR(1,3)+SMALL)
            IF(NUTR(4,3).lt.0.) NUTR(4,3)=NUTR(4,3)+TWOPI
          END IF

        ELSE ! No ICD

          Px_JETS_OLD = 0.
          Py_JETS_OLD = 0.
          Px_JETS_NEW = 0.
          Py_JETS_NEW = 0.

          DO WHILE (LJETS.GT.0)

            Eta_JET = Q(LJETS+9)
            EM_FRAC  = Q(LJETS+14)

            Px_JETS_OLD = Px_JETS_OLD + Q(LJETS+2)
            Py_JETS_OLD = Py_JETS_OLD + Q(LJETS+3)

            IF(abs(Eta_JET).LE.1.3) THEN   ! approx. CC jet
              Px_JETS_NEW = Px_JETS_NEW
     &                         + Q(LJETS+2)*EM_FRAC*CC_EM
     &                         + Q(LJETS+2)*(1.-EM_FRAC)*CC_HAD
              Py_JETS_NEW = Py_JETS_NEW
     &                         + Q(LJETS+3)*EM_FRAC*CC_EM
     &                         + Q(LJETS+3)*(1.-EM_FRAC)*CC_HAD
            ELSE                           ! approx. EC jet
              Px_JETS_NEW = Px_JETS_NEW
     &                         + Q(LJETS+2)*EM_FRAC*EC_EM
     &                         + Q(LJETS+2)*(1.-EM_FRAC)*EC_HAD
              Py_JETS_NEW = Py_JETS_NEW
     &                         + Q(LJETS+3)*EM_FRAC*EC_EM
     &                         + Q(LJETS+3)*(1.-EM_FRAC)*EC_HAD
            END IF

            LJETS = LQ(LJETS)

          END DO

          NUTR(1,1) = NUTR(1,1) + Px_JETS_OLD ! underlying event -px
          NUTR(2,1) = NUTR(2,1) + Py_JETS_OLD !                  -py
          NUTR(1,1) = NUTR(1,1)*COR_MIX - Px_JETS_NEW
          NUTR(2,1) = NUTR(2,1)*COR_MIX - Py_JETS_NEW

          NUTR(3,1) = sqrt(NUTR(1,1)**2+NUTR(2,1)**2)
          NUTR(4,1) = atan2(NUTR(2,1),NUTR(1,1)+SMALL)
          IF(NUTR(4,1).lt.0.) NUTR(4,1)=NUTR(4,1)+TWOPI

          IF(LPNUT3.NE.0) THEN
            Px_MUON = Q(LPNUT1+3)-Q(LPNUT3+3)
            Py_MUON = Q(LPNUT1+4)-Q(LPNUT3+4)
            NUTR(1,3) = NUTR(1,2) - Px_MUON
            NUTR(2,3) = NUTR(2,2) - Py_MUON
            NUTR(3,3) = sqrt(NUTR(1,3)**2+NUTR(2,3)**2)
            NUTR(4,3) = atan2(NUTR(2,3),NUTR(1,3)+SMALL)
            IF(NUTR(4,3).lt.0.) NUTR(4,3)=NUTR(4,3)+TWOPI
          END IF

        END IF



      END IF

      CALL RESET_CAPH

      RETURN


      END
