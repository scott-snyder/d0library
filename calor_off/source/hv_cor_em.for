      SUBROUTINE      HV_COR_EM(NUMRUN,LPOINT,EM)
C------------------------------------------------------------------
C
C Purpose and Methods: Returns HV-gain corrected PELC/PPHO quantities
C                      for runs taken at Calorimeter HV  of 2000V.
C
C     Note           : This routine assumes Q(LPELC/PPHO+19) = calorimeter
C                      eta of cluster (IETA) is already filled.
C
C INPUTS: NUMRUN(I*4)  = RUN NUMBER
C         LPOINT(I*4)   = Link to PELC/PPHO bank.
C         PELC/PPHO bank.
C
C OUTPUT: EM( 1)    = Ex
C         EM( 2)    = Ey
C         EM( 3)    = Ez
C         EM( 4)    = E
C         EM( 5)    = Et
C         EM( 6)    = Theta
C         EM( 7)    = Eta
C         EM( 8)    = Phi
C         EM( 9)    = (sigEx)**2
C         EM(10)    = (sigEy)**2
C         EM(11)    = sigEt
C         EM(12)    = EM energy in cluster outside central tower
C         EM(13)    = Total energy in core cone
C         EM(14)    = Total energy in isolation cone
C         EM(15)    = EM energy in core cone
C         EM(16)    = EM energy in isolation cone
C
C
C Controls : HV_COR_RCP
C
C Created 26-OCT-1992 Hiro Aihara
C
C-------------------------------------------------------------------

      IMPLICIT        NONE

      INCLUDE         'D0$INC:ZEBCOM.INC'
      INCLUDE         'D0$INC:ZLINKC.INC'

      INTEGER*4       LPOINT

      INTEGER*4       NUMRUN

      REAL*4          EM(16)

      REAL*4          Eta_EM, HV_EM, HV_HAD
      REAL*4          EM_CORE, EM_ISO

      INTEGER*4       FIRST_RUN_TO_CORRECT/52470/
      REAL*4          CC_EM /1.015/  ! Defaults to be modified by RCP
      REAL*4          CC_HAD/1.023/  ! values.
      REAL*4          EC_EM /1.016/
      REAL*4          EC_HAD/1.019/

      LOGICAL         FIRST
      DATA            FIRST/.TRUE./

      INTEGER*4       IER

      REAL*4          SMALL
      PARAMETER       (SMALL=1.0E-5)
      REAL*4          TWOPI
      PARAMETER       (TWOPI=6.283185307)

      INTEGER*4       i


      IF(FIRST) THEN

        FIRST = .FALSE.

        IER = 0
C          call INRCP('HV_COR_RCP',IER)

        IF(IER.EQ.0) THEN
          CALL EZPICK('HV_COR_RCP')
          CALL EZGET('FIRST_RUN_TO_CORRECT',
     &                   FIRST_RUN_TO_CORRECT,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_EM',
     &                      'Fail to read FIRST_RUN/Use Default','W')
          END IF
          CALL EZGET('CC_EM',CC_EM,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_EM',
     &                      'Fail to read CC_EM/Use Default','W')
          END IF
          CALL EZGET('EC_EM',EC_EM,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_EM',
     &                      'Fail to read EC_EM/Use Default','W')
          END IF
          CALL EZGET('CC_HAD',CC_HAD,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_EM',
     &                      'Fail to read CC_HAD/Use Default','W')
          END IF
          CALL EZGET('EC_HAD',EC_HAD,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR_EM',
     &                      'Fail to read EC_HAD/Use Default','W')
          END IF

          CALL EZRSET

        ELSE
          CALL ERRMSG('CAL','HV_COR_EM',
     &                  'FAIL to read HV_COR_RCP/Use Default','W')
        END IF

      END IF

      IF(LPOINT.EQ.0) THEN
        CALL ERRMSG('CAL','HV_COR_EM','LPOINT=0','W')
        EM(5)=-999.
        RETURN
      END IF

      EM( 1) = Q(LPOINT+ 3)
      EM( 2) = Q(LPOINT+ 4)
      EM( 3) = Q(LPOINT+ 5)
      EM( 4) = Q(LPOINT+ 6)
      EM( 5) = Q(LPOINT+ 7)
      EM( 6) = Q(LPOINT+ 8)
      EM( 7) = Q(LPOINT+ 9)
      EM( 8) = Q(LPOINT+10)
      EM( 9) = Q(LPOINT+11)
      EM(10) = Q(LPOINT+12)
      EM(11) = Q(LPOINT+13)
      EM(12) = Q(LPOINT+14)
      EM(13) = Q(LPOINT+15)
      EM(14) = Q(LPOINT+16)
      EM(15) = Q(LPOINT+17)
      EM(16) = Q(LPOINT+18)

      IF(NUMRUN.LT.FIRST_RUN_TO_CORRECT) RETURN  ! No correction necessary.

      Eta_EM = Q(LPOINT+19)

      IF(abs(Eta_EM).LE.12.) THEN
        HV_EM  = CC_EM
        HV_HAD = CC_HAD
      ELSE
        HV_EM  = EC_EM
        HV_HAD = EC_HAD
      END IF

      DO i = 1, 5  ! Ex, Ey, Ez, E, Et
        EM(i) = EM(i)*HV_EM
      END DO

      EM( 9) = EM( 9)*HV_EM**2
      EM(10) = EM(10)*HV_EM**2
      EM(11) = EM(11)*HV_EM
      EM(12) = EM(12)*HV_EM

      EM_CORE = EM(15)
      EM_ISO  = EM(16)

      EM(13) = (EM(13) - EM_CORE)*HV_HAD + EM_CORE*HV_EM
      EM(14) = (EM(14) - EM_ISO )*HV_HAD + EM_ISO *HV_EM
      EM(15) = EM_CORE*HV_EM
      EM(16) = EM_ISO *HV_EM

      RETURN


      END
