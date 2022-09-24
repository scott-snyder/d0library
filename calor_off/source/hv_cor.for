      FUNCTION        HV_COR(NUMRUN,ILYR,IETA)
C------------------------------------------------------------------
C
C Purpose and Methods: Returns gain correction factor for runs taken at
C                      Calorimeter HV  of 2000V.
C                      MULTIPLY uncorrected energy by this factor to get
C                      corrected energy.
C
C INPUTS: NUMRUN(I*4)  = RUN NUMBER
C         ILYR(I*4)    = Layer Index
C         IETA(I*4)    = Eta   Index (ie. Detector Eta)
C
C OUTPUT: HV_COR = Correction factor.
C
C Controls : HV_COR_RCP
C
C Created 16-OCT-1992 Hiro Aihara
C
C-------------------------------------------------------------------

      IMPLICIT        NONE

      INCLUDE         'D0$PARAMS:CAL_OFFLINE.PARAMS'

      REAL*4          HV_COR

      INTEGER*4       NUMRUN
      INTEGER*4       ILYR
      INTEGER*4       IETA

      INTEGER*4       FIRST_RUN_TO_CORRECT/52470/
      REAL*4          CC_EM /1.015/   ! Defaults to be modified by RCP
      REAL*4          CC_HAD/1.023/   ! values.
      REAL*4          EC_EM /1.016/
      REAL*4          EC_HAD/1.019/

      LOGICAL         FIRST
      DATA            FIRST/.TRUE./

      INTEGER*4       IER


      IF(FIRST) THEN

        FIRST = .FALSE.

        IER = 0
C          call INRCP('HV_COR_RCP',IER)
        IF(IER.EQ.0) THEN
          CALL EZPICK('HV_COR_RCP')
          CALL EZGET_i('FIRST_RUN_TO_CORRECT',FIRST_RUN_TO_CORRECT,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','HV_COR',
     &                      'Fail to read FIRST_RUN/Use Default','W')
          END IF
          CALL EZGET('CC_EM',CC_EM,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','CC_EM',
     &                      'Fail to read CC_EM/Use Default','W')
          END IF
          CALL EZGET('EC_EM',EC_EM,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','EC_EM',
     &                      'Fail to read EC_EM/Use Default','W')
          END IF
          CALL EZGET('CC_HAD',CC_HAD,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','CC_HAD',
     &                      'Fail to read CC_HAD/Use Default','W')
          END IF
          CALL EZGET('EC_HAD',EC_HAD,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('CAL','EC_HAD',
     &                      'Fail to read EC_HAD/Use Default','W')
          END IF
          CALL EZRSET
        ELSE
          CALL ERRMSG('CAL','HV_COR',
     &                  'FAIL to read HV_COR_RCP/Use Default','W')
        END IF

      END IF

      HV_COR = 1.

      IF(NUMRUN.LT.FIRST_RUN_TO_CORRECT) RETURN

      IF(ILYR.EQ.LYICD) THEN  ! ICD
        HV_COR = 1.
      ELSE IF(ILYR.GE.MNLYEM .AND. ILYR.LE.MXLYEM) THEN ! EM
        IF(abs(IETA).LE.12) THEN     ! CC
          HV_COR = CC_EM
        ELSE                         ! EC
          HV_COR = EC_EM
        END IF
      ELSE IF(ILYR.GE.MNLYFH .AND. ILYR.LE.MXLYCH) THEN ! HAD
        IF(abs(IETA).LE.10 .AND. ILYR.LE.MXLYFH) THEN  ! CCFH
          HV_COR = CC_HAD
        ELSE IF(abs(IETA).LE.6 .AND. ILYR.EQ.MNLYCH) THEN ! CCCH
          HV_COR = CC_HAD
        ELSE IF(abs(IETA).GE.10 .AND. ILYR.LE.MNLYCH) THEN ! ECMH+ECIH
          HV_COR = EC_HAD
        ELSE IF(ILYR.GE.16 .OR.
     &           (ILYR.EQ.MNLYCH .AND.
     &           (abs(IETA).GE.8 .AND. abs(IETA).LE.12))) THEN ! ECOH
          HV_COR = EC_HAD
        END IF
      ELSE IF(ILYR.EQ.MNLYMG) THEN ! CCMG
        HV_COR = CC_HAD
      ELSE IF(ILYR.EQ.MXLYMG) THEN ! ECMG
        HV_COR = EC_HAD
      END IF

      RETURN

      END
