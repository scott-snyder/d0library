      SUBROUTINE DO_NEUTRINO_SEARCH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DOES TOP MASS FIT TO DILEPTON EVENTS
C-   ASSUMING NEUTRINO IS EMITTED A PRIORI UNBIASED IN PHI.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-MAR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:TOP_SOLNS.INC'
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INTEGER IER
      LOGICAL FIRST_TIME
      DOUBLE PRECISION    L1,L2,F1,F2,DELF
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
      INTEGER IS1,IS2,ICOMB,STATUS,NITER,ITER_MAX
      REAL    PHILOS,PHIHIS,DEL_LAMBDAS,DELFS
      real rtmp
C----------------------------------------------------------------------
C
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL EZGET('PHILO',PHILOS,IER)
        CALL EZGET('PHIHI',PHIHIS,IER)
        CALL EZGET_i('NPHI',NPHI,IER)
        CALL EZGET_i('NLAMBDA',NLAMBDA,IER)
        CALL EZGET('DEL_LAMBDA',DEL_LAMBDAS,IER)
        CALL EZGET('DEL_LAMBDA_ITERATE',rtmp,IER)
        del_lambda_iterate = rtmp
        CALL EZGET_i('NUMBER_CONFIGS',NCONFIG,IER)
        CALL EZGET_l('DO_ITERATE',DO_ITERATE,IER)
        CALL EZGET('DELTA_F',DELFS,IER)
        CALL EZGET_i('ITER_MAX',ITER_MAX,IER)
        NPHI = MAX(2,NPHI)
        NLAMBDA = MAX(2,NLAMBDA)
        CONFIG = 1   !OBSERVED CONFIGURATION
C
        PHILO=PHILOS
        PHIHI=PHIHIS
        DEL_LAMBDA=DEL_LAMBDAS
        DELF=DELFS
C
        CALL EZRSET
      ENDIF
C
      DO ICONFIG =  1 , NCONFIG
        DO ICOMB = 1 , 2    !Do both jet cobinations
          DELPHI = (PHIHI-PHILO)/(NPHI-1)
          DO IPHI = 1 , NPHI
            PHI = PHILO + (IPHI-1)*DELPHI
C
            PHI_MIN = PHI   !default
C
            CALL FIND_LAMBDA_LIMITS(IER)
C
            IF ( IER.EQ.0 ) THEN
C
              DO IS1 = 1 , 2
                DO IS2 = 1 , 2
                  DEL_MASS(IS1,IS2) = 99999.
                  SOL_MIN(IS1,IS2) = .FALSE.
                  WEIGHT(IS1,IS2) = 1.0
                ENDDO
              ENDDO
C
              IF ( DO_ITERATE ) THEN
C
C ****  FIND SOLUTION BY ITERATION
C
                DO IS1 = 1 , 2
                  DO IS2 = 1 , 2
                    FIRST_TIME = .TRUE.
                    NITER = 0
                    LAMBDA = LAMBDA_LO
  100               CONTINUE
                    NITER = NITER + 1
                    IF ( LAMBDA.GE.LAMBDA_LO.AND.LAMBDA.LE.LAMBDA_HI
     &                .AND.NITER.LE.ITER_MAX ) THEN
C
                      CALL SETUP_NEUTRINO
C
                      CALL SETUP_W                     !SETUP W VECTORS
C
                      CALL SETUP_TOP                   !SETS UP  TOP 4 VECTORS AND MASS
C
                      IF ( FIRST_TIME ) THEN

                        IF ( SOL(1).AND.SOL(2) ) THEN
                          F2 = TOP1(5,IS1) - TOP2(5,IS2)
                          L2 = LAMBDA
                          LAMBDA = LAMBDA + DEL_LAMBDA_ITERATE
                          FIRST_TIME = .FALSE.
                          GO TO 100
                        ELSE
C NO SOLUTION . TRY AGAIN
                          LAMBDA = LAMBDA + DEL_LAMBDA_ITERATE
                          GO TO 100
                        ENDIF
                      ELSE
C SECOND OR HIGHER STEP
                        IF ( SOL(1).AND.SOL(2) ) THEN
                          L1 = L2
                          F1 = F2
                          L2 = LAMBDA
                          F2 = TOP1(5,IS1) - TOP2(5,IS2)
                          IF(ABS(0.5*(F2+F1)).GT.DELF)THEN
                            IF ( F2.NE.F1 ) THEN
                              LAMBDA = (L1*F2-L2*F1)/(F2-F1)
                            ELSE
                              LAMBDA = LAMBDA + DEL_LAMBDA_ITERATE
                            ENDIF
                            GO TO 100
                          ELSE

C CONVERGED
                            CALL FIND_TOP_SOLUTION1(IS1,IS2)
                          ENDIF
                        ELSE
C NO SOLUTION
                          LAMBDA = LAMBDA + DEL_LAMBDA_ITERATE
                          GO TO 100
                        ENDIF
                      ENDIF
                    ENDIF
                  ENDDO
                ENDDO
              ELSE
C
C ****  FIND SOLUTION BY STEPPING
C
                LAMBDA = LAMBDA_LO
C
                DO WHILE(LAMBDA.GE.LAMBDA_LO.AND.LAMBDA.LT.LAMBDA_HI)
C
                  CALL SETUP_NEUTRINO
C
                  CALL SETUP_W                     !SETUP W VECTORS
C
                  CALL SETUP_TOP                   !SETS UP  TOP 4 VECTORS AND MASS
C
                  CALL FIND_TOP_SOLUTION           !FIND EQUAL MASS SOLUTION
C
                  LAMBDA = LAMBDA + DEL_LAMBDA
                ENDDO
              ENDIF
              CALL PARTON_WEIGHTS                  !CALCULATE PARTON MODEL WEIGHTS
              CALL SAVE_TOP_SOLUTION               !SAVE TOP SOLUTIONS TO NTUPLE
            ENDIF
          ENDDO
          CONFIG = CONFIG + 1
          CALL SWAP_LEPTON12                     !SWAP THE TWO LEPTONS
        ENDDO
        CALL GENERATE_CONFIG     !GENERATE NEXT CONFIGURATION
      ENDDO
C
  999 RETURN
      END
