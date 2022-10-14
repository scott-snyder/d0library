      SUBROUTINE NEURAL_INIT_JNDAT1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the Neural Network parameters from
C-   NEURAL_RCP into the JETNET common block /JNDAT1/.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  30-JAN-1991   Harrison B. Prosper, Pushpa Bhat
C-   Updated  23-JAN-1994   Chip Stewart   (JETNET 3.0)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:JNDAT1.INC'
      INTEGER IER,M,M10(10)
      REAL    P
      LOGICAL EZERROR,LNEW
      DATA LNEW/.TRUE./
      SAVE LNEW
C----------------------------------------------------------------------
C
      CALL EZPICK('NEURAL_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('NEURAL','NEURAL_INIT_JNDAT1',
     &    'Unable to pick bank NEURAL_RCP','F')
      ENDIF
C
C ****  Read in Network structure ONLY IF NOT READ WEIGHT FILE
C
      IF (LNEW) THEN
        
        CALL EZGETA('NETWORK_STRUCTURE',0,0,0,  MSTJN(1),IER) ! # of layers 
        CALL EZGET_iarr('NETWORK_STRUCTURE',M10, IER)     ! Nodes/layer
        IF(IER.EQ.0) THEN
          DO M = 1, MSTJN(1)
            MSTJN(9+M)=M10(M)
          END DO
        END IF
      END IF
C
C ****  EVERYTHING ELSE COMES FROM RCP
C
      CALL EZGET_i('PATTERNS_PER_UPDATE',M,IER) ! # patterns/Update
      IF(IER.EQ.0) MSTJN(2)=M
      CALL EZGET_i('SIGMOID_FUNCTION_TYPE',M,IER)
      IF(IER.EQ.0) MSTJN(3)=M
      CALL EZGET_i('ERROR_MEASURE_TYPE',M, IER)     ! Error measure
      IF(IER.EQ.0) MSTJN(4)=M
      CALL EZGET_i('UPDATING_PROCEDURE',M, IER)
      IF(IER.EQ.0) MSTJN(5)=M
      CALL EZGET_i('UPDATES_PER_EPOCH', M, IER)
      IF(IER.EQ.0) MSTJN(9)=M
      CALL EZGET_i('PRUNING',M, IER)
      IF(IER.EQ.0) MSTJN(21)=M
      CALL EZGET_i('SATURATION_MEASURE',M, IER)
      IF(IER.EQ.0) MSTJN(22)=M
      CALL EZGET_iarr('RECEPTIVE_FIELD_GEOMETRY_IN',M10, IER)
      IF(IER.EQ.0) CALL UCOPY(M10(1),MSTJN(23),2)
      CALL EZGET_iarr('RECEPTIVE_FIELD_GEOMETRY', M10, IER)
      IF(IER.EQ.0) CALL UCOPY(M10(1),MSTJN(25),2)
      CALL EZGET_i('RECEPTIVE_FIELD_HIDDEN', M, IER)
      IF(IER.EQ.0) MSTJN(27)=M
      CALL EZGET_iarr('BIT_PRECISION',M10, IER)
      IF(IER.EQ.0) CALL UCOPY(M10(1),MSTJN(28),3)
      CALL EZGET_i('WARNING_PROCEDURE',M, IER)
      IF(IER.EQ.0) MSTJN(31)=M
      CALL EZGET_i('WARNING_MAX',M, IER)
      IF(IER.EQ.0) MSTJN(32)=M
      CALL EZGET_i('LINE_SEARCH_MAX_ITERATIONS',M, IER)
      IF(IER.EQ.0) MSTJN(35)=M
      CALL EZGET_i('LINE_SEARCH_MAX_RESTARTS', M, IER)
      IF(IER.EQ.0) MSTJN(36)=M
C
C ****  Read in Performance parameters
C
      CALL EZGET('ETA',                     P,IER)
      IF(IER.EQ.0) PARJN(1)=P
      CALL EZGET('ALPHA',                   P, IER)
      IF(IER.EQ.0) PARJN(2)=P
      CALL EZGET('BETA',                    P, IER)
      IF(IER.EQ.0) PARJN(3)=P
      CALL EZGET('WEIGHT_WIDTH',            P, IER)
      IF(IER.EQ.0) PARJN(4) = P
      CALL EZGET('FORGETFULNESS',           P, IER)
      IF(IER.EQ.0) PARJN(5) = P
      CALL EZGET('NOISE_WIDTH',             P, IER)   
      IF(IER.EQ.0) PARJN(6) = P
      CALL EZGET('ETA_CHANGE_SCALE',        P, IER)
      IF(IER.EQ.0) PARJN(11) = P
      CALL EZGET('ALPHA_CHANGE_SCALE',      P, IER)
      IF(IER.EQ.0) PARJN(12) = P
      CALL EZGET('BETA_CHANGE_SCALE',       P, IER)
      IF(IER.EQ.0) PARJN(13) = P
      CALL EZGET('LAMBDA',                  P, IER)
      IF(IER.EQ.0) PARJN(14) = P
      CALL EZGET('LAMBDA_CHANGE_SCALE',     P, IER)
      IF(IER.EQ.0) PARJN(15) = P
      CALL EZGET('GAMMA',                   P, IER)
      IF(IER.EQ.0) PARJN(16) = P
      CALL EZGET('PRUNING_CUT',             P, IER)
      IF(IER.EQ.0) PARJN(17) = P
      CALL EZGET('PRUNING_SCALE',           P, IER)
      IF(IER.EQ.0) PARJN(18) = P
      CALL EZGET('PRUNING_TARGET_ERROR',    P, IER)
      IF(IER.EQ.0) PARJN(19) = P
      CALL EZGET('NOISE_CHANGE_SCALE',      P, IER)
      IF(IER.EQ.0) PARJN(20) = P
      CALL EZGET('QUICKPROP_SCALE',         P, IER)
      IF(IER.EQ.0) PARJN(21) = P
      CALL EZGET('QUICKPROP_MAX_WEIGHT',    P, IER)
      IF(IER.EQ.0) PARJN(22) = P
      CALL EZGET('SIGMOID_ANTI_FLAT',       P, IER)
      IF(IER.EQ.0) PARJN(23) = P
      CALL EZGET('LINE_SEARCH_CONVERGENCE', P, IER)
      IF(IER.EQ.0) PARJN(24) = P
      CALL EZGET('LINE_SEARCH_TOLERANCE',   P, IER)
      IF(IER.EQ.0) PARJN(25) = P
      CALL EZGET('LINE_SEARCH_MIN_CHANGE',  P, IER)
      IF(IER.EQ.0) PARJN(26) = P
      CALL EZGET('LINE_SEARCH_MAX_STEP',    P, IER)
      IF(IER.EQ.0) PARJN(27) = P
      CALL EZGET('SCG_SIGMA_ZERO',          P, IER)
      IF(IER.EQ.0) PARJN(28) = P
      CALL EZGET('SCG_LAMBDA_ZERO',         P, IER)
      IF(IER.EQ.0) PARJN(29) = P
      CALL EZGET('RPROP_SCALE_UP',          P, IER)
      IF(IER.EQ.0) PARJN(30) = P
      CALL EZGET('RPROP_SCALE_DOWN',        P, IER)
      IF(IER.EQ.0) PARJN(31) = P
      CALL EZGET('RPROP_SCALE_UP_MAX',      P, IER)
      IF(IER.EQ.0) PARJN(32) = P
      CALL EZGET('RPROP_SCALE_DOWN_MIN',    P, IER)
      IF(IER.EQ.0) PARJN(33) = P
C
      MSTJN(8) = 1  !JETMAP initialization done
      CALL EZRSET
  999 RETURN
      ENTRY NEURAL_INIT_JNDAT2
      LNEW = .FALSE.
      END
