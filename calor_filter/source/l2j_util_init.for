      FUNCTION L2J_UTIL_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize lookup arrays in common L2J_UTIL
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: see D0$INC:L2J_UTIL.INC
C-
C-   Created  20-NOV-1990   Richard Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:L2J_UTIL.INC'
C
      INTEGER I,K
      REAL TH,ETA
      LOGICAL FIRST,L2J_UTIL_INIT, RET_COND
      SAVE RET_COND
      DATA FIRST /.TRUE./
      DATA RET_COND /.FALSE./
C----------------------------------------------------------------------
      L2J_UTIL_INIT = RET_COND
      IF (.NOT. FIRST) RETURN           ! Do only once
      FIRST = .FALSE.
      L2J_UTIL_INIT = .TRUE.

C---Initialize Lookup arrays:  L2J_PHI. This array covers the legal IPHI
C---space as well as a safety region outside it as defined by L2JBUF.
      DO I = 1,NPHIL                    ! Offline indices
        L2J_PHI(I) = I
      END DO
      DO I = 1,NPHIL1                   ! LV1 indices
        L1J_PHI(I) = I
      END DO
      DO I = 1,L2JBUF                   ! Do wrap around
        L1J_PHI(NPHIL1+ I) = L1J_PHI(I)
        L2J_PHI(NPHIL + I) = L2J_PHI(I)
        L1J_PHI(1 - I)     = L1J_PHI(NPHIL1- I + 1)
        L2J_PHI(1 - I)     = L2J_PHI(NPHIL - I + 1)
      END DO
C---do similar thing with IETA...
C---Only the odd arguments make sense here in such as way as the Ith odd
C---argument is meant to map to the Ith IETA. IETA values outside the
C---IETA range are mapped to IETA=0 which also doesnt exist. This will
C---work fine with PTCAEP2. But has to be watched for other uses...
      DO K = -NETAL,NETAL
        IF (K .NE. 0) L2J_ETA(2*K - SIGN(1,K)) = K
      END DO
      DO K = -NETAL1,NETAL1
        IF (K .NE. 0) L1J_ETA(2*K - SIGN(1,K)) = K
      END DO

      DO K = 1,2*L2JBUF
        L2J_ETA(2*NETAL-1 + K) = 0    ! out of bounds: map to IETA=0
        L1J_ETA(2*NETAL1-1+ K) = 0
        L2J_ETA(-2*NETAL+1 - K)= 0
        L1J_ETA(-2*NETAL1+1- K)= 0
      END DO
C
C---For now, we will also fill the sin theta lookup arrays which are kept
C---as a function of IETA.
      DO K = -NETAL,NETAL
        IF (K .NE. 0) THEN
        ETA = .1*K -.05*SIGN(1,K)
        TH  = EXP(-ETA)                 ! TH = TAN(THETA/2)
        TH  = 2*ATAN(TH)
        L2J_SIN(K) = SIN(TH)
        END IF
      END DO

  999 RET_COND = L2J_UTIL_INIT
      RETURN
      END
