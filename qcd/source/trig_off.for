      SUBROUTINE TRIG_OFF(OFF_EM,OFF_HAD,CAL_ET_CHAN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : NONE
C-   Outputs : off_em(L1_phi, L1_eta), off_had(L1_phi,L1_eta)
C-   Controls:
C-
C-   Created  11-MAR-1992   Nicholas Hadley
C-   EDITTED  17-APR-1992   KATHY STREETS
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
C
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LCAEH, GZCAEH, NREP, L1_ETA,L1_PHI
      INTEGER IC_ETAL, IC_ETAH, IC_PHIL, IC_PHIH
      INTEGER IEC, IPC, IL, IPNT, ICC, ICH
C
      REAL OFF_EM(NPHIL1, -NETAL1:NETAL1)
      REAL OFF_HAD(NPHIL1,-NETAL1:NETAL1)
      REAL CAL_ET_CHAN(NPHIL1,-NETAL1:NETAL1,56)
C
      CHARACTER*80 MSG_STRING
C----------------------------------------------------------------------
C       FIND CAEH BANK
C
      LCAEH = GZCAEH()
      IF (LCAEH.LE.0) GOTO 999
      NREP = IQ(LCAEH+2)
C
C       MAIN LOOP OVER ALL L1 CALORIMETER TOWERS (ETA,PHI)
C
      DO 500 L1_ETA = -NETAL1,NETAL1
C       SET UP ETA POINTERS TO OFFLINE TOWERS
C
        IF (ABS(L1_ETA).GT.16) THEN
          IF (L1_ETA.EQ.17) IC_ETAL = ISIGN(33,L1_ETA)
          IF (L1_ETA.EQ.18) IC_ETAL = ISIGN(34,L1_ETA)
          IF (L1_ETA.EQ.19) IC_ETAL = ISIGN(35,L1_ETA)
          IF (L1_ETA.EQ.20) IC_ETAL = ISIGN(36,L1_ETA)
          IC_ETAH = IC_ETAL
        ELSE
          IF(L1_ETA.GT.0) THEN
            IC_ETAH = L1_ETA*2
            IC_ETAL = IC_ETAH - 1
          ELSE
            IC_ETAL = L1_ETA*2
            IC_ETAH = IC_ETAL + 1
          END IF
        END IF
C
        DO 400 L1_PHI = 1,NPHIL1
          OFF_EM(L1_PHI,L1_ETA) = 0.
          OFF_HAD(L1_PHI,L1_ETA) = 0.
          DO 375 ICH=1,56
            CAL_ET_CHAN(L1_PHI,L1_ETA,ICH)=0.
  375     CONTINUE
          IC_PHIH = L1_PHI*2              ! PHI OFFLINE TOWERS
          IC_PHIL = IC_PHIH - 1
          ICC=0
          DO 300 IEC = IC_ETAL,IC_ETAH        ! ETA OFFLINE LOOP
            DO 200 IPC = IC_PHIL,IC_PHIH      ! PHI OFFLINE LOOP
              DO 100 IL = 1, 7                  ! LOOP OVER EM LAYERS
                ICC=ICC+1
                IF (ICC.GT.56)THEN
                   WRITE(MSG_STRING,1001)
                   CALL INTMSG(MSG_STRING)
                   ICC=56
                ENDIF
                IF(PTCAEP(IEC,IPC,IL).NE.0) THEN
                  IPNT = (PTCAEP(IEC,IPC,IL)-1)*NREP + LCAEH + 8
                  OFF_EM(L1_PHI,L1_ETA) = OFF_EM(L1_PHI,L1_ETA) +
     &              Q(IPNT)
                  CAL_ET_CHAN(L1_PHI,L1_ETA,ICC)=Q(IPNT)
                END IF
  100         CONTINUE
C
              DO 75 IL = 11, 14                ! LOOP OVER FH LAYERS
                ICC=ICC+1
                IF (ICC.GT.56)THEN
                   WRITE(MSG_STRING,1001)
                   CALL INTMSG(MSG_STRING)
                   ICC=56
                ENDIF
                IF (PTCAEP(IEC,IPC,IL).NE.0) THEN
                  IPNT = (PTCAEP(IEC,IPC,IL)-1)*NREP + LCAEH + 8
                  OFF_HAD(L1_PHI,L1_ETA) = OFF_HAD(L1_PHI,L1_ETA) +
     &              Q(IPNT)
                  CAL_ET_CHAN(L1_PHI,L1_ETA,ICC)=Q(IPNT)
                END IF
   75         CONTINUE
C
              DO 50 IL = 15,17                ! LOOP OVER CH LAYERS
                ICC=ICC+1
                IF (ICC.GT.56)THEN
                   WRITE(MSG_STRING,1001)
                   CALL INTMSG(MSG_STRING)
                   ICC=56
                ENDIF
                IF (PTCAEP(IEC,IPC,IL).NE.0) THEN
                  IPNT = (PTCAEP(IEC,IPC,IL)-1)*NREP + LCAEH + 8
                  CAL_ET_CHAN(L1_PHI,L1_ETA,ICC)=Q(IPNT)
                END IF
   50         CONTINUE
C
  200       CONTINUE
  300     CONTINUE
  400   CONTINUE
  500 CONTINUE
C
 1001 FORMAT(' TRIG_OFF ERROR!, ICC.GT.56, SET TO 56!')
  999 RETURN
      END
C*******************************************************************
