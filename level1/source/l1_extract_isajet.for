      SUBROUTINE L1_EXTRACT_ISAJET()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Computes Isajet quantities, N, PX, PY, PZ
C-                         E, MPX, MPY, P, Pt, Phi, Theta and Mpt
C-                         finds out Vertex position
C-                         from the ISAJET bank.
C-
C-   Inputs  : ISAJET event banks.
C-   Outputs : raw event common block variables
C-   Controls: none
C-
C-   Created  21-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          - Moved this processing here from L1C_FILL_ONE_EVENT
C-                          - Replaced D0$INC:TRG_SIMUL_RAW_EVENT.INC with
C-                            D0$INC:L1C_EVENT_RAW.INC 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:L1C_EVENT_RAW.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER  GZISAE, GZISV1, GZCAEP, GZCAD1, GZCAD2, TRULEN
      EXTERNAL GZISAE, GZISV1, GZCAEP, GZCAD1, GZCAD2, TRULEN
C
      INTEGER I, LSV1, PNTR, PARTICLE
C
      INTEGER    NU_E,      NU_MU,      NU_TAU
      PARAMETER (NU_E = 11, NU_MU = 13, NU_TAU = 15)
C
C     Isajet informations
C     ===================
C
      ISAJET_ID = 0
      ISAJET_CS = 0.
      ISAJET_WG = 1.
      Z_VERTEX  = 0.
      ISAJET_N  = 0
      PNTR = GZISAE()
      IF(PNTR.NE.0) THEN
        ISAJET_ID = IQ(PNTR+4)
        ISAJET_CS =  Q(PNTR+11)
        ISAJET_WG =  Q(PNTR+12)
        LSV1 = GZISV1()
        IF(LSV1.NE.0) THEN
          Z_VERTEX   = Q(LSV1+9)
          ISAJET_PX  = 0.
          ISAJET_PY  = 0.
          ISAJET_PZ  = 0.
          ISAJET_E   = 0.
          ISAJET_MPX = 0.
          ISAJET_MPY = 0.
   50     PNTR = LQ(LSV1-IZISP1)
          DO WHILE (PNTR.NE.0)
            ISAJET_N   = ISAJET_N  + 1
            ISAJET_PX  = ISAJET_PX + Q(PNTR+2)
            ISAJET_PY  = ISAJET_PY + Q(PNTR+3)
            ISAJET_PZ  = ISAJET_PZ + Q(PNTR+4)
            ISAJET_E   = ISAJET_E  + SQRT(Q(PNTR+5)**2 + Q(PNTR+6)**2)
            PARTICLE   = IABS(IQ(PNTR+1))
            IF((PARTICLE.EQ.NU_E).OR.
     +                 (PARTICLE.EQ.NU_MU).OR.
     +                         (PARTICLE.EQ.NU_TAU) ) THEN
              ISAJET_MPX = ISAJET_MPX + Q(PNTR+2)
              ISAJET_MPY = ISAJET_MPY + Q(PNTR+3)
            ENDIF
            PNTR = LQ(PNTR)
          ENDDO
          LSV1 = LQ(LSV1)
          IF(LSV1.NE.0) GOTO 50
          ISAJET_PT = ISAJET_PX**2 + ISAJET_PY**2
          ISAJET_P  = SQRT (ISAJET_PT + ISAJET_PZ**2)
          ISAJET_PT = SQRT (ISAJET_PT)
          IF(ISAJET_P.GT.0.1) THEN
            ISAJET_PHI = ATAN2 (ISAJET_PY, ISAJET_PX)
            IF (ISAJET_PHI.LT.0.) ISAJET_PHI = ISAJET_PHI + TWOPI
            ISAJET_THETA = ATAN2 (ISAJET_PT, ISAJET_PZ)
          ELSE
            ISAJET_PHI   = 0.
            ISAJET_THETA = 0.
          ENDIF
          ISAJET_MPT  = SQRT(ISAJET_MPX**2 + ISAJET_MPY**2)
          IF (ISAJET_MPT.NE.0.) THEN
            ISAJET_MPT_PHI = ATAN2 (ISAJET_MPY, ISAJET_MPX)
            IF (ISAJET_MPT_PHI.LT.0.)
     +                   ISAJET_MPT_PHI = ISAJET_MPT_PHI + TWOPI
          ELSE
            ISAJET_MPT_PHI = 0.
          ENDIF
        ENDIF
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
