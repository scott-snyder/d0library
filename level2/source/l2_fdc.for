      SUBROUTINE L2_FDC(ETA,PHI,WETA,WPHI,RESULT_FLAG) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Level-2 Tracking for FDC
C-
C-   Inputs  : ETA,PHI,WETA,WPHI
C-   Outputs : RESULT_FLAG = .TRUE. if to pass event, .FALSE. if not
C-   Controls: STATUS = .FALSE. for bad data
C-
C-   Created  2-MAR-1991    YI-CHENG LIU
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      CHARACTER*80 TEXT
      INTEGER HALF,UNIT,QUAD,SECT,WIRE
      INTEGER NPULSE,TPULSE,NWIR
      INTEGER L2FD
      INTEGER I_PASS,I_NOPASS
      INTEGER SLOW_FLAG
      REAL Dcal_S,Dcal_N
      PARAMETER ( Dcal_S = 178.7 , Dcal_N = 169.0 ) ! was 179.7 , make it
C                                                   ! shorter due to 
C                                                   ! mismatch with NFDC 
      REAL ZVTX , L2_VERT
      REAL ETA,PHI,WETA,WPHI
      REAL ETAMIN,ETAMAX,PHIMIN,PHIMAX,THETAMIN,THETAMAX,THETA
      REAL FROAD(1:4)          ! obtained from a call to entry FDC_ROAD
C                              ! in the subroutine L2_ROAD
C!!! Allow 12 candidate sectors in maximum now, but needs to be tuned. !
      INTEGER  NUM_THETA_HIT,NUM_PHI_HIT
      INTEGER  INNER_THETA_HIT, OUTER_THETA_HIT
      INTEGER  NUM_ON_SECTORS,NUM_ON
C
      LOGICAL ON(0:1,0:1,0:7,0:35)        ! ON flag showing if the sector
C                                         ! is within the given road
C!! Still have to find the corresponding crates for the ON sector, could 
C!! save some time in unpacking.
C
      LOGICAL CRITERIA(2)
C ! Decide whether to use delay line information or not, get from RCP.
C
C-( Careful : Shouldn't use the same sector list name for FDC and CDC ! )
C
      LOGICAL RESULT_FLAG, STATUS
C
C -- Prevent L2_FDC being called when not in the FDC region --
C
      IF (ABS(ETA).LT.(1.4)) GOTO 999
C
      RESULT_FLAG = .FALSE.
      INNER_THETA_HIT = 0
      OUTER_THETA_HIT = 0
      NUM_THETA_HIT = 0
      NUM_PHI_HIT = 0
      NUM_ON_SECTORS = 0
      NUM_ON = 0
C
      ZVTX = L2_VERT()
C
      CALL BKL2FD(L2FD)      ! Book filter result bank.
C
C      CALL FDC_ROAD(ETA,PHI,WETA,WPHI,FROAD) ! Returns ROAD limits within FDC
C
       ETAMIN = ETA - WETA
       ETAMAX = ETA + WETA
C
C
C-( Calculate the corrosponding THEMIN, THEMAX from ETAMIN,ETAMAX )
C
      PHIMIN = PHI - WPHI
      PHIMAX = PHI + WPHI
C
C__ New change : specify road width in theta, not in eta.
      THETAMIN = 2.*ATAN(EXP((-1.)*(ETAMAX)))
      THETAMAX = 2.*ATAN(EXP((-1.)*(ETAMIN)))
C
C-( Find FDC sectors along the road, and store them in ON array. )
C-( Also, mark the crates for hit cells, and store in COMMON BLOCK. )
C
C*** Correct THETAMIN,THETAMAX from EC from the fact that ZVTX might 
C*** be off from the detector center. Dcal : distance from EC to center.
C
      IF (ZVTX.EQ.(0.0)) GOTO 777  !! No need to do the following !!
C
      IF (THETAMIN.LT.(PI/2.0)) THEN    ! ( South )
        THETAMIN = ATAN2((Dcal_S*TAN(THETAMIN)),(Dcal_S-ZVTX))
      ELSE                              ! ( North )
        THETAMIN = PI + ATAN2((Dcal_N*TAN(THETAMIN)),(Dcal_N+ZVTX))
      ENDIF
      IF (THETAMAX.LT.(PI/2.0)) THEN    ! ( South )
        THETAMAX = ATAN2((Dcal_S*TAN(THETAMAX)),(Dcal_S-ZVTX))
      ELSE                              ! ( North )
        THETAMAX = PI + ATAN2((Dcal_N*TAN(THETAMAX)),(Dcal_N+ZVTX))
      ENDIF
C
 777  CONTINUE
C
      FROAD(1) = PHIMIN
      FROAD(2) = PHIMAX
      FROAD(3) = THETAMIN
      FROAD(4) = THETAMAX
C
      CALL L2_FLFSEC(ZVTX,PHIMIN, PHIMAX,THETAMIN,THETAMAX,ON,NUM_ON)
C
      CALL L2_FDCUNP(STATUS)          ! Unpack crates for HIT cells only
      IF (.NOT.STATUS) GO TO 999
C
C-( Now, loop through sectors along the road )
C
C**********************************************************************
C
C-( Decide which half to do )
C
      THETA = (THETAMIN + THETAMAX)/2.0
      IF (TAN(THETA).GT.0) THEN 
        HALF = 1                        ! South FDC
      ELSE
        HALF = 0                        ! North FDC
      ENDIF
C
      DO UNIT = 0,1
        IF (UNIT.EQ.0) THEN
C-( for THETA chamber )
          DO QUAD = 0,7
            DO SECT = 0,5
C-( if within the road then do fast hit finding to verify it )
              IF (ON(HALF,UNIT,QUAD,SECT)) THEN
                NUM_ON_SECTORS = NUM_ON_SECTORS + 1
                NPULSE = 0         ! Number of hits in a wire
                TPULSE = 0         ! Total number of hits in a sector
                NWIR = 0           ! Number of wires hit in a sector
                DO WIRE = 0,7      ! Delay Line not included !
                  CALL L2_FDHITS(FROAD,HALF,UNIT,QUAD,SECT,WIRE,
     &                           NPULSE,ZVTX)
                  IF (NPULSE.GT.0) THEN
                    TPULSE = TPULSE + NPULSE
                    NWIR = NWIR + 1
                  ENDIF            
                ENDDO              ! End of wire loop
C
                IF (TPULSE.GE.5.AND.NWIR.GE.4) THEN
                  NUM_THETA_HIT = NUM_THETA_HIT + 1
                  IF (QUAD.GE.0.AND.QUAD.LE.3) THEN
                    INNER_THETA_HIT = INNER_THETA_HIT + 1
                  ELSEIF (QUAD.GE.4.AND.QUAD.LE.7) THEN
                    OUTER_THETA_HIT = OUTER_THETA_HIT + 1
                  ENDIF 
                ELSE
                ENDIF
              ENDIF               
            ENDDO
          ENDDO
        ELSE
C-( for PHI chamber )--------------------------------------------------
          DO SECT = 0,35
C-( if within the road then do fast hit finding to verify it )
            IF (ON(HALF,UNIT,0,SECT)) THEN
              NUM_ON_SECTORS = NUM_ON_SECTORS + 1
              NPULSE = 0           ! Number of hits in a wire
              TPULSE = 0           ! Total number of hits in a sector
              NWIR = 0             ! Number of wires hit in a sector
C
C ** for getting wire position correctly, have to return QUAD to 0 **
C
              QUAD = 0
C
              DO WIRE = 0,15
                CALL L2_FDHITS(FROAD,HALF,UNIT,QUAD,SECT,WIRE,
     &                         NPULSE,ZVTX)
                IF (NPULSE.GT.0) THEN
                  TPULSE = TPULSE + NPULSE
                  NWIR = NWIR + 1
                ENDIF
              ENDDO                ! End of Wire Loop
              IF (TPULSE.GE.12.AND.NWIR.GE.7) THEN
                NUM_PHI_HIT = NUM_PHI_HIT + 1
              ELSE
              ENDIF      
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C-( !!! Decision !!!! )
C**********************************************************************
C********* Filtering criteria *****************************************
C********************************************************************** 
C
      CRITERIA(1) = (INNER_THETA_HIT.GE.1.AND.NUM_PHI_HIT.GE.1)
      CRITERIA(2) = (INNER_THETA_HIT.GE.1).AND.(OUTER_THETA_HIT.GE.1)
     &               .AND.(NUM_PHI_HIT.GE.1)
C
      IF ((ABS(ETA).LT.(1.5)).AND.CRITERIA(1)) THEN      !
C                                ! 2 CRITERIA, DEPENDS ON ETA.
        RESULT_FLAG = .TRUE.
      ELSE IF (CRITERIA(2)) THEN 
        RESULT_FLAG = .TRUE.          ! checking overlaps also !
      ENDIF
C
      IF (RESULT_FLAG) THEN
        I_PASS = 1        !! PASS !!
      ELSE
        I_PASS = 0        !! NO-PASS !!
      END IF
C**********************************************************************
C 
  999 RETURN
      END
