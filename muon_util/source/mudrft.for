      REAL FUNCTION MUDRFT(TIME,ANGLE,NMOD)
CC    ======================================================
C     CALCULATE DRIFT DISTANCE GIVEN DRIFT TIME AND ANGLE
C     GIVES DISTANCE FROM WIRE IN CM AT MIDPLANE
C
C      HEDIN 12-6-85
C      DH 10-88 PUT IN TSENG FUNCTION
C      DH 11/88  SETUP FOR MC
C      DH 4/91 SETUP FOR EITHER DATA OR MONTE CARLO
C      DH 6/91 allow for differnet modules: not yet used
C      DH 11-91 update lookup table so closer to D0 PDTs
C      SI 11-91 For MC digitization version 2
C      DH 3/92 USE MDFT
C      DH 7/92 fix overflow bug
C      DH 2/93 simpler function near the wire
C      RM 9/93 Modify to use new time-to-distance function; switches
C              from old function to new if Integer word 3 in MDFT
C              bank is .ge. 2
C      ========================================================
      IMPLICIT NONE
      INTEGER MC,IFIRST,L,GZMUD1,NMOD,IFAST,I,J,K
      REAL TIME,ANGLE,SPEED,VELOCITY,TCOR(42)
      REAL DRIFT,DEL,WIDTH,OVERFLOW
      INTEGER MUDVER,IMVER
      INTRINSIC ABS,SQRT
      REAL MRDRFT
      EXTERNAL MUDVER
      DATA MC,IFIRST/0,0/
      DATA WIDTH/3.15/                    ! HALF PDT CELL WIDTH
      DATA SPEED/200./
      DATA OVERFLOW/ 5.4/
      IF(IFIRST.EQ.0) THEN
        IMVER = MUDVER(0)
        IF(IMVER.GT.0) THEN
          IFIRST=1
          MC = IMVER/10
        ENDIF
      ENDIF
C
      IF(TIME.LT.0.) THEN
        MUDRFT=0.
        RETURN
      ENDIF
C
      IF(MC.EQ.1 .OR. MC.EQ.3) THEN             ! 1A MONTE CARLO
        DRIFT = MRDRFT(TIME,ANGLE*180./3.1415)
        MUDRFT = DRIFT/ABS(COS(ANGLE))
      ELSE                         ! DATA
CC  GET CORRECTIONS
        CALL GTMDFT(NMOD,IFAST,TCOR)
C   Fitted function of measured drift velocities(cm/microsec)
        IF(IFAST.LT.2) THEN      ! old function
C
          IF(TIME.LE.140.) THEN
            VELOCITY=6.5*(1.-.325*ANGLE**2)
C          IF(TIME.LT.100..AND.TIME.GT.25.) THEN
C            VELOCITY=(5.0 + (TIME-25.)*.020)*(1.-.325*ANGLE**2)
C          ELSE IF(TIME.LT.140..AND.TIME.GE.100.) THEN
C            VELOCITY=6.5*(1.-.325*ANGLE**2)
C          ELSE IF(TIME.LE.25.) THEN
C            VELOCITY=5.*(1.-.325*ANGLE**2)
          ELSE
            VELOCITY = (6.5+(TIME-140.)*.001)*(1.-.325*ANGLE**2)
          ENDIF
          DRIFT = VELOCITY*TIME/1000.
C   Position at the mid plane
          DEL = DRIFT/ABS(COS(ANGLE))
C   Fiducial Volume of the cell
          IF(ABS(ANGLE) .LT. 0.357) THEN
            IF(DRIFT .GT. 5.3/ABS(COS(ANGLE))) DEL = OVERFLOW
          ELSE
            IF(DRIFT .GT. 1.9/ABS(SIN(ANGLE))) DEL = OVERFLOW
          ENDIF
CCCC      DO QUICK AND DIRTY CORRECTIONS
          IF(DEL.LT.OVERFLOW) THEN
            L=DEL/.25 + .5
            IF(DEL.LT..125) THEN
              MUDRFT=DEL - TCOR(1)*DEL/.125
            ELSE IF(DEL.GE..125.AND.DEL.LT.5.125) THEN
              MUDRFT=DEL-(TCOR(L)+(TCOR(L+1)-TCOR(L))*
     &          (.125+DEL-L*.25)/.25)
            ELSE IF(DEL.GE.5.125) THEN
              MUDRFT=DEL -TCOR(21)
            ENDIF
          ELSE
            MUDRFT=DEL
          ENDIF
        ELSE
C
C	  Use new time-to-distance function from Minichamber studies
C
          MUDRFT = 0.
          DO I = 1,16
            J = (I-1)/4 + 1
            K = I - (J-1)*4 -1
            IF (K .GT. 0) THEN
              MUDRFT = MUDRFT + TCOR(I)*(TIME**J)*(ANGLE**K)
            ELSE
              MUDRFT = MUDRFT + TCOR(I)*(TIME**J)
            ENDIF
          ENDDO
          IF (MUDRFT .GT. 5.08) MUDRFT = 5.08
          DEL = MUDRFT
CCCC      DO QUICK AND DIRTY CORRECTIONS
          IF(DEL.LT.OVERFLOW) THEN
            L=DEL/.25 + .5
            IF(DEL.LT..125) THEN
              MUDRFT=DEL - TCOR(22)*DEL/.125
            ELSE IF(DEL.GE..125.AND.DEL.LT.5.125) THEN
              MUDRFT=DEL-(TCOR(21+L)+(TCOR(L+22)-TCOR(L+21))*
     &          (.125+DEL-L*.25)/.25)
            ELSE IF(DEL.GE.5.125) THEN
              MUDRFT=DEL -TCOR(42)
            ENDIF
          ELSE
            MUDRFT=DEL
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
