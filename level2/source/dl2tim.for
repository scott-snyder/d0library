      SUBROUTINE DL2TIM(SECTOR,TIM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-APR-1991   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ILYR,ISEC,IWIR,CLOWIR
      INTEGER TPULSE(2),NPULSE
      INTEGER I,K1,K2,J,SECTOR(0:3)
      INTEGER IDL,LD
C
      REAL HITLST(5,0:2,0:15),TIM(8,2)
      REAL VALUE(2),SWTIME(0:3,0:6),LENGTH(2)
      REAL DL1,DL2,DLT0(2),SVEL,SLEN,VEL(2),SUM
      REAL DZ(5,2),BSTSM1,BSTSM2,DIFF1,DIFF2
C
C----------------------------------------------------------------------
C
C  Determine drift time to sense wire CLOsest to Delay line
C
      DO ILYR = 0,3
        ISEC = SECTOR(ILYR)
        CLOWIR = 0
        DO J = 0,1
          CALL DFSTRK(ILYR,ISEC,CLOWIR,NPULSE,HITLST)
          IF (NPULSE.EQ.1) THEN
            SWTIME(ILYR,CLOWIR) = HITLST(1,0,CLOWIR)
            CALL DGTTMS(0,ILYR,ISEC,CLOWIR,VALUE,SVEL,SLEN,3)
            IF (VALUE(1).LE.300.OR.VALUE(1).GT.350.) VALUE(1) = 325.
            SWTIME(ILYR,CLOWIR) = SWTIME(ILYR,CLOWIR) - VALUE(1)
          ELSE
            SWTIME(ILYR,CLOWIR) = 0.
          ENDIF
          CLOWIR = 6
        ENDDO
      ENDDO
C
C  Loop over all delay lines and extract timing.
C          
      DO I = 1,8
        ILYR = ((I-1)/2)
        ISEC = SECTOR(ILYR)
        CLOWIR = 0
        IF (MOD(I,2) .EQ. 0) CLOWIR = 6
        TPULSE(1) = 0
        TPULSE(2) = 0
        DO J = 1,2
          IF (J .EQ. 1) THEN            ! North DLs
             IF (MOD(I,2) .EQ. 1) IWIR=7         ! if I is odd
             IF (MOD(I,2) .EQ. 0) IWIR=9         ! if I is even
           ENDIF
          IF (J .EQ. 2) THEN            ! South DLs
            IF (MOD(I,2) .EQ. 1) IWIR=8         ! if I is odd
            IF (MOD(I,2) .EQ. 0) IWIR=10        ! if I is even
          ENDIF
          CALL DFSTRK(ILYR,ISEC,IWIR,NPULSE,HITLST)
          TPULSE(J) = NPULSE
          CALL DGTTMS(0,ILYR,ISEC,IWIR,VALUE,VEL(J),LENGTH(J),3)
          IF (VALUE(1).LE.300.OR.VALUE(1).GT.350.) VALUE(1) = 325.
          DLT0(J) = VALUE(1)
        ENDDO
C
        TIM(I,1) = 0.
        TIM(I,2) = 0.
        IF (TPULSE(1).EQ.0.OR.TPULSE(2).EQ.0) GO TO 5
        LD = 0
        DO K1 = 1,TPULSE(1)
          DO K2 = 1,TPULSE(2)
            IF (K2.LT.K1) GO TO 2
            IF (MOD(I,2).EQ.1) THEN             
              IF (SWTIME(ILYR,0).LE.0.) GO TO 5
              DL1 = HITLST(K1,0,7) - DLT0(1) - SWTIME(ILYR,0)
              DL2 = HITLST(K1,0,8) - DLT0(2) - SWTIME(ILYR,0)
              SUM = DL1 + DL2
              IF (SUM.GE.700.AND.SUM.LE.925.) THEN
                LD = LD + 1
                DZ(LD,1) = DL1
                DZ(LD,2) = DL2
                IF (LD.GE.5) GO TO 10
              ENDIF
            ELSE
              IF (SWTIME(ILYR,6).LE.0.) GO TO 5
              DL1 = HITLST(K1,0,9)  - DLT0(1) - SWTIME(ILYR,6)
              DL2 = HITLST(K1,0,10) - DLT0(2) - SWTIME(ILYR,6)
              SUM = DL1 + DL2
              IF (SUM.GE.700.AND.SUM.LE.925.) THEN
                LD = LD + 1
                DZ(LD,1) = DL1
                DZ(LD,2) = DL2
                IF (LD.GE.5) GO TO 10
              ENDIF
            ENDIF
    2       CONTINUE
          ENDDO
        ENDDO
C
   10   CONTINUE
        BSTSM1 = 10000.
        BSTSM2 = 10000.
        DO IDL = 1,LD
          DIFF1 = DZ(LD,1) - LENGTH(1)
          DIFF2 = DZ(LD,2) - LENGTH(2)
          IF (ABS(DIFF1).LT.BSTSM1) THEN
            TIM(I,1) = DZ(LD,1)*VEL(1)
            BSTSM1 = ABS(DIFF1)
          ENDIF
          IF (ABS(DIFF2).LT.BSTSM2) THEN
            TIM(I,2) = DZ(LD,2)*VEL(2)
            BSTSM2 = ABS(DIFF2)
          ENDIF
        ENDDO
C
    5   CONTINUE
      ENDDO
C
  999 RETURN
      END
