      SUBROUTINE PJSPLIT(SPLMG,DR_CUT,NJET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do splitting/merging of newly found jet NJET
C-                         according to parameter SPLMG.
C-
C-   Inputs  : SPLMG,NJET,DR_CUT
C-   Outputs :
C-   Controls:
C-
C-   Created  13-NOV-1992   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:USER_PJET.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER NJET,IJET,NJ1,IDR,I,J,IJET2,NJ2,IJET3,NJ3,IP
      REAL SPLMG,MIN_DR,TH,ETA1,ETA2,ETA3,PHI1,PHI2,PHI3,DPHI,DR,EKS_DR
      REAL SHARE(4),SHARE_ET,FACTOR,ET1,ET2,DR_PR,DR1,DR2,ET_FRAC
      REAL DR_CUT
      LOGICAL DONE,TESTED(NJMAX)
C----------------------------------------------------------------------
      DONE = .FALSE.
C
C  SPLMG :  0 - NO SPLITTING/MERGING
C          >0 - USE EKS CRITERIA, radius = -SPLMG
C          <0 - USE SHARED ET FRACTION = SPLMG
C
      IF (SPLMG.EQ.0.) GOTO 999
      DO I = 1, NJET-1
        TESTED(I) = .FALSE.
      ENDDO
      DO WHILE (.NOT.DONE.AND.NJET.GT.1)
C
C    FIRST GET CLOSEST PREVIOUSLY FOUND JET
C
        IJET = INT(FLOAT(NJET)/32.) + 1
        NJ1 = NJET - (IJET-1)*32
        MIN_DR = 100.
        IDR = 0
        CALL ISPETA(P_JET(1,NJET),TH,PHI1,ETA1)
        DO I = 1, NJET-1
          IF (.NOT.TESTED(I)) THEN
            CALL ISPETA(P_JET(1,I),TH,PHI2,ETA2)
            DPHI = ABS(PHI2-PHI1)
            IF (DPHI.GT.PI) DPHI = 2*PI-DPHI
            DR = SQRT((ETA2-ETA1)**2+DPHI**2)
            IF (DR.LT.MIN_DR) THEN
              MIN_DR = DR
              IDR = I
            ENDIF
          ENDIF
        ENDDO
C
C  CALCULATE E,ET SHARED WITH NEAREST PREVIOUSLY FOUND JET
C
        SHARE(1) = 0.
        SHARE(2) = 0.
        SHARE(3) = 0.
        SHARE(4) = 0.
        SHARE_ET = 0.
        IF (IDR.NE.0 .AND. MIN_DR.LE.(2.*DR_CUT)) THEN
          IJET2 = INT(FLOAT(IDR)/32.) + 1
          NJ2 = IDR - (IJET2-1)*32
          DO IP = 1, NP
            IF (BTEST(JETMAP(IJET,IP),NJ1) .AND.
     &        BTEST(JETMAP(IJET2,IP),NJ2)) THEN
              CALL ISPETA(P_PART(1,IP),TH,PHI2,ETA2)
              SHARE_ET = SHARE_ET + P_PART(4,IP)*SIN(TH)
              DO I = 1, 4
                SHARE(I) = SHARE(I) + P_PART(I,IP)
              ENDDO
            ENDIF
          ENDDO
        ENDIF
        IF (SPLMG.GT.0) THEN
          EKS_DR = SPLMG
C
C   EKS CRITERIA
C
          IF (IDR.NE.0 .AND. MIN_DR.LT.2*EKS_DR) THEN
            CALL ISPETA(P_JET(1,IDR),TH,PHI2,ETA2)
            FACTOR = 1. +
     &          SHARE_ET/(ET_JET(NJET)+ET_JET(IDR)-2.*SHARE_ET)
            ET1 = (ET_JET(NJET)-SHARE_ET)*FACTOR
            ET2 = (ET_JET(IDR)-SHARE_ET)*FACTOR
            DR_PR = EKS_DR*(ET1 + ET2)
     &          /MAX(ET1,ET2)
            IF (MIN_DR.LT.DR_PR) GOTO 210          ! MERGE
            GOTO 220                               ! SPLIT
          ELSE
            DONE = .TRUE.
          ENDIF
        ELSE                    ! SHARED ET FRACTION
          IF (MIN_DR.LE.2.*DR_CUT) THEN
            ET_FRAC = SHARE_ET/MIN(ET_JET(NJET),ET_JET(IDR))
            IF (ET_FRAC.GT.-SPLMG) GOTO 210           ! MERGE
            GOTO 220                                 ! SPLIT
          ELSE
            DONE = .TRUE.
          ENDIF
        ENDIF
        GOTO 100
  210   CONTINUE
C
C   MERGE JETS: ADD P,ET AND SUBTRACT OVERLAP REGION. ZERO JET #IDR
C
        IF (ISHARE(NJET).NE.0) THEN
          ISHARE(NJET) = 3
        ELSE
          ISHARE(NJET) = 1
        ENDIF
        ET_JET(NJET) = ET_JET(NJET) + ET_JET(IDR)
        ET_JET(IDR) = 0.
        DO I = 1, 4
          P_JET(I,NJET) = P_JET(I,NJET) + P_JET(I,IDR)
          P_JET(I,IDR) = 0.
        END DO
        DO IP = 1, NP
          IF (BTEST(JETMAP(IJET,IP),NJ1) .AND.
     &                BTEST(JETMAP(IJET2,IP),NJ2)) THEN
            CALL ISPETA(P_PART(1,IP),TH,PHI2,ETA2)
            ET_JET(NJET) = ET_JET(NJET) - P_PART(4,IP)*SIN(TH)
            DO I = 1, 4
              P_JET(I,NJET)= P_JET(I,NJET) - P_PART(I,IP)
            END DO
          ENDIF
          IF (BTEST(JETMAP(IJET2,IP),NJ2)) THEN
            JETMAP(IJET,IP) = IBSET(JETMAP(IJET,IP),NJ1)
            JETMAP(IJET,IP) = IBCLR(JETMAP(IJET2,IP),NJ2)
          ENDIF
        ENDDO
C
C     SHIFT JET INFO DOWN 1 INDEX. GO BACK TO START WITH NJET = NEW,
C     MERGED JET.
C
        DO J = IDR, NJET - 1
          ISHARE(J) = ISHARE(J+1)
          ISHARE(J+1) = 0
          ET_JET(J) = ET_JET(J+1)
          ET_JET(J+1) = 0.
          DO I = 1, 4
            P_JET(I,J) = P_JET(I,J+1)
            P_JET(I,J+1) = 0.
          ENDDO
          IJET2 = INT(FLOAT(J)/32.) + 1
          NJ2 = J - (IJET2-1)*32
          IJET3 = INT(FLOAT(J+1)/32.) + 1
          NJ3 = J+1 - (IJET2-1)*32
          DO IP = 1, NP
            IF (BTEST(JETMAP(IJET3,IP),NJ3)) THEN
              JETMAP(IJET2,IP) = IBSET(JETMAP(IJET2,IP),NJ2)
              JETMAP(IJET3,IP) = IBCLR(JETMAP(IJET3,IP),NJ3)
            ENDIF
          ENDDO
          TESTED(J) = TESTED(J+1)
          TESTED(J+1) = .FALSE.
        ENDDO
        NJET = NJET - 1
        GOTO 100
C
  220   CONTINUE
C
C   SPLIT JETS: DIVIDE OVERLAP REGION ACCORDING TO CLOSEST CENTROID
C
        IF (SHARE_ET.GT.0.) THEN
          CALL ISPETA(P_PART(1,IDR),TH,PHI2,ETA2)
          DO IP = 1, NP
            IF (BTEST(JETMAP(IJET,IP),NJ1) .AND.
     &                  BTEST(JETMAP(IJET2,IP),NJ2)) THEN
              CALL ISPETA(P_PART(1,IP),TH,PHI3,ETA3)
              DPHI = ABS(PHI3-PHI1)
              IF (DPHI.GT.PI) DPHI = 2*PI-DPHI
              DR1 = SQRT((ETA3-ETA1)**2+DPHI**2)
              DPHI = ABS(PHI3-PHI2)
              IF (DPHI.GT.PI) DPHI = 2*PI-DPHI
              DR2 = SQRT((ETA3-ETA2)**2+DPHI**2)
              IF (DR2.LT.DR1) THEN
                JETMAP(IJET,IP) = IBCLR(JETMAP(IJET,IP),NJ1)
                ET_JET(NJET) = ET_JET(NJET)
     &                     - P_PART(4,IP)*SIN(TH)
                DO J = 1, 4
                  P_JET(J,NJET) = P_JET(J,NJET) - P_PART(J,IP)
                ENDDO
              ELSE
                JETMAP(IJET2,IP) = IBCLR(JETMAP(IJET2,IP),NJ2)
                ET_JET(IDR) = ET_JET(IDR) - P_PART(4,IP)*SIN(TH)
                DO J = 1, 4
                  P_JET(J,IDR) = P_JET(J,IDR) - P_PART(J,IP)
                ENDDO
              ENDIF
            ENDIF
          ENDDO
          IF (ISHARE(NJET).NE.0) THEN
            ISHARE(NJET) = 3
          ELSE
            ISHARE(NJET) = 2
          ENDIF
          IF (ISHARE(IDR).NE.0) THEN
            ISHARE(IDR) = 3
          ELSE
            ISHARE(IDR) = 2
          ENDIF
        ENDIF
        TESTED(IDR) = .TRUE.
C
  100   CONTINUE
      ENDDO
C
  999 RETURN
      END
