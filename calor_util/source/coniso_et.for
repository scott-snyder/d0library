      SUBROUTINE CONISO_ET(IETAC,ETAC,PHIC,CONRAD1,CONRAD2,EISUM,EOSUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sums up energy in 2 cones surrounding
C-                         input eta and phi directions. Used
C-                         for electron isolation cuts.
C-
C-                   ALMOST THE SAME AS CONISO
C-
C-   Inputs  : IETAC Detector tower eta index of candidate
C-             ETAC Eta of center of cone
C-             PHIC Phi of center of cone
C-
C-   Outputs : EISUM(1) Energy contained in inner cone 
C-             EISUM(2) Et     contained in inner cone 
C-                    
C-             EOSUM(1) Energy contained in outer cone 
C-             EOSUM(2) Et     contained in outer cone 
C-
C-   Controls: NONE
C-
C-   Created  15-DEC-1992   Norman A. Graf
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$LINKS:IZCATE.LINK'
      INCLUDE 'D0$INC:PTCATE.INC'
      INCLUDE 'D0$INC:PTCAEP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      REAL ETAC,PHIC,ETATMP,PHITMP,THETA,R2TMP
      REAL CONRAD1,CN1RAD2,CONRAD2,CN2RAD2
      REAL E(4),EISUM(2),EOSUM(2)
      INTEGER IETAC
      INTEGER IETAHI,IETALO,IPHIHI,IPHILO,JPHI,I,J,K
      INTEGER GZCATE,LCATE,NREP,POINT
C----------------------------------------------------------------------
      CN1RAD2=CONRAD1*CONRAD1
      CN2RAD2=CONRAD2*CONRAD2
C
      DO I = 1,2
        EISUM(I) = 0.
        EOSUM(I) = 0.
      ENDDO
      LCATE = GZCATE()
      NREP = IQ(LCATE+2)
C
C               CALCULATE ETA, PHI LIMITS FOR CONE
C
      IETAHI = INT(IETAC+CONRAD2*10.) + 3
      IETAHI = MIN(NETAL,IETAHI)
      IETALO = INT(IETAC-CONRAD2*10.) - 3
      IETALO = MAX(-NETAL,IETALO)
      IPHIHI = INT((PHIC+CONRAD2)*64./TWOPI) + 2
      IPHILO = INT((PHIC-CONRAD2)*64./TWOPI) - 2
C
C               LOOP OVER THE POSSIBLE TOWERS FOR THIS CONE
C
      DO 50 I = IETALO , IETAHI
        DO 40 J = IPHILO , IPHIHI
          JPHI = J
          IF (J.GE.65) JPHI = MOD(J,64)
          IF (J.LE.0)  JPHI = 64 + J
          POINT = PTCATE(I,JPHI,2)
          IF (POINT.EQ.0) GOTO 40           ! NO TOWER
          POINT = NREP*(POINT-1) + LCATE
          DO 20 K = 1 ,4
            E(K) = Q(POINT+3+K)
   20     CONTINUE
          CALL ETOETA(E,PHITMP,THETA,ETATMP)
          IF (ABS(PHITMP-PHIC).GT.(TWOPI-ABS(PHITMP-PHIC))) THEN
            IF(PHIC.LT.PHITMP) THEN
              PHITMP = PHITMP - TWOPI
            ELSE
              PHITMP = PHITMP + TWOPI
            END IF
          END IF
          R2TMP = (ETATMP-ETAC)**2 + (PHITMP-PHIC)**2
          IF (R2TMP.GT.CN2RAD2) GOTO 40 !POINT OUTSIDE ISOL CONE
C
          IF (R2TMP.GT.CN1RAD2) GOTO 30 !POINT OUTSIDE CORE CONE
          EISUM(1) =  Q(POINT+7) + EISUM(2)    ! E
          EISUM(2) =  Q(POINT+8) + EISUM(2)    ! ET
   30     CONTINUE
          EOSUM(1) =  Q(POINT+7) + EOSUM(2)    ! E 
          EOSUM(2) =  Q(POINT+8) + EOSUM(2)    ! ET
   40   CONTINUE
   50 CONTINUE
  999 RETURN
      END
