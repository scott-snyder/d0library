      SUBROUTINE CONE_ISO(IETAC,ETAC,PHIC,CONE_RAD,E_CONE,
     &  ET_CONE,EM_CONE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sums up energy in 2 cones surrounding
C-                         input eta and phi directions. Used
C-                         for electron isolation cuts.
C-
C-                   BASED ON CONISO_ET
C-
C-   Inputs  : IETAC Detector tower eta index of candidate
C-             ETAC Eta of center of cone
C-             PHIC Phi of center of cone
C-             CONE_RAD(5) array of cone radii, including core
C-             normally .7, .6, .4, .2
C- NOTE: CONES MUST GO FROM SMALLEST TO LARGEST
C-
C-   Outputs : E_CONE(5)  Energy contained in cone
C-             ET_CONE(5) Et     contained in cone
C-
C-   Controls: NONE
C-
C-   CREATED  10-DEC-1993   Paul Rubinov
C-            BASED ON Norman A. Graf
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
      REAL CONE_RAD(5),E_CONE(5),ET_CONE(5),EM_CONE(5)
      REAL CONE_RAD2(5),CONE_RAD_MAX
      REAL E(4)
      INTEGER IETAC,POINTEM
      INTEGER IETAHI,IETALO,IPHIHI,IPHILO,JPHI
      INTEGER I,J,K,L
      INTEGER GZCATE,LCATE,NREP,POINT
C----------------------------------------------------------------------
C
      CONE_RAD_MAX=0.
      DO I = 1,5
        CONE_RAD2(I)= CONE_RAD(I)**2
        E_CONE(I) = 0.
        ET_CONE(I) = 0.
        EM_CONE(I)= 0.
        IF (CONE_RAD(I).GT.CONE_RAD_MAX) CONE_RAD_MAX=CONE_RAD(I)
      ENDDO
      LCATE = GZCATE()
      NREP = IQ(LCATE+2)
C
C               CALCULATE ETA, PHI LIMITS FOR CONE
C
      IETAHI = INT(IETAC+CONE_RAD_MAX*10.) + 5
      IETAHI = MIN(NETAL,IETAHI)
      IETALO = INT(IETAC-CONE_RAD_MAX*10.) - 5
      IETALO = MAX(-NETAL,IETALO)
      IPHIHI = INT((PHIC+CONE_RAD_MAX)*64./TWOPI) + 2
      IPHILO = INT((PHIC-CONE_RAD_MAX)*64./TWOPI) - 2
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
          POINTEM = PTCATE(I,JPHI,1)
          IF(POINTEM.NE.0) POINTEM = NREP*(POINTEM-1) + LCATE
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
          DO 10 L = 1 ,5
            IF (R2TMP.LE.CONE_RAD2(L)) THEN
              E_CONE(L) = Q(POINT+7) + E_CONE(L)
              ET_CONE(L) = Q(POINT+8) + ET_CONE(L)
              IF (POINTEM.NE.0) EM_CONE(L) = Q(POINTEM+7) + EM_CONE(L)
            ENDIF
   10     CONTINUE
C
   40   CONTINUE
   50 CONTINUE
  999 RETURN
      END
