      SUBROUTINE CM3POS_fix(LCASH,WEIGHT_CUT,XBAR3,DBAR3,ETAPHI,DETAPHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates log-weighted center of gravity
C-                         position and dispersion for EM layer 3
C-                         working from CASH bank
C-
C-   Inputs  :  LCASH           link to CASH bank
C-              WEIGHT_CUT      Weighting factor (NIM A311(1992) p.130)
C-
C-   Outputs :  XBAR3(3)        position (x,y,z) of EM 3 shower center
C-              DBAR3(3)        dispersion (x,y,z) of EM3 shower
C-              ETAPHI(3)       position (eta,phi,theta) of EM3 shower
C-              DETAPHI(3)      dispersion (eta,phi,theta) of EM3 shower
C-
C-              Note that one could also use r,phi,z coordinates
C-
C-   Controls: none
C-
C-   Created   6-FEB-1992   Norman A. Graf
C-   Updated  24-MAR-1992   Norman A. Graf  ADDED ETAPHI,DETAPHI 
C-   Updated  20-SEP-1992   Rajendran Raja  REMOVED ZLINKC.INC(LCASH) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CEMPRF.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      REAL XBAR3(3),DBAR3(3),SUMEWT,ECLUS
      REAL ETAPHI(3),DETAPHI(3),E(4),PHI_CELL,THETA_CELL,ETA_CELL
      REAL WT,WEIGHT_CUT,ENERGY
      REAL    XX,YY,ZZ,EM3WTOT
      REAL     CENRAD, DELRAD, CENZED, DELZED, TILT
      INTEGER ARGSOK
      INTEGER I,NCH,POINTER,IOK
      INTEGER  ETAI,PHII,ILYR
      INTEGER PACKED_WORD,LCASH
C
C----------------------------------------------------------------------
C
C ****  TRY NEW EM LAYER 3 X,Y POSITION FINDING HERE...
C
C
C ****  Get energy in layer 3 here...
C
      NCH    = IQ(LCASH+2)
      POINTER=1
      ECLUS = 0
c
      DO I = 1,NCH
        POINTER = POINTER+2
        PACKED_WORD = IQ(LCASH+POINTER)
        ENERGY = Q(LCASH+POINTER+1)
        CALL CAEP_INDICES(PACKED_WORD,ETAI,PHII,ILYR)
        IF(ILYR.GE.LYEM3A .AND. ILYR.LE.LYEM3D) THEN
          ECLUS = ECLUS + ENERGY
        ENDIF
      ENDDO
C
      DO I = 1,3
        XBAR3(I) = 0.
        DBAR3(I) = 0.
        ETAPHI(I)  = 0.
        DETAPHI(I) = 0.
      ENDDO
      SUMEWT = 0.
      POINTER = 1
      IF(ECLUS .GT. 0) THEN
      DO I = 1,NCH
        POINTER = POINTER+2
        PACKED_WORD = IQ(LCASH+POINTER)
        ENERGY = Q(LCASH+POINTER+1)
        CALL CAEP_INDICES(PACKED_WORD,ETAI,PHII,ILYR)
          IF(ILYR.GE.LYEM3A .AND. ILYR.LE.LYEM3D) THEN
            CALL CELXYZ(ETAI,PHII,ILYR,XX,YY,ZZ,IOK)
C
            E(1) = XX
            E(2) = YY
            E(3) = ZZ
            CALL ETOETA(E,PHI_CELL,THETA_CELL,ETA_CELL)
C
            IF(ENERGY .GT. 0) THEN
              WT = WEIGHT_CUT + LOG(ENERGY/ECLUS)
            ELSE
              WT = 0.
            ENDIF
            IF (WT.LT.0.) WT = 0.
            XBAR3(1) = XBAR3(1) + WT*XX
            XBAR3(2) = XBAR3(2) + WT*YY
            XBAR3(3) = XBAR3(3) + WT*ZZ
C
            ETAPHI(1) = ETAPHI(1) + WT*ETA_CELL
            ETAPHI(2) = ETAPHI(2) + WT*PHI_CELL
            ETAPHI(3) = ETAPHI(3) + WT*THETA_CELL
C
            DBAR3(1) = DBAR3(1) + WT*(XX*XX)
            DBAR3(2) = DBAR3(2) + WT*(YY*YY)
            DBAR3(3) = DBAR3(3) + WT*(ZZ*ZZ)
C
            DETAPHI(1) = DETAPHI(1) + WT*ETA_CELL*ETA_CELL
            DETAPHI(2) = DETAPHI(2) + WT*PHI_CELL*PHI_CELL
            DETAPHI(3) = DETAPHI(3) + WT*THETA_CELL*THETA_CELL
C
            SUMEWT = SUMEWT + WT
          ENDIF
        ENDDO
        IF(SUMEWT.GT.0) THEN
          DO I = 1,3
            XBAR3(I) = XBAR3(I)/SUMEWT
            DBAR3(I) = DBAR3(I)/SUMEWT
            DBAR3(I) = DBAR3(I) - XBAR3(I)**2
C
            ETAPHI(I)  = ETAPHI(I)/SUMEWT
            DETAPHI(I) = DETAPHI(I)/SUMEWT
            DETAPHI(I) = DETAPHI(I) - ETAPHI(I)**2
C
          ENDDO
        ENDIF
      ENDIF
  999 RETURN
      END
