      SUBROUTINE GMD_DRAW_GRID
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw (eta, phi)-grid.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  11-DEC-1991   Harrison B. Prosper
C-   Updated   6-MAY-1993   Harrison B. Prosper
C-   Updated  25-MAY-1993   Marc Paterno  Correct FLINT complaints
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:GM_4VECT.INC'
C----------------------------------------------------------------------
      INTEGER N, J, L
      INTEGER IETA, IPHI, IETA_BINS, IPHI_BINS, JETA_BINS, JPHI_BINS
      INTEGER IETA_OFFSET, IETA_STEP, IETA_SKIP
      INTEGER IPHI_OFFSET, IPHI_STEP, IPHI_SKIP
      LOGICAL GRID_LINES
C
      REAL    PHI_TICK, ETA_TICK, SCALE
      REAL    DX, DY, XT, YT, X, Y, PHIMIN, PHIMAX, ETAMIN, ETAMAX
      REAL    ETARANGE, PHIRANGE, ETA_CHAR, PHI_CHAR
      REAL    DETA, DPHI
C
      CHARACTER*3  GRID_COLOR, LABEL_COLOR
      CHARACTER*80 STRING
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Define parameters for displays
C
        CALL PUGETA('GRID COLOR', GRID_COLOR)
        CALL PUGETA('LABEL COLOR', LABEL_COLOR)
        CALL PUGETV('ETA TICK', ETA_TICK)
        CALL PUGETV('PHI TICK', PHI_TICK)
        CALL PUGETV('ETA CHAR', ETA_CHAR)
        CALL PUGETV('PHI CHAR', PHI_CHAR)
        CALL PUGETV('IETA OFFSET', IETA_OFFSET)
        CALL PUGETV('IETA STEP', IETA_STEP)
        CALL PUGETV('IETA SKIP', IETA_SKIP)
        CALL PUGETV('IPHI OFFSET', IPHI_OFFSET)
        CALL PUGETV('IPHI STEP', IPHI_STEP)
        CALL PUGETV('IPHI SKIP', IPHI_SKIP)
        CALL PUGETV('IETA BINS', IETA_BINS)
        JETA_BINS = IETA_BINS/2
C
        CALL PUGETV('IPHI BINS', IPHI_BINS)
        JPHI_BINS = IPHI_BINS/2
C
        CALL PUGETV('PHIMIN', PHIMIN)
        PHIMIN = PHIMIN*PI
C
        CALL PUGETV('PHIMAX', PHIMAX)
        PHIMAX = PHIMAX*PI
C
        CALL PUGETV('ETAMIN', ETAMIN)
        CALL PUGETV('ETAMAX', ETAMAX)
C
C ****  Set values for labels
C
        ETARANGE  = ETAMAX-ETAMIN
        PHIRANGE  = PHIMAX-PHIMIN
        DETA      = ETARANGE/IETA_BINS
        DPHI      = PHIRANGE/IPHI_BINS
        ETA_TICK  = 0.01*ETA_TICK*ETARANGE
        PHI_TICK  = 0.01*PHI_TICK*PHIRANGE
        ETA_CHAR  = 0.01*ETA_CHAR*ETARANGE
        PHI_CHAR  = 0.01*PHI_CHAR*PHIRANGE
        SCALE     = 180.0/PI
      ENDIF
      CALL PUGETV('ADD GRID LINES', GRID_LINES)
C
C ****  Open retained segment
C
      CALL PUOPEN
      CALL PU_GET_SEGMENT_TYPE(GRID_SEGMENT, SEGTYPE)
C
C ****  Set grid color
C
      CALL PXCOLR(GRID_COLOR)
C
C ****  Draw eta lines
C
      IF ( GRID_LINES ) THEN
        X = ETAMIN - DETA
        DO IETA =  -JETA_BINS, JETA_BINS
          IF ( MOD(IETA+IETA_OFFSET, IETA_STEP) .EQ. 0 ) THEN
            DY = ETA_TICK
          ELSE
            DY = 0.0
          ENDIF
          X = X + DETA
          CALL JMOVE (X, PHIMIN-DY)
          CALL JDRAW (X, PHIMAX+DY)
        ENDDO
C
C ****  Draw phi lines
C
        Y = PHIMIN - DPHI
        DO IPHI =  -JPHI_BINS, JPHI_BINS
          IF ( MOD(IPHI+IPHI_OFFSET, IPHI_STEP) .EQ. 0 ) THEN
            DX = PHI_TICK
          ELSE
            DX = 0.0
          ENDIF
          Y = Y + DPHI
          CALL JMOVE (ETAMIN-DX, Y)
          CALL JDRAW (ETAMAX+DX, Y)
        ENDDO
      ENDIF
C
C ****  Set character size
C
      CALL JSIZE(ETA_CHAR, PHI_CHAR)
      CALL PXCOLR(LABEL_COLOR)
C
C ****  Label eta axis
C
      XT = 0.0
      YT = PHIMIN - ETA_TICK - PHI_CHAR
      CALL JJUST  (CENTER, CENTER)              ! Justify centre, centre
      CALL PUSTRG(XT, YT, 'Eta')
C
      X = ETAMIN - DETA
      J = 0
      DO IETA =  -JETA_BINS, JETA_BINS
        X = X + DETA
        IF ( MOD(IETA+IETA_OFFSET, IETA_STEP) .EQ. 0 ) THEN
          J = J + 1
          IF ( J .NE. IETA_SKIP ) THEN
            N = X + SIGN(0.05, X)
            CALL VNUMI (1, N, '(', ' ', ')', STRING, L)
            CALL PUSTRG(X, YT, STRING(2:L-1))
          ENDIF
        ENDIF
      ENDDO
C
C ****  Label phi axis
C
      XT = ETAMIN - PHI_TICK - ETA_CHAR
      YT = 0.5*PHIRANGE + PHIMIN
      CALL JJUST  (RIGHT, CENTER)
      CALL PUSTRG(XT, YT, 'Phi')
C
C ****  Label phi lines
C
      Y = PHIMIN - DPHI
      J = 0
      DO IPHI =  -JPHI_BINS, JPHI_BINS
        Y = Y + DPHI
        IF ( MOD(IPHI+IPHI_OFFSET, IPHI_STEP) .EQ. 0 ) THEN
          J = J + 1
          IF ( J .NE. IPHI_SKIP ) THEN
            N = SCALE*Y + 0.05
            CALL VNUMI (1, N, '(', ' ', ')', STRING, L)
            CALL PUSTRG(XT, Y, STRING(2:L-1))
          ENDIF
        ENDIF
      ENDDO
      CALL PUCLOSE
      RETURN
      END
