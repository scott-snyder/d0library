      SUBROUTINE HOT_CELL_IN_JET(NJFOUND,HOTEX,HOTEY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : match CAID hot_cells to jet
C-
C-   Returned value  : NJFOUND : number of hot cells in jet
C-                     HOTEX   : EX of HOT cells in jet
C-                     HOTEY   : EY of HOT cells in jet
C-   Inputs  : 
C-   Outputs :
C-   Controls:
C-
C-   Created  18-JUL-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER PACKED_ADDRESS
      BYTE BYTES(4)
      EQUIVALENCE (PACKED_ADDRESS,BYTES)
      REAL    XC,YC,ZC,XV,YV,ZV,DIST,E,EX,EY
      REAL    HOTEX,HOTEY,RR,XX,YY,ZZ,ETA_HOT,PHI_HOT,THETA
      INTEGER LVERT,GZVERT,I,NFOUND
      INTEGER LCAID,GZCAID,LPOINT,IETA,IPHI,LAYER,IOK
      INTEGER NMATCH,LJET_MATCH,JET_ALG,NJFOUND
      LOGICAL MATCH
C----------------------------------------------------------------------
      NFOUND = 0
      HOTEX=0
      HOTEY=0
      NJFOUND = 0
C
C ****  Loop over all hot cells
C
      LCAID=GZCAID()
      IF(LCAID.NE.0) THEN
        NFOUND=IQ(LCAID+4)
        LPOINT=LCAID+7
        IF(NFOUND.GT.0) THEN
          LVERT=GZVERT(1)
          XV=Q(LVERT+3)
          YV=Q(LVERT+4)
          ZV=Q(LVERT+5)
          DO I=1,NFOUND
            PACKED_ADDRESS=IQ(LPOINT+1)
            E=Q(LPOINT+2)
            IETA = BYTES(BYTE4)
            IPHI = BYTES(BYTE3)
            LAYER= BYTES(BYTE2)
            IF(IETA*IPHI*LAYER*E.NE.0) THEN  ! PROTECT AGAINST GARBAGE
              CALL CELXYZ(IETA,IPHI,LAYER,XC,YC,ZC,IOK)
              DIST=SQRT((XC-XV)**2+(YC-YV)**2+(ZC-ZV)**2)
              EX=E*XC/DIST
              EY=E*YC/DIST
              JET_ALG = 2
              XX = XC - XV 
              YY = YC - YV
              ZZ = ZC - ZV 
              IF (YY.EQ.0.) THEN
                PHI_HOT = PI * ( -SIGN(0.5, XX) + 0.5 )
              ELSE
                PHI_HOT = ATAN2(YY,XX)
                IF (PHI_HOT.LT.0.) PHI_HOT = PHI_HOT + 2.* PI
              END IF
              RR = SQRT(XX**2 + YY**2 + ZZ**2)
              THETA = ACOS( ZZ / RR )
              ETA_HOT = -LOG(TAN( 0.5 * THETA ))
C ****  check if cell in JET
              CALL MATCH_JET_HOTCELL(JET_ALG,ETA_HOT,PHI_HOT,MATCH,
     &          NMATCH,LJET_MATCH)
              IF (MATCH) THEN
                NJFOUND = NJFOUND + 1
                HOTEX=HOTEX+EX
                HOTEY=HOTEY+EY
              ENDIF
            ENDIF
            LPOINT=LPOINT+2
          ENDDO
        ENDIF
      ENDIF
  999 RETURN
      END
C==============================================================================
      SUBROUTINE MATCH_JET_HOTCELL(JET_ALG,HOT_ETA,HOT_PHI,MATCH,NMATCH,
     &  LJET_MATCH)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'd0$inc:zebcom.inc'
      INCLUDE 'd0$inc:PI.DEF'
      INTEGER JET_ALG,LJETS,GZJETS
      INTEGER NMATCH,LJET_MATCH
      REAL    JET_ETA,JET_PHI,HOT_PHI,HOT_ETA,DELTA_PHI,DELTA_R
      LOGICAL MATCH
C----------------------------------------------------------------------
      CALL SET_CAPH_ALG(JET_ALG)
      LJETS = GZJETS()
      CALL ZSORT(IXCOM,LJETS,6)
      LJETS = GZJETS()
      CALL ZTOPSY(IXCOM,LJETS)
      LJETS = GZJETS()
      NMATCH  = 0
      JET_ETA = 0.
      JET_PHI = 0.
      MATCH   = .FALSE.
      LJET_MATCH = 0
      MATCH = .FALSE.
      LJETS = GZJETS()
      DO WHILE (LJETS.GT.0)
        JET_PHI = Q(LJETS + 8)
        JET_ETA = Q(LJETS + 9)
        DELTA_PHI = ABS(JET_PHI - HOT_PHI)
        IF (DELTA_PHI.GT.PI) DELTA_PHI = 2.*PI - DELTA_PHI
        DELTA_R = SQRT(DELTA_PHI**2.+(JET_ETA-HOT_ETA)**2.)
        IF (DELTA_R.LE.0.5) THEN
          MATCH      = .TRUE.
          IF (.NOT.MATCH) LJET_MATCH = LJETS
          NMATCH = NMATCH + 1
        ENDIF
        LJETS = LQ(LJETS)
      ENDDO
      CALL RESET_CAPH
      RETURN
      END
