      SUBROUTINE PDTRK_3D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : make 3D display for CDC track DTRKs
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  12-JUN-1992   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS/LIST'
      INTEGER LDTRH, GZDTRH, LDTRK, KP, LRCP
      INTEGER IPATH, ERR, PLDALS
      INTEGER NROAD,IER
      REAL    XHPOS, YHPOS, ZHPOS, RCTGRV, CDCBD, TANPHI
      REAL    IMPACT, IMPACT_CUT
      REAL    TLSEG, TLSEG1, TLSEG2
      REAL    EXTRAP, EXTRP1, EXTRP2
      LOGICAL INEL,INMU,INTAU,INVEE
      LOGICAL OK
      LOGICAL FIRST, IFDCDC,EZERROR
      SAVE FIRST
      DATA    TLSEG1/13.0/, TLSEG2/13.0/
      DATA    EXTRP1/0.0/, EXTRP2/55.0/
      DATA FIRST/.TRUE./
C --------------------------------------------------------------------
C
C ****  Getting parameters from RCP files
C
      CALL PUGETV('CDC ONLY', IFDCDC )
      CALL EZPICK('PX_ZTRAKSDIS_RCP')
      IF ( .NOT. EZERROR(IER) ) THEN
        CALL PUGETV('IMPACT CUT', IMPACT_CUT)
        CALL EZRSET
      ENDIF
C
      IF (IMPACT_CUT .LE. 0.0) THEN
        CALL EZLOC('PX_ZTRAKSDIS_RCP',LRCP)
        OK = LRCP .GT. 0
        IF (.NOT. OK) THEN
          CALL EZPICK('PX_CDCDIS_RCP')      
          IF ( .NOT. EZERROR(IER) ) THEN
            CALL PUGETV('CDC IMPACT CUT',IMPACT_CUT)
            CALL EZRSET
          ENDIF
        ENDIF
      ENDIF
C        
      IF ( IFDCDC ) THEN
        TLSEG = TLSEG2
        EXTRAP = EXTRP2
      ELSE
        TLSEG = TLSEG1
        EXTRAP = EXTRP1
      ENDIF
      CALL PATHRS
      LDTRH = GZDTRH()
      IF (LDTRH .EQ. 0) RETURN
      IF (LDGEH .LE. 0) RETURN
      NROAD=0
      CDCBD = C(LDGEH+10)
      CALL PUOPEN
      LDTRK = LQ(LDTRH-1)               ! pointer to bank 'DTRK'
      IF (LDTRK.EQ.0) GOTO 212          ! no DTRK
 211  CONTINUE
      IF (Q(LDTRK+9) .LE. 0.0) GOTO 213
C
C set color for tracks in roads depending on particle type
C
      INMU=(IBITS(IQ(LDTRK),MUROAD,1).EQ.1)
      INEL=(IBITS(IQ(LDTRK),ELROAD,1).EQ.1)
      INTAU=(IBITS(IQ(LDTRK),TAUROAD,1).EQ.1)
      INVEE=(IBITS(IQ(LDTRK),5,1).EQ.1)
      CALL PXCOLR('CYA')
      IF (INVEE) THEN
        CALL PXCOLR('YEL')
        NROAD = NROAD+1
      ENDIF
      IF (INTAU) THEN
        CALL PXCOLR('CYA')
        NROAD = NROAD+1
      ENDIF
      IF (INMU) THEN
        CALL PXCOLR('GRE')
        NROAD = NROAD+1
      ENDIF
      IF (INEL) THEN
        CALL PXCOLR('RED')
        NROAD = NROAD+1
      ENDIF
      KP = LDTRK
C
      TANPHI = TAN(Q(KP+6))
      IMPACT = ABS(Q(KP+8) - TANPHI * Q(KP+7)) / SQRT(1+TANPHI**2)
      IF (IMPACT .GT. IMPACT_CUT) GOTO 213
C
      XHPOS = Q(KP+7) + TLSEG * COS(Q(KP+6))
      YHPOS = Q(KP+8) + TLSEG * SIN(Q(KP+6))
      ZHPOS = Q(KP+11) + TLSEG / TAN(Q(KP+9))
      CALL J3MOVE(XHPOS, YHPOS, ZHPOS)
      XHPOS = Q(KP+7) - (TLSEG + EXTRAP) * COS(Q(KP+6))
      YHPOS = Q(KP+8) - (TLSEG + EXTRAP) * SIN(Q(KP+6))
      ZHPOS = Q(KP+11) - (TLSEG + EXTRAP) / TAN(Q(KP+9))
      CALL J3DRAW(XHPOS, YHPOS, ZHPOS )
C
  213 LDTRK = LQ(LDTRK)                 ! pointer to next bank 'DTRK' 
      IF (LDTRK .NE. 0) GOTO 211          ! if another track
C
  212 CALL JRCLOS
C
      IF (NROAD.GT.0) CALL LEGEND_ROAD
C
  999 RETURN
      END
