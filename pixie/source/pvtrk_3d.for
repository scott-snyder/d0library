      SUBROUTINE PVTRK_3D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : make 3D display for VTX track
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created   7-JUL-1992   Nobuaki Oshima( Ref. PDTRK_3D.FOR )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS/LIST'
      INTEGER LVTRH, GZVTRH, LVTXT, KP
      REAL    XHPOS, YHPOS, ZHPOS, RCTGRV, CDCBD, TANPHI, INPACK
      REAL    TLSEG
      REAL    EXTRAP, EXTRP1, EXTRP2
      INTEGER IPATH, ERR, PLDALS
      INTEGER NROAD
      LOGICAL INEL,INMU,INTAU,INVEE
      LOGICAL FIRST, IFDCDC
      SAVE FIRST
      DATA TLSEG / 8. /
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C-
      CALL PATHRS
      LVTRH = GZVTRH()
      IF (LVTRH.EQ.0) RETURN
      NROAD=0
C-
      CALL PUOPEN
      LVTXT = LQ(LVTRH-1)               ! pointer to bank 'VTXT'
      IF (LVTXT.EQ.0) GOTO 212          ! no VTXT
  211 CONTINUE
      IF (Q(LVTXT+9) .LE. 0.0) GOTO 213
C-
C--- set color for tracks in roads depending on particle type
C-
      INMU=(IBITS(IQ(LVTXT),MUROAD,1).EQ.1)
      INEL=(IBITS(IQ(LVTXT),ELROAD,1).EQ.1)
      INTAU=(IBITS(IQ(LVTXT),TAUROAD,1).EQ.1)
      INVEE=(IBITS(IQ(LVTXT),5,1).EQ.1)
      CALL PXCOLR('CYA')
      IF(INMU)THEN
        CALL PXCOLR('GRE')
        NROAD=NROAD+1
      ENDIF
      IF(INEL)THEN
        CALL PXCOLR('RED')
        NROAD=NROAD+1
      ENDIF
      IF(INTAU)THEN
        CALL PXCOLR('CYA')
        NROAD=NROAD+1
      ENDIF
      IF(INVEE)THEN
        CALL PXCOLR('YEL')
        NROAD=NROAD+1
      ENDIF
      KP = LVTXT
C-
      TANPHI = TAN(Q(KP+6))
      INPACK = ABS(Q(KP+8) - TANPHI * Q(KP+7)) / SQRT(1+TANPHI**2)
      IF (INPACK .GT. 3.0) GOTO 213
C
      XHPOS = Q(KP+7) + TLSEG * COS(Q(KP+6))
      YHPOS = Q(KP+8) + TLSEG * SIN(Q(KP+6))
      ZHPOS = Q(KP+11) + TLSEG / TAN(Q(KP+9))
      CALL J3MOVE(XHPOS, YHPOS, ZHPOS)
      XHPOS = Q(KP+7) - TLSEG * COS(Q(KP+6))
      YHPOS = Q(KP+8) - TLSEG * SIN(Q(KP+6))
      ZHPOS = Q(KP+11) - TLSEG / TAN(Q(KP+9))
      CALL J3DRAW(XHPOS, YHPOS, ZHPOS )
C
  213 LVTXT = LQ(LVTXT)                 ! pointer to next bank 'VTXT'
      IF (LVTXT.NE.0) GOTO 211          ! if another track
C
  212 CALL JRCLOS
C
      IF(NROAD.GT.0) CALL LEGEND_ROAD
C
  999 RETURN
      END
