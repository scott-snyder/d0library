      SUBROUTINE PZTRK_3D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : make 3D display for central detector track ZTRKs
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  15-FEB-1991   Qizhong Li-Demarteau
C-   Updated   4-SEP-1991   S. Hagopian, add call to LEGEND_ROAD; counter NROAD
C-                          use ZTRAKS.PARAM
C-   Updated   1-NOV-1991   Qizhong Li-Demarteau  use VEE bit from IQ(LZTRK)
C-   Updated  28-JAN-1992   Lupe Howell  Update for SGI
C-   Updated  30-OCT-1992   Lupe Howell  Check ZTRAKS parameter before drawing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS/LIST'
      INTEGER LZTRH, GZZTRH, LZTRK, KP
      REAL    XHPOS, YHPOS, ZHPOS, RCTGRV, CDCBD, TANPHI, INPACK
      REAL    TLSEG, TLSEG1, TLSEG2
      REAL    EXTRAP, EXTRP1, EXTRP2
      INTEGER IPATH, ERR, PLDALS,IER
      INTEGER NROAD
      LOGICAL INEL,INMU,INTAU,INVEE,DRAW_ZTRKS,EZERROR
      LOGICAL FIRST
      SAVE FIRST
      DATA    TLSEG1/60.0/, TLSEG2/13.0/
      DATA    EXTRP1/-50.0/, EXTRP2/55.0/
      DATA FIRST/.TRUE./
C --------------------------------------------------------------------
C
C ****  Getting draw flag
C
      CALL EZPICK('PX_ZTRAKSDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PZTRK_3D','Bank PX_ZTRAKSDIS NOT FOUND',
     &    'W')
        GOTO 999
      ENDIF
      CALL PUGETV('ZTRAKS DRAW ZTRKS',DRAW_ZTRKS)
      CALL EZRSET

      IF ( DRAW_ZTRKS ) THEN
        CALL PATHRS
        LZTRH = GZZTRH()
        IF (LZTRH.EQ.0) RETURN
        IF (LDGEH .LE. 0) RETURN
        NROAD=0
        CDCBD = C(LDGEH+10)
        CALL PUOPEN
        LZTRK = LQ(LZTRH-1)               ! pointer to bank 'ZTRK'
        IF (LZTRK.EQ.0) GOTO 212          ! no ZTRK
  211   CONTINUE
C set color for tracks in roads depending on particle type
        INMU=(IBITS(IQ(LZTRK),MUROAD,1).EQ.1)
        INEL=(IBITS(IQ(LZTRK),ELROAD,1).EQ.1)
        INTAU=(IBITS(IQ(LZTRK),TAUROAD,1).EQ.1)
        INVEE=(IBITS(IQ(LZTRK),5,1).EQ.1)
        CALL PXCOLR('FOR')
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
        KP = LQ(LZTRK-1)
        IF (KP .EQ. 0) GOTO 213
C
        TANPHI = TAN(Q(KP+10))
        INPACK = ABS(Q(KP+12) - TANPHI * Q(KP+11)) / SQRT(1+TANPHI**2)
        IF (INPACK .GT. 3.0) GOTO 213
        RCTGRV = SQRT(Q(KP+11)**2 + Q(KP+12)**2)
        IF (RCTGRV .GT. CDCBD) THEN
          TLSEG = TLSEG2
          EXTRAP = EXTRP2
        ELSE
          TLSEG = TLSEG1
          EXTRAP = EXTRP1
        ENDIF
C
        XHPOS = Q( KP+11 ) + TLSEG * COS( Q(KP+10) )
        YHPOS = Q( KP+12 ) + TLSEG * SIN( Q(KP+10) )
        ZHPOS = Q(KP+15) + TLSEG / TAN(Q(KP+13))
        CALL J3MOVE(XHPOS, YHPOS, ZHPOS)
        XHPOS = Q( KP+11 ) - (TLSEG + EXTRAP) * COS( Q(KP+10) )
        YHPOS = Q( KP+12 ) - (TLSEG + EXTRAP) * SIN( Q(KP+10) )
        ZHPOS = Q(KP+15) - (TLSEG + EXTRAP) / TAN(Q(KP+13))
        CALL J3DRAW(XHPOS, YHPOS, ZHPOS )
C
  213   LZTRK = LQ(LZTRK)                 ! pointer to next bank 'ZTRK'
        IF (LZTRK.NE.0) GOTO 211          ! if another track
C
  212   CALL JRCLOS
C
C
        IF(NROAD.GT.0) CALL LEGEND_ROAD
      ENDIF
  999 RETURN
      END
