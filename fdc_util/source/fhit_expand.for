C VAX/DEC CMS REPLACEMENT HISTORY, Element FHIT_EXPAND.FOR
C *2     9-NOV-1993 17:56:49 AVERY "updates in FDC for v12 RECO"
C *1     4-NOV-1993 10:57:34 AVERY "FDC changes for v12 RECO"
C VAX/DEC CMS REPLACEMENT HISTORY, Element FHIT_EXPAND.FOR
      SUBROUTINE FHIT_EXPAND(HALF,UNIT,QUAD,SECTOR,NHIT,N_DL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Expand compressed hit bank, FHIT,
C-      to fill FXSC (but not FXDA) bank.
C-
C-   Inputs  : HALF,UNIT,QUAD,SECTOR
C-   Outputs : Filled FXSC bank
C-
C-   Created  18-OCT-1993   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
C  INPUT:
      INTEGER HALF,UNIT,QUAD,SECTOR
      INTEGER NHIT, N_DL
C
C  Local:
C
C  FHIT_DECODE Arguments:
      INTEGER H,U,QD,S,W
      LOGICAL DL
      INTEGER LR
      LOGICAL ON_SEGMENT
      INTEGER TRK_FDCT,TRK_ZTRK
      REAL    DRIFTD, DRIFTD_MIRROR
      REAL    Z_POS, IONIZATION
C
C  OTHER:
C
      INTEGER LFHIT, GZFHIT
      INTEGER N_HITS(0:MXWIRP)
      INTEGER IDENT(MX_HIT_WIRE, 0:MXWIRP)
      INTEGER FIRST_HIT, IHIT
      INTEGER FIRST_DL, IDL
      INTEGER IQFHIT(2)
      INTEGER LFXSC,GZFXSC
      INTEGER NSW, NWORD, PTR_SC, P2
      INTEGER LOGCHA, IER
      INTEGER MAX_WIRE(0:1),WIRE
C
      REAL    MIP(MX_HIT_WIRE, 0:MXWIRP)
      REAL    Z_DIST(MX_HIT_WIRE, 0:MXWIRP)
      REAL    DR_DIST(0:1, MX_HIT_WIRE, 0:MXWIRP)
      REAL    ERROR(0:1),FDC_ERROR_SLOPE
      REAL    ERR_DL_ONE,ERR_DL_TWO
C
      LOGICAL DONE
      LOGICAL FIRST
C
      SAVE ERROR,FIRST
      DATA FIRST /.TRUE./
      DATA MAX_WIRE /MXWIRT, MXWIRP/
      DATA N_HITS /16*0/
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        ERROR(0) = FDC_ERROR_SLOPE(0.,0)
        ERROR(1) = FDC_ERROR_SLOPE(0.,1)
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('ERR_DL_ONE',ERR_DL_ONE,IER)
        CALL EZGET('ERR_DL_TWO',ERR_DL_TWO,IER)
        CALL EZRSET
      ENDIF
C
      NHIT = 0
      N_DL = 0
      LFHIT = GZFHIT()
      IF ( IQ(LFHIT+1).NE.1 ) THEN
        CALL ERRMSG('FDC-V0-FHIT','FHIT_EXPAND',
     &      'Can not expand V0 FHIT banks, not enough info','W')
        GOTO 999
      ENDIF
C
C Decode FHIT for this sector:
C
      CALL FHITPT(HALF,UNIT,QUAD,SECTOR,FIRST_HIT,NHIT,FIRST_DL,N_DL)
      IF ( NHIT.EQ.0 ) GOTO 999
C
      IDL = FIRST_DL
      DO IHIT = FIRST_HIT, NHIT+FIRST_HIT-1
        CALL GTFHIT(IHIT,IQFHIT)
        CALL FHIT_DECODE(
     &          IQFHIT,
     &          H,U,QD,S,WIRE,
     &          DL,LR,ON_SEGMENT,TRK_FDCT,TRK_ZTRK,
     &          DRIFTD, DRIFTD_MIRROR, Z_POS, IONIZATION )
        N_HITS(WIRE) = N_HITS(WIRE) +1
        DR_DIST(LR,N_HITS(WIRE),WIRE) = DRIFTD
        DR_DIST((1-LR),N_HITS(WIRE),WIRE) = DRIFTD_MIRROR
        MIP(N_HITS(WIRE),WIRE) = IONIZATION
        CALL FCODER(LOGCHA,HALF,UNIT,QUAD,SECTOR,WIRE,0,2)
        IDENT(N_HITS(WIRE),WIRE) = LOGCHA
C
        IF ( DL ) THEN
          CALL GTFHIT(IDL,IQFHIT)
          CALL FHIT_DECODE(
     &          IQFHIT,
     &          H,U,QD,S,W,
     &          DL,LR,ON_SEGMENT,TRK_FDCT,TRK_ZTRK,
     &          DRIFTD, DRIFTD_MIRROR, Z_POS, IONIZATION )
          Z_DIST(N_HITS(WIRE),WIRE) = Z_POS
          IF ( DL ) THEN                ! Two Delay line pulses, (else one)
            IDENT(N_HITS(WIRE),WIRE) = IBSET(LOGCHA,16)
          ENDIF
          IDL = IDL + 1
        ELSE
          Z_DIST(N_HITS(WIRE),WIRE) = 0.0
        ENDIF
      ENDDO
C
C Book and fill FXSC bank with results:
C
      IF ( UNIT.EQ.0 ) THEN
        CALL BKFTSC( HALF,QUAD,SECTOR,NHIT,LFXSC )
      ELSE
        CALL BKFPSC( HALF,SECTOR,NHIT,LFXSC )
      ENDIF
C
      IQ (LFXSC+1) = NHIT
      NSW  = IQ( LFXSC + 2 )
      NWORD = IQ( LFXSC + 3 )
      PTR_SC = 2*NSW + 3
      DO WIRE = 0, MAX_WIRE(UNIT)
        IF ( N_HITS(WIRE).GT.0 ) THEN
          IQ(LFXSC+4+WIRE) = N_HITS(WIRE)
          IQ(LFXSC+4+NSW+WIRE) = PTR_SC + 1
          DO  IHIT = 1, N_HITS(WIRE)
            P2 = LFXSC + PTR_SC
            IQ(P2+1) = IBCLR(IDENT(IHIT,WIRE),16)
            Q (P2+2) = DR_DIST(0,IHIT,WIRE)
            Q (P2+3) = DR_DIST(1,IHIT,WIRE)
            Q (P2+4) = 0.0
            Q (P2+5) = ERROR(UNIT)
            Q (P2+6) = 9999.
            Q (P2+7) = MIP(IHIT,WIRE)
            Q (P2+8) = 0.1
            IQ(P2+9) = 0
            IQ(P2+10) = 0
            IQ(P2+11) = 0
            IQ(P2+12) = 0
            IF ( Z_DIST(IHIT,WIRE) .NE. 0.0 ) THEN
              Q(P2+4) = Z_DIST(IHIT,WIRE)
              IF ( BTEST(IDENT(IHIT,WIRE),16) ) THEN
                Q(P2+6) = ERR_DL_TWO
                IQ(P2+9) = 3
              ELSE
                Q(P2+6) = ERR_DL_ONE
                IQ(P2+9) = 1
              ENDIF
            ENDIF
            PTR_SC = PTR_SC + NWORD
          ENDDO
        ELSE
          IQ(LFXSC+4+WIRE) = 0
          IQ(LFXSC+4+NSW+WIRE) = 0
        ENDIF
        N_HITS(WIRE) = 0
      ENDDO
  999 RETURN
      END
