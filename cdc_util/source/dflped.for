      SUBROUTINE DFLPED
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to fill histograms for pedestals and T0
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: called by DFLHST
C-
C-   Created  11-DEC-1988   Qizhong Li-Demarteau
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:CDRESF.INC'
C
      INTEGER LAY, SEC, WIR, J, NEV, NUMDL, I, IPRDEL, NRUN, FACTOR
      INTEGER LDPDL, NWFADC, NBFADC, JP
      INTEGER LDTMW, NWWIRE, NBWIRE
      INTEGER LDTMD, NWDELY, NBDLAY
      INTEGER LDGNL, NWGN, NBGN, JPG
      INTEGER JPW, JPD, MAXLAY, MAXSEC, ERR
      INTEGER WIRNUM, SWWIRN, DLWIRN
      INTEGER IDPDST, IDPSGM, IDSWT0, IDDLT0, IDGAIN, IDGSGM
      INTEGER IER
      LOGICAL EZERROR
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DFLPED',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('MAXLAY',MAXLAY,ERR)
        CALL EZGET('MAXSEC',MAXSEC,ERR)
        CALL EZRSET
        DO 110 LAY = 0, MAXLAY
          LDPDL = LC( LDPDH - (LAY+1) )
          NBFADC = IC( LDPDL+4 )
          NWFADC = IC( LDPDL+3 )
          LDTMW  = LC( LDTMH - (LAY+1) )
          NBWIRE = IC( LDTMW+4 )
          NWWIRE = IC( LDTMW+3 )
          LDTMD  = LC( LDTMH - (LAY+5) )
          NBDLAY = IC( LDTMD+4 )
          NWDELY = IC( LDTMD+3 )
          LDGNL  = LC( LDGNH - (LAY+1) )
          NBGN   = IC( LDGNL+4 )
          NWGN   = IC( LDGNL+3 )
          DO 120 SEC = 0, MAXSEC
            IDPDST = 2300 + SEC      
            IDPSGM = 2350 + SEC
            IDSWT0 = 2400 + SEC
            IDDLT0 = 2450 + SEC
            IDGAIN = 2500 + SEC
            IDGSGM = 2550 + SEC
            JP = LDPDL + SEC*NBFADC*NWFADC + 4
            JPW = LDTMW + SEC*NBWIRE*NWWIRE + 4 - NWWIRE
            JPG = LDGNL + SEC*NBGN  *NWGN   + 4 - NWGN
            JPD = LDTMD + SEC*NBDLAY*NWDELY + 4
            DO 130 WIR = 0, MXFADC
              WIRNUM = LAY*11 + WIR
              CALL HF1(IDPDST,FLOAT(WIRNUM),C(JP+1))   ! pedestals
              CALL HF1(IDPSGM,FLOAT(WIRNUM),C(JP+2))   ! sigma of the pedestals
              IF (WIR.LE.MXSENS) THEN
                JPW = JPW + NWWIRE
                JPG = JPG + NWGN
                SWWIRN = LAY*7 + WIR
                CALL HF1(IDSWT0,FLOAT(SWWIRN),C(JPW+1)) ! t0 for SW
                CALL HF1(IDGAIN,FLOAT(SWWIRN),C(JPG+1)) ! gain
                CALL HF1(IDGSGM,FLOAT(SWWIRN),C(JPG+2)) ! sigma of the gain 
              ELSE
                DLWIRN = LAY*4 + (WIR - MXSENS)
                CALL HF1(IDDLT0,FLOAT(DLWIRN),C(JPD+1))    ! t0 for DL 
                JPD = JPD + NWDELY
              ENDIF  
  130       JP = JP + NWFADC
  120     CONTINUE
  110   CONTINUE
C
        FIRST=.FALSE.
      ENDIF
  999 RETURN
      END
