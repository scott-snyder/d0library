      SUBROUTINE CDALGN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Computes CDC parameters from curent run
C-
C-   Inputs  :  Content of /CDRESF/ cumulated up to now
C-   Outputs :  New values in /ZEBSTP/ for CDC parameters
C-
C-   Created  19-OCT-1987   Olivier Callot
C-   Updated  12-SEP-1988   Qizhong Li-Demarteau  add run # in output
C-   Updated  22-MAR-1989   Qizhong Li-Demarteau  use SRCP
C-   Updated  11-SEP-1990   Qizhong Li-Demarteau  do PED, SWT0, DLT0 and 
C-                                             gain alignment separately
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  use EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$INC:CDRESF.INC'
      INTEGER LAY, SEC, WIR, J, NEV, NUMDL, I, IPRDEL, NRUN
      INTEGER LDPDL, NWFADC, NBFADC, JP
      INTEGER LDTMW, NWWIRE, NBWIRE
      INTEGER LDTMD, NWDELY, NBDLAY
      INTEGER LDGNL, NWGN, NBGN, JPG
      INTEGER JPW, JPD, JP2, ERR
      INTEGER CDSURV, MAXLAY, MAXSEC
      INTEGER IER
      REAL    RMEAN, SIGMA, CHANGE, GMEAN, GSIGM, RATIO
      CHARACTER*80 MSGTXT
      LOGICAL PDALGN, GNALGN, TWALGN, TDALGN
      LOGICAL EZERROR
C----------------------------------------------------------------------
C
      CALL EZPICK('DTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('DTRAKS','CDALGN',
     &    'Unable to find bank DTRAKS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGET('CDSURV',CDSURV,ERR)
      CALL EZGET('PDALGN',PDALGN,ERR)
      CALL EZGET('GNALGN',GNALGN,ERR)
      CALL EZGET('TWALGN',TWALGN,ERR)
      CALL EZGET('TDALGN',TDALGN,ERR)
      CALL EZGET('MAXLAY',MAXLAY,ERR)
      CALL EZGET('MAXSEC',MAXSEC,ERR)
      CALL EZRSET
      WRITE( MSGTXT, 1000 ) CDSURV
 1000 FORMAT(' End of CDC alignement phase ',I3)
      CALL INTMSG( MSGTXT )
C
      NRUN = IQ(LHEAD+6)
      WRITE(LUNDBG,1005) NRUN, CDSURV
 1005 FORMAT('1------------ result of CDC T0 alignement, run ',
     &  I10,', phase ',I2,' ------------'/)
C
C ****  Computes new value for the pedestals.
C ****  The cumul is done in CDPULS, only for CDSURV=1,2
C
      IF (PDALGN) THEN
        IF ( CDSURV .LE. 2 ) THEN
          DO 10 LAY = 0, MAXLAY
            LDPDL = LC( LDPDH - (LAY+1) )
            NBFADC = IC( LDPDL+4 )
            NWFADC = IC( LDPDL+3 )
            DO 20 SEC = 0, MAXSEC
              IPRDEL = 0
              JP = LDPDL + SEC*NBFADC*NWFADC + 4
C
C ****  Points on DPDL for the current chanel
C
              DO 30 WIR = 0, MXFADC
                IF ( NEVPED(WIR,SEC,LAY) .LE. 1 ) GOTO 30
                IPRDEL = 1
                NEV   = NEVPED(WIR,SEC,LAY)
                RMEAN = SUMPED(WIR,SEC,LAY) / NEV
                SIGMA = SM2PED(WIR,SEC,LAY) / NEV - RMEAN**2
                IF( SIGMA .GT. 0. ) SIGMA = SQRT( SIGMA )
                WRITE( LUNDBG, 1010 ) LAY,SEC,WIR, NEV, RMEAN, SIGMA
 1010           FORMAT(2X,'Pedestal  Layer',I2,' Sector',I3,' Wire',I3,
     &                  ' Nev, mean, sigma ',I6,2F10.3)
                C( JP + 1 ) = RMEAN
                C( JP + 2 ) = SIGMA
   30         JP = JP + NWFADC
              IF( IPRDEL .NE. 0 ) WRITE( LUNDBG,2400)
   20       CONTINUE
   10     CONTINUE
        ENDIF
      ENDIF
C
C ****  Now, normalize and convert T0 ( from residuals in the fits ) and
C ****  Gains ( by definition, the mean pulse area should be 1 MIP )
C
      IF (GNALGN .OR. TWALGN .OR. TDALGN) THEN
        DO 100 LAY = 0, MAXLAY
          LDTMW  = LC( LDTMH - (LAY+1) )
          NBWIRE = IC( LDTMW+4 )
          NWWIRE = IC( LDTMW+3 )
          LDTMD  = LC( LDTMH - (LAY+5) )
          NBDLAY = IC( LDTMD+4 )
          NWDELY = IC( LDTMD+3 )
          LDGNL  = LC( LDGNH - (LAY+1) )
          NBGN   = IC( LDGNL+4 )
          NWGN   = IC( LDGNL+3 )
          DO 110 SEC = 0, MAXSEC
            IPRDEL = 0
            JPW = LDTMW + SEC*NBWIRE*NWWIRE + 4 - NWWIRE
            JPG = LDGNL + SEC*NBGN  *NWGN   + 4 - NWGN
            JPD = LDTMD + SEC*NBDLAY*NWDELY + 4
            DO 120 WIR = 0, NBGN-1
              JPW = JPW + NWWIRE
              JPG = JPG + NWGN
C
C ****  Update the gain for wires AND delay lines, with minimum number of
C ****  events to have significative values
C
              IF (GNALGN) THEN
                IF ( NEVGAI(WIR,SEC,LAY) .LT. 10 ) GOTO 120
                NEV   = NEVGAI(WIR,SEC,LAY)
                GMEAN = SUMGAI(WIR,SEC,LAY)/NEV
                GSIGM = SU2GAI(WIR,SEC,LAY)/NEV - GMEAN**2
                IF( GSIGM .GT. 0. ) GSIGM = SQRT(GSIGM)
                RATIO = GMEAN / C( JPG+1 )
                C( JPG+1 ) = GMEAN
              ENDIF
              IF (TWALGN) THEN
                IF ( WIR .LE. MXSENS ) THEN
C
C ****  First, the residual. C(JPW+2) is the drift velocity
C
                  NEV   = NBRES(WIR,SEC,LAY)
                  RMEAN = SUMRES(WIR,SEC,LAY)/NEV
                  SIGMA = SUMRE2(WIR,SEC,LAY)/NEV - RMEAN**2
                  IF( SIGMA .GT. 0.) SIGMA = SQRT(SIGMA)
                  CHANGE = RMEAN / C( JPW+2 )
                  C( JPW+1 ) = C( JPW+1 ) + CHANGE
                ELSE
                  RMEAN = 0.
                  SIGMA = 0.
                  CHANGE = 0.
                ENDIF
              ENDIF
              IF (GNALGN .OR. TWALGN) THEN
                IF( IPRDEL .EQ. 0 ) WRITE(LUNDBG,2000)
 2000       FORMAT(' Layer Sector Wire    Events      Residual: Mean',
     &             '     Sigma    Change       Gain: Mean',
     &             '     Sigma     Ratio')
                IPRDEL = 1
                WRITE( LUNDBG, 2100 ) LAY, SEC, WIR, NEV, RMEAN, SIGMA,
     &                            CHANGE, GMEAN, GSIGM, RATIO
 2100           FORMAT(1X,I5,I7,I5,I10,10X,2F10.4,F10.2,7X,2F10.6,F10.3)
              ENDIF
C
C ****  If changing T0 for sense wire associated with delay line, we have
C ****  to change also the T0 of the delay lines ( we alway uses Tdelay-Tsens
C ****  in the Delay line processing, see CDGETZ )
C
              IF (TWALGN .AND. TDALGN) THEN
                IF ( WIR .EQ. 0 ) THEN
                  C( JPD+1         ) = C( JPD+1          ) + CHANGE
                  C( JPD+NWDELY+1  ) = C( JPD+NWDELY+1   ) + CHANGE
                ELSEIF ( WIR .EQ. MXSENS ) THEN
                  C( JPD+2*NWDELY+1 )= C( JPD+2*NWDELY+1 ) + CHANGE
                  C( JPD+3*NWDELY+1 )= C( JPD+3*NWDELY+1 ) + CHANGE
                ENDIF
              ENDIF
  120       CONTINUE
C
C ****  Now, the Delay lines special: first, Z residual ( L-R )
C
            IF (TDALGN) THEN
              DO 125 NUMDL = 1, NBDELY
                JPD = LDTMD + (SEC*NBDLAY+ 2*NUMDL-2)*NWDELY + 4
                JP2 = JPD + NWDELY                  ! Pointers on the 2 sides
                NEV = NZRES( NUMDL,SEC,LAY )
                IF ( NEV .NE. 0 ) THEN
                  RMEAN = SZRES(NUMDL,SEC,LAY) / NEV
                  SIGMA = ( SZRE2(NUMDL,SEC,LAY) / NEV - RMEAN**2 )
                  IF( SIGMA .GT. 0 ) SIGMA = SQRT( SIGMA )
                  CHANGE = .5 * RMEAN
                  C( JPD+1 ) = C( JPD+1 ) + CHANGE/C( JPD+2 )
                  C( JP2+1 ) = C( JP2+1 ) + CHANGE/C( JP2+2 )
                  IF( IPRDEL .EQ. 1 ) WRITE(LUNDBG,2400)
 2400             FORMAT(' ')
                  IPRDEL = 2
                  WRITE( LUNDBG, 3000 ) NUMDL, NEV, RMEAN, SIGMA, CHANGE
 3000             FORMAT(18X,'Z Residual delay',I2,
     &                ' Nev, mean, sigma ',I6,2F10.4,' Change ',F10.2)
                ENDIF
C
C ****  Now, the residual of the 'sum rule' Tl+Tr=2*Ts ( see CDGETZ )
C
                NEV = NDLEV( NUMDL, SEC, LAY )
                IF( NEV .GT. 0 ) THEN
                  RMEAN = SDLRES( NUMDL, SEC, LAY ) / NEV
                  SIGMA = SDLRE2( NUMDL, SEC, LAY ) / NEV - RMEAN**2
                  IF( SIGMA .GT. 0 ) SIGMA = SQRT( SIGMA )
                  CHANGE = RMEAN * .5
                  WRITE( LUNDBG, 4000 ) NUMDL, NEV, RMEAN, SIGMA, CHANGE
 4000             FORMAT(19X,'Tsum Delay line',I2,
     &                ' Nev, mean, sigma ',I6,2F10.4,' Change ',F10.2)
                  C( JPD+1 ) = C( JPD+1 ) + CHANGE
                  C( JP2+1 ) = C( JP2+1 ) + CHANGE
                ENDIF
  125         CONTINUE
            ENDIF
            IF( IPRDEL .NE. 0 ) WRITE(LUNDBG,2400)
  110     CONTINUE
  100   CONTINUE
      ENDIF
  999 RETURN
      END
