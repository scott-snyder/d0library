      FUNCTION DTREVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : User Event Routine for D0USER
C-
C-
C-   Created  18-JAN-1988   Olivier Callot
C-   Updated   8-APR-1988   Olivier Callot  New D0USER interface
C-   Updated  10-JUN-1988   Ghita Rahal-Callot  Create a bank DITR containing
C-                          the ISAJET tracks
C-   Updated   6-SEP-1988   Qizhong Li-Demarteau  fill histosgrams 
C-   Updated  22-MAR-1989   Qizhong Li-Demarteau  use SRCP
C-   Updated  12-JUL-1989   Qizhong Li-Demarteau  from CDCEVT to DTREVT 
C-   Updated   4-OCT-1989   Qizhong Li-Demarteau  use PATHST 
C-   Updated  23-DEC-1989   Qizhong Li-Demarteau  link-tree method to build
C-                                                segments is available too
C-   Updated  11-SEP-1990   Qizhong Li-Demarteau  added DE/DX 
C-   Updated  18-FEB-1991   Qizhong Li-Demarteau  added switch for redo/skip 
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-   Updated   1-NOV-1991   Qizhong Li-Demarteau  moved LOGICAL from routine
C-                                         name to Type Declaration Statement
C-   Updated  24-JUL-1992   Qizhong Li-Demarteau  added switch CDCRECO 
C-   Updated  14-NOV-1993   C. Klopfenstein added switch USEDHIT, to allow
C-                          reconstruction from compressed hits. Suppress
C-                          dropping CDCH, DHIT when USEDHIT is true.
C-   Updated   2-SEP-1994   Stefano Lami  added switch CDVALN to perform
C-                          CDC drift velocity self-alignment
C-   Updated   6-FEB-1995   Norman A. Graf  Set bit 0 in DTRH if z position
C-                          corrected by CDGETZ in DHIT
C-
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INTEGER NTOTEV, NEVALG, NRUN, NEVT, ERR
      INTEGER CDSURV, CDNSTP, IPATH, METHOD, IFULL
      INTEGER RUNSAV, IDSAV, RUN, ID, LDTRH, GZDTRH, LCDCH, GZCDCH
      INTEGER IER, CDCRECO
      INTEGER LDLYR, LAYER, GZDLYR, LDHIT, GZDHIT
      REAL    TPART, THITS, TSEGM, TTRAK, TOTHR
      REAL    TSUM,  TTHIT, TTSEG, TTTRK, TTOTH
      CHARACTER*4 DPATH
      CHARACTER*80 MESG
      EQUIVALENCE (IPATH, DPATH)
      LOGICAL DTREVT
      LOGICAL DTRREP, PRODUC, PRODFL, REDOCDC, USEDHIT, CDVALN
      LOGICAL BUILD_DITR
      LOGICAL MCDATA
      LOGICAL EZERROR
C
      DATA    NTOTEV / 0 /
      SAVE RUNSAV, IDSAV
      DATA RUNSAV,IDSAV/-1,-1/
      DATA MCDATA/.FALSE./
C----------------------------------------------------------------------
      DTREVT = .TRUE.
C
C  make sure the CDC full reconstruction is done once per event only
C
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = ID
      ELSE
        RETURN
      ENDIF
C
      IF ( NTOTEV .EQ. 0 ) THEN
      IF (IQ(LHEAD+1) .GT. 1000) MCDATA = .TRUE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','DTREVT',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET_i('DPATH',IPATH,ERR)
        CALL EZGET_i('METHOD',METHOD,ERR)
        CALL EZGET_l('REDOCDC',REDOCDC,ERR)
        CALL EZGET_l('BUILD_DITR',BUILD_DITR,ERR)
        CALL EZGET_l('USEDHIT',USEDHIT,ERR)
        IF (ERR .NE. 0) USEDHIT = .false.
        CALL EZGET('CDCRECO',CDCRECO,ERR)
        IF (ERR .NE. 0) CDCRECO = 3
        CALL EZGET('IFULL',IFULL,ERR)
        CALL EZGET('CDSURV',CDSURV,ERR)
        IF ( CDSURV .NE. 0 )  CALL EZGET('CDNSTP',CDNSTP,ERR)
        IF(.NOT. MCDATA) CALL EZGET_l('CDVALN',CDVALN,ERR)
        CALL EZRSET
        PRODFL = PRODUC()
C
C ****  Init timming measurements
C
        CALL TIMED( TSUM )
        TSUM = 0.
        TTHIT = 0.
        TTSEG = 0.
        TTTRK = 0.
        TTOTH = 0.
        NEVALG = 0
      ENDIF
C
      CALL PATHST(DPATH)
      LCDCH = GZCDCH()
      LDTRH = GZDTRH()
      IF (REDOCDC) THEN
C  if reconstructing from DHIT bank, keep CDCH and DHIT, but drop
C  DLYR banks. Also clear the hit-finding status words in CDCH
        IF (USEDHIT) THEN
          DO LAYER = 0, 3
            LDLYR = GZDLYR(LAYER)
            CALL MZDROP(IXCOM, LDLYR, ' ')
            IQ(LCDCH + 6 + LAYER) = 0
            IQ(LCDCH + 10) = 0
          ENDDO
        ELSE
          IF (LCDCH .GT. 0) CALL MZDROP(IXCOM,LCDCH,' ')
        ENDIF
        IF (LDTRH .GT. 0) CALL MZDROP(IXCOM,LDTRH,' ')
      ELSE
        IF (LDTRH .GT. 0 .AND. IBITS(IQ(LDTRH),IFULL,1) .NE. 0) RETURN
      ENDIF
C
      NTOTEV = NTOTEV + 1
C
C ****  Processing
C
      DBGFLG = .FALSE.
      IF( NTOTEV .LE. NEVDBG ) DBGFLG = .TRUE.
      IF ( DBGFLG ) THEN
        WRITE( LUNDBG, 1000 ) NTOTEV
 1000   FORMAT(/1X,120('=')//'0 Debug of CDC event number ',I6//)
C
        NRUN = IQ(LHEAD+6)
        NEVT = IQ(LHEAD+9)
        WRITE( LUNDBG, 1099 ) NRUN, NEVT
 1099   FORMAT(' --------- RUN',I10,'    EVENT',I6,'  ---------') 
      ENDIF
C
C ****  We measure the time for Hits, Segment finding, Track matching.
C ****  And any other time is given to 'Other'. For production job, it may
C ****  be useful to suppress these calls ( time needed... )
C
      IF (CDCRECO .LE. 0) GOTO 100
      IF( .NOT. PRODFL ) CALL TIMED( TOTHR )
      CALL CDHITS
      IF( .NOT. PRODFL ) CALL TIMED( THITS )
C
      IF (CDCRECO .LE. 1) GOTO 100
      IF (METHOD .EQ. 1) THEN
          CALL DSEGME       ! build segments by link-tree method
      ELSE
          CALL CDTSEG       ! build segments by the road method
      ENDIF
C
      IF( .NOT. PRODFL ) CALL TIMED( TSEGM )
      IF (CDCRECO .LE. 2) GOTO 100
      CALL CDTRAK
      IF( .NOT. PRODFL ) CALL TIMED( TTRAK )
      CALL DCDEDX          ! dE/dx
C
      LDTRH = GZDTRH()
      IQ(LDTRH) = IBSET(IQ(LDTRH),IFULL) ! flag that CDC is fully reconstructed
      LDHIT = GZDHIT()
      LCDCH = GZCDCH()
      IF(LDHIT.GT.0) THEN ! flag that Z position in DHIT corrected by CDGETZ
C
C ****  If hits were corrected at the RAW level...
C
        IF(IBITS(IQ(LDHIT),0,1) .NE. 0) IQ(LDTRH) = IBSET(IQ(LDTRH),0)
C
C ****  If hits were corrected at the DHIT level (i.e. from previous STA)
C
        IF(IBITS(IQ(LCDCH),1,1) .NE. 0) IQ(LDTRH) = IBSET(IQ(LDTRH),1)
      ENDIF
C
 100  CALL CDHFIL      ! to fill the histograms booked in DTRAKS.RCP 
      CALL DFLHST      ! to fill the histograms booked by User Dialog    
      CALL DHSTFL      ! to fill your own histograms 
      CALL PATHRS
C
      IF(CDVALN.AND.(.NOT.MCDATA)) CALL CDCVEL
C
C **** Build the bank DITR containing the ISAJET tracks which cross a fiducial
C **** volume defined by FIDU inside the CDC, when LVLDBG(9) is not 0.
C
      IF(BUILD_DITR .AND.
     &  (LQ(LHEAD - IZISAE) .NE. 0))  CALL CDCISA
C
      IF ( DBGFLG ) THEN
        WRITE( LUNDBG,1100 ) THITS, TSEGM, TTRAK, TOTHR
 1100   FORMAT('0-- time for hits ',F6.2,' , for segments ',F6.2,
     &         ' , for tracks ',F6.2,' , and for other things ',F6.2)
C        IF( LVLDBG(2) .NE. 0 ) CALL SETEDS( .TRUE. )
      ENDIF
      TTOTH = TTOTH + TOTHR
      TTHIT = TTHIT + THITS
      TTSEG = TTSEG + TSEGM
      TTTRK = TTTRK + TTRAK
C
C ****  Build new set of constants if asked
C
      IF ( CDSURV .NE. 0 ) THEN
        NEVALG = NEVALG + 1
        IF ( NEVALG .EQ. CDNSTP ) THEN
          CALL CDALGN
          NEVALG = 0
          CDSURV = CDSURV + 1
          CALL EZSET('CDSURV',CDSURV,ERR)
        ENDIF
      ENDIF
  999 RETURN
C
C ****  Entry for status report
C
      ENTRY DTRREP
C
C ****  In production, no time measurements....
C
      IF ( PRODFL ) RETURN
C
      TSUM = TTOTH + TTHIT + TTSEG + TTTRK
      IF ( NTOTEV .NE. 0 ) THEN
        TPART = 1000. * TSUM / NTOTEV
        TOTHR = 1000. * TTOTH / NTOTEV
        THITS = 1000. * TTHIT / NTOTEV
        TSEGM = 1000. * TTSEG / NTOTEV
        TTRAK = 1000. * TTTRK / NTOTEV
      ELSE
        TPART = 0.
        TOTHR = 0.
        THITS = 0.
        TSEGM = 0.
        TTRAK = 0.
      ENDIF
      WRITE( MESG, 2000 ) NTOTEV, TPART, THITS, TSEGM, TTRAK, TOTHR
 2000 FORMAT(I6,' evts at ',F7.0,' ms/ev ( hit,seg,tra,other =',
     &       4F7.0,' )')
      CALL INTMSG( MESG )
      END
