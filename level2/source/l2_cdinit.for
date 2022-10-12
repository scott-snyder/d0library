      SUBROUTINE L2_CDINIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read initialization file, and define all
C-               parameters & constants needed for LEVEL2 CDC processing
C-
C-   Inputs  : 
C-   Outputs : 
C-
C-   Created  01-NOV-1991  Based on D0$CDC_UTIL$ROOT$SOURCE:CDINIT.FOR
C-   Modified 24-NOV-1993  Make PED errors Warnings only (bad channels exist)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:DDEBUG.INC'
C
      INTEGER GZDTMH, GZDPDH, IER, IERR, IF, ILAYER
      INTEGER IS, IW, J, LDTMW, LDPDL, LENGTH 
      INTEGER MAX_CDCRT, NUMRUN, RUNNO, RUNSAV
      LOGICAL DTRINI, EZERROR, GN_INI, IOK, MCDATA
      LOGICAL OK, PD_INI, TM_INI, READ_DFLSTP
      CHARACTER*26 FILNAM, DFLSTP
      REAL PED, PED_LO, PED_HI, TZERO, TZERO_LO, TZERO_HI
C
      DATA MCDATA/.TRUE./        
      DATA RUNSAV/99999/
C----------------------------------------------------------------------
C
C  read default STP file name from DTRAKS_RCP file
C
        IF( .NOT.DTRINI() ) THEN
          CALL ERRMSG('L2TRAK','DTRINI',
     &    'Unable to init bank L2TRAK_RCP','W')
          GOTO 999
        ENDIF
C
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('L2TRAK','L2_CDINIT',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGETS('CDC_STPFILE',1,DFLSTP,LENGTH,IER)
        CALL EZGET('READ_DFLSTP',READ_DFLSTP,IER)
        CALL EZGET('PD_INI',PD_INI,IER)
        CALL EZGET('TM_INI',TM_INI,IER)
        CALL EZGET('GN_INI',GN_INI,IER)
        CALL EZGET_i('MAX_CDCRT',MAX_CDCRT,IER)
        CALL EZRSET
C
        IF (DFLSTP(12:13).EQ.'D0')  MCDATA = .FALSE.
C
C read debug control parameters from SRCP bank and dump RCP if requested
C
        CALL DRDDBG(OK)
        IF (.NOT. OK) 
     &   CALL INTMSG(' Debug control parameters for CDC are not found')
C
      RUNSAV = RUNNO()
      IF (RUNSAV.EQ.0) RUNSAV=99999
C
C   For Monte Carlo data, use the D0LIBRARY STP file as the default
C   For non-MC data, can also use the STP file from D0LIBRARY as the
C   default STP file if the flag READ_DFLSTP file in DTRAKS.RCP is true
C
      IF (MCDATA .OR. READ_DFLSTP) THEN
        CALL INTMSG(' Read Static Parameter File '//DFLSTP)
        CALL CDISTP(DFLSTP, IERR)
        IF (IERR .EQ. 0) GOTO 200
        GOTO 301
      ENDIF
C
C ****  Error, the requested file was not found...
C
 301  CALL INTMSG(' We can''t read the CDC constants set. Abort')
      CALL EXIT
C
  200 CONTINUE
C
C  update STP banks with the values from DBL3
C
      IF (.NOT. MCDATA) THEN
        CALL DDBINI(RUNSAV,MAX_CDCRT,PD_INI,TM_INI,GN_INI,IOK)
        IF (.NOT. IOK) 
     &  CALL INTMSG(' CDINIT: Error in updating STP banks from DBL3')
C
C  Zeroth order check that MC STP values have been overwritten by 
C  reasonable DBL3 values.  Check that all are non-zero, different from
C  MC values, and fall within an expected range.
C
        LDPDH = GZDPDH()
        LDTMH = GZDTMH()
C
        DO ILAYER = 4,1,-1
C
          LDPDL = LC(LDPDH-ILAYER) ! Peds for layer 3
          PED_LO = 100
          PED_HI = 0
          DO IS=0,31
            DO IF = 0,10
              J = LDPDL + (IS*IC(LDPDL+4) + IF) * IC(LDPDL+3) + 4
              PED = C(J+1)
              IF (PED.EQ.0 .OR. PED.GT.30) THEN
                CALL ERRMSG('L2TRAK','DTRINI',
     &            'Bad PEDESTAL values in DPDL bank','W')
                GOTO 999
              ENDIF
              IF (PED.LT.PED_LO) PED_LO = PED
              IF (PED.GT.PED_HI) PED_HI = PED
            ENDDO
          ENDDO
          IF (PED_LO.EQ.PED_HI) THEN
            CALL ERRMSG('L2TRAK','DTRINI',
     &        'PEDESTALs at default MC values','F')
            GOTO 999
          ENDIF
          IF ((PED_HI-PED_LO).GT.20.0) THEN
            CALL ERRMSG('L2TRAK','DTRINI',
     &        'Range of PEDESTAL values TOO LARGE','W')
            GOTO 999
          ENDIF
C
        ENDDO
C
        DO ILAYER = 4,1,-1
C
          LDTMW = LC(LDTMH-ILAYER) ! WIRE info for layer 3
          TZERO_LO = 500
          TZERO_HI = 0
          DO IS=0,31
            DO IW = 0,6
              J = LDTMW + (IS*IC(LDTMW+4) + IW) * IC(LDTMW+3) + 4
              TZERO = ABS( C(J+1) )
              IF (TZERO.EQ.0 .OR. TZERO.GT.500) THEN
                CALL ERRMSG('L2TRAK','DTRINI',
     &            'Bad TZERO values in DTMW bank','F')
                GOTO 999
              ENDIF
              IF (TZERO.LT.TZERO_LO) TZERO_LO = TZERO
              IF (TZERO.GT.TZERO_HI) TZERO_HI = TZERO
            ENDDO
          ENDDO
          IF (TZERO_LO.EQ.TZERO_HI) THEN
            CALL ERRMSG('L2TRAK','DTRINI',
     &        'TZEROs at default MC values','F')
            GOTO 999
          ENDIF
          IF ((TZERO_HI-TZERO_LO).GT.150.0) THEN
            CALL ERRMSG('L2TRAK','DTRINI',
     &        'Range of TZERO values suspiciously LARGE','F')
            GOTO 999
          ENDIF
        ENDDO
C
      ENDIF
C
      IF( LVLDBG(1) .NE. 0 ) THEN
        IF (LHEAD .GT. 0) NUMRUN = IQ(LHEAD+6)
        WRITE( LUNDBG, 4000 ) NUMRUN
 4000   FORMAT('0',25('-'),' We used the following parameters for run',
     &          I10,X,25('-'))
        CALL CDSAVE(LUNDBG)
        CALL INTMSG(' Initialisation parameters written on debug file')
      ENDIF
  999 RETURN
      END
