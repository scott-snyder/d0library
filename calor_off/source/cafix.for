C VAX/DEC CMS REPLACEMENT HISTORY, Element CAFIX.FOR
C *9    13-MAR-1994 22:42:54 MEENA "Richard V. Astur: Add CORR_LEVEL_PNUT switch back in to CAF-IX"
C *8     7-MAR-1994 00:48:54 MEENA "Richard V. Astur: Add CORR_LEVEL_PNUT switch back in to CAFIX"
C *7    27-FEB-1994 22:17:28 MEENA "Stan M. Krzywdzinski: Added BUILD_GLOB"
C *6    15-NOV-1993 23:59:03 MEENA "Marcel Demarteau: fixed bug getting correction from VCOR"
C *5     8-AUG-1993 14:04:44 MEENA "Marcel Demarteau: fixed bug getting correction from VCOR"
C *4    12-MAY-1993 23:14:04 MEENA "Harrison B. Prosper: Now builds PNUT(5)"
C *3    14-APR-1993 10:30:53 HARRY "PROD_FULL_D0RECO"
C *2    14-JAN-1993 23:40:49 HARRY "Fixes"
C *1    14-JAN-1993 09:49:21 HARRY "Harrison B. Prosper: Fix-up package"
C VAX/DEC CMS REPLACEMENT HISTORY, Element CAFIX.FOR
      FUNCTION CAFIX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Apply accumulated corrections to global
C-   event quantities, for example:
C-
C-      1)  Missing Et
C-      2)  Scalar Et
C-      3)  Jets
C-      4)  Electrons
C-
C-   Returned value  : TRUE If ALL OK
C-   Inputs  : none
C-   Outputs : none
C-   Controls: Needs VCOR and PNUT banks.
C-
C-   Created   1-NOV-1992   Harrison B. Prosper
C-   Updated  14-JAN-1993   Harrison B. Prosper
C-   Updated   9-APR-1993   Harrison B. Prosper   Build PNUT(5)
C-   Updated  23-APR-1993   Meenakshi Narain
C-                          problem fix : create glob bank for every event
C-   Updated  15-SEP-1993   Marcel Demarteau   fixed Et and scalar Et calc.
C-   Updated  30-SEP-1993   Stan M. Krzywdzinski, Harrison B. Prosper
C-      add BUILD_GLOB switch
C-   Updated  13-JAN-1994   Richard Astur - Make into a total correction routine
C-   Updated   9-SEP-1994   Astur: Skip old (pre-10) reco versions
C-   Updated  26-AUG-1997   Bob Hirosky : protect against MET=0.0 in ATAN2 call
C-   Updated  12-SEP-1997   Bob Hirosky : bug fix for MET=0.0 protection
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      REAL      SMALL
      PARAMETER( SMALL = .001 )
C----------------------------------------------------------------------
      LOGICAL   CAFIX, CAFIX_BEGIN, CAFIX_END, EZERROR, OK, BUILD_GLOB
      EXTERNAL  EZERROR
C----------------------------------------------------------------------
      LOGICAL   DO_EM_CORRECTION, DO_JET_CORRECTION, DO_MET_CORRECTION
      SAVE      DO_EM_CORRECTION, DO_JET_CORRECTION, DO_MET_CORRECTION
      LOGICAL   CORRECTEM, CORRECTEM_BEGIN, CORRECTEM_END, CORRECT_MET
      LOGICAL   DO_ZSP_CORRECTION, DO_UND_CORRECTION, DO_CONE_CORRECTION
      SAVE      DO_ZSP_CORRECTION, DO_UND_CORRECTION, DO_CONE_CORRECTION
      INTEGER   ISYS
      SAVE      ISYS
      LOGICAL   MONTE_CARLO_DATA
      EXTERNAL  MONTE_CARLO_DATA
      INTEGER   IVERSION, IPAS
      EXTERNAL  CORRECTEM, CORRECTEM_BEGIN, CORRECTEM_END, CORRECT_MET
C----------------------------------------------------------------------
      INTEGER   GOOD_MUON_MASK
      SAVE      GOOD_MUON_MASK
C---------------------------------------------------------------------
      INTEGER   CORR_LEVEL_PNUT
      SAVE      CORR_LEVEL_PNUT
      INTEGER   GZVCOR, GZPNUT, GZPMUO, IPASS_LOW, IPASS_HIGH
      EXTERNAL  GZVCOR, GZPNUT, GZPMUO
      INTEGER   I, J, K, STATUS, LVCOR, IPASS, ND
      INTEGER   LBANK, LPNUT, LPMUO, RET_MASK, IAND
      REAL      DP(5), DPE(5), EZOE, OLD_PNUT5(5), OLD_PNUT4(5)
      REAL    EX,EY,EZ,ETOT,ET,THETA,ETA,PHI,VAREX,VAREY,SIGET,ETSCAL
      REAL    EXP,EYP,ETP
      REAL    AUX
      CHARACTER*80 CODE,MESS
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST  /.TRUE./
C----------------------------------------------------------------------
      CAFIX = .TRUE.
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
        CALL EZPICK('CAFIX_RCP')
        IF ( EZERROR(STATUS) ) THEN
          CALL ERRMSG
     &      ('NO_CAFIX_RCP_BANK','CAFIX','No CAFIX_RCP bank','F')
        ELSE
C
C ****  Read parameters from RCP file
C
          CALL EZGET('CORR_LEVEL_PNUT',CORR_LEVEL_PNUT, STATUS )
          IF ( STATUS .EQ. 0 ) CALL EZGET('BUILD_GLOB',BUILD_GLOB,
     &      STATUS)
          IF ( STATUS .EQ. 0 ) CALL EZGET('DO_EM_CORRECTION',
     &      DO_EM_CORRECTION, STATUS )
          IF ( STATUS .EQ. 0 ) CALL EZGET('DO_JET_CORRECTION',
     &      DO_JET_CORRECTION, STATUS )
          IF ( STATUS .EQ. 0 ) CALL EZGET('DO_ZSP_CORRECTION',
     &      DO_ZSP_CORRECTION, STATUS )
          IF ( STATUS .EQ. 0 ) CALL EZGET('DO_UND_CORRECTION',
     &      DO_UND_CORRECTION, STATUS )
          IF ( STATUS .EQ. 0 ) CALL EZGET('DO_CONE_CORRECTION',
     &      DO_CONE_CORRECTION, STATUS )
          IF ( STATUS .EQ. 0 ) CALL EZGET('ISYS',
     &      ISYS, STATUS )
          IF ( STATUS .EQ. 0 ) CALL EZGET('DO_MET_CORRECTION',
     &      DO_MET_CORRECTION, STATUS )
          IF ( STATUS .EQ. 0 ) CALL EZGET('GOOD_MUON_MASK',
     &      GOOD_MUON_MASK, STATUS )
          IF ( STATUS .NE. 0 ) THEN
            CALL ERRMSG('RCP error','CAFIX','Parameter read failure',
     &        'F')
            RETURN
          ENDIF
        ENDIF
C
C ****  Initialize GLOBFL by getting some constants from current
C ****  RCP bank
C
        IF ( BUILD_GLOB ) THEN
          CALL GLOBFL(0)
        ENDIF
        CALL EZRSET
      ENDIF
C
C ********************************************************************
C ****  BUILD GLOB BANK
C ********************************************************************
C
      IF ( BUILD_GLOB ) THEN
        CALL GLOBFL(1)
      ENDIF
C
C**********************************************************************
C ****  Skip old RECO
C*********************************************************************
C
      CALL RECO_VERSION( IVERSION, IPAS )
      IF ( .NOT. MONTE_CARLO_DATA() .AND. IVERSION .LT. 10 ) THEN
        CALL ERRMSG('Old RECO','CAFIX','RECO version too old - abort',
     &    'W')
        RETURN
      ENDIF

C********************************************************************
C ***  Call various correction routines
C********************************************************************
      IF ( CORR_LEVEL_PNUT .LE. 0 ) THEN
        DO_MET_CORRECTION = .FALSE.
      ELSE IF ( CORR_LEVEL_PNUT .EQ. 1 ) THEN
        IPASS_LOW = 4
        IPASS_HIGH= 4
      ELSE
        IPASS_LOW = 4
        IPASS_HIGH= 5
      ENDIF

      IF ( DO_EM_CORRECTION ) THEN
        OK = CORRECTEM()
        IF ( .NOT. OK ) CALL ERRMSG('Not OK','CAFIX',
     &      'CORRECTEM returns FALSE','W')
      ENDIF

      IF ( DO_MET_CORRECTION ) THEN
        OK = CORRECT_MET()
        IF ( .NOT. OK ) CALL ERRMSG('Not OK','CAFIX',
     &      'CORRECT_MET returns FALSE','W')
      ENDIF

      IF ( DO_JET_CORRECTION ) THEN
        CALL  CORRECT_JETS( DO_ZSP_CORRECTION, DO_UND_CORRECTION,
     &      DO_CONE_CORRECTION, ISYS, .FALSE., OK )
      ENDIF

C ********************************************************************
C ****  LOOP OVER VCOR BANKS
C ****  Form a vectorial sum of VCOR vectors and build a new PNUT
C ****  bank with the vectorially corrected missing Et etc.
C ********************************************************************
C
      LVCOR = GZVCOR()
      CALL VZERO( DP, 5 )
      CALL VZERO( DPE, 5 )
      DO WHILE ( LVCOR .GT. 0 )
        DP(1) = DP(1) + Q( LVCOR + 3 )
        DP(2) = DP(2) + Q( LVCOR + 4 )
        DP(3) = DP(3) + Q( LVCOR + 5 )
        DP(4) = DP(4) + Q( LVCOR + 6 )
        DP(5) = DP(5) + Q( LVCOR + 11)
C
        DPE(1) = DPE(1) + Q( LVCOR + 7 )
        DPE(2) = DPE(2) + Q( LVCOR + 8 )
        DPE(3) = DPE(3) + Q( LVCOR + 9 )
        DPE(4) = DPE(4) + Q( LVCOR + 10 )
        DPE(5) = DPE(5) + Q( LVCOR + 12)
        LVCOR = LQ( LVCOR )
      ENDDO
C
C ********************************************************************
C ****  Drop PNUT(4,5) if present
C ********************************************************************
C
      LBANK = GZPNUT(5)
      IF ( LBANK .GT. 0 ) THEN
C
C: Debug
C
        OLD_PNUT5( 1 ) = Q( LBANK + 3 )
        OLD_PNUT5( 2 ) = Q( LBANK + 4 )
        OLD_PNUT5( 3 ) = Q( LBANK + 14)
        OLD_PNUT5( 4 ) = Q( LBANK + 10)
        CALL MZDROP(IXCOM,LBANK,' ')
      ENDIF
C
C ****  Drop PNUT(4) if present
C
      LBANK = GZPNUT(4)
      IF ( LBANK .GT. 0 ) THEN
C
C: Debug
C
        OLD_PNUT4( 1 ) = Q( LBANK + 3 )
        OLD_PNUT4( 2 ) = Q( LBANK + 4 )
        OLD_PNUT4( 3 ) = Q( LBANK + 14 )
        OLD_PNUT4( 4 ) = Q( LBANK + 10)
        CALL MZDROP(IXCOM,LBANK,' ')
      ENDIF
C
C ********************************************************************
C ****  Book PNUT(4) and PNUT(5)
C ********************************************************************
C
      DO IPASS = IPASS_LOW, IPASS_HIGH
        CALL BKPNUT(IPASS)
        LBANK = GZPNUT(IPASS)
        IF ( LBANK .LE. 0 ) THEN
          WRITE(MESS(1:4),'(I4)') IPASS
          MESS =
     &        ' FAILED to book PNUT bank with pass number '//MESS(1:4)
          CALL ERRMSG('PNUT_BOOKING_FAILED','CAFIX',MESS,'W')
          GOTO 999
        ENDIF
      ENDDO
C
C ********************************************************************
C ****  Must have PNUT(2)
C ********************************************************************
C
      LPNUT = GZPNUT(2)
      IF ( LPNUT .LE. 0 ) THEN
        CALL ERRMSG
     &      ('NO_PNUT_BANK 2','CAFIX','GZPNUT(2) returned ZERO','W')
        GOTO 999
      ENDIF
C
C***********************************************************************
C ****  Copy PNUT(2) into PNUT(4) and PNUT(5) and add sum of VCOR's.
C ****  also add good muons into PNUT(5).
C********************************************************************
C
      DO IPASS = IPASS_LOW, IPASS_HIGH
        LBANK = GZPNUT(IPASS)
        LPNUT = GZPNUT( 2 )
        ND    = MIN(IQ(LPNUT-1),IQ(LBANK-1))
        CALL UCOPY(Q(LPNUT+1),Q(LBANK+1),ND)

C
C ********************************************************************
C ****  Loop over muons and add clean ones to PNUT5
C ********************************************************************
C
        IF ( IPASS .EQ. 5 ) THEN
          LPMUO = GZPMUO(0)
          DO WHILE ( LPMUO .GT. 0 )
            CALL CLEANMU( LPMUO, RET_MASK, OK )
            IF ( IAND( RET_MASK, GOOD_MUON_MASK ) .EQ. 0 ) THEN
              Q( LBANK + 3 ) = Q( LBANK + 3 ) - Q( LPMUO + 10 )
              Q( LBANK + 4 ) = Q( LBANK + 4 ) - Q( LPMUO + 11 )
              Q( LBANK + 5 ) = Q( LBANK + 5 ) - Q( LPMUO + 12 )
              Q( LBANK + 6 ) = Q( LBANK + 6 ) - Q( LPMUO + 13 )
              Q( LBANK + 14) = Q( LBANK + 14) + Q( LPMUO + 14 )   ! Scalar ET
            ENDIF
            LPMUO          = LQ( LPMUO )
          ENDDO
        ENDIF
C
C*********************************************************************
C ****  Add VCOR sums and update rest of the words
C************************************************************************
C
        Q( LBANK + 3 )  = Q( LBANK + 3 ) - DP(1)            ! 5 Vector P
        Q( LBANK + 4 )  = Q( LBANK + 4 ) - DP(2)
        Q( LBANK + 5 )  = Q( LBANK + 5 ) - DP(3)
        Q( LBANK + 6 )  = SQRT( Q(LBANK+3)**2 + Q(LBANK+4)**2 +
     &      Q(LBANK+5)**2)
        Q( LBANK + 7 )  = SQRT( Q( LBANK + 3 )**2 + Q( LBANK + 4 )**2
     &      )
        Q( LBANK + 14)  = Q( LBANK + 14 ) + DP(5)            ! Scalar ET
        Q( LBANK + 11)  = Q( LBANK + 11 ) + DPE(1)            ! Errors
        Q( LBANK + 12)  = Q( LBANK + 12 ) + DPE(2)
        Q( LBANK + 13)  = Q( LBANK + 13 ) + DPE(5)
        Q( LBANK + 15)  = Q( LBANK + 15 ) + DPE(3)
C                                                          ! phi,theta,eta
        IF (Q( LBANK + 4 ).NE.0.0 .OR. Q( LBANK + 3 ).NE.0.0) THEN
          Q( LBANK + 10)  = ATAN2( Q( LBANK + 4 ), Q( LBANK + 3 ) )
        ELSE
          Q( LBANK + 10) = 0.0
          CALL ERRMSG('No Event MET','CAFIX','Empty PNUT/VCOR Bank',
     &      'W')
        ENDIF
        IF ( Q( LBANK + 10 ) .LT. 0. ) Q( LBANK + 10 ) = Q( LBANK + 10
     &      ) + 2*SNGL(PI)
        EZOE            = -Q( LBANK + 5 )/MAX( SMALL, Q(LBANK + 6 ) )
        THETA           = ACOS( EZOE )
        ETA             = 10.*SIGN(1., EZOE )
        IF ( ABS(EZOE).LT.0.999 ) ETA=-ALOG(TAN(THETA/2.))
        Q( LBANK + 8 )  = THETA
        Q( LBANK + 9 )  = ETA

      ENDDO

      RETURN


C
C ****  INITIALIZE CAFIX
C
      ENTRY CAFIX_BEGIN()
      CALL INRCP('CAFIX_RCP',STATUS)    ! Read in RCP file
      OK = STATUS .EQ. 0
      IF ( .NOT. OK ) THEN
        CALL ERRMSG
     &      ('NO_CAFIX_RCP','CAFIX_BEGIN','No CAFIX.RCP file','W')
      ENDIF
      CAFIX_BEGIN = OK
      CAFIX_BEGIN = ( CAFIX_BEGIN .AND. CORRECTEM_BEGIN() )
      RETURN
C
C ****  TERMINATE CAFIX
C
      ENTRY CAFIX_END()
      CAFIX_END = .TRUE.
      CAFIX_END = ( CAFIX_END .AND. CORRECTEM_END() )
  999 RETURN
      END
