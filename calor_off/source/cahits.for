      FUNCTION CAHITS ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      Call subroutines to generate hit banks CAEP, CAEH, CATE, & PNUT
C-
C-   Created  17-JAN-1989   Serban D. Protopopescu
C-   Updated   3-APR-1992   Chip Stewart  Added switches for each bank
C-   Updated  17-MAR-1993   Joan Guida  Added call to CHOTINFO_EVENT
C-   Updated  23-MAR-1993   Marc Paterno  Added call to AIDA
C-   Updated   7-FEB-1994   Rich Astur  Added CAEQ logic and calls
C-   Updated   8-MAR-1994   Chip Stewart  Added CATD fake CAEP ability
C-   Updated  18-JUN-1995   Chip Stewart  Automatic sense CAEQ on input
C-   Updated   6-NOV-1995   Dhiman Chakraborty   Drop all existing PNUT banks
C-                                            to begin with
C-   Updated  15-NOV-1995   Andrew Brandt   Activate DO_ANALYSIS switch
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL OK, CAHITS, CHTINI
      INTEGER LPNUT,IER
      INTEGER GZPARH,GZPNUT,USUNIT, GZCAEP
      EXTERNAL  GZPARH,GZPNUT,USUNIT,GZCAEP
      LOGICAL FIRST, DO_CAEPFL, DO_CAEHFL, DO_CATEFL, DO_C1PMET
      LOGICAL DO_C2PMET, FILL_PTCAEP, DO_ANALYSIS, DO_HOTSUP, DO_HOTFIND
      LOGICAL DO_AIDA,DO_ICD_CORR
      LOGICAL DO_CAEQFL, DOCAEQ_TO_CAEP,DOCATD_TO_CAEP
      SAVE    DO_CAEQFL, DOCAEQ_TO_CAEP,DOCATD_TO_CAEP,DO_ICD_CORR
      REAL    CAL_BLAST_ET_CUT,SCALAR_ET
      CHARACTER*80 MSG
      DATA FIRST /.TRUE./
      DATA OK    /.TRUE./

C----------------------------------------------------------------------
      IF ( FIRST ) THEN                     ! LOCAL INIT
        OK = CHTINI ()
        CALL EZPICK ( 'CAHITS_RCP' )
        CALL EZERR ( IER )
        IF ( IER.EQ.0 ) THEN
          CALL EZGET ( 'DO_CAEPFL',   DO_CAEPFL, IER )
          IF ( IER.NE.0 ) CALL ERRMSG ( 'NO_CAEPFL_SWITCH', 'CAHITS',
     &      'USE RECENT CAHITS_RCP FORMAT', 'W' )
          IER = 0
          CALL EZGET( 'DO_CAEQFL', DO_CAEQFL, IER )
          IF ( IER .EQ. 0 ) CALL EZGET( 'DOCAEQ_TO_CAEP', DOCAEQ_TO_CAEP
     &      ,IER )
          IF ( IER .NE. 0 ) THEN
            CALL ERRMSG('NO CAEQ SWITCH','CAHITS',
     &        'USE RECENT CAHITS_RCP FORMAT','W')
            DO_CAEQFL = .TRUE.
            DOCAEQ_TO_CAEP = .FALSE.
          ELSE
            CALL CAEQ_CHECK(DOCAEQ_TO_CAEP)
            IF(DOCAEQ_TO_CAEP)CALL ERRMSG ( 'USE_CAEQ', 'CAHITS',
     &        'CAEP BUILT FROM CAEQ INPUT','I')
          ENDIF
C***  Drop all PNUT banks unconditionally
          LPNUT = GZPNUT(0)
          IF(LPNUT.GT.0) CALL MZDROP(IXCOM,LPNUT,'L')
C***          
          CALL EZGET ( 'DO_CAEHFL',   DO_CAEHFL, IER )
          CALL EZGET ( 'DO_CATEFL',   DO_CATEFL, IER )
          CALL EZGET ( 'DO_C1PMET',   DO_C1PMET, IER )
          CALL EZGET ( 'DO_C2PMET',   DO_C2PMET, IER )
          CALL EZGET ( 'FILL_PTCAEP', FILL_PTCAEP, IER )
          CALL EZGET ( 'DO_ANALYSIS', DO_ANALYSIS, IER )
          CALL EZGET ( 'DO_AIDA',     DO_AIDA, IER)
          CALL EZGET ( 'DO_ICD_CORR', DO_ICD_CORR, IER )
          IF ( IER .NE. 0 ) THEN
            DO_ICD_CORR = .FALSE.
            CALL ERRMSG ( 'NO_CAHITS_RCP', 'CAHITS',
     &        'DO_ICD_CORR not found in CAHITS.RCP, setting FALSE','I' )
          ENDIF
          IF ( IER .NE. 0 ) THEN
            DO_AIDA = .FALSE.
            CALL ERRMSG ( 'NO_CAHITS_RCP', 'CAHITS',
     &        'DO_AIDA not found in CAHITS.RCP, setting FALSE', 'I' )
          ENDIF

          CALL EZGET ( 'DO_HOTSUP',   DO_HOTSUP, IER )
          IF ( IER.NE.0 ) THEN
            DO_HOTSUP=.FALSE.
            CALL ERRMSG ( 'NO_CAHITS_RCP', 'CAHITS',
     &        'DO_HOTSUP not found in CAHITS.RCP, setting FALSE', 'I' )
          ENDIF

          CALL EZGET ( 'DO_HOTFIND',  DO_HOTFIND, IER )
          IF ( IER.NE.0 ) THEN
            DO_HOTFIND=.FALSE.
            CALL ERRMSG ( 'NO_CAHITS_RCP', 'CAHITS',
     &        'DO_HOTFIND not found in CAHITS.RCP, setting FALSE', 'I' )
          ENDIF

          IF ( DO_HOTSUP.AND.DO_HOTFIND ) THEN
            DO_HOTSUP=.FALSE.
            DO_HOTFIND=.FALSE.
            CALL ERRMSG ( 'BAD_PARAMETER', 'CAHITS',
     &
     &'DO_HOTSUP and DO_HOTFIND cannot both be true setting FALSE',
     &        'I' )
          ENDIF

          DOCATD_TO_CAEP = .FALSE.
          CALL EZGET( 'DO_CATD_TO_CAEP', DOCATD_TO_CAEP, IER )  ! silent
          IF ( DOCATD_TO_CAEP ) THEN
            CALL ERRMSG('CREATING_CAEP_FROM_CADT','CAHITS',
     &        '- TURNING OFF AIDA,HOTSUP and HOTFIND!','W')
            DO_AIDA         = .FALSE.
            DO_HOTSUP       = .FALSE.
            DO_HOTFIND      = .FALSE.
            DOCAEQ_TO_CAEP  = .FALSE. ! don't do CAEQ too !
          ENDIF

          CALL EZGET ( 'CAL_BLAST_ET_CUT',  CAL_BLAST_ET_CUT, IER )
          IF ( IER.NE.0 ) THEN
            CAL_BLAST_ET_CUT = 15.0E3  ! 15 TeV default 
            CALL ERRMSG ( 'NO_CAL_BLAST_ET_CUT', 'CAHITS',
     &        'CAL_BLAST_ET_CUT not in CAHITS.RCP, set at 15 TEV', 'I' )
          ENDIF

        ELSE
          CALL ERRMSG ( 'NO_CAHITS_RCP', 'CAHITS',
     &      'CAHITS OUT OF CONTROL', 'W' )
        END IF
        CALL EZRSET
        FIRST = .FALSE.
      ENDIF
C
      CAHITS=.TRUE.
      IF ( DOCATD_TO_CAEP ) CALL CATD_TO_CAEP ! fake CAEP with CATD tower info
      IF ( DOCAEQ_TO_CAEP ) CALL CAEQ_TO_CAEP ! Make CAEP with CAEQ
      IF ( DO_CAEPFL )   CALL CAEPFL ( OK )   ! fill CAEP
      IF ( GZCAEP() .LE. 0 ) GOTO 999         ! No CAEP - error
      IF ( DO_CAEQFL  )  CALL CAEQFL          ! Book/fill compressed CAEP
      IF ( DO_ICD_CORR ) CALL ICD_CORR        ! Correct ICD in RECO v12_20
      IF ( DO_HOTFIND )  CALL CHOTINFO_EVENT ()
      IF ( DO_CAEHFL )   CALL CAEHFL   ! fill CAEH
      IF ( FILL_PTCAEP ) CALL CPTCAF   ! fill PTCAEP pointer array
      IF ( DO_AIDA )     CALL AIDA     ! hot cell removal
      IF ( GZPARH() .EQ.0 ) CALL PARHFL ! fill PARH only if needed
      IF ( DO_C1PMET )    CALL C1PMET  ! do missing Et ( first pass )
      IF ( DO_C2PMET )    CALL C2PMET  ! EC/CC corrections & missing Et 2nd pass
C
C ****  SKIP PROCESSING IF SCALAR ET is beyond CAL_BLAST_ET_CUT
C
      IF(CAL_BLAST_ET_CUT.NE.0) THEN
        CALL C2PMET_SCALAR_ET(SCALAR_ET)
        IF(SCALAR_ET.GT.CAL_BLAST_ET_CUT) THEN
          WRITE(MSG,10)SCALAR_ET,CAL_BLAST_ET_CUT
   10     FORMAT('SCALAR ET ',E9.3,' > CUT ',E9.3)
          CALL ERRMSG ( 'CAL_BLAST', 'CAHITS',
     &      MSG, 'W' )
          CAHITS = .FALSE.
          GOTO 999
        END IF
      END IF
C
      CALL CPTCTZ                      ! zero PTCATE pointer array
      IF ( DO_CATEFL )    CALL CATEFL  ! fill CATE
      CALL CPTCTF                      ! fill PTCATE pointer array
      IF ( DO_ANALYSIS )   CALL CHTANL  ! CAHITS analysis routine


  999 RETURN
      END
