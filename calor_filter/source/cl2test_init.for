      FUNCTION CL2TEST_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-              TEST ZEBRA INITIALIZATION FOR LEVEL 2;
C-              set up as package USRINI hook
C-   Inputs  : NONE (reads in CADT)
C-   Outputs :  CAGS, new CADT, zebra surveys, dump of bank(s), histograms;
C-   Controls:  CL2TEST_RCP
C-
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$LINKS:IZSCPH.LINK'
      INCLUDE 'D0$LINKS:IZSL2H.LINK'
      INCLUDE 'D0$LINKS:IZ2CAGS.LINK'
      INTEGER LSTPO,LSTPC,LSTPN,LSCPH,GZSL2H,LSL2H,L2CAGS
      REAL TSEC                         ! time interval and sums in CPU seconds
      INTEGER LUN,USUNIT,IER            ! printing unit
C
      LOGICAL CL2TEST_INIT
      LOGICAL INI_TEST,DROP_GEOM,PRINT_CAGS,PRINT_CADT
      LOGICAL CALOR_INI,CHTINI,CL2HITS_INI,EZERROR,OK
C----------------------------------------------------------------------
      SAVE LUN
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        LUN = USUNIT()                  ! user summary unit
        CALL HCDIR('//PAWC',' ')          ! go to top directory
        CALL HMDIR('CL2TEST_INIT','S')       ! create CL2TEST directory
        CALL INRCP('CL2TEST_RCP',IER)    ! Read in RCP file
        IF (IER .EQ. 0) THEN
          CALL EZPICK('CL2TEST_RCP')       ! Select bank
          OK = .NOT.EZERROR(IER)
        ENDIF
        IF (IER .EQ. 0) CALL EZGET('INI_TEST',INI_TEST,IER)
        IF (IER .EQ. 0) CALL EZGET('DROP_GEOM',DROP_GEOM,IER)
        IF (IER .EQ. 0) CALL EZGET('PRINT_CAGS',PRINT_CAGS,IER)
        IF (IER .EQ. 0) CALL EZGET('PRINT_CADT',PRINT_CADT,IER)
        IF (IER .NE. 0) THEN      ! Error reading RCP
          CALL ERRMSG('CL2TEST','CL2TEST_INIT',
     &  ' Error while reading CL2TEST_RCP','F')
          CALL EZDUMP(LUN,0,0)
        ELSE
          CALL EZDUMP(LUN,0,0)
          CALL EZRSET
        ENDIF
C...time my part of initialization
        CALL TIMED(TSEC)                ! CERN interval timer
        OK = CALOR_INI()
        OK = OK.AND.CHTINI()
        OK = OK.AND.CL2HITS_INI()
        CALL TIMED(TSEC)
        CALL EZPICK('CL2HITS_RCP')
        CALL EZDUMP(LUN,0,0)
        CALL EZRSET
        CALL EZPICK('CAHITS_RCP')
        CALL EZDUMP(LUN,0,0)
        CALL EZRSET
        WRITE(LUN,100)TSEC
  100   FORMAT(' CL2_MAKE_TABLES TOOK ',F16.1,' SECONDS')
C...finish initializing
        IF (INI_TEST) THEN
          CALL DZVERI('check STP',IXSTP,'CFLSU')
          CALL DZSURV('CL2 CREATION SURVEY',IXSTP,LSTPH)  ! in unit 3
          CALL CL2TEST_CAGS_HISTS        
          IF(PRINT_CADT) CALL PRCADT(LUN,0,0,'ALL',3)
          IF(PRINT_CAGS) THEN
            LSL2H = GZSL2H()
            L2CAGS = LC(LSL2H-IZ2CAGS)          ! see if already there
            CALL PRCAGS(LUN,L2CAGS,0,'ONE',3)
          ENDIF
C
C...simulation of dropping which Jan's program will do
          IF (DROP_GEOM) THEN
            LSTPO = LC(LSTPH -IZSTPO )
            IF (LSTPO.GT.0) CALL MZDROP(IXSTP,LSTPO,' ')
            LSTPC = LC(LSTPH -IZSTPC)
            IF (LSTPC.GT.0) CALL MZDROP(IXSTP,LSTPC,' ')
            LSTPN = LC(LSTPH -IZSTPN )
            IF (LSTPN.GT.0) CALL MZDROP(IXSTP,LSTPN,' ')
            LSCPH = LC(LSTPH -IZSCPH )
            IF (LSCPH.GT.0) CALL MZDROP(IXSTP,LSCPH,' ')
            CALL DZSURV('CL2 DOWNLOAD SURVEY',IXSTP,LSTPH)
          ENDIF
        ENDIF
        FIRST = .FALSE.
      ENDIF
      CL2TEST_INIT = .TRUE.
  999 RETURN
      END
