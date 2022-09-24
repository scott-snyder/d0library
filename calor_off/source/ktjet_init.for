C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_INIT.FOR
C *1     3-FEB-1994 14:38:57 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KTJET_INIT.FOR
      FUNCTION KTJET_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize KT algorithm package
C-
C-   Returned value  : TRUE if initialization completed without an error
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   8-JAN-1993   Richard V. Astur
c-   Updated   2-Nov-1995   Add histograms for tests
c-                          Gordon Watts
C-   Updated   10-Nov-1995  Add histograms for number of cells
c-                          Brad Abbott
c-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KTJET.INC'
      LOGICAL KTJET_INIT
      LOGICAL FIRST
      INTEGER IER, ILOOP, I, IPATH
      REAL KTHOLD
      CHARACTER*64 NOWERRMESS

      logical do_hist_temp, FLGVAL

      DATA FIRST /.TRUE./

C----------------------------------------------------------------------
      KTJET_INIT = .TRUE.
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C---Read in detector geometry
C
        CALL CALOR_INI
C
C---Read RCP
C
        CALL INRCP( 'KTJET_RCP', IER )
        IF (IER .NE. 0) THEN
          NOWERRMESS = ' RCP file open failed '
          GOTO 900
        END IF

        CALL EZPICK('KTJET_RCP')
        CALL EZERR(IER)
        IF (IER .NE. 0) THEN
          NOWERRMESS = ' Cannot find KTPJET bank'
          GOTO 900
        END IF

        NOWERRMESS = ' RCP parameter not found'
        CALL EZGET_i('IPRINT', IPRINT, IER)
        IF (IER .NE. 0) GOTO 900
        CALL EZGET_l('DO_PRECLUSTER', DO_PRECLUSTER, IER)
        IF (IER .NE. 0) GOTO 900
        CALL EZGET_l('DO_POSTCLUSTER', DO_POSTCLUSTER, IER)
        IF (IER .NE. 0) GOTO 900
        CALL EZGET_l('DO_SUBJETS', DO_SUBJETS, IER)
        IF (IER .NE. 0) GOTO 900
        CALL EZGET_i('IFSCALE', IFSCALE, IER)
        IF (IER .NE. 0) GOTO 900
        CALL EZGET_iarr('INPUT_TYPE', INPUT_TYPE_KT, IER)
        IF (IER .NE. 0) GOTO 900
        CALL EZGET_rarr('D_SEP', D_SEP_KT, IER)
        IF (IER .NE. 0) GOTO 900
        CALL EZGET_rarr('F_CUT', F_CUT_KT, IER)
        IF (IER .NE. 0) GOTO 900
        CALL EZGET_i('NJETS', NJETS, IER)
        IF (IER .NE. 0) GOTO 900
        CALL EZGET_l('DO_PRECLUSTER', DO_PRECLUSTER, IER)
        IF (IER .NE. 0) GOTO 900
        CALL EZGET_l('MAKE_KTCL', MAKE_KTCL, IER)
        IF (IER .NE. 0) GOTO 900
        CALL EZGETA('KTCUT', 0, 0, 0, N_KTCUTS, IER)
        IF (IER .NE. 0) GOTO 900
        CALL EZGETA('INPUT_TYPE', 0, 0, 0, N_KTALG, IER)
        IF (IER .NE. 0) GOTO 900
        CALL EZGET('KTCUT', KTCUT(1), IER )
        IF (IER .NE. 0) GOTO 900
        CALL EZGET_i('IPATH', IPATH, IER )
        IF (IER .NE. 0) GOTO 900
        CALL EZGET_l('KTLOG10', KTLOG10, IER )
        IF (IER .NE. 0) GOTO 900
C
C: New stuff
C
        CALL EZGET('PREC_WIDTH', PREC_WIDTH, IER )
        IF (IER .NE. 0) GOTO 901
        CALL EZGET('MINIMUM_ET', MINIMUM_ET, IER )
        IF (IER .NE. 0) GOTO 901
        CALL EZGET_l('USE_DET_ETA', USE_DET_ETA, IER )
        IF (IER .NE. 0) GOTO 901
        CALL EZGET('KT_ET_MIN', KT_ET_MIN, IER )
        IF (IER .NE. 0) GOTO 901
        CALL EZGET('NEGATIVE_ET_CUT', NEGATIVE_ET_CUT, IER )
        IF (IER .NE. 0) GOTO 901
        CALL EZGET('MIN_PREC_ET', MIN_PREC_ET, IER )
        IF (IER .NE. 0) GOTO 901
        CALL EZGET_k('ET_ORDERED', ET_ORDERED, IER )
        IF (IER .NE. 0) GOTO 901
        CALL EZGET_l('ETA_ORDERED', ETA_ORDERED, IER )
        IF (IER .NE. 0) GOTO 901
        CALL EZGET_l('KT_MAKE_JETS', KT_MAKE_JETS, IER )
        IF (IER .NE. 0) GOTO 901
        CALL EZGET_l('REMOVE_LOW_ET_CELLS', REMOVE_LOW_ET_CELLS, IER )
        IF (IER .NE. 0) GOTO 901
        CALL EZGET_i('SEARCH_SIZE', SEARCH_SIZE, IER )
        IF (IER .NE. 0) GOTO 901
        CALL EZGET_i('NSCALE', NSCALE, IER )
        IF (IER .NE. 0) GOTO 901
        CALL EZGET_i('IETDEF', IETDEF, IER )
        IF (IER .NE. 0) GOTO 901
        CALL EZGET_i('IETADEF', IETADEF, IER )
        IF (IER .NE. 0) GOTO 901

        GOTO 902
C----------------- BAD READ
  901   CALL ERRMSG('OLD RCP','KTJET_INIT','Cant find new rcp args','F')
        GOTO 900
C---------------------------
  902   CONTINUE
        
C
C---Set path
C
        IF ( IPATH .EQ. 1 ) CALL PATHST('RECO')
        IF ( IPATH .EQ. 2 ) CALL PATHST('MDST')
C
C---Error if too many Cut values
C
        IF ( N_KTCUTS .GT. KTMAX_CUTS ) THEN
          CALL ERRMSG('Too many Ycut values','KTJET_INIT',
     &      'Too many Kt y cut values read from KTJET_RCP','E')
          KTJET_INIT = .FALSE.
          RETURN
        ENDIF
C
C---Order KTCUTS from smallest to largest
C
        ILOOP = 1
        DO WHILE (ILOOP .LT. N_KTCUTS )
          DO I = ILOOP + 1, N_KTCUTS
            IF ( KTCUT( I ) .LT. KTCUT( ILOOP ) ) THEN
              KTHOLD         = KTCUT( ILOOP )
              KTCUT( ILOOP ) = KTCUT( I )
              KTCUT( I )     = KTHOLD
            ENDIF
          ENDDO
          ILOOP = ILOOP + 1
        ENDDO
        IF ( KTLOG10 ) THEN
          DO I = 1, N_KTCUTS
            KTCUT( I ) = EXP( KTCUT(I)*ALOG( 10.))
          ENDDO
        ENDIF

C
C---Histograms.  They are only plotted if the rcp requests it or the
c   verify flag is on in the framework (d0reco).
C
        
        do_hist_temp = .false.
        call ezget_l ('DO_BASIC_HISTS', do_hist_temp, ier)
        do_basic_hist = do_hist_temp .or. FLGVAL('VERIFY')

        if (do_basic_hist) then

c
c  Create a KTJET subdirectory
c

           CALL HCDIR('//PAWC',' ')
           CALL HMDIR('KTJET','S')
           call hcdir ('//PAWC/KTJET', ' ')

c
c  Book several histograms
c

           call hbook1 (100, 'Number preclusters in ktcl bank',
     $          500, 0.0, 499.0, 0.0)
           call hbook1 (101, 'Eta of ktcl preclusters',
     $          40, -10.0, 10.0, 0.0)
           call hbook1 (102, 'phi of ktcl preclusters',
     $          40, 0.0, 7.0, 0.0)
           call hbook1 (103, 'Energy of ktcl preclusters',
     $          500, 0.0, 100.0, 0.0)
           call hbook1 (104, 'Et removed in precluster formation',
     $          100, 0.0, 5.0, 0.0)
           call hbook1 (105, 'Number of large negative et cells',
     $          100, 0.0, 99.0, 0.0)
           call hbook1 (106, 'Number words in ktcl bank',
     $          1000, 0.0, 999.0, 0.0)
           call hbook1 (107, 'Number of EM preclusters',
     $          150, 0.0, 150.0, 0.0)
           call hbook1 (108, 'Number of ICD preclusters',
     $          50, 0.0, 50.0, 0.0)
           call hbook1 (109, 'Number of FH preclusters',
     $          80, 0.0, 80.0, 0.0)
        endif

      ENDIF

C: Done
      GOTO 999
C
C---Error
C
  900 KTJET_INIT = .FALSE.
      CALL ERRMSG('KTJET init failed','KTJET_INIT',NOWERRMESS,'F')

  999 CALL EZRSET
      RETURN
      END
