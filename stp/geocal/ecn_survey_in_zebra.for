      SUBROUTINE ECN_SURVEY_IN_ZEBRA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TAKE ECN SURVEY DATA FROM 'RCP' FILE AND PUT
C-                         IT INTO ZEBRA BANKS FOR EASE OF USE.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   21-JUL-1992   Stephen Kahn
C-   Updated   22-MAR-2004   sss - use idate2k instead of idate.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CALOR_SURVEY.INC'
      INTEGER NMODUL, IERR, I, J, K, ID, IARRAY(50), NUMV, NTOT
      INTEGER IP, JMEAS, NMEAS, INT_DATE
      CHARACTER*4 MODUL_NAME
      CHARACTER*12 CODE1, CODE2, CODE3, CODE4
      CHARACTER*1 C1
      REAL    SARRAY(50)
      LOGICAL USE_THERMAL
      PARAMETER (NMODUL = 16)
C
      IDVSURV = IXSTP + 3               ! create division for survey
                                        ! information
C
      CALL EZPICK('ECN_SURVEY_RCP')
C
      CALL BKCSRV(LCSRV)                ! book CSRV for ECN
      IC(LCSRV + ISREGN) = 2            ! identifies ECN
      CALL EZGET('SURVEY_DATE',SARRAY,IERR)
      IC(LCSRV + ISDATE) = INT_DATE(SARRAY)   ! date of survey
      CALL IDATE2k( I, J, K)              ! today's date
      IC(LCSRV + ISDAT2) = 10000*K + 100*I + J  ! yymmdd format
C
C ... ECN CALORIMETER EM MODULES
C
      CALL BKCSYL(LCSYL)                ! book CSYL -- ECN/EM header bank
      IC(LCSYL+ISCYLN) = 1              ! cylinder identification for EM
C
      NMEAS = 5                         ! number of measurements
      CALL BKCMDL(LCMDL, NMEAS)         ! book CMDL -- Survey Module
                                        ! bank
      C(LCMDL+ISMODL) = 4HNCEM          ! identifies module type
      IC(LCMDL+ISNUMB) = 0              ! Kroon module location number
      IC(LCMDL+ISNMSR) = NMEAS          ! number of measurments for module
C
      CALL EZGET('ECN_EMBOTT', SARRAY, IERR)      ! measurement of
                                                  ! bottom support pl marker
      IF(IERR .EQ. 0) CALL UCOPY( SARRAY(5), C(LCMDL+ISNBX),3)
      CALL EZGET('ECN_EMWEST', SARRAY, IERR)      ! measurement of
                                                  ! west support pl marker
      IF(IERR .EQ. 0) CALL UCOPY( SARRAY(5), C(LCMDL+ISNWX),3)
      CALL EZGET('ECN_EMTOP', SARRAY, IERR)       ! measurement of
                                                  ! top support pl marker
      IF(IERR .EQ. 0) CALL UCOPY( SARRAY(5), C(LCMDL+ISNTX),3)
      CALL EZGET('ECN_EMEAST', SARRAY, IERR)      ! measurement of
                                                  ! east support pl marker
      IF(IERR .EQ. 0) CALL UCOPY( SARRAY(5), C(LCMDL+ISNEX),3)
      CALL EZGET('ECN_EMCTR_PLUG', SARRAY, IERR)  ! measurement of cntr
                                                  ! plug
      IF(IERR .EQ. 0) CALL UCOPY( SARRAY(5), C(LCMDL+ISUCX),3)
C
C ... ECN CALORIMETER IH MODULES
C
      CALL BKCSYL(LCSYL)                ! book CSYL -- ECN/IH header bank
      IC(LCSYL+ISCYLN) = 2              ! cylinder identification for IH
C
      NMEAS = 8                         ! number of measurements
      CALL BKCMDL(LCMDL, NMEAS)         ! book CMDL -- Survey Module
                                        ! bank
      C(LCMDL+ISMODL) = 4HNCIH          ! identifies module type
      IC(LCMDL+ISNUMB) = 0              ! Kroon module location number
      IC(LCMDL+ISNMSR) = NMEAS          ! number of measurments for module
C
      CALL EZGET('ECN_NIHBOTT.5N', SARRAY, IERR)  ! measurement of
                                                  ! downstream bottom ball
      IF(IERR .EQ. 0) CALL UCOPY( SARRAY(5), C(LCMDL+ISNBX),3)
      CALL EZGET('ECN_NIHWEST.5N', SARRAY, IERR)  ! measurement of
                                                  ! downstream west ball
      IF(IERR .EQ. 0) CALL UCOPY( SARRAY(5), C(LCMDL+ISNWX),3)
      CALL EZGET('ECN_NIHTOP_.5N', SARRAY, IERR)  ! measurement of
                                                  ! downstream top ball
      IF(IERR .EQ. 0) CALL UCOPY( SARRAY(5), C(LCMDL+ISNTX),3)
      CALL EZGET('ECN_NIHEAST.5N', SARRAY, IERR)  ! measurement of
                                                  ! downstream east ball
      IF(IERR .EQ. 0) CALL UCOPY( SARRAY(5), C(LCMDL+ISNEX),3)
      CALL EZGET('ECN_SIHBOTT.5S', SARRAY, IERR)  ! measurement of
                                                  ! upstream bottom ball
      IF(IERR .EQ. 0) CALL UCOPY( SARRAY(5), C(LCMDL+ISSBX),3)
      CALL EZGET('ECN_SIHWEST.5S', SARRAY, IERR)  ! measurement of
                                                  ! upstream west ball
      IF(IERR .EQ. 0) CALL UCOPY( SARRAY(5), C(LCMDL+ISSWX),3)
      CALL EZGET('ECN_SIHTOP_.5S', SARRAY, IERR)  ! measurement of
                                                  ! upstream top ball
      IF(IERR .EQ. 0) CALL UCOPY( SARRAY(5), C(LCMDL+ISSTX),3)
      CALL EZGET('ECN_SIHEAST.5S', SARRAY, IERR)  ! measurement of
                                                  ! upstream east ball
      IF(IERR .EQ. 0) CALL UCOPY( SARRAY(5), C(LCMDL+ISSEX),3)
C
C ... NORTH CALORIMETER MH MODULES
C
      CALL BKCSYL(LCSYL)                ! book CSYL -- ECN/MH header bank
      IC(LCSYL+ISCYLN) = 3              ! cylinder identification for MH
C
      NMEAS = 32
      CALL BKCMDL( LCMDL, NMEAS)        ! book CMDL -- Survey measurement bank
      C( LCMDL + ISMODL) = 4HNCMH       ! identifies module type
      C( LCMDL + ISNUMB) = 0            ! all measurments are put in one bank,
                                        ! so no number identifier is used
      IC(LCMDL + ISNMSR) = NMEAS        ! number of measurements in bank
      DO 250 I = 1, NMODUL
        IP = 2*NMWDS*(I-1) + ISNIX      ! pointer to first measurement for
                                        ! module
        IF( I.GE.13) THEN               ! get module identification from
          J = I - 12                    ! GEANT module number (used merely to
        ELSE IF ( I.LE.5) THEN          ! order measurements)
          J = I + 4
        ELSE
          J = I - 14
        END IF
        IF( J.EQ.1 .OR. J.EQ.9) THEN    ! encode parameter name for retrieval
          WRITE (C1,'(I1.1)') J         ! from RCP
          CODE1 = 'ECN_NMH'//C1//'IN'   ! RCP name for downstream inner
                                        ! measurement
          CODE2 = 'ECN_SMH'//C1//'OUT'  ! RCP name for upstream outer
                                        ! measurement
        ELSE IF (J.LE.-2 .AND. J.GE.-8) THEN
          WRITE (C1,'(I1.1)') -J        ! negative numbers are LEFT
                                        ! modules
          CODE1 = 'ECN_NMH'//C1//'LIN'  ! RCP name for downstream inner
                                        ! measurement
          CODE2 = 'ECN_SMH'//C1//'LOUT' ! RCP name for upstream outer
                                        ! measurement
        ELSE IF (J.GE.2 .AND. J.LE.8) THEN
          WRITE (C1,'(I1.1)') J         ! these numbers are RIGHT
                                        ! modules
          CODE1 = 'ECN_NMH'//C1//'RIN'  ! RCP name for downstream inner
                                        ! measurement
          CODE2 = 'ECN_SMH'//C1//'ROUT' ! RCP name for upstream outer
                                        ! measurement
        ELSE
          CALL ERRMSG('INCORR_MH_ID','ECN_SURVEY_IN_ZEBRA',
     +      'incorrect module ID code','S')
          CALL VZERO( C(LCMDL+IP), 2*NMWDS)
          GO TO 250
        END IF
C
        CALL EZGET( CODE1, SARRAY, IERR) ! get downstream inner measurements
        IF(IERR .EQ. 0) THEN
          CALL UCOPY( SARRAY(5), C(LCMDL+IP), 3)
        ELSE
          CALL VZERO(C(LCMDL+IP), NMWDS)
        END IF
        CALL EZGET( CODE2, SARRAY, IERR) ! get upstream outer measurements
        IF(IERR .EQ. 0) THEN
          CALL UCOPY( SARRAY(5), C(LCMDL+IP+NMWDS), 3)
        ELSE
          CALL VZERO(C(LCMDL+IP+NMWDS), NMWDS)
        END IF
  250 CONTINUE
C
C ... NORTH CALORIMETER OH MODULES
C
      CALL BKCSYL(LCSYL)                ! book CSYL -- ECNC/OH header bank
      IC(LCSYL+ISCYLN) = 4              ! cylinder identification for OH
C
      NMEAS = 64                       ! number of measurements per module
      CALL BKCMDL(LCMDL, NMEAS)       ! book CMDL -- Survey Module
                                        ! bank
      C( LCMDL + ISMODL) = 4HNCOH     ! identifies module type
      IC( LCMDL + ISNUMB) = 0         ! module serial number
      IC( LCMDL + ISNMSR) = NMEAS     ! number of measurements for module
      DO 350 I = 1, NMODUL
        IP = 4 * NMWDS * (I-1)
        IF( I.GE.13) THEN               ! get module identification from
          J = I - 12                    ! GEANT module number (used merely to
        ELSE IF (I.LE.4) THEN          ! order measurements)
          J = I + 4
        ELSE
          J = I - 13
        END IF
        IF (J.LE.-1 .AND. J.GE.-8) THEN
          WRITE (C1,'(I1.1)') -J        ! negative numbers are LEFT
                                        ! modules
          CODE1 = 'ECN_NOH'//C1//'LIN'  ! RCP name for downstream inner
                                        ! measurement
          CODE2 = 'ECN_NOH'//C1//'LOUT' ! RCP name for downstream outer
                                        ! measurement
          CODE3 = 'ECN_SOH'//C1//'LIN'  ! RCP name for upstream inner
                                        ! measurement
          CODE4 = 'ECN_SOH'//C1//'LOUT' ! RCP name for upstream outer
                                        ! measurement
        ELSE IF (J.GE.1 .AND. J.LE.8) THEN
          WRITE (C1,'(I1.1)') J         ! these numbers are RIGHT
                                        ! modules
          CODE1 = 'ECN_NOH'//C1//'RIN'  ! RCP name for downstream inner
                                        ! measurement
          CODE2 = 'ECN_NOH'//C1//'ROUT' ! RCP name for downstream outer
                                        ! measurement
          CODE3 = 'ECN_SOH'//C1//'RIN'  ! RCP name for upstream inner
                                        ! measurement
          CODE4 = 'ECN_SOH'//C1//'ROUT' ! RCP name for upstream outer
                                        ! measurement
        ELSE
          CALL ERRMSG('INCORR_OH_ID','ECN_SURVEY_IN_ZEBRA',
     +      'incorrect module ID code','S')
          CALL VZERO( C(LCMDL+IP+ISNIX), 4*NMWDS)
          GO TO 350
        END IF
C
        CALL EZGET(CODE1, SARRAY, IERR) ! downstream inner measurement
        IF ( IERR .EQ. 0) THEN
          CALL UCOPY(SARRAY(5), C(LCMDL+IP+ISNIX), 3)
        ELSE
          CALL VZERO( C(LCMDL+ISNIX+IP), NMWDS)
        END IF
        CALL EZGET(CODE2, SARRAY, IERR) ! downstream outer measurement
        IF ( IERR .EQ. 0) THEN
          CALL UCOPY(SARRAY(5), C(LCMDL+IP+ISNOX), 3)
        ELSE
          CALL VZERO( C(LCMDL+ISNOX+IP), NMWDS)
        END IF
        CALL EZGET(CODE3, SARRAY, IERR) ! upstream inner measurement
        IF ( IERR .EQ. 0) THEN
          CALL UCOPY(SARRAY(5), C(LCMDL+IP+ISSIX), 3)
        ELSE
          CALL VZERO( C(LCMDL+ISSIX+IP), NMWDS)
        END IF
        CALL EZGET(CODE4, SARRAY, IERR) ! upstream outer measurment
        IF ( IERR .EQ. 0) THEN
          CALL UCOPY(SARRAY(5), C(LCMDL+IP+ISSOX), 3)
        ELSE
          CALL VZERO( C(LCMDL+ISSOX+IP), NMWDS)
        END IF
  350 CONTINUE
C
C----------------------------------------------------------------------
  999 RETURN
      END
