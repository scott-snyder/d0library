      SUBROUTINE CC_SURVEY_IN_ZEBRA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TAKE CC SURVEY DATA FROM 'RCP' FILE AND PUT
C-                         IT INTO ZEBRA BANKS FOR EASE OF USE.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-MAR-1991   Stephen Kahn
C-   Updated  22-MAR-2004   sss - use idate2k instead of idate.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CALOR_SURVEY.INC'
      INTEGER NMODUL, IERR, I, J, K, ID, IARRAY(50), NUMV, NTOT
      INTEGER IP, JMEAS, INT_DATE
      CHARACTER*4 MODUL_NAME
      CHARACTER*12 CODE
      REAL    SARRAY(50)
      LOGICAL USE_THERMAL
      PARAMETER (NMODUL = 32)
C
      IDVSURV = IXSTP + 3               ! create division for survey
                                        ! information
      CALL MZDIV(IXSTP, IDVSURV, 'SURVEY', NWSURV, NWSURV_MX, ' ')
C
      CALL MZLINK(IXSTP, '/SURVEY/',LSURV, LCMDL, LCMDL)
C
      CALL EZPICK('CC_SURVEY_RCP')
C
      LCSRV = 0
      CALL BKCSRV(LCSRV)                ! book CSRV -- survey header
                                        ! block
      IC(LCSRV + ISREGN) = 1            ! identifies CC
      CALL EZGET('SURVEY_DATE',SARRAY,IERR)
      IC(LCSRV + ISDATE) = INT_DATE(SARRAY)   ! date of survey
      CALL IDATE2k( I, J, K)              ! today's date
      IC(LCSRV + ISDAT2) = 10000*K + 100*I + J  ! yymmdd format
C
C ... CENTRAL CALORIMETER EM MODULES
C
      CALL BKCSYL(LCSYL)                ! book CSYL -- CC/EM header bank
      IC(LCSYL+ISCYLN) = 1              ! cylinder identification for EM
C
      DO 150 I = 0, NMODUL-1
        WRITE( MODUL_NAME, '(A2,I2.2)') 'EM',I
        CALL EZGETI(MODUL_NAME, ID, IERR)
        IF( IERR .NE. 0) GO TO 150      ! if module does not exist, skip
        CALL BKCMDL(LCMDL,0)            ! book CMDL -- Survey Module
                                        ! bank
        CALL EZGET2(ID, 1, 0, 1, SARRAY, IARRAY, NUMV, NTOT, IERR)
        IF (IERR .NE. 0) GO TO 150
        C(LCMDL+ISMODL) = SARRAY(1)     ! identifies module type
        IC(LCMDL+ISNUMB) = I            ! Kroon module location number
        IC(LCMDL+ISNMSR) = NUMV/6       ! number of measurments for module
        DO 100 J = 1, NUMV, 6
          WRITE( CODE,'(3A4)') (SARRAY(K),K=J,J+2)
          IF(CODE(1:4) .NE. 'CCEM') GO TO 100
          IF( CODE(5:5) .EQ. 'N') THEN  ! establish pointer location to
            IP = ISNILX                 ! north or south end
          ELSE
            IP = ISSILX
          END IF
          IF( CODE(9:9) .EQ. 'o') THEN  ! establish pointer location to
            IP = IP + 2*NMWDS           ! inner or outer part
          END IF
          IF( CODE(10:10) .EQ. '+') THEN    ! establish pointer location
                                        ! to increasing (decreasing)
                                        ! azimuth
            IP = IP + NMWDS
          END IF
          CALL UCOPY( SARRAY(J+3), C(LCMDL+IP), 3)
          JMEAS = (IP-ISNILX)/NMWDS
          CALL SBIT1(C(LCMDL+ISMCOD),JMEAS+1)    ! code word describing
                                        ! which measurments were done
          IF( CODE(11:11) .EQ. 'A') THEN
            C(LCMDL+IP+3) = 0.05        ! BETS orientation error
          ELSE IF (CODE(11:11) .EQ. 'B') THEN
            C(LCMDL+IP+3) = 0.025
          ELSE
            C(LCMDL+IP+3) = 0.0
          END IF
  100   CONTINUE
  150 CONTINUE
C
C ... CENTRAL CALORIMETER FH MODULES
C
      CALL BKCSYL(LCSYL)                ! book CSYL -- CC/FH header bank
      IC(LCSYL+ISCYLN) = 2              ! cylinder identification for FH
C
      DO 250 I = 0, NMODUL-1
        WRITE( MODUL_NAME, '(A2,I2.2)') 'FH',I
        CALL EZGETI(MODUL_NAME, ID, IERR)
        IF( IERR .NE. 0) GO TO 250      ! if module does not exist, skip
        CALL BKCMDL(LCMDL,0)            ! book CMDL -- Survey Module
                                        ! bank
        CALL EZGET2(ID, 1, 0, 1, SARRAY, IARRAY, NUMV, NTOT, IERR)
        IF (IERR .NE. 0) GO TO 250
        C(LCMDL+ISMODL) = SARRAY(1)     ! identifies module type
        IC(LCMDL+ISNUMB) = I            ! Kroon module location number
        IC(LCMDL+ISNMSR) = NUMV/6       ! number of measurments for module
        DO 200 J = 1, NUMV, 6
          WRITE( CODE,'(3A4)') (SARRAY(K),K=J,J+2)
          IF(CODE(1:4) .NE. 'CCFH') GO TO 200
          IF( CODE(5:5) .EQ. 'N') THEN  ! establish pointer location to
            IP = ISNILX                 ! north or south end
          ELSE
            IP = ISSILX
          END IF
          IF( CODE(9:9) .EQ. 'o') THEN  ! establish pointer location to
            IP = IP + 2*NMWDS           ! inner or outer part
          END IF
          IF( CODE(10:10) .EQ. '+') THEN    ! establish pointer location
                                        ! to increasing (decreasing)
                                        ! azimuth
            IP = IP + NMWDS
          END IF
          CALL UCOPY( SARRAY(J+3), C(LCMDL+IP), 3)
          JMEAS = (IP-ISNILX)/NMWDS
          CALL SBIT1(C(LCMDL+ISMCOD),JMEAS+1)    ! code word describing
                                        ! which measurments were done
          IF( CODE(11:11) .EQ. 'A') THEN
            C(LCMDL+IP+3) = 0.05        ! BETS orientation error
          ELSE IF (CODE(11:11) .EQ. 'B') THEN
            C(LCMDL+IP+3) = 0.025
          ELSE
            C(LCMDL+IP+3) = 0.0
          END IF
  200   CONTINUE
  250 CONTINUE
C
C ... CENTRAL CALORIMETER CH MODULES
C
      CALL BKCSYL(LCSYL)                ! book CSYL -- CC/CH header bank
C
      DO 350 I = 0, NMODUL-1
        WRITE( MODUL_NAME, '(A2,I2.2)') 'CH',I
        CALL EZGETI(MODUL_NAME, ID, IERR)
        IF( IERR .NE. 0) GO TO 350      ! if module does not exist, skip
        CALL BKCMDL(LCMDL,0)            ! book CMDL -- Survey Module
                                        ! bank
        IC(LCSYL+ISCYLN) = 3            ! cylinder identification for CH
C
        CALL EZGET2(ID, 1, 0, 1, SARRAY, IARRAY, NUMV, NTOT, IERR)
        IF (IERR .NE. 0) GO TO 350
        C(LCMDL+ISMODL) = SARRAY(1)     ! identifies module type
        IC(LCMDL+ISNUMB) = I            ! Kroon module location number
        IC(LCMDL+ISNMSR) = NUMV/6       ! number of measurments for module
        DO 300 J = 1, NUMV, 6
          WRITE( CODE,'(3A4)') (SARRAY(K),K=J,J+2)
          IF(CODE(1:4) .NE. 'CCCH') GO TO 300
          IF( CODE(5:5) .EQ. 'N') THEN  ! establish pointer location to
            IP = ISNILX                 ! north or south end
          ELSE
            IP = ISSILX
          END IF
          IF( CODE(9:9) .EQ. 'o') THEN  ! establish pointer location to
            IP = IP + 2*NMWDS           ! inner or outer part
          END IF
          IF( CODE(10:10) .EQ. '+') THEN    ! establish pointer location
                                        ! to increasing (decreasing)
                                        ! azimuth
            IP = IP + NMWDS
          END IF
          CALL UCOPY( SARRAY(J+3), C(LCMDL+IP), 3)
          JMEAS = (IP-ISNILX)/NMWDS
          CALL SBIT1(C(LCMDL+ISMCOD),JMEAS+1)    ! code word describing
                                        ! which measurments were done
          IF( CODE(11:11) .EQ. 'A') THEN
            C(LCMDL+IP+3) = 0.05        ! BETS orientation error
          ELSE IF (CODE(11:11) .EQ. 'B') THEN
            C(LCMDL+IP+3) = 0.025
          ELSE
            C(LCMDL+IP+3) = 0.0
          END IF
  300   CONTINUE
  350 CONTINUE
C
C----------------------------------------------------------------------
  999 RETURN
      END
