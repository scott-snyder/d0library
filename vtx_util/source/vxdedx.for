      SUBROUTINE VXDEDX(AREAINF,SINTHE,NHIT,MIP,MIPERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find dE/dX of each track
C-
C-   Inputs  : AREAINF(1,*) = AREA OF THE PULSE
C-             AREAINF(2,*) = DRIFT DISTANCE
C-             AREAINF(3,*) = 1 IF ONLY END 0 HAS HIT
C-                           2 IF ONLY END 1 HAS HIT
C-                            3 IF BOTH END 1 & 0 HAVE HITS
C-             AREAINF(4,*) = 1 IF Z-POSITION FLAG IS BAD
C-                            0 IF Z-POSITION FLAG IS GOOD
C-             AREAINF(5,*) = 1 IF EITHER HITS ARE SATURATED
C-                            0 IF NONE OF HITS ARE SATURATED
C-             SINTHE       = SIN OF THETA ANGLE OF THE TRACK
C-             NHIT NUMBER OF HITS IN THIS TRACK
C-   Outputs:  MIP          = Ionization in MIPS
C-                            -1. if it can't be found
C-             MIPERR       = Error in MIP
C-                            -1. if it can't be found
C-
C-
C-   Created     MAR-1992   M. Pang
C-   Updated   8-JUN-1992   Peter M. Grudberg  Change argument from theta to
C-                          sin(theta); only do theta correction if requested
C-                          (rcp)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MXHIT
      PARAMETER (MXHIT=24)
      REAL AREAINF(5,MXHIT), MIP, MIPERR
      REAL SINTHE,TRUNC
      REAL TOT_AREA,SUM
      REAL IONCONVERSION
      INTEGER NHIT,NSAMPLE,NACPT
      INTEGER I,IER
      LOGICAL FIRST, SATURATED, SIN_CORRECTION
      DATA FIRST /.TRUE./
C------------------------------------------------------------------------
      IF ( FIRST ) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('TRUNCATION',TRUNC,IER)
        CALL EZGET('IONCONVERSION',IONCONVERSION,IER)
        CALL EZGET_l('SATURATED',SATURATED,IER)
        CALL EZGET_l('SIN_CORRECTION',SIN_CORRECTION,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
C  EXCLUDE BAD HITS
C
      NSAMPLE = NHIT
      DO I = 1,NHIT
        IF ( AREAINF(4,I) .EQ. 1. .OR.                  ! Bad Z hit
     &      AREAINF(3,I) .NE. 3. .OR.                   ! Hit on only one end
     &      ( (.NOT.SATURATED) .AND. (AREAINF(5,I).EQ.1.) ) ) THEN
          NSAMPLE = NSAMPLE - 1
          AREAINF(1,I) = 999999.
        ELSE
          AREAINF(1,I) = AREAINF(1,I) / IONCONVERSION
        END IF
      END DO
C
      CALL SORTR(AREAINF,5,NHIT,1)
                                ! SORT ALL THE HITS IN INCREASING ORDER
C
      NACPT = NINT( FLOAT(NSAMPLE) * ( TRUNC/100. ) )
                                        ! TRUNCATION
      TOT_AREA = 0.
      DO I = 1, NACPT
        TOT_AREA = TOT_AREA + AREAINF(1,I)
      END DO
C
      IF ( SIN_CORRECTION ) TOT_AREA = TOT_AREA * SINTHE
C
      IF ( NACPT .NE. 0. ) THEN
        MIP = TOT_AREA / FLOAT(NACPT)              ! dE/dX FOR THIS TRACK
      ELSE
        MIP = -1.
      END IF
C
C   FIND ERROR ON MIP ( dE /dX )
C
      SUM = 0.
      DO I = 1, NACPT
        IF ( SIN_CORRECTION ) THEN
          SUM = SUM + ( AREAINF(1,I)*SINTHE - MIP )**2
        ELSE
          SUM = SUM + ( AREAINF(1,I) - MIP )**2
        ENDIF
      END DO
C
      IF ( NACPT .NE. 0 ) THEN
        MIPERR = SQRT( SUM / FLOAT(NACPT) )
        MIPERR = MIPERR/SQRT(FLOAT(NACPT))         ! ERROR ON MEAN
      ELSE
        MIPERR = -1.
      END IF
C
  999 RETURN
      END
