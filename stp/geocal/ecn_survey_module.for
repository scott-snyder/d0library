      SUBROUTINE ECN_SURVEY_MODULE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SETUP AND STEERING ROUTINE FOR THE  CALCULATION
C-                         OF THE SURVEY CORRECTIONS TO THE COORDINATES
C-                         AND ORIENTATION OF THE MODULES.  CALLS
C-                         "SURVEY_FIT_MODULE" TO CALCULATE THE
C-                         TRANSFORMATION PARAMETERS.  RESULTS ARE STORED
C-                         'CLIN' BANKS IN STP.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-MAR-1991   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CALOR_SURVEY.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLIN.PARAMS'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$LINKS:IZCSYL.LINK'
      INCLUDE 'D0$LINKS:IZCMDL.LINK'
      INCLUDE 'D0$LINKS:IZCTHE.LINK'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCLGA.LINK'
C
      REAL*8    XNOM(3), XSURV(3), XNXN( 3, 3), XNXS( 3, 3)
      REAL*8    R( 3, 3), XSINV(3, 3), DETERM
      REAL*8    DELTA(3), RINV( 3, 3), S( 3, 3), XCNTR(3)
      REAL      DL
      INTEGER I, J, K, NMEAS, LUNO, JCMDL, JCTHE, JCLIN, IX(3), IERR
      INTEGER LZFIND, NDATAW
C
      DATA LUNO / 40 /
C
      CALL EZPICK('CAL_SURVEY_MARKERS_RCP')
      CALL EZGET('EC_THERMAL_COEFF',DL,IERR)
      IF( IERR .NE. 0) THEN
        DL = 1.0
      END IF
C
      LCSRV = LZFIND(IDVSURV, LSURV, 2, ISREGN) ! search for ECN flag
      LCSYL = LC(LCSRV-IZCSYL)
      DO WHILE (LCSYL .NE. 0)           ! loop on cylinder
        LCMDL = LC(LCSYL-IZCMDL)
        DO WHILE (LCMDL .NE. 0)         ! loop on module
          LCTHE = LC(LCMDL-IRCTHE)      ! link to CTHE
          IF(LCTHE .EQ. 0) GO TO 800
C
          DO 50 I = 1, 9
            XNXN(I,1) = 0.
            XNXS(I,1) = 0.
  50      CONTINUE
          DO 60 I = 1, 3
            XNOM(I) = 0.
            XSURV(I) = 0.
            XCNTR(I) = 0.
  60      CONTINUE
          NMEAS = 0
          NDATAW = IC(LCMDL-1)          ! number of data words in bank
C
          DO 300 I = ISNIX-1, NDATAW-4, 4  ! loop on measurements for
                                        ! single module
            DO 100 J = 1, 3
              XCNTR(J) = XCNTR(J) + C(LCTHE+I+J)/IC(LCTHE+ISNMSR)  !
                                        ! save center of module (average
                                        ! of all corners whether
                                        ! measured of not
  100       CONTINUE
C
            IF(C(LCMDL+I+1).EQ.0. .AND. C(LCMDL+I+2).EQ.0. .AND.
     &        C(LCMDL+I+3).EQ.0.) GO TO 300 ! keep valid measurements
            NMEAS = NMEAS + 1         ! increment measurement counter
C
C ...         fill <XS(J)XS(K)> and <XT(J)XS(K)>
C
            DO 210 J = 1, 3
              IF( C(LCMDL+I+J) .GE. 99999.0) GO TO 210    ! C() = 99999.0 is a
                                       ! flag to indicate coordinate not used
                                       ! in fit
              XNOM(J) = XNOM(J) + C(LCTHE+I+J)            ! sum nominal
              XSURV(J) = XSURV(J) + C(LCMDL+I+J)          ! sum survey
            DO 200 K = 1, 3
              IF( C(LCMDL+I+K) .GE. 99999.0) GO TO 200
              XNXN(J,K) = XNXN(J,K) + C(LCTHE+I+J)*C(LCTHE+I+K)
              XNXS(J,K) = XNXS(J,K) + C(LCTHE+I+J)*C(LCMDL+I+K)
  200       CONTINUE
  210       CONTINUE
  300     CONTINUE                    ! end of single module measurement
                                      ! loop
C
          DO 400 J = 1, 3
            XNOM(J) = XNOM(J)/NMEAS   ! mean nominal position of module
            XSURV(J) = XSURV(J)/NMEAS ! mean survey position of module
            DELTA(J) = XNOM(J)-XSURV(J)     ! module translation
  400     CONTINUE
          DO 425 J = 1, 3
          DO 425 K = 1, 3
            XNXN(J,K) = XNXN(J,K)/NMEAS - XNOM(J)*XNOM(K) ! <XN(J)XN(K)>
            XNXS(J,K) = XNXS(J,K)/NMEAS - XNOM(J)*XSURV(K) ! <XN(J)XS(K)>
  425     CONTINUE
C
C ...     INVERT XNXN ==> XSINV
C
          DO 450 I=1, 9            ! copy  3 x 3 matrix to XSINV
  450     XSINV( I, 1) = XNXN( I, 1)     ! since MATIN2 will overwrite
C         CALL MATIN2(XSINV, 3, 3, 3, 0, IX, IERR, DETERM)  ! CERNLIB
                                   ! does not contain MATIN2 anymore
          CALL DINV( 3, XSINV, 3, IX, IERR)
          IF( IERR.NE.0) THEN
            CALL ERRMSG('ECN_SURVEY_MODULE','XSINV',
     +        'ERROR INVERTING MATRIX','W')
          END IF
C
C ...     GET ORIENATION MATRIX
C
          DO 600 J = 1, 3
          DO 600 K = 1, 3
            R(J, K) = 0.
            DO 500 I = 1,3
  500       R(J, K) = R(J, K) + XSINV(K,I)*XNXS(I,J)  ! R = XSINV_T*XNXS_T
          S(J, K) = R(J, K)
          IF( J.EQ.K) S(J, K) = S(J, K) - 1.0
  600     CONTINUE
C
          DO 700 J = 1, 3
          DO 700 K = 1, 3
            IF( J.EQ.K) THEN
              RINV(J, K) = 1.           ! this is R not Rinv yet
            ELSE
              RINV(J, K) = 0.5*(S(J, K) - S(K, J))        !
                                        ! antisymmetrize R
            END IF
  700     CONTINUE
C
C         CALL MATIN1(RINV, 3, 3, 3, 0, IX, IERR, DETERM)    ! it is
                                        ! R_inv that we need to store
                                        ! for the future transformations
          CALL DINV( 3, RINV, 3, IX, IERR)
          IF( IERR .NE. 0) THEN
            CALL ERRMSG('ECN_SURVEY_MODULE','RINV',
     +        'ERROR INVERTING MATRIX','W')
          END IF
C
C ...     book 'CLIN' bank and fill header
C
          LQCLGA = LC(LCMDL-IRCLGA)
          CALL BKCLIN(LQCLIN)           ! lift CLIN for survey results
          IC(LQCLIN) = IOR(IC(LQCLIN),3)   ! set bits to indicate North
                                           ! End Cal
          CALL UCOPY(C(LCSRV+1), C(LQCLIN+1), 10)
          IC(LQCLIN+IGMDID) = -IC(LQCLGA+IGIDEN)   ! minus sign implies NORTH
                                                   ! modules
          IC(LQCLIN+IGFTYP) = 1                    ! indicates THIS LS analysis
          LC(LQCLGA-IXCLIN2) = LQCLIN              ! IXCLIN2 points to north
          LQCLGI = LC(LQCLGA-IXCLGI)               ! alignments
          LC(LQCLGI-IXCLIN2) = LQCLIN
          IF( IC(LQCLGA+IGIDEN) .EQ. ICEC1A) THEN  ! EM module special case
            LQCREG = LC(LQCLGA + 1)                ! link to up-bank
            LQCLGA = LC(LQCREG-IZCLGA)
            DO WHILE (LQCLGA.NE.0)                 ! loop looking for EM banks
              IF(( 1000*(IC(LQCLGA+IGIDEN)/1000) .EQ. ISEMCL ) .AND.
     &          (MOD(IC(LQCLGA+IGIDEN),IELINC) .EQ. 0)) THEN  ! found EM module
                LC(LQCLGA-IXCLIN2) = LQCLIN         ! all EM modules share
                LQCLGI = LC(LQCLGA-IXCLGI)         ! same alignment numbers
                LC(LQCLGI-IXCLIN2) = LQCLIN
              END IF
              LQCLGA = LC(LQCLGA)
            END DO
          END IF
C
C ...     fill translational deviation
C
          DO 750 I = 1, 3               ! <xnom> - <xsurv>
            C(LQCLIN+IGDTX-1+I) = XNOM(I) - XSURV(I)
            C(LQCLIN+IGMDLX-1+I) = XNOM(I)     ! module mean of
                                        ! measurements
  750     CONTINUE
C
C ...     rotational deviations
C
          DO 770 I = 1, 9
  770     C(LQCLIN+IGR11+I-1) = RINV(I,1)*DL
C
          C(LQCLIN+IGTCN) = DL          ! thermal coeff in normal (Z) dirn
          C(LQCLIN+IGTCT) = 0.0         ! thermal coeff in transv (XY) dirn
C
C ...     print if requested
C
          IF(LUNO .NE.0)  THEN
            JCMDL = LCMDL
            CALL PRCMDL( LUNO, JCMDL, 0, 'ONE', 0)
            JCTHE = LCTHE
            CALL PRCTHE( LUNO, JCTHE, 0, 'ONE', 0)
            JCLIN = LQCLIN
            CALL PRCLIN( LUNO, JCLIN, 0, 'ONE', 0)
          END IF
C
  800     CONTINUE
          LCMDL = LC(LCMDL)
        END DO
        LCSYL = LC(LCSYL)
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
