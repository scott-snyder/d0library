      SUBROUTINE CELXYZ(IETA, IPHI, ILAYER, X, Y, Z, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO SUPPLY THE MEAN CARTESIAN COORDINATES OF
C-           A CELL.  THE CELL IS ADDRESSED BY THE PHYSICS VARIABLES.
C-           FOR A CELL THAT IS COMPOSED OF A NUMBER OF SUBCELLS THE 
C-           VOLUME WEIGHTED MEAN IS SUPPLIED.  SURVEY CORRECTIONS
C-           ARE ADDED IF FLAG IS SET.
C-
C-   Inputs  :    IETA      PHYSICS ETA NUMBER
C-                IPHI      PHYSICS PHI NUMBER
C-                ILAYER    PHYSICS LAYER NUMBER
C-   Outputs :    X         CENTER-OF-VOLUME X COORDINATE
C-                Y         CENTER-OF-VOLUME Y COORDINATE
C-                Z         CENTER-OF-VOLUME Z COORDINATE
C-                OK        ERROR FLAG -- 0: OK 
C-                                        1: NO VOLUME FOR GIVEN INDICES
C-                                        2: INVALID RETURN FROM CALPHI
C-   Controls: 
C-
C-   Created   5-JAN-1989   Stephen Kahn, Esq.
C-            15-NOV-1990   Stephen Kahn -- take rotation angles from table
C-            16-OCT-1991   Stephen Kahn -- include survey corrections
C-             2-MAR-1992   Stephen Kahn -- ganged eta=22/CH cells
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$LINKS:IZCEDP.LINK'
      INCLUDE 'D0$PARAMS:CEDP.PARAMS'
      INCLUDE 'D0$PARAMS:CETA.PARAMS'
      INCLUDE 'D0$PARAMS:CLIN.PARAMS'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$LINKS:IZCETA.LINK'
      INCLUDE 'D0$LINKS:IZCLYR.LINK'
      INCLUDE 'D0$LINKS:IZCLIN.LINK'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:SINTBL.INC'
C
      INTEGER IETA, ILAYER, IPHI, MPHI, JPHI, NSBLAY, JBYT, JSBLAY
      INTEGER JETA, KPHI, JLAYER, IERR, OK, I, J, JGANG, JQCLYR
      REAL X, Y, Z, CELVOL, TVOL, VOL, SGN, PSI, X1, Y1, Z1, XNOM(3)
      REAL XNEW(3)
      EQUIVALENCE (XNOM(1),X1), (XNOM(2),Y1), (XNOM(3),Z1)
      CHARACTER MSG*80
      LOGICAL FIRST, USE_SURVEY, DO_GANG, LMSG
      SAVE FIRST, USE_SURVEY, DO_GANG,LMSG
      DATA FIRST /.TRUE./,LMSG/.TRUE./
C
      OK = 1
      IF( FIRST) THEN                  ! initialize PSI
        CALL INIT_SINTBL( IERR)
        IF( IERR .NE. 0) THEN          ! CALPHI in INIT_SINTBL has
                                       ! invalid return
          OK = 2                       ! should not happen
          RETURN
        END IF
C
C&IF VAXELN
C&      CALL EZPICK_NOMSG('CALEVT_RCP',IERR)      ! read CALEVT.RCP if present
C&ELSE                
        CALL EZPICK('CALEVT_RCP')                 ! no errmsg in Level2
        CALL EZERR(IERR)
C&ENDIF
        IF(IERR .EQ. 0 )THEN           ! CALEVT.RCP is there
          CALL EZGET('SURVEY_CONSTANTS',USE_SURVEY,IERR)
          IF( IERR.NE.0) THEN
            USE_SURVEY = .TRUE.       ! if SURVEY_CONSTANTS not set SURVEY
          END IF
          CALL EZGET('DO_GANG',DO_GANG,IERR)
          IF( IERR.NE.0) THEN
            DO_GANG = .FALSE.          ! if flag not set do not gang ETA=22/CH
          END IF
          CALL EZRSET                  ! reset RCP to previous
        ELSE                           ! RCP file not there -- dont use
                                       ! SURVEY or gang ETA=22/CH
          USE_SURVEY = .TRUE.
          DO_GANG = .FALSE.
        END IF
        FIRST = .FALSE.
      END IF
C
      JETA = ABS(IETA)                 ! magnitude of eta
      SGN = SIGN(1, IETA)              ! sign of eta
      IF( SGN.LT. 0. .AND. ILAYER .GE. LYEM3A .AND. ILAYER .LE. LYEM3D
     &  .AND. JETA .LE. 26)THEN
        JLAYER = MOD(ILAYER-1,4) + 3   ! symmetry for negative FLOOR 3
      ELSE
        JLAYER = ILAYER
      END IF
C
      LQCEDP = LC(LCGEH - IZCEDP)      ! pointer to tower dispatching
C                                      ! bank
      LQCETA = LC(LQCEDP - IZCETA - JETA + 1)    ! pointer to constant
C                                        ! eta bank
      LQCLYR = LC(LQCETA - IZCLYR - JLAYER + 1)  ! pointer to first
C                                        ! appropriate layer bank
      MPHI = IC(LQCETA + IGMPHI)         ! number of phi's present for
C                                        ! this eta
      JPHI = MOD( IPHI-1, MPHI) + 1      ! index to representative phi
      NSBLAY = JBYT( C(LQCLYR), JBNSBC, NBNSBC)  ! number of sublayers
      JQCLYR = 0
C
      IF( NSBLAY .LE. 1) THEN
   30   IF( LQCLYR .EQ. 0) GO TO 999
          KPHI = JBYT( C(LQCLYR+ICELID), JBIPHI, NBIPHI)
          IF (KPHI .NE. JPHI) GO TO 50
          X = C( LQCLYR + ICX)           ! mean cell X
          Y = C( LQCLYR + ICY)           ! mean cell Y
          Z = C( LQCLYR + ICZ)*SGN       ! mean cell Z
          GO TO 170
   50   LQCLYR = LC(LQCLYR)
        GO TO 30
      ELSE
        JGANG = 0
        IF(DO_GANG .AND. JETA.EQ.22 .AND. ILAYER.EQ.MNLYCH) THEN
                                         ! cells ganged as specified in 
                                         ! D0 note 918 -- set flags for 
                                         ! specified cases 
          IF(MOD(IPHI,16) .EQ. 1) JGANG = 1
          IF(MOD(IPHI,16) .EQ. 2) JGANG = 2
          IF(MOD(IPHI,16) .EQ. 0) JGANG = 3
          IF(MOD(IPHI,16) .EQ. 15) JGANG = 4
        END IF                           ! end of patch
        X = 0.
        Y = 0.
        Z = 0.
        TVOL = 0.                      ! total volume
  100   IF(LQCLYR .EQ. 0) GO TO 150
          KPHI = JBYT( C(LQCLYR+ICELID), JBIPHI, NBIPHI)
          IF (KPHI .NE. JPHI) GO TO 130
          JSBLAY = JBYT( C(LQCLYR), JBSBCL, NBSBCL)
          IF( JSBLAY .EQ. 0) GO TO 130
          IF((JGANG.EQ.2 .OR. JGANG.EQ.4) .AND. JSBLAY.NE.1) GO TO 130
                                       ! handle "small" ganged cell
          VOL = CELVOL( LQCLYR)        ! volume of sub-cell
          IF( VOL.LE.0. ) GO TO 130
          JQCLYR = LQCLYR            ! saved to use with survey info
          IF(JGANG.EQ.3 .AND. JSBLAY.NE.1) THEN   ! "large" ganged cell
            X = X + VOL*(C(LQCLYR + ICX)*(1.+COSROT(1)) + 
     +                                 C(LQCLYR + ICY)*SINROT(1))
            Y = Y + VOL*(-C(LQCLYR + ICX)*SINROT(1) + C(LQCLYR + ICY)
     +                                 *(1.+COSROT(1)))
            Z = Z + 2.*VOL*C(LQCLYR + ICZ)
            TVOL = TVOL + 2.*VOL
          ELSE IF(JGANG.EQ.1 .AND. JSBLAY.NE.1) THEN   ! "large" ganged cell
            X = X + VOL*(C(LQCLYR + ICX)*(1.+COSROT(1)) - 
     +                                 C(LQCLYR + ICY)*SINROT(1))
            Y = Y + VOL*(C(LQCLYR + ICX)*SINROT(1) + C(LQCLYR + ICY)
     +                                 *(1.+COSROT(1)))
            Z = Z + 2.*VOL*C(LQCLYR + ICZ)
            TVOL = TVOL + 2.*VOL
          ELSE
            X = X + VOL*C(LQCLYR + ICX)  ! wt by sub-cell vol
            Y = Y + VOL*C(LQCLYR + ICY)
            Z = Z + VOL*C(LQCLYR + ICZ)
            TVOL = TVOL + VOL
          END IF
  130   LQCLYR = LC(LQCLYR)
        GO TO 100
C
  150   CONTINUE
        IF(TVOL .EQ. 0) GO TO 999
        X = X/TVOL
        Y = Y/TVOL
        Z = SGN*Z/TVOL
      END IF
C
C     NOW ROTATE COORDINATES FOR PHI .NE. 1
C
  170 CONTINUE
      OK = 0
      MPHI = IC(LQCETA + IGMPHI) 
      KPHI = ((IPHI - 1)/MPHI) * MPHI    ! rotation by KPHI*PSI
C                                        ! needs to be done
      IF (KPHI .NE. 0) THEN              ! no rotation necessary
        X1 = X
        X = X1 * COSROT(KPHI) - Y * SINROT(KPHI)
        Y = X1 * SINROT(KPHI) + Y * COSROT(KPHI)
      END IF
C
      IF(USE_SURVEY) THEN                ! correct for survey
        IF(LC(LCGEH-IZCLIN).EQ.0) THEN
          USE_SURVEY = .FALSE.           ! Survey/Alignment data not
                                        ! present
          CALL ERRMSG('NO SURVEY/ALIGN DATA','CELXYZ',
     &'Link to survey data in STP is zero -- nominal values returned',
     &    'W')
          RETURN
        END IF
        IF(LQCLYR .EQ. 0) LQCLYR = JQCLYR   ! get one of the CLYR banks
        LQCLAY = LC(LQCLYR-IXCLAY)       ! reference link to CLAY
        IF(LQCLAY .EQ. 0) RETURN         ! link to CLAY not set
        LQCLNK = LC(LQCLAY - IZLLNK)
        IF(LQCLNK .EQ. 0) RETURN         ! link to CLNK not set
        J = (IPHI + IC(LQCLNK + IGN1))/IC(LQCLNK+IGJPHI)
        J = MOD(J-1, IC(LQCLNK + IGNSEG)) + 1
        LQCLGA = LC(LQCLNK - J)          ! pick up LQCLGA from module link
                                         ! dispatching bank CLNK
        IF(LQCLGA .EQ. 0) THEN
          LQCLIN = 0
        ELSE IF(IETA .GT. 0) THEN
          LQCLIN = LC(LQCLGA-IXCLIN)     ! reference link to CC or ECS CLIN
        ELSE
          LQCLIN = LC(LQCLGA-IXCLIN2)    ! CC or ECN
        END IF
        IF(LQCLIN .EQ. 0) THEN
          IF( ILAYER .GE. MNLYMG .AND. ILAYER .LE. MXLYMG) RETURN  ! alignments
                                         ! for MG and ICD are not available yet
          IF(LMSG) THEN
            WRITE(MSG,20)IETA,IPHI,ILAYER
   20       FORMAT(' ETA=',I3,' PHI =',I3,' LYR=',I3)
            CALL ERRMSG('NO SURVEY CLIN bank','CELXYZ',MSG,'W')
            LMSG = .FALSE.
          END IF
          RETURN         ! CLIN not available for module
        END IF
        XNOM(1) = X                      ! fill XNOM w/ nominal position
        XNOM(2) = Y
        XNOM(3) = Z
C
        CALL CAL_SURVEY_TRANSF(XNOM, C(LQCLIN+IGDTX), C(LQCLIN+IGR11),
     +    C(LQCLIN+IGMDLX), XNEW)        ! transform X'=<X>-del+R*(X-<X>)
C
        X = XNEW(1)
        Y = XNEW(2)
        Z = XNEW(3)
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
