      SUBROUTINE EC_SURVEY_FIT(LATITUDE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SETUP AND STEERING ROUTINE FOR THE  CALCULATION
C-                         OF THE SURVEY CORRECTIONS TO THE COORDINATES
C-                         AND ORIENTATION OF THE MODULES.  CALLS
C-                         "SURVEY_FIT_MODULE" TO CALCULATE THE
C-                         TRANSFORMATION PARAMETERS.  RESULTS ARE STORED
C-                         'CLIN' BANKS IN STP.
C-
C-   Inputs  : LATITUDE   'N' = NORTH, 'S' = SOUTH
C-   Outputs :
C-   Controls:
C-
C-   Created   17-AUG-1992   Chip Stewart   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) LATITUDE
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
      INTEGER MAX_POINTS
      PARAMETER (MAX_POINTS=100)
      REAL*8    XNOM(3,MAX_POINTS), XSURV(3,MAX_POINTS)
      REAL*8    RM( 3, 3), XT(3,MAX_POINTS)
      REAL*8    TOL,KA,OM,PH,TR(3),SCALE
      REAL      DL,T,D(3)
      INTEGER   MI(100),IEST(10),NFIT,KR
      INTEGER I, J, K, N, NMEAS, LUNO, JCMDL, JCTHE, JCLIN, IERR
      INTEGER LENF,IBCLR, LZFIND, NDATAW,NS, IX,NSLOT,MAP(MAX_POINTS)
      CHARACTER FILENAME*80, LAT*1, MODULE*4,MSG*80
      LOGICAL OK,FIRST,LEM,LIH,LMH,LOH
      SAVE FIRST,DL,TOL,LUNO
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('CAL_SURVEY_MARKERS_RCP')
        CALL EZGET('EC_THERMAL_COEFF',DL,IERR)
        IF( IERR .NE. 0) THEN
          DL = 1.0
        END IF
        CALL EZRSET
        CALL EZPICK('CAWSTP_RCP')
        CALL EZGET('SURVEY_ECEM',LEM,IERR)
        CALL EZGET('SURVEY_ECIH',LIH,IERR)
        CALL EZGET('SURVEY_ECMH',LMH,IERR)
        CALL EZGET('SURVEY_ECOH',LOH,IERR)
        CALL EZGET('SURVEY_TOLERANCE',T,IERR)
        IF( IERR .NE. 0) THEN
          T = 10.0
        END IF
        TOL = T
        CALL EZGETS('EC_DUMP_BANK_FILE',1,FILENAME,LENF,IERR)
        IF( IERR .NE. 0) THEN
          LUNO = 0
        ELSE
          CALL GTUNIT(77,LUNO,IERR)
          CALL D0OPEN(LUNO,FILENAME,'OFL',OK)
        END IF
        CALL EZRSET
      END IF
C
      CALL UPCASE(LATITUDE(1:1),LAT)
      IF (LAT.EQ.'N') THEN
        NS = 1
      ELSE IF (LAT.EQ.'S') THEN
        NS = 2
      ELSE
        CALL ERRMSG('ERROR_ARG_LAT','EC_SURVEY_MODULE2',
     +        'ERROR IN ARGUMENT (N OR S)','W')
        GOTO 999
      END IF
      LCSRV = LZFIND(IDVSURV, LSURV, NS+1, ISREGN) ! search for ECN flag
      LCSYL = LC(LCSRV-IZCSYL)
      DO WHILE (LCSYL .NE. 0)           ! loop on cylinder
        LCMDL = LC(LCSYL-IZCMDL)
        DO WHILE (LCMDL .NE. 0)         ! loop on module
          CALL DHTOC (4,IC(LCMDL+ISMODL),MODULE)
          LCTHE = LC(LCMDL-IRCTHE)      ! link to CTHE
          IF(LCTHE .EQ. 0) GO TO 800
          NMEAS = 0
          NSLOT = 0
          CALL VZERO(MI,MAX_POINTS)
          CALL VZERO(MAP,MAX_POINTS)
          CALL VZERO(IC(LCMDL+ISMCOD),4)! zero bit map of used points
          NDATAW = IC(LCMDL-1)          ! number of data words in bank
C
          DO 300 I = ISNIX-1, NDATAW-4, 4  ! loop on measurements for
                                        ! single module
            NSLOT = NSLOT + 1
            IF(C(LCMDL+I+1).EQ.0. .AND. C(LCMDL+I+2).EQ.0. .AND.
     &        C(LCMDL+I+3).EQ.0.) GO TO 300 ! keep valid measurements
            NMEAS = NMEAS + 1         ! increment measurement counter
            MI(NMEAS) = 1
            MAP(NMEAS) = NSLOT
            J = (NSLOT-1)/32
            K = (NSLOT-1)-(J*32)
            IC(LCMDL+ISMCOD+J)=IBSET(IC(LCMDL+ISMCOD+J),K) 
            DO 210 J = 1, 3
              XNOM(J,NMEAS) = C(LCTHE+I+J)           ! nominal
              XSURV(J,NMEAS) = C(LCMDL+I+J)          ! survey
  210       CONTINUE
  300     CONTINUE                    ! end of single module measurement
                                      ! loop
C
C ****  PREPARE IEST(10) FOR CHABA (7-PARAMETER FIT)
C
          NFIT = NMEAS
          CALL VZERO(IEST(1),10)
          IEST(8)   = 1   !KILL NON UNIFORM SCALE
          IEST(9)   = 1
          IEST(10)  = 1
C
C ****  CHABA FIT
C
          CALL CHABA(IEST,NFIT,XSURV,NMEAS,XNOM,TOL,
     &      RM,KA,OM,PH,TR,SCALE,XT,MI,IERR)
          IF( IERR .NE. 0) THEN
            CALL ERRMSG('ERROR_CHABA','CC_SURVEY_MODULE_CHABA',
     +        'ERROR INVERTING IN SURVEY FIT','W')
          END IF
C MESSAGE
          CALL DHTOC (4,IC(LCMDL+ISMODL),MODULE)
          KR = IC(LCMDL+ISNUMB)
          WRITE(MSG,10)MODULE,KR,NMEAS,NFIT
   10     FORMAT(1X,A5,I3.2,' MEAS ',I3,' FIT',I3)
          CALL INTMSG(MSG)
C
C ...     book 'CLIN' bank and fill header
C
          LQCLGA = LC(LCMDL-IRCLGA)
          CALL BKCLIN(LQCLIN)           ! lift CLIN for survey results
          IC(LQCLIN) = IOR(IC(LQCLIN),1)   ! set bits to indicate End Cal
          IF(NS.EQ.2) IC(LQCLIN) = IOR(IC(LQCLIN),2)   ! bits FOR SOUTH
          CALL UCOPY(C(LCSRV+1), C(LQCLIN+1), 10)
          IC(LQCLIN+IGMDID) = -IC(LQCLGA+IGIDEN)   ! minus sign implies NORTH
                                                   ! modules
          IC(LQCLIN+IGATYP) = 1                    ! indicates survey only
          IC(LQCLIN+IGFTYP) = 2                    ! indicates THIS CHABA FIT
C
C ****  REFERENCE link to CLIN bank
C
          IF(NS.EQ.1) IX = IXCLIN2                 ! IXCLIN2 points to north
          IF(NS.EQ.2) IX = IXCLIN                  ! IXCLIN  points to south
          LC(LQCLGA-IX) = LQCLIN               
          LQCLGI = LC(LQCLGA-IXCLGI)               ! alignments
          LC(LQCLGI-IX) = LQCLIN
          IF( IC(LQCLGA+IGIDEN) .EQ. ICEC1A) THEN  ! EM module special case
            LQCREG = LC(LQCLGA + 1)                ! link to up-bank
            LQCLGA = LC(LQCREG-IZCLGA)
            DO WHILE (LQCLGA.NE.0)                 ! loop looking for EM banks
              IF(( 1000*(IC(LQCLGA+IGIDEN)/1000) .EQ. ISEMCL ) .AND.
     &          (MOD(IC(LQCLGA+IGIDEN),IELINC) .EQ. 0)) THEN  ! found EM module
                LC(LQCLGA-IX) = LQCLIN         ! all EM modules share
                LQCLGI = LC(LQCLGA-IXCLGI)         ! same alignment numbers
                LC(LQCLGI-IX) = LQCLIN
              END IF
              LQCLGA = LC(LQCLGA)
            END DO
          END IF
C
C **** LOOP OVER survey points to modify cmdl code word
C
          CALL VZERO(D,3)
          DO N = 1, NMEAS 
            I = MAP(N)
            J = (I-1)/32
            K = (I-1)-(J*32)
            IF(MI(N).EQ.0) THEN
              IC(LCMDL+ISMCOD+J)=IBCLR(IC(LCMDL+ISMCOD+J),K) 
            ELSE
              DO I = 1, 3
                D(I) = D(I) + (XSURV(I,N)-XT(I,N))**2
              END DO
            END IF
          END DO 
C
C ****  FILL CLIN with null transform if CAWSTP.RCP switch turned off
C
          IF( ((.NOT.LEM).AND.(INDEX(MODULE(2:4),'CEM').GT.0)) 
     &      .OR. ((.NOT.LIH).AND.(INDEX(MODULE(2:4),'CIH').GT.0))
     &      .OR. ((.NOT.LMH).AND.(INDEX(MODULE(2:4),'CMH').GT.0)) 
     &      .OR. ((.NOT.LOH).AND.(INDEX(MODULE(2:4),'COH').GT.0)) ) THEN
            CALL VZERO(TR(1),6) !double precision
            CALL VZERO(RM(1,1),18) !double precision
            SCALE = 1.0
            DO 20 I= 1, 3
   20       RM(I,I) = 1.0
            IC(LQCLIN+IGFTYP) = -1          ! indicates this CLIN is null
            CALL VZERO(IC(LCMDL+ISMCOD),4)
          END IF
C
C ...     fill translational deviation
C
          DO 750 I = 1, 3
            C(LQCLIN+IGDTX-1+I) = - TR(I) ! MODULE TRANSLATION
            C(LQCLIN+IGMDLX-1+I) = 0      ! ROTATION CENTER
  750     CONTINUE
C
C ...     rotational deviations
C
          DO I = 1, 3
            DO J = 1, 3
              C(LQCLIN+IGR11+3*(I-1)+(J-1) ) = RM(I,J)*SCALE
            END DO
          END DO
C
          C(LQCLIN+IGTCN) = SCALE       ! thermal contraction of SS304, normal
                                        ! to particle direction
          C(LQCLIN+IGTCT) = 0.0         ! G10 thermal contraction (currently
                                        ! not filled
          C(LQCLIN+IGPFIT) = NFIT        
          C(LQCLIN+IGCHSQ) = SQRT(D(1)+D(2)+D(3))

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
