      SUBROUTINE CC_SURVEY_FIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CALCULATION OF THE SURVEY CORRECTION
C-                         TRANSFORMATION PARAMETERS.  RESULTS ARE STORED
C-                         'CLIN' BANKS IN STP. Uses subroutine CHABA
C-                         for fit.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   12-AUG-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CALOR_SURVEY.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$PARAMS:CLIN.PARAMS'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$LINKS:IZCSYL.LINK'
      INCLUDE 'D0$LINKS:IZCMDL.LINK'
      INCLUDE 'D0$LINKS:IZCTHE.LINK'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCLGA.LINK'
      INTEGER MAX_POINTS
      PARAMETER (MAX_POINTS=100)
      REAL*8    XNOM(3,MAX_POINTS), XSURV(3,MAX_POINTS)
      REAL*8    RM( 3, 3), XT(3,MAX_POINTS)
      REAL*8    TOL,KA,OM,PH,TR(3),SCALE
      REAL      DL,T,D(3)
      INTEGER   MI(50),IEST(10),NFIT,MAP(MAX_POINTS),NSLOT
      INTEGER I, J, K, N, NMEAS, LUNO, JCMDL, JCTHE, JCLIN, IERR
      INTEGER LZFIND, LENF,IBCLR,KR,NDATAW
      CHARACTER FILENAME*80,MODULE*4,MSG*80
      LOGICAL OK,LEM,LFH,LCH
C----------------------------------------------------------------------
      CALL EZPICK('CAL_SURVEY_MARKERS_RCP')
      CALL EZGET('CC_THERMAL_COEFF',DL,IERR)
      IF( IERR .NE. 0) THEN
        DL = 1.0
      END IF
      CALL EZRSET
      CALL EZPICK('CAWSTP_RCP')
      CALL EZGET('SURVEY_CCEM',LEM,IERR)
      CALL EZGET('SURVEY_CCFH',LFH,IERR)
      CALL EZGET('SURVEY_CCCH',LCH,IERR)
      CALL EZGET('SURVEY_TOLERANCE',T,IERR)
      IF( IERR .NE. 0) THEN
        T = 10.0
      END IF
      TOL = T
      CALL EZGETS('CC_DUMP_BANK_FILE',1,FILENAME,LENF,IERR)
      IF( IERR .NE. 0) THEN
        LUNO = 0
      ELSE
        CALL GTUNIT(77,LUNO,IERR)
        CALL D0OPEN(LUNO,FILENAME,'OFL',OK)
      END IF
      CALL EZRSET
C
      LCSRV = LZFIND(IDVSURV, LSURV, 1, ISREGN) ! search for CC flag
      LCSYL = LC(LCSRV-IZCSYL)
      DO WHILE (LCSYL .NE. 0)           ! loop on cylinder
        LCMDL = LC(LCSYL-IZCMDL)
        DO WHILE (LCMDL .NE. 0)         ! loop on module
          CALL DHTOC (4,IC(LCMDL+ISMODL),MODULE)
          LCTHE = LC(LCMDL-IRCTHE)      ! link to CTHE
          IF(LCTHE .EQ. 0) GO TO 800
C
          NMEAS = 0
          NSLOT = 0
          CALL VZERO(MI,MAX_POINTS)
          CALL VZERO(MAP,MAX_POINTS)
C
          CALL VZERO(IC(LCMDL+ISMCOD),4)! zero bit map of used points
          NDATAW = IC(LCMDL-1)          ! number of data words in bank
          DO 300 I = ISNILX-1, NDATAW-4, 4  ! loop on measurements for
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
            DO 200 J = 1, 3
              XNOM(J,NMEAS) = C(LCTHE+I+J)           ! nominal
              XSURV(J,NMEAS) = C(LCMDL+I+J)          ! survey
  200       CONTINUE
  300     CONTINUE
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
            CALL ERRMSG('ERROR_CHABA','CC_SURVEY_MODULE2',
     +        'ERROR INVERTING IN SURVEY FIT','W')
          END IF
          KR = IC(LCMDL+ISNUMB)
          WRITE(MSG,10)MODULE,KR,NMEAS,NFIT
   10     FORMAT(1X,A5,I3.2,' MEAS ',I3,' FIT',I3)
          CALL INTMSG(MSG)
C
C ...     book 'CLIN' bank and fill header
C
          LQCLGA = LC(LCMDL-IRCLGA)
          CALL BKCLIN(LQCLIN)           ! lift CLIN for survey results
          CALL UCOPY(C(LCSRV+1), C(LQCLIN+1), 10)
          IC(LQCLIN+IGMDID) = IC(LQCLGA+IGIDEN)
          IC(LQCLIN+IGATYP) = 1         ! indicates survey only
          IC(LQCLIN+IGFTYP) = 2         ! indicates this CHABA analysis
          LC(LQCLGA-IXCLIN) = LQCLIN
          LC(LQCLGA-IXCLIN2) = LQCLIN
C
C **** LOOP OVER survey points to update clin code word
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
          IF( ((.NOT.LEM).AND.(INDEX(MODULE,'CCEM').GT.0))   !null CLIN
     &      .OR. ((.NOT.LFH).AND.(INDEX(MODULE,'CCFH').GT.0)) 
     &      .OR. ((.NOT.LCH).AND.(INDEX(MODULE,'CCCH').GT.0)) ) THEN
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
