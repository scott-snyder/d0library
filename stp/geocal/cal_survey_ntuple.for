      SUBROUTINE CAL_SURVEY_NTUPLE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ANALYSIS ROUTINE FOR  THE SURVEY CORRECTIONS
C-                         TO THE COORDINATES AND ORIENTATION OF THE MODULES.
C-                         CREATES AN NTUPLE FOR PAW PROCESSING.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created 13-AUG-1992   Chip Stewart
C-   Revised 30-AUG-1992   Steve Kahn  -- added 2nd NTUPLE based on edge
C-                            rather than corner (as 1st NTUPLE)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CALOR_SURVEY.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CLIN.PARAMS'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$LINKS:IZCSYL.LINK'
      INCLUDE 'D0$LINKS:IZCMDL.LINK'
      INCLUDE 'D0$LINKS:IZCTHE.LINK'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCLGA.LINK'
C
      INTEGER MAX_POINTS
      PARAMETER (MAX_POINTS=100)
      REAL XCNTR(3,MAX_POINTS),XSURV(3,MAX_POINTS),XNOM(3,MAX_POINTS)
      REAL XOLD(3),XNEW(3), VDOT
      INTEGER I, J, K, NMEAS, JCMDL, JCTHE, JCLIN, IER, JMEAS,LTOP,IX
      INTEGER IBB, JB,LUNIT,LENF,LOC,NTUPLE_SIZE,KR,ICYCLE,NS,NDATAW
      INTEGER NEDGE, NTUP2_SIZE
      PARAMETER (NEDGE = 12)
      INTEGER JPAIRS(3,NEDGE)
      PARAMETER (NTUPLE_SIZE=20)
      PARAMETER( NTUP2_SIZE =8 )
      CHARACTER NTUPLE_FILE*80,FILE_EXT*15,FILE*132,LAT*1
      CHARACTER LABELS(NTUPLE_SIZE)*8, MODULE*4, LATITUDE*1
      CHARACTER LAB2(NTUP2_SIZE )*8
      REAL    XTUPLE(NTUPLE_SIZE), YTUPLE(NTUP2_SIZE)
      LOGICAL FIRST,LUSED(MAX_POINTS),LFIT
      SAVE FIRST
C
      DATA FIRST / .TRUE. /
      DATA LABELS/'MOD','KR','POS','XS','YS','ZS','XN','YN','ZN',
     &  'KP','OM','PH','TX','TY','TZ','SC','DX','DY','DZ','F'/
      DATA LAB2 /'MOD','KR','EDGE','TYPE','LN','LS','DL','DLC'/
      DATA JPAIRS / ISNILX, ISNIUX, 1, ISSILX, ISSIUX, 1,
     &   ISNOLX, ISNOUX, 2, ISSOLX, ISSOUX, 2, ISNILX, ISSILX, 3,
     &   ISNIUX, ISSIUX, 3, ISNOLX, ISSOLX, 4, ISNOUX, ISSOUX, 4,
     &   ISNILX, ISNOLX, 5, ISNIUX, ISNOUX, 5, ISSILX, ISSOLX, 5,
     &   ISSIUX, ISSOUX, 5 /
C-----------------------------------------------------------------------
C
      CALL EZLOC('CAWSTP_RCP',LOC)
      IF (LOC.EQ.0) CALL INRCP('CAWSTP_RCP',IER)
      CALL EZPICK('CAWSTP_RCP')
      CALL EZGETS('CAL_SURVEY_NTUPLE_FILE',1,NTUPLE_FILE,LENF,IER)
      CALL EZRSET
      IF ( IER.NE.0 .OR. LENF.LE.0 ) GOTO 999
      CALL GTUNIT(77,LUNIT,IER)
      CALL HCDIR('//PAWC',' ')
      CALL HMDIR('NTUPLE','S')
      CALL HROPEN(LUNIT,'NTUPLE',NTUPLE_FILE,'N',1024,IER)
      CALL HBOOKN(1,'CAL SURVEY NTUPLE',NTUPLE_SIZE,
     &                  'NTUPLE',1024,LABELS)
      CALL HBOOKN(2,'CC EDGES NTUPLE',NTUP2_SIZE,'NTUPLE',1024,LAB2)
      CALL HCDIR('//PAWC/NTUPLE',' ')
      CALL HCDIR('//NTUPLE',' ')
C
      LTOP = LSURV
      DO WHILE (LTOP .NE. 0)           ! loop on csrv header
        LCSYL = LC(LTOP-IZCSYL)
        DO WHILE (LCSYL .NE. 0)           ! loop on cylinder
          LCMDL = LC(LCSYL-IZCMDL)
          DO WHILE (LCMDL .NE. 0)         ! loop on module
            KR   = IC(LCMDL+ISNUMB)
            LCTHE = LC(LCMDL-IRCTHE)      ! link to CTHE
            IF(LCTHE .EQ. 0) GO TO 800
C
            CALL DHTOC (4,IC(LCMDL+ISMODL),MODULE)
            LATITUDE = MODULE(1:1)
            CALL UPCASE(LATITUDE(1:1),LAT)
            IF (LAT.EQ.'N') THEN
              NS = 1
            ELSE IF (LAT.EQ.'S') THEN
              NS = 2
            ELSE IF (LAT.EQ.'C') THEN
              NS = 0
            ELSE
              CALL ERRMSG('ERROR_ARG_LAT','CAL_SURVEY_MODULE2',
     +          'ERROR IN ARGUMENT (N OR S)','W')
              GOTO 800
            END IF
            IF(MODULE(2:4).EQ.'CEM') THEN
              IBB = 20+NS
            ELSE IF(MODULE(2:4).EQ.'CIH') THEN
              IBB = 40+NS
            ELSE IF(MODULE(2:4).EQ.'CFH') THEN
              IBB = 40+NS
            ELSE IF(MODULE(2:4).EQ.'CMH') THEN
              IBB = 60+NS
            ELSE IF(MODULE(2:4).EQ.'CCH') THEN
              IBB = 60+NS
            ELSE IF(MODULE(2:4).EQ.'COH') THEN
              IBB = 80+NS
            ELSE
              GOTO 800
            END IF
            NMEAS = 0
            JMEAS = 0
            NDATAW = IC(LCMDL-1)          ! number of data words in bank
            DO 300 I = ISNIX-1, NDATAW-4, 4  ! loop on measurements for
                                             ! single module
              JMEAS = JMEAS + 1
              IF(C(LCMDL+I+1).EQ.0. .AND. C(LCMDL+I+2).EQ.0. .AND.
     &          C(LCMDL+I+3).EQ.0.) GO TO 300 ! keep valid measurements
              NMEAS = NMEAS + 1         ! increment measurement counter
              J = (JMEAS-1)/32
              K = (JMEAS-1)-(J*32)
              LUSED(NMEAS) = BTEST(IC(LCMDL+ISMCOD+J),K)
              DO 200 J=1, 3
                XNOM(J,NMEAS)  =  C(LCTHE+I+J)
                XSURV(J,NMEAS) =  C(LCMDL+I+J)
  200         CONTINUE
  300       CONTINUE                    ! end of single module measurement
C
C ...     get  'CLIN' bank and fill header
C
            LQCLGA = LC(LCMDL-IRCLGA)
            IF(NS.EQ.0) IX = IXCLIN           ! IXCLIN  points to CC
            IF(NS.EQ.1) IX = IXCLIN2          ! IXCLIN2 points to EC north
            IF(NS.EQ.2) IX = IXCLIN           ! IXCLIN  points to EC south
            LQCLIN = LC(LQCLGA-IX)
C
C ...     fill translational deviation
C
            LFIT = .TRUE.
            IF(LQCLIN .EQ. 0) THEN
              CALL ERRMSG('NO SURVEY INFO','CAL_SURVEY_ANALYZE',
     &          'CLIN bank not found', 'W')
              LFIT = .FALSE.
            END IF
C
            DO I = 1, NMEAS
              XOLD(1) = XNOM(1,I)
              XOLD(2) = XNOM(2,I)
              XOLD(3) = XNOM(3,I)
C
C ****  Transform X'=<X>-del+R*(X-<X>)
C
              CALL CAL_SURVEY_TRANSF(XOLD, C(LQCLIN+IGDTX),
     &          C(LQCLIN+IGR11), C(LQCLIN+IGMDLX), XNEW)
C
C ****  FILL NTUPLE
C
              XTUPLE(1) = IBB    !CCEM,FH,CH
              XTUPLE(2) = KR     !KROON NUMBER
              XTUPLE(3) = I      !MEASUREMENT NUMBER
              XTUPLE(4) = XSURV(1,I)  !SURVEY X
              XTUPLE(5) = XSURV(2,I)  !Y
              XTUPLE(6) = XSURV(3,I)  !Z
              XTUPLE(7) = XNOM(1,I)   !NOMINAL X
              XTUPLE(8) = XNOM(2,I)   !Y
              XTUPLE(9) = XNOM(3,I)   !Z
              IF( LFIT ) THEN
                XTUPLE(10)= C(LQCLIN+IGR11+1)  !ROTAION ELEMENT 1,2 (XY)
                XTUPLE(11)= C(LQCLIN+IGR11+2)  !ELEMENT 1,3 (XZ)
                XTUPLE(12)= C(LQCLIN+IGR11+5)  !ELEMENT 2,3 (YZ)
                XTUPLE(13)= C(LQCLIN+IGDTX)    !TRANS X
                XTUPLE(14)= C(LQCLIN+IGDTX+1)  !Y
                XTUPLE(15)= C(LQCLIN+IGDTX+2)  !Z
                XTUPLE(16)= C(LQCLIN+27)       !SHRINKAGE
                XTUPLE(17)= XNEW(1)-XSURV(1,I) !X NOM  TRANSF TO SURVEY
                XTUPLE(18)= XNEW(2)-XSURV(2,I) !Y
                XTUPLE(19)= XNEW(3)-XSURV(3,I) !Z
                XTUPLE(20) = 0
                IF(LUSED(I)) XTUPLE(20) = 1.
              ELSE
                CALL VZERO(XTUPLE(10),11)
              END IF
              CALL HFN(1,XTUPLE)
            END DO
            IF( LAT .EQ. 'C') THEN
            CALL VZERO(YTUPLE, NTUP2_SIZE)
            DO 700 I = 1, NEDGE
              YTUPLE(1) = IBB    ! module type iden              
              YTUPLE(2) = KR     ! Kroon number
              YTUPLE(3) = I      ! edge number
              YTUPLE(4) = JPAIRS(3,I)  ! edge type
              DO 600 J = 1, 3
                XNOM( J, 1) = C(LCTHE + JPAIRS(1,I) + J - 1)
                XNOM( J, 2) = C(LCTHE + JPAIRS(2,I) + J - 1)
                XSURV(J, 1) = C(LCMDL + JPAIRS(1,I) + J - 1)
                XSURV(J, 2) = C(LCMDL + JPAIRS(2,I) + J - 1)
  600         CONTINUE
              IF(VDOT(XSURV(1,1),XSURV(1,2),3) .EQ. 0.0) GO TO 700
              YTUPLE(5) = SQRT((XNOM(1,1)-XNOM(1,2))**2 +
     +          (XNOM(2,1)-XNOM(2,2))**2 + (XNOM(3,1)-XNOM(3,2))**2)
              YTUPLE(6) = SQRT((XSURV(1,1)-XSURV(1,2))**2 +
     +          (XSURV(2,1)-XSURV(2,2))**2+ (XSURV(3,1)-XSURV(3,2))**2)
              YTUPLE(7) = YTUPLE(6) - YTUPLE(5)
              YTUPLE(8) = YTUPLE(6) - 0.99727*YTUPLE(5)
              CALL HFN(2,YTUPLE)
  700       CONTINUE
            END IF
C
  800       CONTINUE
            LCMDL = LC(LCMDL)
          END DO
          LCSYL = LC(LCSYL)
        END DO
        LTOP = LC(LTOP)
      END DO
C
C     SAVE NTUPLE
C
      CALL HCDIR('//PAWC/NTUPLE',' ')
      CALL HCDIR('//NTUPLE',' ')
      CALL HROUT(0,ICYCLE,' ')
      CALL HREND('NTUPLE')
      CLOSE(LUNIT)
      CALL RLUNIT(77,LUNIT,IER)
      CALL INTMSG(' NTUPLE STORED IN '//NTUPLE_FILE)
C----------------------------------------------------------------------
  999 RETURN
      END
