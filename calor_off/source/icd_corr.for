      SUBROUTINE ICD_CORR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correct RECO v12_20 u_DST's for incorrect
C-                         ICD constants (STP file mixup)
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls:
C-
C-   Created   07-NOV-1995   Mark Sosebee
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER LOC, IER, RECO_VERS, RECO_PASS
      INTEGER I,IETA,IPHI,ILYR,NRUN,RUNNO
      LOGICAL MC, TB, FIRST
      REAL    CICD_1(9:14,64,2), AICD_1
      REAL    CICD_2(9:14,64,2), AICD_2
      REAL    ENERGY
      CHARACTER MSG*80,MODULE*4, PROGRAM_NAME*40
      INTEGER LAST_RUN_1A, RUN_1A_CHK
      INTEGER FIRST_HALF_RUN_1B, FIRST_HALF_RUN_1B_CHK
      INTEGER SECOND_HALF_RUN_1B, SECOND_HALF_RUN_1B_CHK
      INTEGER VERS_CORR, PASS_CORR
      INTEGER LHSTR, GZHSTR
      INTEGER IAD
      BYTE    BIAD(4)
      EQUIVALENCE(IAD,BIAD)
      INTEGER LDROP, LCAEP, LCAEQ, NNN, III
      INTEGER GZCAEP, GZCAEQ
      SAVE    LAST_RUN_1A, FIRST_HALF_RUN_1B, SECOND_HALF_RUN_1B
      SAVE    CICD_1, CICD_2, AICD_1, AICD_2, FIRST
      DATA    FIRST/.TRUE./
      DATA    VERS_CORR/12/
      DATA    PASS_CORR/20/
      DATA    LAST_RUN_1A/66000/
      DATA    FIRST_HALF_RUN_1B/89200/
      DATA    SECOND_HALF_RUN_1B/93887/
C
C----------------------------------------------------------------------
C
C ****  Check RECO version:
C
      LHSTR=GZHSTR()
      DO WHILE (LHSTR.GT.0)
        CALL UHTOC(IQ(LHSTR+7),40,PROGRAM_NAME,40)
        IF(PROGRAM_NAME(1:11).EQ.'FULL_D0RECO') THEN
          RECO_VERS=IQ(LHSTR+3)
          RECO_PASS=IQ(LHSTR+4)
          GOTO 20
        ENDIF
        LHSTR=LQ(LHSTR)
      ENDDO
20    CONTINUE

      MC = BTEST(D0VSN,5)        ! Check for Monte Carlo generated CAD bank
      TB = BTEST(D0VSN,6)        ! Check for NWA data CAD1 bank
      NRUN = RUNNO()
      IF( (RECO_VERS .NE. VERS_CORR) .OR.     
     &    (RECO_PASS .NE. PASS_CORR) ) GOTO 999  !  Only for RECO 12_20 
      IF(MC) GOTO 999            ! Ditto for MC...
      IF(TB) GOTO 999            ! ...and TB
C
C **** If the run number is between the beginning of run 1B and the 
C **** February 1995 shutdown the constants should be o.k. and we
C **** bag out.  Otherwise make the correction:
C
      IF( NRUN .LE. FIRST_HALF_RUN_1B ) GOTO 999
C
C-----------------------------------------------------------------------
C
C ****  Two RCP files are needed: CSF_ICD_1ST_HALF_RUN_1B and
C ****  CSF_ICD_2ND_HALF_RUN_1B.RCP :
C
C-----------------------------------------------------------------------
C
C ****  READ CSF_ICD_1ST_HALF_RUN_1B:
C
C-----------------------------------------------------------------------
C
      IF( FIRST ) THEN 
        CALL EZLOC('CSF_ICD_1ST_HALF_RUN_1B_RCP',LOC)
        IF (LOC.EQ.0) THEN
          CALL INRCP('D0$CALOR_OFF:CSF_ICD_1ST_HALF_RUN_1B.RCP',IER)
        ENDIF

        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT (IER,MSG)
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',MSG,'W')
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',
     & 'FAIL to read CSF_ICD_1ST_HALF_RUN_1B_RCP - no CAEQ changes','W')
          FIRST_HALF_RUN_1B = 1000000
          GOTO 999
        ENDIF

        CALL EZPICK('CSF_ICD_1ST_HALF_RUN_1B_RCP')
        CALL EZERR(IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',
     &     'Fail to pick CSF_ICD_1ST_HALF_RUN_1B_RCP','W')
          CALL INTMSG('Without CSF_ICD_1ST_HALF_RUN_1B_RCP, ICD')
          CALL INTMSG('CAEQ channels will not be modified')
          FIRST_HALF_RUN_1B = 1000000
          GOTO 999
        ENDIF

        CALL EZGET_i('FIRST_HALF_RUN_1B_CHK',
     &            FIRST_HALF_RUN_1B_CHK,IER)
        IF (FIRST_HALF_RUN_1B_CHK .NE. FIRST_HALF_RUN_1B) THEN
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',
     &       'CSF_ICD_1ST_HALF_RUN_1B VERSION MIXUP','W')
          FIRST_HALF_RUN_1B = 1000000
          GOTO 999
        ENDIF

        CALL EZGET('A_ICD_1ST_HALF_RUN_1B',AICD_1,IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',
     &'Fail to fetch A_ICD_1ST_HALF_RUN_1B - no CAEQ modifications','W')
          FIRST_HALF_RUN_1B = 1000000
          GOTO 999
        ENDIF
C
C **** Everything looks o.k. - read the cell-by-cell corrections:
C
        CALL EZGET('CORRECTION_NORTH_LYR_09',CICD_1(9,1,1),IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',
     &'Fail to fetch CORRECTION_NORTH_LYR_09 - no ICD CAEQ changes','W')
          FIRST_HALF_RUN_1B = 1000000
          GOTO 999
        ENDIF
C
C ****  Now the south...
C
        CALL EZGET('CORRECTION_SOUTH_LYR_09',CICD_1(9,1,2),IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',
     &'Fail to fetch CORRECTION_SOUTH_LYR_09 - no ICD CAEQ changes','W')
          FIRST_HALF_RUN_1B = 1000000
          GOTO 999
        ENDIF
        CALL EZRSET

        WRITE (MSG,14)NRUN
14      FORMAT('CORRECT ICD CONSTANTS for 1ST HALF RUN 1B - RUN', I7)
        CALL ERRMSG('ICD','ICD_CORR_D0FIX',MSG,'I')
        CALL ERRMSG('ICD','ICD_CORR_D0FIX',MSG,'S')
C
C---------------------------------------------------------------------------
C  
C ****  READ CSF_ICD_2ND_HALF_RUN_1B:
C
C---------------------------------------------------------------------------
C
        CALL EZLOC('CSF_ICD_2ND_HALF_RUN_1B_RCP',LOC)
        IF (LOC.EQ.0) THEN
          CALL INRCP('D0$CALOR_OFF:CSF_ICD_2ND_HALF_RUN_1B.RCP',IER)
        ENDIF

        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT (IER,MSG)
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',MSG,'W')
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',
     &'FAIL to read CSF_ICD_2ND_HALF_RUN_1B_RCP - no CAEQ changes','W')
          FIRST_HALF_RUN_1B = 1000000
          GOTO 999
        ENDIF

        CALL EZPICK('CSF_ICD_2ND_HALF_RUN_1B_RCP')
        CALL EZERR(IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',
     &     'Fail to pick CSF_ICD_2ND_HALF_RUN_1B_RCP','W')
          CALL INTMSG('Without CSF_ICD_2ND_HALF_RUN_1B_RCP, ICD')
          CALL INTMSG('CAEQ channels will not be modified')
          FIRST_HALF_RUN_1B = 1000000
          GOTO 999
        ENDIF

        CALL EZGET_i('SECOND_HALF_RUN_1B_CHK',
     &            SECOND_HALF_RUN_1B_CHK,IER)
        IF (SECOND_HALF_RUN_1B_CHK .NE. SECOND_HALF_RUN_1B) THEN
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',
     &       'CSF_ICD_2ND_HALF_RUN_1B VERSION MIXUP','W')
          FIRST_HALF_RUN_1B = 1000000
          GOTO 999
        ENDIF

        CALL EZGET('A_ICD_2ND_HALF_RUN_1B',AICD_2,IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',
     &'Fail to fetch A_ICD_2ND_HALF_RUN_1B - no CAEQ modifications','W')
          FIRST_HALF_RUN_1B = 1000000
          GOTO 999
        ENDIF
C
C **** Everything looks o.k. - read the cell-by-cell corrections:
C
        CALL EZGET('CORRECTION_NORTH_LYR_09',CICD_2(9,1,1),IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',
     &'Fail to fetch CORRECTION_NORTH_LYR_09 - no ICD CAEQ changes','W')
          FIRST_HALF_RUN_1B = 1000000
          GOTO 999
        ENDIF
C
C ****  Now the south...
C
        CALL EZGET('CORRECTION_SOUTH_LYR_09',CICD_2(9,1,2),IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',
     &'Fail to fetch CORRECTION_SOUTH_LYR_09 - no ICD CAEQ changes','W')
          FIRST_HALF_RUN_1B = 1000000
          GOTO 999
        ENDIF
        CALL EZRSET

        WRITE (MSG,14)NRUN
        CALL ERRMSG('ICD','ICD_CORR_D0FIX',MSG,'I')
        CALL ERRMSG('ICD','ICD_CORR_D0FIX',MSG,'S')

        FIRST = .FALSE.
      ENDIF
C
C------------------------------------------------------------------------
C------------------------------------------------------------------------
C **** If CAEP exists, use it;  if CAEQ but no caep, create CAEP from CAEQ:
C
      LCAEQ = GZCAEQ() 
      LCAEP = GZCAEP()
      IF(  LCAEQ.LE. 0 ) THEN                         ! No CAEQ - error
        IF(LCAEP.LE. 0 ) THEN
          CALL ERRMSG('ICD','ICD_CORR_D0FIX',
     &  'No CAEQ or CAEP bank found - cannot make ICD CAEQ changes','W')
          GOTO 999
        ENDIF
      ELSE
        IF(LCAEP .LE. 0 ) CALL CAEQ_TO_CAEP
        LCAEP = GZCAEP() 
      ENDIF
C----------------------------------------------------------------------
C
C **** This is where we do the correction: 
C
      NNN   = IQ(LCAEP+3)                     !   Number of channels
      DO III = 1, NNN
        ENERGY = Q(LCAEP+5+(III-1)*2)
        IAD   = IQ(LCAEP+4+(III-1)*2)
        IETA  = BIAD(4)                          
        IPHI  = BIAD(3)
        ILYR  = BIAD(2)
C
        IF( ILYR .EQ. LYICD) THEN             !   ICD layer
          IF( ABS(IETA) .GE.9 .AND. ABS(IETA) .LE.14 ) THEN  ! ICD eta range
            IF( IETA .LT. 0 ) THEN            !   North
              Q(LCAEP+5+(III-1)*2) = 
     &         ENERGY * CICD_2(ABS(IETA),IPHI,1)
     &                / CICD_1(ABS(IETA),IPHI,1)
     &                * AICD_2 / AICD_1
            ELSE                              !   South
              Q(LCAEP+5+(III-1)*2) = 
     &         ENERGY * CICD_2(ABS(IETA),IPHI,2)
     &                / CICD_1(ABS(IETA),IPHI,2)
     &                * AICD_2 / AICD_1
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C----------------------------------------------------------------------
999   RETURN
      END
