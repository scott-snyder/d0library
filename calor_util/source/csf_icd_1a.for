      SUBROUTINE CSF_ICD_1A
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Correct the CSFH (A(4)) and CSFC banks for the
C-                         RUN 1A or RUN 1B (up to the Feb. '95 shutdown)
C-                         state of the ICD.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls:
C-
C-   Created   08-MAR-1995   Chip Stewart, Mark Sosebee
C-   Modified  03-NOV-1995   Mark Sosebee (3 running periods: 1A, early
C-                           1B, and late 1B)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INCLUDE 'D0$INC:CUNFLG.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZCSFH,LOC,IER,ICD_MG_W,GZCSFW
      INTEGER LCSFC,GZCSFC,I,IETA,IPHI,ILYR,NRUN,RUNNO
      INTEGER CAL_MODULE,IMOD,LZFIND,ICDH
      LOGICAL MC,TB,FIRST,CEXIST
      REAL    CICD(9:14,64,2),AICD,A(8:10)
      REAL    WTS_OLD(8:14,8:10)
      CHARACTER MSG*80,MODULE*4
      INTEGER LAST_RUN_1A,RUN_1A_CHK,ICDC,IZ
      INTEGER FIRST_HALF_RUN_1B, FIRST_HALF_RUN_1B_CHK
      INTEGER SECOND_HALF_RUN_1B, SECOND_HALF_RUN_1B_CHK
      SAVE    FIRST, LAST_RUN_1A, FIRST_HALF_RUN_1B, SECOND_HALF_RUN_1B
      SAVE    CICD,AICD,WTS_OLD
      DATA FIRST/.TRUE./, LAST_RUN_1A/66000/
      DATA FIRST_HALF_RUN_1B/89200/, SECOND_HALF_RUN_1B/93887/
C----------------------------------------------------------------------
C
C ****  Check data source
C
      MC = BTEST(D0VSN,5)        ! Check for Monte Carlo generated CAD bank
      TB = BTEST(D0VSN,6)        ! Check for NWA data CAD1 bank
      NRUN = RUNNO()
      IF(NRUN.GT.SECOND_HALF_RUN_1B) 
     &       GOTO 999            ! If run number > SECOND_HALF_RUN_1B, bag out
      IF(MC) GOTO 999            ! Ditto for MC...
      IF(TB) GOTO 999            ! ...and TB
C-----------------------------------------------------------------------
C ****  3 possibilites: either it's run 1a data, it's 1B up until the
C ****  Feb. '95 shutdown, or it's  Feb. '95 until the start of run 1C -
C-----------------------------------------------------------------------
      IF(FIRST) THEN
C
C ****  If we make it this far then it must be a pre-run 1C run number,
C ****  so we will need the old ICD and MG sampling weights:
C
        IF( NRUN .LE. LAST_RUN_1A ) THEN
C
C ****  READ CSF_ICD_1A_RCP
C
          CALL EZLOC('CSF_ICD_1A_RCP',LOC)
          IF (LOC.EQ.0) THEN
            CALL INRCP('D0$CALOR_OFF:CSF_ICD_1A.RCP',IER)
          END IF
          IF(IER.NE.0) THEN
            CALL EZGET_ERROR_TEXT (IER,MSG)
            CALL ERRMSG('CAL','CSF_ICD_1A',MSG,'W')
            CALL ERRMSG('CAL','CSF_ICD_1A',
     &      'FAIL to read CSF_ICD_1A_RCP - Use Default Constants','W')
            LAST_RUN_1A = 0
            GOTO 999
          END IF
          CALL EZPICK('CSF_ICD_1A_RCP')
          CALL EZERR(IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('ICD','CSF_ICD_1A',
     &        'Fail to pick CSF_ICD_1A_RCP','W')
            CALL INTMSG('Without CSF_ICD_1A_RCP, CAHITS will use')
            CALL INTMSG(' latest ICD corrections on RUN 1A data')
            LAST_RUN_1A = 0
            GOTO 999
          END IF
          CALL EZGET('LAST_RUN_1A_CHK',RUN_1A_CHK,IER)
          IF (RUN_1A_CHK .NE. LAST_RUN_1A) THEN
            CALL ERRMSG('ICD','CSF_ICD_1A',
     &        'CSF_ICD_1A VERSION MIXUP','W')
            LAST_RUN_1A = 0
            GOTO 999
          END IF
          CALL EZGET('A_ICD_1A',AICD,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('ICD','CSF_ICD_1A',
     &        'Fail to fetch A_ICD_1A - use latest constants','W')
            LAST_RUN_1A = 0
            GOTO 999
          END IF
          CALL EZGET('WTS_ICD_MG_OLD',WTS_OLD(8,8),IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('ICD','CSF_ICD_1A',
     &'Fail to fetch run1A ICD/MG weights - use latest constants','W')
            LAST_RUN_1A = 0
            GOTO 999
          END IF
          FIRST= .FALSE.
          WRITE (MSG,12)NRUN
   12     FORMAT('CORRECT ICD CONSTANTS for RUN 1A - RUN', I7)
          CALL ERRMSG('CAL','CSF_ICD_1A',MSG,'I')
          CALL ERRMSG('CAL','CSF_ICD_1A',MSG,'S')
C
C ****  Check for run 1B before Feb. '95 shutdown;
C ****  if so, read CSF_ICD_1ST_HALF_RUN_1B_RCP:
C
        ELSEIF( NRUN .LE. FIRST_HALF_RUN_1B ) THEN
          CALL EZLOC('CSF_ICD_1ST_HALF_RUN_1B_RCP',LOC)
          IF (LOC.EQ.0) THEN
            CALL INRCP('D0$CALOR_OFF:CSF_ICD_1ST_HALF_RUN_1B.RCP',IER)
          END IF
          IF(IER.NE.0) THEN
            CALL EZGET_ERROR_TEXT (IER,MSG)
            CALL ERRMSG('CAL','CSF_ICD_1A',MSG,'W')
            CALL ERRMSG('CAL','CSF_ICD_1A',
     & 'FAIL to read CSF_ICD_1ST_HALF_RUN_1B_RCP - use defaults','W')
            FIRST_HALF_RUN_1B = 0
            GOTO 999
          END IF
          CALL EZPICK('CSF_ICD_1ST_HALF_RUN_1B_RCP')
          CALL EZERR(IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('ICD','CSF_ICD_1A',
     &        'Fail to pick CSF_ICD_1ST_HALF_RUN_1B_RCP','W')
            CALL INTMSG('Without CSF_ICD_1ST_HALF_RUN_1B_RCP, CAHITS')
            CALL INTMSG('will use default ICD corrections on')
            CALL INTMSG('mid-RUN 1B data')
            FIRST_HALF_RUN_1B = 0
            GOTO 999
          END IF
          CALL EZGET('FIRST_HALF_RUN_1B_CHK',
     &                FIRST_HALF_RUN_1B_CHK,IER)
          IF (FIRST_HALF_RUN_1B_CHK .NE. FIRST_HALF_RUN_1B) THEN
            CALL ERRMSG('ICD','CSF_ICD_1A',
     &        'CSF_ICD_1A VERSION MIXUP','W')
            FIRST_HALF_RUN_1B = 0
            GOTO 999
          END IF
          CALL EZGET('A_ICD_1ST_HALF_RUN_1B',AICD,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('ICD','CSF_ICD_1A',
     &'Fail to fetch A_ICD_1ST_HALF_RUN_1B - use default constants','W')
            FIRST_HALF_RUN_1B = 0
            GOTO 999
          END IF
          CALL EZGET('WTS_ICD_MG_OLD',WTS_OLD(8,8),IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('ICD','CSF_ICD_1A',
     &'Fail to fetch run1B ICD/MG weights - use latest constants','W')
            FIRST_HALF_RUN_1B = 0
            GOTO 999
          END IF
          FIRST= .FALSE.
          WRITE (MSG,13)NRUN
   13     FORMAT('CORRECT ICD CONSTANTS for 1st half RUN 1B - RUN', I7)
          CALL ERRMSG('CAL','CSF_ICD_1A',MSG,'I')
          CALL ERRMSG('CAL','CSF_ICD_1A',MSG,'S')
C
C ****  Finally, check for run 1B post-Feb. '95 shutdown, prior to
C ****  run 1C; if so, read CSF_ICD_2ND_HALF_RUN_1B_RCP:
C
        ELSEIF( NRUN .LE. SECOND_HALF_RUN_1B ) THEN
          CALL EZLOC('CSF_ICD_2ND_HALF_RUN_1B_RCP',LOC)
          IF (LOC.EQ.0) THEN
            CALL INRCP('D0$CALOR_OFF:CSF_ICD_2ND_HALF_RUN_1B.RCP',IER)
          END IF
          IF(IER.NE.0) THEN
            CALL EZGET_ERROR_TEXT (IER,MSG)
            CALL ERRMSG('CAL','CSF_ICD_1A',MSG,'W')
            CALL ERRMSG('CAL','CSF_ICD_1A',
     &'FAIL to read CSF_ICD_2ND_HALF_RUN_1B_RCP - use defaults','W')
            SECOND_HALF_RUN_1B = 0
            GOTO 999
          END IF
          CALL EZPICK('CSF_ICD_2ND_HALF_RUN_1B_RCP')
          CALL EZERR(IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('ICD','CSF_ICD_1A',
     &        'Fail to pick CSF_ICD_2ND_HALF_RUN_1B_RCP','W')
            CALL INTMSG('Without CSF_ICD_2ND_HALF_RUN_1B_RCP,')
            CALL INTMSG('CAHITS will use latest ICD corrections')
            CALL INTMSG('on 2nd half RUN 1B data')
            SECOND_HALF_RUN_1B = 0
            GOTO 999
          END IF
          CALL EZGET('SECOND_HALF_RUN_1B_CHK',
     &                SECOND_HALF_RUN_1B_CHK,IER)
          IF (SECOND_HALF_RUN_1B_CHK .NE. SECOND_HALF_RUN_1B) THEN
            CALL ERRMSG('ICD','CSF_ICD_1A',
     &        'CSF_ICD_1A VERSION MIXUP','W')
            SECOND_HALF_RUN_1B = 0
            GOTO 999
          END IF
          CALL EZGET('A_ICD_2ND_HALF_RUN_1B',AICD,IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('ICD','CSF_ICD_1A',
     &'Fail to fetch A_ICD_2ND_HALF_RUN_1B - use defaults', 'W')
            SECOND_HALF_RUN_1B = 0
            GOTO 999
          END IF
          CALL EZGET('WTS_ICD_MG_OLD',WTS_OLD(8,8),IER)
          IF(IER.NE.0) THEN
            CALL ERRMSG('ICD','CSF_ICD_1A',
     &'Fail to fetch run1B ICD/MG weights - use latest constants','W')
            SECOND_HALF_RUN_1B = 0
            GOTO 999
          END IF
          FIRST= .FALSE.
          WRITE (MSG,14)NRUN
   14     FORMAT('CORRECT ICD CONSTANTS for 2nd HALF RUN 1B - RUN', I7)
          CALL ERRMSG('CAL','CSF_ICD_1A',MSG,'I')
          CALL ERRMSG('CAL','CSF_ICD_1A',MSG,'S')
        ENDIF
C
C **** Everything looks o.k. - read the cell-by-cell corrections:
C
        CALL EZGET('CORRECTION_NORTH_LYR_09',CICD(9,1,1),IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('ICD','CSF_ICD_1A',
     &'Fail to fetch CORRECTION_NORTH_LYR_09 - use latest constants'
     &        ,'W')
          LAST_RUN_1A = 0
          FIRST_HALF_RUN_1B = 0
          SECOND_HALF_RUN_1B = 0
          GOTO 999
        END IF
C
C ****  Now the south...
C
        CALL EZGET('CORRECTION_SOUTH_LYR_09',CICD(9,1,2),IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('ICD','CSF_ICD_1A',
     &'Fail to fetch CORRECTION_SOUTH_LYR_09 - use latest constants'
     &        ,'W')
          LAST_RUN_1A = 0
          FIRST_HALF_RUN_1B = 0
          SECOND_HALF_RUN_1B = 0
          GOTO 999
        END IF
        CALL EZRSET
      ENDIF
C
C ****  Make temp link area:
C
      CALL STP_GSLINK('CSFICD1A',ICDC )
      CALL STP_GSLINK('CSFICD1A',ICDH )
      CALL STP_GSLINK('CSFICD1A',ICD_MG_W )
      STP_LSLINK(ICDC) = GZCSFC ()
      STP_LSLINK(ICDH) = GZCSFH ()
      STP_LSLINK(ICD_MG_W) = GZCSFW ()
C
C ****  get A for the MG's and replace the default value of A for 
C ****  the ICD before looping over cells:
C
      IETA =-12
      DO ILYR = 8, 10
        IMOD = CAL_MODULE(IETA,ILYR,MODULE)
        IF(ILYR.EQ.9) THEN 
          C(STP_LSLINK(ICDH)+1+IMOD) = AICD
          A(ILYR) = AICD
        ELSE
          A(ILYR) = C(STP_LSLINK(ICDH)+1+IMOD)
        ENDIF
      ENDDO
C
C ****  LOOP OVER CELLS
C
      DO ILYR = 8, 10   !   MG's and ICD
        DO IETA = -NETAL, NETAL
          IF(IETA.LT.0) THEN
            IZ = 1  ! NORTH
          ELSE
            IZ = 2  ! SOUTH
          END IF
          IF(CEXIST(IETA,1,ILYR)) THEN
C
C ****  Check for CSFC bank - book it if not present:
C
            IF( ILYR .EQ. 9) THEN
              LCSFC = GZCSFC ()
              IF (LCSFC.EQ.0) THEN
                CALL BKCSFC(0,LCSFC)
                STP_LSLINK(ICDC) = LCSFC
                IC(STP_LSLINK(ICDC)+2) = ILYR
              ELSE
                LCSFC = LZFIND(IDVSTP,LCSFC,ILYR,2)
                IF (LCSFC.EQ.0) THEN
                  CALL BKCSFC(0,LCSFC)
                  STP_LSLINK(ICDC) = LCSFC
                  IC(STP_LSLINK(ICDC)+2) = ILYR
                ELSE
                  STP_LSLINK(ICDC) = LCSFC
                END IF
              END IF
            END IF
C
C ****  LOOP OVER PHI
C
            DO IPHI = 1, NPHIL
              IF(CEXIST(IETA,IPHI,ILYR) .AND. 
     &           ILYR .EQ. 9) THEN
C
C ****  DETERMINE INDEX INTO CSFC BANK & REPLACE 
C ****  ICD CELL_BY_CELL CORRECTIONS:
C
                I =IPHI+(IETA+NETAL)*NPHIL  ! 1 to 4800
                C(STP_LSLINK(ICDC)+2+I) = CICD(ABS(IETA),IPHI,IZ)
              END IF
            END DO
C
C ****  Now, replace the default A*W by the new values:
C 
            I =ILYR+(IABS(IETA)-1)*NLYRL  ! 1 to 629 index into CSFW
            C(STP_LSLINK(ICD_MG_W)+1+I) = 
     &            A(ILYR)*WTS_OLD(ABS(IETA),ILYR)
          END IF
        END DO
      END DO
      CALL STP_RSLINK('CSFICD1A',ICDC )
      CALL STP_RSLINK('CSFICD1A',ICDH )
      CALL STP_RSLINK('CSF_ICD',ICD_MG_W )
C----------------------------------------------------------------------
  999 RETURN
      END
