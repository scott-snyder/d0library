C----------------------------------------------------------------------
C
C                   DDDDDDDD           0000   0
C                   D       D         0    0 0
C                   D        D       0      0
C                   D         D     0      0 0
C                   D         D     0     0  0
C                   D         D     0    0   0
C                   D         D     0   0    0
C                   D         D     0  0     0
C                   D        D       00     0
C                   D       D        00    0
C                   DDDDDDDD        0  0000
C
C
C----------------------------------------------------------------------
C
C#######################################################################
      PROGRAM CSFCORRECT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-   Purpose and Methods : Create the CSF bank structure which contains the
C-   sampling fraction weights adn corrections contained in CSF_RCP.
C-   CSF banks contain the conversion factor from calorimeter cell ADC
C-   counts to total energy for every cell in the CC, ECN, ECS, ICD
C-   and Massless Gaps. See D0$STP$CAL:CSFMAKE.DOC for more detail.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  24-JUN-1993   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I, NA, NB, IER, IUSER, RCPUNIT
      LOGICAL OK
      REAL     A(10),ALPHA(10)
      REAL    TB_MOMENTUM_CORR(10), PULS_INSTABILITY(10)
      CHARACTER*14 MAIN_RCP_FILE,MAIN_RCP
      CHARACTER*80 MSG
      PARAMETER( MAIN_RCP_FILE = 'CSFCORRECT_RCP' )
      PARAMETER( MAIN_RCP = 'CSF_RCP' )
      PARAMETER( IUSER = 1946 )
C----------------------------------------------------------------------
      IER = 0
C
C ****  Read in Main RCP file, CSF and corrections to be applied
C
      CALL MZEBRA(0)
      CALL INZSTP
      CALL INRCP(MAIN_RCP_FILE,IER)
      CALL EZPICK(MAIN_RCP)
C
C ****  GET OVERALL CONVERSION 'A' : CHARGE to TOTAL ENERGY
C
      CALL EZGET('ALPHA',ALPHA,IER)  ! CCEM ECEM CCMG ICD ECMG CCFH ECIH ECMH CCCH ECOH
      IF(IER.NE.0) GOTO 999
      CALL EZGET_SIZE('ALPHA',NA,IER)  !  NUMBER OF MODULES
      IF (NA.NE.10) THEN  
        MSG = ' NEED 10 VALUES IN ALPHA '
        CALL ERRMSG('A_WRONG','CSFCORRECT... EXITING',MSG,'W')
        GOTO 999
      END IF
C
C ****  Read in corrections
C
      CALL EZGET('TBL2_PULSER_INSTABILITY_CORR',PULS_INSTABILITY,IER)  
      IF(IER.NE.0) GOTO 999
      CALL EZGET_SIZE('TBL2_PULSER_INSTABILITY_CORR',NB,IER)  
      IF (NB.NE.10) THEN  
        MSG = ' NEED 10 VALUES IN PULSER_INSTABILITY '
        CALL ERRMSG('PULSER_INSTABILITY_WRONG','CSFCORRECT... EXITING',
     &    MSG,'W')
        GOTO 999
      END IF
      CALL EZGET('TB_MOMENTUM_CORR',TB_MOMENTUM_CORR,IER)  
      IF(IER.NE.0) GOTO 999
      CALL EZGET_SIZE('TB_MOMENTUM_CORR',NB,IER)  
      IF (NB.NE.10) THEN  
        MSG = ' NEED 10 VALUES IN TB_MOMENTUM '
        CALL ERRMSG('TB_MOMENTUM_WRONG','CSFCORRECT... EXITING',MSG,'W')
        GOTO 999
      END IF
C
C ****  Apply Corrections to 'A' : CHARGE to TOTAL ENERGY
C
      DO I = 1, NA
        A(I) = ALPHA(I) * TB_MOMENTUM_CORR(I) * PULS_INSTABILITY(I)
      ENDDO
C
C ****  Modify CSFCORRECT : set new values
C
      CALL EZSET ('A',A,IER)
C
C ****  Write out the corrected RCP file
C
      CALL GTUNIT (IUSER,RCPUNIT,IER)
      CALL D0OPEN (RCPUNIT,'CSF.RCP','OF',OK)
      CALL EZDUMP (RCPUNIT,0,0)
      CALL EZRSET
C----------------------------------------------------------------------
  999 CONTINUE
      END
