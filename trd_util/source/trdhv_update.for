      SUBROUTINE TRDHV_UPDATE(ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Updates the HV information
C-                         in Lars fashion.
C-   Inputs  : Transmit data thru common TROP 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-DEC-1992   J-F Glicenstein
C-   Updated   4-FEB-1993   JFG  Automatically takes the closest values for
C-                          HV from HVMON or DBMON
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTROP.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:DBMUKEY.INC'
C     Lars current keys COMMON block.
      CHARACTER*8 CLASS_HVM
      DATA CLASS_HVM/'TRD_HV_A'/
      CHARACTER*6 CLASS_DBM
      DATA CLASS_DBM/'DBM_CD'/
      INTEGER NDEV_CANARY,NDEV_GAS,IER,NDEV_HV,LTROP
      INTEGER IDAT,ITIM,I,IERR
      PARAMETER (NDEV_HV = 48)
      PARAMETER (NDEV_CANARY = 6)
      PARAMETER (NDEV_GAS = 15)
      INTEGER V1TIM(2),ENDVL,NTRIES,NMAX,ADD_GAS
      DATA NMAX/3/,ADD_GAS/3/
      INTEGER         VTIM( 2 ),IRET1,MAXCHN,IRET_HVM
      REAL       VAL_HV_HVM(4,NDEV_HV), XHOU_HVM
      REAL       VAL_HV_DBM(NDEV_HV+3), XHOU_DBM
      REAL VAL_HV(NDEV_HV+3)
      INTEGER IRET_DBM
      INTEGER*2 UNPACKED_TIME(7)
      INTEGER II
      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND
      LOGICAL OK,SYS$NUMTIM,ERROR
      EXTERNAL SYS$NUMTIM
C     Validity dates
      INTEGER INFTIMDBM,SUPTIMDBM,INFDATDBM,SUPDATDBM
      INTEGER INFTIMHVM,SUPTIMHVM,INFDATHVM,SUPDATHVM
      SAVE INFTIMDBM,SUPTIMDBM,INFDATDBM,SUPDATDBM
      SAVE INFTIMHVM,SUPTIMHVM,INFDATHVM,SUPDATHVM
      LOGICAL DBM_VALID,HVM_VALID,FIRST
      INTEGER STAVAL,ENDVAL
      CHARACTER*17 HV_DEV(NDEV_HV+3)
      DATA FIRST/.TRUE./
      CHARACTER*1 X1
      CHARACTER*2 X2

      integer ihyes/3HYES/
      integer ilhyes/3Hyes/
C
      IF (FIRST) THEN
               DO 10 I=1,MIN0(9,NDEV_HV)
          WRITE(X1,1001) I
          HV_DEV(I) = 'TRD_HV_00'//X1//'.VOLT'
   10   CONTINUE
        DO 20 I=10,MAX0(9,NDEV_HV)
          WRITE(X2,1002) I
          HV_DEV(I) = 'TRD_HV_0'//X2//'.VOLT'
   20   CONTINUE
          HV_DEV(NDEV_HV+1) = 'TRD_CNGRIDHV.VOLT'
          HV_DEV(NDEV_HV+2) = 'TRD_CNPOTHV.VOLT'
          HV_DEV(NDEV_HV+3) = 'TRD_CNWINDHV.VOLT'
       FIRST = .FALSE.
       CALL EZPICK('TRD_RCP')
       CALL EZGET('USE_HVMON',I,IERR)
       CALL EZRSET
      ENDIF
      DBM_VALID = .TRUE.
      HVM_VALID = .TRUE.      
C  
C     Gets date and time for the event (might be replaced by a routine)
      VTIM( 1 ) = IQ( LHEAD + 4 )     ! Vax system time, for that event
      VTIM( 2 ) = IQ( LHEAD + 5 )
      OK = SYS$NUMTIM(UNPACKED_TIME(1),VTIM(1))
      YEAR = UNPACKED_TIME(1) - (UNPACKED_TIME(1)/100)*100
      MONTH = UNPACKED_TIME(2)
      DAY = UNPACKED_TIME(3)
      IDAT = 10000*YEAR+100*MONTH+DAY
      HOUR = UNPACKED_TIME(4)
      MINUTE = UNPACKED_TIME(5)
      SECOND = UNPACKED_TIME(6)
      ITIM = HOUR*10000+100*MINUTE+SECOND
C     Look if the HVMON and DBMON validity dates are still valid
      IF (IDAT.GT.SUPDATDBM.OR.IDAT.LT.INFDATDBM) THEN
       DBM_VALID = .FALSE.
      ENDIF
      IF (ITIM.GT.SUPTIMDBM.OR.IDAT.LT.INFTIMDBM) THEN
       DBM_VALID = .FALSE.
      ENDIF
      IF (IDAT.GT.SUPDATHVM.OR.IDAT.LT.INFDATHVM) THEN
       HVM_VALID = .FALSE.
      ENDIF
      IF (ITIM.GT.SUPTIMHVM.OR.IDAT.LT.INFTIMHVM) THEN
       HVM_VALID = .FALSE.
      ENDIF
C     If the validity dates are not correct, update from database
      IF (.NOT.DBM_VALID) THEN
C     Updates a la Lars
      NTRIES = 0
      CALL UCOPY(VTIM,V1TIM,2)
 100  CONTINUE
      NTRIES = NTRIES+1
C     Only 3 tries right now
      IF (NTRIES.GT.NMAX) THEN
       ERROR = .TRUE.
       GOTO 999
      ENDIF
      CALL DBMU_GETDM( CLASS_DBM,HV_DEV(1), NDEV_HV+3,V1TIM,1,' ', 
     &VAL_HV_DBM,XHOU_DBM, IRET_DBM)
      IF (IRET_DBM.EQ.0) THEN
C     No gas data, retries
       ENDVL = DBKEYS(4)
       CALL DBINCT(ENDVL,ADD_GAS,ENDVL)
       CALL D3UUT(V1TIM,ENDVL)
       GOTO 100
      ENDIF
      IF (IRET_DBM.LT.0) THEN
C     If no update or error occurs, send an warning message
       ERROR = .TRUE.
       CALL ERRMSG('DBMON error','TRDHV_UPDATE',' ','W')
      ELSE
C     If no error, get the new validity keys
       STAVAL = DBKEYS(3)       
       ENDVAL = DBKEYS(4)
       CALL DBUPTS(INFDATDBM,INFTIMDBM,STAVAL)       
       CALL DBUPTS(SUPDATDBM,SUPTIMDBM,ENDVAL)       
       CALL UCOPY(VAL_HV_DBM,VAL_HV,NDEV_HV+3)
      ENDIF
      ENDIF
C
      IF (.NOT.HVM_VALID.AND.I.EQ.iHYES.OR.I.EQ.ilHyes) THEN
      CALL DBMU_GETHV( CLASS_HVM,VTIM,' ',NDEV_HV,VAL_HV_HVM,
     &XHOU_HVM,IRET_HVM)
C    If IRET_HVM = 0, then simply there was no update: take the data
C    from DBMON
      IF (IRET_HVM.LT.0) THEN
C     If an error occurs, send an error message
       ERROR = .TRUE.
       CALL ERRMSG('HVMON error','TRDHV_UPDATE',' ','W')
      ELSE IF (IRET_HVM.EQ.NDEV_HV) THEN
C     If no error, get the new validity keys
       STAVAL = DBKEYS(3)       
       ENDVAL = DBKEYS(4)
       CALL DBUPTS(INFDATHVM,INFTIMHVM,STAVAL)       
       CALL DBUPTS(SUPDATHVM,SUPTIMHVM,ENDVAL)       
      ENDIF
      ENDIF
C     Tests which one of DBMON and HVMON is closest in time
      IF ((IRET_DBM.GT.0).AND.(IRET_HVM.GT.0).AND.((I.EQ.iHYES).OR.(I.
     &  EQ.ilHyes))) THEN
       IF ((ABS(XHOU_HVM).LT.ABS(XHOU_DBM))) THEN
        DO 30 I = 1,NDEV_HV
         VAL_HV(I) = VAL_HV_HVM(1,I)  
 30     CONTINUE
       ENDIF
      ENDIF
C     
C     Replaces all the data in common TROP
      IF (IRET_DBM.EQ.(NDEV_HV+3).OR.IRET_HVM.EQ.NDEV_HV) THEN
      LTROP=LC(LTGEN-IZTROP)
      IF (LTROP.GT.0) THEN
C     Update date.
        IC(LTROP+1) = IDAT
        IC(LTROP+2) = ITIM
        DO 900 I = 1,NDEV_HV
          C(LTROP+5+I) = VAL_HV(I)
  900   CONTINUE
C   7/11/93 Added canary HV devices
          C(LTROP+6+NDEV_HV+NDEV_GAS) = VAL_HV(NDEV_HV+1)
          C(LTROP+7+NDEV_HV+NDEV_GAS) = VAL_HV(NDEV_HV+2)
          C(LTROP+8+NDEV_HV+NDEV_GAS) = VAL_HV(NDEV_HV+3)
      ENDIF
      ENDIF
  999 RETURN
 1001 FORMAT(I1)
 1002 FORMAT(I2)
      END
