      SUBROUTINE TRDCAN_UPDATE(ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Updates the canary/gas information
C-                         in Lars fashion.
C-   Inputs  : Transmit data thru common TROP
C-   Outputs :
C-   Controls:
C-
C-   Created   2-DEC-1992   J-F Glicenstein
C-   Updated  14-APR-1993   J.Fr. Glicenstein Protection against IRETi=0
C-   Updated  18-NOV-1993   J.Fr. Glicenstein  Change to run1B devices
C-   Updated   7-DEC-1993   JFG  remove canary HV devices (added in
C-            TRDHV_UPDATE 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTROP.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:DBMUKEY.INC'
      CHARACTER*11 CLASS(2)
      DATA CLASS/'CANARY','DBM_TRD_GAS'/
      INTEGER ADD_CAN,ADD_GAS,NMAX
      DATA ADD_CAN/3/,ADD_GAS/3/,NMAX/3/
      INTEGER NDEV_CANARY,NDEV_GAS,IER,NDEV_HV,LTROP
      INTEGER IDAT,ITIM,I
      PARAMETER (NDEV_HV = 48)
      PARAMETER (NDEV_CANARY = 6)
      CHARACTER*8 CANARY_DEV(NDEV_CANARY)
      PARAMETER (NDEV_GAS = 15)
      DATA CANARY_DEV/'TRD.GAIN','TRD.DGAI','TRD.ATTC','TRD.DATT',
     &'TRD.ENRJ','TRD.SNRJ'/
      CHARACTER*17 GAS_DEV_RUN1A(NDEV_GAS)
      DATA GAS_DEV_RUN1A/'TRD_PT722.PRES','TRD_PT101.PRES',
     &'TRD_TT615.TEMP',
     &'TRD_TT390.TEMP','TRD_OXT505.CONC','TRD_MT503.CONC',
     &'TRD_PT209.PRES','TRD_PT266.PRES','TRD_PT269.PRES',
     &'TRD_PT309.PRES','TRD_PT366.PRES','TRD_PT369.PRES',
     &'TRD_PT409.PRES','TRD_PT466.PRES','TRD_PT469.PRES'/
      CHARACTER*17 GAS_DEV(NDEV_GAS)
       DATA GAS_DEV/'TRD_GAS.PPAT','TRD_GAS.PRAT','TRD_GAS.TRM',
     &'TRD_GAS.TPLT','TRD_GAS.AO2','TRD_GAS.PCAN',
     &'TRD_GAS.PDT1','TRD_GAS.PRD1','TRD_GAS.PGP1',
     &'TRD_GAS.PDT2','TRD_GAS.PRD2','TRD_GAS.PGP2',
     &'TRD_GAS.PDT3','TRD_GAS.PRD3','TRD_GAS.PGP3'/
      LOGICAL RUN1A
      EXTERNAL RUN1A
      INTEGER V1TIM(2),ENDVL,NTRIES
      INTEGER         VTIM( 2 ),IRET1,MAXCHN,IRET
      REAL       VAL_CANARY(1,NDEV_CANARY), VAL_GAS(1,NDEV_GAS), XHOU
      INTEGER*2 UNPACKED_TIME(7)
      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND
      LOGICAL OK,SYS$NUMTIM,ERROR,FIRST
      DATA FIRST/.TRUE./
      EXTERNAL SYS$NUMTIM
C
C   If RUN1A, then gets back to old gas devices
      IF (FIRST.AND.RUN1A()) THEN
        DO 5 I = 1,NDEV_GAS
 5      GAS_DEV(I) = GAS_DEV_RUN1A(I)
        CLASS(2) = 'DBM_CD'
       FIRST = .FALSE.
      ENDIF
C
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
C
      NTRIES = 0
      CALL UCOPY(VTIM,V1TIM,2)
C     Implement Lars R. idea
 100  CONTINUE
      NTRIES = NTRIES+1
C     Only 3 tries right now
      IF (NTRIES.GT.NMAX) THEN
       ERROR = .TRUE.
       GOTO 999
      ENDIF
      CALL DBMU_GETDM( CLASS(1),CANARY_DEV(1), NDEV_CANARY,V1TIM,1,' ',
     &VAL_CANARY,XHOU, IRET1 )
      IF (IRET1.EQ.0) THEN
C     Could be a CDC/FDC only run, retries
       ENDVL = DBKEYS(4)
       CALL DBINCT(ENDVL,ADD_CAN,ENDVL)
       CALL D3UUT(V1TIM,ENDVL)
       GOTO 100
      ENDIF
      IF (IRET1.LT.0) THEN
C     If no update or error occurs, exits
       ERROR = .TRUE.
       GOTO 999
      ENDIF
C
C Now gas data
      NTRIES = 0
      CALL UCOPY(VTIM,V1TIM,2)
C     Implement Lars R. idea
 200  CONTINUE
      NTRIES = NTRIES+1
C     Only 3 tries right now
      IF (NTRIES.GT.NMAX) THEN
       ERROR = .TRUE.
       GOTO 999
      ENDIF
      CALL DBMU_GETDM( CLASS(2),GAS_DEV(1),NDEV_GAS,V1TIM,1,' ',
     &VAL_GAS, XHOU, IRET )
      IF (IRET.EQ.0) THEN
C     No gas data, retries
       ENDVL = DBKEYS(4)
       CALL DBINCT(ENDVL,ADD_GAS,ENDVL)
       CALL D3UUT(V1TIM,ENDVL)
       GOTO 200
      ENDIF
      IF (IRET.LT.0) THEN
C     If no update or error occurs, exits
       ERROR = .TRUE.
       GOTO 999
      ENDIF
C     Replaces all the data in common TROP
      LTROP=LC(LTGEN-IZTROP)
      IF (LTROP.GT.0) THEN
C     Update date.
        IC(LTROP+1) = IDAT
        IC(LTROP+2) = ITIM
        C(LTROP+3) = VAL_CANARY(1,1) ! gain with 50% half max (ADC counts/10)
        C(LTROP+4) = VAL_CANARY(1,3) ! slope (%)
        C(LTROP+5) = VAL_CANARY(1,5) ! gain with 80% half max (ADC counts)
        DO 800 I = 1,NDEV_GAS
          C(LTROP+5+NDEV_HV+I) = VAL_GAS(1,I)
  800   CONTINUE
      ENDIF
  999 RETURN
      END
