      SUBROUTINE TRD_FIRST_UPDATE(ID1,IT1,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds the first HV update in HVMON
C-                      ** Only valid at the RECO level. **
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-DEC-1992   Jean-Francois Glicenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ID1,IT1
      LOGICAL OK
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
	  INCLUDE 'D0$LINKS:IZTROP.LINK'
      CHARACTER*8 CLASS
      DATA CLASS/'TRD_HV_A'/
      INTEGER NDEV_CANARY,NDEV_GAS,IER,NDEV_HV,LTROP
      INTEGER IDAT,ITIM,I
      INTEGER GZTROP
      EXTERNAL GZTROP
      PARAMETER (NDEV_HV = 48)
      PARAMETER (NDEV_CANARY = 6)
      PARAMETER (NDEV_GAS = 18)
      INTEGER         VTIM( 2 ),IRET1,MAXCHN,IRET
      REAL       VAL_HV(4,NDEV_HV), XHOU
      INTEGER*2 UNPACKED_TIME(7)
      INTEGER II
      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND
      LOGICAL OKK,SYS$NUMTIM,ERROR
      EXTERNAL SYS$NUMTIM
C
      VTIM( 1 ) = IQ( LHEAD + 4 )     ! Vax system time
      VTIM( 2 ) = IQ( LHEAD + 5 )
      OKK = SYS$NUMTIM(UNPACKED_TIME(1),VTIM(1))
      YEAR = UNPACKED_TIME(1) - (UNPACKED_TIME(1)/100)*100
      MONTH = UNPACKED_TIME(2)
      DAY = UNPACKED_TIME(3)
      IDAT = 10000*YEAR+100*MONTH+DAY
      HOUR = UNPACKED_TIME(4)
      MINUTE = UNPACKED_TIME(5)
      SECOND = UNPACKED_TIME(6)
      ITIM = HOUR*10000+100*MINUTE+SECOND
C
      CALL DBMU_GETHV( CLASS,VTIM,' ',NDEV_HV,VAL_HV,XHOU, IRET )
      IF (IRET.LT.0) THEN
C     If no update or error occurs, exits
       ERROR = .TRUE.
       GOTO 999
      ENDIF
      IF (IRET.EQ.0) GOTO 999
      LTROP = LC(LTGEN-IZTROP)
      IF (LTROP.LT.0) GOTO 999
      IF (IRET.EQ.NDEV_HV) THEN
        IC(LTROP+1) = IDAT
        IC(LTROP+2) = ITIM
         DO 800 I = 1,NDEV_HV
          C(LTROP+5+I) = VAL_HV(1,I)
  800    CONTINUE
        OK = .TRUE.
      ENDIF
  999 RETURN
      END
