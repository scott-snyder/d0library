      SUBROUTINE DBCLB_READ(PATH,DECT,CALTYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For a specific run, give data base info
C-
C-   Inputs :  CALTYPE - Calibration type
C-             PATH    - DBL3 Path name
C-             DECT    - Detector type
C-
C-   Outputs : None
C-
C-   Created    24-FEB-90   by Jan Guida, Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CALIB.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:DBSTP.INC'
      INCLUDE 'D0$INC:LKCALIB.INC'
C
      CHARACTER*(*) CALTYPE,PATH,DECT
      CHARACTER*80 MSG
      INTEGER NRUN,CRATE
      INTEGER PEDRUN,LBANK
      INTEGER DATE,TIME,D(3),T(3)
C
      EQUIVALENCE(CALIB_LNK(1),LBANK)
C
C----------------------------------------------------------------------
C
      CALL GETPAR(1,' Enter input run Number > ','I',NRUN)
      IF(DECT.EQ.'MUO')THEN
        CALL GETPAR(1,' Enter Module Number >','I',CRATE)
      ELSE
        CALL GETPAR(1,' Enter Crate Number >','I',CRATE)
      ENDIF
      IF (CRATE.GT.MAXCRT) THEN
        CALL INTMSG(' Invalid Crate Number ')
        GO TO 999
      ENDIF
C
      IF (NRUN.LE.1 .OR. NRUN.GT.999999990) NRUN = 999999990
      CALL DBCLB_FETCH(PATH,NRUN,CRATE)
      IF (LBANK.EQ.0) THEN
        CALL INTMSG(' Error in finding requested data ')
        WRITE(MSG,14)DECT,CALTYPE,CALTYPE(17:17),NRUN,CRATE
   14   FORMAT(1X,A3,' Requested Data for Type = ',A8,'-',A1,' Run = ',
     &        I9.9,' Crate = ',I3.3)
        CALL INTMSG(MSG)
        GO TO 999
      ENDIF
      CALL INTMSG(
     &' **************************************************************')
      WRITE(MSG,15)NRUN,CRATE,CALTYPE,CALTYPE(17:17)
   15 FORMAT(2x,' Input run Number = ',I9,' Crate = ',I3,1x,
     &     'Type = ',A8,'-',A1)
      CALL INTMSG(MSG)
      PEDRUN = IC(LBANK+6)
      WRITE(MSG,20)PEDRUN
   20 FORMAT(2x,' Calibration Run Number = ',I10)
      CALL INTMSG(MSG)
C
      DATE = IC(LBANK+7)
      TIME = IC(LBANK+8)
      D(1) = DATE/10000
      D(2) = DATE/100 - D(1)*100
      D(3) = DATE - D(1)*10000 - D(2)*100
      T(1) = TIME/10000
      T(2) = TIME/100 - T(1)*100
      T(3) = TIME - T(1)*10000 - T(2)*100
      WRITE(MSG,25)D(1),D(2),D(3),T(1),T(2),T(3)
   25 FORMAT(2x,' Run taken on Date = ',I2.2,'-',I2.2,'-',I2.2,
     &   ' Time = ',I2.2,':',I2.2,':',I2.2)
      CALL INTMSG(MSG)
      WRITE(MSG,30)IC(LKEYS(CRATE)+3),IC(LKEYS(CRATE)+4)
   30 FORMAT(3x,'Corrected validity range for this run = '
     &     ,I10,' TO ',I10)
      CALL INTMSG(MSG)
      CALL INTMSG(
     &' **************************************************************')
C
  999 RETURN
      END
