      SUBROUTINE FETCH_URAN_TROP(URANIUM_RUN,UR,PRES,TEMP,HV,HP,QJT_UR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read Uranium info. from TROP bank
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-JAN-1995   Srini Rajagopalan - Stolen from Lars
C-   Modified  1-NOV-1994   Lewis Taylor Goss--modified to work with CALIB DB
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTROP.LINK'
C
C         | NDEV_HV+J*13+11 = Uranium run number - run J
C         | NDEV_HV+J*13+12 = Time and date taken (CLDR packed form) - run J
C         | NDEV_HV+J*13+13 = TRD Pressure - run J
C         | NDEV_HV+J*13+14 = TRD Temperature - run J
C         | NDEV_HV+J*13+15 = Uranium noise for layer 1 - run J
C         | NDEV_HV+J*13+16 = Uranium noise for layer 2 - run J
C J = 1,2 | NDEV_HV+J*13+17 = Uranium noise for layer 3 - run J
C         | NDEV_HV+J*13+18 = Anode high voltage for layer 1 - run J
C         | NDEV_HV+J*13+19 = Anode high voltage for layer 2 - run J
C         | NDEV_HV+J*13+20 = Anode high voltage for layer 3 - run J
C         | NDEV_HV+J*13+21 = Pot. high voltage for layer 1 - run J
C         | NDEV_HV+J*13+22 = Pot. high voltage for layer 2 - run J
C         | NDEV_HV+J*13+23 = Pot. high voltage for layer 3 - run J
C
      INTEGER URANIUM_RUN(2)
      REAL UR(2,3),PRES(2),TEMP(2),HV(2,3),HP(2,3),QJT_UR(2)
      INTEGER LTROP,J,NDEV_HV
      PARAMETER (NDEV_HV = 48)
C----------------------------------------------------------------------
      LTROP = LC(LTGEN-IZTROP)
C
      DO J = 1,2
        URANIUM_RUN(J) = INT(C(LTROP+NDEV_HV+J*13+11))
        QJT_UR(J)      = C(LTROP+NDEV_HV+J*13+12)
        PRES(J)        = C(LTROP+NDEV_HV+J*13+13)
        TEMP(J)        = C(LTROP+NDEV_HV+J*13+14)
        UR(J,1)        = C(LTROP+NDEV_HV+J*13+15)
        UR(J,2)        = C(LTROP+NDEV_HV+J*13+16)
        UR(J,3)        = C(LTROP+NDEV_HV+J*13+17)
        HV(J,1)        = C(LTROP+NDEV_HV+J*13+18)
        HV(J,2)        = C(LTROP+NDEV_HV+J*13+19)
        HV(J,3)        = C(LTROP+NDEV_HV+J*13+20)
        HP(J,1)        = C(LTROP+NDEV_HV+J*13+21)
        HP(J,2)        = C(LTROP+NDEV_HV+J*13+22)
        HP(J,3)        = C(LTROP+NDEV_HV+J*13+23)
      ENDDO
C
  999 CONTINUE
      END
