      SUBROUTINE PRSAHT(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  18-JAN-1995   Andrei Mayorov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LUN
      INTEGER LSAHS,GZSAHS,LSAHT,LHIT
      EXTERNAL GZSAHS
      INTEGER I,J,NHIT,STA,SEC,PL,TUBE
C----------------------------------------------------------------------
      WRITE(LUN,*) ' **** SAHT bank ****'
      LSAHS=GZSAHS()
      DO I=1,18
        LSAHT=LQ(LSAHS-I)
        NHIT=IQ(LSAHS+I)
        WRITE(LUN,*) ' SAHT #',I,' nhit=',NHIT
        DO J=1,NHIT
          LHIT=LSAHT+13*(J-1)
          STA=IBITS(IQ(LHIT+2),0,5)
          SEC=IBITS(IQ(LHIT+2),5,5)
          PL=IBITS(IQ(LHIT+2),10,5)
          TUBE=IBITS(IQ(LHIT+2),16,16)
          WRITE(LUN,100) IQ(LHIT+1),IQ(LHIT+2),STA,SEC,PL,TUBE,
     &      IQ(LHIT+3),Q(LHIT+4)
        END DO
      END DO
  999 RETURN
  100 FORMAT('flag ',I2,'  tube ad ',I7,'(',3(I1,1X),I3,')''  geom ad.',
     &  I8,'  dist.',E10.3)
      END
