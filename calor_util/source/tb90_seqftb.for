      SUBROUTINE TB90_SEQFTB(SEQV,NUMG,IFTBV,CONV,PINV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Feedthrough Board address(es) of
C-                 channel(s) corresponding to a sequential ADC address.
C-   Inputs  : Sequential ADC address - SEQV
C-   Outputs : Arrays of values for Feedthrough Board address(es)
C-             NUMG - number of corresponding Feedthrough Board address(es)
C-             IFTBV(), CONV(), PINV().
C-   Controls: 
C-
C-   Created  7-FEB-1989   
C-   by  Andrew P. White
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TB_SORT_ORD.INC'
      INTEGER I,ISTAT,IP,VAL
      INTEGER SEQV,NUMG,IFTBV(20),CONV(20),PINV(20)
C----------------------------------------------------------------------
C
      NUMG=0
C
C- Find the first occurence of a value of SEQADC.
C
        DO 10 I=1,SIZE
          VAL=SEQADC(I,2)
          IP=I
          IF(VAL.EQ.SEQV) GO TO 50
   10   CONTINUE
        GO TO 999
C
C- Now extract the ganged channels (if any).
C
   50 NUMG=ING(IP,2)
      DO 100 I=1,NUMG
        IFTBV(I)=IFTB(IP,2)
        CONV(I)=CON(IP,2)
        PINV(I)=PIN(IP,2)
        IP=IP+1
  100 CONTINUE
C
  999 RETURN
      END
