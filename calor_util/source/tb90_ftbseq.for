      SUBROUTINE TB90_FTBSEQ(IFTBV,CONV,PINV,SEQV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Returns the SEQUENTIAL ADC address of 
C-                          channel corresponding to a FEEDTHROUGH
C-                          address
C-   Inputs  : FEEDTHROUGH address IFTBV,CONV,PINV
C-   Outputs : SEQUENTIAL ADC address SEQV
C-   Controls: 
C-
C-   Created  20-FEB-1990   AWHITE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TB_SORT_ORD.INC'
      INTEGER IFTBV,CONV,PINV,SEQV
      INTEGER VAL,VAL1,VAL2,IP,IMARK1,IMARK2,I
C----------------------------------------------------------------------
      SEQV=0
C
C--- Find the first occurrence of IFTBV value
C
      DO 10 I=1,SIZE
        IMARK1=I
        VAL=IFTB(I,2)
        IF(VAL.EQ.IFTBV) GO TO 15
   10 CONTINUE
      GO TO 999
C
C--- Now find the first occurrence of CONV
C
   15 DO 20 I=IMARK1,SIZE
        IMARK2=I
        VAL1=IFTB(I,2)
        IF(VAL1.NE.IFTBV) GO TO 999
        VAL2=CON(I,2)
        IF(VAL2.EQ.CONV) GO TO 25
   20 CONTINUE
      GO TO 999
C
C--- Now find the required pin - PINV
C
   25 DO 30 I=IMARK2,SIZE
        VAL1=CON(I,2)
        IF(VAL1.NE.CONV) GO TO 999
        VAL2=PIN(I,2)
        IP=I
        IF(VAL2.EQ.PINV) GO TO 35
   30 CONTINUE
      GO TO 999
C
C--- Return the corresponding SEQUENTIAL ADC address
C
   35 SEQV=SEQADC(IP,2)
C
  999 RETURN
      END
