      SUBROUTINE UPKING(IVAL,CVAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpacks a string given a integer value
C-
C-   Inputs  : IVAL - packed integer value
C-   Outputs : CVAL - String that represents the packed value
C-
C-   Created   9-MAR-1989   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IVAL,IVAL1,ICHAR, TIVAL, JBEG, JEND, I
      CHARACTER*1 GTLET  
      CHARACTER*1  NX(4)
      CHARACTER*4 CVAL
C----------------------------------------------------------------------
      TIVAL = IVAL
      DO 10 I=1, 4
        NX(I)= ' '
   10 CONTINUE
      JBEG=1
      JEND=0
C  Determing how many letters in string
      DO 20 I=1,4  
        TIVAL=INT(TIVAL/100)
        IF (TIVAL.EQ.0) THEN
          JEND = I
          GO TO 30
        ENDIF
   20 CONTINUE
   30 TIVAL =IVAL
      I=JEND-JBEG+1
C  Unpacking string
      DO 40 ICHAR=JBEG, JEND
        IVAL1=INT( TIVAL/(100.**(I-1)) ) 
        TIVAL=MOD( TIVAL,(100**(I-1)) )
        NX(I)=GTLET(IVAL1)
        I = I - 1
   40 CONTINUE
      DO 50 I=1,4
        CVAL(I:I)=NX(I)
   50 CONTINUE
  999 RETURN
      END
