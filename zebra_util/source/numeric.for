      SUBROUTINE NUMERIC(CRD,ITYP,INT_NUM,REAL_NUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode character data into integer or real
C-
C-   Inputs  : CRD  Character*(*)
C-   Outputs : ITYP=1 Integer data, =2 Real data, =0, undefined
C-           : INT_NUM  Integer decoded value
C-           : REAL_NUM Real decoded value
C- Routine takes the first numerical field on CRD
C-
C-   Created  26-NOV-1987   Rajendran Raja
C-   Updated  20-AUG-1992   sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER I,J,K,L,M,LENG,IFLOAT,ITYP,INT_NUM
      REAL REAL_NUM
      CHARACTER*(*) CRD
      CHARACTER*1 NUMER(11)
      DATA NUMER/'.','0','1','2','3','4','5','6','7','8','9'/
C
      ITYP=0   !default 
C
      LENG=LEN(CRD)
C
          DO 20 J = 1 , LENG
            DO 30 K= 1 , 11
              IF(CRD(J:J).EQ.NUMER(K))THEN   !1st numeric character
                IF(J.EQ.1)GO TO 20           !NO blank encountered
                IF(CRD(J-1:J-1).NE.' ')GO TO 20 !Prev Char not blank
                DO 40 L = J+1, LENG
                  DO 50 M = 1,11
                    IF(CRD(L:L).EQ.NUMER(M))GO TO 40
   50             CONTINUE
                  GO TO 11      !End of numerical field
   40           CONTINUE
              ENDIF
   30       CONTINUE
   20     CONTINUE
C
      ITYP=0
      GO TO 999
   11 CONTINUE     !Numeric field present
      IFLOAT=0
      DO 60 I = J, L-1
        IF(CRD(I:I).EQ.'.')IFLOAT=1
   60 CONTINUE
      IF(IFLOAT.EQ.0)THEN
        READ(CRD(J:L-1),*)INT_NUM
        ITYP=1
      ELSEIF((L-J).GT.1)THEN    ! A mere '.' won't do
        READ(CRD(J:L-1),*)REAL_NUM
        ITYP=2
      ENDIF
C
  999 RETURN
      END
