      SUBROUTINE UNPACK_REAL (OUTPUT,SCALE,OFFSET,N,P,LSTART,LNEXT)
C----------------------------------------------------------------------
C-   PURPOSE AND METHODS : UNPACK N REALS PACKED WITH PACK_REAL
C-   CREATED  21-DEC-1992   ALAIN PLUQUET
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER N,P,LSTART,LNEXT,I,J,K,L,OFFSET,JBYT
      REAL OUTPUT(*),SCALE
C----------------------------------------------------------------------
      IF (N.GT.0) THEN
        L=32/P
        DO I=1,N
          J=(I-1)/P
          K=MOD(I-1,P)*L+1
          OUTPUT(OFFSET-1+I)=FLOAT(JBYT(IQ(LSTART+J),K,L))/SCALE
        ENDDO
        LNEXT=LSTART+1+(N-1)/P
      ELSE
        LNEXT=LSTART
      ENDIF
      END
