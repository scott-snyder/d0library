      SUBROUTINE UNPACK_INTEGER (OUTPUT,OFFSET,N,P,LSTART,LNEXT)
C----------------------------------------------------------------------
C-   PURPOSE AND METHODS : UNPACK N INTEGERS PACKED WITH PACK_INTEGER
C-   CREATED  21-DEC-1992   ALAIN PLUQUET
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER OUTPUT(*),N,P,LSTART,LNEXT,I,J,K,L,OFFSET,JBYT
      IF (N.GT.0) THEN
        L=32/P
        DO I=1,N
          J=(I-1)/P
          K=MOD(I-1,P)*L+1
          OUTPUT(OFFSET-1+I)=JBYT(IQ(LSTART+J),K,L)
        ENDDO
        LNEXT=LSTART+1+(N-1)/P
      ELSE
        LNEXT=LSTART
      ENDIF
      END
