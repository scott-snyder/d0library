      SUBROUTINE PACK_INTEGER (INPUT,OFFSET,N,P,LSTART,LNEXT)
C----------------------------------------------------------------------
C-   PURPOSE AND METHODS : pack n integers 
C-   INPUTS: INPUT     integer(*)
C-           OFFSET    integer
C-           N         integer
C-           P         integer
C-           LSTART    integer
C-
C-   N integers in array INPUT (first integer at position OFFSET in INPUT)
C-               |
C-               | packing with P integers per ZEBRA word
C-               |
C-               V
C-   bank(s) (first bank pointed to by LSTART)
C_
C-   P=16 allows 16 integers in [0,7]     per zebra word
C-   P=8  allows  8 integers in [0,15]    per zebra word
C-   P=4  allows  4 integers in [0,255]   per zebra word
C-   P=2  allows  2 integers in [0,65535] per zebra word
C-
C-   OUTPUTS : LNEXT   integer
C-             After packing, the next available link is LNEXT.
C-   CREATED  21-DEC-1992   ALAIN PLUQUET
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER INPUT(*),N,P,LSTART,LNEXT,I,J,K,L,OFFSET
C----------------------------------------------------------------------
c      PRINT*,' ENTER PACK_INTEGER, N=',N
      IF (N.GT.0) THEN
        L=32/P
        DO I=1,N
          J=(I-1)/P
          K=MOD(I-1,P)*L+1
c          PRINT*,'I,J,K',I,J,K,' OFFSET-1+I',OFFSET-1+I,' INPUT',
c     &      INPUT(OFFSET-1+I)
          CALL SBYT(INPUT(OFFSET-1+I),IQ(LSTART+J),K,L)
        ENDDO
c        print*,' dans pack_integr.lstart',lstart,'n',n,'p',p
        LNEXT=LSTART+1+(N-1)/P
      ELSE
        LNEXT=LSTART
      ENDIF
      END
