      SUBROUTINE PACK_REAL (INPUT,SCALE,OFFSET,N,P,LSTART,LNEXT)
C----------------------------------------------------------------------
C-   PURPOSE AND METHODS : pack n reals
C-   INPUTS: INPUT     real(*)
C-           SCALE     real
C-           OFFSET    integer
C-           N         integer
C-           P         integer
C-           LSTART    integer
C-
C-   N reals in array INPUT (first real at position OFFSET in INPUT)
C-               |
C-               | packing with P reals per ZEBRA word. Before packing,
C-               | reals are multiplied by SCALE and truncated.
C-               |
C-               V
C-   bank(s) (first bank pointed to by LSTART)
C_
C-   OUTPUTS : LNEXT   integer
C-             After packing, the next available link is LNEXT.
C-   CREATED  21-DEC-1992   ALAIN PLUQUET
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER N,P,LSTART,LNEXT,I,J,K,L,OFFSET
      REAL INPUT(*),SCALE
C----------------------------------------------------------------------
      IF (N.GT.0) THEN
c        print*,' dans pack_real input,scale',(input(i),i=1,n),
c     &    scale,'offset,n,p,lstart ',offset,n,p,lstart
        L=32/P
        DO I=1,N
          J=(I-1)/P
          K=MOD(I-1,P)*L+1
c          print*,'i,j,k',i,j,k,' offset-1+i', offset-1+i
          CALL SBYT(NINT(INPUT(OFFSET-1+I)*SCALE),IQ(LSTART+J),K,L)
        ENDDO
        LNEXT=LSTART+1+(N-1)/P
      ELSE
        LNEXT=LSTART
      ENDIF
      END
