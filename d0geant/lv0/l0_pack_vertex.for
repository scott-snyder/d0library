      SUBROUTINE L0_PACK_VERTEX
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pack the level 0 vertex information
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   5-DEC-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER BLOCK_TYPE, BLOCK_LENGTH
      INTEGER LAST_BUNCH, FIRST_BUNCH
      INTEGER LAST_CHAN, FIRST_CHAN
      INTEGER BLOCK_WORD, DESCRIPTOR
      INTEGER FASTZ, J
      INTEGER VBOARD, HEAD
      INTEGER VERTEX
      LOGICAL FIRST
C
      INCLUDE 'D0$INC:LV0PARAM.INC'
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        BLOCK_TYPE=3
        BLOCK_LENGTH= 8
        LAST_BUNCH=1
        FIRST_BUNCH=1
        LAST_CHAN=80
        FIRST_CHAN=1
        FASTZ=0
        HEAD= 87                        ! Saved for HEAD and ADC get new value
        FIRST=.FALSE.
      ENDIF
      DO J = 1 , 2                      ! 1: north, 2: south
C
C ****  Decide between north and south
C
        IF ( J.EQ.1 ) THEN
          HEAD=87
        ELSE
          HEAD=95
        ENDIF
C
C ****  Pack BLOCK_WORD
C
        CALL MVBITS(BLOCK_TYPE,0,16,BLOCK_WORD,16)
        CALL MVBITS(BLOCK_LENGTH,0,16,BLOCK_WORD,0)
        DATA_WORD(HEAD+1)=BLOCK_WORD
C
C ****  Pack DESCRIPTOR
C
        CALL MVBITS(LAST_BUNCH,0,8,DESCRIPTOR,24)
        CALL MVBITS(FIRST_BUNCH,0,8,DESCRIPTOR,16)
        CALL MVBITS(FASTZ,0,1,DESCRIPTOR,1)
        VBOARD=J-1
        CALL MVBITS(VBOARD,0,1,DESCRIPTOR,0)
        DATA_WORD(HEAD+2)=DESCRIPTOR
C
C ****  Pack DATA_WORDs
C

C
C ****  Vertex Word 1
C
        CALL MVBITS(T2SUM(1,J),0,24,DATA_WORD(HEAD+3),8)
        CALL MVBITS(N(1,J),0,8,DATA_WORD(HEAD+3),0)
C
C ****  Vertex Word 2
C
        CALL MVBITS(TMIN(1,J),0,8,DATA_WORD(HEAD+4),24)
        CALL MVBITS(TMAX(1,J),0,8,DATA_WORD(HEAD+4),16)
        CALL MVBITS(TSUM(1,J),0,16,DATA_WORD(HEAD+4),0)
C
C ****  Vertex Word 3
C
        CALL MVBITS(T2SUM(2,J),0,24,DATA_WORD(HEAD+5),8)
        CALL MVBITS(N(2,J),0,8,DATA_WORD(HEAD+5),0)
C
C ****  Vertex Word 4
C
        CALL MVBITS(TMIN(2,J),0,8,DATA_WORD(HEAD+6),24)
        CALL MVBITS(TMAX(2,J),0,8,DATA_WORD(HEAD+6),16)
        CALL MVBITS(TSUM(2,J),0,16,DATA_WORD(HEAD+6),0)
C
C ****  Vertex Word 5
C
        CALL MVBITS(NINT(SIGMA(J)),0,8,DATA_WORD(HEAD+7),24)
        CALL MVBITS(TAVE(J),0,8,DATA_WORD(HEAD+7),16)
        CALL MVBITS(NUMHITS,0,8,DATA_WORD(HEAD+7),8)
        CALL MVBITS(EXCLUSIVE,0,1,DATA_WORD(HEAD+7),1)
        CALL MVBITS(SHORTY,0,1,DATA_WORD(HEAD+7),0)
C
C ****  Vertex Word 6
C
        IF ( J.EQ.1 ) THEN
          VERTEX=NINT(VTX/.75)
          CALL MVBITS(VERTEX,0,16,DATA_WORD(HEAD+8),16)
        ELSE
          CALL MVBITS(0,0,16,DATA_WORD(HEAD+8),16)
        ENDIF
C        CALL MVBITS(FASTZ,0,8,DATA_WORD(HEAD+8),8)
        CALL MVBITS(1,0,1,DATA_WORD(HEAD+8),MI-1)
        IF ( MI.EQ.4 ) THEN
          CALL MVBITS(1,0,1,DATA_WORD(HEAD+8),2)
        ELSEIF ( MI.EQ.1 ) THEN
          CALL MVBITS(1,0,1,DATA_WORD(HEAD+8),1)
        ENDIF
        CALL MVBITS(GOODZ,0,1,DATA_WORD(HEAD+8),4)
        CALL MVBITS(INTER,0,1,DATA_WORD(HEAD+8),5)
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
