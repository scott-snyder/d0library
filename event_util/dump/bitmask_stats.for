      SUBROUTINE BITMASK_STATS(ITYPE,MASK,NBITS,COUNTER,CORRELATE)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Keep statistics based on the bits set in MASK
C-
C-   Inputs  : ITYPE  - 0 ==> Count cleared bits, else count set bits
C-             MASK   - The bitmask to use in accumulating statistics
C-             NBITS  - The number of bits to test.
C-             COUNTER - Running statistics summary. COUNTER(1,I) 
C-                is number of times bit I is set (or times cleared 
C-                if ITYPE=0).  COUNTER(2,I) is number of times all
C-                bits prior to and including bit I have been set
C-                (cumulative).
C-   Outputs :
C-   Controls:
C-
C-   Created   3-Feb-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITYPE,MASK(*),NBITS,COUNTER(2,*),CORRELATE(NBITS,*)
      INTEGER I,J,JBIT,II,JJ
C-----------------------------------------------------------------------
C
C
      IF( ITYPE.EQ.0 ) THEN
        DO I=1,NBITS/32+1
          IF( ITYPE.EQ.0 ) MASK(I)=NOT(MASK(I))
        ENDDO
      ENDIF
C
      DO I=1,NBITS
        II=(I-1)/32+1
        JJ=MOD(I-1,32)+1
        IF( JBIT(MASK(II),JJ).NE.0 ) THEN
          COUNTER(1,I)=COUNTER(1,I)+1
          DO J=1,NBITS
            II=(J-1)/32+1
            JJ=MOD(J-1,32)+1
            IF( JBIT(MASK(II),JJ).NE.0 ) CORRELATE(I,J)=CORRELATE(I,J)+1
          ENDDO
        ENDIF
      ENDDO
C
      DO I=1,NBITS
        II=(I-1)/32+1
        JJ=MOD(I-1,32)+1
        IF( JBIT(MASK(II),JJ).NE.0 ) THEN
          COUNTER(2,I)=COUNTER(2,I)+1
        ELSE
          GOTO 999
        ENDIF
      ENDDO
C
  999 CONTINUE
C
C  Reset mask if counting cleared bits...
C
      IF( ITYPE.EQ.0 ) THEN
        DO I=1,NBITS/32+1
          IF( ITYPE.EQ.0 ) MASK(I)=NOT(MASK(I))
        ENDDO
      ENDIF
C
      RETURN
      END
