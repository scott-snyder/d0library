      SUBROUTINE DTRKSG(NUSEGM,LDTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to fill segment number in DTRK bank.
C-                         Due to the limit of the available bits in the
C-                         status word, if the segment # is greater than
C-                         127, the # 127 is stored in DTRK status word.
C-
C-   Inputs  : NUSEGM(0:3) [I] : Used segment numbers
C-             LDTRK: DTRK bank address
C-   Outputs : none
C-
C-   Created  21-OCT-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NUSEGM(0:3), LDTRK
      INTEGER LAY, LAYBIT, NUMSEG
      INTEGER STABIT, NBIT, MAXSEG
      PARAMETER( STABIT = 4 )
      PARAMETER( NBIT = 7 )
      PARAMETER( MAXSEG = 127 )
C----------------------------------------------------------------------
C
      DO 100 LAY = 0, 3
        IF (NUSEGM(LAY) .GT. 0) THEN
          LAYBIT = STABIT + LAY * NBIT
          NUMSEG = MIN(NUSEGM(LAY),MAXSEG)
          CALL MVBITS(NUMSEG,0,NBIT,IQ(LDTRK+1),LAYBIT)
        ENDIF
  100 CONTINUE
C
  999 RETURN
      END
