      SUBROUTINE UDST_TO_DST_FILL_BANK(LADDR,TYPE,INDEX,WORD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill WORD into bank at address LADDR with offset 
C-                         INDEX
C-
C-   Inputs  : LADDR,TYPE,INDEX,WORD
C-
C-   Created  18-NOV-1995   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LADDR,INDEX,BITMASK
      REAL    WORD(2),OUT1(2),OUT2(2)
      CHARACTER*1 TYPE
      IF (LADDR .EQ. 0) GOTO 999
      IF (IQ(LADDR-1).LT.INDEX) THEN
        CALL ERRMSG('bank size','UDST_TO_DST_FILL_BANK',
     &    'bank too small','W')
        GOTO 999
      ENDIF
      IF(TYPE .EQ. 'I')THEN
        IF(INDEX.EQ.0)THEN
C... only write into the 16 LSB of IQ(LBANK+0), others are reserved for ZEBRA
          CALL SPLIT_BITMASK(IQ(LADDR+INDEX),OUT1)
          BITMASK=WORD(1)
          CALL SPLIT_BITMASK(BITMASK,OUT2)
          OUT1(2)=OUT2(2)
          CALL REMAKE_BITMASK(BITMASK,OUT1)
          IQ(LADDR+INDEX) = BITMASK
        ELSE
          IQ(LADDR+INDEX) =WORD(1)
        ENDIF
      ELSEIF( TYPE .EQ. 'B' .AND. INDEX.GT.0)THEN
        CALL REMAKE_BITMASK(BITMASK,WORD)
        IQ(LADDR+INDEX)= BITMASK
      ELSEIF (TYPE .EQ. 'R' .AND. INDEX.GT.0) THEN
        Q(LADDR+INDEX) =WORD(1)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
