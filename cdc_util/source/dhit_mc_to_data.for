      SUBROUTINE DHIT_MC_TO_DATA(NWORDS,DHIT_WORDS,CONVERTED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : convert DHIT from MC format to data format
C-
C-   Inputs  : NWORDS: number of words per hit in DHIT bank (2 or 3)
C-             DHIT_WORDS: array of NWORDS words for this hit in DHIT format
C-                         for MC
C-   Outputs : DHIT_WORDS: array of NWORDS words for this hit in DHIT format
C-                         for data
C-             CONVERTED: 1 -- Yes; 0 -- Not
C-             (When a hit is not on a track the convertion is not done)
C-
C-   Created   6-APR-1995   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER NWORDS, DHIT_WORDS(3), WORDS(3)
      INTEGER LAYER, SECTOR, WIRE, JHIT, ISIDE, ONTRK, IZTRK
      INTEGER LDTMW, IPTM, NEW_SECTOR, HTDATA, TEMP
      INTEGER*2 HTINFO(2)
      EQUIVALENCE (HTINFO(1),HTDATA)
      REAL    TIME, ZPOS, AREA, VELO, TTIME
      LOGICAL CONVERTED
C----------------------------------------------------------------------
C
      CONVERTED = .FALSE.
      CALL UCOPY(DHIT_WORDS,WORDS,NWORDS)
      CALL DHIT_UNPACK(WORDS,LAYER,SECTOR,WIRE,JHIT,ONTRK,
     &                 ISIDE,IZTRK,TIME,ZPOS,AREA,NWORDS)
      IF (.NOT.ONTRK) GOTO 999
      IF (LDGEH .LE. 0 .OR. LDTMH .LE. 0) THEN
        CALL ERRMSG('DHIT_MC_TO_DATA','DHIT_MC_TO_DATA',
     &              'no STP banks, could NOT convert!','W')
        GOTO 999
      ENDIF
      LDTMW = LC(LDTMH -(LAYER+1))
      IPTM = LDTMW + (SECTOR*IC(LDTMW+4)+WIRE)*IC(LDTMW+3) + 4
      VELO = C(IPTM + 2)
      IF (VELO .EQ. 0) VELO = 0.004    ! avoid deviding by zero
      TTIME = (C(LC(LDGEH-3)+11+(LAYER*2)) + C(LC(LDGEH-3)+19+WIRE))
     &        * C(LC(LDGEH-3)+9) * PI / (VELO * 180)
      HTDATA = DHIT_WORDS(2)
      TEMP = NINT(TTIME) * 10 - HTINFO(WORD1)
      IF (TEMP .GT. 32768) THEN
        TEMP = 32767
      ENDIF
      HTINFO(WORD1) = TEMP
      DHIT_WORDS(2) = HTDATA
      IF (BTEST(WORDS(1),0)) THEN
        DHIT_WORDS(1) = IBCLR(DHIT_WORDS(1),0)
      ELSE
        DHIT_WORDS(1) = IBSET(DHIT_WORDS(1),0)
        NEW_SECTOR = SECTOR - 1
        IF (NEW_SECTOR .LE. 0) NEW_SECTOR = NEW_SECTOR + 32
        CALL MVBITS(NEW_SECTOR,0,5,DHIT_WORDS(1),11)
      ENDIF
      CONVERTED = .TRUE.
C
  999 RETURN
      END
