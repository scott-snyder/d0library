
      SUBROUTINE L0_PACK_ADC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pack the level 0 time and charge in one ADC card
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   4-DEC-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ERR
      INTEGER BLOCK_TYPE, BLOCK_LENGTH
      INTEGER LAST_BUNCH, FIRST_BUNCH
      INTEGER LAST_CHAN, FIRST_CHAN
      INTEGER BLOCK_WORD, DESCRIPTOR
      INTEGER CHANNEL, HEAD
      REAL FACTOR, OFFSET(3,80)
      INTEGER DATA(80,2)
      INTEGER CHARGE(80), TIME(80)
      LOGICAL EZERROR
      LOGICAL FIRST
C
      INCLUDE 'D0$INC:LV0PARAM.INC'
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FACTOR=1.
        BLOCK_TYPE=1
        BLOCK_LENGTH= 82
        LAST_BUNCH=1
        FIRST_BUNCH=1
        LAST_CHAN=80
        FIRST_CHAN=1
        HEAD=5                          ! Saved for HEADER
        CALL VZERO (DATA,160)
C
C ****  Fetch offset values
C
        CALL INRCP('MCLEVEL0_RCP',ERR)
        CALL EZPICK('MCLEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('MCLEVEL0-no-rcp','L0_PACK_ADC',
     &                              'MCLEVEL0_RCP not found.','W')
        ELSE
          CALL EZGET('OFFSET',OFFSET,ERR)
        ENDIF
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
C
C ****  Correct INFO(CHANNEL,1) and INFO(CHANNEL,2)
C
      DO CHANNEL = 1 , 80
        DATA(CHANNEL,1)=INFO(CHANNEL,1)+OFFSET(3,CHANNEL) ! charge
        DATA(CHANNEL,2)=INFO(CHANNEL,2)*FACTOR+OFFSET(2,CHANNEL) ! time
      ENDDO
C
C ****  Fetch CORRECT_TIME
C
      DO CHANNEL = 1 , 80
        CHARGE(CHANNEL)=DATA(CHANNEL,1)
        TIME(CHANNEL)=DATA(CHANNEL,2)
      ENDDO
C      
      CALL VZERO(TCOR,80)
      CALL L0_ADC_EXPECT(FIRST_BUNCH,CHARGE,TIME)
C
      DO CHANNEL = 1 , 80
        IF ( DATA(CHANNEL,2).EQ.0 ) THEN
          DATA(CHANNEL,2)=1000
C        ELSE
C          TCOR(CHANNEL)=DATA(CHANNEL,2)
        ENDIF
      ENDDO
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
      CALL MVBITS(LAST_CHAN,0,8,DESCRIPTOR,8)
      CALL MVBITS(FIRST_CHAN,0,8,DESCRIPTOR,0)
      DATA_WORD(HEAD+2)=DESCRIPTOR
C
C ****  Pack DATA_WORD(I) FOR I.GE.3
C
      DO CHANNEL = 1 ,80
        CALL MVBITS(TCOR(CHANNEL),0,8,DATA_WORD(HEAD+CHANNEL+2),24)
        CALL MVBITS(DATA(CHANNEL,1),0,10,DATA_WORD(HEAD+CHANNEL+2),14)
        CALL MVBITS(0,0,3,DATA_WORD(HEAD+CHANNEL+2),10)
        CALL MVBITS(DATA(CHANNEL,2),0,10,DATA_WORD(HEAD+CHANNEL+2),0)
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
      END
