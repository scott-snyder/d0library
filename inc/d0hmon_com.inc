      INTEGER GET_ID,TIMER1_ID,TIMER2_ID,TIMER3_ID
      PARAMETER (GET_ID=32)
      PARAMETER (TIMER1_ID=1)
      PARAMETER (TIMER2_ID=2)
      PARAMETER (TIMER3_ID=3)
      LOGICAL*1 DISPOK
      INTEGER GET_TIME(2),TIMER1_TIME(2),TIMER2_TIME(2),TIMER3_TIME(2),
     *        ALARM_CHAN
      CHARACTER*80 DEVNAME,DEVADDR
      LOGICAL*1 GO_OK
      CHARACTER*511 DEVLIST
      CHARACTER*6 MICMON
      CHARACTER*256 ALARM_MSG
      CHARACTER*8 TIMER1_DEF,TIMER2_DEF,TIMER3_DEF
      INTEGER*2 ALARM_IOSB(4)
      COMMON /D0HMON_COM/ DISPOK,GET_TIME,TIMER1_TIME,TIMER2_TIME,
     *      TIMER3_TIME,DEVNAME,GO_OK,DEVLIST,MICMON,ALARM_CHAN,
     *      ALARM_MSG,TIMER1_DEF,TIMER2_DEF,TIMER3_DEF,DEVADDR,
     *      ALARM_IOSB
