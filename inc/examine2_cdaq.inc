C----------------------------------------------------------------------
C-
C-   Created   3-AUG-1990   Boaz Klima
C-
C----------------------------------------------------------------------
      INTEGER MAXDEV
      PARAMETER( MAXDEV    = 500 )
      INTEGER MAXBUF
      PARAMETER( MAXBUF    = 10000 )
      INTEGER MAXINTBUF
      PARAMETER( MAXINTBUF = 1000 )
C
C ****  Internal Wastebasket into which CDAQ dumps the data
C ****  received from the front end.
C
      INTEGER DEVICE_BUFFER(MAXBUF)
      INTEGER TOTAL_ELEMENTS, ELEMENTS_PER_DEVICE(MAXDEV)
C
      REAL    TIMEOUT_FACTOR
      PARAMETER( TIMEOUT_FACTOR = 1.3 )
      REAL    PERIOD,TIMEOUT
      LOGICAL PERIODIC,CDAQ_INIT_RUN
      CHARACTER*32 DEVICES(MAXDEV),DEVICES_SUBSET(MAXDEV)
      INTEGER REQUEST_ID,NUMDEV,ITEM_ID(MAXDEV),NUMDEV_SUBSET
      INTEGER CYCLE_PERIOD(2),TIMEOUT_PERIOD(2),CDAQ_EVENT_FLAG
C
      COMMON /EXAMINE2_CDAQ/ REQUEST_ID,
     &                       DEVICE_BUFFER,
     &                       TOTAL_ELEMENTS,
     &                       ELEMENTS_PER_DEVICE,
C
     &                       TIMEOUT_PERIOD,CYCLE_PERIOD,
     &                       PERIODIC,PERIOD,TIMEOUT,CDAQ_EVENT_FLAG,
     &                       ITEM_ID,NUMDEV,DEVICES,
     &                       NUMDEV_SUBSET,DEVICES_SUBSET,
     &                       CDAQ_INIT_RUN
