C----------------------------------------------------------------------
C-
C-   Created   2-JUN-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      LOGICAL DEBUG, OK, TRANSFORM_DATA, CONTINUE_PAGE, PERIODIC,
     &  CHECK_LIMITS,LOG_DATA, SINGLE_CONTROL_MODE
      INTEGER ACTIVE_PAGEID, CURRENT_PAGEID, CURRENT_REQUESTID
      INTEGER NUMBER_PAGES, NUMBER_DEVBANKS
      INTEGER READING_REQ, SETTING_REQ(MAXSET),PAGEID(MAXPAGE)
      INTEGER NDEVICE_BUF,NATTRIBUTE_BUF
      INTEGER NREADING_BUF,NSETTING_BUF,NRECORD,NITEM
      INTEGER START_PAGE,END_PAGE,PAGE_ROWS
      INTEGER START_FIELD,END_FIELD,PAGE_FIELDS
      INTEGER START_ROW,END_ROW,NUMBER_ROWS
      INTEGER READ_ID(MAXDEV),READ_NN(MAXDEV),NREAD_ARRAY
      INTEGER SET_DEVICE(MAXDEV)
      INTEGER BAD_COLOR, ANALOG_COLOR, BINARY_COLOR, GROUP_COLOR,
     &        DEVICE_COLOR, FIELD_COLOR
      REAL    CYCLE, TIMEOUT_FACTOR, TIMEOUT
C
      CHARACTER*4  ATTRIBUTE_BUF(MAXATTBUF)
      CHARACTER*12
     &  PAGEBANK(MAXPAGE),DEVICE_BUF(MAXDEVBUF),
     &  READING_BUF(MAXREADBUF), SETTING_BUF(MAXSETBUF),
     &  READ_DEVICE(MAXDEV),READ_SUPPLY(MAXDEV)
      CHARACTER*32 DEVBANK(MAXTYP),READ_ARRAY(MAXDEV)
      CHARACTER*80 RECORD(MAXREC)
      RECORD / ITEM_TYPE /  ITEM(MAXITEM)
C----------------------------------------------------------------------
      COMMON /LVS_PAGES/ DEBUG, TRANSFORM_DATA, CONTINUE_PAGE,PERIODIC,
     &                   ACTIVE_PAGEID, CURRENT_PAGEID,
     &                   CURRENT_REQUESTID,
     &                   READING_REQ,SETTING_REQ,
     &                   NUMBER_PAGES, NUMBER_DEVBANKS,
     &                   PAGEID, ITEM, NDEVICE_BUF, NATTRIBUTE_BUF,
     &                   NREADING_BUF, NSETTING_BUF, NRECORD, NITEM,
     &                   START_PAGE,END_PAGE,PAGE_ROWS,
     &                   START_FIELD,END_FIELD,PAGE_FIELDS,
     &                   START_ROW,END_ROW,NUMBER_ROWS,
     &                   CYCLE, TIMEOUT_FACTOR, TIMEOUT,
     &                   PAGEBANK,DEVBANK,
     &                   DEVICE_BUF,READING_BUF,
     &                   ATTRIBUTE_BUF, SETTING_BUF,
     &                   RECORD,
     &                   NREAD_ARRAY,READ_ID,READ_NN,
     &                   READ_DEVICE,READ_SUPPLY,READ_ARRAY,
     &                   CHECK_LIMITS,LOG_DATA,SET_DEVICE,
     &                   BAD_COLOR, ANALOG_COLOR, BINARY_COLOR, 
     &                   GROUP_COLOR,DEVICE_COLOR, FIELD_COLOR,
     &                   SINGLE_CONTROL_MODE
