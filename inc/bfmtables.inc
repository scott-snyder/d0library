C
C	Global Common Block Definitions for
C	the Data Acquisition Buffer Manager
C
      Integer*4         TABLE_SIZE
      Parameter        (TABLE_SIZE = 4096)
      Common/BFM_TABLES/BFM_TABLES(0:TABLE_SIZE-1)
      Integer*4         BFM_TABLES
C
