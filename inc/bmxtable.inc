C
C	Common Block Definitions for the Buffer
C	Multiplexer. 
C
      Integer*4         BMX_TABLE_SIZE
      Parameter        (BMX_TABLE_SIZE = 511)
      Common/BMX_TABLES/BMX_TABLES(0:BMX_TABLE_SIZE)
      Integer*4         BMX_TABLES
C
      Parameter BMX_UNAMES_FP = 0	
      Parameter BMX_UNAMES_BP = 1	
      Parameter BMX_ACTMES_FP = 2	
      Parameter BMX_ACTMES_BP = 3	
C
