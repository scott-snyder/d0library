C ------------------------------------------------------------------------
C             Data Logger Stream Definition Declarations
C             and DLCB (Data Logger Control Block) structure declaration
C ------------------------------------------------------------------------
       PARAMETER MAX_STREAMS = 16,               
     1           SETUP = 1,                      
     1           MANAGE = 2,                     
     1           CLOSE = 3,                      
     1           EXIT = 4                        ! Forced 'cleanup'
C
       STRUCTURE /STREAM_DEF/
                 INTEGER*4 UNIT_NO               
                 INTEGER*4 MASK                  
                 INTEGER*4 COUNTER               
                 LOGICAL*1 OPEN                  
                 LOGICAL*1 WRITTEN               
       END STRUCTURE
C
       STRUCTURE /DLCB/
                 INTEGER*4 OPTION                
                 INTEGER*4 RECORD_TYPE
                 INTEGER*4 TRIGGER_TYPE
                 INTEGER*4 EVENT_SIZE            
                 INTEGER*4 RUN_NO
                 INTEGER*4 LF_UNIT               
                 RECORD /FIBDEF1/ FIB            
                 RECORD /DQFDEF/ DQF             
                 INTEGER*2 DQF_CHAN              
                 RECORD /STREAM_DEF/ STREAMS(MAX_STREAMS)
       END STRUCTURE
C
       PARAMETER FIB1$K_Length = 28
C
C
