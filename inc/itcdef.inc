C----------------------
C Imported routines
C-------------------
                EXTERNAL ITC_Target_On,ITC_Target_Off,ITC_Find_Activity
                EXTERNAL ITC_Declare_Connect_Event,ITC_read
                EXTERNAL ITC_Declare_Receive_Event,ITC_Wait_Any
                EXTERNAL ITC_Declare_DisConn_Event
                EXTERNAL SYS$bintim,SYS$setimr,SYS$readef,SYS$cantim
                EXTERNAL LIB$get_ef,SYS$setef,SYS$clref
                EXTERNAL ITC_GETPID,ITC_GET_NET_NUMBER,ITC_GET_NODE
                EXTERNAL ITC_WRITE
C-----------------------
C Imported routines
C funtion result types
C-----------------------
                INTEGER*4 ITC_Target_On,ITC_Target_Off,ITC_Find_Activity
                INTEGER*4 ITC_Declare_Connect_Event,ITC_read
                INTEGER*4 ITC_Declare_Receive_Event,ITC_Wait_Any
                INTEGER*4 ITC_Declare_DisConn_Event
                INTEGER*4 SYS$bintim,SYS$setimr,SYS$readef,SYS$cantim
                INTEGER*4 LIB$get_ef,SYS$setef,SYS$clref
                INTEGER  ITC_GETPID,ITC_GET_NET_NUMBER,ITC_GET_NODE
                INTEGER ITC_WRITE
C-----------------------
C Parameters for argument number in ITC_WAIT
C Common block for EF flags
C-----------------------
                INTEGER    MXTIM
                PARAMETER (MXTIM=3)
                INTEGER    TIME_ID(3) /1,2,3/
                INTEGER    CONN_ID /4/
                INTEGER    MESS_ID /5/
                INTEGER    BROK_ID /6/
                INTEGER    EF_CONN,EF_BROK,EF_MESS,EF_TIME(3),RQ_TIME(3)
                INTEGER    QDTIME(2,3)
                COMMON /INTRUP/ EF_CONN,EF_BROK,EF_MESS,EF_TIME,
     &            RQ_TIME,QDTIME
