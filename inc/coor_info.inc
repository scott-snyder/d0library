C
C        The following is a common containing data relavent to COOR.
C
C
C           NODE         :  Name of node on which DAQ is running
C           DEFINED      :  Logical, true if task defined a trigger
C           GOOD_TAB     :  Validity flag for following 5 items
C           NUM_BIT      :  Number of valid entries in each of next 4 items
C           BIT_LIST     :  List of booked trigger bits
C           NAME_LIST    :  List of names of booked triggers
C           PID_LIST     :  List of process ID of trigger owners
C           REQUESTED    :  List of booleans telling which bits requested
C           TASK_NAME    :  Name of this task
C           CONNECT_MADE :  Boolean False if connection has not been made
C           CONFIG_TYPE  :  Name of detector of interest to this task
C
      COMMON /COOR_INFO/NODE,DEFINED,GOOD_TAB,NUM_BIT,
     1 BIT_LIST,NAME_LIST,PID_LIST,REQUESTED,TASK_NAME,CONNECT_MADE,
     2 CONFIG_TYPE
      LOGICAL*1 CONNECT_MADE,DEFINED,GOOD_TAB,REQUESTED(32)
      INTEGER NUM_BIT,BIT_LIST(32),PID_LIST(32)
      CHARACTER*32 NAME_LIST(32)
      CHARACTER*6 NODE
      CHARACTER*10 TASK_NAME
      CHARACTER*3 CONFIG_TYPE
C
