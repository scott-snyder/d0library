C
C The resource table is used to keep track of all available resources
C
C Node definition
C
C       node_name       :complete name of the node
C       node_type       :the system (platform) type of the node
C       node_jobs       :the number of jobs currently running on this node
C       node_job_limit  :maximum number of jobs allowed on this node
C       disk_ref        :an array of strings  which reference the ID
C                        for each disk available to this node
C       drive_ref        :an array of strings which reference the ID
C                        for each tape drive available to this host
C
C Disk definition
C
C       disk_id         :the identifier of the disk referred to in the node
C                        definition
C       disk_host       :the name of the host node
C       disk_size       :total disk area on this device
C       disk_allocated  :disk area on this device allocated by current jobs
C       disk_name       :name of this disk or disk area
C       disk_name_local :local name of this disk or disk area
C     disk_spooled_file :location and file name of files which have been
C                        spooled onto the disk
C
C
C Tape drive definition
C
C       drive_id         :the identifier of the disk referred to in the node
C                        definition
C       drive_host       :the name of the host node
C       drive_media      :the drive media, (8mm,9tr)
C       drive_status     :disk area on this device allocated by current jobs
C       drive_avsp       :a switch indicating whether the drive is AVlaible
C                         to tape SPooling jobs.
C       drive_name       :local name of this device
C
C Control switches and variables
C
C       MONITOR_LOG      : .TRUE./.FALSE. Provide extensive log information
C                         in monitior log file.  (.TRUE.)
C       MANAGER_LOG      : .TRUE./.FALSE. Provide extensive log information
C                         in job_manager log file. (.TRUE.)  
C       MONITOR_WAIT_TIME:Time in seconds to wait before monitor cycling.(60
C                          sec)
C       MANAGER_WAIT_TIME:Time in seconds to wait before manager cycling.
C                          (15 sec)
C       LOCAL_USER       :User name of the local ProMan account. This is 
C			  required for remote processes to communicate
C			  back to the PM.
C


      INTEGER max_node,max_disk,max_drive,max_spooled_file,max_ref
      PARAMETER ( max_node        =20)
      PARAMETER ( max_disk        =40)
      PARAMETER ( max_drive       =40)
      PARAMETER ( max_spooled_file=10)
      PARAMETER ( max_ref         =10)
C
C
C
      INTEGER ifrt,ilrt 
      CHARACTER*20 node_name(max_node),node_type(max_node),
     &  disk_ref(max_node,max_ref),drive_ref(max_node,max_ref)
      INTEGER node_jobs(max_node),node_job_limit(max_node)
      INTEGER MONITOR_WAIT_TIME,MANAGER_WAIT_TIME
      LOGICAL MONITOR_LOG,MANAGER_LOG
C
C entries in disk_id refer to this table, it has information about disk
C resources
C
      CHARACTER*40 disk_id(max_disk),disk_host(max_disk),
     &  disk_name(max_disk),disk_name_local(max_disk),
     &  disk_spooled_file(max_disk,max_spooled_file)
      INTEGER disk_size(max_disk),disk_allocated(max_disk)
C
C tape_id points to this table, it has information about tape drive resources
C
      CHARACTER*40 drive_id(max_drive),drive_host(max_drive),
     &  drive_type(max_drive),drive_avsp(max_drive),
     &  drive_name(max_drive),
     &  drive_status(max_drive)
C
C Local user
C
      CHARACTER*40 LOCAL_USER
C
C common block definition
C
      COMMON/resource_table/ifrt,
     &  node_name,node_type,
     &  node_jobs,node_job_limit,
     &  disk_ref,drive_ref,
     &  disk_id,disk_host,
     &  disk_size,disk_name,disk_name_local,
     &  disk_allocated,disk_spooled_file,
     &  drive_id,drive_host,
     &  drive_type,drive_avsp,drive_name,drive_status,
     &  MONITOR_WAIT_TIME,MANAGER_WAIT_TIME,
     &  MONITOR_LOG,MANAGER_LOG,LOCAL_USER,
     &  ilrt(128)
CDEC$ PSECT /RESOURCE_TABLE/ ALIGN=9
