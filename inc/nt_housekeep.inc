C----------------------------------------------------------------------
C-
C-   Created  24-MAY-1991   B.S.Acharya
C-
C----------------------------------------------------------------------
      INTEGER MAXUNIT                   
      PARAMETER (MAXUNIT=50)            
      INTEGER IUSER                     
      INTEGER NFILES                    
      INTEGER LREC                      
      INTEGER IDLIST(MAXUNIT)           
      INTEGER UNIT_LIST(MAXUNIT)        
      INTEGER COUNTER(MAXUNIT)          
C                                       
      LOGICAL NEW_FILE(MAXUNIT)         
C
      CHARACTER*32 TOPDIR_LIST(MAXUNIT)
      CHARACTER*80 FILE_LIST(MAXUNIT)
C
      COMMON /NT_HOUSEKEEP/ IUSER,LREC,NFILES,
     &  COUNTER,NEW_FILE,UNIT_LIST,IDLIST
      COMMON /NT_STORE/TOPDIR_LIST,FILE_LIST
