      LOGICAL FUNCTION ISARCP_END()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     user hook for ISAJET end-of-run
C-
C-   Updated  11-NOV-1989   Rajendran Raja  (BASED ON ISGEND)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LHBOOK
      CHARACTER*80 FILNAM,SEED_FILE
      INTEGER IER,GTUNIT_ID,SEED_UNIT,LEN
      COMMON/SEED/XSEED 
      CHARACTER*24 XSEED    
C----------------------------------------------------------------------
C
      CALL ISAEND
      CALL ISAFFL
      CALL HISTDO                       ! Print histograms
C
      CALL EZPICK('ISARCP_RCP')
      CALL EZGET('HBOOK_SAVE',LHBOOK,IER)      
C
C ****  final random number seed
C
      CALL EZGET('GTUNIT_ID',GTUNIT_ID,IER)
      CALL EZ_FILE_OPEN(GTUNIT_ID,'RANDOM_SEED_SAVE_FILE','OF',
     &  SEED_UNIT,SEED_FILE,IER)
      WRITE(SEED_UNIT,302) XSEED    
302   FORMAT(//' FINAL RANDOM NUMBER SEED = ',A24)  
C
C     dialog input in ISAJET
C        READ*, SEED 
C        WRITE(ITCOM,*) SEED 
C        CALL RANFST(SEED)   
C
      IF(LHBOOK)THEN
        CALL EZGETS('HBOOK_SAVE_FILE',1,FILNAM,LEN,IER)
        CALL HCDIR('//PAWC',' ')          ! This to get the directory
C                                       ! CALORIMETER put out. HRPUT
C                                       ! Does not put out current directory
C                                       ! by name. Calorimeter then maps
C                                       ! in PAW to //LUNxx where xx is the
C                                       ! Unit number. When CERN screws up
C                                       ! They do so royally.
C
C
        CALL HRPUT(0,FILNAM,'NT')         ! Store all for this dir+subdirs.
      END IF

      ISARCP_END=.TRUE.
  999 RETURN
      END
