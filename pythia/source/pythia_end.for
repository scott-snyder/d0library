      SUBROUTINE PYTHIA_END
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-FEB-1991   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LHBOOK,PHBOOK
      CHARACTER*80 FILNAM
      INTEGER IER,LEN
C----------------------------------------------------------------------
C
      CALL EZPICK('PYTHIA_RCP')
      CALL EZGET('HBOOK_SAVE',LHBOOK,IER)      
      CALL EZGET('HBOOK_PRINT',PHBOOK,IER)      
      CALL EZRSET
C
      CALL PYTFFL
C
      IF(PHBOOK) CALL HISTDO                       ! Print histograms
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
C----------------------------------------------------------------------
  999 RETURN
      END
