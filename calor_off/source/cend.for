      SUBROUTINE CEND
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Produce an analysis summary.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-JAN-1989   Harrison B. Prosper, John Womersley
C-   Updated  22-SEP-1989   Chip Stewart  HBP
C-   Updated  12-OCT-1989   Harrison B. Prosper
C-   Call CPREND from here. This is an entry point in CPRTST
C-   Updated  11-DEC-1989   Harrison B. Prosper
C-      Use new RCP string routine EZGETS
C-   Updated  14-SEP-1990   Harrison B. Prosper
C-      Print out histograms here (all directories)
C-   Updated  21-SEP-1990   K. Wyatt Merritt
C-      Add call to EVCLWO to close output streams
C-   Updated   9-DEC-1991   Harrison B. Prosper
C-      Add call to DO_HBOOK_CLOSE
C-   Updated  22-APR-1992   Harrison B. Prosper  
C-      Go back to old version for now 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL OK,CAL_END
      LOGICAL LHBOOK
      LOGICAL FLGVAL,WRITE_STREAMS
      INTEGER IER, LENGTH
      CHARACTER*80 FILNAM
C----------------------------------------------------------------------
C
      OK = CAL_END ()                   ! User analysis summary HOOK
C
C ****  Printout HBOOK histograms from ALL Directories
C
      CALL HLDIR('//PAWC','T')          ! List ALL Directories
      CALL HPDIR('//PAWC','T')          ! Print ALL Histograms
C
C ****  Close HBOOK RZ file
C
      CALL EZPICK('CALFRAME_RCP')
      CALL EZGET('HBOOK_SAVE',LHBOOK,IER)
C
      IF ( LHBOOK ) THEN
C        CALL DO_HBOOK_CLOSE

        CALL EZGETS('HBOOK_FILE',1,FILNAM,LENGTH,IER)
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('CALORIMETER','CEND',
     &    ' HBOOK_FILE name NOT FOUND','W')
        ELSE
          CALL HCDIR('//PAWC',' ')      ! This to get the directory
C                                       ! CALORIMETER put out. HRPUT
C                                       ! Does not put out current directory
C                                       ! by name. Calorimeter then maps
C                                       ! in PAW to //LUNxx where xx is the
C                                       ! Unit number. When CERN screws up
C                                       ! They do so royally.
C
C
          CALL HRPUT(0,FILNAM,'NT')     ! Store all for this dir+subdirs.
        ENDIF
      ENDIF
C
      CALL EZRSET
C
C ****  Close unit number opened by CPRTST
C
      CALL CPREND
C
C ****  Close event output streams
C
      WRITE_STREAMS = FLGVAL('WRITE_STREAMS')
      IF (WRITE_STREAMS) CALL EVCLWO('ALL')
C
  999 RETURN
      END
