      LOGICAL FUNCTION TOP_LEPTONS_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialization hook for top
C-                             dilepton analysis
C-
C-   Inputs  : None
C-   Outputs : Logical .TOP_LEPTONS_INIT.
C-   Controls: Calls Initialization steering routine TOP_LEPTONS_READ_CUTS
C-             for top->dileptons package
C-
C-   Created  10-JUL-1992   Stephen J. Wimpenny
C-   Modified 13-JUL-1992   Jim Cochran         -  added NTUPLE booking
C-   Modified 17-Aug-1992   Fix to allow Hist and Ntuples together JC
C-   Updated  14-SEP-1992   Meenakshi Narain  clean up RCP variable passing
C-                           mechanism
C-   Updated  20-SEP-1992   Meenakshi Narain  remove ntuple book call
C-   Updated  24-Sep-1992   rest of NTUPLE handling removed
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL DO_HISTOGRAMS
      INTEGER IER
      CHARACTER*80 MSG
C
      DATA MSG / ' TOP_LEPTONS V6.01 10-May-1994 ' /
C
C *** Set logical flag to show that init routine has been called
C
      TOP_LEPTONS_INIT=.TRUE.
C
C *** Read RCP file
C
      CALL INRCP('TOP_LEPTONS_RCP',IER)
      IF(IER.NE.0) THEN
        CALL ERRMSG('No RCP file -> default cuts used',
     1     'TOP_LEPTONS_INIT',' ','W')
        DO_HISTOGRAMS=.FALSE.
      ELSE
C
        CALL INTMSG(MSG)
C
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET('PLOT_HISTOGRAMS',DO_HISTOGRAMS,IER)
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_INIT',' ','F')
        CALL EZRSET
      ENDIF
C
C *** Book Histograms
C
      IF(DO_HISTOGRAMS) THEN
        CALL TOP_LEPTONS_HIST_BOOK
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
