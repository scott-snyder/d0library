      FUNCTION l2em_ntuple_setup()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads in L2EM_NTUPLE_RCP
C-
C-   Returned value  : TRUE if RCP file is found
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  11-OCT-1992   James T. McKinley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'd0$params:l2em_ntuple.def'
      LOGICAL l2em_ntuple_setup
      LOGICAL ezerr
      INTEGER ier
C----------------------------------------------------------------------
      l2em_ntuple_setup = .TRUE.
C
C ****  read in rcp file
C
      CALL inrcp('L2EM_NTUPLE_RCP',ier)
      IF (ier.EQ.0) THEN
        CALL ezpick('L2EM_NTUPLE_RCP')
        IF(ezerr(ier)) THEN
          CALL errmsg('INRCP-NO BANK','L2EM_NTUPLE_SETUP',
     &    'Could not find L2EM_NTUPLE_RCP bank','F')
          l2em_ntuple_setup = .FALSE.
          RETURN
        ENDIF
        CALL ezrset
        CALL l2em_ntuple_init           ! setup ntuple variables
      ELSE
        CALL errmsg('INRCP-NO RCP','L2EM_NTUPLE_SETUP',
     &       'Cannot read L2EM_NTUPLE_RCP bank','F')
        l2em_ntuple_setup = .FALSE.
      END IF
C
      RETURN
      END
