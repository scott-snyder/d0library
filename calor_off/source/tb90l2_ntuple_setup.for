      FUNCTION tb90l2_ntuple_SETUP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads in tb90l2_ntuple_RCP
C-   To be called from EXM_SETUP hook of examine2
C-
C-   Returned value  : TRUE if found RCP file
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  27-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'd0$params:tb90l2_ntuple.def'
      LOGICAL tb90l2_ntuple_setup
      LOGICAL ezerr
      INTEGER ier
C----------------------------------------------------------------------
      tb90l2_ntuple_setup = .true.
C
C ****  read in rcp file
C
      CALL inrcp('TB90L2_NTUPLE_RCP',ier)
      IF (ier.EQ.0) THEN
        CALL ezpick('TB90L2_NTUPLE_RCP')
        IF(ezerr(ier)) THEN
          CALL errmsg('INRCP-NO BANK','TB90L2_NTUPLE_SETUP',
     &    ' COULD NOT FIND TB90L2_NTUPLE_RCP BANK ','F')
          tb90l2_ntuple_setup = .false.
          RETURN
        ENDIF
        CALL ezrset
        CALL tb90l2_ntuple_init           ! setup ntuple variables
        CALL tb90l2_ntuple_cluster_init
      ELSE
        CALL errmsg('INRCP-NO RCP','TB90L2_NTUPLE_SETUP',
     &       'NO TB90L2_NTUPLE_RCP','W')
        CALL errmsg('INRCP','TB90L2_NTUPLE_SETUP',
     &    'CANT CONTINUE without TB90L2_NTUPLE_RCP','F')
        tb90l2_ntuple_setup = .false.
      END IF
      RETURN
      END
