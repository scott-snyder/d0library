      SUBROUTINE GEECEL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets up EC EM section Geometry
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  13-SEP-1988   Rajendran Raja
C-   Updated  13-SEP-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL VOLPOS('EM_MOTHER_VOLUME+Z')        !EM Polycone Mother volume
      CALL STZDIV('EM1_DIVISIONS+Z','EC_EEM+') !Z divisions.
      CALL STZDIV('EM2_DIVISIONS+Z','EC_EEM+') !Z divisions.
      CALL STZDIV('EM3_DIVISIONS+Z','EC_EEM+') !Z divisions.
      CALL STZDIV('EM4_DIVISIONS+Z','EC_EEM+') !Z divisions.
      CALL VOLPOS('EM_SUPPORT_PLATE+Z')        !Support plate
      CALL VOLPOS('EM_SUPPORT_PIPE+Z')         !Support pipe
C
      CALL VOLORD('EM_MOTHER_VOLUME+Z',3)
C Ordering the contents of EEM+ in Z.
C
C Now for negative Z
C
      CALL VOLPOS('EM_MOTHER_VOLUME-Z')        !EM Polycone Mother volume
      CALL STZDIV('EM1_DIVISIONS-Z','EC_EEM-') !Z divisions.
      CALL STZDIV('EM2_DIVISIONS-Z','EC_EEM-') !Z divisions.
      CALL STZDIV('EM3_DIVISIONS-Z','EC_EEM-') !Z divisions.
      CALL STZDIV('EM4_DIVISIONS-Z','EC_EEM-') !Z divisions.
      CALL VOLPOS('EM_SUPPORT_PLATE-Z')        !Support plate
      CALL VOLPOS('EM_SUPPORT_PIPE-Z')         !Support pipe
C
      CALL VOLORD('EM_MOTHER_VOLUME-Z',3)
C Ordering the contents of EEM- in Z.
C
      RETURN
      END
