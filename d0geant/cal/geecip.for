      SUBROUTINE GEECIP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets Up EC Insert Plugs .
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  1-FEB-1986    Rajendran Raja
C-   Updated  14-SEP-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL VOLPOS('IFH_MOTHER_VOLUME+Z')       !Fine Hadronic module
      CALL STZDIV('IFH_DIVISIONS+Z','EC_EIF+')  !Z DIVISIONS.
      CALL VOLPOS('IFH_ENDPLATE+Z')
      CALL VOLPOS('IFH_SUPPORT_PIPE+Z')         !Support pipe
C
      CALL VOLORD('IFH_MOTHER_VOLUME+Z',3)
C Ordering the contents of EIF+ in Z.
C
      CALL VOLPOS('ICH_MOTHER_VOLUME+Z')
      CALL STZDIV('ICH_DIVISIONS+Z','EC_EIC+')  !Z DIVISIONS.
      CALL VOLPOS('ICH_ENDPLATE+Z')
      CALL VOLPOS('ICH_SUPPORT_PIPE+Z')         !Support pipe
C
      CALL VOLORD('ICH_MOTHER_VOLUME+Z',3)
C Ordering the contents of EIC+ in Z.
C
C
C Now for negative Z
C
      CALL VOLPOS('IFH_MOTHER_VOLUME-Z')       !Fine Hadronic module
      CALL STZDIV('IFH_DIVISIONS-Z','EC_EIF-')  !Z DIVISIONS.
      CALL VOLPOS('IFH_ENDPLATE-Z')
      CALL VOLPOS('IFH_SUPPORT_PIPE-Z')         !Support pipe
C
      CALL VOLORD('IFH_MOTHER_VOLUME-Z',3)
C Ordering the contents of EIF- in Z.
C
      CALL VOLPOS('ICH_MOTHER_VOLUME-Z')
      CALL STZDIV('ICH_DIVISIONS-Z','EC_EIC-')  !Z DIVISIONS.
      CALL VOLPOS('ICH_ENDPLATE-Z')
      CALL VOLPOS('ICH_SUPPORT_PIPE-Z')         !Support pipe
C
      CALL VOLORD('ICH_MOTHER_VOLUME-Z',3)
C Ordering the contents of EIC- in Z.
C
      RETURN
      END
