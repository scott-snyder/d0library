      SUBROUTINE EMSV_LINK_INI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize EMSV_LINK link area
C-
C-   Inputs  : EMSV_LINK
C-   Outputs : Zebra knows about them
C-   Controls:
C-
C-   Created  30-SEP-1994   Lewis Taylor Goss
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:EMSV_LINK.INC'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
C...tell ZEBRA about EMSV permanent link area
C...assume both event links are all structural
        CALL MZLINK(IXCOM,'/EMSV_LINK/',L2EMSV(1),L2EMSV(EMCAND_MAX),
     &    L2EMSV(1))
      ENDIF
  999 RETURN
      END
