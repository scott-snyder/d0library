      SUBROUTINE HMATRIX_MAKE_LINK_AREA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make ZHMATRIX into a link area.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-JAN-1991   Harrison B. Prosper
C-      Code taken from HMATRIX_INI.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZLINK(IDVSTP,'/ZHMATRIX/',HSTLNK,HRFLNK,
     &    HRFLNK(HRF_LNKMX))
      ENDIF
  999 RETURN
      END
