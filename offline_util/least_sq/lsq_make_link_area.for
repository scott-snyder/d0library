      SUBROUTINE LSQ_MAKE_LINK_AREA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make ZLSQ into a link area.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Updated  21-FEB-1992   Rajendran Raja   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLSQ.INC'
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZLINK(IDVSTP,'/ZLSQ/',LSTLNK,LRFLNK,
     &    LRFLNK(LSQ_MAX))
      ENDIF
  999 RETURN
      END
