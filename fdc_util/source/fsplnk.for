      SUBROUTINE FSPLNK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a permanent link area for the FDC STP
C-                         bank links
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  19-MAY-1989   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER ICALL
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF(ICALL .LE. 0) THEN
        ICALL = 1
        IF ( LSTPH .LE. 0 ) CALL INZSTP
        CALL MZLINK(IXSTP,'/FDCPRM/',FDCPRM,LFPSE(1,1,7,35),FDCPRM)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
