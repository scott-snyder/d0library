      SUBROUTINE SHLEND
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  End shower library writing job:
C-                          Close RZ file, print histograms
C-
C-   Created  21-FEB-1989   John Womersley
C-   Updated   1-FEB-1990   Rajendran Raja  NO PURGING ANYMORE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SHLCON.INC'
C----------------------------------------------------------------------
      IF (ISUNIT.NE.0)THEN
        CALL ERRMSG('SHOWERLIBRARY','SHLEND',
     &    'DOING SHSTAT AND SHLIB_CLOSE','W')
        CALL SHSTAT                     ! DO AT END OF PROCESS
        CALL SHLIB_CLOSE(ISUNIT,'SHOWER_LIBRARY')   ! CLOSE LIBRARY
      ENDIF
  999 RETURN
      END
