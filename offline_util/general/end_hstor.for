      FUNCTION END_HSTOR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store histograms automatically in Examine
C-                         upon END PROCESSING
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-JUL-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL END_HSTOR
C----------------------------------------------------------------------
      END_HSTOR = .TRUE.
      CALL ERRMSG('END_HSTOR','END_HSTOR',
     &  'STORING ALL HISTOGRAMS AWAY BY DEFAULT','S')
      CALL D0HSTR(0)                    ! store all away
  999 RETURN
      END
