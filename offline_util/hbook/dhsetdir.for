      SUBROUTINE DHSETDIR(DIRECTORY,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the HBOOK directory or create it if it
C-   does not yet exist by calling DHDIR. If a file has been declared
C-   with a call to DHDIR_DECLARE_FILE then the specified directory 
C-   will be replicated within the declared file. 
C-
C-   Inputs  : DIRECTORY  [C*]  HBOOK directory (eg. //PAWC/BONG)
C-   Outputs : STATUS     [I]   0 - OK
C-   Controls: 
C-
C-   Created  17-JUN-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DIRECTORY
      INTEGER STATUS
C----------------------------------------------------------------------
      CALL DHDIR(' ',DIRECTORY(1:LEN(DIRECTORY)),STATUS,' ')
  999 RETURN
      END
