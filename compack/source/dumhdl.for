      FUNCTION DUMHDL(SIGARG,MECARG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy error handler to just resignal error
C-                         VAX specific
C-
C-   Inputs  : SIGARG: Signaled error arguments from system
C-             MECARG: Auxillary information about error
C-   Outputs : None
C-   Controls: None
C-
C-   Documented  22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DUMHDL
      INTEGER*4 SIGARG(*),MECARG(5)
C&IF VAXVMS
      INCLUDE '($SSDEF)'
C----------------------------------------------------------------------
      DUMHDL=SS$_RESIGNAL                !Let system handle errors 
C&ELSE
C&      DUMHDL=1
C&ENDIF
      RETURN
      END
