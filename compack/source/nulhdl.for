      FUNCTION NULHDL(SIGARG,MECARG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Error handler to output error  message from 
C-                         special routine, PUTMSG. Avoids traceback.
C-                         VAX-specific
C-
C-   Inputs  : SIGARG: Description of errors
C-             MECARG: Information about errors
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER*4 NULHDL,SIGARG(*),MECARG(5)
C&IF VAXVMS
      INCLUDE '($SSDEF)'
      EXTERNAL PUTMSG
      INTEGER ISTAT,SYS$PUTMSG
C----------------------------------------------------------------------
      IF(SIGARG(2).NE.SS$_ACCVIO) THEN     !May get further signalling
        ISTAT=SYS$PUTMSG(SIGARG,PUTMSG,)
        NULHDL=SS$_CONTINUE                !Want to continue running
C
C     May want to be much more selective as far as which errors are 'allowed'
C
      ELSE
        NULHDL=SS$_RESIGNAL                !CAN't continue running
      ENDIF
C&ELSE
C&      NULHDL=1
C&ENDIF
      RETURN
      END
