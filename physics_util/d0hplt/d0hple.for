      SUBROUTINE D0HPLE
C=====================================================================
C
C  Description:  This is the routine called at the end of processing
C  ============  of each event, if one is in the HISPAK menu.
C                Because HBOOK4 uses directories, and the default
C                directory can not changed from an interrupt menu
C                because it messes up the filling of histograms,
C                all D0HPLT commands must be handled by setting a
C                flag and executing the command at the end of processing
C                the current event, after changing the directory to the
C                one that the D0HPLT user is using.
C
C
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - July 1,1988
C         revised 28-JUN-1989   added calls to FLGSET (S. Protopopescu)
C
C=======================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
C
      CHARACTER*32 COMMAND
      CHARACTER*32 CURDIR
      INTEGER HNUM,I
      INTEGER DEVICE
      CHARACTER*5 TERM
      INTEGER IDALL(200)
      INTEGER NALL
      LOGICAL FIRST,LINT,INTAST
      LOGICAL FLGVAL
      DATA FIRST/.TRUE./
      DATA DEVICE/1/
      DATA HNUM/0/
C
C  Executable Code:
C  =================
C
C  Save the current directory to restore it when finished.
C  =========================================================
C
      CALL HCDIR(CURDIR,'R')
C
C  Change back to the HBOOK directory that the user thinks he's in...
C  ===================================================================
C
      CALL D0HUSD
C
C  Execute the user's command
C  ===============================
C
      COMMAND=' '
      IF (FLGVAL('D0HPID')) THEN
        COMMAND = 'PLOT'
        CALL FLGSET('D0HPID',.FALSE.)
      ELSE IF (FLGVAL('D0HUPH')) THEN
        COMMAND = 'UPLOT'
      ELSE IF (FLGVAL('D0HLAS')) THEN
        COMMAND = 'LPLOT'
        CALL FLGSET('D0HLAS',.FALSE.)
      ELSE IF (FLGVAL('D0HLGO')) THEN
        COMMAND = 'LEGO'
        CALL FLGSET('D0HLGO',.FALSE.)
      ELSE IF (FLGVAL('D0HEAD')) THEN
        COMMAND = 'HEAD'
        CALL FLGSET('D0HEAD',.FALSE.)
      ELSE IF (FLGVAL('D0HLIS')) THEN
        COMMAND = 'LIST'
        CALL FLGSET('D0HLIS',.FALSE.)
      ELSE IF (FLGVAL('D0HSHW')) THEN
        COMMAND = 'SHOW'
        CALL FLGSET('D0HSHW',.FALSE.)
      ELSE IF (FLGVAL('D0HINX')) THEN
        COMMAND = 'INDEX'
        CALL FLGSET('D0HINX',.FALSE.)
      ELSE IF (FLGVAL('D0HCHD')) THEN
        COMMAND = 'CHANGE DIRECTORY'
        CALL FLGSET('D0HCHD',.FALSE.)
      ELSE IF (FLGVAL('D0HTYP')) THEN
        COMMAND = 'TYPE'
        CALL FLGSET('D0HTYP',.FALSE.)
      ELSE IF (FLGVAL('D0HCLR')) THEN
        COMMAND = 'CLEAR'
        CALL FLGSET('D0HCLR',.FALSE.)
      ELSE IF (FLGVAL('D0HPRT')) THEN
        COMMAND = 'PRINT'
        CALL FLGSET('D0HPRT',.FALSE.)
      ELSE IF (FLGVAL('D0HSTR')) THEN
        COMMAND = 'STORE'
        CALL FLGSET('D0HSTR',.FALSE.)
      ELSE IF (FLGVAL('D0HBAK')) THEN
        COMMAND = 'BPLOT'
        CALL FLGSET('D0HBAK',.FALSE.)
      ELSE IF (FLGVAL('D0HSAM')) THEN
        COMMAND = 'SPLOT'
        CALL FLGSET('D0HSAM',.FALSE.)
      ELSE IF (FLGVAL('D0HNEX')) THEN
        COMMAND = 'NPLOT'
        CALL FLGSET('D0HNEX',.FALSE.)
      ELSE IF (FLGVAL('D0HZON')) THEN
        COMMAND = 'NZONE'
        CALL FLGSET('D0HZON',.FALSE.)
      ENDIF
      CALL D0HPLD(COMMAND)
      CALL HCDIR(CURDIR,' ')
C
      RETURN
      END
