      SUBROUTINE D0HPLT(COMMAND)
C=====================================================================
C
C  Description:  Simple interactive histogram plotting package
C  ============
C                Main driving subroutine
C
C  Input:
C  COMMAND = instruction to D0H package
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - July 1,1988
C-   Updated  24-FEB-1990   Harrison B. Prosper
C-      Use switch D0HUPH for stopping updating
C-      histogram.
C
C=======================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
C
      CHARACTER*(*) COMMAND
      INTEGER HNUM,I,IFLAG
      INTEGER DEVICE
      CHARACTER*5 TERM
      INTEGER IDALL(200)
      INTEGER NALL
      LOGICAL FIRST,LINT,INTAST,FLGVAL
      DATA FIRST/.TRUE./
      DATA DEVICE/1/
      DATA HNUM/0/
C
C  Executable Code:
C  =================
C
      IF (COMMAND(1:4) .EQ. 'PLOT') THEN
        CALL FLGSET('D0HPID',.TRUE.)
C
      ELSE IF (COMMAND(1:4) .EQ. 'LEGO') THEN
        CALL FLGSET('D0HLGO',.TRUE.)
C
      ELSE IF (COMMAND(1:4) .EQ. 'LIST') THEN
        CALL FLGSET('D0HLIS',.TRUE.)
C
      ELSE IF (COMMAND(1:4) .EQ. 'HEAD') THEN
        CALL FLGSET('D0HEAD',.TRUE.)
C
      ELSE IF (COMMAND(1:5) .EQ. 'UPLOT') THEN
        CALL FLGSET('D0HUPH',.TRUE.)
C
      ELSE IF (COMMAND(1:10) .EQ. 'STOP_UPLOT') THEN
C            CALL FLGSET('D0HUPH',.FALSE.)
        IFLAG = -1
        CALL D0HUPH(IFLAG)
C
      ELSE IF (COMMAND(1:4) .EQ. 'SHOW') THEN
        CALL FLGSET('D0HSHW',.TRUE.)
C
      ELSE IF (COMMAND(1:5) .EQ. 'INDEX') THEN
        CALL FLGSET('D0HINX',.TRUE.)
C
      ELSE IF (COMMAND(1:16) .EQ. 'CHANGE DIRECTORY') THEN
        CALL FLGSET('D0HCHD',.TRUE.)
C
      ELSE IF (COMMAND(1:4) .EQ. 'TYPE') THEN
        CALL FLGSET('D0HTYP',.TRUE.)
C
      ELSE IF (COMMAND(1:5) .EQ. 'CLEAR') THEN
        CALL FLGSET('D0HCLR',.TRUE.)
C
      ELSE IF (COMMAND(1:5) .EQ. 'PRINT') THEN
        CALL FLGSET('D0HPRT',.TRUE.)
C
      ELSE IF (COMMAND(1:5) .EQ. 'STORE') THEN
        CALL FLGSET('D0HSTR',.TRUE.)
C
      ELSE IF (COMMAND(1:5) .EQ. 'BPLOT') THEN
        CALL FLGSET('D0HBAK',.TRUE.)
C
      ELSE IF (COMMAND(1:5) .EQ. 'SPLOT') THEN
        CALL FLGSET('D0HSAM',.TRUE.)
C
      ELSE IF (COMMAND(1:5) .EQ. 'NPLOT') THEN
        CALL FLGSET('D0HNEX',.TRUE.)
C
      ELSE IF (COMMAND(1:5) .EQ. 'LPLOT') THEN
        CALL FLGSET('D0HLAS',.TRUE.)
C
      ELSE IF (COMMAND(1:5) .EQ. 'NZONE') THEN
        CALL FLGSET('D0HZON',.TRUE.)
C
      ELSE IF (COMMAND(1:10) .EQ. 'END HISPAK') THEN
        FIRST = .TRUE.
C
      ELSE IF (COMMAND(1:7) .EQ. 'EXECUTE') THEN
C
        CALL D0HPLE
C
      ENDIF
C
      RETURN
      END
