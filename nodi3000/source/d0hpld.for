C=====================================================================
      SUBROUTINE D0HPLD(COMMAND)
C=====================================================================
C
C  Description:  Simple interactive histogram plotting package
C  ============
C                Dispatching routine
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
C  Original Creation - July 1,1988 used to be subroutine D0HPLT
C
C=======================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
C 
      CHARACTER*(*) COMMAND
      INTEGER HNUM,I
      INTEGER DEVICE
      CHARACTER*5 TERM
      INTEGER IDALL(200)
      INTEGER NALL
      LOGICAL FIRST,LINT,INTAST
      DATA FIRST/.TRUE./
      DATA DEVICE/1/
      DATA HNUM/0/
C
C  Executable Code:
C  =================
C
      IF (COMMAND(1:4) .EQ. 'PLOT') THEN
         CALL INTMSG(' PLOTTING DUMMIED IN THIS VERSION')
      ELSE IF (COMMAND(1:10) .EQ. 'STOP_UPLOT') THEN
         CALL INTMSG(' PLOTTING DUMMIED IN THIS VERSION')
      ELSE IF (COMMAND(1:5) .EQ. 'UPLOT') THEN
         CALL INTMSG(' PLOTTING DUMMIED IN THIS VERSION')
      ELSE IF (COMMAND(1:5) .EQ. 'NZONE') THEN
         CALL INTMSG(' PLOTTING DUMMIED IN THIS VERSION')

C
      ELSE IF (COMMAND(1:4) .EQ. 'SHOW') THEN
         CALL GETPAR(1,' HISTOGRAM NUMBER ?  >','I',HNUM)
         CALL D0HSHW(HNUM)
C
      ELSE IF (COMMAND(1:5) .EQ. 'INDEX') THEN
         CALL D0HINX
C
      ELSE IF (COMMAND(1:16) .EQ. 'CHANGE DIRECTORY') THEN
         CALL D0HCHD
C
      ELSE IF (COMMAND(1:4) .EQ. 'TYPE') THEN
         CALL GETPAR(1,' HISTOGRAM NUMBER ? 0 FOR ALL >','I',HNUM)
         CALL D0HTYP(HNUM)
C
      ELSE IF (COMMAND(1:5) .EQ. 'CLEAR') THEN
         CALL GETPAR(1,' HISTOGRAM NUMBER ? 0 FOR ALL >','I',HNUM)
         CALL D0HCLR(HNUM)
C
      ELSE IF (COMMAND(1:5) .EQ. 'PRINT') THEN
         CALL GETPAR(1,' HISTOGRAM NUMBER ? 0 FOR ALL >','I',HNUM)
         CALL D0HPRT(HNUM)
C
      ELSE IF (COMMAND(1:5) .EQ. 'STORE') THEN
         CALL GETPAR(1,' HISTOGRAM NUMBER ? 0 FOR ALL >','I',HNUM)
         CALL D0HSTR(HNUM)
C
      ELSE IF (COMMAND(1:5) .EQ. 'BPLOT') THEN
         CALL INTMSG(' PLOTTING DUMMIED IN THIS VERSION')
C
      ELSE IF (COMMAND(1:5) .EQ. 'SPLOT') THEN
         CALL INTMSG(' PLOTTING DUMMIED IN THIS VERSION')
C
      ELSE IF (COMMAND(1:5) .EQ. 'NPLOT') THEN
         CALL INTMSG(' PLOTTING DUMMIED IN THIS VERSION')
C
      ELSE IF (COMMAND(1:5) .EQ. 'LPLOT') THEN
         CALL INTMSG(' PLOTTING DUMMIED IN THIS VERSION')
C
      ELSE IF (COMMAND(1:10) .EQ. 'END HISPAK') THEN
         FIRST = .TRUE.
      ENDIF
C
      RETURN
      END
