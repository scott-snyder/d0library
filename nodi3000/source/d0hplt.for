C DEC/CMS REPLACEMENT HISTORY, Element D0HPLT.FOR
C *9    21-AUG-1990 16:11:58 HAGOPIAN "FIX CALL TO D0HUPH.FOR """
C *8    21-AUG-1990 16:07:09 HAGOPIAN "DUMMY CALL TO D0HUPH"
C *7    15-AUG-1990 17:50:23 CSTEWART "Chip Stewart: added call to D0HUPH at stop updating"
C *6    21-MAY-1990 15:20:04 HAGOPIAN ""
C *5     1-MAR-1990 11:12:51 HAGOPIAN "ADD STOP UPDATING"
C *4    22-FEB-1990 15:02:34 HAGOPIAN "ADD UPDATING HIST"
C *3    16-JUN-1989 14:22:49 TAMI "Modified to work with HBOOK3 and HBOOK4"
C *2    11-JAN-1989 14:00:46 TAMI "CALL D0HINT EVERYTIME WE PLOT"
C *1    18-SEP-1988 16:39:36 TAMI "Menu dispatching routine for D0HPLT"
C DEC/CMS REPLACEMENT HISTORY, Element D0HPLT.FOR
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
      IF (FLGVAL('HBOOK4')) THEN
C
         IF (COMMAND(1:4) .EQ. 'PLOT') THEN
            CALL FLGSET('D0HPID',.TRUE.)
C
         ELSE IF (COMMAND(1:5) .EQ. 'UPLOT') THEN
            CALL FLGSET('D0HUPH',.TRUE.)
C
         ELSE IF (COMMAND(1:10) .EQ. 'STOP_UPLOT') THEN
C            CALL FLGSET('D0HUPH',.FALSE.)
            IFLAG = -1
C           CALL D0HUPH(IFLAG) 
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
      ELSE
         CALL D0HPLD(COMMAND)
      ENDIF
C
      RETURN
      END
