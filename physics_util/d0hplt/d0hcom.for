      SUBROUTINE D0HCOM
C=======================================================================
C
C  Description:  This routine is designed to fix things up before 
C  ============  returning to COMPACK menu for the next D0HPLT 
C                command.
C
C  Author:
C  =======
C  Tami Kramer
C  
C  Revision History:
C  =================
C  Original Creation - August 24, 1988
C
C=======================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
C 
      INTEGER ESCAPE
      PARAMETER (ESCAPE=27)
      CHARACTER*4 DECSTR
      CHARACTER*19 HDSSTR
      CHARACTER*1 CODSTR
      CHARACTER*3 CDRV
C
C  Executable Code:
C  =================
C
C  Get the device driver name for the terminal device...
C  =======================================================
C  
      CALL D0HDRV(1,CDRV)
      IF (CDRV .EQ. '240') THEN
        CALL SETFLG
      ENDIF
      IF (CDRV .EQ. 'HDS') THEN
         CALL SETFLG
         HDSSTR(1:1) = CHAR(ESCAPE)
         HDSSTR(2:6) = '[?38l'
         WRITE(6,*) HDSSTR
         CALL INTMSG(' USE TEXT/GRAPH KEY TO SEE PLOT')
         CALL INTMSG(' SHIFT TEXT/GRAPH TO SWITCH BACK')
      ENDIF
      IF (CDRV .EQ. 'COD') THEN
         CODSTR(1:1) = CHAR(24)
         WRITE(6,*) CODSTR
         CALL INTMSG(' USE CTRL-F14 TO SEE PLOT ')
      ENDIF
      IF (CDRV .EQ. 'GPV') THEN
         CALL INTMSG(' HISTOGRAM PLOTTED')
      ENDIF
      RETURN
      END
