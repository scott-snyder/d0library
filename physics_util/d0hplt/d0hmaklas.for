C==================================================================
      SUBROUTINE D0HMAKLAS(IDNUM)
C==================================================================
C
C  Description:  Makes a copy of histogram IDNUM for your favorite
C  ============  printer.  On machines with more than one printer,
C                you must assign SYS$PRINT to be your favorite
C                printer queue.
C
C                Ex:  ASSIGN WH13W_PRINT SYS$PRINT
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - August 27,1988
C
C===================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      INTEGER IDNUM
      INTEGER DEVICE,LASERP,ICRT
      LOGICAL FLGVAL
      EXTERNAL FLGVAL
      LOGICAL LEXIST,HEXIST
      CHARACTER*5 TERM
      DATA ICRT/1/
      DATA DEVICE/1/
      DATA LASERP/2/
C
C  Executable Code:
C  ================
C
      LEXIST = HEXIST(IDNUM)
      IF (LEXIST) THEN
        CALL IGMETA(0,ICRT)
        CALL IGMETA(1,LASERP)
        CALL HPLOT(IDNUM,' ','HIST',0)
        CALL IGMETA(0,LASERP)
        CALL ICLWK(LASERP)
        CALL IGMETA(1,ICRT)
        CALL D0HCOM
      ELSE
        CALL INTMSG(' HISTOGRAM DOES NOT EXIST')
      ENDIF
      RETURN
      END
