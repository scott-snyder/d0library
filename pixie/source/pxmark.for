       SUBROUTINE PXMARK(ICOLR,IMARK,XMARK,YMARK,ZMARK)
C====================================================================
C
C  Description:  Marks the point XMARK,YMARK,ZMARK with DI-3000
C  ============  mark # IMARK
C                IMARK=1   .
C                IMARK=2   +
C                IMARK=3   *
C                IMARK=4   o
C                imark=5   x
C
C  Author:
C  ========
C  Tami Kramer
C
C  Conditions necessary before call:
C  =================================
C  Graphics initialized
C
C  Revision History:
C  =================
C  Original Creation - December 5,1986
C  Updated- 10-JAN-1990 Lupe Howell: ICOLOR was changed to a character 
C                                   type to implement color table.
C  Updated 7-JAN-1992  Include all 5 marks
C
C=====================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ===================
C
      CHARACTER*(*) ICOLR
      INTEGER IMARK
      REAL XMARK,YMARK,ZMARK
      REAL SCALE
      INTEGER DSPDEV
      REAL XDIFF,YDIFF,XSIZE,YSIZE
      REAL XMIN,XMAX,YMIN,YMAX
      CHARACTER*3 DRVNAM
      DATA DSPDEV/1/
      DATA XSIZE,YSIZE/.05,.05/
C
C  Executable Code:
C  ================
C
      CALL J4RGET(1,XMIN,XMAX,YMIN,YMAX)
      XDIFF=(XMAX-XMIN)
      YDIFF=(YMAX-YMIN)
      XSIZE=XDIFF*.01
      YSIZE=YDIFF*.01
      CALL PXCOLR(ICOLR)
      CALL D0HDRV(DSPDEV,DRVNAM)
      SCALE=.5
      IF(IMARK.EQ.5)THEN
        SCALE=2.
        IF(XDIFF.GT.1500)SCALE=1.
C MARK ARE BAD ON VAXSTATION
        IF(DRVNAM.EQ.'GPV'.OR.DRVNAM.EQ.'XDW'.OR.DRVNAM.EQ.'X11')THEN
          IF(XDIFF.GT.1500)THEN
            SCALE=4.
          ELSE
            SCALE=2. 
          ENDIF
        ENDIF
      ENDIF
      XSIZE=XSIZE/SCALE
      YSIZE=YSIZE/SCALE
      CALL JJUST(2,2)
      CALL JSIZE(XSIZE,YSIZE)
      CALL J3MOVE(XMARK,YMARK,ZMARK)
      IF(IMARK.EQ.1)THEN
        CALL J1STRG('.')
      ELSE IF(IMARK.EQ.2)THEN
        CALL J1STRG('+')
      ELSE IF(IMARK.EQ.3)THEN
        CALL J1STRG('*')
      ELSE IF(IMARK.EQ.4)THEN
        CALL J1STRG('O')
      ELSE
        CALL J3DRAW(XMARK+XSIZE,YMARK+YSIZE,ZMARK)
        CALL J3MOVE(XMARK,YMARK,ZMARK)
        CALL J3DRAW(XMARK-XSIZE,YMARK-YSIZE,ZMARK)
        CALL J3MOVE(XMARK,YMARK,ZMARK)
        CALL J3DRAW(XMARK+XSIZE,YMARK-YSIZE,ZMARK)
        CALL J3MOVE(XMARK,YMARK,ZMARK)
        CALL J3DRAW(XMARK-XSIZE,YMARK+YSIZE,ZMARK)
      ENDIF
C
      RETURN
      END
