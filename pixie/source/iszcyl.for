      SUBROUTINE ISZCYL(XC,YC,ZC,INRAD,OUTRAD,ZWID)
C=======================================================================
C
C  Description:  Draws a cylinder with inner radius INRAD and outer
C  ============  radius OUTRAD and length ZWID.
C
C  Author:
C  ==========
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - June 23, 1988
C
C========================================================================
C
      IMPLICIT NONE
C
C  Local Declarations:
C  ====================
C
      REAL XC,YC,ZC,INRAD,OUTRAD,ZWID
      REAL XPOS,YPOS,ZBPOS,ZEPOS
C
C  Executable Code:
C  ==================
C      
      CALL JCIRCL(XC,YC,ZC-ZWID,INRAD,32)
      CALL JCIRCL(XC,YC,ZC-ZWID,OUTRAD,32)
      CALL JCIRCL(XC,YC,ZC+ZWID,INRAD,32)
      CALL JCIRCL(XC,YC,ZC+ZWID,OUTRAD,32)
      XPOS = XC
      YPOS = YC+OUTRAD
      ZBPOS = ZC - ZWID
      ZEPOS = ZC + ZWID
      CALL J3MOVE(XPOS,YPOS,ZBPOS)
      CALL J3DRAW(XPOS,YPOS,ZEPOS)
      CALL J3MOVE(XPOS,-YPOS,ZBPOS)
      CALL J3DRAW(XPOS,-YPOS,ZEPOS)
      YPOS = YC + INRAD
      CALL J3MOVE(XPOS,YPOS,ZBPOS)
      CALL J3DRAW(XPOS,YPOS,ZEPOS)
      CALL J3MOVE(XPOS,-YPOS,ZBPOS)
      CALL J3DRAW(XPOS,-YPOS,ZEPOS)
      RETURN
      END
