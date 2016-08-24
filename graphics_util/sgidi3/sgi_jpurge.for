C DEC/CMS REPLACEMENT HISTORY, Element JPURGE.FOR
C *1     3-JUN-1992 14:23:38 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JPURGE.FOR
      SUBROUTINE JPURGE(NAME)
C  DELETE A RETAINED SEGMENT
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      IF(NSEGS.EQ.0) RETURN
      DO 10 I=1,NSEGS
        IF(NAME.NE.NUMSEG(I))GO TO 10
        NS=I
        GO TO 20
   10 ENDDO
      RETURN
C  FOUND IT
   20 CONTINUE
      CALL J_DELSEGM(NAME)          ! DELETE FROM SEGMENT STORAGE
      CALL DELOBJ(OBJID(NS))        ! DELETE GRAPHICS OBJECT
      END