C DEC/CMS REPLACEMENT HISTORY, Element JARSET.FOR
C *1     3-JUN-1992 13:08:00 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JARSET.FOR
      SUBROUTINE JARSET
C  RESET ALL CURRENT ATTRIBUTES TO DEFAULT VALUES
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
C  SIZE LIMITS
      CALL JPATH(IPTHDF)
      CALL JFONT(IFONDF)
      CALL JJUST(IHJUDF,IVJUDF)
      CALL JSIZE(XSIZED,YSIZED)
      CALL JGAP(GAPD)
      CALL JBASE(XBASND,YBASND,ZBASND)
      CALL JPLANE(XPLNND,YPLNND,ZPLNND)
      CALL JCOLOR(ICLRDF)
      CALL JLSTYL(ILSTDF)
      CALL JINTEN(IINTDF)
      CALL JLWIDE(ILWDDF)
      CALL JPEN(IPENDF)
      CALL JPEDGE(IPEDDF)
      CALL JPIDEX(IPCLDF,IPNTDF)
      CALL JPINTR(IPINDF)
      CALL JCMARK(IMRKDF)
      CALL JDVISB(IVISDF)
C  SET UP VIEWING VECTORS
C      IF(DI3DIN.GT.0) CALL J_PROJS
      END
