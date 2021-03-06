      SUBROUTINE JARSET
C  RESET ALL CURRENT ATTRIBUTES TO DEFAULT VALUES
      INCLUDE 'D0$INC:DI3INC.INC'
C
C  SIZE LIMITS
      NSMXX=100                          ! MAX # SEGMENTS
      NBLOCX=400000                     ! SIZE OF SEGMENT STORAGE
      NPMAX=2000                        ! MAX # POLYGON SIDES
      CALL JPATH(IPTHDF)
      CALL JFONT(IFONDF)
      CALL JJUST(IHJUDF,IVJUDF)
      CALL JSIZE(XSIZED,YSIZED)
      CALL JGAP(GAPD)
      CALL JBASE(XBASND,YBASDN,ZBASND)
      CALL JPLANE(XPLNND,YPLNND,YPLNND)
C      CALL JCESUB()
C      CALL JCESUP()
C      CALL JSCEFL()
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
      CALL J_VUVECTS
      END
