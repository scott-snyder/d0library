      SUBROUTINE JVISBL(NM,IVISF)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      IF(NSEGS.EQ.0) RETURN
      IF(NSEGS.GT.100)RETURN
      ISG=0
      DO 10 I=1,NSEGS
        IF(NM.NE.NUMSEG(I)) GO TO 10
        ISG=I
        GO TO 20
   10 ENDDO
      RETURN
   20 IVIS(ISG)=IVISF
      END
