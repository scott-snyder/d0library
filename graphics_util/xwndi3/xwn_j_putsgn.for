      SUBROUTINE J_PUTSGN(IDSUBR,N,VAL)
      DIMENSION VAL(*)
C  PUT DISPLAY LIST STUFF INTO SEGMENT BLOC STORAGE
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      DATA IDCUR/0/
      IF(IFMODE.EQ.1)RETURN
      IF(IABS(IDSUBR).EQ.IDCUR) GO TO 10
      IBLOC(ISPTR)=IABS(IDSUBR)
      ISPTR=ISPTR+1
      IDCUR=IDSUBR
      IF(ISPTR.EQ.NBLOCX) GO TO 1000
   10 DO 20 I=1,N
C        IF(IDEBUG.GE.5) TYPE 999,I,VAL(I)
C  999   FORMAT('                        ',I3,F)
        SBLOC(ISPTR)=VAL(I)
        ISPTR=ISPTR+1
        IF(ISPTR.EQ.NBLOCX) GO TO 1000
   20 CONTINUE
      IF(IDSUBR.GE.0) RETURN
      IDCUR=0
      NCALLS(NSEGS)=NCALLS(NSEGS)+1
      RETURN
 1000 WRITE(JUNIT,1001)ISPTR,NBLOCX
 1001 FORMAT(' PUTSGN--SEG STOR OFLO - ISPTR,NBLOCX:',2I)
C  BE DRASTIC FOR NOW
      CALL JRCLOS
      NSEGS=0
      ISPTR=1
      RETURN
      END