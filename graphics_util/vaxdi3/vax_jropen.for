      SUBROUTINE JROPEN(NAME)
C  OPEN A RETAINED SEGMENT
      INCLUDE 'D0$INC:DI3INC.INC'
      DATA NSMXX/100/
C
      IF(NSEGS.EQ.0) ISPTR=1
      IF(NSEGS.GE.NSMXX) GO TO 1000
      NSEGS=NSEGS+1
      ISEGNM=NAME
      IF(IDEBUG.GT.8)TYPE 888,NSEGS,NAME,RJ4
  888 FORMAT(' SEG',I2,I,/,'  WI:',4F,/,'  VP:',4F,/,'  VS:',4F)
      PUTS=.FALSE.
      NUMSEG(NSEGS)=NAME
      INDS1(NSEGS)=ISPTR
      NCALLS(NSEGS)=0
      ITTYP(NSEGS)=ITYPDF
      IDETEC(NSEGS)=IDETDF
C  SAVE THE WINDOWS FOR THIS SEGMENT
      DO 10 I=1,4
        DO 15 J=1,3
          RJ4SAV(I,J,NSEGS)=RJ4(I,J)
   15   CONTINUE
   10 CONTINUE
C  SET UP CONDITIONS FOR THIS SEGMENT
      CALL JARSET
C  STORE PARAMETERS AT BEGINNING OF STORAGE FOR THIS SEGMENT
      CALL JASAVE(IBLOC(ISPTR))
      ISPTR=ISPTR+23
C  TRIGGER THE RETAINED SEGMENT STORAGE
      PUTS=.TRUE.
      RETURN
 1000 WRITE(JUNIT,*)' JROPEN--TOO MANY SEGMENTS. (NOT OPENED).'
      END
