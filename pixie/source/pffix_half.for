      SUBROUTINE PFFIX_HALF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Temporary kludge to fix bug in FTRH 
C-      track bank (to fill in ftrh+5 and +6)
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  25-JAN-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZFDCT.LINK'
C
      INTEGER LFTRH,GZFTRH
      INTEGER LFDCT
      INTEGER HALF,NUMTRK(0:1)
      INTEGER RUN,ID
      INTEGER RUNSAV,IDSAV
C----------------------------------------------------------------------
C
C  Once per event.
C
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = ID
      ELSE
        GOTO 999
      ENDIF
C
      LFTRH=GZFTRH()
      IF(LFTRH.LE.5) THEN
        GOTO 999
      ENDIF
      IQ(LFTRH+5)=0
      IQ(LFTRH+6)=0
C
      LFDCT=LQ(LFTRH-IZFDCT)
      IF(LFDCT.LE.5) THEN
        GOTO 999
      ENDIF
C
      NUMTRK(0)=0
      NUMTRK(1)=0
      DO  WHILE (LFDCT.GT.5)
        HALF=IAND(1,IQ(LFDCT+1))
        NUMTRK(HALF)=NUMTRK(HALF)+1
        LFDCT=LQ(LFDCT)
      ENDDO
      IQ(LFTRH+5)=NUMTRK(0)
      IQ(LFTRH+6)=NUMTRK(1)
C
  999 RETURN
      END
