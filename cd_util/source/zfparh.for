      SUBROUTINE ZFPARH(PHIMIN,PHIMAX,THEMIN,THEMAX,PT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store ROAD=(PHIMIN,PHIMAX,THEMIN,THEMAX,PT)
C-   in PARH bank
C-
C-   Inputs  : ROAD 
C-
C-   Created  18-FEB-1991   Daria Zieminska 
C-   Updated   5-SEP-1991   Qizhong Li-Demarteau  clean up 
C-   Updated  11-MAR-1992   Qizhong Li-Demarteau  added check for the 
C-                                     old/illegal PARH bank to avoid 
C-                                     overwritting and crash
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPARH.LINK'
      INTEGER GZPARH,LPARH,LROAD
      REAL PHIMIN,PHIMAX,THEMIN,THEMAX,PT
C----------------------------------------------------------------------
      LPARH = GZPARH()
      IF (LPARH .LE. 0) CALL BKPARH(LPARH)
      IF (LPARH .LE. 0) GOTO 999
      IF (IQ(LPARH-1) .LT. 10) THEN
        CALL ERRMSG('ZTRAKS','ZFPARH','could not store road parameters
     & in an old/illegal PARH bank','W')
        GOTO 999
      ENDIF
C
C  Push the bank if needed
C
      IF (IQ(LPARH-1)-5*IQ(LPARH+10)-10.LT.5) THEN
        CALL MZPUSH(IXCOM,LPARH,0,5,' ')
      ENDIF
      LPARH = GZPARH()
      LROAD=LPARH+11+5*IQ(LPARH+10) 
      IQ(LPARH+10)=IQ(LPARH+10)+1
      Q(LROAD)=PHIMIN
      Q(LROAD+1)=PHIMAX
      Q(LROAD+2)=THEMIN 
      Q(LROAD+3)=THEMAX
      Q(LROAD+4)=PT
  999 RETURN
      END
