      SUBROUTINE PMVERN(IVIEW,ITRAK,NPTRAK)
C======================================================================
C-
C-   Purpose and Methods : To plot the Vernier pad solutions and
C-                         circle the one used
C-
C-   Inputs  : IVIEW - tell which view (see PMEVNT)
C-
C-   Created  26-MAR-1990   Carol C. Francis
C-   DH 4/90 add views 7-12
C======================================================================
      IMPLICIT NONE
C======================================================================
C
C    Variable Declarations
C    =====================
      INTEGER K,L
      INTEGER PXMUTK
      INTEGER NPTRAK,ITRAK                    ! Number of points on track
      INTEGER IWADD,IHMUOH,ITSIGN,IDELT,IPAD  ! GTMHTT
      INTEGER IFW1,IFW2,INRAW,IORIEN,NHWIR    !GTMUOH
      INTEGER IVIEW
      REAL CORT1,CORT2,CORP1,CORP2,CORDT1,CORDT2    !GTMUOH
      REAL DDIS1,DDIS2,TDIV1,TDIV2,VERD1,VERD2    !GTMUOH
      REAL XCWIR,YCWIR,ZCWIR            ! GTMUOH
      REAL XHTRAK,YHTRAK,ZHTRAK,IHWADD  ! GTMHOT
      REAL VSOLU1,VSOLU2,S                ! Vernier solutions
C
C    Executable Code
C    ===============
C
C    Get information about the track
C    ===============================
      DO 100 L = 1,NPTRAK
        CALL GTMHTT(ITRAK,L,IWADD,IHMUOH,ITSIGN,IDELT,IPAD)
        CALL GTMUOH(IHMUOH,IWADD,IFW1,IFW2,INRAW,IORIEN,
     X              NHWIR,CORT1,CORT2,CORP1,CORP2,CORDT1,
     X              CORDT2,DDIS1,DDIS2,TDIV1,TDIV2,VERD1,
     X              VERD2,XCWIR,YCWIR,ZCWIR)
        IORIEN = IABS(IORIEN)
C
C    Plot the vernier solutions
C    ==========================
        S=1.
        IF (IPAD.NE.0) THEN
          IF (IORIEN.EQ.1) THEN
            VSOLU1 = YCWIR+VERD1+(((IABS(IPAD))-1)*60.96)
            VSOLU2 = YCWIR+VERD2+(((IABS(IPAD))-1)*60.96)
            IF(IVIEW.EQ.5.OR.IVIEW.EQ.11) THEN
              IF(IVIEW.EQ.11) S=-1.
              CALL PXMARKV('GRE',2,VSOLU1,S*XCWIR,ZCWIR)
              CALL PXMARKV('GRE',2,VSOLU2,S*XCWIR,ZCWIR)
              IF (IPAD.LT.0) CALL JCIRCL(VSOLU1,S*XCWIR,ZCWIR,1.8,0)
              IF (IPAD.GT.0) CALL JCIRCL(VSOLU2,S*XCWIR,ZCWIR,1.8,0)
            ENDIF
          ENDIF
          IF (IORIEN.EQ.2) THEN
            VSOLU1 = XCWIR+VERD1+(((IABS(IPAD))-1)*60.96)
            VSOLU2 = XCWIR+VERD2+(((IABS(IPAD))-1)*60.96)
            IF(IVIEW.EQ.2.OR.IVIEW.EQ.8) THEN
              IF(IVIEW.EQ.8) S=-1.
              CALL PXMARKV('GRE',2,VSOLU1,S*YCWIR,ZCWIR)
              CALL PXMARKV('GRE',2,VSOLU2,S*YCWIR,ZCWIR)
              IF (IPAD.LT.0) CALL JCIRCL(VSOLU1,S*YCWIR,ZCWIR,1.8,0)
              IF (IPAD.GT.0) CALL JCIRCL(VSOLU2,S*YCWIR,ZCWIR,1.8,0)
            ENDIF
          ENDIF
          IF (IORIEN.EQ.3) THEN
            VSOLU1 = YCWIR+VERD1+(((IABS(IPAD))-1)*60.96)
            VSOLU2 = YCWIR+VERD2+(((IABS(IPAD))-1)*60.96)
            IF(IVIEW.EQ.4.OR.IVIEW.EQ.10) THEN
              IF(IVIEW.EQ.10) S=-1.
              CALL PXMARKV('GRE',2,VSOLU1,S*ZCWIR,XCWIR)
              CALL PXMARKV('GRE',2,VSOLU2,S*ZCWIR,XCWIR)
              IF (IPAD.LT.0) CALL JCIRCL(VSOLU1,S*ZCWIR,XCWIR,1.8,0)
              IF (IPAD.GT.0) CALL JCIRCL(VSOLU2,S*ZCWIR,XCWIR,1.8,0)
            ENDIF
          ENDIF
          IF (IORIEN.EQ.4) THEN
            VSOLU1 = XCWIR+VERD1+(((IABS(IPAD))-1)*60.96)
            VSOLU2 = XCWIR+VERD2+(((IABS(IPAD))-1)*60.96)
            IF(IVIEW.EQ.3.OR.IVIEW.EQ.9) THEN
              IF(IVIEW.EQ.9) S=-1.
              CALL PXMARKV('GRE',2,VSOLU1,S*ZCWIR,YCWIR)
              CALL PXMARKV('GRE',2,VSOLU2,S*ZCWIR,YCWIR)
              IF (IPAD.LT.0) CALL JCIRCL(VSOLU1,S*ZCWIR,YCWIR,1.8,0)
              IF (IPAD.GT.0) CALL JCIRCL(VSOLU2,S*ZCWIR,YCWIR,1.8,0)
            ENDIF
          ENDIF
        ENDIF
C
  100 CONTINUE
C
  999 RETURN
      END
