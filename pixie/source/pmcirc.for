      SUBROUTINE PMCIRC(IVIEW,ITRAK,NPTRAK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To draw circles around the drift time
C-                         hits in the bend view
C-
C-   Inputs  :
C-
C-   Created  26-MAR-1990   Carol C. Francis
C-   DH 4/90 FIX ITSIGN=0; add views 7-12
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
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
      REAL DDIS,TDIV
C
C    Executable code
C    ===============
C
C    Get information about the track
C    ===============================
      DO 100 L = 1,NPTRAK
        CALL GTMHTT(ITRAK,L,IWADD,IHMUOH,ITSIGN,IDELT,IPAD)
        IF(ITSIGN.NE.0) THEN
          CALL GTMUOH(IHMUOH,IWADD,IFW1,IFW2,INRAW,IORIEN,
     X              NHWIR,CORT1,CORT2,CORP1,CORP2,CORDT1,
     X              CORDT2,DDIS1,DDIS2,TDIV1,TDIV2,VERD1,
     X              VERD2,XCWIR,YCWIR,ZCWIR)
          IORIEN = IABS(IORIEN)
C
C    Draw the circles
C    ================K
          IF (ITSIGN.EQ.-2) DDIS = -DDIS2
          IF (ITSIGN.EQ.-1) DDIS = -DDIS1
          IF (ITSIGN.EQ.1)  DDIS =  DDIS1
          IF (ITSIGN.EQ.2)  DDIS =  DDIS2
          IF (ABS(ITSIGN).EQ.2) TDIV = TDIV2
          IF (ABS(ITSIGN).EQ.1) TDIV = TDIV1
C
          IF (IORIEN.EQ.2) THEN
            IF (IVIEW.EQ.1) CALL JCIRCL(ZCWIR+DDIS,YCWIR,0.,1.8,0)
            IF (IVIEW.EQ.7) CALL JCIRCL(ZCWIR+DDIS,-YCWIR,0.,1.8,0)
          ELSE IF(IORIEN.EQ.3) THEN
            IF (IVIEW.EQ.3) CALL JCIRCL(XCWIR+DDIS,ZCWIR,0.,1.8,0)
            IF (IVIEW.EQ.9) CALL JCIRCL(XCWIR+DDIS,-ZCWIR,0.,1.8,0)
          ELSE IF(IORIEN.EQ.4) THEN
            IF (IVIEW.EQ.4) CALL JCIRCL(YCWIR+DDIS,ZCWIR,0.,1.8,0)
            IF (IVIEW.EQ.10) CALL JCIRCL(YCWIR+DDIS,-ZCWIR,0.,1.8,0)
          ELSE IF (IORIEN.EQ.1) THEN
            IF (IVIEW.EQ.6) CALL JCIRCL(ZCWIR+DDIS,XCWIR,0.,1.8,0)
            IF (IVIEW.EQ.12) CALL JCIRCL(ZCWIR+DDIS,-XCWIR,0.,1.8,0)
          ENDIF
C
        ENDIF
  100 CONTINUE
  999 RETURN
      END
