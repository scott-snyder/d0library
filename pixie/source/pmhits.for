      SUBROUTINE PMHITS(IPHITS,IVIEW,HITFLG,DTIMFLG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To display the processed hits
C-
C-   Inputs  : IPHITS - processed hit number
C-             IVIEW - the view that is being drawn
C-             HITFLG - flag to plot all hits or hits on tracks
C-      see PMEVNT for definition
C-   Created  31-JAN-1990   Carol Francis
C-   DH 4/90 add views 7-12
C -  SH 10/26 added view 13 (view 3 rotated by 90)
C-   SH 4/20/93 - Added HITFLG argument
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER IVIEW                     ! Default view
      INTEGER IPHITS                    ! Processed hit number
      INTEGER HITFLG                    ! draw hit flag
      INTEGER DTIMFLG                   ! draw drift time flag
C_____________________________________________________________________
C  Local Declarations
C  ==================
      INTEGER IWADD,IFW1,IFW2,INRAW,IORIEN,NHWIR
      REAL CORT1,CORT2,CORP1,CORP2
      REAL CORDT1,CORDT2
      REAL DDIS1,DDIS2,S
      REAL TDIV1,TDIV2
      REAL VERD1,VERD2
      REAL XCWIR,YCWIR,ZCWIR
      CHARACTER*3 COLOR
C
C  Executable program
C  ==================
      CALL GTMUOH(IPHITS,IWADD,IFW1,IFW2,INRAW,IORIEN,NHWIR,CORT1,
     X      CORT2,CORP1,CORP2,CORDT1,CORDT2,DDIS1,DDIS2,TDIV1,TDIV2,
     X      VERD1,VERD2,XCWIR,YCWIR,ZCWIR)
      IFW2   = ABS(IFW2)
      IORIEN = ABS(IORIEN)
      COLOR='RED'
C IFW2 - track label if hit on track
C Check if hit not on track
      IF (IFW2 .LT. 1) THEN
C check if hits not on track not wanted
        IF(HITFLG.EQ.1)GO TO 999
C check if hits off tracks should be a different color
        IF(HITFLG.EQ.2)COLOR='CYA'
      ENDIF
C
C  =========================================================================
      IF (IVIEW .EQ. 1.OR.IVIEW.EQ.7) THEN
        S=1.
        IF(IVIEW.EQ.7) S=-1.
C  =========================================================================
C  Display drift time solutions... if desired - Must check IORIEN to find
C  the correct dimension in which to add drift distances and time division.
C  =========================================================================
C
        IF (DTIMFLG .EQ. 1) THEN
          IF (IORIEN .EQ. 1) THEN
            CALL PXMARK(COLOR,5,ZCWIR+DDIS1,S*(YCWIR+TDIV1),XCWIR)
            CALL PXMARK(COLOR,5,ZCWIR-DDIS1,S*(YCWIR+TDIV1),XCWIR)
          ENDIF
          IF (IORIEN .EQ. 2) THEN
            CALL PXMARK(COLOR,5,ZCWIR+DDIS1,S*YCWIR,XCWIR+TDIV1)
            CALL PXMARK(COLOR,5,ZCWIR-DDIS1,S*YCWIR,XCWIR+TDIV1)
          ENDIF
          IF (IORIEN .EQ. 4) THEN
            CALL PXMARK(COLOR,5,ZCWIR,S*(YCWIR+DDIS1),XCWIR+TDIV1)
            CALL PXMARK(COLOR,5,ZCWIR,S*(YCWIR-DDIS1),XCWIR+TDIV1)
          ENDIF
          IF (IORIEN .EQ. 3) THEN
            CALL PXMARK(COLOR,5,ZCWIR,S*(YCWIR+TDIV1),XCWIR+DDIS1)
            CALL PXMARK(COLOR,5,ZCWIR,S*(YCWIR+TDIV1),XCWIR-DDIS1)
          ENDIF
C  ==================================================================
C  Check to see if there are any more hits on wire and mark the drift
C  time solutions:
C  ==================================================================
          IF (NHWIR .GT. 1) THEN
            IF (IORIEN .EQ. 1) THEN
              CALL PXMARK(COLOR,5,ZCWIR+DDIS2,S*(YCWIR+TDIV2),XCWIR)
              CALL PXMARK(COLOR,5,ZCWIR-DDIS2,S*(YCWIR+TDIV2),XCWIR)
            ENDIF
            IF (IORIEN .EQ. 2) THEN
              CALL PXMARK(COLOR,5,ZCWIR+DDIS2,S*YCWIR,XCWIR+TDIV2)
              CALL PXMARK(COLOR,5,ZCWIR-DDIS2,S*YCWIR,XCWIR+TDIV2)
            ENDIF
            IF (IORIEN .EQ. 4) THEN
              CALL PXMARK(COLOR,5,ZCWIR,S*(YCWIR+DDIS2),XCWIR+TDIV2)
              CALL PXMARK(COLOR,5,ZCWIR,S*(YCWIR-DDIS2),XCWIR+TDIV2)
            ENDIF
            IF (IORIEN .EQ. 3) THEN
              CALL PXMARK(COLOR,5,ZCWIR,S*(YCWIR+TDIV2),XCWIR+DDIS2)
              CALL PXMARK(COLOR,5,ZCWIR,S*(YCWIR+TDIV2),XCWIR-DDIS2)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C  ========================================================================
      IF (IVIEW .EQ. 2.OR.IVIEW.EQ.8) THEN
        S=1.
        IF(IVIEW.EQ.8) S=-1.
C  ========================================================================
C  Display drift time solutions... if desired - Must check IORIEN to find
C  the correct dimension in which to add drift distances and time division.
C  ========================================================================
        IF (DTIMFLG .EQ. 1) THEN
          IF (IORIEN .EQ. 1) THEN
            CALL PXMARK(COLOR,4,XCWIR,S*(YCWIR+TDIV1),ZCWIR+DDIS1)
            CALL PXMARK(COLOR,4,XCWIR,S*(YCWIR+TDIV1),ZCWIR-DDIS1)
          ENDIF
          IF (IORIEN .EQ. 2) THEN
            CALL PXMARK(COLOR,4,XCWIR+TDIV1,S*YCWIR,ZCWIR+DDIS1)
            CALL PXMARK(COLOR,4,XCWIR+TDIV1,S*YCWIR,ZCWIR-DDIS1)
          ENDIF
          IF (IORIEN .EQ. 4) THEN
            CALL PXMARK(COLOR,4,XCWIR+TDIV1,S*(YCWIR+DDIS1),ZCWIR)
            CALL PXMARK(COLOR,4,XCWIR+TDIV1,S*(YCWIR-DDIS1),ZCWIR)
          ENDIF
          IF (IORIEN .EQ. 3) THEN
            CALL PXMARK(COLOR,4,XCWIR+DDIS1,S*(YCWIR+TDIV1),ZCWIR)
            CALL PXMARK(COLOR,4,XCWIR-DDIS1,S*(YCWIR+TDIV1),ZCWIR)
          ENDIF
C  ==================================================================
C  Check to see if there are any more hits on wire and mark the drift
C  time solutions:
C  ==================================================================
          IF (NHWIR .GT. 1) THEN
            IF (IORIEN .EQ. 1) THEN
              CALL PXMARK(COLOR,4,XCWIR,S*(YCWIR+TDIV2),ZCWIR+DDIS2)
              CALL PXMARK(COLOR,4,XCWIR,S*(YCWIR+TDIV2),ZCWIR-DDIS2)
            ENDIF
            IF (IORIEN .EQ. 2) THEN
              CALL PXMARK(COLOR,4,XCWIR+TDIV2,S*YCWIR,ZCWIR+DDIS2)
              CALL PXMARK(COLOR,4,XCWIR+TDIV2,S*YCWIR,ZCWIR-DDIS2)
            ENDIF
            IF (IORIEN .EQ. 4) THEN
              CALL PXMARK(COLOR,4,XCWIR+TDIV2,S*(YCWIR+DDIS2),ZCWIR)
              CALL PXMARK(COLOR,4,XCWIR+TDIV2,S*(YCWIR-DDIS2),ZCWIR)
            ENDIF
            IF (IORIEN .EQ. 3) THEN
              CALL PXMARK(COLOR,4,XCWIR+DDIS2,S*(YCWIR+TDIV2),ZCWIR)
              CALL PXMARK(COLOR,4,XCWIR-DDIS2,S*(YCWIR+TDIV2),ZCWIR)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C  =======================================================================
      IF (IVIEW .EQ. 3.OR.IVIEW.EQ.9) THEN
        S=1.
        IF(IVIEW.EQ.9) S=-1.
C  =======================================================================
C  Display drift time solutions... if desired - Must check IORIEN to find
C  the correct dimension in which to add drift distances and time division.
C  =======================================================================
        IF (DTIMFLG .EQ. 1) THEN
          IF (IORIEN .EQ. 1) THEN
            CALL PXMARK(COLOR,3,XCWIR,S*(ZCWIR+DDIS1),YCWIR+TDIV1)
            CALL PXMARK(COLOR,3,XCWIR,S*(ZCWIR-DDIS1),YCWIR+TDIV1)
          ENDIF
          IF (IORIEN .EQ. 2) THEN
            CALL PXMARK(COLOR,3,XCWIR+TDIV1,S*(ZCWIR+DDIS1),YCWIR)
            CALL PXMARK(COLOR,3,XCWIR+TDIV1,S*(ZCWIR-DDIS1),YCWIR)
          ENDIF
          IF (IORIEN .EQ. 4) THEN
            CALL PXMARK(COLOR,3,XCWIR+TDIV1,S*ZCWIR,YCWIR+DDIS1)
            CALL PXMARK(COLOR,3,XCWIR+TDIV1,S*ZCWIR,YCWIR-DDIS1)
          ENDIF
          IF (IORIEN .EQ. 3) THEN
            CALL PXMARK(COLOR,3,XCWIR+DDIS1,S*ZCWIR,YCWIR+TDIV1)
            CALL PXMARK(COLOR,3,XCWIR-DDIS1,S*ZCWIR,YCWIR+TDIV1)
          ENDIF
C  ==================================================================
C  Check to see if there are any more hits on wire and mark the drift
C  time solutions:
C  ==================================================================
C
          IF (NHWIR .GT. 1) THEN
            IF (IORIEN .EQ. 1) THEN
              CALL PXMARK(COLOR,3,XCWIR,S*(ZCWIR+DDIS2),YCWIR+TDIV2)
              CALL PXMARK(COLOR,3,XCWIR,S*(ZCWIR-DDIS2),YCWIR+TDIV2)
            ENDIF
            IF (IORIEN .EQ. 2) THEN
              CALL PXMARK(COLOR,3,XCWIR+TDIV2,S*(ZCWIR+DDIS2),YCWIR)
              CALL PXMARK(COLOR,3,XCWIR+TDIV2,S*(ZCWIR-DDIS2),YCWIR)
            ENDIF
            IF (IORIEN .EQ. 4) THEN
              CALL PXMARK(COLOR,3,XCWIR+TDIV2,S*ZCWIR,YCWIR+DDIS2)
              CALL PXMARK(COLOR,3,XCWIR+TDIV2,S*ZCWIR,YCWIR-DDIS2)
            ENDIF
            IF (IORIEN .EQ. 3) THEN
              CALL PXMARK(COLOR,3,XCWIR+DDIS2,S*ZCWIR,YCWIR+TDIV2)
              CALL PXMARK(COLOR,3,XCWIR-DDIS2,S*ZCWIR,YCWIR+TDIV2)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C  =========================================================================
      IF (IVIEW .EQ. 4.OR.IVIEW.EQ.10) THEN
        S=1.
        IF(IVIEW.EQ.10) S=-1.
C  =========================================================================
C  Display drift time solutions... if desired - Must check IORIEN to find
C  the correct dimension in which to add drift distances and time division.
C  =========================================================================
C
        IF (DTIMFLG .EQ. 1) THEN
          IF (IORIEN .EQ. 1) THEN
            CALL PXMARK(COLOR,5,YCWIR+TDIV1,S*(ZCWIR+DDIS1),XCWIR)
            CALL PXMARK(COLOR,5,YCWIR+TDIV1,S*(ZCWIR-DDIS1),XCWIR)
          ENDIF
          IF (IORIEN .EQ. 2) THEN
            CALL PXMARK(COLOR,5,YCWIR,S*(ZCWIR+DDIS1),XCWIR+TDIV1)
            CALL PXMARK(COLOR,5,YCWIR,S*(ZCWIR-DDIS1),XCWIR+TDIV1)
          ENDIF
          IF (IORIEN .EQ. 4) THEN
            CALL PXMARK(COLOR,5,YCWIR+DDIS1,S*ZCWIR,XCWIR+TDIV1)
            CALL PXMARK(COLOR,5,YCWIR-DDIS1,S*ZCWIR,XCWIR+TDIV1)
          ENDIF
          IF (IORIEN .EQ. 3) THEN
            CALL PXMARK(COLOR,5,YCWIR+TDIV1,S*ZCWIR,XCWIR+DDIS1)
            CALL PXMARK(COLOR,5,YCWIR+TDIV1,S*ZCWIR,XCWIR-DDIS1)
          ENDIF
C  ==================================================================
C  Check to see if there are any more hits on wire and mark the drift
C  time solutions:
C  ==================================================================
          IF (NHWIR .GT. 1) THEN
            IF (IORIEN .EQ. 1) THEN
              CALL PXMARK(COLOR,5,YCWIR+TDIV2,S*(ZCWIR+DDIS2),XCWIR)
              CALL PXMARK(COLOR,5,YCWIR+TDIV2,S*(ZCWIR-DDIS2),XCWIR)
            ENDIF
            IF (IORIEN .EQ. 2) THEN
              CALL PXMARK(COLOR,5,YCWIR,S*(ZCWIR+DDIS2),XCWIR+TDIV2)
              CALL PXMARK(COLOR,5,YCWIR,S*(ZCWIR-DDIS2),XCWIR+TDIV2)
            ENDIF
            IF (IORIEN .EQ. 4) THEN
              CALL PXMARK(COLOR,5,YCWIR+DDIS2,S*ZCWIR,XCWIR+TDIV2)
              CALL PXMARK(COLOR,5,YCWIR-DDIS2,S*ZCWIR,XCWIR+TDIV2)
            ENDIF
            IF (IORIEN .EQ. 3) THEN
              CALL PXMARK(COLOR,5,YCWIR+TDIV2,S*ZCWIR,XCWIR+DDIS2)
              CALL PXMARK(COLOR,5,YCWIR+TDIV2,S*ZCWIR,XCWIR-DDIS2)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C  ========================================================================
      IF (IVIEW .EQ. 5.OR.IVIEW.EQ.11) THEN
        S=1.
        IF(IVIEW.EQ.11) S=-1.
C  ========================================================================
C  Display drift time solutions... if desired - Must check IORIEN to find
C  the correct dimension in which to add drift distances and time division.
C  ========================================================================
        IF (DTIMFLG .EQ. 1) THEN
          IF (IORIEN .EQ. 1) THEN
            CALL PXMARK(COLOR,4,YCWIR+TDIV1,S*XCWIR,ZCWIR+DDIS1)
            CALL PXMARK(COLOR,4,YCWIR+TDIV1,S*XCWIR,ZCWIR-DDIS1)
          ENDIF
          IF (IORIEN .EQ. 2) THEN
            CALL PXMARK(COLOR,4,YCWIR,S*(XCWIR+TDIV1),ZCWIR+DDIS1)
            CALL PXMARK(COLOR,4,YCWIR,S*(XCWIR+TDIV1),ZCWIR-DDIS1)
          ENDIF
          IF (IORIEN .EQ. 4) THEN
            CALL PXMARK(COLOR,4,YCWIR+DDIS1,S*(XCWIR+TDIV1),ZCWIR)
            CALL PXMARK(COLOR,4,YCWIR-DDIS1,S*(XCWIR+TDIV1),ZCWIR)
          ENDIF
          IF (IORIEN .EQ. 3) THEN
            CALL PXMARK(COLOR,4,YCWIR+TDIV1,S*(XCWIR+DDIS1),ZCWIR)
            CALL PXMARK(COLOR,4,YCWIR+TDIV1,S*(XCWIR-DDIS1),ZCWIR)
          ENDIF
C  ==================================================================
C  Check to see if there are any more hits on wire and mark the drift
C  time solutions:
C  ==================================================================
          IF (NHWIR .GT. 1) THEN
            IF (IORIEN .EQ. 1) THEN
              CALL PXMARK(COLOR,4,YCWIR+TDIV2,S*XCWIR,ZCWIR+DDIS2)
              CALL PXMARK(COLOR,4,YCWIR+TDIV2,S*XCWIR,ZCWIR-DDIS2)
            ENDIF
            IF (IORIEN .EQ. 2) THEN
              CALL PXMARK(COLOR,4,YCWIR,S*(XCWIR+TDIV2),ZCWIR+DDIS2)
              CALL PXMARK(COLOR,4,YCWIR,S*(XCWIR+TDIV2),ZCWIR-DDIS2)
            ENDIF
            IF (IORIEN .EQ. 4) THEN
              CALL PXMARK(COLOR,4,YCWIR+DDIS2,S*(XCWIR+TDIV2),ZCWIR)
              CALL PXMARK(COLOR,4,YCWIR-DDIS2,S*(XCWIR+TDIV2),ZCWIR)
            ENDIF
            IF (IORIEN .EQ. 3) THEN
              CALL PXMARK(COLOR,4,YCWIR+TDIV2,S*(XCWIR+DDIS2),ZCWIR)
              CALL PXMARK(COLOR,4,YCWIR+TDIV2,S*(XCWIR-DDIS2),ZCWIR)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C  =======================================================================
      IF (IVIEW .EQ. 6.OR.IVIEW.EQ.12) THEN
        S=1.
        IF(IVIEW.EQ.12) S=-1.
C  =======================================================================
C  Display drift time solutions... if desired - Must check IORIEN to find
C  the correct dimension in which to add drift distances and time division.
C  =======================================================================
        IF (DTIMFLG .EQ. 1) THEN
          IF (IORIEN .EQ. 1) THEN
            CALL PXMARK(COLOR,3,ZCWIR+DDIS1,S*XCWIR,YCWIR+TDIV1)
            CALL PXMARK(COLOR,3,ZCWIR-DDIS1,S*XCWIR,YCWIR+TDIV1)
          ENDIF
          IF (IORIEN .EQ. 2) THEN
            CALL PXMARK(COLOR,3,ZCWIR+DDIS1,S*(XCWIR+TDIV1),YCWIR)
            CALL PXMARK(COLOR,3,ZCWIR-DDIS1,S*(XCWIR+TDIV1),YCWIR)
          ENDIF
          IF (IORIEN .EQ. 4) THEN
            CALL PXMARK(COLOR,3,ZCWIR,S*(XCWIR+TDIV1),YCWIR+DDIS1)
            CALL PXMARK(COLOR,3,ZCWIR,S*(XCWIR+TDIV1),YCWIR-DDIS1)
          ENDIF
          IF (IORIEN .EQ. 3) THEN
            CALL PXMARK(COLOR,3,ZCWIR,S*(XCWIR+DDIS1),YCWIR+TDIV1)
            CALL PXMARK(COLOR,3,ZCWIR,S*(XCWIR-DDIS1),YCWIR+TDIV1)
          ENDIF
C  ==================================================================
C  Check to see if there are any more hits on wire and mark the drift
C  time solutions:
C  ==================================================================
C
          IF (NHWIR .GT. 1) THEN
            IF (IORIEN .EQ. 1) THEN
              CALL PXMARK(COLOR,3,ZCWIR+DDIS2,S*XCWIR,YCWIR+TDIV2)
              CALL PXMARK(COLOR,3,ZCWIR-DDIS2,S*XCWIR,YCWIR+TDIV2)
            ENDIF
            IF (IORIEN .EQ. 2) THEN
              CALL PXMARK(COLOR,3,ZCWIR+DDIS2,S*(XCWIR+TDIV2),YCWIR)
              CALL PXMARK(COLOR,3,ZCWIR-DDIS2,S*(XCWIR+TDIV2),YCWIR)
            ENDIF
            IF (IORIEN .EQ. 4) THEN
              CALL PXMARK(COLOR,3,ZCWIR,S*(XCWIR+TDIV2),YCWIR+DDIS2)
              CALL PXMARK(COLOR,3,ZCWIR,S*(XCWIR+TDIV2),YCWIR-DDIS2)
            ENDIF
            IF (IORIEN .EQ. 3) THEN
              CALL PXMARK(COLOR,3,ZCWIR,S*(XCWIR+DDIS2),YCWIR+TDIV2)
              CALL PXMARK(COLOR,3,ZCWIR,S*(XCWIR-DDIS2),YCWIR+TDIV2)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C  =======================================================================
      IF (IVIEW .EQ. 13) THEN
        S=1.
C  =======================================================================
C  Display drift time solutions... if desired - Must check IORIEN to find
C  the correct dimension in which to add drift distances and time division.
C  =======================================================================
        IF (DTIMFLG .EQ. 1) THEN
          IF (IORIEN .EQ. 1) THEN
            CALL PXMARK(COLOR,3,S*(ZCWIR+DDIS1),XCWIR,YCWIR+TDIV1)
            CALL PXMARK(COLOR,3,S*(ZCWIR-DDIS1),XCWIR,YCWIR+TDIV1)
          ENDIF
          IF (IORIEN .EQ. 2) THEN
            CALL PXMARK(COLOR,3,S*(ZCWIR+DDIS1),XCWIR+TDIV1,YCWIR)
            CALL PXMARK(COLOR,3,S*(ZCWIR-DDIS1),XCWIR+TDIV1,YCWIR)
          ENDIF
          IF (IORIEN .EQ. 4) THEN
            CALL PXMARK(COLOR,3,S*ZCWIR,XCWIR+TDIV1,YCWIR+DDIS1)
            CALL PXMARK(COLOR,3,S*ZCWIR,XCWIR+TDIV1,YCWIR-DDIS1)
          ENDIF
          IF (IORIEN .EQ. 3) THEN
            CALL PXMARK(COLOR,3,S*ZCWIR,XCWIR+DDIS1,YCWIR+TDIV1)
            CALL PXMARK(COLOR,3,S*ZCWIR,XCWIR-DDIS1,YCWIR+TDIV1)
          ENDIF
C  ==================================================================
C  Check to see if there are any more hits on wire and mark the drift
C  time solutions:
C  ==================================================================
C
          IF (NHWIR .GT. 1) THEN
            IF (IORIEN .EQ. 1) THEN
              CALL PXMARK(COLOR,3,S*(ZCWIR+DDIS2),XCWIR,YCWIR+TDIV2)
              CALL PXMARK(COLOR,3,S*(ZCWIR-DDIS2),XCWIR,YCWIR+TDIV2)
            ENDIF
            IF (IORIEN .EQ. 2) THEN
              CALL PXMARK(COLOR,3,S*(ZCWIR+DDIS2),XCWIR+TDIV2,YCWIR)
              CALL PXMARK(COLOR,3,S*(ZCWIR-DDIS2),XCWIR+TDIV2,YCWIR)
            ENDIF
            IF (IORIEN .EQ. 4) THEN
              CALL PXMARK(COLOR,3,S*ZCWIR,XCWIR+TDIV2,YCWIR+DDIS2)
              CALL PXMARK(COLOR,3,S*ZCWIR,XCWIR+TDIV2,YCWIR-DDIS2)
            ENDIF
            IF (IORIEN .EQ. 3) THEN
              CALL PXMARK(COLOR,3,S*ZCWIR,XCWIR+DDIS2,YCWIR+TDIV2)
              CALL PXMARK(COLOR,3,S*ZCWIR,XCWIR-DDIS2,YCWIR+TDIV2)
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
  999 RETURN
      END
