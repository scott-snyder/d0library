      SUBROUTINE GTFDCT_LINK(LFDCT,QTRAK,QHSEC,LADDER)
C-----------------------------------------------------------------------
C
C    Purpose and Methods : Returns a FDC track and associated hits
C
C    Input  : LFDCT        = FDCT track bank address
C
C    Output : QTRAK(1:26)= contains information on the fitted track
C             QHSEC(1:3,1:34)= contains info on hits in fitted track
C             LADDER(0:2)= contains info on the segment ladder to form track
C
C-   Created  xx-DEC-1989   Daria Zieminska
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up
C-   Updated  23-JUL-1990   Jeffrey Bantly  add theta angle to track 
C-   Updated  24-JAN-1991   Jeffrey Bantly  add segment ladder to output 
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of QTRAK
C-    to accomodate theta and phi errors and two spare words.
C-   Updated  17-FEB-1993   Robert E. Avery  Don't change value of 
C-    input variable (use a new variable for FDTH bank pointer).
C
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LFDCT,LFDTH,NHSEC,LADDER(0:2)
      REAL QTRAK(26),QHSEC(3,34)
C-----------------------------------------------------------------------
      IF (LFDCT.EQ.0) THEN           ! track deleted
        CALL VZERO(QTRAK,26)
        CALL VZERO(QHSEC,102)
        CALL VZERO(LADDER(0),3)
        GO TO 999
      END IF
      CALL UCOPY(Q(LFDCT+1),QTRAK,26)
      CALL UCOPY(QTRAK(2),NHSEC,1)      ! fill track fit info
      LFDTH=LQ(LFDCT-1)
      IF(LFDTH.LE.0) THEN
        CALL VZERO(QHSEC,102)
        CALL VZERO(LADDER(0),3)
        GOTO 999
      ENDIF
      CALL UCOPY(Q(LFDTH+1),QHSEC(1,1),3*NHSEC)   ! fill track hits info
      CALL UCOPY(Q(LFDTH+103),LADDER(0),3)        ! fill segment ladder info
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
