      SUBROUTINE PFRZAD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action Routine to draw a multiple window 
C-                         display of an FDC R-Z view and a single track's
C-                         FADC traces
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  11-FEB-1991   Jeffrey Bantly
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT array.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ICALL,JCALL,IFL
      INTEGER TRKNUM,LADDER(0:2),IADD,LOGCHA
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB,MODULE
C
      REAL    CONT(62),FIADD
      EQUIVALENCE (FIADD, IADD )
C
      SAVE JCALL,IFL
C
      DATA JCALL,IFL/3,2/
C
C----------------------------------------------------------------------
      IF (JCALL.EQ.3) THEN
        JCALL=1
      ELSE IF (JCALL.EQ.1) THEN
        JCALL=2
      ELSE IF (JCALL.EQ.2) THEN
        JCALL=3
      ENDIF
C
C  List all of the FDC tracks shown and choose track desired.
C
      IF (JCALL.EQ.1) CALL PFPICK_TRACK(TRKNUM,HALF,IFL)
      IF (TRKNUM.LE.0) THEN
        CALL INTMSG(' No FDC tracks present and-or chosen.')
        GOTO 999
      ENDIF
C
C  Fetch segment ladder and obtain logical channel address.
C
      IF (JCALL.EQ.1) CALL FGETLDR2(TRKNUM,LADDER)
      IF ((LADDER(0)+LADDER(1)+LADDER(2)).LE.0 ) THEN
        CALL INTMSG(' No FDC segment ladder available.')
        GOTO 999
      ENDIF
C
      IF (LADDER(JCALL-1).GT.0) THEN
        MODULE=HALF*3+JCALL-1
        CALL GTFSEG(MODULE,LADDER(JCALL-1),CONT)
        FIADD=CONT(2)
        LOGCHA=IADD
C
C  Decode address and call FADC display routine with info.
C
        CALL FCODER(LOGCHA,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
C
        IF (UNIT.EQ.0) THEN
          CALL PF8ADC_MID(HALF,QUAD,SECTOR)
        ELSE IF (UNIT.EQ.1) THEN
          CALL PF16AD_MID(HALF,SECTOR)
        ENDIF
C
      ENDIF
C
C  Done.
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
