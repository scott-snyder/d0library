      LOGICAL FUNCTION TRD_FILLS_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TRD EXAMINE process loop
C-
C-   Returned value  : true
C-
C-   Created  19-MAR-1991   JFG Recreated from one of the multiple existing
C-                          versions of TRDEVT.
C-   Updated   6-JAN-1994   A. Zylberstejn
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TCNTRL.INC'
      INTEGER ILINKS(3),NUMTRIG,IER,ITRIGGER,KTRIGGER
      LOGICAL FIRST
      CHARACTER*4 SWPHYS(10)
      INTEGER     ISWPHY(10)
      CHARACTER*3 C3
C      EQUIVALENCE (SWPHYS(1),ISWPHY(1))
      DATA ILINKS/3*0/
      DATA FIRST/.TRUE./
      IF(FIRST)THEN
        FIRST=.FALSE.
C      Not in a "first" loop to allow on-line change of RCP
        CALL EZPICK('TRDHIT_RCP')
        CALL EZGET('TEXAMIN',TEXAMIN,IER)
        CALL EZGET('TTRAKIN',TTRAKIN,IER)
        CALL EZGET('NUM_PHYS_SWIT',NUMTRIG,IER)
        CALL EZGET('PHYSICS_SWITCHES',ISWPHY(1),IER)
        CALL EZRSET
      END IF
C loops on the triggers (not fully implemented!)
      IF (TEXAMIN) THEN
        DO 10 ITRIGGER = 1,NUMTRIG
          CALL UHTOC(ISWPHY(ITRIGGER),3,C3,3)
          IF (C3.EQ.'Y') THEN
            KTRIGGER = ITRIGGER - 1
            CALL TRREMP(KTRIGGER)
          ENDIF
   10   CONTINUE
      ENDIF
      IF (TTRAKIN) THEN
        CALL TTRAKS(ILINKS)
      ENDIF
      TRD_FILLS_EVT = .TRUE.
  999 RETURN
      END
