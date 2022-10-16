      LOGICAL FUNCTION MU_B
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Flags events for the SAMUS muon offline filter
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: MU_B.RCP file
C-
C-   Return Value: .TRUE.      keep event for filter
C-                 .FALSE. not keep event for filter
C-
C-   Created  03-MAY-1993   Dmitri Denisov (from Daria Zieminska template)
C-   Updated  28-APR-1994   Dmitri Denisov, SAMUS - quadrants 13 and 14 only
C-   Updated  09-OCT-1994   Dmitri Denisov, add calorimeter confirmation cut
C-                          and number of point on track cut.
C-   Updated   1-DEC-1995   Andrei Mayorov, change stops to errmsg 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL PASSED,FIRST
      INTEGER IER,LMUOT,NTRACKS,ITRACK,QUAD,LPARH,GZPARH,LPMUO,GZPMUO
      INTEGER NSAMUS,NPOINTS,I
      REAL PCUT,PMU,ECUT,EHAD
C
      EXTERNAL GZPARH,GZPMUO
C
      DATA FIRST /.TRUE./
C
      SAVE FIRST
C
C...  Read RCP file if first event
C
      IF (FIRST) THEN
        CALL INRCP('MU_B_RCP',IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('MU_B_RCP not found','MU_B',' ','F')
        ELSE
          CALL EZPICK('MU_B_RCP')
        END IF
        CALL EZGET('P_MUB', PCUT, IER)
        IF (IER.NE.0)  CALL ERRMSG('PCUT not defined','MU_B',' ','F')
        CALL EZGET('ECUT_MUB', ECUT, IER)
        IF (IER.NE.0) CALL ERRMSG('ECUT_MUB  not defined','MU_B',' ',
     &    'F')
        CALL EZGET_i('NPOINTS_MUB', NPOINTS, IER)
        IF (IER.NE.0) CALL ERRMSG('NPOINTS_MUB  not defined','MU_B',
     &  ' ','F') 
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
C...  Starting check of the event
C
      PASSED = .FALSE.
C
C...  Is there PMUO in this event?
C
      LPARH=GZPARH(I)
      IF(LPARH.LE.0) GO TO 2000
      NTRACKS=IQ(LPARH+2)
      IF(NTRACKS.LE.0) GO TO 2000
C
C...  Loop over all muon tracks
C
      DO 99 ITRACK=1,NTRACKS
        LPMUO=GZPMUO(ITRACK)
        IF(LPMUO.LE.0) then
          CALL errmsg('Error PMUO not found','MU_B',' ','W')
          GO TO 99
        ELSE
C
C...  Check if SAMUS track
C
        QUAD=IQ(LPMUO+7)
        IF(QUAD.LE.12) GO TO 99
        IF(QUAD.GE.15) GO TO 99
        PMU=ABS(Q(LPMUO+13))
        IF(PMU.LE.PCUT) GO TO 99
        EHAD=Q(LPMUO+34)-Q(LPMUO+80)
        IF(EHAD.LE.ECUT) GO TO 99
        LMUOT=LQ(LPMUO-2)
        IF(LMUOT.LE.0) then
          CALL errmsg('Error MUOT not found','MU_B',' ','W')
          GO TO 99
        ELSE     
        NSAMUS=IQ(LMUOT+2)
        IF(NSAMUS.LT.NPOINTS) GO TO 99
        PASSED=.TRUE.
        GO TO 2000
        END IF
        END IF
   99 CONTINUE
C
C...   See if event passed selection
C
 2000 MU_B = PASSED
C
  999 RETURN
      END
