      SUBROUTINE VTXERT (TIME, XMINE, XERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : convert drift time to drift distance and its
C-                         error for "realistic" MC
C-   Drift times have been found from reconstruction of MC tracks,
C-   errors have been taken from the paper on test beam results
C-
C-   Inputs  : TIME - drift time
C-   Outputs : XMINE - coordinate,
C-             XERROR - error of coordinate
C-   Controls:
C-
C-   Created  12-SEP-1992   Alexandre Zinchenko
C-   Updated  23-MAR-1993   A.Zinchenko - add message
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST
      INTEGER I, IP
      REAL TIME, XMINE, XERROR, TIME0(16), ERROR(16), DX, DT, DT0
      DATA TIME0/ 0., 17.3, 28.7, 65.0, 151.4, 266.9, 400.3, 534.3,
     &            672.3, 810.3, 948.3, 1086.2, 1225.3, 1364.3, 1511.5,
     &            1658.6/
      DATA ERROR/ 0.020, 0.012, 0.010, 0.007, 0.0045, 0.004, 0.004,
     *            0.0045, 0.005, 0.0055, 0.0055, 0.006, 0.006, 0.006,
     *            0.0065, 0.0065/
      DATA DX/0.1/, FIRST/.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL ERRMSG('New_Monte_Carlo','VTXERT',
     &    'Using time-to-distance conversion for realistic MC','W')
        FIRST = .FALSE.
      ENDIF
      DO 10 I = 1,16
        IF (TIME0(I).GE.TIME) GO TO 11
   10 CONTINUE
      I = 16
   11 IF ( I .GT. 2 ) THEN
        DT0 = TIME0(I)-TIME0(I-1)
        DT = TIME-TIME0(I-1)
        XMINE = DT/DT0*DX + (I-3)*DX
      ELSE
        XMINE = 0.
      ENDIF
      IF (XMINE.LT.0.) XMINE = 0.
      IP = XMINE/DX + 1.5
      IP = MIN0(IP,15)
      XERROR = ERROR(IP)
  999 RETURN
      END
