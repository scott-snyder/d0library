C=======================================================================
      SUBROUTINE ISZEVT(NPART,ID,PX,PY,PZ,PT,P,THETA,I3D)
C=======================================================================
C
C  Description:  Checks flags and plots uncut tracks.
C  ============
C
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - June 2, 1987
C  Updated Jan 4, 1990 Lupe Howell
C
C=================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
C
C  Local Declarations:
C  ====================
C
      INTEGER SI
      PARAMETER(SI=1000)
      INTEGER NPART,ID(SI),I3D
      INTEGER ICHRG,IDCHRG,I
      REAL PX(SI),PY(SI),PZ(SI),P(SI),PT(SI),THETA(SI),PSCAL
      REAL THMIN,PTMIN,PMIN,PI,THMDE
      INTEGER TERCOD
      LOGICAL LOGSCA,NEUTLS,NEUSON
      DATA PI/3.1416/
C
C  Executable Code:
C  =================
C
      CALL PUGETV('THETA MIN CUT',THMDE)
      CALL PUGET_l('LOG SCALE ON',LOGSCA)
      CALL PUGET_l('NEUTRALS PLOTTED',NEUTLS)
      CALL PUGET_l('NEUTRINOS PLOTTED',NEUSON)
      CALL PUGETV('P MIN CUT',PMIN)
      CALL PUGETV('PT MIN CUT',PTMIN)
      THMIN = (PI/180.)*THMDE
      DO 46 I = 1,NPART
C
C  If the particle is a neutrino, and neutrinos are turned off,
C  skip to the end of loop (don't plot the track).
C  =================================================================
C
         IF(((ABS(ID(I)) .EQ. 11).OR.(ABS(ID(I)).EQ.13)
     X     .OR.(ABS(ID(I)) .EQ. 15)).AND.(.NOT.NEUSON)) GO TO 46
         CALL PXCOLR('GRE')
         CALL JLSTYL(0)
C
C  Decide on a color and linestyle for the track depending upon ID(I)
C  =====================================================================
C
         CALL ISZOPT(ID(I))
C
C  See if the particle is charged... if not, don't plot it...
C  ===============================================================
C
         ICHRG = IDCHRG(ID(I))
         IF (NEUSON) THEN
            IF((ABS(ID(I)) .EQ. 11).OR.(ABS(ID(I)).EQ.13)
     X        .OR.(ABS(ID(I)) .EQ. 15)) ICHRG = 1
         ENDIF
         IF ((ICHRG .NE. 0).OR.(NEUTLS)) THEN
C
C  The following takes care of cuts on Theta min, P min and Pt min.
C  =================================================================
C
            IF ((THETA(I) .GT. THMIN).AND.
     X                     (THETA(I) .LT. (PI-THMIN))) THEN
               IF (P(I) .GT. PMIN) THEN
                  IF (PT(I) .GT. PTMIN) THEN
C
                     IF (I3D .EQ. 0) THEN 
                        PSCAL = 1.
                        IF (LOGSCA) THEN
                            PSCAL = 1/PT(I)
                            PSCAL=PSCAL*(LOG(PT(I))-LOG(.1))
                            IF (PSCAL .LE. 0) 
     X                      PSCAL = PSCAL*(LOG(.2)-LOG(.1))
                        ENDIF
                     ELSE
                        PSCAL = PT(I)/P(I)
                        IF (LOGSCA) THEN
                            PSCAL = 1/P(I) 
                            PSCAL=PSCAL*(LOG(PT(I)/P(I))-LOG(.1))
                            IF (PSCAL .LE. 0) 
     X                      PSCAL = PSCAL*(LOG(.2)-LOG(.1))
                        ENDIF
                     ENDIF
                     CALL ISZTRK(PX(I),PY(I),PZ(I),PSCAL)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
   46 CONTINUE
      RETURN
      END
C
C====================================================================
