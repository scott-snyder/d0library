      SUBROUTINE GTSTTH(ITRAK,LS,IHIT,ISTA,ISEC,IADDR,GEO,DIST)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Gets all the information from the STTH
C-                         Zebra bank for the hit # IHIT on track ITRAK
C-
C-   Inputs  :  ITRAK - Number of the track for which one would like info.
C-              LS    - Ref to STTH or 0
C-              IHIT  - Number of the hit for which one would like info.
C-                      If IHIT = 0, then IHIT = NA + 10*NB + 100*NC
C-
C-    Output :  ISTA  - Station number
C-              ISEC  - Section number
C-              IADDR - Module number*256 + tube number
C-              GEO(6) - Tube geometry (x,y,z, vx,vy,vz)
C-              DIST   - Drift distance
C-
C-    Created :
C-
C-    Modified: 04-FEB-1995     I.Mandrichenko  New format of STTH
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N_STTH
      PARAMETER   (N_STTH=8)
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER IHIT,ITRAK,LS
      INTEGER ISTA,ISEC,IADDR
      REAL    DIST,GEO(6)
C
      INTEGER I,GZSTTH,LSTTH,ISTTH,IMOD,IMODX,NABC,NH
      INTEGER JSTA,JSEC
C
      LSTTH = LS
      IF( LSTTH.LE.0 ) LSTTH = GZSTTH(ITRAK)
C
      IF (LSTTH .NE. 0) THEN
        ISTTH = LSTTH
        IF( IHIT.EQ.0 ) THEN
          NABC = 0
          NH = IQ(LSTTH-1)/N_STTH
          DO I=1,NH
            IMOD = IQ(ISTTH+1)/256
            CALL MUMDAT(IMOD,IMODX,JSEC,JSTA)
            IF( JSTA.EQ.1 .OR. JSTA.EQ.4 ) THEN
              NABC = NABC + 1
            ELSE IF( JSTA.EQ.2 .OR. JSTA.EQ.5 ) THEN
              NABC = NABC + 10
            ELSE IF( JSTA.EQ.3 .OR. JSTA.EQ.6 ) THEN
              NABC = NABC + 100
            END IF
            ISTTH = ISTTH + N_STTH
          END DO
          IHIT = NABC
C
        ELSE
          ISTTH = ISTTH + (IHIT-1)*N_STTH
          IADDR = IQ(ISTTH+1)
          IMOD = IADDR/256
          CALL MUMDAT(IMOD,IMODX,ISEC,ISTA)
          CALL UCOPY(Q(ISTTH+2),GEO,6)
          DIST = Q(ISTTH+8)
        ENDIF
      ENDIF
C
      RETURN
      END
