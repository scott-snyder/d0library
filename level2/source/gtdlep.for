      SUBROUTINE GTDLEP(NTRACK,PHI,ETA,ZVTX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the number of tracks found in the CDC
C-                         r-phi view, from DLEP filter result bank and
C-                         the parameters associated with each track.
C-
C-   Inputs  : none
C-   Outputs : NTRACK = Number of tracks found in r-phi view
C-             PHI(*) = Phi half-sector number ranging from 0-63.
C-             ETA(*) = Eta of the track
C-             ZVTX(*) = Z vertex position of the track.
C-      
C-   Controls: none
C-
C-   Created  20-APR-1991   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZDLEP.LINK'
C
      INTEGER LSUP,LDLEP,GZFRES
      INTEGER NPHI,NTRACK
C
      REAL PHI(*),ETA(*),ZVTX(*)
C
C----------------------------------------------------------------------
C
      NTRACK = 0
      LSUP = GZFRES()
      IF (LSUP.LE.0) THEN
        CALL INTMSG(' Filter result bank FRES not found ')
        GO TO 999
      ENDIF
C
      LDLEP = LQ(LSUP - IZDLEP)
      IF (LDLEP.LE.0) THEN
        CALL INTMSG(' Filter result bank DLEP not found ')
        NTRACK = 0
        GO TO 999
      ENDIF
C
      NTRACK = IQ(LDLEP + 1)
      DO NPHI = 1,NTRACK
        ZVTX(NPHI) = Q(LDLEP + 3*NPHI - 1)
        PHI(NPHI)  = Q(LDLEP + 3*NPHI)
        ETA(NPHI)  = Q(LDLEP + 3*NPHI + 1)
      ENDDO
  999 RETURN
      END
