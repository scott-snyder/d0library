      SUBROUTINE DLINFO(LDTRK,NZ,R,Z,EZ,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the Delay line information associated with
C-   LDTRK.
C-
C-   Inputs  : LDTRK = Address of DTRK bank
C-   Outputs : NZ = Number of delay line hits
C-             R(1-8) = Radial position of Delay line 1=inner, 8=outer
C-             Z(1-8) = For each R, the Z of delay line; 0 if no hit
C-             EZ(1-8) = For each Z, weight for the hit; 0 if none
C-             IER = Non zero if any errors
C-   Controls: 
C-
C-   Created  10-AUG-1993   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER   MAX_HIT               ! Max # of SW hits in CDC
      PARAMETER (MAX_HIT = 28)
C
      INTEGER   MAX_DL                ! Max # of DL hits in CDC
      PARAMETER (MAX_DL=8)
C
      INTEGER LDTRK,IER
      INTEGER NZ,IHIT,JHIT,NHIT
      INTEGER QUALITY(MAX_HIT),INDEX(MAX_DL)
C
      REAL    R(MAX_DL),Z(MAX_DL),EZ(MAX_DL)
      REAL    RADIUS(MAX_DL)
      REAL    HITX(MAX_HIT),HITY(MAX_HIT),HITZ(MAX_HIT)
      REAL    WR(MAX_HIT),WZ(MAX_HIT)
C
C Index contains the SW number for which there must be a delay line information.
C
      DATA INDEX /1,7,8,14,15,21,22,28/
C
C Nominal radial position of SW0/SW6 closest to delay line.
C
      DATA RADIUS /52.093, 55.913, 57.275, 61.095,
     &             62.491, 66.311, 67.733, 71.553/
C
C----------------------------------------------------------------------
C
C Init...
C
      NZ = 0
      CALL VZERO(R(1),8)
      CALL VZERO(Z(1),8)
      CALL VZERO(EZ(1),8)
C
C Fetch contents for this track from DTRK bank
C
      IF (LDTRK.LE.0) THEN
        IER = -1
        GO TO 999
      ENDIF
C
      CALL DTRKHITS(LDTRK,NHIT,HITX,HITY,HITZ,WR,WZ,QUALITY)
C
C Sort out Hit information, checking if delay line or not.
C Check quality of hit and require +z and -z to exist. Reject hit otherwise.
C Store sorted information in arrays R,Z and EZ.
C
      DO IHIT = 1,MAX_HIT
        IF (WZ(IHIT).NE.0.) THEN
          IF (QUALITY(IHIT).EQ.3) THEN
            IF (IHIT.LE.14) THEN
              JHIT = (IHIT/4) + 1
            ELSE
              JHIT = (IHIT-14)/4 + 5
            ENDIF
            IF (INDEX(JHIT).EQ.IHIT) THEN
              NZ = NZ + 1
              R(JHIT) = SQRT(HITX(IHIT)**2 + HITY(IHIT)**2)
C             R(JHIT) = RADIUS(JHIT)
              Z(JHIT) = HITZ(IHIT)
              EZ(JHIT) = WZ(IHIT)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C      
  999 RETURN
      END
