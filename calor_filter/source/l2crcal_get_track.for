      FUNCTION L2CRCAL_GET_TRACK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get all the MUON tracks from either ISAJET or
C-              from reconstructed MUON banks
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  31-MAY-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL L2CRCAL_GET_TRACK
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$INC:L2CRCAL_CONT.INC'
      INCLUDE 'D0$INC:L2CRCALS.INC'
      REAL ALPHA,DENOM
C ** GEANT VARIABLES:
C INPUT VARIABLES
      INTEGER ITRAK,LGCAH,GZGCAH,IPATH
C OUTPUT VARIABLES
      INTEGER ITRA
      REAL VERT(3,3),PG(4,2),TOTAL(4),PTOT
      INTEGER IDATA(8),NPOINT
C LOCAL VARIABLES
      INTEGER GZMUOT,LMUOT,IOFF
      CHARACTER*4 OLD_PATH
C----------------------------------------------------------------------
      CALL PATHGT(OLD_PATH)
C---Choose where to get the track. Of course for real data-taking, this will
C---be the MUON system.
      L2CRCAL_GET_TRACK = .FALSE.
      IF (MUON_TRACK_MODE(L2CRCAL_PARAM) .GT. 1) GOTO 3000
      GOTO 2000
C**********************************************************************
 2000 CONTINUE                          ! GEANT TRACK
      ITRAK = 1                         ! First possible track index
      NTRAK = 0
      LGCAH = GZGCAH(ITRAK)
      IF (LGCAH .LE. 0) GOTO 500
      CALL GTGCAH(ITRAK,ITRA,VERT,PG,IDATA,TOTAL,NPOINT)
      IF (ITRAK .LT. 0) GOTO 500         ! No more tracks
      NTRAK = NTRAK + 1
      ITRAK = ITRAK + 1
      XPART(NTRAK) = VERT(1,2)          !vertex at entry into cal
      YPART(NTRAK) = VERT(2,2)
      ZPART(NTRAK) = VERT(3,2)

      PTOT = SQRT(PG(1,2)**2 + PG(2,2)**2 + PG(3,2)**2) !P at cal entry
      IF (PTOT .GE. .1) THEN
        XCOS(NTRAK) =PG(1,2)/PTOT
        YCOS(NTRAK) =PG(2,2)/PTOT
        ZCOS(NTRAK) =PG(3,2)/PTOT
      END IF
      ITRAK = ITRA                      ! Actual track found
C      PRINT 998,XPART(NTRAK),YPART(NTRAK),ZPART(NTRAK),XCOS(NTRAK),
C     &   YCOS(NTRAK),ZCOS(NTRAK)
  500 CONTINUE
      GOTO 999
C**********************************************************************
 3000 CONTINUE                          ! Get it from MUOT bank
      NTRAK     = 0                     ! # of muon tracks
      DO IPATH = 1,2
        CALL PATHST('RECO')
        IF (IPATH .EQ. 2) CALL PATHST('FILT')
 3010   LMUOT = GZMUOT(NTRAK+1)           ! Get link to this track number
        IF (LMUOT .LE. 0) GOTO 3050       ! No more muon tracks
        NTRAK = NTRAK + 1
        SMSG  = ' TOO MANY TRACKS'
        MUMSG = ' Too many Muon Tracks. Not all will be looked at'
        IF (NTRAK .GT. NTRAK_MAX) GOTO 800
        MUFLAG( NTRAK ) =IQ( LMUOT + 3)   ! FLAG WORK 0=ABC,1=BC,2=AC,3=
C                                        ! AB,4=C
        IOFF = 0                          ! Take A layer stuff
        IF (MUFLAG(NTRAK) .EQ. 1 .OR. MUFLAG(NTRAK) .EQ. 4) IOFF = 3
        XPART( NTRAK ) = Q( LMUOT + IOFF + 5)    ! Coordinates at A layer.
        YPART( NTRAK ) = Q( LMUOT + IOFF + 6)
        ZPART( NTRAK ) = Q( LMUOT + IOFF + 7)
        XCOS(  NTRAK ) = Q( LMUOT + IOFF + 11)   ! Direction cosine inside magnet
        YCOS(  NTRAK ) = Q( LMUOT + IOFF + 12)
        ZCOS(  NTRAK ) = Q( LMUOT + IOFF + 13)
c---Now we depend on MUON_TRACK_MODE to tell us whether to keep these
C---direction cosines or to force them to point to the beam line.
        IF (MUON_TRACK_MODE(L2CRCAL_PARAM) .EQ. 1 .OR. MUON_TRACK_MODE(
     &  L2CRCAL_PARAM) .EQ. 3) THEN  ! Point to beam line
          DENOM = ABS(1. - ZCOS(NTRAK)**2) + 1.E-10
          ALPHA = SQRT( (XPART(NTRAK)**2 + YPART(NTRAK)**2)/DENOM)
          XCOS(NTRAK) = XPART(NTRAK)*ALPHA
          YCOS(NTRAK) = YPART(NTRAK)*ALPHA
        END IF
        GOTO 3010
 3050   CONTINUE
      END DO
      CALL PATHST(OLD_PATH)

      IF (NTRAK .LE. 0) GOTO 800

  999 L2CRCAL_GET_TRACK = .TRUE.
      RETURN
  800 L2CRCAL_GET_TRACK = .FALSE.
      CALL PATHST(OLD_PATH)
      RETURN
      END
