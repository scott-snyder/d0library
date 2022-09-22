      SUBROUTINE L2CRCAL
     &         (PARAM_SET_NUMBER,HARDWARE,RESULT_FLAG,EXTRA_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Cosmic Ray Calorimeter Tool: Point MUON track
C-                         into CC and return list of 'ideal' cells that were
C-                         hit. Go further and try to cluster around this
C-                         track.
C-
C-   Inputs  : PARAM_SET_NUMBER : Number in series of parameters to use.
C-             HARDWARE : Mask with bit set for Level-1 trigger which
C-                        started this filter.
C-   Outputs : RESULT_FLAG : Flag set to TRUE when TOOL wants event passed
C-             EXTRA_FLAG :  Not used
C-   Controls: None
C-
C-   Created  20-AUG-90   by the L2STATE program
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:L2CRCAL_CONT.INC'      ! L2CRCAL tool common
      INCLUDE 'D0$INC:L2CRCALS.INC'
      INTEGER PARAM_SET_NUMBER,HARDWARE
      LOGICAL RESULT_FLAG,EXTRA_FLAG
      LOGICAL OUT_OF_RANGE
      INTEGER LCRCA,LADD,ITRACK,I
C&IF VAXVMS,VAXELN,LINUX
      BYTE BADD(4)
      EQUIVALENCE(LADD,BADD)            ! Allow us to 'pack' LADD
C&ENDIF
      LOGICAL L2CRCAL_GET_TRACK         ! Get muon track direction
      LOGICAL L2CRCAL_TRACK_MUON        ! Which cells would it hit?
      LOGICAL L2CRCAL_CLUSTER           ! Cluster energy around track
      CHARACTER*16 MODID
      CHARACTER*4 OLD_PATH
C----------------------------------------------------------------------
C---Set path
      CALL PATHGT(OLD_PATH)
      CALL PATHST('FILT')
C---Get parameter set number
      L2CRCAL_PARAM = PARAM_SET_NUMBER
C
      RESULT_FLAG=.FALSE.               ! Assume the worse
C
C---We must first test to see that our initialization went okay.
C
      IF ( .NOT. L2CRCAL_INIT_OK ) THEN
        MODID = 'L2CRCAL'
        SMSG = 'INIT FAILED'
        MUMSG = ' L2CRCAL initialization failed: cannot proceed '
        GOTO 800
      END IF
C---First we see of MUON_L2 has returned any muon tracks. We will load them
C---into our common block.
      IF ( .NOT. L2CRCAL_GET_TRACK() ) THEN
        MODID = 'L2CRCAL_GET_TRACK'
        GOTO 800
      END IF
      IF ( NTRAK .LT. 1) THEN
        MODID = 'L2CRCAL_GET_TRACK'
        SMSG  = 'NO TRACKS'
        GOTO 800
      END IF
C---Now go through calorimeter and make list of cells hit in MUCA zebra
C---bank. We will choose the Muon track that is the 'best' (used the most
C---layers. To do this we look at MUFLAG
C********** FOR NOW, JUST TAKE ONE! ********************
      DO ITRACK  = 1,NTRAK
        IF (MUFLAG(ITRACK) .EQ. 0) GOTO 100        ! Got a good one
      END DO
  100 CONTINUE

      IF ( .NOT. L2CRCAL_TRACK_MUON(ITRACK,OUT_OF_RANGE)) THEN
        MODID = 'L2CRCAL_TRACK_MUON'
        GOTO 800
      END IF

C---Using our ideal list of 'hit' cells. Cluster around each hit using a
C---cone of radius = IMUHIT_CONE cells. Report clustered energy per layer
C---and total energy.
      IF (IMUHIT_CONE( L2CRCAL_PARAM) .GE. 0) THEN
        IF (.NOT. L2CRCAL_CLUSTER() ) THEN
          MODID = 'L2CRCAL_CLUSTER'
          GOTO 800
        END IF
      END IF

C---We have list of cells that our 'track' goes through. Store it in
C---CRCA zebra bank.
      CALL BKCRCA(NMCELL,LCRCA)       ! NMCELL is returned # of cells
      IQ(LCRCA + 1) = NMCELL
      IQ(LCRCA + 2) = IMUHIT_CONE(L2CRCAL_PARAM)  ! Cluster cone radius
      IQ(LCRCA + 3) = MUON_TRACK_MODE(L2CRCAL_PARAM)
      Q(LCRCA + 4) = DPHI_CUT(L2CRCAL_PARAM)
C                                     ! Maximum delta phi of track (mod pi)

      DO I = 1,NMCELL                 ! Cycle over cells
C---Pack the cell data in 4 bytes: highest byte is IETA,IPHI,ILAYER and
C---least significant byte is 0.
C&IF VAXVMS,VAXELN,LINUX
        BADD(4)  = IMUHIT_ETA(I)
        BADD(3)  = IMUHIT_PHI(I)
        BADD(2)  = IMUHIT_LYR(I)
        BADD(1)  = 0                    ! FOR NOW
C&ELSE
C&
C&ENDIF
C
        IQ(LCRCA + 2*(I-1) + 5) = LADD
        IF (IMUHIT_CONE(L2CRCAL_PARAM)  .GE. 0) Q(LCRCA + 2*(I-1) + 6) =
     &    MUHIT_E(I)
      END DO

      RESULT_FLAG = .TRUE.
      CALL PATHST(OLD_PATH)
      RETURN
  800 CONTINUE
      CALL ERRMSG(SMSG,MODID,MUMSG,'W')
      RESULT_FLAG = .FALSE.
      CALL PATHST(OLD_PATH)

  999 RETURN
      END
