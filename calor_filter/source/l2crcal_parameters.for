      SUBROUTINE L2CRCAL_PARAMETERS (NEWPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize for L2CRCAL tool. Set logical
C-      flag -L2CRCAL_INIT_OK- depending on outcome.
C-
C-   Inputs  : NEWPAR : Number of parameter sets to read
C-   Outputs : None
C-   Controls: None
C-
C-   Created  20-AUG-90   by the L2STATE program
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:L2CRCAL_CONT.INC'
      INCLUDE 'D0$INC:L2CRCALS.INC'
C&IF VAXVMS,VAXELN
      BYTE NEWPAR
C&ELSE
C&      INTEGER NEWPAR
C&ENDIF
      INTEGER NEWPAR1
      LOGICAL L2J_UTIL_INIT             ! Init L2J_UTIL comon
      LOGICAL L2CR_READ                 ! Read L2CR bank into common
      INTEGER GZL2CR,GZSL2H,LSL2H,IER
      CHARACTER*16 MODID
C----------------------------------------------------------------------
      L2CRCAL_INIT_OK = .TRUE.          ! So far so good

C$$$ Temporary fix: vms_filter does not read in .stp file.
      IF (GZSL2H() .LE. 0) THEN
        CALL L2CRCAL_INIT
        GOTO 500
      ELSE IF (GZL2CR() .LE. 0) THEN
        CALL L2CRCAL_INIT
        GOTO 500
      END IF


C
C---Our L2CRCAL SRCP bank is hanging under SL2H. Declare it to SRCP and
C---read in values
C
      LSL2H = GZSL2H()                  ! Get link to L2 STP header
      IF (LSL2H .LE. 0) THEN
        MUMSG = ' Cant find SL2H bank '
        GOTO 800
      END IF
C---Tell RCP package that we have SRCP banks here.
      CALL EZNAME(' ',LSL2H,IZL2CRCAL_RCP)
      CALL EZERR(IER)
      IF (IER .NE. 0) THEN
        MUMSG = ' Could not declare RCP chain with EZNAME '
        GOTO 800
      END IF
  500 CONTINUE
C
C---Get L2CR bank and read its contents into L2CRCAL common. This common
C---holds CC geometry information and relations to cell indices.
C
      IF ( .NOT. L2CR_READ() ) THEN
        MUMSG = ' Could not read in L2CR correctly '
        GOTO 800
      END IF

C---Select bank
      CALL EZPICK('L2CRCAL')
      CALL EZERR(IER)
      IF (IER .NE. 0) THEN
        MUMSG = ' Could not find L2CRCAL bank '
        GOTO 800
      END IF
C---How many values will I be reading in?
      CALL EZGETA('DPHI_CUT',0,0,0,NEWPAR1,IER)
      IF (IER .NE. 0) THEN
        MUMSG = ' Cant get size of parameter array from EZGETA '
        SMSG  = ' EZGETA fail'
        MODID = 'L2CRCAL_PARAMETERS'
        GOTO 800
      END IF
C---Check to see if we must read in too many.
      IF (NEWPAR1 .GT. L2CRCAL_PARAM_MAX) THEN
        MUMSG = ' Tried to read in to many parameter sets '
        SMSG  = ' Too many P sets'
        MODID = ' L2CRCAL_PARAMETERS'
        GOTO 800
        END IF
C---Read values
      CALL EZGET_i('MUON_TRACK_MODE',MUON_TRACK_MODE,IER)
      IF (IER .NE. 0) THEN
        MUMSG = ' Could not read MUON_TRACK_MODE '
        GOTO 800
      END IF
      CALL EZGET('DPHI_CUT',DPHI_CUT,IER)
      IF (IER .NE. 0) THEN
        MUMSG = ' Could not read DPHI_CUT '
        GOTO 800
      END IF
      CALL EZGET_i('IMUHIT_CONE',IMUHIT_CONE,IER)
      IF (IER .NE. 0) THEN
        MUMSG = ' Could not read IMUHIT_CONE '
        GOTO 800
      END IF

C
C---Initialize L2J_UTIL common. This common holds some lookup arrays.
C
      IF (.NOT. L2J_UTIL_INIT() ) THEN
        MUMSG = ' Could not init L2J_UTIL common '
        GOTO 800
      END IF


      RETURN
  800 CONTINUE
      CALL EZRSET
      L2CRCAL_INIT_OK = .FALSE.
      CALL ERRMSG(SMSG,MODID,MUMSG,'W')
  999 RETURN
      END
