      FUNCTION STPCAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called by GUSTEP if there is a
C-                         hit in the Calorimeter. GSCHIT or DSCHIT
C-                         Stores the energy. GSCHIT is the "normal"
C-                         GEANT rorutine. DSCHIT is AMJ's Software
C-                         Tower routine
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  10-MAR-1986   Stephan Linn
C-   Updated  11-JUL-1986   A.M.Jonckheere
C-   Updated  13-SEP-1988   Rajendran Raja
C-   Updated  28-NOV-1988   Elliott A. Treadwell, Harrison B. Prosper
C-                          Added code to select general D0 SRCP bank
C-   Updated  14-JUL-1989   Harrison B. Prosper
C-   Made into logical function
C-   Updated  19-JUL-1989   John Womersley  shower library
C-   Updated   9-MAR-1992   Stuart Fuess   add IDTYPE dead/live digit
C-
C-   ---- Unrecorded updates -----
C-
C-   Updated  11-MAY-1992   K. Wyatt Merritt  Add code to flag bad 4-momentum
C-                          in CAL_P_START (believed to be caused by particles
C-                          that fall below the cutoff in the step which takes
C-                          them into the calorimeter).  In these cases, 
C-                          CAL_P_START is set to -9999. and the GCAH bank
C-                          should be dropped when creating the shower library,
C-                          but the kinetic energy of the particle should be
C-                          kept in the sum. 
C-   Updated  15-JUL-1992   K. Wyatt Merritt   Allow BAD_MOM flag only for
C-                          hadronic tracks.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL STPCAL
      LOGICAL BAD_MOM
C
      INTEGER IHIT,NUBUF
      REAL UBUF(10)
      REAL DEVOL
C EM ATTENUTATION FACTOR
      REAL EMATTN,EMA
      REAL R,CMASS
C
      INCLUDE 'D0$INC:GCSETS.INC'
      INCLUDE 'D0$INC:GCTRAK.INC'
      INCLUDE 'D0$INC:GCKINE.INC'
      INCLUDE 'D0$INC:GCKING.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:DCALOG.INC'
      INCLUDE 'D0$INC:GCVOLU.INC'
      INCLUDE 'D0$INC:CALTRK.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      INTEGER IDEAD,UCEC,LAYER
      INTEGER ILAST
C
C----------------------------------------------------------------------
      STPCAL = .TRUE.
      IF ( DCAL .LT. 2 ) GOTO 999
C
      IF (FIRST) THEN
        FIRST  = .FALSE.
        CALL EZPICK('SRCP_REST')  ! select general D0 geometry parameters
        CALL EZGSET('STPCAL_EM_ATTENUATION',EMATTN,1)  !EM attenuation factor
        CALL EZRSET    ! Reset to original SRCP bank
      ENDIF
C
C SHOULD WE BE IN HERE?
      IF ( DUCA.LE.1 .AND.
     &          DECA.LE.1 .AND.
     &                  DEAD.LE.1 ) GOTO 999
C
C ****  Unpack IDTYPE
      IDEAD = IDTYPE/10000
      UCEC = MOD(IDTYPE,10000)/1000
      LAYER = MOD(IDTYPE,1000)/10
C
C ****  Should we be here?
      IF ( UCEC.NE.3 .AND. UCEC.NE.4 ) GOTO 999
C
C ****  Geometric volumes - no CALTOWERS - dE/dx *only*
      IF ( .NOT. CALTOW ) THEN
        IF ( INWVOL.EQ.1 ) THEN
          DEVOL = 0.
        ELSE
          IF ( LINN_PARAM .AND. NGKINE.GT.0 ) CALL GOKING
C
C ****  Attentuate electrons (actually boost hadrons) - transition effect.
          EMA = EMATTN
          IF (  PLATE_GEOM .OR.                 ! Plate Geometry
     &         (IDEAD.EQ.1) .OR.                ! Dead material
     &         (ITRTYP.NE.4) .OR.               ! .NOT. Hadron
     &         (LAYER.GT.17) .OR.               ! DEAD material
     &         (LAYER.GE.8 .AND. LAYER.LE.10) ) ! MSG & ICD
     &                             EMA = 1.     ! no boost
          DEVOL = DEVOL + DESTEP/EMA
        ENDIF
C
        IF ( (INWVOL.EQ.2 .OR. ISTOP.NE.0) .AND.
     &          (DEVOL.NE.0) ) THEN
          CALL GSCHIT(ISET,IDET,ITRA,NUMBV,DEVOL,1,IHIT) ! Store hit
        ENDIF
C
C ****  Caltowers
      ELSEIF ( CALTOW ) THEN
C
C ****  Trace all the tracks on the stack, except for electrons and
C ****  positrons when reading from SHLB with SHWG=3
C
        IF ( NO_SHLB .OR. RAJA_SHLB) THEN
          IF (LINN_PARAM .AND. NGKINE.GT.0 ) CALL GOKING
C
C ****  Check if first step into calorimeter volume (FOR GCAH)
          IF ( CAL_PRIMARY.EQ.0 ) THEN
            CAL_PRIMARY = 1
            IF (RAJA_SHLB) THEN
              IF (IPART.EQ.2 .OR. IPART.EQ.3) THEN
                ISTOP = 1
                GO TO 999
              ENDIF
            ENDIF
            CALL UCOPY(VECT,CAL_XYZ_START,3)
            CAL_P_START(1) = VECT(4)*VECT(7)
            CAL_P_START(2) = VECT(5)*VECT(7)
            CAL_P_START(3) = VECT(6)*VECT(7)
            CAL_P_START(4) = GETOT
            CMASS = SQRT(AMAX1(.00000001,GETOT*GETOT - 
     &        CAL_P_START(1)**2 - CAL_P_START(2)**2 -
     &        CAL_P_START(3)**2))
            BAD_MOM = .FALSE.
            IF (AMASS .EQ. 0.) THEN
              IF (CMASS .GT. .0001) BAD_MOM = .TRUE.
            ELSE 
              R = CMASS/AMASS
              IF (R.LT.0.98 .OR. R.GT.1.02) BAD_MOM = .TRUE.
            ENDIF
          ENDIF
C
C ****  Check if PRIMARY TRACK LOSS IDENTITY (FOR GCAH)
          IF ( ISTOP.NE.0 ) THEN
            IF ( CAL_PRIMARY.EQ.1 ) THEN
              CAL_PRIMARY = 2
              CALL UCOPY( VECT, CAL_XYZ_INTERACT,3)
              IF ( ISTOP.EQ.1 ) THEN
C FIND INTERACTION TYPE
                CALL GET_TYPE_INTERACT(KCASE,CAL_TYPE_INTERACT)
              ELSE
                CAL_TYPE_INTERACT = 2
              ENDIF
              IF (BAD_MOM .AND. CAL_TYPE_INTERACT.EQ.2 .AND.
     &          ITRTYP.EQ.4) CAL_P_START(4) = -9999.
            ENDIF
          ENDIF
C
C ****  Attentuate electrons (actually boost hadrons) - transition effect.
          EMA = EMATTN
          IF (  PLATE_GEOM .OR.                 ! Plate Geometry
     &         (IDEAD.EQ.1) .OR.                ! Dead material
     &         (ITRTYP.NE.4) .OR.               ! .NOT. Hadron
     &         (LAYER.GT.17) .OR.               ! DEAD material
     &         (LAYER.GE.8 .AND. LAYER.LE.10) ) ! MSG & ICD
     &                             EMA = 1.     ! no boost
          DEVOL = DESTEP/EMA
C
C ****  Store if dE/dx .NE. 0 or in ICD
          IF ( DEVOL.NE.0 .OR. LAYER.EQ.9 ) THEN
            CALL DSCHIT(ITRA,VECT,IHSET,CHARGE,INWVOL,DEVOL) ! USE AMJ's Towers
          ENDIF
C
C ****  JW SHOWER LIBRARY
        ELSE IF (JW_SHLB) THEN
          IF (INWVOL.EQ.1) THEN        ! entering calorimeter
            IF ( IPART.GE.4 .AND. IPART.LE.6 ) GOTO 990  ! skip muons, neutrinos
C
            INWVOL=3
C
C ****  Check that track p > 250 MeV
            IF  (VECT(7).GT..25) THEN
              CALL SHCOPY                   ! copy hits from shower library
            ENDIF
C
            ISTOP = 1                       ! stop tracking
            DESTEP = 0.                     ! no energy loss since already
            DEVOL = 0.                      !   stored
            GETOT = 0.
            VECT(7)=0.                  ! zero the momentum
          ENDIF
C
  990     CONTINUE
        ENDIF
      ENDIF
C
  999 RETURN
      END
