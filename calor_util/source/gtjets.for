      SUBROUTINE GTJETS (IJETS,IVERS,E,THETA,PHI,ETA,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return data for given JETS bank number IJETS.
C-   Use GTJETS_TOTAL to get number of JETS banks.
C-
C-   Inputs  : IJETS    [I]     JETS bank number in linear chain
C-                              (begins at 1).
C-
C-   Outputs : IVERS    [I]     Bank version,
C-             E(7)     [R]     Px,Py,Pz,E,Et,Sigma,EMfraction
C-             THETA    [R]     Theta of jet centre
C-             PHI      [R]     Phi of jet centre
C-             ETA      [R]     Eta of jet centre
C-             IER      [I]     Error code; 0 --- OK
C-                              -4 --- No JETS bank.
C-                              -5 --- JNEP Et below Etmin
C-
C-   Controls:
C-
C-   Notes:
C-           Look at D0$ZEB$PROC:JETS.ZEB for details of the JETS bank
C-
C-   Call
C-
C-      GTJETS_TOTAL (NUM_JETS,IER)
C-
C-   to get the total number of JETS banks.
C-          NUM_JETS  [I]  Number of JETS banks
C-          IER       [I]  0 = ok
C-                        -4 = Algorithm CAPH not found
C-                        -13= no MDST bank and path is set to 'MDST'
C
C-
C-   Call
C-
C-      GTJETS_MAX_ENERGY
C-      (ENERGY_TYPE, ENERGY, IJETS, IER)       (Not yet implemented)
C-
C-   to return the IJETS'th bank with the maximum energy or Et.
C-
C-   ENERGY_TYPE        'ET' For Et, 'EN' for ENergy
C-
C-   Call
C-
C-     GTJETS_NOEP
C-
C-     to set a flag to exclude electrons/photon jets.  This entry
C-     tells GTJETS to check if the
C-     cluster survives the minimum Et cut even if cells shared with
C-     any identified electron/photon are excluded.  If it does, it
C-     returns quantities calculated from the cells which are not
C-     shared with any electron/photon.
C-     Caution:  Anyone analyzing data which has version 1 of JETS
C-     bank must run it through CAJNEP package first so JETS bank
C-     version is updated to 2, GTJETS_NOEP will act just like GTJETS
C-     otherwise. GTJETS_TOTAL is not sensitive to the NJEP switch.
C-
C-   Call
C-
C-      GTJETS_NOEP_RESET
C-
C-   to reset the flag so that GTJETS does not use JNEP information.
C-
C-
C-   Created  11-JAN-1990   Harrison B. Prosper
C-   Updated  19-JUN-1991   Andrew J. Milder
C-                          Modified for MicroDST format -- checks PATH
C-   Updated  20-NOV-1991   Nick Hadley, Boaz Klima
C-      No ZEB file in comments, add Sigma and EMfraction TO E(7)
C-   Updated   8-FEB-1992   Chip Stewart  - GTJETS_NJEP & GTJETS_NJEP_OFF
C-   Updated  19-DEC-1992   Andrew J. Milder  New MDST format 
C-   Modified 18-SEP-1993   R. Astur "Make offical and mdst compatible",
C-   Updated  15-OCT-1993   Marc Paterno  Several small bug fixes. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJNEP.LINK'
      INCLUDE 'D0$INC:LKCAPH.INC'
C
      INTEGER IJETS
      INTEGER IVERS
      REAL    E(7),ETA_WID,PHI_WID
      REAL    THETA,PHI,ETA,EMFRAC
      INTEGER IER,ISHARE
C
      INTEGER NJETS,LJETS,LJETS_FIRST,JJETS
      CHARACTER*4 PATH
C
      INTEGER NZBANK,LMDST,GZMDST
      INTEGER GZJETS,NUM_JETS,NREP,IOFF
      INTEGER I,BASE
      INTEGER IALGO,LJNEP,LCAPH, LVCOR
      REAL ETJNEP,ETMIN
      LOGICAL NOPE
      LOGICAL GET_UNCORRECTED
      SAVE    GET_UNCORRECTED
      SAVE    NOPE
      DATA    NOPE  /.FALSE./
      DATA GET_UNCORRECTED /.FALSE./
C----------------------------------------------------------------------
C
      CALL PATHGT(PATH)
      CALL VZERO(E,7)
      IF (PATH.EQ.'MDST') THEN
        IER = 0
        LMDST = GZMDST()
        IF (LMDST.LE.0) THEN
          IER = -4
          GOTO 998
        ENDIF
C
        LCAPH = JCAPH
        IF( LCAPH.LE.0 ) THEN
          IER = -4
          GOTO 998
        ENDIF
        NUM_JETS = NINT(Q(LCAPH+3))
        NREP = NINT(Q(LCAPH-1))
        IF( IJETS.GT.NUM_JETS ) THEN
          IER = -5
          GOTO 998
        ENDIF
        IOFF = NINT(Q(LCAPH))+(IJETS-1)*NREP-1
        LJETS = IOFF + LMDST
        BASE = LJETS
        IVERS= NINT(Q(BASE+1))
      ELSE
        IER = 0
        LJETS_FIRST = GZJETS()
        IF ( LJETS_FIRST .LE. 0 ) THEN
          IER = - 4
          GOTO 998
        ENDIF
C
C ****  Locate IJETS'th bank
C
        LJETS = LJETS_FIRST
        JJETS = 1
        DO WHILE ( LJETS .GT. 0 )
          IF ( IJETS .EQ. JJETS ) THEN
            GOTO 100
          ELSE
            LJETS = LQ(LJETS)             ! Get next JETS bank address
            JJETS = JJETS + 1
          ENDIF
        ENDDO
        IER = -5                        ! Bad IJETS
        GOTO 998
  100   CONTINUE
C
        BASE = LJETS
        IVERS= IQ(BASE+1)
C
C ****  Check for JNEP bank and return data from JNEP if it exists
C
        LJNEP = LQ(LJETS-IZJNEP)
        IF (NOPE.AND.(LJNEP.GT.0)) THEN
          IF ( IVERS .EQ. 1 ) THEN
            CALL ERRMSG ('INVALID VERSION','GTJETS',
     &        'USE_JNEP not enabled for version 1 of JETS bank','W')
            NOPE = .FALSE.
            GOTO 100
          END IF
          LCAPH = LQ(LJETS+1)
          IALGO = IQ(LCAPH+4)
          IF( IALGO .EQ. 2) THEN
            ETMIN = Q(LCAPH+7)
          ELSE
            ETMIN  = Q(LCAPH+11)
          ENDIF
          ETJNEP = Q(LJNEP+6)
          IF ( ETJNEP .LT. ETMIN ) THEN  !kill Jet if JNEP below Et min
            IER = -5
            CALL VZERO(E,7)
            THETA = -100
            ETA = -100
            PHI = -100
            GOTO 998
          END IF
          BASE = LJNEP
        END IF
        IVERS= IQ(BASE+1)
      ENDIF
C
C: Fill jet kinematic variables
C
      DO I =  1,5
        E(I) = Q(BASE+I+1)                ! Get energies
      ENDDO
      THETA= Q(BASE+7)
      PHI  = Q(BASE+8)
      ETA  = Q(BASE+9)
C
C: Check version number to get shape information
C
      IF ( IVERS.GT.1 ) THEN
        E(6) = SQRT ( Q(BASE+12)**2 + Q(BASE+13)**2 )
        E(7) = Q(BASE+14)
      ELSE ! looks like old format
        CALL GTJTSH(IJETS,PHI_WID,ETA_WID,EMFRAC,ISHARE,IER)
        IF (IER.EQ.0) THEN
          E(6) = SQRT(PHI_WID**2 + ETA_WID**2)
          E(7) = EMFRAC
        ENDIF
      ENDIF
C
C: If the want uncorrected energies, get them if there is a VCOR
C
      IF ( GET_UNCORRECTED ) THEN       ! Get the uncorrected energies
      IF ( IVERS .LT. 5 ) THEN
        GOTO 998                        ! No Energy corrections done
      ELSEIF ( IQ( BASE + 26 ) .GE. 0 ) THEN
        GOTO 998                        ! Bit 31 is OFF, no correction done
      ELSEIF ( LQ( BASE - 7 ) .EQ. 0 ) THEN
        IER = -15                       ! No VCOR bank. Cant get the
        GOTO 998                        ! uncorrected energies
      ELSE
        LVCOR = LQ( BASE - 7 )
        E(5) = (1. - Q(LVCOR+2+4)/E(4) )*E(5)
        DO I = 1, 4
          E(I) = E(I) - Q( LVCOR + 2 + I )
        ENDDO
      ENDIF
      ENDIF
C
  998 GET_UNCORRECTED = .FALSE.       ! RESET
      RETURN
C
C ****************************************************
C ****  ENTRY point to get total number of JETS banks
C ****************************************************
      ENTRY GTJETS_TOTAL (NJETS,IER)
C
      CALL PATHGT(PATH)
      IF (PATH.EQ.'MDST') THEN
        LCAPH = JCAPH
        IF( LCAPH.LE.0 ) THEN
          IER = -4
          NJETS = 0
        ELSE
          IER = 0
          NJETS = NINT(Q(LCAPH+3))
        ENDIF
      ELSE
        IF ( JCAPH .LE. 0 ) THEN
          IER = -4
          NJETS = 0
        ELSE
          LJETS_FIRST = GZJETS()
          IF ( LJETS_FIRST .GT. 0 ) THEN
            NJETS = NZBANK(IXCOM,LJETS_FIRST)
            IER = 0
          ELSE
            NJETS = 0
            IER =0
          ENDIF
        ENDIF
      ENDIF
C
      RETURN
C **********************************************************************
C ***  Entry point to get only those jets which survive the standard ***
C ***  Et cut even after the cells shared by identified electrons/   ***
C ***  photons are excluded from them                                ***
C **********************************************************************
      ENTRY GTJETS_NOEP
      NOPE = .TRUE.
      RETURN
C
C ****************************************************
C ****  ENTRY point to get total number of JETS banks
C ****************************************************
      ENTRY GTJETS_JNEP_RESET
      NOPE = .FALSE.
      RETURN

C*************************************************
C Entry point to get Uncorrected energy of jet
C************************************************
      ENTRY GTJETS_UNCORRECTED
      GET_UNCORRECTED = .TRUE.
      RETURN

      END
