      SUBROUTINE GTJETS_ALL (IJETS,IVERS,E,THETA,PHI,ETA,
     &  SIG,EMFRAC,ISPL,NCELL,HOT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return data for given JETS bank number IJETS.
C-
C-   Inputs  : IJETS    [I]     JETS bank number in linear chain
C-                              (begins at 1).
C-
C-   Outputs : IVERS    [I]     Bank version,
C-             E(5)     [R]     Px,Py,Pz,E,Et
C-             THETA    [R]     Theta of jet center
C-             PHI      [R]     Phi of jet center
C-             ETA      [R]     Eta of jet center
C-             SIG(4)   [R]     SigmaX,SigmaY,EtaWidth,PhiWidth,
C-             EMFRAC   [R]     EMfraction
C-             ISPL     [I]     split/merge flag
C-             NCELL    [I]     Num Cells > 1 GeV
C-             HOT(3)   [R]     ICDfrac,CHfrac,CellRatio
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
C
C-   Created  19-DEC-1992   Andrew J. Milder  Based on GTJETS, but will
C-        Retrieve all JETS info, modified for new MDST format. 
C-        JNEP machinery removed.
C-   Modified 22-SEP-1993   R. Astur - Protect against IJETS .LT. 1
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IJETS,ISPL
      INTEGER IVERS,LMDST,GZMDST,NREP,IOFF
      REAL    E(*),ETA_WID,PHI_WID,SIG(*),HOT(*)
      REAL    THETA,PHI,ETA,EMFRAC
      INTEGER IER,ISHARE,NUM_JETS,NCELL
C
      INTEGER NJETS,LJETS,LJETS_FIRST,JJETS
      CHARACTER*4 PATH
C
      INTEGER GZJETS
      INTEGER I,J,K,LCAPH
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LKCAPH.INC'
C----------------------------------------------------------------------
C
      CALL PATHGT(PATH)
      IF (PATH.EQ.'MDST') THEN
        LMDST = GZMDST()
        IF (LMDST.LE.0) THEN
          IER = -4
          GOTO 999
        ENDIF
C
        LCAPH = JCAPH
        IF( LCAPH.LE.0 ) THEN
          IER = -4
          GOTO 999
        ENDIF
        NUM_JETS = NINT(Q(LCAPH+3))
        NREP = NINT(Q(LCAPH-1))
        IF( IJETS.GT.NUM_JETS .OR. IJETS .LT. 1 ) THEN
          IER = -5
          GOTO 999
        ENDIF
        IOFF = NINT(Q(LCAPH)) + (IJETS-1)*NREP-1
        LJETS = IOFF + LMDST
        IVERS= NINT(Q(LJETS+1))
        ISPL = NINT(Q(LJETS+15))
        IF ( IVERS.GT.2 ) NCELL = NINT(Q(LJETS+16))
      ELSE
        IER = 0
        LJETS_FIRST = GZJETS()
        IF ( LJETS_FIRST .LE. 0 ) THEN
          IER = - 4
          GOTO 999
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
        GOTO 999
  100   CONTINUE
        IVERS= IQ(LJETS+1)
        ISPL = IQ(LJETS+15)
        IF ( IVERS.GT.2 ) NCELL = IQ(LJETS+16)
      ENDIF
      DO I =  1,5
        E(I) = Q(LJETS+I+1)                ! Get energies
      ENDDO
      THETA= Q(LJETS+7)
      PHI  = Q(LJETS+8)
      ETA  = Q(LJETS+9)
      IF ( IVERS.GT.1 ) THEN
        SIG(1) = Q(LJETS+10)
        SIG(2) = Q(LJETS+11)
        SIG(3) = Q(LJETS+12)
        SIG(4) = Q(LJETS+13)
      ELSE
        SIG(1) = 0.
        SIG(2) = 0.
        SIG(3) = 0.
        SIG(4) = 0.
      ENDIF
      EMFRAC = Q(LJETS+14)
      IF ( IVERS.GT.2 ) THEN
        HOT(1) = Q(LJETS+17)
        HOT(2) = Q(LJETS+18)
        HOT(3) = Q(LJETS+19)
      ELSE
        NCELL =  0
        HOT(1) = 0.
        HOT(2) = 0.
        HOT(3) = 0.
      ENDIF
C
  999 CONTINUE
      RETURN
      END
