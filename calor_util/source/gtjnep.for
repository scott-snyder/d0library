      SUBROUTINE GTJNEP (IJNEP,IVERS,E,DIREC,FRAC_E,DIFF_DIREC,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return data for given JNEP bank number IJNEP
C-   and for its supporting JETS bank.  Use GTJNEP_TOTAL to get the number
C-   of JNEP banks.
C-
C-   Inputs  : IJNEP    [I]     JNEP bank number in linear chain
C-                              (begins at 1).
C-
C-   Outputs : IVERS        [I]     Bank version,
C-             E(7)         [R]     Ex,Ey,Ez,E,Et,sig**2(Ex),sig**2(Ey)
C-             DIREC(3)     [R]     Theta, phi, eta of jet center
C-             FRAC_E(7)    [R]     E(JNEP)/E(supporting JETS)
C-             DIFF_DIREC(3)[R]     DIREC(JETS) - DIREC(JNEP)
C-             IER      [I]     Error code; 0 --- OK
C-                              -4 --- No JNEP bank.
C-                              -5 --- Bad IJNEP value
C-   Controls:
C-
C-   Notes:
C-
C-   Call
C-
C-      GTJNEP_TOTAL (NUM_JNEP,IER)
C-
C-   to get the total number of JNEP banks.
C-
C-   Call
C-
C-      GTJNEP_MAX_ENERGY
C-      (ENERGY_TYPE, ENERGY, IJNEP, IER)       (Not yet implemented)
C-
C-   to return the IJNEP'th bank with the maximum energy or Et.
C-
C-   ENERGY_TYPE        'ET' For Et, 'EN' for ENergy
C-
C=======================================================================
C
C  Bank Name : JNEP
C  Author    : Dhiman Chakraborty
C  Date      : 21-NOV-1991
C  Tree description: PROC_TREE
C
C             JETS after exclusion of cells shared with PELC and PPHO
C
C    LQ     Q/IQ
C ___________________________________________________________________________
C     -1            struc. link to JPTR
C      0            next   link to none
C     +1            up     link to JETS
C     +2            origin link to JETS
C ............................................................................
C            I-5             bank number
C             -4             bank name,  'JNEP'
C             -3             NL=1
C             -2             NS=1
C             -1             ND=16
C              0             STATUS
C             +1     I       bank version (=1)
C             +2     F       Px
C             +3     F       Py
C             +4     F       Pz
C             +5     F       E
C             +6     F       Et
C             +7     F       Theta
C             +8     F       Phi
C             +9     F       Eta
C             +10    F       Sig**2(Ex)
C             +11    F       Sig**2(Ey)
C             +12    F       RMS Eta width
C             +13    F       RMS Phi width
C             +14    F       Fraction of EM Et = EM_ET/TOTAL_ET
C             +15    I       Flag for merging/splitting ( see below )
C             +16    F       Fraction E(JNEP)/E(JETS)
C
C=======================================================================
C
C  Definitions ( Cells are those in the CATE bank ):
C  -----------
C
C  E  - Sum(Ei) over all the cells included in the jet except those which
C       are also shared by electron/photon clusters.
C
C  Px - Sum(Exi).
C  Py - Sum(Eyi).
C  Pz - Sum(Ezi).
C
C  Et - Sum[(Ei)*sin(Thetai)].
C
C  For Theta, Phi and Eta the following code was used
C  to calculate them from Px,Py,Pz
C
C  PARAMETER( SMALL = 1.0E-5 )
C  Phi=ATAN2(Py,Px+SMALL)
C  IF(Phi.LT.0) Phi=Phi+TWOPI
C  EZOE=(Pz+SMALL)/(SQRT(Px*Px+Py*Py+Pz*Pz)+SMALL)
C  Theta=ACOS(EZOE)
C  Eta=-ALOG(TAN(Theta/2.)+SMALL)
C
C  Sig**2(Ex) - Sum[Sig**2(Exi)].
C
C  Sig**2(Ey) - Sum[Sig**2(Eyi)].
C
C  EM Et - Sum[(Ei)*sin(Thetai)] if cell I is in an EM calorimeter
C
C  RMS Eta WIDTH = SQRT[sum[Eti*(Etai - Eta_average)**2]/Et]
C  RMS Phi WIDTH = SQRT[sum[Eti*(Phii - Phi_average)**2]/Et]
C
C  Note:
C  ----
C
C  One assumes zero mass for each cell included in the jet. The invariant
C  mass of the jet is given by:
C
C       Jet Mass = SQRT [ E**2 - ( Px**2 + Py**2 + Pz**2 )]
C
C=======================================================================
C-
C-    Created  15-DEC-1991  Dhiman Chakraborty
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IJNEP
      INTEGER IVERS
      REAL    E(7), DIREC(3)
      REAL    FRAC_E(7),DIFF_DIREC(3)
      REAL    E_JETS(7)
C      REAL    ETA_WID,PHI_WID
      INTEGER IER,ISHARE
C
      INTEGER NJNEP,LJETS,LJNEP,JJNEP
      CHARACTER*4 ENERGY_TYPE,PATH
      REAL    ENERGY_MAX,ENERGY
C
      INTEGER GZJNEP
      INTEGER I,J,K
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C
C      CALL PATHGT(PATH)
C      IF (PATH.EQ.'MDST') THEN
C        CALL GTMDST_JNEP(IJETS,IVERS,E,THETA,PHI,ETA,PHI_WID,
C     &    ETA_WID,ETFRAC,ISHARE,IER)
C      ELSE
      CALL VZERO(E,7)
      CALL VZERO(FRAC_E,7)
      CALL VZERO(DIREC,3)
      CALL VZERO(DIFF_DIREC,3)
      IER = 0
      LJNEP = GZJNEP()
      IF ( LJNEP .LE. 0 ) THEN
        IER = - 4
        GOTO 999
      ENDIF
C
C ****  Locate IJNEP'th bank
C
      JJNEP = 1
      LJETS = LQ(LJNEP+1)
      DO WHILE ( LJETS .GT. 0 )
        IF ( IJNEP .EQ. JJNEP ) THEN
          GOTO 100
        ELSE
          LJETS = LQ(LJNEP+1)         ! Get supporting JETS bank address
   50     LJETS = LQ(LJETS)           ! Get next JETS bank address
          LJNEP = LQ(LJETS-2)         ! Get JNEP bank address
          IF ( LJNEP .GT. 0 ) JJNEP = JJNEP + 1
        ENDIF
      ENDDO
      IER = -5                        ! Bad IJETS
      GOTO 999
  100 CONTINUE
C
      IVERS= IQ(LJNEP+1)
      DO I =  1,5
        E(I) = Q(LJNEP+1+I)                ! Get energies and
        E_JETS(I) = Q(LJETS+1+I)
      ENDDO
      E(6) = Q(LJNEP+10)
      E(7) = Q(LJNEP+11)
      E_JETS(6) = Q(LJETS+10)
      E_JETS(7) = Q(LJETS+11)
      DO I = 1,7
        IF ( E_JETS(I) .NE. 0. )
     &     FRAC_E(I) = E(I)/E_JETS(I)        ! fractions w.r.t. JETS
      ENDDO
      DO I =  1,3
        DIREC(I) = Q(LJNEP+6+I)        ! Get direction and
        DIFF_DIREC(I) = Q(LJETS+6+I) - DIREC(I)
      ENDDO                              ! differences w.r.t. JETS
C      ENDIF
C
      RETURN
C
C ****************************************************
C ****  ENTRY point to get total number of JNEP banks
C ****************************************************
      ENTRY GTJNEP_TOTAL (NJNEP,IER)
C
C      CALL PATHGT(PATH)
C      IF (PATH.EQ.'MDST') THEN
C        CALL GTMDST_JNEP_TOTAL(NJNEP,IER)

C      ELSE
      NJNEP = 0
      LJNEP = GZJNEP()
      IF ( LJNEP .LE. 0) THEN
        IER = -4
      ELSE
        LJETS = LQ(LJNEP+1)
        DO WHILE ( LJETS .GT. 0 )
          IF ( LQ(LJETS-2) .GT. 0 ) NJNEP = NJNEP+1
          LJETS = LQ(LJETS)
        ENDDO
        IER = 0
      ENDIF
C      ENDIF
C
  999 RETURN
      END

