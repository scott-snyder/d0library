C DEC/CMS REPLACEMENT HISTORY, Element GTCACL.FOR
C *2    14-DEC-1989 15:13:23 HARRY "Nearest-Neignbor code update"
C *1    12-DEC-1989 15:14:57 HARRY "Get routines "
C DEC/CMS REPLACEMENT HISTORY, Element GTCACL.FOR
      SUBROUTINE GTCACL (ICACL,IVERS,E,THETA,PHI,ETA,X,Y,Z,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return data for given CACL bank number ICACL.
C-   Use GTCACL_TOTAL to get number of CACL banks.
C-
C-   Inputs  : ICACL    [I]     CACL bank number in linear chain
C-                              (begins at 1).
C-
C-   Outputs : IVERS(3) [I]     Bank version,
C-                              cluster-number,
C-                              cluster-type
C-             E(7)     [R]     Ex,Ey,Ez,E,Et,sig**2(Ex),sig**2(Ey)
C-             THETA    [R]     Theta of cluster centre
C-             PHI      [R]     Phi of cluster centre
C-             ETA      [R]     Eta of cluster centre
C-             X,Y,Z    [R]     Cartesian coordinate of cluster centre.
C-             IER      [I]     Error code; 0 --- OK
C-                              -4 --- No CACL bank.
C-   Controls:
C-
C-   Notes:
C-
C-   Call
C-
C-      GTCACL_TOTAL (NUM_CACL,IER)
C-
C-   to get the total number of CACL banks.
C-
C-   Call
C-
C-      GTCACL_MAX_ENERGY
C-      (ENERGY_TYPE, ENERGY, ICACL, IER)
C-
C-   to return the ICACL'th bank with the maximum energy or Et.
C-
C-   ENERGY_TYPE        'ET' For Et, 'EN' for ENergy
C-
C=======================================================================
C
C  Bank name: CACL
C  Author   : Serban D. Protopopescu, Rajendran Raja
C  Date     : Nov. 23,1988
C  Tree description: PROC_ZEBANKS
C
C             Calorimeter energy cluster bank
C
C    LQ     Q/IQ
C ___________________________________________________________________________
C     -4            ref. (MUOT)  muon track
C     -3            ref. (JETS)
C     -2            ref. to connected CACL
C     -1            struct.  (CACH) pointers to cal. hits
C      0            next     to next CACL
C     +1            up       to PROC
C     +2            origin   to PROC for first and previous CACL for others
C ............................................................................
C            I-5             bank number
C             -4             bank name,  'CACL'
C             -3             NL=4
C             -2             NS=1
C             -1             ND=16
C              0             STATUS
C             +1     I       bank version (=1)
C             +2     I       cluster number
C             +3     I       type (1 for em, 2 for hadr., 3 for CC/EC region)
C             +4     F       Ex
C             +5     F       Ey
C             +6     F       Ez
C             +7     F       E
C             +8     F       Et
C             +9     F       sig**2(Ex)
C             +10    F       sig**2(Ey)
C             +11    F       Theta
C             +12    F       phi
C             +13    F       eta
C             +14    F       X of Center of energy
C             +15    F       Y of center of energy
C             +16    F       Z of center of energy
C=======================================================================
C Note: The Center of Energy is obtained by giving a weight
C to each cell of Energy**Power, where the Power is adjusted to
C get the closest to the true center. If Power = 1, True center of
C energy is obtained.
C-
C-   Created   12-OCT-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER ICACL

      INTEGER IVERS(*)
      REAL    E(7)
      REAL    THETA,PHI,ETA,X,Y,Z
      INTEGER IER
C
      INTEGER NCACL,LCACL,LCACL_FIRST,JCACL
      CHARACTER*4 ENERGY_TYPE
      REAL    ENERGY_MAX,ENERGY
C
      INTEGER NZBANK
      INTEGER GZCACL
      INTEGER I,J,K,BASE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C
      IER = 0
      LCACL_FIRST = GZCACL()
      IF ( LCACL_FIRST .LE. 0 ) THEN
        IER = - 4
        GOTO 999
      ENDIF
C
C ****  Locate ICACL'th bank
C
      LCACL = LCACL_FIRST
      JCACL = 1
      DO WHILE ( LCACL .GT. 0 )
        IF ( ICACL .EQ. JCACL ) THEN
          GOTO 100
        ELSE
          LCACL = LQ(LCACL)             ! Get next CACL bank address
          JCACL = JCACL + 1
        ENDIF
      ENDDO
  100 CONTINUE
C
      IF ( LCACL .GT. 0 ) THEN
        BASE = LCACL
        DO I =  1,3
          IVERS(I) = IQ(BASE+I)                ! Get energies
        ENDDO
        BASE = BASE + 3
        DO I =  1,7
          E(I) = Q(BASE+I)                ! Get energies
        ENDDO
        BASE = BASE + 7
        THETA= Q(BASE+1)
        PHI  = Q(BASE+2)
        ETA  = Q(BASE+3)
        X    = Q(BASE+4)
        Y    = Q(BASE+5)
        Z    = Q(BASE+6)
      ELSE
        IER =-3                         ! bad ICACL
      ENDIF
C
      RETURN
C
C ****************************************************
C ****  ENTRY point to get total number of CACL banks
C ****************************************************
      ENTRY GTCACL_TOTAL (NCACL,IER)
C
      LCACL_FIRST = GZCACL()
      IF ( LCACL_FIRST .GT. 0 ) THEN
        NCACL = NZBANK(IXCOM,LCACL_FIRST)
        IER = 0
      ELSE
        NCACL = 0
        IER =-4
      ENDIF
C
  999 RETURN
      END
