      SUBROUTINE GTJTSH(IJETS,PHI_WID,ETA_WID,ETFRAC,ISHARE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return JTSH data for given jet IJETS
C-
C-   Inputs  : IJETS    [I]     JETS bank number in linear chain
C-                              (begins at 1).
C-
C-   Outputs :  PHI_WID  [R]     Phi width
C-              ETA_WID  [R]     Eta width
C-              ETFRAC   [R]     Fraction of total Et in EM
C-              ISHARE   [I]     Energy shared? ( 0 = No )
C-              IER = error code, 0 = ok, -4 = no JETS bank, -5= no JTSH bank
C-   Controls:
C-
C-   Created  11-SEP-1991   Andrew Milder
C-   Updated  20-NOV-1991   Nick Hadley, Boaz Klima
C-                          Add testing for version 2 of JETS ( no JTSH! )
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IJETS,IVERS
      REAL    E(7),ETA_WID,PHI_WID
      REAL    THETA,PHI,ETA,ETFRAC
      INTEGER IER,ISHARE
C
      INTEGER NJETS,LJETS,LJETS_FIRST,JJETS,LJTSH
      CHARACTER*4 ENERGY_TYPE,PATH
      REAL    ENERGY_MAX,ENERGY
C
      INTEGER NZBANK
      INTEGER GZJETS
      INTEGER I,J,K,BASE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJTSH.LINK'
C----------------------------------------------------------------------
C
      CALL PATHGT(PATH)
      IF (PATH.EQ.'MDST') THEN
        CALL GTMDST_JETS(IJETS,IVERS,E,THETA,PHI,ETA,PHI_WID,
     &    ETA_WID,ETFRAC,ISHARE,IER)
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
C
C ****  Test if version 1 of JETS ( otherwise no JTSH )
C
        IF ( IQ(LJETS+1).GT.1 ) THEN
          IER = -5
          GOTO 999
        ELSE
          LJTSH = LQ(LJETS-IZJTSH)
          IF ( LJTSH .LE. 0 ) THEN
            IER = - 5
            GOTO 999
          ENDIF
          ETA_WID = Q(LJTSH+3)
          PHI_WID = Q(LJTSH+4)
          ETFRAC =  Q(LJTSH+5)
          ISHARE = IQ(LJTSH+10)
        ENDIF
      ENDIF
  999 RETURN
      END
