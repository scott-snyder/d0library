      FUNCTION MATCH_JETS(LJET1,LJET2,NUMJET1,NUMJET2,NUMMATCH,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Try to match a given set of "jets" to another
C-   given set of "jets". The jets can be any linear chain of banks
C-   which contain the pair (Eta,Phi). For example, JETS, PJET, PELC, PPHO,
C-   or CACL. For example, use MATCH_JETS to match JETS to PJET, or
C-   PELC to JETS to find electron jets. MATCH_JETS uses the general
C-   utility MATCHJETS.
C-
C-      (A)
C-
C-   Use the entry point MATCH_JETS_BEGIN(IETA1,IPHI1,IETA2,IPHI2) to
C-   specify the position of ETA and PHI within the two sets of banks.
C-
C-   E.g. for JETS      IETA1 = 9       IPHI1 = 8
C-        for PJET      IETA2 = 10,     IPHI2 = 8
C-
C-      (B)
C-
C-   Use the entry point
C-
C-      LJET2 = GZ_MATCH_JETS(J,LJET1)
C-
C-   to return the address LJET2 of the "PJET" bank matched to the "JETS"
C-   bank "J". LJET1 is the address of the "JETS" bank.
C-   If GZ_MATCH_JETS returns zero then no match was found.
C-
C-      (C)
C-
C-      *** IMPORTANT ***
C-
C-   You should use MATCH_JETS_END to release the bank addresses
C-   when they are no longer needed. These addresses are stored in
C-   a temporary link area (/ZLINKA/).
C-
C-   Inputs  : LJET1    [I]     Address of 1st "JETS" bank
C-             LJET2    [I]     Address of 1st "PJET" bank
C-
C-   Outputs : NUMJET1  [I]     Number of "JETS"
C-             NUMJET2  [I]     Number of "PJET"
C-             NUMMATCH [I]     Number of matched jets
C-             IER              [I]     0 -- OK
C-
C-   Controls:
C-
C-   Created   4-JAN-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LJET1
      INTEGER LJET2
      INTEGER NUMJET1
      INTEGER NUMJET2
      INTEGER NUMMATCH
      INTEGER IER
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      INTEGER MATCH_JETS,GZ_MATCH_JETS,MATCH_JETS_BEGIN,MATCH_JETS_END
      INTEGER NPJET,NJET,PJET,JET,IJET,LADDR,LLJET,L,LJETS,LPJET
      INTEGER JETA1,JPHI1,JETA2,JPHI2
      INTEGER IETA1,IPHI1,IETA2,IPHI2
C
      INTEGER NMAX
      PARAMETER( NMAX = 50 )
C
      INTEGER PJETID(NMAX),JETID(NMAX)
      CHARACTER*8 PJETNAME(NMAX),JETNAME(NMAX)
C
      INTEGER PJETNUM(NMAX)
      REAL    PJET_ETA(NMAX),PJET_PHI(NMAX)
      REAL    JET_ETA(NMAX),JET_PHI(NMAX)
      REAL    DPHI,DETA
C----------------------------------------------------------------------
      LOGICAL FIRST,RESERVED
      SAVE FIRST,RESERVED,NJET,NPJET,PJETNUM,
     &  PJETID,PJETNAME,JETID,JETNAME
      DATA FIRST/.TRUE./
      DATA RESERVED/.FALSE./
C----------------------------------------------------------------------
      NUMJET1 = 0
      NUMJET2 = 0
      NUMMATCH= 0
      IER     = 0
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL INZLNK                     ! Setup /ZLINKA/
      ENDIF
C
C ****  Check pointer to "JETS" bank
C
      IF ( LJET1 .LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','MATCH_JETS','LJET1 is ZERO','W')
        IER = -1
        GOTO 999
      ENDIF
C
C ****  Check pointer to "PJET" bank
C
      IF ( LJET2 .LE. 0 ) THEN
        CALL ERRMSG('CALORIMETER','MATCH_JETS','LJET2 is ZERO','W')
        IER = -1
        GOTO 999
      ENDIF
C
C ****  If links reserved first release them
C
      IF ( RESERVED ) THEN
        DO JET = 1, NJET
          CALL RSLINK(JETNAME(JET),JETID(JET))
        ENDDO
        DO PJET = 1, NPJET
          CALL RSLINK(PJETNAME(PJET),PJETID(PJET))
        ENDDO
      ENDIF
      RESERVED = .TRUE.                 ! Links reserved
C
C ****  Get "JETS" parameters into local arrays
C
      LJETS   = LJET1
      NJET = 0
      DO WHILE ( LJETS .GT. 0 )
        IF ( NJET .LT. NMAX ) THEN
          NJET = NJET + 1
          JET_ETA(NJET) = Q(LJETS+ IETA1)
          JET_PHI(NJET) = Q(LJETS+ IPHI1)
C
C ****  Store address in temporary link area
C
          CALL STRINT('JETS',NJET,JETNAME(NJET),L)
          CALL GSLINK(JETNAME(NJET),JETID(NJET))
          LSLINK(JETID(NJET)) = LJETS
          LJETS = LQ(LJETS)
        ELSE
          CALL ERRMSG('CALORIMETER','MATCH_JETS',
     &      'Too many "JETS"','W')
          LJETS = 0
        ENDIF
      ENDDO
C
C ****  Get "PJET" parameters into local arrays
C
      LPJET   = LJET2
      NPJET = 0
      DO WHILE ( LPJET .GT. 0 )
        IF ( NJET .LT. NMAX ) THEN
          NPJET = NPJET + 1
          PJET_ETA(NPJET) = Q(LPJET+IETA2)
          PJET_PHI(NPJET) = Q(LPJET+IPHI2)
C
C ****  Store address in temporary link area
C
          CALL STRINT('PJET',NPJET,PJETNAME(NPJET),L)
          CALL GSLINK(PJETNAME(NPJET),PJETID(NPJET))
          LSLINK(PJETID(NPJET)) = LPJET
          LPJET = LQ(LPJET)
        ELSE
          CALL ERRMSG('CALORIMETER','MATCH_JETS',
     &      'Too many "PJET"','W')
          LPJET = 0
        ENDIF
      ENDDO
C
C ****  Match "JETS" to "PJET"
C
      CALL MATCHJETS
     &  (NJET,JET_ETA,JET_PHI,NPJET,PJET_ETA,PJET_PHI,PJETNUM)
C
C ****  Return number of jets
C
      NUMJET1  = NJET
      NUMJET2  = NPJET
      NUMMATCH = 0
      DO JET =  1, NJET
        IF ( PJETNUM(JET) .GT. 0 ) THEN
          NUMMATCH = NUMMATCH + 1
        ENDIF
      ENDDO
      RETURN
C
C ****  Return address of matched PJET
C
      ENTRY GZ_MATCH_JETS(IJET,LLJET)
      LADDR = 0
      LLJET = 0
      IF ( (IJET .GE. 1) .AND. (IJET .LE. NJET) ) THEN
        LLJET = LSLINK(JETID(IJET))
        IF ( PJETNUM(IJET) .GT. 0 ) THEN
          LADDR = LSLINK(PJETID(PJETNUM(IJET)))
        ENDIF
      ENDIF
      GZ_MATCH_JETS = LADDR
      RETURN
C
C ****  Setup MATCH_JETS
C
      ENTRY MATCH_JETS_BEGIN(JETA1,JPHI1,JETA2,JPHI2)
      IETA1 = JETA1
      IPHI1 = JPHI1
      IETA2 = JETA2
      IPHI2 = JPHI2
      RETURN
C
C ****  Release temporary link area
C
      ENTRY MATCH_JETS_END
      IF ( RESERVED ) THEN
        DO JET = 1, NJET
          CALL RSLINK(JETNAME(JET),JETID(JET))
        ENDDO
        DO PJET = 1, NPJET
          CALL RSLINK(PJETNAME(PJET),PJETID(PJET))
        ENDDO
      ENDIF
      RESERVED = .FALSE.
  999 RETURN
      END
