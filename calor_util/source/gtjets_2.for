      SUBROUTINE GTJETS_2(IJETS, IFLAG, N1GEV, ICD_FRAC, CH_FRAC,
     &  HOT_RAT, NCDC, N90, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return data for given JETS bank number IJETS.
C-   Use GTJETS_TOTAL to get number of JETS banks.
C-
C-
C-   Inputs  : IJETS    [I]     JETS bank number in linear chain
C-                              (begins at 1).
C-
C-   Outputs : IFLAG    [I]     Flag for splitting/merging
C-             N1GEV    [I]     N of cells above threshold (1gev def)
C-             ICD_FRAC [R]     Fraction of ICD/MG Et(EtICD+EtMG/TOTAL_ET)
C-             CH_FRAC  [R]     Fraction of CH Et (Et CH/TOTAL_ET)
C-             HOT_RAT  [R]     Ratio of hottest to next-hottest cell
C-             NCDC     [I]     Number of CDC tracks in jet cone
C-             N90      [I]     Number of TOWERS comprising 90% of jet Et
C-             IER      [I]     Error code; 0 --- OK
C-                              -4 --- No JETS bank.
C-                              -5 --- JNEP Et below Etmin
C-
C-   Call
C-        GTJETS_2B (SEED,PRECLU,ETNO,ETUL,ETIL)
C-        for seed, preclu, and underlying event energy
C-        !!! must be called after GTJETS_2 for valid information !!!
C-
C-            SEED    [R]     Jet Seed ET
C-            PRECLU  [R]     Jet Precluster ET                           
C-            ETNO    [R]     Estimated ET Noise in jet     
C-            ETUL    [R]     Estimated ET Underlying in jet
C-            ETIL    [R]     ICR Region Estimated ET Underlying in jet
C-
C-   Call
C-        GTJETS_2C (DEMF,DCHF,DICF,ERLO,ERHI)
C-        for energy added to EMF, CHF, ICF and ERLO+ERHI
C-        !!! must be called after GTJETS_2 for valid information !!!
C-
C-            DEMF    [R]     Change in EM fraction      
C-            DCHF    [R]     Change in CH fraction      
C-            DICF    [R]     Change in ICD fraction      
C-            ERLO    [R]     Error value of low systematic band 
C-                            ETLO=ETCOR*(1-ERLO)
C-            ERHI    [R]     Error value of high systematic band
C-                            ETHI=ETCOR*(1+ERHI)
C-
C-   Call
C-        GTJETS_2D (ET_JNEP)
C-
C-           ET_JNEP  [R]    ET from JNEP bank -999 if undefined -100 if no JNEP
C-
C-     Call GTJETS_2E (NV1,NV2)
C-           
C-           NV1  [I] Number of tracks from Vertex1 pointing to jet
C-           NV2  [I] Number of tracks from Vertex2 pointing to jet
C-
C-  Adapted from GTJETS 18-SEP-1993  Richard V. Astur
C   Updated 10-MAR-1994  Andrew G. Brandt Add CDC for V5
C                        Note MDST only has CDC for V>3
C-   Updated  26-SEP-1994   J. Yu  Fixed memory overwrite problem for
C-                                 N1GEV, N90, and NCDC.
C-   Updated  30-AUG-1995   Bob Hirosky   ADD GTJETS_2B ENTRY !currently
C-                                gets info not available in MDST
C-   Updated  07-NOV-1995   Andrew G. Brandt ADD GTJETS_2C ENTRY
C-   Updated  15-NOV-1995   Andrew G. Brandt ADD GTJETS_2D ENTRY
C-   Updated  13-MAR-1996   Brent May/Andrew Brandt added GTJETS_2E ENTRY
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IJETS
      INTEGER IVERS
      REAL    E(7),THETA,PHI,ETA
      INTEGER IER
C
      INTEGER IFLAG, N1GEV, NCDC, N90,JBYT
      REAL ICD_FRAC, CH_FRAC,HOT_RAT
      INTEGER LJETS,LJETS_FIRST,JJETS
      CHARACTER*4 PATH
C
      INTEGER LMDST,GZMDST
      INTEGER GZJETS,NUM_JETS,NREP,IOFF
      INTEGER BASE
      INTEGER IALGO,LJNEP,LCAPH
      REAL ETJNEP,ETMIN
      LOGICAL NOPE
C
      REAL SEED,PRECLU,ETNO,ETUL,ETIL
C
      REAL DEMF,DCHF,DICF,ERLO,ERHI,ESTAT(2),ESYST(2),ET_JNEP
C
      INTEGER NV1,NV2
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZJNEP.LINK'
      INCLUDE 'D0$INC:LKCAPH.INC'
C----------------------------------------------------------------------
C
      CALL PATHGT(PATH)
      CALL VZERO(E,7)
      IF (PATH.EQ.'MDST') THEN
        IER = 0
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
        IF( IJETS.GT.NUM_JETS ) THEN
          IER = -5
          GOTO 999
        ENDIF
        IOFF = NINT(Q(LCAPH))+(IJETS-1)*NREP-1
        LJETS = IOFF + LMDST
        BASE = LJETS
        IVERS= NINT(Q(BASE+1))
        IFLAG= NINT(Q(BASE+15))
        IF ( IVERS .GE. 5 ) THEN
          N1GEV= NINT(Q(BASE+16  ))
          NCDC = JBYT(IQ(BASE+27),1,8)
          N90  = NINT(Q(BASE+21  ))
        ELSE IF ( IVERS .GE. 4 ) THEN
          N1GEV= NINT(Q(BASE+16  ))
          NCDC = NINT(Q(BASE+20  ))
          N90  = NINT(Q(BASE+21  ))
        ELSE
          N1GEV = -1
          NCDC  = -1
          N90   = -1
        ENDIF
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
        BASE = LJETS
        IVERS= IQ(BASE+1)
C
C ****  Check for JNEP bank and return data from JNEP if it exists
C
        LJNEP = 0
        IF ( PATH .NE. 'MDST') LJNEP = LQ(LJETS-IZJNEP)
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
            GOTO 999
          END IF
          BASE = LJNEP
        END IF
        IVERS= IQ(BASE+1)
C
C ****  Fixed memory over write problem by
C ****  doing the followings (9/26/94 J.Yu)
C ****  N1GeV = NINT(Q(BASE+16)) => N1GEV=IQ(BASE+16)
C ****  NCDC  = NINT(Q(BASE+20)) => NCDC =IQ(BASE+20)
C ****  N90   = NINT(Q(BASE+21)) => N90  =IQ(BASE+21)
C
        IF ( IVERS .GE. 5 ) THEN
          N1GEV= IQ(BASE+16  )
          NCDC = JBYT(IQ(BASE+27),1,8)
          N90  = IQ(BASE+21  )
        ELSE IF ( IVERS .GE. 3 ) THEN
          N1GEV= IQ(BASE+16  )
          NCDC = IQ(BASE+20  )
          N90  = IQ(BASE+21  )
        ELSE
          N1GEV = -1
          NCDC  = -1
          N90   = -1
        ENDIF
        IFLAG= IQ(BASE+15)
      ENDIF
      IF ( IVERS.GE.3 ) THEN
        ICD_FRAC = Q( BASE + 17 )
        CH_FRAC  = Q( BASE + 18 )
        HOT_RAT  = Q( BASE + 19 )
      ELSE ! looks like old format
        ICD_FRAC = -999.
        CH_FRAC  = -999.
        HOT_RAT  = -999.
      ENDIF

  999 RETURN
C
C ****************************************************
C ****  ENTRY point to get SEED, PRECLU, AND UNDERLYING EVENT INFO
C !!!! GTJETS_2 MUST BE CALLED FIRST !!!!
C ****************************************************
      ENTRY GTJETS_2B (SEED,PRECLU,ETNO,ETUL,ETIL)
      IF ((PATH.EQ.'MDST').OR.( IVERS .LT. 7 )) THEN
        SEED = -999.
        PRECLU = -999.
        ETNO = -999.
        ETUL = -999.
        ETIL = -999.
      ELSE
        SEED = Q( BASE + 36 )
        PRECLU = Q( BASE + 39 )
        ETNO = Q( BASE + 30 )
        ETUL = Q( BASE + 31 )
        ETIL = Q( BASE + 38 )
      ENDIF
  998 RETURN
C
C ****************************************************
C ****  ENTRY point to get values for uncorrecting
C ****  and HI+LOW errors
C ****************************************************
      ENTRY GTJETS_2C (DEMF,DCHF,DICF,ERLO,ERHI)
      IF ((PATH.EQ.'MDST').OR.( IVERS .LT. 9 )) THEN
        DEMF = -999.
        DCHF = -999.
        DICF = -999.
        ERLO  = -999.
        ERHI  = -999.
      ELSE
        DEMF = Q( BASE + 33 )
        DCHF = Q( BASE + 46 )
        DICF = Q( BASE + 47 )
        ESTAT(1) = Q( BASE + 48 )
        ESTAT(2) = Q( BASE + 49 )
        ESYST(1) = Q( BASE + 50 )
        ESYST(2) = Q( BASE + 51 )
        ERLO=SQRT(ESTAT(1)**2+ESYST(1)**2)
        ERHI=SQRT(ESTAT(2)**2+ESYST(2)**2)
      ENDIF
  997 RETURN
C
C ****************************************************
C ****  ENTRY point to get JNEP ET
C ****************************************************
      ENTRY GTJETS_2D (ET_JNEP)
      ET_JNEP = -999.
      IF (PATH.EQ.'RECO'.AND.IVERS.GT.1) THEN
        LJNEP = LQ(LJETS-IZJNEP)
        IF(LJNEP.GT.0) THEN
          ET_JNEP = Q(LJNEP+6)
        ELSE
          ET_JNEP = -100.
        END IF
      END IF
  996 RETURN
C
C ****************************************************
C ****  ENTRY point to get number of tracks in jet from vertex 1,2
C ****  Must have called GTJETS_2 already
C ****************************************************
      ENTRY GTJETS_2E (NV1,NV2)
      NV1 = -1
      NV2 = -1
      IF (IVERS .GE. 5) THEN
        NV1 = JBYT(IQ(BASE+27),9,8)
        NV2 = JBYT(IQ(BASE+27),17,8)
      ENDIF

  995 RETURN
      END
