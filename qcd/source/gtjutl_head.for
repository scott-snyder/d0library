C----------------------------------------------------------------------
      SUBROUTINE GTJUTL_HEAD
     & (IVERS,FILTM,FILTM2,SLOWZ,SLOWF,MINTF,FASTZ,FASTF,
     &  TKVZ,TKVDZ,TOOL,L0Q,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To unpack the JUTL bank. If path not set to
C-    'MDST' it will first fill the JUTL bank.
C-    If the passed L1JET and L2JET arrays is not big enough, it will
C-    just overwrite and/or crash.
C-
C-   Inputs  : none
C-   Outputs for GTJUTL_HEAD :
C-             IVERS              (I)    version number
C-             FILTM              (I)    QCD filter bit mask
C-             FILTM2             (I)    QCD2 filter bit mask
C-             SLOWZ              (R)    L0 slow vertex z
C-             SLOWF              (I)    L0 slow good z flag (1/0 = T/F)
C-             MINTF              (I)    Multiple interaction flag (L0) (1-4)
C-             FASTZ              (R)    L0 fast vertex z
C-             FASTF              (I)    L0 fast good z flag (1/0 = T/F)
C-             TKVZ(3)            (R)    Tracking vertex z
C-             TKVDZ(3)           (R)    Tracking vertex delta z
C-             TOOL               (I)    Multiple interaction tool (1-4)
C-             L0Q                (R)    L0 quality
C-             IER                (I)    0 is good
C-   Outputs  for GTJUTL_JETS :
C-             NL1                (I)    Number of L1 jet candidates
C-             L1JET ( 3,NL1 )   (R)    eta, phi, ET of L1 jets
C-             NL2                (I)    Number of L2 jet candidates
C-             L2JET ( 5,NL2 )   (R)    eta,phi,Et,width,emf of L2 jets
C-             CSIZ               (R)    Cone size for L2 jets
C-             IER                (I)    0 is good
C-   Controls:
C-          Call GTJUTL_HEAD
C-          (FILTM,FILTM2,SLOWZ,SLOWF,MINTF,FASTZ,FASTF,
C-          TKVZ,TKVDZ,TOOL,L0Q,IER)
C-          to get vertex/filter bit mask information.
C-
C-             Call GTJUTL_JETS( JRUN, NL1,L1JET,NL2,L2JET,CSIZ,IER )
C-             to get list of L1 and L2 jet candidates.
C-             JRUN=1 for 1A  2 for 1B
C-   Created  30-DEC-1992   Lars Rasmussen
C-   Modified 12-JAN-1993   Andrew Brandt  incorporated changes in JUTL bank
C-   Modified 25-JUN-1993   Andrew Brandt  NLi<NLiMAX add QCD_JUTL_JETS.INC
C-   Updated  24-SEP-1993   Andrew G. Brandt save MITOOL L0QUAL
C-   Updated  18-NOV-1993   Andrew G. Brandt fixed occasional CSIZ bug
C-   Updated  01-MAR-1994   Andrew G. Brandt Update for JUTL V4,5
C-   Updated  10-MAR-1994   Andrew G. Brandt No EVTQM save IVERS add JRUN
C-   Updated  21-MAR-1994   Andrew G. Brandt Add D0MDST code
C-   Updated  03-NOV-1994   Andrew G. Brandt Convert to CW allow TT+LJ
C-   Updated  30-JAN-1996   Andrew G. Brandt LZFIDH to GZJUTL
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
C need for NMAX
      INCLUDE 'D0$INC:QCD_JUTL_JETS.INC/LIST'
C
      INTEGER GZJUTL
C
      INTEGER  IVERS,FILTM,FILTM2,SLOWF,MINTF,FASTF,IER,TOOL
      REAL     SLOWZ, FASTZ, TKVZ(3), TKVDZ(3), L0Q
      INTEGER  NTTS, NLJS, NL2S, JRUN
      REAL     TTJET(3,*), LJJET(3,*), L2JET(5,*), CSIZ
C
      INTEGER      I, LJUTL, PNTR, NHEADR, PNTRFX, IVERSV
      SAVE IVERSV
      SAVE NHEADR
      CHARACTER*4  PATH
C----------------------------------------------------------------------
C- Reset stuff
C
      LJUTL = 0
      IVERS = -999
      FILTM = -999
      FILTM2= -999
      SLOWZ = -999.
      SLOWF = -999
      MINTF = -999
      FASTZ = -999.
      FASTF = -999
      DO I = 1,3
         TKVZ(I)  = -999.
         TKVDZ(I) = -999.
      END DO
      L0Q  = -999.
      TOOL = -999
C
C- Find the JUTL bank. It's not obvious that the following is the
C- right way to do it. If the path is set to 'MDST' and JUTL is not
C- filled, it's an error condition. If it's not a 'MDST' case, we
C- will fill the bank if JUTL not already filled. We guess that JUTLFL
C- is doing the right thing if it's not a 'MDST' case.
C-
C- (this piece of code is repeated at the GTJUTL_JETS entry)
C
      CALL PATHGT( PATH )
      LJUTL = GZJUTL()
      IF ( PATH .NE. 'MDST' .AND. LJUTL .LE. 0 ) THEN
         CALL JUTLFL(D0MDST)
         LJUTL = GZJUTL()
      END IF
      IF ( LJUTL .LE. 0 ) THEN
         IER = -4
         RETURN
      END IF
C
C- Get the header information
C

      IVERS    = IQ( LJUTL + 1 )
      IVERSV   = IVERS
      FILTM    = IQ( LJUTL + 2 )
C
C If IVERS>3 then should have 2 QCD masks
C
      FILTM2 = IQ( LJUTL + 3 )
C
C Bail out if D0MDST
C
      IF(D0MDST)  GO TO 999
C
      SLOWZ    =  Q( LJUTL +  4 )
      SLOWF    = NINT( Q( LJUTL +  5 ) )
      MINTF    = NINT( Q( LJUTL +  6 ) )
      FASTZ    =  Q( LJUTL +  7 )
      FASTF    = NINT( Q( LJUTL +  8 ) )
      TKVZ(1)  = Q( LJUTL +  9 )
      TKVDZ(1) = Q( LJUTL +  10 )
      TKVZ(2)  = Q( LJUTL +  11 )
      TKVDZ(2) = Q( LJUTL +  12 )
      TKVZ(3)  = Q( LJUTL +  13 )
      TKVDZ(3) = Q( LJUTL +  14 )
C
C Add L0 quality and MITOOL in later version of MDST
C
      L0Q   = Q( LJUTL +  15 )
      IF(L0Q.LE.0.) L0Q=-1.
      TOOL   = NINT( Q( LJUTL +  16 ) )
      IF(L0Q.EQ.-1..AND.TOOL.LE.0) TOOL=-1
C
 999  CONTINUE
      IER = 0
C
C Find where L1L2 starts from version number for use w/GTJUTL_JETS
C
      IF(IVERSV.EQ.1) THEN
        NHEADR=40
      ELSE
        NHEADR=20
      END IF
      IF(D0MDST) NHEADR=3
C
      RETURN
C
C----------------------------------------------------------------------
      ENTRY GTJUTL_JETS(JRUN,NTTS,TTJET,NLJS,LJJET,NL2S,L2JET,CSIZ,IER)
C----------------------------------------------------------------------
C- Reset stuff
C
      LJUTL = 0
      NTTS = 0
      NLJS = 0
      NL2S = 0
C
C- Find the JUTL bank.
C
      CALL PATHGT( PATH )
      LJUTL = GZJUTL()
      IF ( PATH .NE. 'MDST' .AND. LJUTL .LE. 0 ) THEN
         CALL JUTLFL(D0MDST)
         LJUTL = GZJUTL()
      END IF
      IF ( LJUTL .LE. 0 ) THEN
         IER = -4
         RETURN
      END IF
C
C- Get the TT jet information ...
C
      PNTR = LJUTL+NHEADR
      NTTS = NINT( Q( PNTR + 1 ) )
      PNTR = PNTR + 1
C
C PNTRFX .NE. 0 if NOBJ>MAX
C
      IF(NTTS.GT.MAX_TT) THEN
        PNTRFX=3*(NTTS-MAX_TT)
      ELSE
        PNTRFX=0
      END IF
      NTTS = MIN(NTTS,MAX_TT)
C
C Fill TT
C
      IF(NTTS.GT.0) THEN
        DO I = 1, NTTS
          TTJET( 1, I ) = Q( PNTR + 1 )
          TTJET( 2, I ) = Q( PNTR + 2 )
          TTJET( 3, I ) = Q( PNTR + 3 )
          PNTR = PNTR + 3
        END DO
        PNTR=PNTR+PNTRFX
      END IF
C
C- For 1B also have large tiles
C
      IF(JRUN.EQ.2) THEN
        IF(IVERSV.GT.3) THEN
          NLJS =  NINT( Q( PNTR + 1 ) )
          PNTR = PNTR + 1
          IF(NLJS.GT.MAX_LJ) THEN
            PNTRFX=3*(NLJS-MAX_LJ)
          ELSE
            PNTRFX=0
          END IF
          NLJS=MIN(NLJS,MAX_LJ)
C
C LJ not available so set NLJS to -1
C
        ELSE
          NLJS=-1
        END IF
C
C Fill LJS
C
        IF(NLJS.GT.0) THEN
          DO I = 1, NLJS
            LJJET( 1, I ) = Q( PNTR + 1 )
            LJJET( 2, I ) = Q( PNTR + 2 )
            LJJET( 3, I ) = Q( PNTR + 3 )
            PNTR = PNTR + 3
          END DO
          PNTR=PNTR+PNTRFX
        END IF
      END IF
C
C- If large tiles could exist but don't leave 1 space after trigger towers
C
      IF(IVERSV.GT.3.AND.JRUN.EQ.1) PNTR=PNTR+1
C
C- ... and the L2 jet information
C
      NL2S = NINT( Q( PNTR + 1) )
      NL2S = MIN(NL2S,MAX_L2)
      PNTR = PNTR + 1
      CSIZ = Q( PNTR + 1)
      PNTR = PNTR + 1
      DO I = 1, NL2S
         L2JET( 1, I ) = Q( PNTR + 1 )
         L2JET( 2, I ) = Q( PNTR + 2 )
         L2JET( 3, I ) = Q( PNTR + 3 )
         L2JET( 4, I ) = Q( PNTR + 4 )
         L2JET( 5, I ) = Q( PNTR + 5 )
         PNTR = PNTR + 5
      END DO
C
C- Note: Don't use extra scalar ET and missing ET words here
C
      IER = 0
      RETURN
      END
