      SUBROUTINE CALBLD(KTRA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Extract energies from CALTOWER working bank
C-   and place into Zebra bank after each track is finished.
C-      Called by CALTRK one/primary track, if SCAL(1).EQ.1.
C-
C-   Inputs  : KTRA (track #), /CALTRK/ (working storage of track energies)
C-   Outputs : CAEP and/or GCAH of /ZEBCOM/
C-
C-   Created  16-JUL-1987   A.M.Jonckheere
C-   Updated  30-NOV-1988   A.M.Jonckheere : Do binary rather than sequential
C-                                              search for CAEP data
C-   Updated  10-FEB-1989   Alan M. Jonckheere : Convert to CAEP and Physics
C-                              indices - essentially a total rewrite
C-   Updated   9-JUN-1989   John Womersley  :  Accept KTRA=0 and store only
C-                              the sum of all tracks
C-   Updated  12-OCT-1989   Alan M. Jonckheere  Restart from V2.3_01 version
C-                              Add in development in Beta area.
C-   Updated  30-SEP-1989   Chip Stewart - Save Mom and Pos at entry to CAL,
C-                              1st interaction point and purely hadronic
C-                              energy in addition to total in each cell.
C-
C-    ---- Unrecorded updates -----
C-
C-   Updated  12-MAY-1992   K. Wyatt Merritt  Add flag for "reentrant GCAH"
C-                              condition, so that ISP1_LINK will not be
C-                              called when backward going energy has messed
C-                              up the JSTAK stack.  Flag this condition with
C-                              E at cal entry = +9999., which will signal 
C-                              showerlibrary to drop the whole ISP1 track. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C-
      INTEGER KTRA
C-
      INCLUDE 'D0$INC:CALTRK.INC'       ! CAL TRACK WORKING BANK
      INCLUDE 'D0$INC:GCUNIT.INC'       ! GEANT UNITS BANK
      INCLUDE 'D0$INC:GCONST.INC'       ! GEANT CONTSTANTS BANK
      INCLUDE 'D0$INC:GCKINE.INC'       ! GEANT KINEMATICS BANKS
C ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC'       ! MAIN STORAGE
      INCLUDE 'D0$INC:ZLINKA.INC'       ! TEMPORARY LINK AREA
C D0GEANT BANKS
      INCLUDE 'D0$INC:D0LOG.INC'        ! LOGICAL SWITCHES
      INCLUDE 'D0$INC:HCAL.INC'         ! CAL TOTALS
C TEMPORARY LINKS INDICES
      INTEGER GZCAEP,GZGCAH,GZCAHT
      INTEGER NCAEP,NDCAEP,NCCAEP
      INTEGER NCAEPD,NDCAEPD,NCCAEPD
      INTEGER MCAEP,MDCAEP,MCCAEP
      INTEGER NGCAH,NDGCAH,NCGCAH
      INTEGER IETA,IPHI,LAYER
      INTEGER LGEAN,LGCAH,LCAEP
C
      REAL TTUCA,TTECA,TTCRK,TTMSG,TTSCN,TTUCS,TTECS,TTMSS,TTMRG
      REAL EDATA
      INTEGER IADDR,TAG
      INTEGER I,KD,NIO
C
      INTEGER KPART,IDTRK,NVERT,NUBUF,NR
      REAL    VVERT(3),P(9),ETRK,PSQ,PT,ESMR,SMRFAC,UBUF(10)
      REAL    ETOT,ETOTS
      REAL    DR(3),X,Y,Z
C
      INTEGER JADDR,JINDX
      LOGICAL LNEW
C
      INTEGER MPART,MJET
      LOGICAL IOK
C
      LOGICAL REENTRANT_GCAH
      INTEGER CHECK
C
      CHARACTER*8 CBLLNK
      DATA CBLLNK /'CALBLD  '/
C
      DATA TAG/0/
C----------------------------------------------------------------------
C
C ****  Zero totals
      TTUCA = 0
      TTECA = 0
      TTCRK = 0
      TTMSG = 0
      TTSCN = 0
      TTUCS = 0
      TTECS = 0
      TTMSS = 0
      TTMRG = 0
C
      IF ( KTRA.LT.0 ) GOTO 999
      IF ( CALTOT+DEDTOT+SCNTOT.LE.0 ) GOTO 999
C
C  HEAD BANK
      IF ( LHEAD.LE.0 ) THEN
        WRITE(LOUT,* )
     +      ' ****** ZEBRA HEADER BANK DOES NOT EXIST, RETURN ******'
        RETURN
      ENDIF
C
      CALL PATHST('GEAN')               ! Working under GEAN
      CALL BKGEAN(LGEAN)                ! Make sure GEAN is booked
C
C GCAH bank for this track
      NGCAH = 0
      IF ( SCAL(2).NE.0 .AND. KTRA.GT.0 ) THEN
        CALL GSLINK(CBLLNK,NGCAH)
C
        IF ( SCAL(2).EQ.2. ) THEN
          NR = 3
        ELSE
          NR = 2
        ENDIF
        CHECK = GZGCAH(KTRA)
        REENTRANT_GCAH = CHECK .GT. 0
        CALL BKGCAH(KTRA,LGCAH,NR)
        LSLINK(NGCAH) = LGCAH
        NDGCAH = IQ(LSLINK(NGCAH)-1)      ! Number of data words available
        NCGCAH = IQ(LSLINK(NGCAH)+33)     ! Number of channels used
      ENDIF
C
C ****  Now book high level banks in CAEP branch
      NCAEP = 0
      NCAEPD = 0
      IF ( SCAL(3).NE.0 .OR. SCAL(5).GE.0 ) THEN
C
C  Book CAEP bank - Sum of all tracks
        CALL BKCAEP(500,LCAEP)            ! Book 1st bank (live material)
        CALL GSLINK(CBLLNK,NCAEP)       ! Get temporary link
        LSLINK(NCAEP) = LCAEP
        NDCAEP = IQ(LSLINK(NCAEP)-1)      ! Number of data words available
        NCCAEP = IQ(LSLINK(NCAEP)+3)      ! Number of channels used
C
        CALL BKCAEP_2(500,LCAEP)          ! Book 2d bank (dead material)
        CALL GSLINK(CBLLNK,NCAEPD)      ! Get temporary link
        LSLINK(NCAEPD) = LCAEP
        NDCAEPD = IQ(LSLINK(NCAEPD)-1)    ! Number of data words available
        NCCAEPD = IQ(LSLINK(NCAEPD)+3)    ! Number of channels used
      ENDIF
C
C ****  Now calculate some things we need for Smearing
C
      IF ( KTRA.GT.0 ) THEN
        CALL GFKINE(KTRA,VVERT,P,KPART,NVERT,UBUF,NUBUF)
        ETRK = P(4)
        PSQ = P(1)**2 + P(2)**2 + P(3)**2
        P(4) = SQRT(PSQ)
        IF ( ETRK**2.GT.PSQ ) THEN
          P(5) = SQRT(ETRK**2-PSQ)
        ELSE
          P(5) = 0.
        ENDIF
        PT = SQRT(P(1)**2+P(2)**2)
        IF ( PT.EQ.0.) THEN
          PT = 1.E-9
          P(2) = -1.E-9
        ENDIF
        P(6) = ATAN2(PT,P(3))                 ! THETA
        P(7) = ATAN2(-P(2),-P(1)) + PI        ! PHI
        P(8) = -LOG(TAN(P(6)/2.))             ! ETA
        CALL GEAISA(KPART,IDTRK)   ! Track type converted to GEANT code
C
C ****  Parton #/Jet #
        MPART = UBUF(6)
        MJET  = UBUF(7)
      ENDIF
C
C ****  Now begin loop over data in working storage
C
C  We want random number for smearing only once/track
      LNEW = .TRUE.
C
      DO 900 IETA = -37, 37                      ! Loop over IETA
        IF ( EETA(IETA).LE.0. ) GOTO 900
        DO 800 IPHI = 1, 64                      ! Loop over IPHI
          DO 700 LAYER = 1, NLAYER               ! Loop over LAYER's
            EDATA = EWRK(LAYER,IPHI,IETA)
            IF ( EDATA.GT.0. ) THEN
              P(9) = EDATA              ! Used by SMRFAC
C Now smear the data where appropriate
              IF ( LAYER.EQ.8 ) THEN
                TTMSG = TTMSG + EDATA        ! UC MassLess Gaps
                ESMR = EDATA
                TTMSS =TTMSS + ESMR
              ELSEIF ( LAYER.EQ.9 ) THEN
                TTSCN = TTSCN + EDATA        ! ICD
                ESMR = EDATA
              ELSEIF ( LAYER.EQ.10 ) THEN
                TTMSG = TTMSG + EDATA        ! EC MassLess Gaps
                ESMR = EDATA
                TTMSS =TTMSS + ESMR
              ELSEIF ( LAYER.GT.17 ) THEN
                TTCRK = TTCRK + EDATA        ! Crack + Cryostat
                ESMR = EDATA
              ELSEIF ( (IABS(IETA).GE.8).AND.(IABS(IETA).LE.11).AND.
     &                   (IPHI.EQ.18).AND.(LAYER.GE.15)) THEN
                TTMRG = TTMRG + EDATA        ! Missing Main Ring channels
                ESMR = EDATA
              ELSEIF ( (IABS(IETA).LE.12).AND.(LAYER.LE.7) .OR.
     &                   (IABS(IETA).LE.10).AND.(LAYER.LE.13) .OR.
     &                   (IABS(IETA).LE.7) ) THEN
                TTUCA = TTUCA + EDATA        !CC
                IF ( KTRA.GT.0 )THEN
                  ESMR = EDATA*SMRFAC(LNEW,P,IDTRK,IETA,IPHI,LAYER)
                ELSE
                  ESMR = EDATA
                ENDIF
                LNEW = .FALSE.
                TTUCS = TTUCS + ESMR
              ELSE
                TTECA = TTECA + EDATA        ! EC
                IF ( KTRA.GT.0 )THEN
                  ESMR = EDATA*SMRFAC(LNEW,P,IDTRK,IETA,IPHI,LAYER)
                ELSE
                  ESMR = EDATA
                ENDIF
                LNEW = .FALSE.
                TTECS = TTECS + ESMR
              ENDIF
C
C ****  Pack Address
              IADDR = IETA
              IADDR = 256*IADDR + IPHI
              IADDR = 256*IADDR + LAYER
              IADDR = 256*IADDR + TAG
C
C ****  Have to look for this channel in CAEP
              IF ( LAYER.LE.17 ) THEN
                MCAEP = NCAEP           ! Live material
                MCCAEP = NCCAEP
                MDCAEP = NDCAEP
              ELSE
                MCAEP = NCAEPD          ! Dead material
                MCCAEP = NCCAEPD
                MDCAEP = NDCAEPD
              ENDIF
              IF ( MCAEP.GT.0 ) THEN
C
                IF ( MCCAEP.GT.0 ) THEN
                  CALL BINSEA(IADDR,2*MCCAEP,IQ(LSLINK(MCAEP)+4),2,0,
     &                JADDR,JINDX)
                ELSE
                  JADDR = -640000000
                  JINDX = 0
                ENDIF
                JINDX = JINDX + 4
C
                IF ( IADDR-JADDR ) 10,20,30
C  Found addr higher than present address
C    Have to make room to insert new one
   10           IF ( 2*(MCCAEP+1)+3.GT.MDCAEP ) THEN      ! Enough room to add
C                                                       ! new cell?
                  CALL MZPUSH(IXMAIN,LSLINK(MCAEP),0,1000,'I') ! Make room
                  MDCAEP = IQ(LSLINK(MCAEP)-1)
                ENDIF
                DO 100 KD = 2*MCCAEP+3, JINDX, -1   ! Move data up 2 words
                  IQ(LSLINK(MCAEP)+KD+2) = IQ(LSLINK(MCAEP)+KD)
  100           CONTINUE
                IQ(LSLINK(MCAEP)+3) = IQ(LSLINK(MCAEP)+3) + 1    ! Bump count
                IQ(LSLINK(MCAEP)+JINDX)   = IADDR         ! Store address
                Q(LSLINK(MCAEP)+JINDX+1)  = ESMR          ! Store data
                MCCAEP = IQ(LSLINK(MCAEP)+3)
                GOTO 200
C  Found addr equal to present address - add in new data
   20           Q(LSLINK(MCAEP)+JINDX+1)  = Q(LSLINK(MCAEP)+JINDX+1) +
     &            ESMR
                GOTO 200
C  Present Addr is higher than any previously entered
C    Add in new data to end of bank
   30           CONTINUE
                IF ( 2*(MCCAEP+1)+3.GT.MDCAEP ) THEN      ! Enough room to add
C                                                       ! new cell?
                  CALL MZPUSH(IXMAIN,LSLINK(MCAEP),0,1000,'I') ! Make room
                  MDCAEP = IQ(LSLINK(MCAEP)-1)
                ENDIF
                IQ(LSLINK(MCAEP)+3) = IQ(LSLINK(MCAEP)+3) + 1    ! Bump count
                MCCAEP = IQ(LSLINK(MCAEP)+3)
                IQ(LSLINK(MCAEP)+2*MCCAEP+2) = IADDR    ! Store address
                Q(LSLINK(MCAEP)+2*MCCAEP+3)  = ESMR     ! Store data
              ENDIF
  200         CONTINUE
C
C ****  Save counts
              IF ( LAYER.LE.17 ) THEN
                NCCAEP = MCCAEP         ! Live material
                NDCAEP = MDCAEP
              ELSE
                NCCAEPD = MCCAEP        ! Dead material
                NDCAEPD = MDCAEP
              ENDIF
C
C ****  Now store energies in GCAH bank, if required
C
              IF (NGCAH.GT.0 .AND. KTRA.GT.0 .AND. .NOT.REENTRANT_GCAH)
     &           THEN
                IF ( NR*(NCGCAH+1)+33.GT.NDGCAH ) THEN    ! Enough room to add
C                                                         ! new cell?
                  CALL MZPUSH(IXMAIN,LSLINK(NGCAH),0,1000,'I')  ! Make room
                  NDGCAH = IQ(LSLINK(NGCAH)-1)
                ENDIF
                IQ(LSLINK(NGCAH)+33) = IQ(LSLINK(NGCAH)+33) + 1 ! Bump count
                NCGCAH = IQ(LSLINK(NGCAH)+33)
                IQ(LSLINK(NGCAH)+NR*(NCGCAH-1)+34) = IADDR      ! Store address
                Q(LSLINK(NGCAH)+NR*(NCGCAH-1)+35)  = EDATA      ! Store data
C
C ****  Hadronic part stored ONLY if NR = 3 (SCAL(2)=2)
                IF ( SCAL(2).EQ.2.) Q(LSLINK(NGCAH)+NR*(NCGCAH-1)+36) =
     &            EWRK_H(LAYER,IPHI,IETA)
              ENDIF
C
C ****  Calculate data for Ideal hits  ???? NOT IMPLIMENTED ????
              IF ( LAYER.LE.17 .AND. SCAL(4).NE.0 ) THEN ! Live material ONLY
                CALL CELXYZ(IETA,IPHI,LAYER,X,Y,Z,IOK)
                DR(1) = X - PKINE(4)
                DR(2) = Y - PKINE(5)
                DR(3) = Z - PKINE(6)
                CALL JETFIL(MPART,IETA,IPHI,LAYER,DR,ESMR)
              ENDIF
            ENDIF
  700     CONTINUE
  800   CONTINUE
  900 CONTINUE
C
      IF ( NGCAH.GT.0 .AND. KTRA.GT.0 ) THEN
C
        IF ( .NOT. REENTRANT_GCAH ) THEN
C ****  Fill in Event totals
          Q(LSLINK(NGCAH)+18) = TTUCA + TTECA
          Q(LSLINK(NGCAH)+19) = TTMSG
          Q(LSLINK(NGCAH)+20) = TTSCN
          Q(LSLINK(NGCAH)+21) = TTCRK + TTMRG
C
C ****  Fill in Primary Track Kinematics
          CALL UCOPY(CAL_P_START(1),Q(LSLINK(NGCAH)+22),4)
          CALL UCOPY(CAL_XYZ_START(1),Q(LSLINK(NGCAH)+26),3)
          CALL UCOPY(CAL_XYZ_INTERACT(1),Q(LSLINK(NGCAH)+29),3)
          IQ(LSLINK(NGCAH)+32) = CAL_TYPE_INTERACT
C
C ****  Collapse track bank back to minimum size.
          CALL MZPUSH(IXMAIN,LSLINK(NGCAH),0,NR*NCGCAH+33-NDGCAH,'I')
          NDGCAH = IQ(LSLINK(NGCAH)-1)    ! Number of data words available
          CALL ISP1_LINK(LSLINK(NGCAH))   ! FILL LINK FROM ISP1
        ELSE
          Q(LSLINK(NGCAH)+25) = 9999. !Flag bank so showerlibrary can drop
                                      ! ISP1 tracks with a reentrant GCAH
        ENDIF
      ENDIF
C
C ****  Release CAEP LINKS
      CALL RSLINK(CBLLNK,NGCAH)     ! Release link
      CALL RSLINK(CBLLNK,NCAEP)
      CALL RSLINK(CBLLNK,NCAEPD)
C
C ****  Add in track totals to event totals
      TOTUCA = TOTUCA + TTUCA
      TOTECA = TOTECA + TTECA
      TOTCRK = TOTCRK + TTCRK
      TOTMSG = TOTMSG + TTMSG
      TOTSCN = TOTSCN + TTSCN
      TOTUCS = TOTUCS + TTUCS
      TOTECS = TOTECS + TTECS
      TOTMSS = TOTMSS + TTMSS
      TOTMRG = TOTMRG + TTMRG
C
C ****  Zero working storage banks
  999 CALTOT = 0
      DEDTOT = 0
      SCNTOT = 0
      CALL VZERO(EWRK,75*64*NLAYER)
      CALL VZERO(EETA,75)
      IF ( SCAL(2).EQ.2.0 ) CALL VZERO(EWRK_H,75*64*NLAYER)
C
C ****  Reset Primary Track Data
      CAL_PRIMARY = 0
      CALL VZERO(CAL_P_START,4)
      CALL VZERO(CAL_XYZ_START,3)
      CALL VZERO(CAL_XYZ_INTERACT,3)
      CAL_TYPE_INTERACT = 0
C
      END
