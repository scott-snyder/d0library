      SUBROUTINE UDST_GET_EMCLUS(LPELC1,XDATA,UCSHPTR,UCWXPTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill electron parameters into array XDATA
C-
C-   Inputs  : LPELC1     - pointer to electron bank
C-             UCSHPTR    - pointer to start position in UCSH list
C-             UCWXPTR    - pointer to start position in UCWX list
C-
C-   Outputs : XDATA( 1) - energy
C-             XDATA( 2) - transverse energy
C-             XDATA( 3) - theta
C-             XDATA( 4) - eta
C-             XDATA( 5) - phi
C-             XDATA( 6) - detector eta
C-             XDATA( 7) - FH1 energy
C-             XDATA( 8) - EM fraction
C-             XDATA( 9) - EM energy in cluster outside central tower
C-             XDATA(10) - total energy in core cone
C-             XDATA(11) - total energy in isolation cone
C-             XDATA(12) - EM energy in core cone
C-             XDATA(13) - EM energy in isolation cone
C-             XDATA(14) - Et in cone 0.4
C-             XDATA(15) - E IN CONE 0.7
C-             XDATA(16) - Et in cone 0.7
C-             XDATA(17) - energy in EM layer 1
C-             XDATA(18) - energy in EM layer 2
C-             XDATA(19) - energy in EM layer 3
C-             XDATA(20) - energy in EM layer 4
C-             XDATA(21) - Hmatrix chi squared
C-             XDATA(22) - x of shower centroid
C-             XDATA(23) - y of shower centroid
C-             XDATA(24) - z of shower centroid
C-             XDATA(25) - dispersion (from deta,dphi)
C-             XDATA(26) - dispersion (from dx,dy,dz)
C-             XDATA(27) - distance from CC phi crack in radians
C-             XDATA(28) - number of cells in cluster
C-             XDATA(29) - electron quality flag bits  0-15 (CLEANEM)
C-             XDATA(30) - electron quality flag bits 16-31 (CLEANEM)
C-             XDATA(31) - matched L1 EM tower Et
C-             XDATA(32) - matched L2 candidate Et (no z-vertex correction)
C-             XDATA(33) - matched L2 candidate Et (with z-vertex correction)
C-             XDATA(34) - cut mask of matched L2 candidate
C-             XDATA(35) - sima5-sigma3 in CC and Energy in 7x7-55 in EC
C-             XDATA(36) - VARIANCE ON E
C-             XDATA(37) - Link to CASH bank
C-             XDATA(38) - Energy in physics isolation cone=0.6
C-             XDATA(39) - Et in physics isolation cone=0.6
C-             XDATA(40) - Packed hits in PELC road(HMTE+16)
C-             XDATA(41) - Packed hits in PELC road(HMTE+16)
C-             XDATA(42) - Packed hits in PELC road(HMTE+17)
C-             XDATA(43) - Packed hits in PELC road(HMTE+17)
C-             XDATA(44) - Packed TRD hits in PELC road(HMTE+18)
C-             XDATA(45) - Packed TRD hits in PELC road(HMTE+18)
C-             XDATA(46) - CACL energy
C-             XDATA(47)...XDATA(52) - 6 TRD WORDS
C-             XDATA(53) - TRD info available (1)
C-             XDATA(54) - TRD truncated Mean
C-             XDATA(55) - link to ZTRK object in UDST
C-             XDATA(56) - number of tracks in road
C-             XDATA(57) - distance of closest track
C-             XDATA(58) - number of tracks in a cone around the cluster
C-             XDATA(59) - RdeltaPHI
C-             XDATA(60) - deltaZ for CC, deltaR for EC
C-             XDATA(61) - significance of track match
C-             XDATA(62) - Lower Phi limit for Road into CD
C-             XDATA(63) - Upper Phi limit for Road into CD
C-             XDATA(64) - Lower Theta limit for Road into CD
C-             XDATA(65) - Upper Theta limit for Road into CD
C-             XDATA(66) - global fit (ELFIT) chisq / dof
C-             XDATA(67) - global fit (ELFIT) theta
C-             XDATA(68) - global fit (ELFIT) phifilling of z of point on FTRK
C-             XDATA(69) - global fit (ELFIT) : NDOF
C-
C-   Created  29-SEP-1992   Ulrich Heintz
C-   Updated   3-APR-1993   Ulrich Heintz - fix filling of z of point on FTRK
C-   Updated  14-AUG-1993   Ulrich Heintz - fixed storage of CLEANEM mask, add
C-                                          error on Et
C-   Updated   1-DEC-1993   Ian Adam - drop tracking info, keep link to
C-                           corresponding TRAK, also to UCSH
C-   Updated   4-JAN-1994   Ian Adam - add road limits from HMTE for CLEANEM
C-   Updated  18-JAN-1994   Ian Adam  - Set LZTRKE=0 if #links too small
C-   Updated  10-Mar-1994   H. Greenlee - Undo CORRECTEM
C-   Updated  31-MAR-1994   Ulrich Heintz - move undoing of CORRECTEM to
C-    subroutine UNCORRECTEM. Use Q(L2EM+21) instead of Q(L2EM+22) in EC
C-   Updated  27-MAR-1995   Ulrich Heintz - change sequence of words to include
C-    TRD information with PPHOs
C-   Updated  23-MAY-1995   Ian Adam  - check HMTE version for ELFIT words
C-   Updated  07-AUG-1995   Chip Stewart - use ZLINKC link area (for LCACL)
C-   Updated  19-OCT-1995   Ian Adam  - Use UHTOC to get bank name
C-   Updated  20-OCT-1995   Ian Adam  - Add words for UDST v5 & UCWX link
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVERT.LINK'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZLINKC.INC'

      INTEGER LPELC1,LRCP,HMTE_VERSION,LTRDT
      INTEGER IETA_HOT(5),IPHI_HOT(5),I,IPTR,IER,IHITS,INT_TRD(10),NINT
      INTEGER NVAR,ISTATUS,LL2EM,GZL2EM,VERSION,KELEC,KPHOT,KELEC1
      INTEGER KPHOT1,ID_ELEC,ID_PHOT,UDST_ZTRK_LINK,UCSHPTR,UCWXPTR
      REAL    P4(4),VARP3(3),DPIDPJ(3),ET_L1,ET_L2
      REAL    EDPTH(5),PDPTH(5),ENERGY_HOT(5),CLQUAN(50),CTQUAN(50)
      REAL    WEIGHT_CUT,X(3),DX(3),ETAPHI(3),DETAPHI(3),STATUS(2)
      CHARACTER*4 BANK_NAME
      LOGICAL FIRST,CMATCH_TRIGELEC,OK
      DATA    FIRST/.TRUE./
      PARAMETER( KELEC=83,KPHOT=69 )
      CHARACTER*8 PPHO_TAGS(KPHOT),PELC_TAGS(KELEC)
      DATA PPHO_TAGS/
     &   'EP'      ,'ETP'     ,'THP'     ,'ETAP'    ,'PHIP'    ,'IETAP'
     &  ,'FH1P'    ,'FEMP'    ,'EEDGP'   ,'ECORP'   ,'ECONP'   ,'EMCORP'
     &  ,'EMCONP'  ,'ETCN1P'  ,'ECON2P'  ,'ETCN2P'  ,'EMLY1P'  ,'EMLY2P'
     &  ,'EMLY3P'  ,'EMLY4P'  ,'CHISQP'  ,'XP'      ,'YP'      ,'ZP'
     &  ,'DISPP'   ,'XDISPP'  ,'DCRAKP'  ,'NCELLP'  ,'QUALP_'  ,'QUALP'
     &  ,'L1ETP'   ,'L2ETP'   ,'L2ETCP'  ,'MASKP'   ,'SIG53P'  ,'ENVARP'
     &  ,'LCASHP'  ,'ECON3P'  ,'ETCN3P'  ,'HITP1_'  ,'HITP1'   ,'HITP2_'
     &  ,'HITP2'   ,'HITP3_'  ,'HITP3'   ,'ECACLP'  ,'TRD11P'  ,'TRD12P'
     &  ,'TRD21P'  ,'TRD22P'  ,'TRD31P'  ,'TRD32P'  ,'TRDACP'  ,'TRDEFP'
     &  ,'TRDLP'   ,'TRDL2P'
     &  ,'LCAWXP'  ,'WIN15P'  ,'WIN3P'   ,'WIN5P'   ,'WIN7P'   ,'ZVCLP'
     &  ,'XYICLP'  ,'VCALP'   ,'LIK5P'   ,'LIK4P'   ,'LIK3P'   ,'VCOGP'
     &  ,'LZTRKP'
     &  /
      DATA PELC_TAGS/
     &   'EE'      ,'ETE'     ,'THE'     ,'ETAE'    ,'PHIE'    ,'IETAE'
     &  ,'FH1E'    ,'FEME'    ,'EEDGE'   ,'ECORE'   ,'ECONE'   ,'EMCORE'
     &  ,'EMCONE'  ,'ETCN1E'  ,'ECON2E'  ,'ETCN2E'  ,'EMLY1E'  ,'EMLY2E'
     &  ,'EMLY3E'  ,'EMLY4E'  ,'CHISQE'  ,'XE'      ,'YE'      ,'ZE'
     &  ,'DISPE'   ,'XDISPE'  ,'DCRAKE'  ,'NCELLE'  ,'QUALE_'  ,'QUALE'
     &  ,'L1ETE'   ,'L2ETE'   ,'L2ETCE'  ,'MASKE'   ,'SIG53E'  ,'ENVARE'
     &  ,'LCASHE'  ,'ECON3E'  ,'ETCN3E'  ,'HITE1_'  ,'HITE1'   ,'HITE2_'
     &  ,'HITE2'   ,'HITE3_'  ,'HITE3'   ,'ECACLE'  ,'TRD11E'  ,'TRD12E'
     &  ,'TRD21E'  ,'TRD22E'  ,'TRD31E'  ,'TRD32E'  ,'TRDACC'  ,'TRDEFF'
     &  ,'TRDLE'   ,'TRDL2E'
     &  ,'LCAWXE'  ,'WIN15E'  ,'WIN3E'   ,'WIN5E'   ,'WIN7E'   ,'ZVCLE'
     &  ,'XYICLE'  ,'VCALE'   ,'LIK5E'   ,'LIK4E'   ,'LIK3E'   ,'VCOGE'
     &  ,'LZTRKE'  ,'NTRKE'   ,'DCLTKE'  ,'NTKCON'  ,'RDPHI'   ,'DZ'
     &  ,'TRKSIG'  ,'ROLPHI'  ,'ROUPHI'  ,'ROLTHE'  ,'ROUTHE'  ,'GCHISQ'
     &  ,'GTHE'    ,'GPHIE'   ,'GNDOF'
     &  /
      REAL
     &   EE      ,ETE     ,THE     ,ETAE    ,PHIE    ,IETAE
     &  ,FH1E    ,FEME    ,EEDGE   ,ECORE   ,ECONE   ,EMCORE
     &  ,EMCONE  ,ETCN1E  ,ECON2E  ,ETCN2E  ,EMLY1E  ,EMLY2E
     &  ,EMLY3E  ,EMLY4E  ,CHISQE  ,XE      ,YE      ,ZE
     &  ,DISPE   ,XDISPE  ,DCRAKE  ,NCELLE  ,QUALE_  ,QUALE
     &  ,L1ETE   ,L2ETE   ,L2ETCE  ,MASKE   ,SIG53E  ,ENVARE
     &  ,LCASHE  ,ECON3E  ,ETCN3E  ,HITE1_  ,HITE1   ,HITE2_
     &  ,HITE2   ,HITE3_  ,HITE3   ,ECACLE  ,TRD11E  ,TRD12E
     &  ,TRD21E  ,TRD22E  ,TRD31E  ,TRD32E  ,TRDACC  ,TRDEFF
     &  ,TRDLE   ,TRDL2E
     &  ,LCAWXE  ,WIN15E  ,WIN3E   ,WIN5E   ,WIN7E   ,ZVCLE
     &  ,XYICLE  ,VCALE   ,LIK5E   ,LIK4E   ,LIK3E   ,VCOGE
     &  ,LZTRKE  ,NTRKE   ,DCLTKE  ,NTKCON  ,RDPHI   ,DZ
     &  ,TRKSIG  ,ROLPHI  ,ROUPHI  ,ROLTHE  ,ROUTHE  ,GCHISQ
     &  ,GTHE    ,GPHIE   ,GNDOF
      COMMON/PELC_OBJECT/
     &   EE      ,ETE     ,THE     ,ETAE    ,PHIE    ,IETAE
     &  ,FH1E    ,FEME    ,EEDGE   ,ECORE   ,ECONE   ,EMCORE
     &  ,EMCONE  ,ETCN1E  ,ECON2E  ,ETCN2E  ,EMLY1E  ,EMLY2E
     &  ,EMLY3E  ,EMLY4E  ,CHISQE  ,XE      ,YE      ,ZE
     &  ,DISPE   ,XDISPE  ,DCRAKE  ,NCELLE  ,QUALE_  ,QUALE
     &  ,L1ETE   ,L2ETE   ,L2ETCE  ,MASKE   ,SIG53E  ,ENVARE
     &  ,LCASHE  ,ECON3E  ,ETCN3E  ,HITE1_  ,HITE1   ,HITE2_
     &  ,HITE2   ,HITE3_  ,HITE3   ,ECACLE  ,TRD11E  ,TRD12E
     &  ,TRD21E  ,TRD22E  ,TRD31E  ,TRD32E  ,TRDACC  ,TRDEFF
     &  ,TRDLE   ,TRDL2E
     &  ,LCAWXE  ,WIN15E  ,WIN3E   ,WIN5E   ,WIN7E   ,ZVCLE
     &  ,XYICLE  ,VCALE   ,LIK5E   ,LIK4E   ,LIK3E   ,VCOGE
     &  ,LZTRKE  ,NTRKE   ,DCLTKE  ,NTKCON  ,RDPHI   ,DZ
     &  ,TRKSIG  ,ROLPHI  ,ROUPHI  ,ROLTHE  ,ROUTHE  ,GCHISQ
     &  ,GTHE    ,GPHIE   ,GNDOF
C
      REAL    XX(KELEC),XDATA(KELEC)
      EQUIVALENCE(XX,EE)
C
      LPELC =  LPELC1
      IF(FIRST)THEN
        FIRST=.FALSE.
        IER=0
        CALL EZLOC('CLEANEM_RCP',LRCP)
        IF (LRCP.LE.0)CALL INRCP('CLEANEM_RCP',IER)
        IF (IER.EQ.0) CALL EZPICK('CLEANEM_RCP')
        IF (IER.EQ.0) CALL EZERR(IER)
        IF (IER.EQ.0) CALL EZGET('WEIGHT_CUT',WEIGHT_CUT,IER)
        IF(IER.NE.0)CALL ERRMSG('CLEANEM_RCP','UDST_GET_EMCLUS',
     &      'bank not found','F')
        CALL EZRSET
      ENDIF
      CALL VZERO(XX,KELEC)
C
C ****  Get version number of the bank
C
      VERSION = IQ(LPELC+1)
C
C ****  copy words from PELC/PPHO bank
C
      EE     = Q(LPELC+6)        ! energy
      ETE    = Q(LPELC+7)        ! transverse energy
      THE    = Q(LPELC+8)        ! theta
      ETAE   = Q(LPELC+9)        ! eta
      PHIE   = Q(LPELC+10)       ! phi
      EEDGE  = Q(LPELC+14)       ! EM energy in cluster outside central tower
      ECORE  = Q(LPELC+15)       ! Total energy in core cone
      ECONE  = Q(LPELC+16)       ! Total energy in isolation cone
      EMCORE = Q(LPELC+17)       ! EM energy in core cone
      EMCONE = Q(LPELC+18)       ! EM energy in isolation cone
      IETAE  = Q(LPELC+19)       ! detector eta
      NTRKE  = Q(LPELC+21)       ! number of tracks in road
      DCLTKE = Q(LPELC+22)       ! distance of closest track
      XE     = Q(LPELC+23)       ! X cog
      YE     = Q(LPELC+24)       ! Y cog
      ZE     = Q(LPELC+25)       ! Z cog
      DO I=1,3
        P4(I)=Q(LPELC+2+I)
        DPIDPJ(I)=Q(LPELC+26+I)
      ENDDO
      P4(4)=Q(LPELC+6)
      VARP3(1)=Q(LPELC+11)
      VARP3(2)=Q(LPELC+12)
      VARP3(3)=Q(LPELC+26)
      CALL GET_E_VARIANCE(P4,VARP3,DPIDPJ,ENVARE) !   VARIANCE on E
C
C ****  Get info from CLEANEM
C
      LTRDT=0
      LCACL  = LQ(LPELC-2)
      CALL CLEANEM(LPELC,1,OK,ISTATUS)
      CALL CLEANEM_TQUANS(NVAR,CTQUAN)
      CALL UHTOC(IQ(LPELC-4),4,BANK_NAME,4)
      IF(BANK_NAME.EQ.'PELC')THEN  ! if electron get angles from tracking
        NTKCON  = CTQUAN(2)
        RDPHI   = CTQUAN(10)
        DZ      = CTQUAN(11)
        IF(VERSION.LT.5)THEN
          TRKSIG = CTQUAN(12)
        ELSE
          TRKSIG = Q(LPELC+20)
        ENDIF
        LZTRK = LQ(LPELC-3)
        IF(LZTRK.GT.0)THEN
          LTRDT = LQ(LZTRK-9)
        ELSE
          CALL ERRMSG('ZTRK','UDST_GET_EMCLUS','bank not found','W')
        ENDIF
      ELSE
        IF (IQ(LCACL+1).GE.6) LTRDT = LQ(LCACL-5)
      ENDIF
      IF(LTRDT .GT. 0)THEN
        CALL TRD_ON_MICRO(LTRDT,INT_TRD,NINT)
        TRD11E = FLOAT(INT_TRD(1))
        TRD12E = FLOAT(INT_TRD(2))
        TRD21E = FLOAT(INT_TRD(3))
        TRD22E = FLOAT(INT_TRD(4))
        TRD31E = FLOAT(INT_TRD(5))
        TRD32E = FLOAT(INT_TRD(6))
        IF(Q(LTRDT+1).GT.4)THEN
          TRDLE = Q(LTRDT+17)
        ELSE
          TRDLE = 0.
        ENDIF
        IF(Q(LTRDT+1).GT.3)THEN
          TRDL2E = Q(LTRDT+19)
        ELSE
          TRDL2E = 0.
        ENDIF
      ELSEIF(BANK_NAME.EQ.'PELC'.OR.IQ(LCACL+1).GE.6)THEN
        CALL ERRMSG('TRDT','UDST_GET_EMCLUS','bank not found','W')
      ENDIF
      CALL SPLIT_BITMASK(ISTATUS,STATUS)
      QUALE_  = STATUS(1)
      QUALE   = STATUS(2)
      CALL CLEANEM_CQUANS(NVAR,CLQUAN)
      DCRAKE  = CLQUAN(20)
      XDISPE  = CLQUAN(11)           ! xy dispersion
      FEME    = CLQUAN(9)            ! EM fraction
C
C **** copy words from CACL bank
C
      IF(LCACL.GT.0)THEN
        ECACLE =Q(LCACL+17)                            ! FH1 energy
        FH1E =Q(LCACL+19)                            ! FH1 energy
        IF (VERSION.GE.3) THEN
          ETCN1E = Q(LCACL+29)     ! Et in cone 0.4
          ECON2E = Q(LCACL+30)     ! E in cone 0.7
          ETCN2E = Q(LCACL+31)     ! Et in cone 0.7
          ECON3E = Q(LCACL+32)     ! E in cone 0.6
          ETCN3E = Q(LCACL+33)     ! Et in cone 0.6
        ENDIF
C **** copy words from CASH bank
        LCASH  = LQ(LCACL-2)
        IF(LCASH.GT.0)THEN
          NCELLE = FLOAT(IQ(LCASH+2))                  ! Ncells
          CALL CM3POS(LCASH,WEIGHT_CUT,X,DX,ETAPHI,DETAPHI)
          IF(X(1).NE.-999.)THEN  ! CM3POS didn't fail
C ****  compute dispersion eta/phi and xyz
            IF(DETAPHI(1).LT.0)DETAPHI(1)=0.
            IF(DETAPHI(2).LT.0)DETAPHI(2)=0.
            DISPE =SQRT(DETAPHI(1)+DETAPHI(2)) ! dispersion
          ELSE
            DISPE = 0.
          ENDIF
C ****  compute layer energies
          CALL CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
          EMLY1E = EDPTH(1)      ! Layer energies
          EMLY2E = EDPTH(2)      ! Layer energies
          EMLY3E = EDPTH(3)      ! Layer energies
          EMLY4E = EDPTH(4)      ! Layer energies
        ENDIF
      ENDIF
C
C **** copy words from HMTE bank
C
      LHMTE  = LQ(LPELC-1)
      IF(LHMTE.GT.0)THEN
        HMTE_VERSION = IQ(LHMTE+1)
        CHISQE = Q(LHMTE+7)     ! truncated chi squared
        ROLPHI = Q(LHMTE+9)     ! Lower Phi limit for Road into CD
        ROUPHI = Q(LHMTE+10)    ! Upper Phi limit for Road into CD
        ROLTHE = Q(LHMTE+11)    ! Lower Theta limit for Road into CD
        ROUTHE = Q(LHMTE+12)    ! Upper Theta limit for Road into CD
        IHITS  = IQ(LHMTE+16)    ! packed hits in PELC road
        CALL SPLIT_BITMASK(IHITS,STATUS)
        HITE1_ = STATUS(1)
        HITE1  = STATUS(2)
        IHITS  = IQ(LHMTE+17)    !Packed hits in PELC road
        CALL SPLIT_BITMASK(IHITS,STATUS)
        HITE2_ = STATUS(1)
        HITE2  = STATUS(2)
        IHITS  = IQ(LHMTE+18)    !Packed hits in PELC road
        CALL SPLIT_BITMASK(IHITS,STATUS)
        HITE3_ = STATUS(1)
        HITE3  = STATUS(2)
        IF (HMTE_VERSION.GE.4) THEN
          GCHISQ = Q(LHMTE+21)
          GTHE   = Q(LHMTE+22)
          GPHIE  = Q(LHMTE+23)
          GNDOF  = IQ(LHMTE+24)
        ELSE
          GCHISQ = 0.0
          GTHE   = 0.0
          GPHIE  = 0.0
          GNDOF  = 0.0
        ENDIF
      ENDIF
C
C ****  match L1 and L2 objects - get words from L2EM and ESUM banks
C
C... get L2EM version number
      LL2EM=GZL2EM()
      MASKE=0.
      IF(LL2EM.GT.0)THEN
        IF(IQ(LL2EM+1).GE.3)MASKE=16. ! version of L2EM bank
      ENDIF
      IF(CMATCH_TRIGELEC(LPELC,2.,2.,49,0.,0.,L1ETE,L2ETE,IPTR))THEN
        MASKE=MASKE+1.                  ! passes ELE shape cuts
        L2ETCE =Q(IPTR+35)              ! vertex corrected Et
        IF(ABS(IQ(IPTR+6)).LE.12)THEN
          SIG53E =Q(IPTR+17)-Q(IPTR+16) ! SIGMA5-SIGMA3 in CC
        ELSE
          SIG53E =Q(IPTR+21)            ! (5x5-3x3)/3x3 in EC
        ENDIF
      ELSE
        L1ETE  = 0.
        L2ETE  = 0.
        L2ETCE = 0.
        SIG53E = -99.
      ENDIF
      IF(CMATCH_TRIGELEC(LPELC,2.,2.,53,0.,0.,ET_L1,ET_L2,IPTR))THEN
        MASKE=MASKE+2.              ! passes ELE shape cuts and track match
      ENDIF
      IF(CMATCH_TRIGELEC(LPELC,2.,2.,177,0.4,0.15,ET_L1,ET_L2,IPTR))THEN
        MASKE=MASKE+4.              ! passes ELE shape cuts and 0.4 isolation
      ENDIF
      IF(CMATCH_TRIGELEC(LPELC,2.,2.,177,0.6,0.15,ET_L1,ET_L2,IPTR))THEN
        MASKE=MASKE+8.              ! passes ELE shape cuts and 0.6 isolation
      ENDIF
C
C ****  sequential location of associated ZTRK bank
C
      IF(IQ(LPELC-3).GE.3)THEN  ! check whether there is a link to ZTRK
        LZTRK=LQ(LPELC-3)
        IF (LZTRK.GT.0) THEN
          CALL UDST_GET_TRACK_LINK(LZTRK,UDST_ZTRK_LINK)
          LZTRKE = FLOAT(UDST_ZTRK_LINK)
        ELSE
          LZTRKE = 0
        ENDIF
      ELSE
        LZTRKE = 0
      ENDIF
C
C **** link to CASH bank is filled in calling routine
C
      LCASHE = FLOAT(UCSHPTR)
      LCAWXE = FLOAT(UCWXPTR)
C
C- Stuff from PELC bank version 6

      IF (VERSION.GE.6) then
        WIN15E    = Q(LPELC + 37)
        WIN3E     = Q(LPELC + 38)
        WIN5E     = Q(LPELC + 39)
        WIN7E     = Q(LPELC + 40)
        ZVCLE     = Q(LPELC + 34)
        XYICLE    = Q(LPELC + 35)
        VCALE     = FLOAT(IQ(LPELC + 36))
        LIK5E     = Q(LPELC + 41)
        LIK4E     = Q(LPELC + 42)
        LIK3E     = Q(LPELC + 43)
        TRDEFF    = Q(LPELC + 44)
        TRDACC    = Q(LPELC + 45)
        VCOGE     = FLOAT(IQ(LPELC + 47))
      ELSE
        TRDACC  = CTQUAN(22)
        TRDEFF  = CTQUAN(23)
        WIN15E    = 0.0
        WIN3E     = 0.0
        WIN3E     = 0.0
        WIN3E     = 0.0
        ZVCLE     = 0.0
        XYICLE    = 0.0
        VCALE     = 0.0
        LIK5E     = 0.0
        LIK4E     = 0.0
        LIK3E     = 0.0
        VCOGE     = 0.0
      ENDIF

      DO I=1,KELEC
        XDATA(I) = XX(I)
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
C
      ENTRY UDST_PELC_TAGS(KELEC1,ID_ELEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book PELC group
C-
C-   Created  18-AUG-1993   Ulrich Heintz
C-
C----------------------------------------------------------------------
      KELEC1=KELEC
      ID_ELEC=3
      CALL UDST_BOOK_GROUP(ID_ELEC,'PELC',PELC_TAGS,KELEC)
C----------------------------------------------------------------------
      RETURN
C
      ENTRY UDST_PPHO_TAGS(KPHOT1,ID_PHOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book PPHO group
C-
C-   Created  18-AUG-1993   Ulrich Heintz
C-
C----------------------------------------------------------------------
      KPHOT1=KPHOT
      ID_PHOT=4
      CALL UDST_BOOK_GROUP(ID_PHOT,'PPHO',PPHO_TAGS,KPHOT)
C----------------------------------------------------------------------
      RETURN
      END
