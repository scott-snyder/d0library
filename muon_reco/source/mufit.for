      SUBROUTINE MUFIT(IFIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : STEER Fitting pacakge of muon tracks
C-   in the muon system
C-
C-   Created  18-OCT-1991   Asher Klatchko
C-   Updated  24-OCT-1991   A.Klatchko  Add polinomial fit for no A layer muons
C-   Updated  26-OCT-1991   A.Klatchko  Add Energy loss in Cal for Kalman
C-   Updated   1-NOV-1991   A.Klatchko  Add USER bank
C-   Updated  25-NOV-1991   A.Klatchko
C-   Updated  09-DEC-1991   S. ABACHI   MUON bank counter corrected
C-   Updated  31-DEC-1991   A.Klatchko  ALLOW MUOT GOOD MUONS TO BE FILLED IN
C-   MFIT IF CHEBYCHEV INTERPOLATION WORKS
C-   Updated   2-JAN-1992   A.Klatchko  NO KALMAN IF MOMENTUM .LT. 5.5
C-   Updated   3-JAN-1992   A.Klatchko CHEBYCHEV INTERPOLATION even if filter
C-   fails + momentum dependent tolerance
C-   Updated   13-JAN-1992   S. ABACHI  MUOT tracks excluded by flag now allowed
C-   Updated   14-JAN-1992   S. ABACHI  IMTRK incremented correctly
C-   Updated   15-JAN-1992   S. ABACHI  ERRMSG argument modified
C-   Updated   15-JAN-1992   S. ABACHI  Copy MUOT if SAMUS or no fit
C-   Updated   23-JAN-1992   S. ABACHI  MUON was correctly booked+ flag allowed
C-   Updated   24-JAN-1992   S. ABACHI  MUOT copied when SAMUS hit present
C-   Updated   29-JAN-1992   A.Klatchko ADD BUFFER AGAINST MUPQCK_T
C-   Updated   04-FEB-1992   S. ABACHI  ERRMSG protected aginst large pcal
C-   Updated   04-FEB-1992   S. ABACHI  If (p-dedx)<pmin, then MUOT is copied
C-   Updated   14-FEB-1992   S. ABACHI  rcp parameter KFIt added
C-   Updated   10-JUN-1992   S. ABACHI  IFLG upto 2 accepted
C-   Updated    3-JAN-1995   I.Mandrichenko
C-                                      Use MUKF bank for SAMUS tracks
C-   Updated   11-FEB-1995   M. Fortner Remove unused variables
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL FIRST
      INTEGER IFIT,IFAIL
      INTEGER IC,IERZ
      PARAMETER (IC=30)
      INTEGER NTRACKS,ITRAK,QUAD,IMTRK
      INTEGER NFITDA,NFITDBC
      INTEGER HITA,HITB,MUVERT
C
      REAL DIC(3),DOC(3)
      REAL DE_T,DE_C,PCAL,DPFIT
      INTEGER HITC,CONVERGE
      REAL PX,PY,PZ
      REAL MAG(3)
C
      REAL CHI_TRACK
      REAL VMU(3),B_TOR
      INTEGER GZMUOT,LMUOT,IFLG,IERL
      INTEGER LMUON,KFIT,AMISS,SAKFIT
C
      DATA FIRST/.TRUE./
C ======================================================================
      IF(FIRST)THEN
C ****  Read MURECO.RCP file
        CALL EZPICK('MURECO_RCP')
        CALL EZGET('MUVERT',MUVERT,IERZ)
        CALL EZGET('KFIT',KFIT,IERZ)
        CALL EZRSET
        CALL EZPICK ('SAMUS_UTIL_PARAM')
        CALL EZGET('SAKFIT',SAKFIT,IERZ)
        IF( IERZ.NE.0 ) SAKFIT = 2
        FIRST=.FALSE.
      ENDIF
      IFIT=0
C-----------------------------------------------------------------------
C    number of muon tracks from MTRH
C ======================================================================
      CALL GTMTRH(NTRACKS)
      IF(NTRACKS .EQ. 0)GOTO 999
C=======================================================================
C
      IMTRK = 0
      DO 666 ITRAK=1,NTRACKS
        LMUOT = GZMUOT(ITRAK)
        IFLG = IQ(LMUOT + 7)
        IF(IFLG .GT. 2) GOTO 666
C
        AMISS = IQ(LMUOT+4)
C<<
        CALL BKMUON(0,LMUON)
        IMTRK = IQ(LMUON-5)
C<<
        IERL = 0
        IF (IQ(LMUOT+3).EQ.13 .OR. IQ(LMUOT+3).EQ.14) THEN
            !
            ! For SAMUS: Run K.F., create MUKF and fill MFIT
            ! from MUKF
          IF( SAKFIT.GE.1 ) CALL SAKMFIT(ITRAK,IMTRK,IERL)
          IF( SAKFIT.GE.2 ) CALL SA_MFIT_FILL(ITRAK,IMTRK,IERL)
        END IF
C<<
        IF( IERL.NE.1 ) THEN
            !
            ! Non-SAMUS or SAMUS and KF failed or KF not allowed
          IF (KFIT .EQ. 0) THEN
            CALL COP_MUOT(MUVERT,ITRAK,HITA,HITB,HITC,NFITDA,NFITDBC,
     &            QUAD,MAG,DIC,DOC,VMU,PCAL,DPFIT,DE_T,DE_C,PX,PY,PZ,
     &            CONVERGE,CHI_TRACK,B_TOR)
          ENDIF
          CALL MFIT_FILL(IMTRK,ITRAK,HITA,HITB,HITC,NFITDA,NFITDBC,
     &          QUAD,MAG,DIC,DOC,VMU,PCAL,DPFIT,DE_T,DE_C,PX,PY,PZ,
     &          CONVERGE,CHI_TRACK,B_TOR,AMISS,IFAIL)
        END IF
C-----------------------------------------------------------------C
  666 CONTINUE
      IFIT = 0
  999 CONTINUE
      CALL EZRSET
      RETURN
      END
