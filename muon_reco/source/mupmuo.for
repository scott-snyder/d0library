      SUBROUTINE MUPMUO(IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill PMUO bank
C-
C-   Inputs  : (None)
C-   Outputs : IERR     I      error code  (not yet defined)
C-   Controls:
C-
C-   Created    12-JUN-1990   Shuichi Kunori
C-   DH fix looping 11/90
C-   Modified   17-DEC-1990   Shahriar Abachi    Filled with new muon contents
C-   Modified   30-JAN-1991   Shahriar Abachi    Isolation added and format
C-                                               correcteed
C-   Modified   15-FEB-1991   Shahriar Abachi    CD tracks added
C-   Updated    08-APR-1991   Shahriar Abachi    Angle between MU&CD introduced
C-   Updated    03-JUL-1991   Shahriar Abachi    Link to ZTRK added
C-   Updated    10-OCT-1991   Shahriar Abachi    Only good muons accpeted.
C-   Updated    11-OCT-1991   Shahriar Abachi    ZTRK link added to ZTRK bank
C-   Updated    12-DEC-1991   Shahriar Abachi    new structure with global fit
C    D. HEDIN 12-22-91, fix checks of MUON bank
C-   Updated    21-FEB-1992   Shahriar Abachi    # of trks put in PARH bank
C-   Updated    09-JUN-1992   Shahriar Abachi    IFLG upto 2 accepted
C    DH 7-31-92  add MUOT word IFW2 to PMUO word 44, FIX TOF ERROR
C    SK 10-14-92 expand to 100 words.
C    SK 18-DEC-92 replace impact parameter routine by new muimp_bnb
C                 and calorimeter energy routine by muot_cal_match1.
C                 Both routines are in D0$MUON_RECO.
C-   Updated    06-JAN-1993   Shahriar Abachi    Vertex number added,MUCPLN
C-                                               argument corrected.
C-   Updated    09-JAN-1993   Shahriar Abachi    Trigger words filled up
C-   Updated    27-JAN-1993   Shahriar Abachi    New pmuo changes
C-   Updated    18-mar-1993   Cecilia Gerber skip calorimeter part if doing
C-                            global fit from DSTs.
C-   Updated    20-APR-1993   Shahriar Abachi    Word 54 used for vertex used
C-   Updated  14-FEB-1994   Daria Zieminska  add MTC results
C-   Updated  13_APR-1994   Elizabeth Gallas add track energy (word 90)
C-   Updated  17_May-1994   D. Wood  Update version number to 5
C-   Updated  29-JUN-1994   D. Wood changes for refitting
C-   Updated  11-Jul-1994   M. Fortner, use MTRG bank for trig words
C-   Updated  06-Feb-1995   D. Wood, use IND_MUON in MSCINT_TOF
C-   Updated  28-Jul-1995   D. Wood, moved call to MUCPLN to work for MUREFIT=3
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:MUPHYS.INC'
      INCLUDE 'D0$LINKS:IZMTCA.LINK'
      INCLUDE 'D0$LINKS:IZMRFT_MUON.LINK'
C  -- input/output arguments...
      INTEGER IERR
C  -- local variables...
      INTEGER IMUON,IPMUO,I,LMUON,LPMUO,LMUOT,NS1,NS2
      INTEGER GZMUON,IFLG,LZTRK,LPARH,GZPARH,IFMUOT,IT,LMTCA
      INTEGER GZMUOT,NMUOT,IMUOT,LMUOTX,NDOF,IRC_MUTFLT
      INTEGER IAPLN,IFPLN,IVERT_MTC,MTCREFIT
      INTEGER GZMTRG,IL1WA,IL1SA,ISTAT,IPTHI,IPTLO,IXTRA,JERR
      REAL    DELT,EDELT,CHISQ
      REAL SC_TOF,EXPECTED_TOF
      INTEGER MUREFIT,LPMUO_OLD,ND,ND_OLD,ND_NEW,LMRFT,IND_MUON
      REAL DIMP_BEND,DIMP_NONB,ZVERT_MTC


      INTEGER IER
      LOGICAL FIRST
      REAL TOT_E(4),HAD_E(4),TOT_E_OPP(4),HAD_E_OPP(4)

      EXTERNAL GZMUON,GZMUOT,GZMTRG
      DATA FIRST /.TRUE./
C
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('MURECO_RCP')
        CALL EZGET('MUREFIT',MUREFIT,IER)
        CALL EZGET('MTCREFIT',MTCREFIT,IER)
        CALL EZRSET
      END IF
C     -- reset the error code...
      IERR=0
      IT = 0
C
C     -- reserve temporary reference link area...
      CALL GRLINK('MUMUON',IMUON)
      CALL GRLINK('MUPMUO',IPMUO)
C
C     -- number of MUOT tracks...
      CALL GTMTRH(NMUOT)
C
C  Loop over MUON banks.
C  =====================
C
      LRLINK(IMUON) = GZMUON(0)
C

 1000 CONTINUE
      IF(LRLINK(IMUON).NE.0) THEN
C
C - Accept good MUOT muons if copy of MUOT is used
C
        LMUON = LRLINK(IMUON)
        NS1 = IQ(LMUON - 2)
        IFMUOT = IQ(LMUON + 4)
        LMUOT = LQ(LMUON - NS1 - 1)
        IF(LMUOT .GT. 0 .AND. IFMUOT .EQ. 2) THEN
          IFLG = IQ(LMUOT + 7)
          IF(IFLG .GT. 2) THEN
            GOTO 998
          ENDIF
        ENDIF

C      -- check the status flag...
C        IF(IQ(LRLINK(IMUON)+10).EQ.0) THEN
C
C        -- book PMUO bank...
C
        CALL BKPMUO(0,0,LRLINK(IPMUO))
        IT = IT + 1
C
C        -- store muon variables...
        LMUON=LRLINK(IMUON)
        LPMUO=LRLINK(IPMUO)
C
C        -- reference pointers, MUOT,MUON,VERT...
C
        NS1=IQ(LMUON-2)
        NS2=IQ(LPMUO-2)
C             (( MUOT ))
        LMUOT = LQ(LMUON-NS1-1)
        LQ(LPMUO-NS2-1) = LMUOT
C             (( MUON ))
        LQ(LPMUO-NS2-2)=LMUON
C             (( VERT ))
        LQ(LPMUO-NS2-3)= LQ(LMUON-NS1-2)
C             (( ZTRK ))
        LZTRK = LQ(LMUON-NS1-3)
        IF (LZTRK.GT.0) THEN
          LQ(LPMUO-NS2-4)= LZTRK
C - Fill ZTRK bank with the associated muon track
          IF(LQ(LZTRK - 3) .EQ. 0) THEN
            LQ(LZTRK - 3) = LPMUO
          ELSE
            LQ(LZTRK - 3) = 0
          ENDIF
        ENDIF
C for refitting, copy old PMUO information
        IF(MUREFIT.GE.2) THEN
          LMRFT = LQ(LMUON-IZMRFT_MUON)
          IF(LMRFT.GT.0) THEN
            LPMUO_OLD = LQ(LMRFT-3)
            IF(LPMUO_OLD.GT.0) THEN
              ND_OLD = IQ(LPMUO_OLD-1)
              ND_NEW = IQ(LPMUO-1)
              ND = MIN(ND_OLD,ND_NEW)
              CALL UCOPY(IQ(LPMUO_OLD+1),IQ(LPMUO+1),ND)
            ENDIF
          ENDIF
        ENDIF
C
        IQ(LPMUO+1)=5  !Bank version
C        -- particle ID...
        IQ(LPMUO+2)=IQ(LMUON+2)
C        -- dedx flag
        IQ(LPMUO+3)=IQ(LMUON+3)
C        -- method of calculation
        IQ(LPMUO+4)=IQ(LMUON+4)
C        -- vertex flag
        IQ(LPMUO+5) = IQ(LMUON + 8)
C        -- number of CD tracks
        IQ(LPMUO+6) = IQ(LMUON+5)
C
        IQ(LPMUO+7) = IQ(LMUON+9)
        IQ(LPMUO+8) = IQ(LMUON+10)
        IQ(LPMUO+9) = IQ(LMUON+6)
C
C        -- px,py,pz,p,pt,theta,eta,phi...
        CALL UCOPY(Q(LMUON+11),Q(LPMUO+10),8)
C        -- sigma(px,py,pz,p,pt),chisq
        CALL UCOPY(Q(LMUON+26),Q(LPMUO+18),6)
C        -- vertex x,y,z...
        CALL UCOPY(Q(LMUON+37),Q(LPMUO+25),3)
C        -- isolation
        CALL UCOPY(Q(LMUON+32),Q(LPMUO+28),5)
C        -- E loss expected & observed
        Q(LPMUO+33) = Q(LMUON+24)
        CALL UCOPY(Q(LMUON+43),Q(LPMUO+34),3)
C        -- Angle between muon and nearest CD track (degrees)
        CALL UCOPY(Q(LMUON+40),Q(LPMUO+37),3)
C        -- Cone size for CD finding
        Q(LPMUO+40) = Q(LMUON+23)
C        -- Impact parameter
        Q(LPMUO+41) = Q(LMUON+53)
        Q(LPMUO+42) = Q(LMUON+54)
CC        IF(IQ(LMUON+4) .NE. 1) Q(LPMUO+42) = Q(LPMUO+41)
C - Energy loss in toroid
        Q(LPMUO+43) = Q(LMUON+25)
C
C - User flag word.
        IQ(LPMUO+45) = 0
C - Vertex number
        IQ(LPMUO+54) = IQ(LMUON + 63)
C - impact parameters.
        CALL MUIMP_BNB(LMUOT,DIMP_BEND,DIMP_NONB)
        Q(LPMUO+56)=DIMP_BEND
        Q(LPMUO+57)=DIMP_NONB

        Q(LPMUO+58)= 0.
        Q(LPMUO+59)= 0.
C - coordinates and direction cosine at vertex, before/after toroid
C          at vertex.
        Q(LPMUO+60)=0.0
        Q(LPMUO+61)=0.0
        Q(LPMUO+62)=0.0
        Q(LPMUO+63)=0.0
        Q(LPMUO+64)=0.0
        Q(LPMUO+65)=0.0
C          behind calorimeter.
        Q(LPMUO+66)=Q(LMUOT+ 8)
        Q(LPMUO+67)=Q(LMUOT+ 9)
        Q(LPMUO+68)=Q(LMUOT+10)
        Q(LPMUO+69)=Q(LMUOT+14)
        Q(LPMUO+70)=Q(LMUOT+15)
        Q(LPMUO+71)=Q(LMUOT+16)
C          behind toroid.
        Q(LPMUO+72)=Q(LMUOT+11)
        Q(LPMUO+73)=Q(LMUOT+12)
        Q(LPMUO+74)=Q(LMUOT+13)
        Q(LPMUO+75)=Q(LMUOT+17)
        Q(LPMUO+76)=Q(LMUOT+18)
        Q(LPMUO+77)=Q(LMUOT+19)
C - energy in the calorimeter...
        DO I=1,5
          Q(LPMUO+I+77) = Q(LMUON+I+55)
        ENDDO
        DO I=1,2
          Q(LPMUO+I+82) = Q(LMUON+I+60)
        ENDDO
        Q(LPMUO+85) = 0.0
C
C --- store Scintillator TOF from MTOF bank ---
C
        IND_MUON = IQ(LMUON-5)
        CALL MSCINT_TOF(IND_MUON,SC_TOF,EXPECTED_TOF)
        Q(LPMUO+52)=SC_TOF
        Q(LPMUO+53)=EXPECTED_TOF
C
        IF (MUREFIT.EQ.2) GO TO 500
********************************************direct access to MUOT*******
CCCCCCCCCCCCCCCC        Q(LPMUO+24) = Q(LMUON+46)
        DO IMUOT=1,NMUOT
          LMUOTX = GZMUOT(IMUOT)
          IF(LMUOTX.EQ.LMUOT) THEN
C               -- float the track
            IF(MUREFIT.LT.3 .OR. BTEST(IQ(LMUOT+5),21)) THEN
              CALL MUTFLT(IMUOT,DELT,EDELT,CHISQ,NDOF,IRC_MUTFLT)
C               -- store floated time shift (-9999. if failed)
              IF(IRC_MUTFLT.GE.0) THEN
                Q(LPMUO+24) = DELT
              ELSE
                Q(LPMUO+24) = -9999.
              ENDIF
            ENDIF
          ENDIF
        ENDDO                         ! end of loop over MUOT banks
C - IFW2 quality flag
        IF(MUREFIT.GE.2) THEN
C set refit bit if appropriate
          IQ(LMUOT+5) = IBSET(IQ(LMUOT+5),22)
        ENDIF
        IQ(LPMUO+44) = IQ(LMUOT+5)
C - Hits on track, A,B,C and hits in track fit, A, B, C.
        CALL MUCPLN(LMUOT,IQ(LPMUO+46),IQ(LPMUO+47),IAPLN,IFPLN)
        IF(MUREFIT.GE.2) GOTO 500
C - Level 1, L1.5 trigger words
        IF (GZMTRG(0).EQ.0) CALL MUANLZ(JERR,6,0,4)
        CALL MOTWRD(IL1WA,IL1SA,ISTAT,IPTHI,IPTLO,IXTRA)
        IQ(LPMUO+48) = IL1WA
        IQ(LPMUO+49) = IL1SA
        IQ(LPMUO+50) = ISTAT
        IQ(LPMUO+51) = IPTHI
*********************************************end of MUOT access*********
C  words 52,53   (scint. tof)  already filled
C
        CALL MUOT_CAL_MATCH1(LMUOT,TOT_E,HAD_E,TOT_E_OPP,HAD_E_OPP
     &     ,IER)
        IF(IER.EQ.0) THEN
          DO I=1,4
            Q(LPMUO+I+85)=TOT_E_OPP(I)
          ENDDO
        ELSE
          DO I=86,89
            Q(LPMUO+I)=0.0
          ENDDO
        ENDIF
  500   CONTINUE
C  Store MTC results
C
        LMTCA=LQ(LMUON-IZMTCA)
        IF (LMTCA.GT.0)THEN
          Q(LPMUO+91)=Q(LMTCA+13)  ! tres
          Q(LPMUO+92)=Q(LMTCA+24)  ! tres_v
          Q(LPMUO+93)=Q(LMTCA+14)  ! fract
          Q(LPMUO+94)=Q(LMTCA+15)  ! hfract
C          Q(LPMUO+95)=Q(LMTCA+16)  ! ghfract
          Q(LPMUO+96)=Q(LMTCA+17)  ! echi
          Q(LPMUO+97)=Q(LMTCA+25)  ! en3
          Q(LPMUO+98)=Q(LMTCA+27)  ! efract_h(1)
          Q(LPMUO+99)=FLOAT(IQ(LMTCA+6)) ! LYRMU
          Q(LPMUO+100)=Q(LMTCA+34) !  ECHI2
C- EG added 13-APR-94
          Q(LPMUO+90)=Q(LMTCA+35) !  ETRACK
        END IF
C new MTC vertex information
        IF(MTCREFIT.GT.0) THEN
          CALL MU_VERTEX_ONE(LPMUO,IVERT_MTC,ZVERT_MTC)
          IQ(LPMUO+55) = IVERT_MTC
          Q(LPMUO+95) = ZVERT_MTC
        ELSE
          IQ(LPMUO+55) = IQ(LMUON + 63)
          Q(LPMUO+95) = -202.
        ENDIF
  998   CONTINUE
C        -- update the bank pointer...
        LRLINK(IMUON)=LQ(LRLINK(IMUON))
        GO TO 1000
      ENDIF
C
      LPARH = GZPARH()
      IQ(LPARH + 2) = IT
C
C     -- release temporary reference link area...
C
      CALL RRLINK('MUMUON',IMUON)
      CALL RRLINK('MUPMUO',IPMUO)

  999 RETURN
      END
