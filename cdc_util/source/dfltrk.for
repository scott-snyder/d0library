      SUBROUTINE DFLTRK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-        fill histograms associated with  a full track
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: called by DFLHST
C-
C-   Created  22-AUG-1988   Qizhong Li-Demarteau
C-   Updated   6-JUL-1989   Qizhong Li-Demarteau    clean up 
C-   Updated  27-JUL-1989   Qizhong Li-Demarteau  use modified bank DTRH 
C-   Updated  11-SEP-1990   Qizhong Li-Demarteau  added dE/dX histogram 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLTRK.INC'
C
      LOGICAL HSTCHD, HSTCHI, HSTCHZ, HSTDLR, HSTEFF, HSTSWR, HSTDSC
      LOGICAL HSTFLG, DLMDLP, HSTDEX
      INTEGER ID, HIDCHD, HIDSWR, HIDDLR, HISID1, HISID2, HIDTRR
      INTEGER HIDDS2, HIDDSC, HIDDM2, HIDDSM, HIDDP2, HIDDSP
      INTEGER NID00, NID01, NID02, NID03, NID04, NID05, NID06(4)
      INTEGER NID07, NID08
      INTEGER LAYER, LAY, SEC, WIR, LHIT, NUMHIT, ISIDE, HLABEL
      INTEGER NTRK, NSEG, NFADC, NWTRK, NZTRK, NSEGMT
      INTEGER IPHIT, IPTR, PLDTRK, PLDTTH, KPDSEC, PLDTSG
      INTEGER PDCDA, PDCDA1, PDCDA2, PTRESS
      INTEGER I, J, WRFLAG, DLFLAG, OUTWIR(8), NID350(8)
      INTEGER NRUN, NEVT
      REAL    YR, WRRES, WRRESS, ZPOS, CHI2XY, CHI2RZ, DLRES, DEDX
      REAL    AREA, AREA1, AREA2, AREAAV, AREASW, RATE1, RATE2
      REAL    PHI, THETA, ZBEAM, R0, Z0
C
      DATA OUTWIR/0,6,7,13,14,20,21,27/
C
C----------------------------------------------------------------------
C
C get flags for booked histograms first
      CALL DTSCHD(HSTCHD)
      CALL DTSCHI(HSTCHI)
      CALL DTSCHZ(HSTCHZ)
      CALL DTSDLR(HSTDLR)
      CALL DTSDSC(HSTDSC,DLMDLP)
      CALL DTSEFF(HSTEFF)
      CALL DTSSWR(HSTSWR)
      CALL DTSDEX(HSTDEX)
C 
      LDGEH = LC(LSCDC-4)
      NTRK = 0
      IF (LDTRH .GT. 0) NTRK = IQ(LDTRH+2)
      IF (HSTEFF) THEN
        CALL HFF1(1000,NID00,FLOAT(NTRK),1.) ! fill # of full tracks
        DO 1001 LAYER = 0,3
          ID = 1006 + LAYER
          NSEG = 0
          IF (LDTRH .GT. 0) NSEG = IQ(LDTRH+3+LAYER)
          CALL HFF1(ID,NID06(LAYER+1),FLOAT(NSEG),1.) 
C                                        !  fill # of segments
 1001   CONTINUE
      ENDIF  
      IF (LDTRH .LE. 0) GOTO 999
      PLDTRK = LQ(LDTRH-1)               ! pointer to bank 'DTRK'
      IF (PLDTRK.EQ.0) GOTO 999         ! no full track
C
 100  PLDTTH = LQ(PLDTRK-1)             ! pointer to bank 'DTTH'
      NWTRK = IQ(PLDTRK+2)              ! # of sen. wires on full track
      NZTRK = IQ(PLDTRK+5)              ! # of Z points on full track
      IF (HSTEFF) THEN
        CALL HFF1(1001,NID01,FLOAT(NWTRK),1.)
        CALL HFF1(1002,NID02,FLOAT(NZTRK),1.)
        PHI = Q(PLDTRK+6)
        CALL HF1(1013,PHI,1.0)
        THETA = Q(PLDTRK+9)
        IF (THETA .GT. 0) THEN
          CALL HF1(1010,THETA,1.0)
          R0 = Q(PLDTRK+10)
          Z0 = Q(PLDTRK+11)
          ZBEAM = Z0 - (R0/TAN(THETA))
          CALL HF1(1011,ZBEAM,1.0)
C          CALL HF2(1012,ZBEAM,THETA,1.0)
        ENDIF
      ENDIF  
C
      IF (HSTCHI) THEN
        CHI2XY = Q(PLDTRK+12)            ! chi sq of XY fit
        CHI2RZ = Q(PLDTRK+13)            ! chi sq of RZ fit
        IF (NWTRK.GT.2.AND.CHI2XY.NE.0.) THEN
          CHI2XY = CHI2XY/(NWTRK-2)         ! chisq / (degree of freedom)
          CALL HFF1(1100,NID07,CHI2XY,1.)
        ENDIF
        IF (NZTRK.GT.2.AND.CHI2RZ.NE.0. .AND. Q(PLDTRK+9) .GT. 0.0) THEN
          CHI2RZ = CHI2RZ/(NZTRK-2)         ! chisq / (degree of freedom)
          CALL HFF1(1101,NID08,CHI2RZ,1.)
        ENDIF
      ENDIF
C
      IF (HSTDEX) THEN
        DEDX = Q(PLDTRK + 20)             ! dE/dX
        CALL HF1(1110,DEDX,1.)
      ENDIF
C
      DO 200 I = 0,27
        WRFLAG = IBITS(IQ(PLDTRK+3),I,1)  ! XXflag=0 no hit on this wire;
        DLFLAG = IBITS(IQ(PLDTRK+4),I,1)  ! XXflag=1 this wire has been hit;
        IF (HSTEFF) THEN
          CALL HFF1(1005,NID05,FLOAT(I),1.) ! if efficiency = 1, ...
          IF (WRFLAG.NE.0) THEN
            CALL HFF1(1003,NID03,FLOAT(I),1.)
          ENDIF
          IF (DLFLAG.NE.0) THEN
            CALL HFF1(1004,NID04,FLOAT(I),1.)
          ENDIF
        ENDIF  
        HSTFLG = HSTCHD.OR.HSTCHZ.OR.HSTSWR.OR.HSTDSC
        HSTFLG = HSTFLG.OR.HSTDLR
        IF (.NOT.HSTFLG)  GOTO 200
        IF (WRFLAG.NE.0) THEN
          HLABEL = IQ(PLDTTH+1)                ! get hit label
          WRRES = Q(PLDTTH+2)                  ! residual 
          PLDTTH = PLDTTH+2
          LAY = IBITS(HLABEL, 16, 2)
          SEC = IBITS(HLABEL, 11, 5)
          WIR = IBITS(HLABEL,  8, 3)
          NUMHIT = IBITS(HLABEL,  1, 7)
          ISIDE  = IBITS(HLABEL,  0, 1)
          KPDSEC = LDSEC(SEC, LAY)
          LHIT   = IQ(KPDSEC + 3)
          NFADC  = IQ(KPDSEC + 2)
          IPHIT  = IQ(KPDSEC + NFADC + 4 + WIR) +
     &                     (NUMHIT-1) * LHIT + KPDSEC
          YR = Q(IPHIT + ISIDE + 2) - C(LC(LDGEH-3) +26+WIR) !
C                                          ! drift distance
          IF (HSTCHD) THEN
            PDCDA = LDCDA(SEC,LAY) + IQ(IPHIT+10) 
            ! point to DCDA of this hit
            AREA = Q(PDCDA+3)
C            HIDCHD = 3000 + I + SEC * 28
            HIDCHD = 3000 + I 
            CALL HFILL(HIDCHD,YR,AREA,1.)
          ENDIF  
          IF (HSTSWR) THEN
            HIDTRR = 1200 + I
            CALL HFILL(HIDTRR,YR,WRRES,1.)  ! residuals from full track
          ENDIF
C
          IF (HSTCHZ.OR.HSTDSC) THEN
            DO 201 J=1,8
              IF ((I.EQ.OUTWIR(J)).AND.(DLFLAG.NE.0)) THEN
                ZPOS = Q(IPHIT+4)                ! get Z position
                PDCDA = LDCDA(SEC,LAY) + IQ(IPHIT+10) 
C                                  ! point to DCDA of this hit
                AREASW = Q(PDCDA+3)
                PDCDA1 = LDCDA(SEC,LAY) + IQ(IPHIT+11) 
C                                  ! point to DCDA of DL- hit
                AREA1 = Q(PDCDA1+3)
                PDCDA2 = LDCDA(SEC,LAY) + IQ(IPHIT+12) 
C                                  ! point to DCDA of DL+ hit
                AREA2 = Q(PDCDA2+3)
                IF (HSTCHZ) THEN
                  RATE1 = AREA1/AREASW
                  HISID1 = 4000 + J + SEC * 8
                  CALL HBAVFL(HISID1,ZPOS,RATE1)  
C                                     ! <DL- charge / SW_o charge> vs Z
                  RATE2 = AREA2/AREASW
                  HISID2 = 4300 + J + SEC * 8
                  CALL HBAVFL(HISID2,ZPOS,RATE2)  
C                                     ! <DL+ charge / SW_o charge> vs Z
                ENDIF
                IF (HSTDSC) THEN
                  AREAAV = (AREA1 + AREA2) / 2.  
C                               ! take average of DL- and DL+
                  HIDDS2 = 4620 + J
C                   CALL HFILL(HIDDS2,AREASW,AREAAV)
                  CALL HFF2(HIDDS2,NID350(J),AREASW,AREAAV,1.)
                  HIDDSC = 4650 + J
                  CALL HBAVFL(HIDDSC,AREASW,AREAAV)
                  IF (DLMDLP) THEN
                    HIDDM2 = 4600 + J
                    CALL HFF2(HIDDM2,NID350(J),AREASW,AREA1,1.)
                    HIDDSM = 4630 + J
                    CALL HBAVFL(HIDDSM,AREASW,AREA1)
                    HIDDP2 = 4610 + J
                    CALL HFF2(HIDDP2,NID350(J),AREASW,AREA2,1.)
                    HIDDSP = 4640 + J
                    CALL HBAVFL(HIDDSP,AREASW,AREA2)
                  ENDIF  
                ENDIF  
              ENDIF
 201        CONTINUE
          ENDIF  
        ENDIF
 200  CONTINUE
C
      IF (HSTDLR) THEN
        DO 300 I = 0,27
          DLFLAG = IBITS(IQ(PLDTRK+4),I,1) 
          IF (DLFLAG.NE.0) THEN
            HLABEL = IQ(PLDTTH+1)        ! get hit label
            DLRES = Q(PLDTTH+2)          ! residuals from RZ track
            PLDTTH = PLDTTH+2
            LAY = IBITS(HLABEL, 16, 2)
            SEC = IBITS(HLABEL, 11, 5)
            WIR = IBITS(HLABEL,  8, 3)
            NUMHIT = IBITS(HLABEL,  1, 7)
            ISIDE  = IBITS(HLABEL,  0, 1)
            KPDSEC = LDSEC(SEC, LAY)
            LHIT   = IQ(KPDSEC + 3)
            NFADC  = IQ(KPDSEC + 2)
            IPHIT  = IQ(KPDSEC + NFADC + 4 + WIR) +
     &                      (NUMHIT-1) * LHIT + KPDSEC
            ZPOS = Q(IPHIT+4)                ! get Z position
            DO 301 J=1,8
              IF (I.EQ.OUTWIR(J)) THEN
C                HIDDLR = 1300 + J + SEC * 8
                HIDDLR = 1300 + J 
                CALL HFILL(HIDDLR,ZPOS,DLRES,1.)  
              ENDIF  
 301        CONTINUE 
          ENDIF  
 300    CONTINUE
      ENDIF  
C
      IF (LQ(PLDTRK).EQ.0) GOTO 999          ! return if last track
      PLDTRK = LQ(PLDTRK)                    ! pointer to next bank 'DTRK' 
      GOTO 100
C
  999 RETURN
      END
