      SUBROUTINE L2EMFL (TGETA,TGPHI,IETA,IPHI,LYR,ET_VTX0,SUMEM,RA1,
     &  RA12,RA3,RA4,RBF,SIGMA3,SIGMA5,SIG3_MID,SH13,SH24,SH35,SH57,
     &  CONE_R,CONE_FRAC,DETA_TR,DPHI_TR,CHECK_TRACK,IFAILED,PAR_SET,
     &  AETA,APHI,XYZ_CLUS,ET_ZCORR,CUTBITS,ZETA)

C----------------------------------------------------------------------
C-
C-   Purpose and Methods : BOOK AND FILL L2EM BANK
C-
C-   Inputs  : BANK CONTENTS
C-   Outputs : L2EM bank
C-   Controls: CHECK_TRACK (help in interpreting IFAILED codes)
C-
C-   Created  17-MAR-1992   Yi Xia
C-   Modified 6-AUG-1992    S Fahey added fine position and Et with Z corr
C-   Modified 2-SEP-1992    S Fahey added CUTBITS bit mask
C-   Modified 7-DEC-1992    James T. McKinley add vertex corrected eta to
C-                          end of bank. Also move ENDIF for IF(LL2EM.EQ.0)
C-                          above zeroing variables to guarantee that things
C-                          get zeroed even if bank doesn't get booked.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER TGETA,TGPHI,IETA,IPHI,LYR,IFAILED,PAR_SET,CUTBITS
      REAL ET_VTX0,SUMEM,RA1,
     &  RA12,RA3,RA4,RBF,SIGMA3,SIGMA5,SIG3_MID,SH13,SH24,SH35,SH57,
     &  CONE_R,CONE_FRAC,DETA_TR,DPHI_TR,AETA,APHI,XYZ_CLUS(3),
     &  ET_ZCORR,ZETA
      LOGICAL CHECK_TRACK,FIRST
      INTEGER I,LL2EM
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      CALL BKL2EM(1,LL2EM)   ! book L2EM bank (for now one per candidate bank)
      IF (LL2EM .EQ. 0) THEN
        CALL ERRMSG('L2EM','L2EM','no L2EM bank created','E')
      ELSE
        IQ(LL2EM + 4) = TGETA
        IQ(LL2EM + 5) = TGPHI
        IQ(LL2EM + 6) = IETA
        IQ(LL2EM + 7) = IPHI
        IQ(LL2EM + 8) = LYR
        Q(LL2EM +  9) = ET_VTX0
        Q(LL2EM + 10) = SUMEM
        Q(LL2EM + 11) = RA1
        Q(LL2EM + 12) = RA12
        Q(LL2EM + 13) = RA3
        Q(LL2EM + 14) = RA4
        Q(LL2EM + 15) = RBF
        Q(LL2EM + 16) = SIGMA3
        Q(LL2EM + 17) = SIGMA5
        Q(LL2EM + 18) = SIG3_MID
        Q(LL2EM + 19) = SH13
        Q(LL2EM + 20) = SH24
        Q(LL2EM + 21) = SH35
        Q(LL2EM + 22) = SH57
        Q(LL2EM + 23) = CONE_R
        Q(LL2EM + 24) = CONE_FRAC
        Q(LL2EM + 25) = DETA_TR
        Q(LL2EM + 26) = DPHI_TR
C
C... +27 is the number of tracks found: 
C         0 want a track and didn't find one
C         1 want a track and found one
C        -1 didn't want a track and found one
C        999 didn't look for a track
        IF (.NOT. CHECK_TRACK) THEN
          IQ(LL2EM + 27) = 999     !no inquire on track match
        ELSE
          IF (IFAILED .EQ. 0) THEN
            IQ(LL2EM + 27) = 1    !complete success
          ELSEIF (IFAILED .EQ. 100) THEN
            IQ(LL2EM + 27) = 0     !no track match when desired
          ELSEIF (IFAILED .EQ. 101) THEN
            IQ(LL2EM + 27) = -1   !found undesired track
          ENDIF
        ENDIF
        IQ(LL2EM + 28) = IFAILED
        IQ(LL2EM + 29) = PAR_SET
        Q(LL2EM + 30) = AETA
        Q(LL2EM + 31) = APHI
        DO I = 1,3
          Q(LL2EM + 31 + I) = XYZ_CLUS(I)
        ENDDO
        Q(LL2EM + 35) = ET_ZCORR
        IQ(LL2EM + 36) = CUTBITS
        Q(LL2EM + 37) = ZETA
      ENDIF
C
C...now clear these values to avoid confusion on the next candidate
      TGETA = 0
      TGPHI = 0
      IETA = 0
      IPHI  = 0
      LYR = 0
      ET_VTX0 = 0
      SUMEM = 0
      RA1 = 0.
      RA12  = 0.
      RA3 = 0.
      RA4 = 0.
      RBF = 0.
      SIGMA3  = 0.
      SIGMA5  = 0.
      SIG3_MID  = 0.
      SH13  = 0.
      SH24  = 0.
      SH35  = 0.
      SH57  = 0.
      CONE_FRAC = 0.
      DETA_TR = 0.    !now these are recalculated every track attempt
      DPHI_TR = 0.
      IFAILED = 0
      AETA = 0.
      APHI = 0.
      CALL VZERO(XYZ_CLUS,3)
      ET_ZCORR = 0.
      ZETA = 0.
C
C...DO NOT zero CONE_R,PAR_SET,CHECK_TRACK,CUTBITS!!!! 
C... these variables are not recomputed every candidate 
  999 RETURN
      END
