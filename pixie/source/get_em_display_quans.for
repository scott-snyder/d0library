      SUBROUTINE GET_EM_DISPLAY_QUANS(LCLUS,QUANS,NAMQUANS,NQUANS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURN QUANTITIES OF INTEREST FOR EM CLUSTERS
C-                         FOR DISPLAY PURPOSES
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-MAR-1993   NORMAN A. GRAF
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,II
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LCLUS,LCACL,LCASH,LHMTR
      CHARACTER*10 NAMQUANS(50)
      INTEGER NQUANS
      REAL QUANS(50)
      INTEGER VSN,NCELLS
      REAL CHISQ,ISOLATION
C
      INTEGER IPHI_HOT(5),IETA_HOT(5),NTRACKS
      REAL EDPTH(5),PDPTH(5),ENERGY_HOT(5),DCLA
      REAL DIFF_PHI
C
      INTEGER LZTRK,LZFIT,LDTRK,LFDCT,LVTXT,LTRDT,LLYR
      INTEGER TRK_BIT,ITRK,TRACK_OFFSET
      REAL TRD_TRUNCATED_MEAN,TRD_ANODE(3),ETOT_LIKE,ETOT_LEFF
      REAL TRK_PHI,TRK_THETA,TRK_MIP,TRK_XPT,TRK_YPT,TRK_ZPT
      REAL VTX_PHI,VTX_THETA,VTX_MIP,VTX_XPT,VTX_YPT,VTX_ZPT
C----------------------------------------------------------------------
      VSN = IQ(LCLUS+1)
      NQUANS = 0
C      IF(VSN.LT.3) THEN
C        CALL ERRMSG('OLD BANK VERSION','GET_EM_QUANS',
C     &    'Bank version too old for this routine; rerun CAPHEL! ','W')
C        GOTO 999
C      ENDIF
      CALL VZERO(QUANS,50)
      LCACL = LQ(LCLUS-2)
      LCASH = LQ(LCACL-2)
      LHMTR = LQ(LCLUS-1)
C
      NCELLS = IQ(LCASH+2)
      CALL CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
C
      CHISQ = Q(LHMTR+7)
      IF(Q(LCLUS+17).GT.0) THEN
        ISOLATION = (Q(LCLUS+16) - Q(LCLUS+17))/Q(LCLUS+17)
      ELSE
        ISOLATION = (Q(LCLUS+16) - Q(LCLUS+6))/Q(LCLUS+6)
      ENDIF
C
C ****  Fill in arrays...
C
C      NQUANS = NQUANS+1
C      QUANS(NQUANS) = Q(LCLUS+3)     !EX
C      NAMQUANS(NQUANS) = 'EX'
C      NQUANS = NQUANS+1
C      QUANS(NQUANS) = Q(LCLUS+4)     !EY
C      NAMQUANS(NQUANS) = 'EY'
C      NQUANS = NQUANS+1
C      QUANS(NQUANS) = Q(LCLUS+5)     !EZ
C      NAMQUANS(NQUANS) = 'EZ'
      NQUANS = NQUANS+1
      QUANS(NQUANS) = Q(LCLUS+6)     !E
      NAMQUANS(NQUANS) = 'ENERGY'
      NQUANS = NQUANS+1
      QUANS(NQUANS) = Q(LCLUS+7)     !ET
      NAMQUANS(NQUANS) = 'ET'
C      NQUANS = NQUANS+1
C      QUANS(NQUANS) = IETA_HOT(3)    !TOWER IETA OF HOTTEST CELL IN EM3
C      NAMQUANS(NQUANS) = 'IETA'
C      NQUANS = NQUANS+1
C      QUANS(NQUANS) = IPHI_HOT(3)    !TOWER IPHI OF HOTTEST CELL IN EM3
C      NAMQUANS(NQUANS) = 'IPHI'
      NQUANS = NQUANS+1
      QUANS(NQUANS) = Q(LCLUS+9)            ! eta
      NAMQUANS(NQUANS) = 'ETA'
      NQUANS = NQUANS+1
      QUANS(NQUANS) = Q(LCLUS+10)           ! phi
      NAMQUANS(NQUANS) = 'PHI'
      NQUANS = NQUANS+1
      QUANS(NQUANS) = Q(LCLUS+8)           ! theta
      NAMQUANS(NQUANS) = 'THETA'
C      NQUANS = NQUANS+1
C      QUANS(NQUANS) = Q(LCLUS+23)           !X OF SHOWER CENTER
C      NAMQUANS(NQUANS) = 'X COG'
C      NQUANS = NQUANS+1
C      QUANS(NQUANS) = Q(LCLUS+24)           !Y
C      NAMQUANS(NQUANS) = 'Y COG'
C      NQUANS = NQUANS+1
C      QUANS(NQUANS) = Q(LCLUS+25)           !Z
C      NAMQUANS(NQUANS) = 'Z COG'
      NQUANS = NQUANS+1
      QUANS(NQUANS) = NCELLS
      NAMQUANS(NQUANS) = 'NCELLS'
C      NQUANS = NQUANS+1
C      QUANS(NQUANS) = Q(LCLUS+15)           !TOTAL E CORECLUS
C      NAMQUANS(NQUANS) = 'CORE ETOT'
C      NQUANS = NQUANS+1
C      QUANS(NQUANS) = Q(LCLUS+16)           !TOTAL E IS0CLUS
C      NAMQUANS(NQUANS) = 'ISOL ETOT'
      NQUANS = NQUANS+1
      QUANS(NQUANS) = ISOLATION
      NAMQUANS(NQUANS) = 'ISOLATION'
      NQUANS = NQUANS+1
      QUANS(NQUANS) = CHISQ
      NAMQUANS(NQUANS) = 'TRUN CHISQ'
C
      IF (IQ(LCLUS-4).EQ.'PELC') THEN
C
        LZTRK = LQ(LCLUS-3)
        IF(LZTRK.LE.0) THEN
          CALL ERRMSG('NOZTRACK','GET_EM_QUANS',
     &      'No Ztrack associated with electron ! ','W')
          GOTO 999      ! No Ztrack associated with the electron
        ENDIF
        LZFIT = LQ(LZTRK-1)             ! Link to global fit
        IF (LZFIT .LE. 0) THEN          ! This should not happen
          CALL ERRMSG('NOZFIT','GET_EM_QUANS',
     &      'No ZFIT info associated with electron ZTRK ! ','W')
          GOTO 999      ! No ZFIT info
        ENDIF
        TRK_BIT = 0
        LVTXT = LQ(LZTRK-6)
        LDTRK = LQ(LZTRK-7)
        LFDCT = LQ(LZTRK-8)
        LTRDT = LQ(LZTRK-9)
C
        NQUANS = NQUANS+1
        QUANS(NQUANS) = Q(LCLUS+21)   ! NZTRKS IN ROAD
        NAMQUANS(NQUANS) = 'NZTRKS'
        NQUANS = NQUANS+1
        QUANS(NQUANS) = Q(LCLUS+22)   ! DISTANCE OF CLOSEST APPROACH
        NAMQUANS(NQUANS) = 'DCLA'
        NQUANS = NQUANS+1
        QUANS(NQUANS) = DIFF_PHI(Q(LCLUS+10),Q(LZFIT+10)) ! PHI OF ZFIT TRACK
        NAMQUANS(NQUANS) = 'TRK DPHI'
        NQUANS = NQUANS+1
        QUANS(NQUANS) = Q((LCLUS+11)-Q(LZFIT+13))   ! THETA
        NAMQUANS(NQUANS) = 'TRK DTHETA'
C
C
        IF(LDTRK.GT.0) THEN
          TRK_MIP   = Q(LDTRK+20)
          NQUANS = NQUANS+1
          QUANS(NQUANS) = TRK_MIP
          NAMQUANS(NQUANS) = 'CDC MIP'
        ELSEIF(LFDCT.GT.0) THEN
          TRK_MIP   = Q(LFDCT+20)
          NQUANS = NQUANS+1
          QUANS(NQUANS) = TRK_MIP
          NAMQUANS(NQUANS) = 'FDC MIP'
        ENDIF
        IF(LVTXT.GT.0) THEN
          NQUANS = NQUANS+1
          VTX_MIP   = Q(LVTXT+20)
          QUANS(NQUANS) = TRK_MIP
          NAMQUANS(NQUANS) = 'VTX MIP'
        ENDIF
C
        IF(LTRDT.NE.0) THEN
          NQUANS = NQUANS+1
          ETOT_LIKE = Q(LTRDT+6)        ! Likelihood based on total
                                        ! anode energy
          ETOT_LEFF = Q(LTRDT+16)       ! Efficiency derived from likelihood
          TRD_TRUNCATED_MEAN = Q(LTRDT+5)
          QUANS(NQUANS) =TRD_TRUNCATED_MEAN
          NAMQUANS(NQUANS) = 'TRD ENERGY'
          NQUANS = NQUANS+1
          QUANS(NQUANS) = ETOT_LIKE
          NAMQUANS(NQUANS) = 'LIKELIHOOD'
        ENDIF
      ENDIF
C
  999 RETURN
      END
