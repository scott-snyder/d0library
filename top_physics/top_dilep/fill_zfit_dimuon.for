      SUBROUTINE FILL_ZFIT_DIMUON
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill Zfit informatoin for dimuon events
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-APR-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL first
      SAVE first
      DATA first / .true. /
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:PI.DEF'
      include 'd0$inc:top_DILEP_ANALYSIS.INC'
      INTEGER I,LZFIT2,GZFIT2,IFLAG,IER,NTAGS,IERR
      REAL XDATA(1000), FITPAR(14)
      REAL MU_VECTS(3,2),SIGMA_MU(2),MET_VECT(2),SIGMA_MET(2,2),SCET
      LOGICAL DO_Z_FIT_MUMU
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('top_dilep_rcp')
        CALL EZGET('DO_Z_FIT_MUMU',DO_Z_FIT_MUMU,IER)
        CALL EZERR(IER)
      ENDIF
C
C ****  get links for all objects
C
      CALL gtslink('ISOLMUON', nwant_muon_tght,
     &  ntot_muon_tght, muon_link_tght)
C

      if (nmuon_tght.lt.2) goto 999

      IF (DO_Z_FIT_MUMU) THEN
      IFLAG = 1
      DO I=1,2
        MU_VECTS(1,I) = Q(muon_link_tght(I)+15)
        MU_VECTS(2,I) = Q(muon_link_tght(I)+17)
        MU_VECTS(3,I) = 1.0/Q(muon_link_tght(I)+13)
C *** use rcp inputs for these
        SIGMA_MU(I) = 0.0
      ENDDO
      MET_VECT(1) = metx
      MET_VECT(2) = mety
      SIGMA_MET(1,1) = meterx
      SIGMA_MET(2,2) = metery
      SIGMA_MET(1,2) = meterxy
      SIGMA_MET(2,1) = meterxy
      SCET = mets
C
C
      IF(ABS(MU_VECTS(2,1)-MU_VECTS(2,2)).GT.0.001) THEN
        CALL Z_FIT_MUMU(IFLAG,MU_VECTS,SIGMA_MU,MET_VECT,SIGMA_MET,
     &  SCET,FITPAR,IERR)
      ELSE
        IERR = 1
      ENDIF
      IF(IERR.NE.0) GOTO 850
      xz_fit(27)  =  FITPAR(3)
      xz_fit(28)  =  FITPAR(4)
      xz_fit(29)  =  FITPAR(5)
      xz_fit(30)  =  FITPAR(6)
      xz_fit(31)  =  FITPAR(7)
      xz_fit(32)  =  FITPAR(11)
      xz_fit(33)  =  FITPAR(12)
      xz_fit(34)  =  FITPAR(13)
      xz_fit(35)  =  FITPAR(14)
      ENDIF
C
  850 CONTINUE
C
C *** Z fitting
C *** using FIT_TWO package (Bhat etal...)
C *** get info from fit2.zeb
C
      LZFIT2 = GZFIT2(0)
      IF (LZFIT2.GT.0) THEN
        xz_fit(1)  =Q(LZFIT2+5)
        xz_fit(2)  =Q(LZFIT2+6)
        xz_fit(3)  =Q(LZFIT2+7)
        xz_fit(4)  =Q(LZFIT2+8)
        xz_fit(5)  =Q(LZFIT2+9)
        xz_fit(6)  =Q(LZFIT2+10)
        xz_fit(7)  =Q(LZFIT2+11)
        xz_fit(8)  =Q(LZFIT2+12)
        xz_fit(9)  =Q(LZFIT2+13)
        xz_fit(10) =Q(LZFIT2+14)
        xz_fit(11) =Q(LZFIT2+15)
        xz_fit(12) =Q(LZFIT2+16)
        xz_fit(13) =Q(LZFIT2+36)
        xz_fit(14) =Q(LZFIT2+37)
        xz_fit(15) =Q(LZFIT2+38)
        ixz_fit(16)=IQ(LZFIT2+39)
        xz_fit(17) =Q(LZFIT2+4)
        ixz_fit(18)=IQ(LZFIT2+3)
        xz_fit(19) =Q(LZFIT2+40)
        xz_fit(20) =Q(LZFIT2+41)
        xz_fit(21) =Q(LZFIT2+42)
        xz_fit(22) =Q(LZFIT2+43)
        xz_fit(23) =Q(LZFIT2+44)
        xz_fit(24) =Q(LZFIT2+45)
        xz_fit(25) =Q(LZFIT2+46)
        xz_fit(26) =Q(LZFIT2+47)
      ENDIF
  999 RETURN
C...........................................................................
      ENTRY Z_FIT_INFO(NVAR,XDATA)
      nvar = 35
      CALL ucopy(xz_fit,xdata,nvar)
      RETURN
C...........................................................................
      ENTRY EVT_Z_FIT_TAGS(NTAGS,TZ_FIT)
      ntags = 35
      TZ_FIT(1) ='INVP1B'
      TZ_FIT(2) ='THETA1B'
      TZ_FIT(3) ='PHI1B'
      TZ_FIT(4) ='INVP2B'
      TZ_FIT(5) ='THETA2B'
      TZ_FIT(6) ='PHI2B'
      TZ_FIT(7) ='INVP1A'
      TZ_FIT(8) ='THETA1A'
      TZ_FIT(9) ='PHI1A'
      TZ_FIT(10)='INVP2A'
      TZ_FIT(11)='THETA2A'
      TZ_FIT(12)='PHI2A'
      TZ_FIT(13)='META'
      TZ_FIT(14)='M12B'
      TZ_FIT(15)='M12A'
      TZ_FIT(16)='ISTAT'
      TZ_FIT(17)='CHI2ZF'
      TZ_FIT(18)='DOFZF'
      TZ_FIT(19)='S1'
      TZ_FIT(20)='S2'
      TZ_FIT(21)='S3'
      TZ_FIT(22)='S4'
      TZ_FIT(23)='S5'
      TZ_FIT(24)='S6'
      TZ_FIT(25)='S7'
      TZ_FIT(26)='S8'
      TZ_FIT(27)='INVPZFMIN_H'
      TZ_FIT(28)='CHI2ZF_H'
      TZ_FIT(29)='PZA_H'
      TZ_FIT(30)='INVP1A_H'
      TZ_FIT(31)='INVP2A_H'
      TZ_FIT(32)='PULLINVP1_H'
      TZ_FIT(33)='PULLINVP2_H'
      TZ_FIT(34)='PULLMETX_H'
      TZ_FIT(35)='PULLMETY_H'
      RETURN
      END
