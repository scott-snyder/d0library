      SUBROUTINE FILL_PHOT_INFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-APR-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:pi.def'
      INCLUDE 'D0$INC:top_DILEP_ANALYSIS.INC'
      INTEGER I,J,LVCOR,STATUS,NTAGS,KMAX,NZTRAKS,VERTEX_ID,LZTRKPV
      REAL    XDATA(1000)
      REAL    CQUAN(50),TQUAN(50),DETA
      REAL  UNCORR_EN,UNCORR_ET,EN,ET,RSTATUS(2)
      REAL FHAD/0.5/,E_LKL,ELIKE
      REAL    ZVTXP,ETZV(4),THZV(4)
      INTEGER mask_all,mask_trdoff,dummy,elike_set_mask_cc,
     &  elike_set_mask_ec,mask_ec,ier
      data mask_ec / z'0F'  /
      data mask_all / z'1F'  /
      data mask_trdoff / z'0F'  /
      CHARACTER*4 BANK,PELC
      DATA    PELC/'PELC'/
      LOGICAL OK
C----------------------------------------------------------------------
C
C ****  get links for all objects
C
      CALL gtslink('GAM_TGHT', nwant_gamtght,ntot_gamtght,
     &    phot_link_tght)
      ngam_tght = min(ntot_gamtght,nwant_gamtght)
      CALL gtslink('GAM_LSE', nwant_phot, ntot_phot, phot_link)
      nphot = min(ntot_phot,nwant_phot)
C
      ixphot(1) = nphot
      DO i=1,nphot
        ptr_gam_tght(i)=0
        DO j=1,ngam_tght
          IF (phot_link(i).EQ.phot_link_tght(j)) THEN
            ptr_gam_tght(i)=1
          ENDIF
        END DO
      END DO
C
      DO i=1,nphot
        lppho=phot_link(i)
        CALL cleanem(lppho,1,ok,status)
        CALL cleanem_cquans(mvar,cquan)
        CALL cleanem_tquans(mvar,tquan)
        CALL SPLIT_BITMASK(STATUS,RSTATUS)
        deta = cquan(5)
        uncorr_en=0.
        uncorr_et=0.
        IF ( LQ( LPPHO - 4 ) .GT. 0 ) THEN
          LVCOR = LQ( LPPHO - 4 )
          EN = Q(LPPHO+6)
          ET = Q(LPPHO+7)
          UNCORR_ET = (1. - Q(LVCOR+2+4)/EN )*ET
          UNCORR_EN = EN - Q(LVCOR+6)
        ENDIF
        xphot(i+1) = cquan(2) ! E
        xphot(i+1*nwant_phot+1) = cquan(3) ! Et
        xphot(i+2*nwant_phot+1) = cquan(5)     ! Det Eta
        xphot(i+3*nwant_phot+1) = cquan(17)    ! Eta
        xphot(i+4*nwant_phot+1) = cquan(18)    ! phi
        xphot(i+5*nwant_phot+1) = cquan(19)    ! theta
        xphot(i+6*nwant_phot+1)= rstatus(2)
        xphot(i+7*nwant_phot+1)= rstatus(1)
        ixphot(i+8*nwant_phot+1) = cquan(21)    ! ncells
        xphot(i+9*nwant_phot+1) = cquan(4)     ! chisq
        xphot(i+10*nwant_phot+1) = cquan(9)     ! emfrac
        xphot(i+11*nwant_phot+1) = cquan(13)   ! e_fiso1
        xphot(i+12*nwant_phot+1) = cquan(23)   ! e_iso1
        xphot(i+13*nwant_phot+1) = cquan(25)   ! et_iso1
        xphot(i+14*nwant_phot+1) = uncorr_en    ! uncorrected en
        xphot(i+15*nwant_phot+1) = uncorr_et    ! uncorrected Et
        ixphot(i+16*nwant_phot+1) = ptr_gam_tght(i) ! tight gamma    flag
        ixphot(i+17*nwant_phot+1) = 0 !  will be filled later (nem flag)
        yem(10+i)=cquan(3)
        ixphot(i+18*nwant_phot+1) = tquan(1)    ! nztraks
        ixphot(i+19*nwant_phot+1) = tquan(2)    ! ntrkcone
        xphot(i+20*nwant_phot+1) = tquan(10)   ! rdphi
        xphot(i+21*nwant_phot+1) = tquan(11)   ! dz
        xphot(i+22*nwant_phot+1) = tquan(12)   ! matchsig
        IF (tquan(13).GT.0) THEN
          xphot(i+23*nwant_phot+1) = tquan(13) ! MIPS CDC
        ELSE IF (tquan(14).GT.0) THEN
          xphot(i+23*nwant_phot+1) = tquan(14) ! MIPS FDC
        ENDIF
        xphot(i+24*nwant_phot+1) = tquan(22)   ! TRD_ACC
        xphot(i+25*nwant_phot+1) = tquan(23)   ! TRD_EFF
        ixphot(i+26*nwant_phot+1) = tquan(24)   ! hitsinfo
        do j=1,2
          xphot(i+(26+j)*nwant_phot+1) = tquan(24+j)   ! hitsinfo
        enddo
        do j=3,14
          ixphot(i+(26+j)*nwant_phot+1) = tquan(24+j)   ! hitsinfo
        enddo
        ixphot(i+41*nwant_phot+1) = tquan(39)   ! IDVERT FOR photon
        CALL GET_PPHO_TRACKS(LPPHO,NZTRAKS,LZTRK,VERTEX_ID)
        IF (NZTRAKS.GT.0) THEN
          LZTRKPV = LZTRK
          IF (LQ(LPPHO-3).NE.LZTRKPV) THEN
            LQ(LPPHO-3) = LZTRKPV
          ENDIF
          IF(ABS(Q(LPPHO+19)) .LE. 12)THEN
            FHAD = 0.52
            DUMMY = ELIKE_SET_MASK_CC(MASK_ALL)
            E_LKL = ELIKE(LPPHO,FHAD,IER)
            xphot(I+43*nwant_phot+1) = E_LKL
            Ixphot(I+44*nwant_phot+1) = IER
            DUMMY = ELIKE_SET_MASK_CC(MASK_TRDOFF)
            E_LKL = ELIKE(LPPHO,FHAD,IER)
            xphot(I+45*nwant_phot+1) = E_LKL
            Ixphot(I+46*nwant_phot+1) = IER
          ELSE
            FHAD = 0.62
            DUMMY = ELIKE_SET_MASK_EC(MASK_ALL)
            E_LKL = ELIKE(LPPHO,FHAD,IER)
            xphot(I+43*nwant_phot+1) = E_LKL
            Ixphot(I+45*nwant_phot+1) = IER
            DUMMY = ELIKE_SET_MASK_EC(MASK_TRDOFF)
            E_LKL = ELIKE(LPPHO,FHAD,IER)
            xphot(I+44*nwant_phot+1) = E_LKL
            Ixphot(I+46*nwant_phot+1) = IER
          ENDIF
          CALL EM_MVTXVAR(LPPHO,LZTRKPV,ZVTXP,ETZV,THZV)
          xphot(I+47*nwant_phot+1) = ZVTXP
          do j=1,3
            xphot(I+(47+j)*nwant_phot+1) = ETZV(j)
            xphot(I+(50+j)*nwant_phot+1) = THZV(j)
          enddo
        ENDIF

      ENDDO
  999 RETURN
C...........................................................................
      ENTRY PHOT_INFO(NVAR,XDATA)
      nvar = nwant_phot*nvar_phot+1
      CALL ucopy(xphot,xdata,nvar)
      RETURN
C...........................................................................
      ENTRY PHOT_TAGS(NTAGS,TPHOT,KMAX)
      KMAX = nwant_phot
      ntags = nvar_phot+1
      TPHOT(1) ='NPHOT'
      TPHOT(2)='EP'
      TPHOT(3)='ETP'
      TPHOT(4)='DETAP'
      TPHOT(5)='ETAP'
      TPHOT(6)='PHIP'
      TPHOT(7)='THP'
      TPHOT(8) ='CSTATP_'
      TPHOT(9) ='CSTATP'
      TPHOT(10)='NCELP:I'
      TPHOT(11)='CHISQP'
      TPHOT(12)='EMFP'
      TPHOT(13)='FISOLP'
      TPHOT(14)='E_ISOP'
      TPHOT(15)='ETISOP'
      TPHOT(16)='UCEN_P'
      TPHOT(17)='UCET_P'
      TPHOT(18)='PTRT_P:I'
      TPHOT(19)='FLGEMP:I'
      TPHOT(20)='NZTRKP:I'
      TPHOT(21)='NCTRKP:I'
      TPHOT(22)='RDPHIP'
      TPHOT(23)='DZDRP'
      TPHOT(24)='MSIGP'
      TPHOT(25)='DEDXP'
      TPHOT(26)='TRDACCP'
      TPHOT(27)='TRDEFFP'
      TPHOT(28)='FLGCDP:I'
      TPHOT(29)='RHVTXWP'
      TPHOT(30)='RHCDCWP'
      TPHOT(31)='NHVTXXYP:I'
      TPHOT(32)='NHVTX3DP:I'
      TPHOT(33)='NHCDCXYP:I'
      TPHOT(34)='NHCDC3DP:I'
      TPHOT(35)='NHCDCZSP:I'
      TPHOT(36)='NHATRD1P:I'
      TPHOT(37)='NHATRD2P:I'
      TPHOT(38)='NHATRD3P:I'
      TPHOT(39)='NHCTRD1P:I'
      TPHOT(40)='NHCTRD2P:I'
      TPHOT(41)='NHCTRD3P:I'
      TPHOT(42)='NHCLOUDP:I'
      TPHOT(43)='NVERTIDP:I'
      TPHOT(44)='PLKL_ALL'
      TPHOT(45)='PLKL_NOTRD'
      TPHOT(46)='IER_PLKL_ALL:I'
      TPHOT(47)='IER_PLKL_NOTRD:I'
      TPHOT(48)='ZVTXP'
      TPHOT(49)='ETZV1P'
      TPHOT(50)='ETZV2P'
      TPHOT(51)='ETZV3P'
      TPHOT(52)='THZV1P'
      TPHOT(53)='THZV2P'
      TPHOT(54)='THZV3P'
      RETURN
      END
