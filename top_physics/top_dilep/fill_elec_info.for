      SUBROUTINE FILL_ELEC_INFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : setup array with electron related information
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
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:top_DILEP_ANALYSIS.INC'
      INTEGER I,J,LVCOR,STATUS,NTAGS,KMAX
      REAL    XDATA(1000)
      REAL    CQUAN(50),TQUAN(50),DETA
      REAL  UNCORR_EN,UNCORR_ET,EN,ET,RSTATUS(2)
      REAL FHAD/0.5/,E_LKL,ELIKE
      REAL    ZVTXE,ETZV(4),THZV(4)
      INTEGER mask_all,mask_trdoff,dummy,elike_set_mask_cc,
     &  elike_set_mask_ec,mask_ec,ier
      PARAMETER( mask_ec = '0F'X  )
      PARAMETER( mask_all = '1F'X  )
      PARAMETER( mask_trdoff = '0F'X  )
      LOGICAL OK
C----------------------------------------------------------------------
C
C ****  get ELECTRON links
C
      CALL gtslink('ELE_TGHT', nwant_eletght,ntot_eletght,
     &    elec_link_tght)
      nelec_tght = min(ntot_eletght,nwant_eletght)
      CALL gtslink('ELE_LSE', nwant_ele, ntot_ele, elec_link)
      nelec = min(ntot_ele,nwant_ele)
C
C ****  Fill arrays
C
      ixelec(1) = nelec
      DO i=1,nelec
        ptr_ele_tght(i)=0
        DO j=1,nelec_tght
          IF (elec_link(i).EQ.elec_link_tght(j)) THEN
            ptr_ele_tght(i)=1
          ENDIF
        END DO
      END DO
      DO i=1,nelec
        lpelc=elec_link(i)
        CALL cleanem(lpelc,1,ok,status)
        CALL cleanem_cquans(mvar,cquan)
        CALL cleanem_tquans(mvar,tquan)
        CALL SPLIT_BITMASK(STATUS,RSTATUS)
        deta = cquan(5)
        uncorr_en =0.
        uncorr_et =0.
        IF ( LQ( LPELC - 4 ) .GT. 0 ) THEN
          LVCOR = LQ( LPELC - 4 )
          EN = Q(LPELC+6)
          ET = Q(LPELC+7)
          UNCORR_ET = (1. - Q(LVCOR+2+4)/EN )*ET
          UNCORR_EN = EN - Q(LVCOR+6)
        ENDIF
C
        xelec(i+1) = cquan(2) ! E
        xelec(i+nwant_ele+1)   = cquan(3) ! Et
        xelec(i+2*nwant_ele+1) = cquan(5)     ! Det Eta
        xelec(i+3*nwant_ele+1) = cquan(17)    ! Eta
        xelec(i+4*nwant_ele+1) = cquan(18)    ! phi
        xelec(i+5*nwant_ele+1) = cquan(19)    ! theta
        xelec(i+6*nwant_ele+1)= rstatus(2)
        xelec(i+7*nwant_ele+1)= rstatus(1)
        ixelec(i+8*nwant_ele+1) = cquan(21)    ! ncells
        xelec(i+9*nwant_ele+1) = cquan(4)     ! chisq
        xelec(i+10*nwant_ele+1) = cquan(9)     ! emfrac
        xelec(i+11*nwant_ele+1) = cquan(13)   ! e_fiso1
        xelec(i+12*nwant_ele+1) = cquan(23)   ! e_iso1
        xelec(i+13*nwant_ele+1) = cquan(25)   ! et_iso1
        xelec(i+14*nwant_ele+1) = uncorr_en    ! uncorrected en
        xelec(i+15*nwant_ele+1) = uncorr_et    ! uncorrected Et
        ixelec(i+16*nwant_ele+1) = ptr_ele_tght(i) ! tight electron flag
        ixelec(i+17*nwant_ele+1) = 0 !  will be filled later (nem flag)
        yem(i)=cquan(3)
        ixelec(i+18*nwant_ele+1) = tquan(1)    ! nztraks
        ixelec(i+19*nwant_ele+1) = tquan(2)    ! ntrkcone
        xelec(i+20*nwant_ele+1) = tquan(10)   ! rdphi
        xelec(i+21*nwant_ele+1) = tquan(11)   ! dz
        xelec(i+22*nwant_ele+1) = tquan(12)   ! matchsig
        IF (tquan(13).GT.0) THEN
          xelec(i+23*nwant_ele+1) = tquan(13) ! MIPS CDC
        ELSE IF (tquan(14).GT.0) THEN
          xelec(i+23*nwant_ele+1) = tquan(14) ! MIPS FDC
        ENDIF
        xelec(i+24*nwant_ele+1) = tquan(22)   ! TRD_ACC
        xelec(i+25*nwant_ele+1) = tquan(23)   ! TRD_EFF
        ixelec(i+26*nwant_ele+1) = tquan(24)   ! hitsinfo
        do j=1,2
          xelec(i+(26+j)*nwant_ele+1) = tquan(24+j)   ! hitsinfo
        enddo
        do j=3,14
          ixelec(i+(26+j)*nwant_ele+1) = tquan(24+j)   ! hitsinfo
        enddo
        ixelec(i+41*nwant_ele+1) = tquan(39)   ! vertid
*
*LIKELIHOOD
*
        IF(ABS(Q(LPELC+19)) .LE. 12)THEN
          FHAD = 0.52
          DUMMY = ELIKE_SET_MASK_CC(MASK_ALL)
          E_LKL = ELIKE(LPELC,FHAD,IER)
          XELEC(I+42*NWANT_ELE+1) = E_LKL
          IXELEC(I+44*NWANT_ELE+1) = IER
          DUMMY = ELIKE_SET_MASK_CC(MASK_TRDOFF)
          E_LKL = ELIKE(LPELC,FHAD,IER)
          XELEC(I+43*NWANT_ELE+1) = E_LKL
          IXELEC(I+45*NWANT_ELE+1) = IER
        ELSE
          FHAD = 0.62
          DUMMY = ELIKE_SET_MASK_EC(MASK_ALL)
          E_LKL = ELIKE(LPELC,FHAD,IER)
          XELEC(I+42*NWANT_ELE+1) = E_LKL
          IXELEC(I+44*NWANT_ELE+1) = IER
          DUMMY = ELIKE_SET_MASK_EC(MASK_TRDOFF)
          E_LKL = ELIKE(LPELC,FHAD,IER)
          XELEC(I+43*NWANT_ELE+1) = E_LKL
          IXELEC(I+45*NWANT_ELE+1) = IER
        ENDIF
C-
        LZTRK = LQ(LPELC-3)
        CALL EM_MVTXVAR(LPELC,LZTRK,ZVTXE,ETZV,THZV)
        xelec(I+47*nwant_ele+1) = ZVTXE
        do j=1,3
          xelec(I+(47+j)*nwant_ele+1) = ETZV(j)
          xelec(I+(50+j)*nwant_ele+1) = THZV(j)
        enddo
C-
      ENDDO
  999 RETURN
C...........................................................................
      ENTRY ELEC_INFO(NVAR,XDATA)
      nvar = nwant_ele*nvar_elec+1
      CALL ucopy(xelec,xdata,nvar)
      RETURN
C...........................................................................
      ENTRY ELEC_TAGS(NTAGS,TELEC,KMAX)
      KMAX = nwant_ele
      ntags = nvar_elec+1
      TELEC(1) ='NELEC'
      TELEC(2)='EE'
      TELEC(3)='ETE'
      TELEC(4)='DETAE'
      TELEC(5)='ETAE'
      TELEC(6)='PHIE'
      TELEC(7)='THE'
      TELEC(8) ='CSTAT_'
      TELEC(9) ='CSTAT'
      TELEC(10)='NCELE:I'
      TELEC(11)='CHISQE'
      TELEC(12)='EMFE'
      TELEC(13)='FISOLE'
      TELEC(14)='E_ISOE'
      TELEC(15)='ETISOE'
      TELEC(16)='UCEN_E'
      TELEC(17)='UCET_E'
      TELEC(18)='PTRT_E:I'
      TELEC(19)='FLGEME:I'
      TELEC(20)='NZTRKE:I'
      TELEC(21)='NCTRKE:I'
      TELEC(22)='RDPHIE'
      TELEC(23)='DZDRE'
      TELEC(24)='MSIGE'
      TELEC(25)='DEDXE'
      TELEC(26)='TRDACC'
      TELEC(27)='TRDEFF'
      TELEC(28)='FLGCDE:I'
      TELEC(29)='RHVTXWE'
      TELEC(30)='RHCDCWE'
      TELEC(31)='NHVTXXYE:I'
      TELEC(32)='NHVTX3DE:I'
      TELEC(33)='NHCDCXYE:I'
      TELEC(34)='NHCDC3DE:I'
      TELEC(35)='NHCDCZSE:I'
      TELEC(36)='NHATRD1E:I'
      TELEC(37)='NHATRD2E:I'
      TELEC(38)='NHATRD3E:I'
      TELEC(39)='NHCTRD1E:I'
      TELEC(40)='NHCTRD2E:I'
      TELEC(41)='NHCTRD3E:I'
      TELEC(42)='NHCLOUDE:I'
      TELEC(43)='NVERTIDE:I'
      TELEC(44)='ELKL_ALL'
      TELEC(45)='ELKL_NOTRD'
      TELEC(46)='IER_ELKL_ALL:I'
      TELEC(47)='IER_ELKL_NOTRD:I'
      TELEC(48)='ZVTXE'
      TELEC(49)='ETZV1E'
      TELEC(50)='ETZV2E'
      TELEC(51)='ETZV3E'
      TELEC(52)='THZV1E'
      TELEC(53)='THZV2E'
      TELEC(54)='THZV3E'
      RETURN
      END
