      SUBROUTINE mpf_fill_JETS(algorithm)

C----------------------------------------------------------------------
C-   Purpose and Methods : fill jet arrays for column-wise ntuples
C-
C-   Created  Jul-01-1995   Bob Kehoe
C-   Updated  Oct-18-1995   Bob Kehoe -- renamed include files for IBM's
C----------------------------------------------------------------------

      IMPLICIT NONE
      INCLUDE 'D0$INC:MPF_JETRES.INC'
      INCLUDE 'D0$INC:MPF_FILLJET.INC'
      INTEGER k,algorithm

C----------------------------------------------------------------------
C-        INITIALIZE VARIABLES
      IF (algorithm.EQ.1) THEN
        nj7 = nj_not_em
        DO k = 1,nj7
          jt_et7(k) = jet_e(5,k)
          jt_epr7(k) = jet_e(6,k)
          jt_e7(k) = jet_e(4,k)
          jt_ex7(k) = jet_e(1,k)
          jt_ey7(k) = jet_e(2,k)
          jt_eta7(k) = jet_eta(1,k)
          jt_deta7(k) = jet_eta(2,k)
          jt_phi7(k) = jet_phi(1,k)
          jt_emf7(k) = jet_qual(1,k)
          jt_msp7(k) = jet_qual(2,k)
          jt_rms7(k) = jet_qual(3,k)
          jt_ntr7(k) = jet_qual(5,k)
          jt_ncl7(k) = jet_qual(6,k)
          jt_icd7(k) = jet_qual(7,k)
          jt_chf7(k) = jet_qual(8,k)
          jt_mxc7(k) = jet_qual(9,k)
        ENDDO
      ELSEIF (algorithm.EQ.2) THEN
        nj5 = nj_not_em
        DO k = 1,nj5
          jt_et5(k) = jet_e(5,k)
          jt_epr5(k) = jet_e(6,k)
          jt_e5(k) = jet_e(4,k)
          jt_ex5(k) = jet_e(1,k)
          jt_ey5(k) = jet_e(2,k)
          jt_eta5(k) = jet_eta(1,k)
          jt_deta5(k) = jet_eta(2,k)
          jt_phi5(k) = jet_phi(1,k)
          jt_emf5(k) = jet_qual(1,k)
          jt_msp5(k) = jet_qual(2,k)
          jt_rms5(k) = jet_qual(3,k)
          jt_ntr5(k) = jet_qual(5,k)
          jt_ncl5(k) = jet_qual(6,k)
          jt_icd5(k) = jet_qual(7,k)
          jt_chf5(k) = jet_qual(8,k)
          jt_mxc5(k) = jet_qual(9,k)
        ENDDO
      ELSEIF (algorithm.EQ.3) THEN
        nj3 = nj_not_em
        DO k = 1,nj3
          jt_et3(k) = jet_e(5,k)
          jt_epr3(k) = jet_e(6,k)
          jt_e3(k) = jet_e(4,k)
          jt_ex3(k) = jet_e(1,k)
          jt_ey3(k) = jet_e(2,k)
          jt_eta3(k) = jet_eta(1,k)
          jt_deta3(k) = jet_eta(2,k)
          jt_phi3(k) = jet_phi(1,k)
          jt_emf3(k) = jet_qual(1,k)
          jt_msp3(k) = jet_qual(2,k)
          jt_rms3(k) = jet_qual(3,k)
          jt_ntr3(k) = jet_qual(5,k)
          jt_ncl3(k) = jet_qual(6,k)
          jt_icd3(k) = jet_qual(7,k)
          jt_chf3(k) = jet_qual(8,k)
          jt_mxc3(k) = jet_qual(9,k)
        ENDDO
      ELSEIF (algorithm.EQ.4) THEN
        njn = nj_not_em
        DO k = 1,njn
          jt_etn(k) = jet_e(5,k)
          jt_eprn(k) = jet_e(6,k)
          jt_en(k) = jet_e(4,k)
          jt_exn(k) = jet_e(1,k)
          jt_eyn(k) = jet_e(2,k)
          jt_etan(k) = jet_eta(1,k)
          jt_detan(k) = jet_eta(2,k)
          jt_phin(k) = jet_phi(1,k)
          jt_emfn(k) = jet_qual(1,k)
          jt_mspn(k) = jet_qual(2,k)
          jt_rmsn(k) = jet_qual(3,k)
          jt_ntrn(k) = jet_qual(5,k)
          jt_ncln(k) = jet_qual(6,k)
          jt_icdn(k) = jet_qual(7,k)
          jt_chfn(k) = jet_qual(8,k)
          jt_mxcn(k) = jet_qual(9,k)
        ENDDO
      ELSEIF (algorithm.EQ.5) THEN
        nj1 = nj_not_em
        DO k = 1,nj1
          jt_et1(k) = jet_e(5,k)
          jt_epr1(k) = jet_e(6,k)
          jt_e1(k) = jet_e(4,k)
          jt_ex1(k) = jet_e(1,k)
          jt_ey1(k) = jet_e(2,k)
          jt_eta1(k) = jet_eta(1,k)
          jt_deta1(k) = jet_eta(2,k)
          jt_phi1(k) = jet_phi(1,k)
          jt_emf1(k) = jet_qual(1,k)
          jt_msp1(k) = jet_qual(2,k)
          jt_rms1(k) = jet_qual(3,k)
          jt_ntr1(k) = jet_qual(5,k)
          jt_ncl1(k) = jet_qual(6,k)
          jt_icd1(k) = jet_qual(7,k)
          jt_chf1(k) = jet_qual(8,k)
          jt_mxc1(k) = jet_qual(9,k)
        ENDDO
      ELSEIF (algorithm.EQ.6) THEN
        njt = nj_not_em
        DO k = 1,njt
          jt_ett(k) = jet_e(5,k)
          jt_eprt(k) = jet_e(6,k)
          jt_nrgt(k) = jet_e(4,k)
          jt_ext(k) = jet_e(1,k)
          jt_eyt(k) = jet_e(2,k)
          jt_etat(k) = jet_eta(1,k)
          jt_detat(k) = jet_eta(2,k)
          jt_phit(k) = jet_phi(1,k)
          jt_emft(k) = jet_qual(1,k)
          jt_mspt(k) = jet_qual(2,k)
          jt_rmst(k) = jet_qual(3,k)
          jt_ntrt(k) = jet_qual(5,k)
          jt_nclt(k) = jet_qual(6,k)
          jt_icdt(k) = jet_qual(7,k)
          jt_chft(k) = jet_qual(8,k)
          jt_mxct(k) = jet_qual(9,k)
        ENDDO
      ENDIF
C
  999 RETURN
      END
