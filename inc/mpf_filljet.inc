C----------------------------------------------------------------------
C-   Created  Jul-01-1995   Bob Kehoe
C-   Updated  Oct-18-1995   Bob Kehoe -- renam to mpf_filljet for IBM's
C----------------------------------------------------------------------

      INTEGER limit2
      PARAMETER (limit2 = 20)
      COMMON /mpf_filljets/nj_not_em,jet_eta,jet_phi,jet_e,            
     &    jet_qual
      integer nj_not_em
      REAL jet_eta(10,limit2),jet_phi(10,limit2)
      REAL jet_e(50,limit2),jet_qual(50,limit2)
