C----------------------------------------------------------------------
C-
C-   Created  17-AUG-1994   Gregory L. Griffin
C-   Updated  19-OCT-1994   Dan Owen  added MFP_FLAG 
C-
C----------------------------------------------------------------------
      INTEGER L15C_TERM_INFO(L15CT_NUM_MIN:L15CT_NUM_MAX,
     +                       L15TM_NUM_MIN:L15TM_NUM_MAX, 6)
c
c   1 -- local term allocation flag. 0 =  not allocated, -1 = allocated
c   2 -- framework term number
c   3 -- reference type (EM or TOT)
c   4 -- global DSP tool number
c   5 -- local  DSP tool number
c   6 -- mask of triggers
c
      INTEGER MFP_FLAG

c    MFP_FLAG = 0 for normal events; <> 0 for mark and force pass events


      INTEGER L15CAL_NUM_TERMS_COORSIM

c     The number of terms allocated by COOR_SIM, not the number of terms
c     used by the hardware

      COMMON /L15C_TERM_INFO/ L15C_TERM_INFO, MFP_FLAG,                 
     &                        L15CAL_NUM_TERMS_COORSIM
