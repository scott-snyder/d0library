C----------------------------------------------------------------------
C-
C-   Created   7-NOV-1994   Greg Griffin
C-
C----------------------------------------------------------------------
      INTEGER L15c_terms_passed, L15c_terms_incmplt
C***** TODO: use parameters
      LOGICAL L15_CAL_TERM_STATES(0:7)
      COMMON /L15CT_INFO/ 
     &        L15c_terms_passed, L15c_terms_incmplt,
     &        L15_CAL_TERM_STATES
