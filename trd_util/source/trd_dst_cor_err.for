      SUBROUTINE TRD_DST_COR_ERR(C,A,S)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : handles error messages for TRD_DST_COR.FOR
C-
C-   Inputs  : C,A,S  messages from TRD_DST_COR.FOR
C-   Outputs : none
C-   Controls: none
C-
C-   Created   5-JUL-1993   Alain PLUQUET
C-
C----------------------------------------------------------------------
      CHARACTER*(*) C,A,S
      CHARACTER*132 M1,M2,M3
      M1='TRD_DST_COR '//C//' '//A(1:3)//' '//S(1:3)
      M2='TRD_DST_COR'
      M3='Correction is not available.'
      CALL ERRMSG(M1,M2,M3,'W')
      END
