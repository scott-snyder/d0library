      FUNCTION USER_NTUP_FILL()
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills extra words into USER.INC
C-   Need also USER_BOOK
C-   Created  07-NOV-1994  Andrew G. Brandt
C-   Modified 10-JUL-1995  Andrew G. Brandt
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:QCD_JUTL_HEAD.INC/LIST'
      INCLUDE 'D0$INC:QCD_NTUP_INFO.INC/LIST'
      INCLUDE 'D0$INC:QCD_JET.INC/LIST'
      INCLUDE 'D0$INC:QCD_EVT_INFO.INC/LIST'
      INCLUDE 'D0$INC:USER.INC/LIST'
C
      LOGICAL USER_NTUP_FILL
C----------------------------------------------------------------------
      USER_NTUP_FILL=.TRUE.
C
  999 RETURN
      END
