      SUBROUTINE D0HUSD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Set HBOOK4 directory to the one requested by call to
C-      D0HSDN
C-
C-   ENTRY D0HSDN(USRDIR)  : set directory name
C-   Input:
C-   USRDIR= character string with directory name
C-
C-
C-   Created  28-JUN-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) USRDIR
      CHARACTER*40 SAVDIR
      INTEGER LENGTH,TRULEN
      SAVE SAVDIR,LENGTH
C----------------------------------------------------------------------
C
      CALL HCDIR(SAVDIR(1:LENGTH),' ')
C
      RETURN
C
      ENTRY D0HSDN(USRDIR)
      LENGTH=TRULEN(USRDIR)
      SAVDIR=USRDIR(1:LENGTH)
  999 RETURN
      END

