      SUBROUTINE GET_TIME(TIMSTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-       get time
C-
C-   Created  21-SEP-1989   Serban D. Protopopescu
C-   Updated   9-Mar-1992   Herbert Greenlee
C-      Added ELSE block
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TIMSTR
      CHARACTER*8 T
      CHARACTER*9 D
      CHARACTER*26 CTEMP
      INTEGER OFFTIM, D0TLOCT
C----------------------------------------------------------------------
C&IF VAXVMS
      CALL TIME(T)
      CALL DATE(D)
      TIMSTR=' '//D//' '//T
C&ELSE
C&      CALL OFTSTR(D0TLOCT(OFFTIM()),CTEMP)
C&      TIMSTR = ' '//CTEMP(1:20)
C&ENDIF
  999 RETURN
      END
