      SUBROUTINE LDDUM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   The following code is a dummy call just to get GEANT routines
C-   loaded that are only called from geant3xx.olb  but reside
C-   in D0Geant.olb.  the call must never actually be executed.......
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  Al Jonckheere
C-   Updated  14-JUL-1989   Rajendran Raja
C-   Updated  28-FEB-1990   Alan M. Jonckheere  Add NRAN 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL A,B,C,D,E,F,G
      INTEGER L,M,N,O,P
C
      LOGICAL LCALL
      DATA LCALL/.FALSE./
C
      IF(LCALL) THEN
        CALL GTNINO         ! V3.10.62
        CALL GUDIGI
        CALL GUFLD(A,B)
        CALL GUHADR
        CALL GUKINE
        CALL GUOUT
        CALL GUPHAD
        CALL GUSTEP
        CALL GUTRAK
        CALL GUTREV
        CALL QNEXT
        CALL TRDBLK
        CALL NRAN(A,L)
C
C&IF VAXVMS,SIUNIX,IBMAIX,ULTRIX,SUNOS,ALFOSF
C these are the interactive calls
C       CALL GUIGET(L,M,N)
C       CALL GUINTI
C&ENDIF
C
      ENDIF
  999 RETURN
      END
