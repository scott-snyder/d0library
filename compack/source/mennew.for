      SUBROUTINE MENNEW(MENNAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Declare a new menu level
C-
C-   Inputs  : MENNAM: Name of menu level.
C-   Outputs : None
C-   Controls: MAXLEV is increased
C-
C-   Created  20-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) MENNAM
      CHARACTER*132 CTEMP
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
C&IF VAXVMS
      INCLUDE 'D0$COMPACK$SOURCE:COMMSG.DEF'
C&ENDIF
C
      CHARACTER*80 TRANUP
      INTEGER I,TRULEN
C----------------------------------------------------------------------
      DO I=1,UPRLEV       !Check for uniquness of menu name
        IF(MENNAM.EQ.NAMLEV(I)) THEN
C&IF VAXVMS
          CALL ABOMEN(COMPACK__ALREXIST,MENNAM(1:TRULEN(MENNAM)))
C&ELSE
C&          CTEMP = '0Menu already exists:'//MENNAM(1:TRULEN(MENNAM))
C&          CALL INTMSG(CTEMP)
C&          STOP
C&ENDIF
        ENDIF
      ENDDO
      CALL SETCHK         !See if initialization is needed
      UPRLEV=UPRLEV+1
      IF(UPRLEV.GT.MAXLEV) THEN
C&IF VAXVMS
        CALL ABOMEN(COMPACK__MAXLEV,' ')
C&ELSE
C&        CALL INTMSG('0Maximum number of levels exceeded!'//CHAR(7))
C&        STOP
C&ENDIF
      ELSE
        NAMLEV(UPRLEV)=TRANUP(MENNAM)
        SETUP=.FALSE.
        IF(UPRLEV.EQ.1) THEN
          CURLEV=1
        ENDIF
      ENDIF
      RETURN
      END
