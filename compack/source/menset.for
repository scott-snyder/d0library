      SUBROUTINE MENSET(MENNAM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize a new menu level and call MLEVXX or
C-                         RDSET to read in setup information
C-
C-   Inputs  : MENNAM: Name of menu level to initialize
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated  19-MAY-1989   Jan S. Hoftun  (Use MENDEF for MENUDEF call) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) MENNAM
      CHARACTER*132 CTEMP
C
C     Get definitions for COMPACK
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INTEGER I,ISTAT,TRULEN
C&IF VAXVMS
      INCLUDE 'D0$COMPACK$SOURCE:COMMSG.DEF'
C&ENDIF
C
      CHARACTER*80 TRANUP
C----------------------------------------------------------------------
      DO I=1,UPRLEV       !Check for uniquness of menu name
        IF(MENNAM.EQ.NAMLEV(I)) THEN
C&IF VAXVMS
          CALL ABOMEN(COMPACK__ALREXIST,MENNAM(1:TRULEN(MENNAM)))
C&ELSE
C&          CTEMP = '0Menu already exits:'//MENNAM(1:TRULEN(MENNAM))
C&          CALL INTMSG(CTEMP)
C&          STOP
C&ENDIF
        ENDIF
      ENDDO
      CALL SETCHK           !See if initialization is needed
      IF(MENNAM.EQ.'MENUDEF') THEN
        CALL MENDEF
      ELSE
C
C     Read startup file (name from MENNAM)
C
        UPRLEV=UPRLEV+1
        IF(UPRLEV.GT.MAXLEV) THEN
C&IF VAXVMS
          CALL ABOMEN(COMPACK__MAXLEV,' ')
C&ELSE
C&         CALL INTMSG('0Maximum number of levels exceeded!'//CHAR(7))
C&         STOP
C&ENDIF
        ELSE
          NAMLEV(UPRLEV)=TRANUP(MENNAM)
          SETUP=.FALSE.
          IF(UPRLEV.EQ.1) THEN
            CURLEV=1
          ENDIF
          CALL RDSET(MENNAM,UPRLEV)
        ENDIF
      ENDIF
      RETURN
      END
