      SUBROUTINE STAMSG(STRING,CENTER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output string to STATUS part of screen if
C-                         possible. VAX-specific.
C-
C-   Inputs  : STRING: Characters to be output
C-             CENTER: Flag indicating whether to center text or not.
C-   Outputs : None
C-   Controls: None
C-
C-   Created  21-OCT-1988   Jan S. Hoftun
C-   Updated  19-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRING
      LOGICAL CENTER
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      LOGICAL ISTAT,SMG$PUT_LINE,SMG$ERASE_DISPLAY
      INTEGER TRULEN,LIBCUR,LIBGET
      INTEGER I,J,K,M
      LOGICAL GETDEV
      CHARACTER*132 BLNK
C&IF VAXVMS
C&ELSE
C&      CHARACTER*132 CTEMP
C&ENDIF
      DATA BLNK/' '/
C----------------------------------------------------------------------
      IF(SMGON.AND.GETDEV().AND.STAFLG) THEN    ! Should it be written
C                                               ! to upper display?
        ISTAT=LIBGET(I,J)
        IF(STRING(1:1).EQ.'0') THEN
          ISTAT=SMG$PUT_LINE(MINID2,' ',1,0,%VAL(0),1,%VAL(0),%VAL(0))
          IF(.NOT.ISTAT) THEN
            CALL MSGSCR(ISTAT,'PUT STATUS-->')
          ENDIF
        ELSEIF(STRING(1:1).EQ.'1') THEN
          ISTAT=SMG$ERASE_DISPLAY(MINID2,1,1,STALIN-2,1)
          IF(.NOT.ISTAT) THEN
            CALL MSGSCR(ISTAT,'ERASE STATUS-->')
          ENDIF
        ENDIF
        K=TRULEN(STRING)+1
        IF(CENTER) THEN
          M=MAX((PBCOLS-K-1)/2,0)
        ELSE
          M=0
        ENDIF
        IF(STRING(1:1).EQ.'+') THEN
C&IF VAXVMS
          ISTAT=SMG$PUT_LINE(MINID2,BLNK(1:M)//STRING(2:K),0,1,0,1)
C&ELSE
C&          CTEMP = BLNK(1:M)//STRING(2:K)
C&          ISTAT=SMG$PUT_LINE(MINID2,CTEMP(1:M+K-1),0,1,0,1,
C&     &                       %VAL(0),%VAL(0))
C&ENDIF
        ELSE
C&IF VAXVMS
        ISTAT=SMG$PUT_LINE(MINID2,BLNK(1:M)//STRING(2:K),1,1,0,1)
C&ELSE
C&        CTEMP = BLNK(1:M)//STRING(2:K)
C&        ISTAT=SMG$PUT_LINE(MINID2,CTEMP(1:M+K-1),1,1,0,1,
C&     &                     %VAL(0),%VAL(0))
C&ENDIF
        ENDIF
        IF(.NOT.ISTAT) THEN
          CALL MSGSCR(ISTAT,'PUT STATUS-->')
        ENDIF
        IF(.NOT.FULSCR) THEN
          ISTAT=LIBCUR(I,J)  !Reset cursor pos
        ENDIF
      ELSE
        CALL INTMSG(STRING)
      ENDIF
      RETURN
      END
