      SUBROUTINE BROAST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : AST for trapping broadcast messages in COMPACK
C-                         VAX-specific
C-
C-   Inputs  : NONE
C-
C-   Outputs : NONE
C-
C-   Created   8-OCT-1987   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER SMG$GET_BROADCAST_MESSAGE,ISTAT,ILEN,LIBGET,LIBCUR
      INTEGER I,J,IS,TRULEN,SMG$CANCEL_INPUT
      LOGICAL GETDEV
      CHARACTER*132 MESSAG
C----------------------------------------------------------------------
      ISTAT=1
      DO WHILE (ISTAT.EQ.1)
        ISTAT=SMG$GET_BROADCAST_MESSAGE(PASTID,MESSAG,ILEN)
        IF(ISTAT) THEN
          IF(SPLFLG) THEN
            IS=LIBGET(I,J)
            CALL INTMSG('0'//MESSAG(1:ILEN))
            IS=LIBCUR(I,J)
          ELSE
            CALL SAVSCR
            CALL SPLTIT
            CALL INTMSG('1'//MESSAG(1:ILEN))
            CALL INTMSG('0'//'Use ENTER (lower right) to toggle'//
     *              ' split screen')
            CALL REPSCR
            ISTAT=SMG$CANCEL_INPUT(KEYID)
          ENDIF
        ENDIF
      ENDDO
C&ENDIF
  999 RETURN
      END
