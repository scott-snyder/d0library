      SUBROUTINE GETSYI(NODNAM,HWTYPE,NODARE,NODNUM,VMSVER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find out certain system char. about current node
C-                         Very VAX-specific
C-
C-   Inputs  : NONE
C-   Outputs : NODNAM : CHARACTER*15 (at least) parameter with node name
C-             HWTYPE:     CHARACTER*4 parameter with CPU type
C-             NODARE:  INTEGER DECNET node area number
C-             NODNUM:   INTEGER DECNET node number within area
C-             VMSVER:   CHARACTER*8 parameter for VMS version number
C-
C-   Created   15-DEC-1987   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NODNAM,HWTYPE,VMSVER
      INTEGER NODARE,NODNUM
C&IF VAXVMS
      INCLUDE '($SYIDEF)'
      INTEGER I
      INTEGER ISTAT,LIB$GETSYI
      CHARACTER*64 CHRBUF
C----------------------------------------------------------------------
      CHRBUF=' '
      ISTAT=LIB$GETSYI(SYI$_NODENAME,,CHRBUF,I,,)
      IF(ISTAT.NE.1) THEN
        CALL MSGSCR(ISTAT,' GETSYI-->')
        NODNAM='Not found'
      ELSE
        NODNAM=CHRBUF(1:I)
      ENDIF
      CHRBUF=' '
      ISTAT=LIB$GETSYI(SYI$_NODE_HWTYPE,,CHRBUF,I,,)
      IF(ISTAT.NE.1) THEN
        CALL MSGSCR(ISTAT,' GETSYI-->')
        HWTYPE='UNKN'
      ELSE
        HWTYPE=CHRBUF(1:I)
      ENDIF
      ISTAT=LIB$GETSYI(SYI$_NODE_AREA,NODARE,,,,)
      IF(ISTAT.NE.1) THEN
        CALL MSGSCR(ISTAT,' GETSYI-->')
        NODARE=0
      ENDIF
      ISTAT=LIB$GETSYI(SYI$_NODE_NUMBER,NODNUM,,,,)
      IF(ISTAT.NE.1) THEN
        CALL MSGSCR(ISTAT,' GETSYI-->')
        NODNUM=0
      ENDIF
      ISTAT=LIB$GETSYI(SYI$_VERSION,,CHRBUF,I,,)
      IF(ISTAT.NE.1) THEN
        CALL MSGSCR(ISTAT,' GETSYI-->')
        VMSVER=' '
      ELSE
        VMSVER=CHRBUF(1:I)
      ENDIF
C&ENDIF
      RETURN
      END
