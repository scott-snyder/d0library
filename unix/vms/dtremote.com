$!========================================================================
$!
$! Name      : DTREMOTE.COM
$!
$! Purpose   : Pop a decterm
$!
$! Arguments : P1 - display node
$!             P2 - server id (default: 0)
$!             P3 - Number of lines (default: 40)
$!             P4 - font (default: see below)
$!             P5 - additional window_att options
$!
$! Created  20-OCT-1994   john hobbs
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$! Initialization -- Find latest and greatest version of DTREMOTE.COM 
$!
$   IF F$TYPE(LIBTEST).NES."STRING"
$   THEN
$           DEF SYS$OUTPUT NL:
$           @D0$DISK:[D0LIBRARY]D0LOCAL
$           DEAS SYS$OUTPUT
$           IF F$TYPE(LIBTEST).NES."STRING" THEN EXIT
$           LIBTEST UNIX
$           DTREMOTE = F$SEARCH("DTREMOTE.COM")
$           IF DTREMOTE.EQS."" THEN  -
                DTREMOTE = F$SEARCH("D0$BETA:[GAMMA.UNIX.VMS]DTREMOTE.COM")
$           IF DTREMOTE.EQS."" THEN  -
                DTREMOTE = F$SEARCH("D0$UNIX$VMS:DTREMOTE.COM")
$           IF DTREMOTE.NES."" THEN -
                @'DTREMOTE' "''P1'" "''P2'" "''P3'" "''P4'" "''P5'"
$           EXIT
$   ENDIF
$
$   NN = F$GETSYI("NODENAME")
$   IF( P1.EQS."" ) 
$   THEN 
$       WRITE SYS$OUTPUT "DTREMOTE: no display node"
$       GOTO EXIT
$   ENDIF
$   IF( P2.EQS."" ) THEN P2=0
$   IF( P3.EQS."" ) THEN P3=40
$   IF( P4.EQS."" ) THEN -
        P4="-dec-terminal-medium-r-normal--14-140-75-75-c-80-dec-dectech"
$   IF( P5.NES."" ) THEN P5=","+P5
$
$! Allow private color setup
$
$  define/nolog UIS$VT_HIGHLIGHT_MASK 51456
$
$! Do the window...
$
$   SET DISPLAY/CREATE/NODE='P1'/TRANS=TCPIP/SERVER='P2'
$   CREATE/TERM/DETACH/WINDOW_ATT=(TITLE='NN',ICON_NAM='NN',-
      ROWS='P3',FONT='P4',INITIAL_STATE="ICON"'P5')
$
$EXIT:
$   SET NOON
$   EXIT
