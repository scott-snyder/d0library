$!========================================================================
$!
$! Name      : Q_SOLVE
$!
$! Purpose   : link and run q_solve.exe
$!
$! Arguments : P1   DEBUG IF WANT DEBUG VERSION
$!
$! Created  30-JUN-1993   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ COPY D0$UTIL$GENERAL:Q_SOLVE.FOR,Q_GET_UNKNOWNS.FOR,PROB_QUEUE_BUSY.FOR, -
      Q_WRITE_HEADINGS.FOR,R_ERLANG_SOLVE.FOR Q_TEMP.FOR
$ @D0$UTIL:LGO Q_TEMP 'P1'
$ RENAME Q_TEMP.EXE Q_SOLVE.EXE
$ PURGE Q_SOLVE.EXE
$ DELETE Q_TEMP.FOR;*
$ DELETE Q_TEMP.OBJ;*
$EXIT:
$   EXIT
