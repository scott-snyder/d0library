$!========================================================================
$!
$! Name      : SFO
$!
$! Purpose   : Invoke the SQL pre-compiler for FORTRAN files. This
$!             procedure is used by SFORTRAN.EXE
$!
$! Arguments : p1       File_spec
$!             p2       Qualifiers
$!
$! Created   7-OCT-1990   Harrison B. Prosper
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   file_spec   = p1
$   qualifiers  = p2
$   
$   SQLPRE      :== $SYS$SYSTEM:SQL$PRE
$   SQLPRE/FORTRAN'qualifiers' 'file_spec'
$   
$   EXTN        = F$PARSE(file_spec,,,"TYPE")
$   
$   IF EXTN .EQS. ".SFO$"
$   THEN
$       DELETE/NOLOG/NOCONFIRM 'file_spec';*
$   ENDIF
$EXIT:
$   EXIT
