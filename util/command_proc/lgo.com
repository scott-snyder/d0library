$!------------------------------------------------
$!
$!   Compile, link and run a single Fortran file
$!      temporarily redfines d0$inc,d0$params,d0$links to point locally first
$!
$!     J. Linnemann      16-March-88
$!
$!      P1, the name of a .FOR file containing a main program
$!      P2 (optional) is DEBUG if desired
$!
$!------------------------------------------------
$   ON ERROR     Then Goto EXIT
$   ON CONTROL_Y Then Goto EXIT
$ DefDir = "''F$ENVIRONMENT(""DEFAULT"")'"
$ P8 = "INC"
$ GOSUB set_local 
$ P8 = "LINKS"
$ GOSUB set_local 
$ P8 = "PARAMS"
$ GOSUB set_local 
$ pname = "''P1'" - ".FOR"
$ IF (P2.EQS."DEBUG") 
$   THEN 
$       FORTRAN/DEB=all/NOOPT 'P1'
$       WRITE SYS$OUTPUT "Linking ''pname'"
$       LINK/DEBUG 'pname',D0$UTIL:DEB_UTIL4.OPT/OPT,'CERNP'
$   ELSE
$       FORTRAN 'P1'
$       WRITE SYS$OUTPUT "Linking ''pname'"
$       LINK 'pname',D0$UTIL:UTIL4.OPT/OPT,'CERNP'
$ ENDIF
$!  the next line says to get the input for the program from the terminal
$ DEFINE/USER_MODE SYS$INPUT SYS$COMMAND
$ RUN 'pname'
$EXIT:
$ P8 = "INC"
$ GOSUB reset_local 
$ P8 = "LINKS"
$ GOSUB reset_local 
$ P8 = "PARAMS"
$ GOSUB reset_local 
$   EXIT
$set_local: 
$ old'P8' = F$TRNLNM("D0$''P8'","LNM$PROCESS_TABLE")
$ old = old'P8'
$ IF (old.NES."") 
$ THEN 
$   DEFINE D0$'P8' "''DefDir'","''old'",D0$'P8'$ROOT:[000000]
$   SHO LOG D0$'P8'
$ ELSE
$   DEFINE D0$'P8' "''DefDir'",D0$'P8'$ROOT:[000000]
$ ENDIF
$ RETURN
$reset_local: 
$ DEASSIGN D0$'P8'
$   old = old'P8' 
$ IF (old.NES."") 
$ THEN 
$   DEFINE D0$'P8' "''old'",D0$'P8'$ROOT:[000000]
$   WRITE SYS$OUTPUT "Reset Logicals:"
$   SHO LOG D0$'P8'
$ ENDIF
$ RETURN
