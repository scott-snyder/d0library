$!========================================================================
$!
$! Name      : DBL3_ERROR
$!
$! Purpose   : To explain DBL3 error codes
$!
$! Arguments : IQUEST(1) error code
$!
$! Created  23-OCT-1992   SHAHRIAR ABACHI
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$ if p1 .eqs. "" then -
   inquire p1 " Type IQUEST(1) error code >  "
$!
$ write sys$output " "
$ write sys$output -
 " ERROR CODE        ERROR EXPLANATION                     GENERATING ROUTINES"
$ write sys$output -
 " ----------        -----------------                           --------"
$!
$ error_code = p1
$ error_file = f$search("d0$dbl3:dbl3_error_codes.dat")
$ string = " ''error_code' |"
$! search 'error_file' "''string'"
$!
$   open/read/end_of_file=erfile_end tmpfile 'error_file'
$!
$ is = 0
$ er_loop:
$   read/end_of_file=search_end tmpfile rec
$   m1 = f$locate("''string'",rec)
$   if (m1 .gt. 10) then goto er_loop
$ try_more:
$   dum = f$extract(m1,80,rec)
$   lsw = f$locate("|",dum)
$ if lsw .gt. 20 then goto close_file
$   error_express = f$extract(lsw+1,80,dum)
$   l2 = f$locate("|",error_express)
$   error_expression = f$extract(0,l2-1,error_express)
$ write sys$output -
  "    ''error_code'        ''error_expression'"
$ rout_loop:
$   read/end_of_file=rout_end tmpfile rec
$   dum = f$extract(13,2,rec)
$! if (dum .nes. "  " .and. dum .nes. "--") 
$ if (dum .nes. "  ") 
$   then
$   is = is + 1
$   error_code = "  "
$   goto try_more
$ endif
$ close_file:
$ close tmpfile
$!
$!
$   open/read/end_of_file=erfile_end tmpfile 'error_file'
$ er_loop2:
$   read/end_of_file=search_end2 tmpfile rec
$   m2 = f$locate("''string'",rec)
$   if (m2 .gt. 10) then goto er_loop2
$ er_loop3:
$   ls = f$locate("|",rec)
$ if ls .gt. 20 then goto search_end2
$   dum = f$extract(m2+20,80,rec)
$   lsw = f$locate("|",dum)
$   gen_routin2 = f$extract(lsw+1,80,dum)
$   l2 = f$locate("|",gen_routin2)
$   gen_routin = f$extract(0,l2-1,gen_routin2)
$   gen_routin3 = f$extract(0,10,gen_routin2)
$ write sys$output -
 "                                                              ''gen_routin'"
$ if (gen_routin3 .nes. "          ")
$ then
$   read/end_of_file=search_end2 tmpfile rec
$   goto er_loop3
$ endif
$!
$ search_end2:
$ goto erfile_end
$ rout_end:
$ search_end:
$ write sys$output " No such error code exists !"
$ erfile_end:
$ close tmpfile
$!
$EXIT:
$   EXIT
