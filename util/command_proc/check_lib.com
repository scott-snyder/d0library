$   sav_ver = f$verify(0)
$!========================================================================
$!
$! Name      : CHECK_LIB.COM
$!
$! Purpose   : List the flag files of a given library on specified machines.
$!
$! Arguments : P1 = Library to verify
$!             P2 = Node to check (include "::" in name)
$!             P3 = Output file
$!             P4 = T/O/B for Test/Official/Both
$!
$! Created  11-FEB-1992   Dan Holzman
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$! Initializations
$!
$! Header
$!
$   write sys$output ""
$   write sys$output "*** Library Verification ***"
$   write sys$output ""
$!
$! Get the library name.  Prompt 1 can be used.
$!
$   n = 0
$GET_LIB_NAME:
$   if p1 .eqs. ""
$   then
$       inquire/nopunct libname "Verify what library? "
$   else
$       libname = p1
$   endif
$!
$! Verify that this is a library.  If it is not, get another name.
$!
$   library_status 'libname'
$   if .not. d0$status 
$   then
$       n = n+1
$       write sys$output "''libname' is not a valid library.  Please reenter."
$       p1 = ""
$       if n .lt. 5 
$       then 
$           goto GET_LIB_NAME
$       else
$           goto end
$       endif
$   endif
$!
$! Get the node list.  P2 may be used.  "*" will get all nodes checked.
$!
$   if p2 .eqs. ""
$   then
$       inquire/nopunct nodestring "Nodes? [* for all]"
$   else
$       nodestring = p2
$   endif
$!
$! get output, screen is default
$!
$   if p3 .eqs. ""
$   then
$       inquire/nopunct outname "Output to? [sys$output]"
$       if outname .eqs. "" then outname = "sys$output"
$   else
$       outname = p3
$   endif
$   outname = f$edit (outname,"UPCASE")
$!
$! Test, Official, or both?
$!
$   n = 0
$GET_MODE:
$   if p4 .eqs. ""
$   then
$       inquire/nopunct mode "Test, Official, or Both? "
$   else
$       mode = p4
$   endif
$!
$! verify the option
$!
$   mode = f$edit (mode,"UPCASE")
$   if f$locate ("''mode'","TOB") .eqs. f$length ("TOB")
$   then
$       n = n+1
$       write sys$output "Invalid mode.  Please Reenter."
$       p4 = ""
$       if n .lt. 5 
$       then 
$           goto GET_MODE
$       else
$           goto end
$       endif
$   endif
$!
$! If * is used for p2, make a string composed of all nodes.
$!
$   if nodestring .eqs. "*"
$   then
$       nodestring = ""
$       search/out=check_lib.tmp d0$d0_news_data:d0_world.dis "::"
$       open/read nodelist check_lib.tmp 
$READ_LOOP:
$       read/end=END_READ nodelist temp
$       if f$locate ("$",temp) .nes. f$length(temp) then goto read_loop
$       temp = f$edit(f$extract(0,8,temp),"trim")
$       temp2 = nodestring
$       nodestring = temp2 + temp + ","
$       goto READ_LOOP
$END_READ:
$       close nodelist
$       del/nolog check_lib.tmp;
$   endif
$PROCESS:
$!
$! Force my error handling
$!
$   on error then continue
$   on control_y then goto END
$   on severe_error then continue
$!
$! Prepare the output file or re-direct to screen.
$!
$   if outname .nes. "SYS$OUTPUT" 
$   then 
$       open/write prog$out 'outname' 
$       outfile = "true"
$   else
$       define/nolog prog$out sys$output
$       outfile = "false"
$   endif
$!
$! Get the official listing for O and B modes only.
$!
$   if f$locate("''mode'", "OB") .nes. f$length ("OB")
$   then
$       write prog$out "*** Official Libraries ***"
$       write prog$out ""
$!
$! initialize for get_this_node
$!
$       nodetemp == nodestring
$NODE_LOOP_1:
$       call GET_THIS_NODE
$       if node .eqs. "" then goto end_loop
$       thistag = f$search("''node'D0$DISK:[D0LIBRARY.''libname']000*.*")
$       if thistag .nes. ""
$       then
$           thistagname = -
                f$ext(f$locate("000",thistag),f$length(thistag),thistag)
$           thistagdate = f$file_attributes (thistag,"CDT")
$       else
$           thistagname = "Tag not found on node ''node'"
$           thistagdate = ""
$       endif
$       write prog$out "''node'   ''thistagname'   ''thistagdate'"
$       goto NODE_LOOP_1
$END_LOOP:
$       write sys$output ""
$   endif
$!
$! Get the test listing for T and B modes only.
$!
$   if f$locate ("''mode'","TB") .nes. f$length ("TB")
$   then
$       write prog$out "*** Test Libraries ***"
$       write prog$out ""
$!
$! initialize for get_this_node
$!
$       nodetemp == nodestring
$NODE_LOOP_2:
$       call GET_THIS_NODE
$       if node .eqs. "" then goto end_loop
$       thistag = -
            f$search("''node'D0$DISK:[D0LIBRARY.TEST.''libname']000*.*")
$       if thistag .nes. ""
$       then
$           thistagname = -
                f$ext(f$locate("000",thistag),f$length(thistag),thistag)
$           thistagdate = f$file_attributes (thistag,"CDT")
$       else
$           thistagname = "Tag not found on node ''node'"
$           thistagdate = ""
$       endif
$       write prog$out "''node'   ''thistagname'   ''thistagdate'"
$       goto NODE_LOOP_2
$END_LOOP:
$   endif
$!
$END:
$   if outfile 
$   then 
$       close prog$out
$       if f$trnlnm("prog$name") .nes. "" then deassign prog$name
$   endif
$EXIT:
$   sav_ver = f$verify(sav_ver)
$   EXIT
$GET_THIS_NODE:SUBROUTINE
$!
$! get the current node name from the string
$!
$   comma = f$locate(",",nodetemp)
$   node == f$extract (0,comma,nodetemp)
$   nodetemp == nodetemp - node - ","
$ENDSUBROUTINE
