$ if (p1 .nes. "SET") then goto no_set
$ Set_Test = "True"
$ ReSet_Test = "False"
$ goto execute
$no_set:
$ if (p1 .nes. "RESET") then goto no_reset
$ Set_Test = "False"
$ ReSet_Test = "True"
$ goto execute
$no_reset:
$ if (p1 .nes. "SHOW") then goto error
$ Set_Test = "False"
$ ReSet_Test = "False"
$ Show_Lib = "True"
$ gosub show_test_libraries
$ goto exit
$ 
$no_set:
$error:
$ write sys$output ""
$ write sys$output "This command procedure cannot be invoked by @ but"
$ write sys$output "must be invoked with the appropriate global symbols"
$ write sys$output ""
$ goto exit
$!
$!
$!
$execute:
$ User_Command = P2
$ First_Pass = "TRUE"
$ if f$trnlnm("test_libraries","lnm$process_directory") .eqs. "" then -
     gosub setup_logical_table
$ define := "define/table=test_libraries"
$ deassign := "deassign/table=test_libraries"
$ gosub get_valid_libraries
$!
$start:
$ if (.not. First_Pass) .and. (P2 .nes. "") then goto exit
$ First_Pass = "FALSE"
$ if (User_Command .nes. "") then goto parse_User_Command
$ write sys$output -
              "(use commas to separate, ALL keyword, HELP for help, ? for show)"
$ inquire User_Command "Library (ies) ? 
$ if (User_Command .eqs. "") then goto exit
$parse_User_Command:
$ User_Command = f$edit(User_Command,"UPCASE,TRIM")
$ if User_Command .nes. "?" then goto parse1
$ gosub show_test_libraries
$ User_Command = ""
$ goto start
$parse1:
$ User_Command = f$edit(User_Command,"UPCASE,TRIM")
$ if User_Command .nes. "HELP" then goto parse2
$ gosub help_test_libraries
$ User_Command = ""
$ goto start
$!
$parse2:
$ request = f$edit(User_Command,"UPCASE,TRIM")
$next_field:
$ if (request .eqs. "") then goto end_field
$ lib_name = f$extract(0,f$locate(",",request),request)
$ request = request - lib_name - ","
$ if (lib_name .eqs. "") then goto next_field
$ gosub set_test_library
$ if lib_name .eqs. "ALL" then goto exit
$ goto next_field
$end_field:
$ User_Command = ""
$ goto start
$!
$!
$!
$exit:
$ exit
$!
$!
$!
$help_test_libraries:
$ if (Reset_Test) then goto other_help
$ type sys$input

This command procedure re-defines a set of logical names for each 
D0 library that you want to test. The logicals names will be defined as
search lists pointing the test area first, then the library area.

$ return
$other_help:
$ type sys$input

This command procedure resets the logical names to their original value
D0 library that you want to test. The logicals names will be defined as
search lists pointing the test area first, then the library area.

$ return
$!
$!
$!
$show_test_libraries:
$ type sys$input

The following libraries maybe tested:
(The libraries already under test are displayed in reverse video)

$ out_line = ""
$next_lib:
$ lib= f$search("d0$disk:[d0library.test]*.dir")
$ if lib .eqs. "" then goto end_lib
$ lib = f$parse(lib,,,"NAME","SYNTAX_ONLY")
$ if ((f$length(out_line) + 5 + f$length(lib)) .le. 80) then goto add_buffer
$ write sys$output out_line
$ out_line = ""
$add_buffer:
$ if (f$trnlnm("testing_''lib'") .or. f$trnlnm("testing_all")) then -
     out_line = out_line + "     [7m" + lib + "[0m"
$ if .not. (f$trnlnm("testing_''lib'") .or. f$trnlnm("testing_all")) then -
     out_line = out_line + "     " + lib
$ goto next_lib
$end_lib:
$ write sys$output out_line
$ write sys$output ""
$ return
$!
$!
$!
$get_valid_libraries:
$ valid_libraries = ""
$next_valid_lib:
$ lib= f$search("d0$disk:[d0library.test]*.dir")
$ if lib .eqs. "" then goto end_valid_lib
$ lib = f$parse(lib,,,"NAME","SYNTAX_ONLY")
$ valid_libraries = valid_libraries + lib + "/"
$ goto next_valid_lib
$end_valid_lib:
$ return
$!
$!
$!
$set_test_library:
$ if (lib_name .eqs. "ALL") then goto set_all_tests
$ if (f$locate(lib_name+"/",valid_libraries) -
      .ge. f$length(valid_libraries)) then goto invalid_lib
$ if ((Set_Test) .and. -
      (f$trnlnm("testing_''lib_name'") .or. f$trnlnm("testing_all"))) then -
     goto already_under_test
$ if (Reset_Test) .and. (f$trnlnm("testing_all")) then -
     goto reset_all_first
$ if (Reset_Test) .and. (.not. f$trnlnm("testing_''lib_name'")) then -
     goto not_under_test
$ write sys$output ""
$ if (Set_Test) then write sys$output "''LIB_NAME' is now under test"
$ if (Reset_Test) then write sys$output "''LIB_NAME' is not anymore under test"
$ write sys$output ""
$ if (Set_Test) then define/nolog testing_'lib_name' "YES"
$ if (Reset_Test) then define/nolog testing_'lib_name' "NO"
$ if (Set_Test) then define/nolog d0$'lib_name' d0$root:[test.'Lib_Name'],-
                                                d0$root:['Lib_Name']
$ if (Reset_Test) then deassign d0$'lib_name'
$next_lib_log:
$ lib_dir = f$search("d0$disk:[d0library.test.''lib_name']*.dir")
$ if (lib_dir .eqs. "") then goto end_lib_log
$ lib_dir = f$parse(lib_dir,,,"NAME","SYNTAX_ONLY")
$ if (Set_Test) then define/nolog d0$'lib_name'$'lib_dir' -
                             d0$root:[test.'lib_name'.'lib_dir'],-
                             d0$root:['lib_name'.'lib_dir']
$ if (Reset_Test) then deassign d0$'lib_name'$'lib_dir'
$ goto next_lib_log
$end_lib_log:
$ goto end_set_test_lib
$set_all_tests:
$ deassign/all
$ root_dir = "''f$trnlnm("D0$ROOT")'" - "]" + "TEST.]"
$ if (Set_Test) then -
     define/nolog/translation=concealed d0$root -
        "''root_dir'","''f$trnlnm("D0$ROOT")'"
$ if (Set_Test) then define/nolog Testing_All "YES"
$ if (Reset_Test) then define/nolog Testing_All "NO"
$ write sys$output ""
$ if (Set_Test) then -
     write sys$output "All existing libraries are under test"
$ if (Reset_Test) then -
     write sys$output "All existing libraries are not anymore under test"
$ write sys$output ""
$ goto end_set_test_lib
$already_under_test:
$ write sys$output ""
$ write sys$output "''LIB_NAME' is already under test"
$ write sys$output ""
$ goto end_set_test_lib
$reset_all_first:
$ write sys$output ""
$ write sys$output "When all libraries are under test, you must reset them all first "
$ write sys$output "Please use $ NOLIBTEST ALL"
$ write sys$output ""
$ goto end_set_test_lib
$invalid_lib:
$ if f$search("d0$disk:[d0library]''lib_name'.dir") .eqs. "" then -
     goto unknown_lib
$not_under_test:
$ write sys$output ""
$ write sys$output "''LIB_NAME' is not currently under test"
$ write sys$output ""
$ goto end_set_test_lib
$unknown_lib:
$ write sys$output ""
$ write sys$output "''LIB_NAME' is not a known D0 library"
$ write sys$output ""
$ goto end_set_test_lib
$end_set_test_lib:
$ return
$!
$!
$!
$setup_logical_table:
$ if f$trnlnm("lnm$file_dev","lnm$process_directory") .eqs. "TEST_LIBRARIES" -
     then return
$ Directory_Table = "Lnm$Process_Directory"
$ gosub Lnm_File_Dev_Content
$ if .not. Table_Empty then goto define_search_list
$ Directory_Table = "Lnm$System_Directory"
$ gosub Lnm_File_Dev_Content
$ if .not. Table_Empty then goto define_search_list
$ logical_tables = ",LNM$PROCESS,LNM$JOB,LNM$GROUP,LNM$D0,LNM$SYSTEM"
$! show symb logical_tables
$define_search_list:
$ logical_tables = "TEST_LIBRARIES"+Logical_Tables
$ create/name_table/parent=lnm$process_directory test_libraries
$ define/table=lnm$process_directory lnm$file_dev 'logical_tables'
$ return
$!
$!
$!
$Lnm_File_Dev_Content:
$ Table_Empty = "True"
$ logical_tables = ""
$ i=0
$next_table:
$ table = f$trnlnm("lnm$file_dev","''Directory_Table'",i)
$ if table .eqs. "" then goto no_more_tables
$ if Table_Empty then Table_Empty = "False"
$ if table .eqs. "LNM$SYSTEM" then table = "LNM$D0," + table
$ logical_tables = logical_tables+","+table
$ i = i+1
$ goto next_table
$no_more_tables:
$ return
