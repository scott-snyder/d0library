$!========================================================================
$!
$! Name      : D0SETUP
$!
$! Purpose   : Execute SETUP procedures for D-ZERO products.
$!              Search for *SETUP*.COM until one of the following succeeds:
$!              
$!                  o   If D0 
$!                      Search ONLINE disk for setup_xxxx.com
$!                  o   Search d0$xxxx: for setup_xxxx.com
$!                  o   Search D0$UTIL:D0PRODUCTS.LIST
$!                  o   Search using FERMILAB SETUP
$!
$! Arguments : P1   Product name
$!            [P2]  Sub-product name
$!            [P3-P8]  arguments
$!
$! Created  30-OCT-1991   Harrison B. Prosper
$! Modified 12-MAR-1992   Harrison B. Prosper 
$!      Improve search
$! Modified 25-MAR-1992   Harrison B. Prosper 
$!      Search only top-most area of online area
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   Product     = p1
$   sub_product = p2
$   
$   arg1    = p3
$   arg2    = p4
$   arg3    = p5
$   arg4    = p6
$   arg5    = p7
$   
$   command_procedure   = ""
$   Used_product_list   = 0
$   
$   WR              :== write sys$output
$
$   MESSAGE_OFF     :== SET MESSAGE/NOFACILITY/NOIDENT/NOSEVERITY/NOTEXT
$   MESSAGE_ON      :== SET MESSAGE/FACILITY/IDENT/SEVERITY/TEXT
$   
$   product_list    = "D0$UTIL:D0PRODUCTS.LIST"
$   product_file    = "SYS$LOGIN:D0PRODUCTS.TMP"
$   NumberFound     == 0
$   
$!================================================
$!   Get Product Name
$!================================================
$   IF Product .EQS. ""
$   THEN
$       INQUIRE Product "D-ZERO Product Name "
$   ENDIF
$   IF Product .EQS. "" THEN GOTO EXIT
$   
$!================================================
$!
$!   If this is D0:: then first search ONLINE
$!   disk area product_EXEC
$!   
$!================================================
$   
$   IF F$TRNLNM("SYS$CLUSTER_NODE") .EQS. "D0::"
$   THEN
$       product_com_file    = "ONLINE:[''product'_EXEC]SETUP*.COM"
$   
$       call Search_For_Procedure 'product_com_file'
$   ENDIF
$   
$   IF NumberFound .GT. 0 THEN GOTO Get_Procedure_Names
$   
$!================================================
$!
$!   Search D0Library for setup procedures
$!   
$!================================================
$   
$   product_com_file    = "D0$" + Product + "$ROOT:[000000]*SETUP*.COM"
$   
$   call Search_For_Procedure 'product_com_file'
$   
$   IF NumberFound .GT. 0 THEN GOTO Get_Procedure_Names
$   
$!================================================
$!
$!  Search of D0library failed so search
$!  for product in product list
$!
$!================================================
$   
$   IF F$SEARCH(product_list) .NES. ""
$   THEN
$       MESSAGE_OFF
$       SEARCH 'product_list'/OUTPUT='product_file' "''product'/"
$       MESSAGE_ON
$   ENDIF
$   
$   IF F$SEARCH(product_file) .EQS. "" THEN GOTO Execute_FNAL_SETUP
$   
$!================================================
$!   Get Procedure Names
$!================================================
$   
$Get_Procedure_Names:
$   
$   Last_Record = ""
$   OPEN/READ infile 'product_file'
$   NumberFound == 0
$   
$DO_LOOP:
$       
$   READ/END=CloseFile infile record
$   IF record .EQS. last_record THEN GOTO do_loop
$   last_record = record
$   NumberFound == NumberFound + 1
$   string  = F$STRING(NumberFound)
$   com_file = F$ELEMENT(0,";",F$EDIT(F$ELEMENT(1,"/",record),"TRIM"))
$!================================================
$!   If SETUP.COM is amongst this list then execute
$!   it only.
$!================================================
$   IF F$PARSE(com_file,,,"NAME") .EQS. "SETUP"
$   THEN
$       NumberFound == 1
$       com_file1 = com_file
$       GOTO CloseFile
$   ENDIF
$   
$   com_file'string' = com_file
$   GOTO DO_LOOP
$       
$CloseFile:
$   
$   CLOSE infile
$   
$   IF NumberFound .EQ. 0 THEN GOTO Execute_FNAL_SETUP
$   
$!================================================
$!   Present list of procedures
$!================================================
$       
$   IF NumberFound .EQ. 1
$   THEN
$       command_procedure = com_file1
$   ELSE
$       
$       i   = 0
$DO_LOOP2:
$       i   = i + 1
$       string  = F$STRING(i)
$       com_file = com_file'string'
$       wr "''string'.   ''com_file'"
$       IF i .LT. NumberFound THEN GOTO DO_LOOP2
$       
$       WR "  "
$       INQUIRE which_one "Which one [1] "
$       IF  which_one  .EQS. "" 
$       THEN
$           which_one = "1"
$       ENDIF
$       
$       command_procedure = com_file'which_one'
$       
$   ENDIF
$   
$!================================================
$!   execute procedure
$!================================================
$   
$Execute_Procedure:
$   
$   IF Command_procedure .EQS. ""
$   THEN
$       WRITE SYS$OUTPUT "%D0SETUP-E-BADPROD, Unknown D-ZERO Product"
$       GOTO EXIT
$   ENDIF
$   IF F$SEARCH(Command_Procedure) .EQS. ""
$   THEN
$       WRITE SYS$OUTPUT  -
        "%D0SETUP-E-NOTFOUND, Cannot find procedure ''command_procedure'"
$       GOTO EXIT
$   ENDIF
$   
$   wr "  Executing: ''command_procedure'"
$   wr "  "
$   
$   @'command_procedure' "''arg1'"   "''arg2'"  "''arg3'"   "''arg4'" -
        "''arg5'"
$   GOTO EXIT
$   
$Execute_FNAL_Setup:
$   
$   IF F$TYPE(SETUP) .NES. ""
$   THEN
$       wr "  Executing: SETUP ''product'"
$       SETUP "''product'" "''sub_product'" "''arg1'"
$   ENDIF
$   
$EXIT:
$   IF F$SEARCH("''product_file'*;*") .NES. ""
$   THEN
$       DELETE/NOCONFIRM/NOLOG 'product_file'*;*
$   ENDIF
$   EXIT
$   
$!================================================
$!   Search routine
$!================================================
$Search_for_procedure:  SUBROUTINE
$   
$   product_com_file    = P1
$   NameList            = "/"
$   NumberFound         == 0
$   
$   OPEN/WRITE outfile1 'product_file'_1
$   OPEN/WRITE outfile2 'product_file'_2
$   
$Get_Next_Com_File:
$   
$   com_file    = F$SEARCH(product_com_file)
$   
$   IF com_file .EQS. "" THEN GOTO Close_Com_FIle
$   com_name    = F$PARSE(com_file,,,"NAME")    !Get name of procedure
$   
$!================================================
$!   If sub_product name is specified then look
$!   only for those procedures which contain the
$!   sub_product name.
$!================================================
$   IF sub_product .EQS. "" THEN Goto Check_Name
$   
    IF F$LOCATE(sub_product,com_name) .GE. F$LENGTH(com_name) THEN  -
       GOTO Get_Next_Com_File
$   
$!================================================
$!   Check Procedure Name
$!================================================
$Check_Name:
$   
$   SubString   = "/" + com_name + "/"
$   
$   IF F$LOCATE(SubString,NameList) .LT. F$LENGTH(NameList) THEN -
        GOTO Get_Next_Com_File
$   
$!================================================
$!   Add new procedure name to Namelist
$!================================================
$   
$   NameList = NameList + "/" + com_name + "/"
$   
$   NumberFound   == NumberFound + 1
$   
$   IF F$EXTRACT(0,5,com_name) .EQS. "SETUP"
$   THEN
$       WRITE outfile1 product + "/" + com_file
$   ELSE
$       WRITE outfile2 product + "/" + com_file
$   ENDIF
$   
$!   wr " Name: ''NameList'"
$!   wr "        File: ''com_file'"
$   
$   GOTO Get_Next_Com_File
$       
$Close_Com_FIle:
$   
$   CLOSE outfile1
$   CLOSE outfile2
$   
$   COPY/CONC/NOCONFIRM/NOLOG 'product_file'_1,'product_file'_2 'product_file'
$   IF F$SEARCH("''product_file'_*") .NES. ""
$   THEN
$       DELETE/NOCONFIRM/NOLOG 'product_file'_*;*
$   ENDIF
$   
$ENDSUBROUTINE
