$!========================================================================
$!
$! Name      : MAKE_D0PRODUCTS_LIST
$!
$! Purpose   : Create D0PRODUCTS.LIST
$!
$! Arguments : None
$!
$! Created   2-NOV-1991   Harrison B. Prosper
$! Modified 12-MAR-1992   Harrison B. Prosper 
$!      Minor changes
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   wr              :== WRITE SYS$OUTPUT
$   products_list   = "D0PRODUCTS.LIST"
$   products_temp   = "SYS$LOGIN:D0PRODUCTS.TMP"
$   
$!================================================
$!   Get list of command procedures
$!================================================
$   wr " Getting list of setup procedures"
$   DIRECTORY/NOHEAD -
             /NOTRAIL -
             /COLUMN=1 -
             /VERSION=1 -
             /OUTPUT='products_temp'    d0$root:[*]*setup*.com, -
                                        d0$root:[test.*]*setup*.com
$   IF F$SEARCH(products_temp) .EQS. ""
$   THEN
$       WR "%MAKEPROD-E-NOTCRE, ''products_temp' was not created"
$       EXIT
$   ENDIF
$   
$!================================================
$!   Remove TEST.
$!================================================
$   wr " Merge official and test areas"
$   COPY/NOLOG/NOCONFIRM    NL: 'products_temp'1
$   OPEN/APPEND outfile 'products_temp'1
$   OPEN/READ   infile  'products_temp'
$   
$Read_Temp:
$   
$   READ/END=Close_Temp infile  record
$   record  = record - "TEST."
$   record  = F$ELEMENT(0,";",record)
$   write outfile record
$   GOTO Read_Temp
$   
$Close_Temp:
$   
$   CLOSE infile
$   CLOSE outfile
$!================================================
$!   Sort temporary file
$!================================================
$   WR " Sorting list"
$   
$   SORT 'products_temp'1 'products_temp'
$   
$!================================================
$!   Create a default product name
$!================================================
$   WR " Creating ''products_list'"
$   COPY/NOLOG/NOCONFIRM    NL: 'products_list'_1
$   COPY/NOLOG/NOCONFIRM    NL: 'products_list'_2
$   
$   OPEN/READ   infile      'products_temp'
$   OPEN/APPEND listfile1   'products_list'_1
$   OPEN/APPEND listfile2   'products_list'_2
$   
$   last_record = ""
$   
$Get_Next_Record:
$   
$   READ/END=Close_Files infile  record
$   IF record .EQS. last_record THEN GOTO Get_next_Record
$   last_record = record
$   
$   dir_area    = F$ELEMENT(0,"]",record)
$   dir_area    = F$ELEMENT(1,"[",dir_area)
$   com_file    = F$ELEMENT(1,"]",record)
$   com_file    = F$PARSE(com_file,,,"NAME")
$   product     = com_file  - "_SETUP"
$   product     = product   - "SETUP_"
$   IF product .EQS. "SETUP"
$   THEN
$       product = dir_area
$   ENDIF
$   
$   record  = product + "/D0$" + dir_area + ":" + com_file + ".COM"
$   
$   IF F$EXTRACT(0,5,com_file) .EQS. "SETUP"
$   THEN
$       write listfile1 record
$   ELSE
$       write listfile2 record
$   ENDIF
$   
$   GOTO Get_Next_Record
$   
$Close_Files:
$   
$   CLOSE infile
$   CLOSE listfile1
$   CLOSE listfile2
$   
$   SORT 'products_list'_1 'products_list'_1
$   SORT 'products_list'_2 'products_list'_2
$   
$   COPY/CONC/NOCONFIRM/NOLOG 'products_list'_1,'products_list'_2 -
        'products_list'
$   
$EXIT:
$   
$   IF F$SEARCH("''products_temp'*") .NES. ""
$   THEN
$       DELETE/NOCONFIRM/NOLOG 'products_temp'*;*
$   ENDIF
$   
$   IF F$SEARCH("''products_list'_*") .NES. ""
$   THEN
$       DELETE/NOCONFIRM/NOLOG 'products_list'_*;*
$   ENDIF
$   
$   EXIT
