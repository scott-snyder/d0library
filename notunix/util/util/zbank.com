$!------------------------------------------------
$!
$! Name      : ZBANK
$!
$! Purpose   : create pr,gz,bk,.link .zeb files for new Zebra bank
$!
$! Arguments : you will be prompted
$!
$! Created  11-OCT-1989   Rajendran Raja
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!------------------------------------------------
$!
$!   determine author name and date
$!
$!------------------------------------------------
$   def_author          = ""
$   
$   IF F$SEARCH("SYS$LOGIN:MY_EDITOR.TPU") .EQS. "" THEN GOTO AUTHOR
$   OPEN/READ MYFILE SYS$LOGIN:MY_EDITOR.TPU
$
$READ_EDITOR_TPU:
$
$   READ/END_OF_FILE=CLOSE_MY_FILE myfile YOUR_NAME
$   L   = F$LENGTH(YOUR_NAME)
$   line= F$EDIT(YOUR_NAME,"UPCASE")
$   IF F$LOCATE("$AUTHOR_NAME",line) .GE. L THEN GOTO READ_EDITOR_TPU
$
$   I   = F$LOCATE("""",YOUR_NAME)
$   YOUR_NAME   = F$EXTRACT(I+1,L-I,YOUR_NAME)
$   I   = F$LOCATE("""",YOUR_NAME)
$   YOUR_NAME   = F$EXTRACT(0,I,YOUR_NAME)
$
$   TIME_STAMP = F$time()
$!   WRITE SYS$OUTPUT "NAME: ''YOUR_NAME' DATE: ''TIME_STAMP'"
$
$CLOSE_MY_FILE:
$
$   CLOSE myfile
$
$!------------------------------------------------
$!------------------------------------------------
$!
$!   Determine Bank name, Links, Format and parentage
$!
$!------------------------------------------------
$
$ If P1 .eqs. "" Then Inquire P1 "Name of new ZEBRA bank"
$ Inquire nlinks  "Number of Links"
$ Inquire nstruc  "Number of structural links"
$ Inquire ndata   "Number of data words"
$ Inquire nform   "Format of Bank (.e.g -I/3F )"
$ Inquire nparent "Name of Parent Bank "
$ Inquire noff    -
 "Structural link  from which the bank being created hangs off parent "
$
$WRITE SYS$OUTPUT "Copying shell files over..."
$COPY D0$UTIL:BKXXXX.FOR BK'P1'.FOR
$COPY D0$UTIL:PRXXXX.FOR PR'P1'.FOR
$COPY D0$UTIL:GZXXXX.FOR GZ'P1'.FOR
$COPY D0$UTIL:XXXXFL.FOR 'P1'FL.FOR
$COPY D0$UTIL:XXXX.ZEB 'P1'.ZEB
$COPY D0$UTIL:IZXXXX.LINK IZ'P1'.LINK
$
$WRITE SYS$OUTPUT "Working on BK''P1'.FOR file..."
$SWAP/NOLOG BK'P1'.FOR XXXX 'P1'
$SWAP/NOLOG BK'P1'.FOR XPARENT 'nparent'
$SWAP/NOLOG BK'P1'.FOR XNL 'nlinks'
$SWAP/NOLOG BK'P1'.FOR XNS 'nstruc'
$SWAP/NOLOG BK'P1'.FOR XND 'ndata'
$SWAP/NOLOG BK'P1'.FOR XFORM "''nform'"
$SWAP/NOLOG BK'P1'.FOR XAUTHOR "''YOUR_NAME'"
$SWAP/NOLOG BK'P1'.FOR XDATE "''TIME_STAMP'"
$PURGE/NOLOG BK'P1'.FOR
$
$WRITE SYS$OUTPUT "Working on PR''P1'.FOR file..."
$SWAP/NOLOG PR'P1'.FOR XXXX 'P1'
$SWAP/NOLOG PR'P1'.FOR XDATE "''TIME_STAMP'"
$SWAP/NOLOG PR'P1'.FOR XAUTHOR "''YOUR_NAME'"
$PURGE/NOLOG PR'P1'.FOR
$
$WRITE SYS$OUTPUT "Working on GZ''P1'.FOR file..."
$SWAP/NOLOG GZ'P1'.FOR XXXX 'P1'
$SWAP/NOLOG GZ'P1'.FOR XPARENT 'nparent'
$SWAP/NOLOG GZ'P1'.FOR XAUTHOR "''YOUR_NAME'"
$SWAP/NOLOG GZ'P1'.FOR XDATE "''TIME_STAMP'"
$PURGE/NOLOG GZ'P1'.FOR
$
$WRITE SYS$OUTPUT "Working on ''P1'FL.FOR file..."
$SWAP/NOLOG 'P1'FL.FOR XXXX 'P1'
$SWAP/NOLOG 'P1'FL.FOR XAUTHOR "''YOUR_NAME'"
$SWAP/NOLOG 'P1'FL.FOR XDATE "''TIME_STAMP'"
$PURGE/NOLOG 'P1'FL.FOR
$
$WRITE SYS$OUTPUT "Working on IZ''P1'.LINK file..."
$SWAP/NOLOG IZ'P1'.LINK XXXX 'P1'
$SWAP/NOLOG IZ'P1'.LINK XPARENT 'nparent'
$SWAP/NOLOG IZ'P1'.LINK XOFF 'noff'
$SWAP/NOLOG IZ'P1'.LINK XAUTHOR "''YOUR_NAME'"
$SWAP/NOLOG IZ'P1'.LINK XDATE "''TIME_STAMP'"
$PURGE/NOLOG IZ'P1'.LINK
$
$WRITE SYS$OUTPUT "Working on ''P1'.ZEB file..."
$SWAP/NOLOG 'P1'.ZEB XXXX 'P1'
$SWAP/NOLOG 'P1'.ZEB XPARENT 'nparent'
$SWAP/NOLOG 'P1'.ZEB XNL 'nlinks'
$SWAP/NOLOG 'P1'.ZEB XNS 'nstruc'
$SWAP/NOLOG 'P1'.ZEB XND 'ndata'
$SWAP/NOLOG 'P1'.ZEB XAUTHOR "''YOUR_NAME'"
$SWAP/NOLOG 'P1'.ZEB XDATE "''TIME_STAMP'"
$PURGE/NOLOG 'P1'.ZEB
$
$PURGE/NOLOG SWAP.LOG
$
$
$!------------------------------------------------
$!
$!   Write Output disclaimer
$!
$!------------------------------------------------
$   WRITE SYS$OUTPUT " "
$   WRITE SYS$OUTPUT "***********************************************"
$   WRITE SYS$OUTPUT "   The following routines have been created"
$   WRITE SYS$OUTPUT "   BK''P1'.FOR,GZ''P1'.FOR,''P1'FL.FOR,PR''P1'.FOR"
$   WRITE SYS$OUTPUT "   IZ''p1'.LINK and ''p1'.ZEB"
$   WRITE SYS$OUTPUT "   Please edit and fill in these shells adequately"
$   WRITE SYS$OUTPUT "***********************************************"
$   WRITE SYS$OUTPUT " "
$EXIT:
$   EXIT
