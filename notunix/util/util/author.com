
$!-----------------------------------------------------------------------------
$!   Name : AUTHOR.COM
$!-----------------------------------------------------------------------------
$   def_author          = ""
$   
$   IF F$SEARCH("SYS$LOGIN:MY_EDITOR.TPU") .EQS. "" THEN GOTO AUTHOR
$   OPEN/READ MYFILE SYS$LOGIN:MY_EDITOR.TPU
$
$READ_EDITOR_TPU:
$
$   READ/END_OF_FILE=CLOSE_MY_FILE myfile your_name
$   L   = F$LENGTH(your_name)
$   line= F$EDIT(your_name,"UPCASE")
$   IF F$LOCATE("$AUTHOR_NAME",line) .GE. L THEN GOTO READ_EDITOR_TPU
$
$   I   = F$LOCATE("""",your_name)
$   your_name   = F$EXTRACT(I+1,L-I,your_name)
$   I   = F$LOCATE("""",your_name)
$   your_name   = F$EXTRACT(0,I,your_name)
$
$   TIME_STAMP = F$time()
$   WRITE SYS$OUTPUT "NAME: ''your_name' DATE: ''TIME_STAMP'"
$
$CLOSE_MY_FILE:
$
$   CLOSE myfile
$   EXIT
