$!------------------------------------------------
$!
$!     A.M.Jonckheere       7-OCT-1987
$!
$!   DELDIR.COM - deletes directory and all daughters
$!                in a Project directory
$!
$!------------------------------------------------
$ SAVE_VERIFY = F$VERIFY(0)
$   ON ERROR     Then $ Goto EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$   DEFDIR = F$ENVIRONMENT("DEFAULT")
$   DIRTMP = DEFDIR
$!
$   WRITE SYS$OUTPUT "''DEFDIR'"
$   WRITE SYS$OUTPUT "Delete this directory and ALL daughters????"
$   READ/PROMPT="ARE YOU SURE? (Y/N): " SYS$COMMAND YN
$   IF .NOT.YN THEN GOTO EXIT
$!
$   L = 0
$ LOOP:
$   LEN = F$LENGTH(DIRTMP)
$   LOFF = F$LOCATE(".",DIRTMP)
$   IF LOFF .NE. LEN 
$   THEN 
$       L = L + 1
$       DIRTMP = F$EXTRACT(LOFF+1,LEN-LOFF,DIRTMP)
$       GOTO LOOP
$   ENDIF
$!
$   TOPDIR = F$EXTRACT(0,LEN-1,DIRTMP)
$   IF f$search("*.dir") .nes. ""
$   THEN
$       set acl/default [...]*.dir
$       set file/prot=(o:rwed) [...]*.*;*
$   ENDIF
$   DEL/EXCLUDE=*.DIR [...]*.*;*
$!
$ DODIR:
$   fff = F$SEARCH("*.*")       ! reset search list
$   IF F$SEARCH("*.dir") .NES. ""
$   THEN
$       DEL/LOG [...]*.DIR;*
$       GOTO DODIR
$   ENDIF
$!
$ LAST:
$   IF L .EQ. 0
$   THEN 
$       WRITE SYS$OUTPUT "CAN'T DELETE THE TOP LEVEL DIRECTORY"
$       GOTO EXIT
$   ENDIF
$!
$   SET DEF [-]
$   SET ACL/DEFAULT 'TOPDIR'.DIR
$   SET PROT=(O:RWED) 'TOPDIR'.DIR
$   DEL/LOG 'TOPDIR'.DIR;*
$!
$ EXIT:
$   SAVE_VERIFY = F$VERIFY(SAVE_VERIFY)
$     EXIT

