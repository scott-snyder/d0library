$!     Extract expanded KEEP sequences from PATCHY PAM.
$!     FEP and SDP, 13 Jan 1986
$!
$ PUBLICFILES
$ PATCHY
$ ASS $2$DUA9:[PAIGE.ISAJET] PISA:
$ YPATCHY
PISA:ISAJET5 CDE_TEMP TTY TTY .GO
+OPTION,KEEPORD
+OPTION,SEQORD
+USE,ISACDE
+USE,CDE510
+EXE
+PAM
+QUIT
$ EDIT/EDT/NOCOMMAND CDE_TEMP.FOR
INCLUDE CDEHEAD.CRA
SUBSTITUTE/+KEEP/&KEEP/WHOLE
FIND END
INCLUDE CDETAIL.CRA
WRITE CDE_TEMP.CRA
QUIT
$ YPATCHY
PISA:ISAJET5 CDE CDE_TEMP TTY .GO
$ DELETE CDE_TEMP.*;*
$ WRITE SYS$OUTPUT "Common blocks written to CDE.FOR"
$ WRITE SYS$OUTPUT "This is made possible through the power of PATCHY."