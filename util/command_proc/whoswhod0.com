$!------------------------------------------------
$!
$!     Valerio Luccio - D0 @ Stony Brook      17-DEC-1987
$!
$!   Search for a name (or part) in D0 mailing list
$!
$!------------------------------------------------
$   ON ERROR     Then Goto EXIT
$   ON CONTROL_Y Then Goto EXIT
$Prompt:
$   if p1 .eqs. "" then -
        read -
        /prompt="Name(s) (or part) to be searched (CTRL/Z to Exit) ? " -
        /end_of_file=EXIT -
        sys$command p1
$   type/nopage sys$input

Name                      MAIL address               Phone number
------------------------------------------------------------------------------

$   if p1 .nes. "" then search d0$docs:user_list.txt 'p1'
$   if p1 .eqs. "" then type/page d0$docs:user_list.txt 
$   WRITE SYS$OUTPUT " "
$EXIT:
$   EXIT
