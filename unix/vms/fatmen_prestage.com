$!========================================================================
$!
$! Name      : FATMEN_PRESTAGE
$!
$! Purpose   : Remote execute interface for FATMEN PRESTAGE.
$!
$! Arguments : P1 - File containing generic names to prestage.
$!
$! Created   5-MAY-1994   Herbert Greenlee
$!
$!========================================================================
$!
$! Initialization
$!
$ if f$type(libtest).nes."STRING"
$ then
$       @d0$disk:[d0library]d0local
$       if f$type(libtest).nes."STRING" then exit
$       libtest fatmen
$       libbeta fatmen
$       libtes unix
$       define d0$unix$vms [],d0$beta:[gamma.unix.vms],d0$unix$root:[vms]
$       prestage = f$search("d0$unix$vms:fatmen_prestage.com")
$       if prestage.nes."" then @'prestage' "''p1'" "''p2'"
$       exit
$ endif
$!
$! Rename file list to a unique name (similar to name used by fatmen).
$!
$ new_list = "PRESTAGE_''f$extract(3,100,f$cvtime(,,""date""))'-" + -
             "''f$cvtime(,,""hour"")'-" + - 
             "''f$cvtime(,,""minute"")'-" + -
             "''f$cvtime(,,""second"")'.LIS"
$ old_list = f$search(p1)
$ if old_list.nes.""
$ then
$       rena/log 'old_list' 'new_list'
$ else
$       write sys$otuput "''FATMEN_PRESTAGE: ''old_list' not found"
$       exit
$ endif
$!
$! Submit job
$!
$ @d0$fatmen:prestage 'new_list'
$!
$! Clean up old prestage files
$!
$ set noon
$ del/log/created/before="-1-" prestage_*.lis;*
$ del/log/created/before="-1-" prestage_*.log;*
