#! /bin/csh -f
#!========================================================================
#!
#! Name      : SETUP_FZDIFF
#!
#! Purpose   : Setup FZDIFF
#!
#! Arguments : None
#!
#! Created  11-SEP-1992   Herbert Greenlee
#!
#!========================================================================
ln -sf `uff $d0util/fzdiff/fzdiff.rcp` fzdiff_rcp
alias fzdiff `uff $d0util/fzdiff.x`
