#!/bin/csh
#
#   Setup script for SPYTHIA
#
alias spythia `uff $d0spythia/spythia.x`
alias michmodel `uff $d0spythia/michmodel.x`
ln -sf `uff $d0spythia/spythia.rcp` spythia_rcp
#
echo 'Setup SPYTHIA'
echo 'To run Spythia, type spythia'
echo 'To run the Michigan model maker, type michmodel'
echo ''
echo 'Spythia is controlled by spythia_rcp'

exit 0
