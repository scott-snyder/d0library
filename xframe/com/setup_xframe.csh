#! /bin/csh -f 

setenv UIDPATH `uff $d0xframe/source/d0xuid`
alias d0x `uff $d0xframe/d0x`
alias d0x_color `uff $d0xframe/source/d0xuid`
alias d0x_bw `uff $d0xframe/source/d0xbw`

echo "    d0x    now available, default for COLOR"
echo "  d0x_bw is an alias to swithch to B&W"
echo "  d0x_color   for color"
