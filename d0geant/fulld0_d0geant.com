$!******************************************************!                       
$!     File Name : FULLD0_D0GEANT.COM                   !                       
$!     Generated by the Program Builder                 !                       
$!     It defines a set of logical names for the        !                       
$!     Run Control Parameters files associated with     !                       
$!     the combined packages.                           !                       
$!     11-JUL-97 10:00:44                               !                       
$!******************************************************!                       
$
$ IF F$SEARCH("D0$D0GEANT:D0GEANT_SETUP.COM") .NES. "" THEN -                   
   @D0$D0GEANT:D0GEANT_SETUP.COM -                                              
   D0$D0GEANT$ROOT:[000000]FULLD0 "''P1'" "''P2'" "''P3'" 'RCP_TYPE'            