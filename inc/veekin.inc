C--   common for kinematical fitting procedure
C-
C-   Created SEP -1991   Vladimir Burtovoy
C-   Updated  21-OCT-1991   Daria Zieminska  eliminate Zebra links 
      COMMON /VEEKIN/ NTR,XMT,IND(10),STR(4,10),ETR(4,10),ITR(4,10)
      INTEGER NTR,IND,ITR
      REAL    XMT,STR,ETR
C-            NTR                 - number of fitting tracks
C-            STR(1,i) & ETR(1,i) - momentum and error for i-th track
C-            STR(2,i) & ETR(2,i) - theta    and error for i-th track
C-            STR(3,i) & ETR(3,i) - phi      and error for i-th track
C-            STR(4,i) & ETR(4,i) - mass     and error for i-th track
C-            ITR(4,i) - positive track number in group of unmeasured
C-                       variables, if this i-th track is unmeasured
C-            ITR(4,i) - negative track number in group of measured
C-                       variables, if this i-th track is measured
C-            IND(i) - number, which indicates the  combination  of
C-                     measured or known (for mass) parameters for
C-                     i-th track. In general, the following combinations may
C-                     be treated  by the VKINEMA program:
C-
C-                     IND(i) = 1 -    P  Theta  Phi  M
C-                            = 2 -    P  Theta  Phi  -
C- for D0 Central Detector -> = 3 -    -  Theta  Phi  M
C-                            = 4 -    -  Theta  Phi  -
C-                            = 5 -    -    -     -   -
C-                            = 6 -    -    -     -   M
C-                            = 7 -    P    -     -   M
C----------------------------------------------------------------------
