$!---------------------------- DBLIB_VAX.COM ----------------------------
$!                                                                              
$! Command procedure for the creation of the basic library DBLIB310.OLB
$!                                                                              
$!----------------------------------------------------------------------   
$ SETUP PATCHY
$ YPATCHY                                                                       
  - D0$DBL3$DBL3:DBL3 TTY TTY .GO
+EXE.
+USE, VAX, *ZVAX.
+USE, $SERVER.
+USE, *DBLIB.       
+PAM, 11, R=QCDE, T=A.  FNAL::LIB:[LIB.CERN.CNL201A.PAM]ZEBRA.PAM
+PAM, 12,    T=C, T=A.  D0$DBL3$DBL3:DBL3.CAR
+QUIT.
