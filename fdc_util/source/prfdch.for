      SUBROUTINE PRFDCH(PRUNIT,LJFDCH,NFDCH,CFL,IFL)
C------------------------------------------------------------------
C-
C-  Purpose and Methods : Print out  FDCH (Forward Drift Chamber 
C-                        hits Header) bank
C-
C-  Input  : PRUNIT= unit number for printout
C-           LKFDCH = bank address
C-           NFDCH = bank number  (not used)
C-           CFL   = only one bank per path (not necessary)
C-           IFL   = level of printout
C-              IFL   = 0  no printout
C-              IFL  >= 1  prints number of hits in FDC
C-              IFL   = 3  print out full bank contents
C-
C-   Created  xx-JAN-1987   Daria Zieminska
C-   Updated  14-MAR-1989   Jeffrey Bantly
C-   Updated  24-JUL-1990   Jeffrey Bantly  add delay line word 
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-
C-------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER PRUNIT,NFDCH,IFL,LKFDCH,LJFDCH
      INTEGER NHITSW,NHITDL,NWIRES,NWORDS,IVERS
      INTEGER GZFDCH
C
      CHARACTER CFL*(*)
C-------------------------------------------------------------------
C
      IF (IFL.LE.0) GOTO 999
      LKFDCH=LJFDCH
      IF(LKFDCH.LE.0) LKFDCH=GZFDCH()
      IF(LKFDCH.LE.0) THEN
        WRITE(PRUNIT,1011) LKFDCH
        GO TO 999
      END IF
C
      IVERS=IBITS(IQ(LKFDCH),13,5)
      NHITSW=IQ(LKFDCH+1)    ! number of hits in Forward Chamber
      NHITDL=IQ(LKFDCH+10)   ! number of delay line hits in Forward Chamber
      IF ( IFL .GE. 1 ) WRITE(PRUNIT,101) IVERS,NHITSW,NHITDL
C
      IF ( IFL .LT. 3 ) GOTO 999
      NWORDS=IQ(LKFDCH+2)
      NWIRES=IQ(LKFDCH+3)
      WRITE(PRUNIT,102) NWIRES,NWORDS
C
      NWORDS=IQ(LKFDCH+4)
      NWIRES=IQ(LKFDCH+5)
      WRITE(PRUNIT,103) NWIRES,NWORDS
C
      NWORDS=IQ(LKFDCH+6)
      NWIRES=IQ(LKFDCH+7)
      WRITE(PRUNIT,104) NWIRES,NWORDS
C
      NWORDS=IQ(LKFDCH+8)
      NWIRES=IQ(LKFDCH+9)
      WRITE(PRUNIT,105) NWIRES,NWORDS
C
  101 FORMAT(/' Hit bank for Forward Drift Chamber FDCH - Version',I3/,
     $' Number of hits in FDC=',I8,
     $'    Number of delay line hits in FDC',I8)
  102 FORMAT(' Theta hits banks FTSC - Number of wires per sector=',I4,
     $'  Number of words per hit=',I4)
  103 FORMAT(' Theta hits banks FTDA - Number of FADCs per sector=',I4,
     $'  Number of words per hit=',I4)
  104 FORMAT(' Phi   hits banks FPSC - Number of wires per sector=',I4,
     $'  Number of words per hit=',I4)
  105 FORMAT(' Phi   data banks FPDA - Number of FADCs per sector=',I4,
     $'  Number of words per hit=',I4)
 1011 FORMAT(/' WRONG ADDRESS, LKFDCH =',I10)
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
