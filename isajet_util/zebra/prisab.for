C DEC/CMS REPLACEMENT HISTORY, Element PRISAB.FOR
C *5     9-NOV-1988 15:04:15 SERBAN "check bank exists"
C *4    15-MAR-1988 11:45:21 SERBAN "added HIGGS reaction"
C *3     2-FEB-1987 14:32:53 SERBAN "fixed format 104"
C *2    20-JAN-1987 14:10:47 SERBAN "fixed type declarations"
C *1     8-DEC-1986 15:16:56 SERBAN "print ISAB bank"
C DEC/CMS REPLACEMENT HISTORY, Element PRISAB.FOR
      SUBROUTINE PRISAB(PRUNIT,LISABI,NISAB,CFL,IFL)
C------------------------------------------------------------------
C-
C-  Print out for ISAB (event) bank
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LISABI,NISAB,CFL and IFL ignored
C-
C-     SDP  Jan,1986
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISAB.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LISABI,LISAB
      INTEGER NISAB,IFL,NREAC,K
      CHARACTER CFL*(*)
      CHARACTER*8 REACTN(10)
      DATA REACTN/'(TWOJET)','(E+E-)','(DRLYAN)',
     1            '(MBIAS)','(SUSY)','(WPAIR)','(HIGGS)',
     1            ' ',' ','(GEANT)'/
      LISAB=LQ(LHEAD-IZISAB)
      IF(LISAB.GT.0.AND.IQ(LHEAD+1).EQ.1001) THEN  ! check bank exists
C
C          Print titles
C
          WRITE(PRUNIT,100)
C
C   Print contents of bank
C
         WRITE(PRUNIT,101) IQ(LISAB+1)
         NREAC=IQ(LISAB+2)
         WRITE(PRUNIT,102) NREAC,REACTN(NREAC)
         WRITE(PRUNIT,103) IQ(LISAB+3),Q(LISAB+4)
         WRITE(PRUNIT,104) (Q(LISAB+K),K=5,22)
C                            
      ENDIF
      RETURN
  100 FORMAT('0',//,1X,80('-'),/,' ISAJET BEG. RUN BANK (ISAB)',/)
  101 FORMAT(/' Isajet version no.=',I8)
  102 FORMAT(/' Reaction=',I3,2X,A8)
  103 FORMAT(/' NO. of events generated=',I10,3X,' CM ENERGY=',F12.3)
  104 FORMAT(/' PT  limits=',6F15.3,/,' TH  limits=',6F15.3,/,
     $ ' PHI limits=',6F15.3)
      END
