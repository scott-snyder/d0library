C DEC/CMS REPLACEMENT HISTORY, Element PRISAF.FOR
C *2     9-NOV-1988 15:04:31 SERBAN "check bank exists"
C *1     9-DEC-1986 09:55:35 SERBAN "print end of run bank ISAF"
C DEC/CMS REPLACEMENT HISTORY, Element PRISAF.FOR
      SUBROUTINE PRISAF(PRUNIT,LISAFI,NISAF,CFL,IFL)
C-------------------------------------------------------------------
C-                                                                 -
C-  Print out for ISAF, end of run bank                            -
C-                                                                 -
C-  INPUT:                                                         -
C-  PRUNIT= unit number for printout                               -
C-  LISAFI,NISAF,CFL and IFL ignored                               -
C-                                                                 -
C-     SDP  Dec,1986                                               -
C-                                                                 -
C-------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZISAF.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LISAFI,LISAF
      INTEGER NISAF,IFL
      CHARACTER CFL*(*)
C
      LISAF=LQ(LHEAD-IZISAF)
      IF(LISAF.GT.0.AND.IQ(LHEAD+1).EQ.1002) THEN   ! check bank exists
C
C          Print titles
C
          WRITE(PRUNIT,100)
C
C   Print contents of bank
C
         WRITE(PRUNIT,101) IQ(LISAF+1),IQ(LISAF+2)
         WRITE(PRUNIT,102) Q(LISAF+3),Q(LISAF+4)
C
      ENDIF
      RETURN
  100 FORMAT('0',//,1X,80('-'),/,' ISAJET END RUN BANK (ISAF)',/)
  101 FORMAT(/,'  no. of events for this run=',I8,/,
     $         '  no. of events requested   =',I8)
  102 FORMAT(/,'  cross section (microbarns)=',E15.4,/,
     $         '       final weight         =',E15.4)
      END
