      SUBROUTINE PRL2EM( PRUNIT, LL2EM, NL2EM, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Dump the contents of L2EM
C-
C-   Inputs  :PRUNIT = unit number to print to.
C-            LL2EM  = link to L2EM
C-            NL2EM,CFL are irrlevant as L2EM is unique for an event
C-            IFL = 0 for full printout
C-   Outputs :
C-   Controls:
C-
C-   Created  16-MAY-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,IFL,NL2EM,LL2EM
      CHARACTER*(*) CFL
      INTEGER ICAND,J,NCAND,NREP,IP
C----------------------------------------------------------------------
C     not everything is implemented - just print it out
C
      IF (LL2EM .LE. 0) RETURN
      NCAND = IQ(LL2EM + 3)
      NREP = IQ(LL2EM + 2)
C
C     first part of table:
C
      WRITE(PRUNIT,'(1X,30(''*''),'' L2EM BANK '',30(''*''),/,
     &      ''       Eta/Phi/Layer                 '',
     &      ''EM1 EM1+2 EM3  EM4  FH1 -----SIGMA-----'',/,
     &      ''        L1         L2     ET   sEM  '',
     &      ''/sEM /sEM /sEM /sEM /sEM   3    5  3+MID'',/,
     &      ''    ------------------- ----- ----- ---- '',
     &      ''---- ---- ---- ---- ---- ---- -----'')')
      IP = LL2EM + 3
      DO ICAND=1,NCAND
        IP = IP + (ICAND-1)*NREP
        WRITE(PRUNIT,'(I3,1X,I3,''/'',I2,''/-- '',
     &    I3,''/'',I2,''/'',I2,2F6.1,5F5.2,3F5.1)')
     &    ICAND,(IQ(IP+J),J=1,5),(Q(IP+J),J=6,15)
      END DO
C
C     second part of table:
C
      WRITE(PRUNIT,'(/,''                          '',
     &  ''               Width'',/,
     &  ''      SH13  SH24  SH35  SH57 CONE fISO '',
     &  '' Eta  Phi   #Trks  Status  Parset'',/,
     &  ''     ----- ----- ----- ----- ---- ---- '',
     &  ''----------  -----  ------  ------'')')
      IP = LL2EM + 3
      DO ICAND=1,NCAND
        IP = IP + (ICAND-1)*NREP
        WRITE(PRUNIT,'(I3,1X,4F6.1,2F5.2,F6.2,F5.2,I6,2I8)')
     &    ICAND,(Q(IP+J),J=16,23),(IQ(IP+J),J=24,26)
      END DO
      WRITE(PRUNIT,'(/)')
C
  999 RETURN
      END
