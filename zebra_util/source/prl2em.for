
      SUBROUTINE PRL2EM( PRUNIT, LL2EM1, NL2EM, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Dump the contents of L2EM
C-
C-   Inputs  :PRUNIT = unit number to print to.
C-            LL2EM  = Pointer to one bank (CFL = 'ONE') or to the
C-                     first of a linear structure (CFL = 'LINEAR')
C-            NL2EM  = Bank number, used only if CFL = 'ONE' and LL2EM = 0
C-            CFL    = Character flag:
C-                     'ONE'  LL2EM point to a bank, or if <0, NL2EM is
C-                            the bank number.
C-                     'LINEAR'  LL2EM points to the first bank of the
C-                            linear structure
C-            IFL = 0 for full printout
C-   Outputs :
C-   Controls:
C-
C-   Created  16-MAY-1990   Richard V. Astur
C-   Revised   8-AUG-1992   S Fahey  for version 2 of L2EM
C-   Updated   7-SEP-1992   S Fahey  for version 3 of L2EM
C-   Updated   7-DEC-1992   James T. McKinley version 4 of L2EM, also fix
C-                          formats for SIG3_MID (causing output conversion
C-                          error), SIGMA5, SIGMA3, SH13, SH24, SH35, SH57 
C-                          (increase precision to 2 decimal places), 
C-                          cluster X,Y,Z (increase size, negative values
C-                          greater than 99 would cause output conversion 
C-                          error).
C-  Updated    6-AUG-1993   James T. McKinley  fix bug in handling linear
C-                          chain.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZL2EM.LINK'
      INCLUDE 'D0$PARAMS:L2_EM.PARAMS'
      INTEGER PRUNIT,IFL,NL2EM,LL2EM,LL2EM1,GZL2EM,LZLOC
      CHARACTER*(*) CFL
      INTEGER ICAND,J,NCAND,NREP,NVER,IP,CUTBITS
      CHARACTER*40 SHAPE
      CHARACTER*20 TRACK
C----------------------------------------------------------------------
C
      LL2EM = LL2EM1
      IF (CFL .EQ. 'LINEAR') THEN
        IF (LL2EM .LE. 0) GOTO 990
      ELSEIF (CFL .EQ. 'ONE') THEN
        IF (LL2EM .LE. 0) THEN
          IF (NL2EM .EQ. 0) GOTO 980
          LL2EM = LZLOC( IXMAIN,'L2EM',NL2EM)
        ENDIF
      ELSE
        WRITE(PRUNIT, 1000) CFL
 1000   FORMAT(/' ** PRL2EM ** Illegal value of CFL =',A/)
        GOTO 999
      ENDIF
    1 CONTINUE
      NCAND = IQ(LL2EM + 3)
      NREP = IQ(LL2EM + 2)
      NVER = IQ(LL2EM + 1)
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
     &    I3,''/'',I2,''/'',I2,2F6.1,5F5.2,2F5.2,1F9.2)')
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
        WRITE(PRUNIT,'(I3,1X,4F6.2,2F5.2,F6.2,F5.2,I6,2I8)')
     &    ICAND,(Q(IP+J),J=16,23),(IQ(IP+J),J=24,26)
      END DO
C
C     third part of table:
C
      IF (NVER.GE.5) THEN
C
        WRITE(PRUNIT,'(/,''      '',
     &  ''ZEta  AEta  APhi  Xclus    Yclus    Zclus    Et_Z_E_corr '',
     &  /, ''     ----- ----- ----- -------- -------- --------
     &  ----------- '')')
C
        IP = LL2EM + 3
        DO ICAND=1,NCAND
          IP = IP + (ICAND-1)*NREP
          WRITE(PRUNIT,
     &      '(I3,1X,3F6.2,1F8.2,1X,1F8.2,1X,1F8.2,2X,1F7.2)')
     &      ICAND,Q(IP+34),(Q(IP+J),J=27,32)
        END DO
C
      ELSEIF (NVER.GE.4) THEN
C
        WRITE(PRUNIT,'(/,''      '',
     &  ''ZEta  AEta  APhi  Xclus    Yclus    Zclus    Et_Zcorr '',
     &  /, ''     ----- ----- ----- -------- -------- -------- --------
     &'')')
C
        IP = LL2EM + 3
        DO ICAND=1,NCAND
          IP = IP + (ICAND-1)*NREP
          WRITE(PRUNIT,
     &      '(I3,1X,3F6.2,1F8.2,1X,1F8.2,1X,1F8.2,2X,1F7.2)')
     &      ICAND,Q(IP+34),(Q(IP+J),J=27,32)
        END DO
C
      ELSEIF (NVER.GE.2) THEN
C
        WRITE(PRUNIT,'(/,''      '',
     &  ''AEta  APhi  Xclus    Yclus    Zclus    Et_Zcorr '',
     &  /, ''     ----- ----- -------- -------- -------- --------
     &'')')
C
        IP = LL2EM + 3
        DO ICAND=1,NCAND
          IP = IP + (ICAND-1)*NREP
          WRITE(PRUNIT,'(I3,1X,2F6.2,1F8.2,1X,1F8.2,1X,1F8.2,2X,1F7.2)')
     &      ICAND,(Q(IP+J),J=27,32)
        END DO
C
      END IF
C
C     forth part of the table
C
      IF (NVER.GE.3) THEN
        CUTBITS = IQ(IP+33)
        SHAPE = 'NO SHAPE CUTS'
        TRACK = 'IGNORE TRACK'
        IF (BTEST(CUTBITS,ELE_BIT))   SHAPE = 'ELECTRON'
        IF (BTEST(CUTBITS,GAM_BIT))   SHAPE = 'PHOTON'
        IF (BTEST(CUTBITS,LONG_BIT))  SHAPE = SHAPE(1:9) // 'LONG'
        IF (BTEST(CUTBITS,TRANS_BIT)) SHAPE = SHAPE(1:14) // 'TRANS'
        IF (BTEST(CUTBITS,TIGHT_BIT)) SHAPE = SHAPE(1:20) // 'TIGHT'
        IF (BTEST(CUTBITS,ISO_BIT))   SHAPE = SHAPE(1:26) // 'ISOLATION'
        IF (BTEST(CUTBITS,TRK_REQ_BIT))  TRACK = 'REQUIRE TRACK'
        IF (BTEST(CUTBITS,TRK_VETO_BIT)) TRACK = 'VETO TRACK'
        WRITE(PRUNIT,'(/,''      Cuts used:  '')')
        WRITE(PRUNIT,'(A58,A20)') SHAPE,TRACK
      END IF
      WRITE(PRUNIT,'(/)')
C
C *** Look if another bank is needed
C
      IF (CFL .EQ. 'ONE') GOTO 999
      IF (CFL .EQ. 'LINEAR') THEN
        LL2EM = LQ(LL2EM)
        IF (LL2EM .NE. 0) GOTO 1
      ENDIF
  999 RETURN
C
C ** Errors
C
  990 WRITE( PRUNIT, 2000) LL2EM
 2000 FORMAT(/' ** PRL2EM ** called for LINEAR without valid bank',
     &        ' pointer, L2EM =',I10/)
      GOTO 999
  980 WRITE( PRUNIT, 2100) LL2EM,NL2EM
 2100 FORMAT(/' ** PRL2EM ** called for ONE without bank pointer and',
     &        ' bank number, LL2EM = ',I10,' NL2EM =', I10/)
      GOTO 999
      END
