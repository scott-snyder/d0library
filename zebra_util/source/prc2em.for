      SUBROUTINE PRC2EM(PRUNIT, LC2EM1, NC2EM, CFL, IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Dump the contents of C2EM
C-
C-   Inputs  :PRUNIT = unit number to dump to.
C-            LC2EM1 = Pointer to one bank (CFL = 'ONE') or to the
C-                     Unused if CFL = 'ALL'.
C-            NC2EM  = Bank number, used only if CFL = 'ONE' and LL2EM.LE.0
C-            CFL    = Character flag:
C-                     'ONE'  LC2EM points to a bank, or if LC2EM.LE.0, 
C-                        NC2EM is the bank number.
C-                     'ALL' print all banks
C-            IFL = 0 for full printout, -1 to supress repetition banks
C-   Outputs : C2EM.DAT
C-   Controls:
C-
C-   Created  29-APR-1993   James T. McKinley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,IFL,NC2EM,LC2EM,LC2EM1,GZC2EM
      CHARACTER*(*) CFL
      INTEGER I,NR,NP
C----------------------------------------------------------------------
C
      CALL UPCASE(CFL,CFL)
C
      LC2EM = 0
      IF(CFL.EQ.'ONE')THEN
        IF(LC2EM1.LE.0)THEN
          IF(NC2EM.LE.0)THEN
            CALL ERRMSG('PRC2EM','INVALID BANK',
     &        'User requested C2EM bank is invalid','W')
            GOTO 999
          ENDIF
          LC2EM = GZC2EM()
          DO I=2,NC2EM
            LC2EM = LQ(LC2EM)
            IF(LC2EM.LE.0)THEN
              CALL ERRMSG('PRC2EM','INVALID BANK',
     &         'User requested C2EM bank is invalid','W')
              GOTO 999
            ENDIF
          ENDDO
        ELSE
          LC2EM = LC2EM1
        ENDIF
      ELSEIF(CFL.EQ.'ALL')THEN
        LC2EM = GZC2EM()
      ENDIF
C
      DOWHILE(LC2EM.GT.0)
C
        WRITE(PRUNIT,9998)
 9998   FORMAT(2X,'DUMP OF C2EM BANK CONTENTS:')
        WRITE(PRUNIT,9999)
 9999   FORMAT(2X,'---------------------------',/)
C
C L2 info for L1 candidate, (L2 parameter set independent info)
C
        WRITE(PRUNIT,10000) IQ(LC2EM+1),IQ(LC2EM+2),IQ(LC2EM+3),
     &      IQ(LC2EM+4)
10000   FORMAT(5X,'VERSION = ',I4,2X,'NFIX = ',I4,2X,'NR = ',I4,2X,
     &      'NP = ',I4,/)
C
        WRITE(PRUNIT,10010) IQ(LC2EM+5),IQ(LC2EM+6),IQ(LC2EM+7),
     &      IQ(LC2EM+8),IQ(LC2EM+9)
10010   FORMAT(5X,'TTETA = ',I3,2X,'TTPHI = ',I3,2X,'IETA = ',I3,
     &       2X,'IPHI = ',I3,2X,'ILYR = ',I3,/)
C
        WRITE(PRUNIT,10020) Q(LC2EM+10),Q(LC2EM+11),Q(LC2EM+12)
10020   FORMAT(5X,'AETA = ',F11.2,2X,'ZETA = ',F11.2,2X,'APHI = ',F11.2,
     &    /)
C
        WRITE(PRUNIT,10030) Q(LC2EM+13),Q(LC2EM+14),Q(LC2EM+15)
10030   FORMAT(5X,'XCLUS = ',F11.2,2X,'YCLUS = ',F11.2,2X,'ZCLUS = ',
     &      F11.2,/)
C
        WRITE(PRUNIT,10040) Q(LC2EM+16),Q(LC2EM+17),Q(LC2EM+18)
10040   FORMAT(5X,'ET_VTX0 = ',F11.2,2X,'ET_ZCORR = ',F11.2,2X,
     &      'SUMEM = ',F11.2,/)
C
        WRITE(PRUNIT,10050) Q(LC2EM+19),Q(LC2EM+20),Q(LC2EM+21)
10050   FORMAT(5X,'EM1/SUMEM = ',F6.2,2X,'(EM1+EM2)/SUMEM = ',F6.2,2X,
     &      'EM3/SUMEM = ',F6.2)
        WRITE(PRUNIT,10051) Q(LC2EM+22),Q(LC2EM+23)
10051   FORMAT(5X,'EM4/SUMEM = ',F6.2,2X,'FH1/SUMEM = ',F6.2,/)
C
        WRITE(PRUNIT,10060) Q(LC2EM+24),Q(LC2EM+25),Q(LC2EM+26)
10060   FORMAT(5X,'SIGMA3 = ',F8.4,2X,'SIGMA5 = ',F8.4,2X,
     &      'SIGMA3-SIG3_MID = ',F12.5,/)
C
        WRITE(PRUNIT,10070) Q(LC2EM+27),Q(LC2EM+28),Q(LC2EM+29),
     &      Q(LC2EM+30)
10070   FORMAT(5X,'SH13 = ',F8.4,2X,'SH24 = ',F8.4,2X,'SH35 = ',F8.4,
     &      2X,'SH57 = ',F8.4,/)
C
        IF(IFL.EQ.-1) GOTO 1
C
C repetition banks (parameter set dependent L2 info)
C
        WRITE(PRUNIT,10080)
10080   FORMAT(5X,'PAR_SET',2X,' NTRAK ',2X,'IFAILED',2X,'CUTBITS',2X,
     &      ' CONE_R ',2X,'FCONE_ET',2X,' DETA ',2X,' DPHI ')
        WRITE(PRUNIT,10090)
10090   FORMAT(5X,'-------',2X,'-------',2X,'-------',2X,'-------',2X,
     &      '--------',2X,'--------',2X,'------',2X,'------')
C
        NR = IQ(LC2EM+3)
        NP = IQ(LC2EM+4)
        DO I=0,NP-1
          WRITE(PRUNIT,10100) IQ(LC2EM+31+NR*I),IQ(LC2EM+32+NR*I),
     &        IQ(LC2EM+33+NR*I),IQ(LC2EM+34+NR*I),Q(LC2EM+35+NR*I),
     &        Q(LC2EM+36+NR*I),Q(LC2EM+37+NR*I),Q(LC2EM+38+NR*I)
10100     FORMAT(7X,I3,6X,I4,5X,I3,4X,I7,4X,F5.2,5X,F5.2,4X,F5.3,3X,
     &        F5.3)
        ENDDO
C
    1   CONTINUE
C
        WRITE(PRUNIT,10101)         ! FORM FEED
10101   FORMAT('1')
C
        IF(CFL.EQ.'ALL')THEN
          LC2EM = LQ(LC2EM)
        ELSEIF(CFL.EQ.'ONE')THEN
          LC2EM = 0
        ENDIF
C
      ENDDO
C
  999 RETURN
      END
