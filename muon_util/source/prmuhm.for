      SUBROUTINE PRMUHM(PRUNIT,LMUHM,NMUHM,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print out MUHM; Hit module data
C-
C-   Inputs  :  PRUNIT - Unit number for printout
C-              LMUHP - Bank address
C-              NMUHP - Bank number
C-              CFL - Flag to control print out
C-              IFL - How much to print
C-
C-   Outputs :  None
C-
C-   Created :  1-JAN-95  M. Fortner
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LMUHM,NMUHM,IFL,LFLAG
      CHARACTER CFL*(*)
      INTEGER L1,L2,I,J,K,GZMUHM
      EXTERNAL GZMUHM
C----------------------------------------------------------------------
      IF(CFL.EQ.'ALL') THEN
        L1 = 10
        L2 = 457
      ELSE
        L1 = 1
        L2 = 1
        LFLAG=LMUHM
        IF(LFLAG.EQ.0) THEN
          LFLAG=GZMUHM(NMUHM)
          IF(LFLAG.EQ.0) THEN
            WRITE(PRUNIT,100) NMUHM
 100        FORMAT(' NO MUHM BANK FOR MODULE #',I3)
            RETURN
          ENDIF
        ENDIF
      ENDIF
C
      WRITE(PRUNIT,200)
 200  FORMAT('0',10X,' BANK MUHM: MUON HIT MODULES '/)
C
      DO 300 I=L1,L2
        IF (I.EQ.1) THEN
          K = LFLAG
        ELSE
          K = GZMUHM(I)
          IF (K.EQ.0) GOTO 300
        ENDIF
C
        WRITE(PRUNIT,201) (IQ(K+J),J=1,5)
 201    FORMAT(1X,'Module ',I3 / 6X,'Raw:  ','MUD1 Offset ',I6,
     &         ', Flag:',Z8,', Hits:',I3,', MUHP # ',I5)
        IF(IQ(K+1).LT.400) THEN
          WRITE(PRUNIT,202) (IQ(K+J),J=6,10)
 202      FORMAT(6X,'Proc: ','Flag ',Z4,', MUOH Hits ',I3,', MUOH # ',
     &           I5,', MSCT Hits ',I3,', MSCT # ',I4)
        ELSE
          WRITE(PRUNIT,203) (IQ(K+J),J=6,8)
 203    FORMAT(6X,'Proc: ','Flag ',I4,', SAPH Hits ',I3,', SAPH # ',I5)
        ENDIF
        WRITE(PRUNIT,204) (IQ(K+J),J=13,15),(IQ(K+J),J=19,16,-1)
 204    FORMAT(6X,'Trig: ','Flag ',I2,', Cent. Fl ',I2,', Coarse ',Z8/
     &         12X,'Fine Cent (127-0):',4(1X,Z8))
        WRITE(PRUNIT,205) (IQ(K+J),J=22,20,-1)
 205    FORMAT(12X,'Latch Bits (95-0):',3(1X,Z8)/)
C
 300  CONTINUE
C
      RETURN
      END
