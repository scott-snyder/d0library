      SUBROUTINE PRMUD1(PRUNIT,LMUD1,NMUD1,CFL,IFL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    PRINT OUT MUD1 - RAW MUON DATA
CC
CC    PRUNIT - UNIT NUMBER FOR PRINTOUT
CC    LMUD1 - BANK ADDRESS
CC    NMUD1 - BANK NUMBER
CC    CFL - FLAG TO CONTROL PRINT OUT 'ALL' gets header
CC    IFL - HOW MUCH TO PRINT
CC
CC    THERE WILL ONLY BE A SINGLE RAW MUON BANK
CC    SO IGNORE IFL
CC
CC    DH MARCH 1986
CC    DH UPDATE 10-86
CC    SK UPDATE 16-MAY-87 DH FIX FORMAT 7-87
CC    SK UPDATE 14-DEC-88
CC    Dh 1/90 latest format
CC    DH 11/90 MASK 16 BITS
CC    DH 11/90 use version number: setup for OLD (pre 1991) GEANT
CC       and current (1990) data
CC    DH 1/91 NEWEST HEADER
CC    DH 4/91 add option 'ALL'
CC    OE 5/91 add SAMUS words
CC    DH 11/91 add sequencer words
CC    DH 5/92 12 bits for module word count
CC    DW 7/94 check mud1 format (1a/1b)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER PRUNIT,LMUD1,NMUD1,IFL,LRAW
      CHARACTER CFL*(*)
      INTEGER LZLOC,NMUONH,I,J,K,NVERS,ND,IMODH,ICONTW,NMODC
      INTEGER IMOD,IPLN,ICELL,IERR,IADD,GZMUD1,NRMUD,NCRATE,CR
      INTEGER L,NH,NM,MODNUM(200),MODWRD(200),B12,B16,B8,PL,IE,IO
      INTEGER IVER,MUDVER
C
      EXTERNAL MUDVER
C
      DATA B8,B12,B16/255,4095,65535/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      LRAW=LMUD1
      IF(LRAW.EQ.0) THEN
        LRAW=GZMUD1(0)
        IF(LRAW.EQ.0) THEN
          WRITE(PRUNIT,100)
  100     FORMAT(' NO MUON RAW DATA BANK')
          GO TO 999
        ENDIF
      ENDIF
C
C check format (1a/1b)
      IVER = MUDVER(0)
      IF(MOD(IVER,10).GE.2) THEN
        CALL PRMUD1_1B(PRUNIT,LRAW,NMUD1,CFL,IFL)
        GOTO 999
      ENDIF
C
      NCRATE=0
      WRITE(PRUNIT,200)
C
CC              Loop over crates and fill flagging arrays
      L=LRAW
 1000 CONTINUE
C     -- check if crate exists or not...
      IF(IQ(LRAW-1)-16.LE.(L-LRAW)) GO TO 999
      NH=IQ(L+1)
      IF(NH.LE.0.OR.NH.GT.1000) GO TO 999
C     -- crate header block...
      NH=NH+1
      NCRATE=NCRATE+1
      NVERS=IAND(IQ(L+4),B16)           ! Software version number
      IF(NVERS.EQ.1) THEN       ! MONTE CARLO
        NH=IQ(L+1)+1
        NM=IQ(L+1)-6
        IF(CFL.EQ.'ALL')
     A    WRITE(PRUNIT,1210) IQ(L+1),IQ(L+2),IQ(L+3),IQ(L+4),IQ(L+5)
 1210   FORMAT(5X,I12,'  crate header words -1'
     +      /5X,I12,'  event number / FFFF  '
     +      /5X,I12,'  crate ID             '
     +      /5X,I12,'  data type  (1=MC,    '
     +      /5X,I12,'  delay / gain for pulsing data')
        NMUONH=0
C
        IF(NM.GT.0) THEN
          DO 1220 I=1,NM
            MODNUM(I)=IQ(L+I+5)/2**16
            MODWRD(I)=IAND(IQ(L+I+5),B12) 
            NMUONH=NMUONH+MODWRD(I)
 1220     CONTINUE
          IF(CFL.EQ.'ALL')
     A      WRITE(PRUNIT,1221) (MODNUM(I),MODWRD(I),I=1,NM)
 1221     FORMAT(10X/5X,'module numbers / module word count:'
     +      /(5X,6(I8,I4)))
        ELSE
          IF(CFL.EQ.'ALL')
     A      WRITE(PRUNIT,1222)
 1222     FORMAT(10X/10X,'=== no module with hit in this crate ===')
        ENDIF
C
        IF(CFL.EQ.'ALL') THEN
          WRITE(PRUNIT,1223) NCRATE,IQ(L+NH-1)
 1223     FORMAT(/5X,' crate ',I5,'  number of data words',I10)
          WRITE(PRUNIT,1224) IQ(L+NH)
 1224     FORMAT(5X,I12,'  trigger type')
        ENDIF
C
C  Hit Block.
C  ===========
C
        NMUONH=NMUONH/9
C
        IF(NMUONH.GT.0) THEN
          IF(NCRATE.EQ.1.OR.CFL.EQ.'ALL')
     A      WRITE(PRUNIT,1101)
 1101     FORMAT('0',10X,' MUD1 HITS IN MUON GEANT MODULES  '//
     A      ' PL ADDRESS  PAD 0A   PAD 0B    TIME1    TIME2 ',
     A      ' PAD 1A   PAD 1B    DELT1    DELT2 ')
          DO 1010 I=1,NMUONH
            K=9*(I-1)+L+NH
            IADD=IQ(K+1)
            CALL MULTCH(IADD,IO,IE)
            PL=IE+2*IO
            CALL MUADD(IADD,IMOD,IPLN,ICELL,IERR)
            WRITE(PRUNIT,1102) PL,IMOD,IPLN,ICELL,(IQ(K+J),J=2,9)
 1102       FORMAT(1X,I1,1X,I3,I2,I3,1X,2I9,2I8,2I9,2I8)
 1010     CONTINUE
        ENDIF
        L=L+NH+NMUONH*9
      ELSE                       ! EVERYTHING ELSE
        ND=IQ(L+NH)
        ICONTW=ISHFT(IQ(L+3),-16)
        NMODC=IAND(ICONTW,B8)
        IF(NMODC.EQ.0) NMODC=NH-7
        CALL GETBYT(IQ(L+3),1,8,CR)
        IF (CFL.EQ.'ALL') WRITE(PRUNIT,277) CR
  277   FORMAT(//,20X,'  CRATE NUMBER #',I4,/,21X,20('=')/)
        IF(CFL.EQ.'ALL')
     A    WRITE(PRUNIT,210) IQ(L+1),IQ(L+2),IQ(L+3),IQ(L+4),IQ(L+5),
     A    IQ(L+6)
  200   FORMAT(//' ',5X,54('*'),/,6X,'*',52X,'*',/,5X,
     +    ' *  ---- CONTENTS IN MUD1 BANK (muon raw data ) ----  *',/
     +    6X,'*',52X,'*',/,6X,54('*'),/)
  210   FORMAT(5X,I12,'  crate header words -1'
     +      /5X,I12,'  event number / FFFF  '
     +      /5X,I12,'  crate ID             '
     +      /5X,I12,'  version number       '
     +      /5X,I12,'  data type  (1=MC,    '
     +      /5X,I12,'  delay / gain for pulsing data')
        NMUONH=0
C
        IF(NMODC.GT.0) THEN
          DO 220 I=1,NMODC
            IMODH=IQ(L+6+I)
            MODNUM(I)=ISHFT(IMODH,-16)
            MODWRD(I)=IAND(IMODH,B12)
            NMUONH=NMUONH+MODWRD(I)
  220     CONTINUE
          IF(CFL.EQ.'ALL')
     A      WRITE(PRUNIT,221) (MODNUM(I),MODWRD(I),I=1,NMODC)
  221     FORMAT(10X/5X,'module numbers / module word count:'
     +      /(5X,6(I8,I4)))
        ELSE
          IF(CFL.EQ.'ALL')
     A      WRITE(PRUNIT,222)
  222     FORMAT(10X/10X,'=== no module with hit in this crate ===')
        ENDIF
C
        IF(CFL.EQ.'ALL')
     A    WRITE(PRUNIT,223) NCRATE,IQ(L+NH)
  223   FORMAT(/5X,' crate ',I5,'  number of data words',I10)
C
C  Hit Block.
C  ===========
C
        IF (CR.LT.182) THEN
          NMUONH=NMUONH/9
C
          IF(NMUONH.GT.0) THEN
            IF(NCRATE.EQ.1.OR.CFL.EQ.'ALL')
     C        WRITE(PRUNIT,101)
  101       FORMAT('0',10X,' BANK MUD1: MUON RAW DATA '//
     A        ' PL ADDRESS   TIME1    TIME2  PAD 0A   PAD 0B  ',
     A        '  DELT2    DELT1  PAD 1A   PAD 1B   ')
            DO 10 I=1,NMUONH
              K=9*(I-1)+L+NH
              IADD=IQ(K+1)
              CALL MULTCH(IADD,IO,IE)
              PL=IE+2*IO
              CALL MUADD(IADD,IMOD,IPLN,ICELL,IERR)
              WRITE(PRUNIT,102) PL,IMOD,IPLN,ICELL,(IAND(B16,IQ(K+J)),
     &          J=2,9)
  102         FORMAT(1X,I1,1X,I3,I2,I3,1X,2I9,2I8,2I9,2I8)
   10       CONTINUE
          ENDIF
          L=L+NH+ND+1
        ELSE
          NMUONH=NMUONH/3
          IF (NCRATE.EQ.1.OR.CFL.EQ.'ALL') WRITE(PRUNIT,103)
  103     FORMAT('0',20X,' BANK MUD1: SAMUS RAW DATA '//,10X,
     A      ' ADDRESS    TIME1    TIME2')
          DO 11 I=1,NMUONH
            K=3*(I-1)+L+NH
            IADD=IQ(K+1)
            CALL SAADD(IADD,IMOD,ICELL,IERR)
            WRITE(PRUNIT,104) IMOD,ICELL,IQ(K+2),IQ(K+3)
  104       FORMAT(11X,I3,I4,2I9)
   11     CONTINUE
          L=L+NH+NMUONH*3+1
        END IF
        IF (CFL.EQ.'ALL') WRITE(PRUNIT,2101) IQ(L+1),IQ(L+2),IQ(L+3),
     &    IQ(L+4)
 2101   FORMAT(//20X,' CRATE TRAILER ',/,20X,15('-')
     +      /5X,I12,'  Total word count     '
     +      /5X,Z12,'  Trigger # | crate ID '
     +      /5X,Z12,'  Token Pass Status    '
     +      /5X,I12,'  Checksum             '
     +      /60('.'))
        L=L+4
      ENDIF
      GO TO 1000
  999 CONTINUE
      RETURN
      END
