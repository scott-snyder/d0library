      SUBROUTINE L2_COMP_VT(LUN,L2VT,IB,OKOK,CFAST,CSLOW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-        compare output banks from L0VT
C
C     grabs info on the L2EM bank - electron tool results L2EM
C
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Modified  2-Jun-1994   Andrzej Zieminski
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:HEADER.INC'
c
      INTEGER L2VT(*),IB,LUN
      LOGICAL OKOK
      LOGICAL LDUMP,LBANK,LGOOD,LMULT,LZFST,LZSLW
      LOGICAL L2_NXT_INT_DIFF, L2_NXT_REAL_DIFF
C
      INTEGER IP1,IP2,I,J,NRUN,NEV
      INTEGER BANK1,BANK2,GOOD1,GOOD2,MULT1,MULT2
      REAL ZFST1,ZFST2,ZSLW1,ZSLW2,CFAST,CSLOW
C
      CHARACTER*1 CBANK,CGOOD,CMULT,CZFST,CZSLW
C
      DATA CBANK/' '/,CGOOD/' '/,CMULT/' '/,CZFST/' '/,CZSLW/' '/
C----------------------------------------------------------------------
C
      OKOK = .TRUE.
      NRUN = IQ(LHEAD+6)
      NEV = IQ(LHEAD+9)
C
      IF (IB.NE.2) THEN
        CALL ERRMSG('L2_COMPARE','L2_COMP_VT',
     &    'L2_COMP_VT called for IB.NE.2 ! Not allowed!!!','F')
        GOTO 999
      ENDIF
C
C     make sure there is L2VT - no mistakes
C
      IF (L2VT(1).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_VT: Run/Event '',2I7,
     &      '' has NO L2VT bank for SIMULATION'')') NRUN,NEV
        GOTO 999
      ENDIF
      IF (L2VT(2).LE.0) THEN
        WRITE(LUN,'('' L2_COMP_VT: Run/Event '',2I7,
     &      '' has NO L2VT bank for DATA'')') NRUN,NEV
        GOTO 999
      ENDIF
C
C     ok, compare the two
C
C
      IP1 = L2VT(1)
      IP2 = L2VT(2)
      LBANK = L2_NXT_INT_DIFF(IP1,IP2,BANK1,BANK2,CBANK) 
      LZFST = L2_NXT_REAL_DIFF(IP1,IP2,ZFST1,ZFST2,CFAST,CZFST) 
      LGOOD = L2_NXT_INT_DIFF(IP1,IP2,GOOD1,GOOD2,CGOOD)
      LZSLW = L2_NXT_REAL_DIFF(IP1,IP2,ZSLW1,ZSLW2,CSLOW,CZSLW)
      LMULT = L2_NXT_INT_DIFF(IP1,IP2,MULT1,MULT2,CMULT) 
C
      LDUMP = LZFST.OR.LZSLW
      IF (LDUMP) THEN
        OKOK = .FALSE.
        WRITE(LUN,'(/,'' L2_COMP_VT:  RUN/EVENT '',2I7,
     &        '' L0VT entry discrepancy'',/,
     &        '' "*" denote variables whose differences is '',
     &        ''outside of tolerances'')') NRUN,NEV
          WRITE(LUN,'(
     &      '' Bank Vers.  Evt Qual.    Mult_INTCN '',
     &      ''   Zfast      Zslow '',/,
     &      '' --------------------------------------- '',
     &      '' --------------------------------------'')')
          WRITE(LUN,'('' SIM'',3(I10,A1),2(F10.2,A1))')
     &      BANK1,CBANK,GOOD1,CGOOD,MULT1,CMULT,ZFST1,CZFST,ZSLW1,CZSLW
          WRITE(LUN,'('' DAT'',3(I10,A1),2(F10.2,A1))')
     &      BANK2,CBANK,GOOD2,CGOOD,MULT2,CMULT,ZFST2,CZFST,ZSLW2,CZSLW
        ENDIF
C
      RETURN
C
  999 CONTINUE
C
C     things are amiss
C
      OKOK = .FALSE.
      RETURN
C
      END
