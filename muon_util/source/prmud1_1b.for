C&IF VAXVMS
      OPTIONS /CHECK=NOOVERFLOW
C&ENDIF
      SUBROUTINE PRMUD1_1B(PRUNIT,LRAW,NMUD1,CFL,IFL)
C====================================================================
C
C-   Purpose and Methods : Formatted dump of MUD1 bank (run 1b format)
C
C-   Inputs  :  PRUNIT - Print unit for printout
CC    LRAW - BANK ADDRESS (or 0 to call GZMUD1)
CC    NMUD1 - BANK NUMBER  (irrelevent)
CC    CFL - FLAG TO CONTROL PRINT OUT 'ALL' gets header (irrelevant)
CC    IFL - HOW MUCH TO PRINT (not used)
C
C  Revision History:
C  =================
C    July-1994,  D. Hedin and D. Wood
C=======================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  Argument Declarations:
C  ======================
C
      INTEGER PRUNIT,LRAW,NMUD1,IFL
      CHARACTER*(*) CFL
C
C  Local Declarations:
C  ===================
C
      INTEGER GZMUD1,IMUD1,LMUD1
      INTEGER IMADC,IMU16,I,K,ICRATE,NMOD,IDMOD,NWORD_ADC
      INTEGER NHIT_ADC,ICELL,IMASK,ND,ICOUNT,J16
      INTEGER IWIR,IPLN,IAB
      INTEGER MODMSK(15,2)
      LOGICAL FIRST
      DATA IMADC,IMU16/0,0/,FIRST/.TRUE./
C
C  Executable Code:
C  ================
C
      IF (FIRST) THEN
        IMADC = IBSET(IMADC,18)
        IMADC = IBSET(IMADC,19)
        IMADC = IBSET(IMADC,20)
        IMADC = IBSET(IMADC,31)
        J16 = 2**16-1
        IMU16 = ISHFT(J16,16)
        FIRST = .FALSE.
      ENDIF
      LMUD1 = LRAW
      IF(LMUD1.LE.0) THEN
        LMUD1 = GZMUD1(0)
        IF (LMUD1 .EQ. 0) THEN                   ! No bank present
          GOTO 999
        ENDIF
      ENDIF
      ND = IQ(LMUD1-1)
      WRITE(PRUNIT,3) ND
    3 FORMAT(/1X,'FORMATTED MUD1 DUMP --- TOTAL WORDS:',I12)
      IF (ND .LE. 16) THEN            ! No data present
        RETURN
      ENDIF
      IMUD1 = LMUD1
C
C                Get crate header information
C
 1000 CONTINUE
      CALL VZERO(MODMSK,15*2)
      ICRATE = IBITS(IQ(IMUD1+3),24,8)
      NMOD = (IQ(IMUD1+1)-7)/8
      WRITE(PRUNIT,5) ICRATE,(IQ(IMUD1+I),I=1,8)
    5 FORMAT(1X,' CRATE ',I4,' HEADER FIRST 8 WORDS:'/1X,8(Z8,2X))

C
C                Get module header information - run 1B
C
      WRITE(PRUNIT,15)
   15 FORMAT(1X,'MOD MODWORD  ADC      WORDS    FLAGS    ',
     &      'PL00-31  PL32-63  PL64-95  PL96-127')
      IMUD1 = IMUD1 + 8
      DO I=1,NMOD
        IDMOD = IBITS(IQ(IMUD1+1),16,16)             ! Module ID
        WRITE(PRUNIT,25) IDMOD,(IQ(IMUD1+K),K=1,8)
        MODMSK(I,1) = IDMOD
        MODMSK(I,2) = IQ(IMUD1+2)
   25   FORMAT(1X,I3,1X,8(Z8,1X))
        IMUD1 = IMUD1 + 8
      ENDDO
C loop over ADC blocks
      DO IAB=1,2
        NWORD_ADC = IQ(IMUD1+1)+1
        NHIT_ADC = (NWORD_ADC-1)/4
        IMUD1 = IMUD1+1
        IF(IAB.EQ.1) THEN
          WRITE(PRUNIT,33) 'A',NWORD_ADC,NHIT_ADC
        ELSE
          WRITE(PRUNIT,33) 'B',NWORD_ADC,NHIT_ADC
        ENDIF
   33   FORMAT(1X,'ADC BLOCK ',A1,1X,I10,', WORDS   ',I10,' HITS'/
     &        '   MASK   MOD pln wir  T1/D2/  T2/D1/SE  BE/BO/',
     &        '  AE/AO/SO     upper 2 bytes')
        DO I=1,NHIT_ADC
          ICELL = IBITS(IQ(IMUD1+1),21,7)       ! Cell number
          iwir=icell/4
          ipln=mod(icell,4)
          IMASK = IAND(IQ(IMUD1+1),IMADC)        ! ADC address mask
          IDMOD = 0
          DO K=1,NMOD
            IF(IMASK.EQ.MODMSK(K,2)) IDMOD = MODMSK(K,1)
          ENDDO
          WRITE(PRUNIT,35) IMASK,IDMOD,IPLN,IWIR,
     &      (IAND(J16,IQ(IMUD1+K)),K=1,4),
     &      (IAND(J16,ISHFT(IQ(IMUD1+K),-16)),K=1,4)
   35     FORMAT(1X,Z8,1X,I3,1X,2I3,1X,4(I8,1X),4(Z4,1X))
          IMUD1 = IMUD1+4
        ENDDO
      ENDDO
C
C                Get crate trailer information
C
      ICRATE = IBITS(IQ(IMUD1+2),0,8)        ! Crate ID
      WRITE(PRUNIT,45) ICRATE,(IQ(IMUD1+K),K=1,4)
   45 FORMAT(1X,' CRATE ',I4,' TRAILER WORDS:'/
     &  1X,4(Z8,2X))
      IMUD1 = IMUD1+4
      ICOUNT = IMUD1-LMUD1
      IF(ICOUNT+16.LT.ND) GOTO 1000   ! next crate
C print any remainig words (MUD1 trailer)
      WRITE(PRUNIT,55) (IQ(IMUD1+K),K=ICOUNT+1,ND)
   55 FORMAT(1X,' MUD1 TRAILER WORDS:'/
     &  1X,4(Z8,2X))
C
  999 CONTINUE
      RETURN
      END
