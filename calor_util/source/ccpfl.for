      SUBROUTINE CCPFL(PREFIX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank CCPT, CCPC, OR CCUA
C-                         From D0$STP$GEOCAL:CPT_XXXX.DAT,
C-                         D0$STP$GEOCAL:CPC_XXXX.DAT or 
C-                         D0$STP$GEOCAL:UAR_XXXX.DAT   ASCII Files.
C-                         Book different banks for cell & cell+cable
C-                         capacitance.

C-
C-   Inputs  :PREFIX [C]     Prefix for file name
C-                           'CPT_' Total Capacitance
C-                           'CPC_' Cell Capacitance
C-                           'UAR_' Uranium Area
C-
C-   Outputs : IER  [I]     0 = OK
C-   Controls: None
C-
C-   Created   31-JUL-1991 Jan Guida, Chip Stewart
C-   Updated   17-Feb-1992 Herbert Greenlee
C-      UNIX version.  Replace OPEN with call to D0OPEN.
C-      Use machine independent word order in ICAP/JCAP equivalence.
C-   Updated  13-NOV-1993   Jan Guida  Remove IOS variable (FLINT) 
C-   Updated  18-MAR-2004   sss - use idate2k instead of idate
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      CHARACTER*(*) PREFIX
      INTEGER LCCPH, GZCCPH,LCCP, IER, LUN, NHEAD
      INTEGER IADC,IBLS,ITWR,IDEP,KCHAN
      INTEGER ICRT,ICRATE(12)
      REAL RCAP(4608)
      INTEGER*2 ICAP(4608)
      INTEGER   JCAP(2304)
      EQUIVALENCE ( ICAP(1),JCAP(1)  )
      CHARACTER FILENAME*50
      CHARACTER*80 MSG_STRING
      INTEGER I,J,K,JDATE,JTIME
      CHARACTER*8 STIME
      LOGICAL OK
      DATA ICRATE/7,17,27,37,47,57,8,18,28,38,48,58/
C----------------------------------------------------------------------
C
C ****  BOOK CCPH HEADER
C
      IER = 0
      LCCPH = GZCCPH()    ! GET LINK.
      IF(LCCPH.EQ.0)CALL BKCCPH(0,LCCPH)
C
C ****   read cap file
C
      DO ICRT = 1,12
        WRITE(FILENAME,4)PREFIX(1:4),ICRATE(ICRT)
    4   FORMAT('D0$STP$GEOCAL:',A4,I3.3,'.DAT')
        CALL GTUNIT(12,LUN,IER)
        CALL D0OPEN(LUN,FILENAME,'I',OK)
        IF (.NOT.OK) THEN
          WRITE(MSG_STRING,'(A50)')FILENAME
          CALL ERRMSG('FILE NOT FOUND','CCPFL',MSG_STRING,'W')
          IER = -1
          GOTO 999
        ENDIF
        DO IADC = 0,11
          DO IBLS = 0,7
            DO ITWR = 0,3
              KCHAN = IADC*384 + IBLS*48 + ITWR*12
              READ(LUN,100)(RCAP(KCHAN+IDEP+1),IDEP=0,11)
  100         FORMAT(12F10.3)
              DO IDEP = 0, 11, 2
                ICAP(KCHAN+IDEP+WORD1) = NINT( RCAP(KCHAN+IDEP+1) )
                ICAP(KCHAN+IDEP+WORD2) = NINT( RCAP(KCHAN+IDEP+2) )
              END DO
            ENDDO                         ! ITWR
          ENDDO                           ! IBLS
        ENDDO                             ! IADC
        CLOSE (UNIT=LUN)
        CALL RLUNIT(12,LUN,IER)
C
C ****  COPY TO ZEBRA
C
        IF( PREFIX(3:3).EQ.'T') THEN
          CALL BKCCPT(LCCPH,LCCP)
        ELSE IF( PREFIX(3:3).EQ.'C') THEN
          CALL BKCCPC(LCCPH,LCCP)
        ELSE
          CALL BKCCUA(LCCPH,LCCP)
        END IF
        CALL IDATE2k(I,J,K)
        JDATE=I*10000+J*100+K
        CALL TIME(STIME)
        READ(STIME,'(I2,1X,I2,1X,I2)')I,J,K
        JTIME=I*10000+J*100+K
        IC(LCCP+2) = 0                    ! Lowest run number
        IC(LCCP+3) = 9999999              ! Highest run runber
        IC(LCCP+4) = JDATE                ! Date generated
        IC(LCCP+5) = JTIME                ! Time generated
        IC(LCCP+6) = ICRATE(ICRT)         ! ADC Crate
        NHEAD = 7
        CALL UCOPY(JCAP(1),IC(LCCP+NHEAD+1),2304)
      ENDDO
C
C----------------------------------------------------------------------
  999 RETURN
      END
