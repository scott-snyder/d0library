      SUBROUTINE BKCDH1(NHITS, LCDH1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book VTX Level 2 compressed hits bank CDH1.
C-
C-   Inputs  : NHITS, number of hits in total
C-   Outputs : LCDH1 pointer to CDH1 bank
C-
C-   Created  11-FEB-1994   Liang-ping Chen
C-   Updated  25-FEB-1994   Liang-Ping Chen  book FRES if necessary 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDH1.LINK'
      INCLUDE 'D0$LINKS:IZL2CH.LINK'
C
      INTEGER NHITS, LCDH1
C
      REAL LEAST_COUNT
      REAL WEIGHT(2), WEIGHT_BITVAL
      INTEGER LL2CH, GZL2CH, ISETVN
      INTEGER NSECT, NHEAD, NWDSHT, TABLE_VERSION
      INTEGER NL, NS, ND, NIO, IER
      INTEGER N_BEFORE, N_AFTER, BANK_VERSION
      INTEGER THR1(2), THR2(2), THR3(2)
      INTEGER IWEIGHT, VCHT_PACK_HITINFO
      INTEGER IDUMMY
      LOGICAL FIRST
      INTEGER LFRES, GZFRES, MPL2CH(5)
      CHARACTER*4 STRING
      DATA STRING / 'L2CH' /
      DATA FIRST / .TRUE. /
      DATA NL, NS / 0, 0 /
      DATA NSECT, NHEAD, NWDSHT / 80, 9, 2 / 
      DATA BANK_VERSION / 0 /
      DATA LEAST_COUNT / 0.25 /
      DATA WEIGHT_BITVAL/0.05/
      DATA MPL2CH / 0, 4, 4, 4, 1 /
c
      DATA THR1 /2,2/
      DATA THR2 /6,6/
      DATA THR3 /6,6/
      DATA WEIGHT /0.5,0.5/
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
c        CALL EZPICK('VTRAKS_RCP')
c        CALL EZGET('TABLE_VERSION',TABLE_VERSION,IER)
c        IF ( IER .NE. 0 ) TABLE_VERSION = 1
        TABLE_VERSION = 1
C        CALL EZGET('PULTH1',THR1,IER)
C        CALL EZGET('PULTH2',THR2,IER)
C        CALL EZGET('PULTH3',THR3,IER)
C        CALL EZGET('PULWEI',WEIGHT,IER)
C        CALL EZGET('BINS_BEFORE_PEAK',N_BEFORE,IER)
        N_BEFORE = 2
        N_AFTER = 3 - N_BEFORE
C        CALL EZRSET
        CALL MZFORM('CDH1','7I 1F 1I -B',NIO)
        CALL UCTOH(string, MPL2CH, 4, 4)
      ENDIF
C                                   

      LL2CH = GZL2CH()
      IF ( LL2CH .EQ. 0 ) THEN
        LFRES=GZFRES()
        IF (LFRES.EQ.0 ) THEN 
          CALL BKFRES(LFRES)
        ENDIF
        IF (LFRES.GT.0) THEN 
          CALL MZLIFT ( IXMAIN, LL2CH, LFRES, -IZL2CH, MPL2CH, 4 )
        ENDIF  
      ENDIF

      IF ( LL2CH .GT. 0 ) THEN
        ND = NHEAD + NSECT*2 + NWDSHT*NHITS
        CALL MZBOOK(IXMAIN,LCDH1,LL2CH,-IZCDH1,'CDH1',NL,NS,ND,NIO,0)
      ELSE
        CALL ERRMSG('L2CH does not exist','BKCDH1',            
     &    'Cannot book CDH1 - no supporting bank','W')
        LCDH1 = 0
        GO TO 999
      ENDIF
C
C ****  Fill header info (do not fill the NHITS word (word 3) and crate mask
C ****  word (word 9) - fill them when the hits are actually stored and 
C ****  all crates are unpacked.
C
      IQ(LCDH1) = ISETVN(IQ(LCDH1),BANK_VERSION)
      IQ(LCDH1+1) = BANK_VERSION
      IQ(LCDH1+2) = NHEAD
      IQ(LCDH1+4) = NWDSHT
C
C ****  Build hitfinding info word
C
      IWEIGHT = NINT(WEIGHT(1)/WEIGHT_BITVAL)
      IQ(LCDH1+5) =  VCHT_PACK_HITINFO(IDUMMY,IWEIGHT,
     &               THR1(1),THR2(1),THR3(1),TABLE_VERSION)
C
      IQ(LCDH1+6) = N_BEFORE
      IQ(LCDH1+7) = N_AFTER
      Q(LCDH1+8)  = LEAST_COUNT
C
  999 RETURN
      END
