      SUBROUTINE PREONC(PRUNIT,LEONC,NEONC,CFL,IFL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    PRINT OUT EONC - CALIBRATION END TASK OLD/NEW CRITERIA
CC
CC    PRUNIT - UNIT NUMBER FOR PRINTOUT
CC    LEONC - BANK ADDRESS
CC    NEONC - BANK NUMBER
CC    CFL - FLAG TO CONTROL PRINT OUT
CC    IFL - HOW MUCH TO PRINT
CC
CC               SHOULD ONLY BE ONE EONC BANK
CC    DH 1-87                                          
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSCPH.LINK'
      INCLUDE 'D0$LINKS:IZEONC.LINK'
      INTEGER PRUNIT,LEONC,NEONC,IFL,LSCPH,I,KEONC
      CHARACTER CFL*(*)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(CFL.EQ.'ALL') THEN               
        KEONC=0
        LSCPH=LC(LSTPH-IZSCPH)
        IF(LSCPH.NE.0) THEN
          KEONC=LC(LSCPH-IZEONC)
        ENDIF
        IF(KEONC.EQ.0) THEN
          WRITE(PRUNIT,100)
 100      FORMAT('0   NO EONC BANK  '/)
        ELSE
          WRITE(PRUNIT,101) (IC(KEONC+I),I=1,8),(C(KEONC+I),I=9,21)
 101  FORMAT('0 BANK EONC -- CALIBRATION OLD/NEW CRITERIA '//
     A ' Type ',I8,'      Status ',I5,'      Spare ',I5/
     A ' Minimum and Maximum Runs this set is valid for = ',2I6/     
     A ' Generated for run type   ',I6/
     A ' Date and Time generated   ',2I12//
     A ' Old/new constant comparison epsilon = ',F10.2/
     A ' Old/new constant comparison value   = ',F10.2/
     A '                             Spares    ',2F5.1//
     A '                   LIMITS    '//
     A ' Change in average value of constant ',F8.2/
     A ' Change in average width             ',F8.2/   
     A ' Change in dispersion of values      ',F8.2/
     A ' Change in dispersion of widths      ',F8.2/    
     A ' Change in fraction dead channels    ',F8.2/
     A ' Average deviation old to new        ',F8.2/
     A ' Fraction new > (old+epsilon)        ',F8.2/
     A ' Fraction new < (old-epsilon)        ',F8.2/
     A ' Fraction |old-new| > Old/new value  ',F8.2//)    
        ENDIF
      ENDIF
      RETURN
      END
