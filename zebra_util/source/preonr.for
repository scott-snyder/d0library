      SUBROUTINE PREONR(PRUNIT,LEONR,NEONR,CFL,IFL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    PRINT OUT EONR - CALIBRATION END TASK OLD/NEW RESULTS 
CC
CC    PRUNIT - UNIT NUMBER FOR PRINTOUT
CC    LEONR - BANK ADDRESS
CC    NEONR - BANK NUMBER
CC    CFL - FLAG TO CONTROL PRINT OUT
CC    IFL - HOW MUCH TO PRINT
CC
CC    THIS BANK CAN BE PLACED ANYWHERE; ONE MUST GIVE LEONR  
CC    NEONR ONLY USED IN PRINT STATEMENT
CC    DH 1-97                                          
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT,LEONR,NEONR,IFL,LSCPH,I       
      CHARACTER CFL*(*)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(CFL.EQ.'ALL') THEN               
        IF(LEONR.EQ.0) THEN
          WRITE(PRUNIT,100) NEONR
 100      FORMAT('0   NO EONR POINTER GIVEN TO PREONR FOR MODULE ',I8//)
        ELSE
          WRITE(PRUNIT,101) (IC(LEONR+I),I=1,12),(C(LEONR+I),I=13,25)
 101  FORMAT('0         BANK EONR -- CALIBRATION OLD/NEW RESULTS  '//
     A ' Type ',I8,'      Status ',I5,'      Spare ',I5/
     A ' Old run used as reference set                  = ', I6/     
     A ' Generated for run type                           ',I6/
     A ' Run generated                                  = ', I6/     
     A ' Date and Time generated   ',2I12//
     A ' Quality Flag 1 (bits 1-9 --> values 13-21      = ', O6/     
     A ' Quality flag 2 (currently unused)              = ', O6/     
     A ' Module number                                  = ', I6/     
     A ' Number of channels                             = ', I6/     
     A '               RESULTS       '//
     A ' Change in average value of constant ',F8.2/
     A ' Change in average width             ',F8.2/   
     A ' Change in dispersion of values      ',F8.2/
     A ' Change in dispersion of widths      ',F8.2/    
     A ' Change in fraction dead channels    ',F8.2/
     A ' Average deviation old to new        ',F8.2/
     A ' Fraction new > (old+epsilon)        ',F8.2/
     A ' Fraction new < (old-epsilon)        ',F8.2/
     A ' Fraction |old-new| > Old/new value  ',F8.2/    
     A ' Spares ',4F6.0//)
        ENDIF
      ENDIF
      RETURN
      END
