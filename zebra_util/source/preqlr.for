      SUBROUTINE PREQLR(PRUNIT,LEQLR,NEQLR,CFL,IFL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    PRINT OUT EQLR - CALIBRATION END TASK QUALITY RESULTS 
CC
CC    PRUNIT - UNIT NUMBER FOR PRINTOUT
CC    LEQLR - BANK ADDRESS
CC    NEQLR - BANK NUMBER
CC    CFL - FLAG TO CONTROL PRINT OUT
CC    IFL - HOW MUCH TO PRINT
CC
CC    THIS BANK CAN BE PLACED ANYWHERE; ONE MUST GIVE LEQLR
CC    NEQLR ONLY USED IN PRINT STATEMENT
CC    DH 2-87                                          
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT,LEQLR,NEQLR,IFL,I
      CHARACTER CFL*(*)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(CFL.EQ.'ALL') THEN               
        IF(LEQLR.EQ.0) THEN
          WRITE(PRUNIT,100) NEQLR
 100      FORMAT('0   NO EQLR POINTER TO PREQLR FOR MODULE ',I8//)
        ELSE
          WRITE(PRUNIT,101) (IC(LEQLR+I),I=1,12),(C(LEQLR+I),I=13,20)
 101  FORMAT('0 BANK EQLR -- CALIBRATION QUALITY RESULTS  '//
     A ' Type ',I8,'      Status ',I5,'      Spares ',2I5/
     A ' Generated for run type   ',I6/ 
     A ' Run generated            ',I6/
     A ' Date and Time generated   ',2I12//
     A ' Quality flag 1 (bits 1-->8 values 13-20) = ',O6/
     A ' Quality flag 2 (currently unused)        = ',O6/
     A ' Module number                            = ',I6/
     A ' Number of channels                       = ',I6/
     A '                   RESULTS   '//
     A ' Average value                       ',F8.2/   
     A ' Average width                       ',F8.2/
     A ' Dispersion of values                ',F8.2/    
     A ' Dispersion of widths                ',F8.2/
     A ' Fraction of dead channels           ',F8.2/
     A ' Fraction channels > high value      ',F8.2/
     A ' Fraction channels < low value       ',F8.2/
     A ' Fraction widths   > high width      ',F8.2//)    
        ENDIF
      ENDIF
      RETURN
      END
