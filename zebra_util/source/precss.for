      SUBROUTINE PRECSS(PRUNIT,LECSS,NECSS,CFL,IFL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    PRINT OUT ECSS - CALIBRATION END TASK SAVE SUMMARY    
CC
CC    PRUNIT - UNIT NUMBER FOR PRINTOUT
CC    LECSS - BANK ADDRESS
CC    NECSS - BANK NUMBER
CC    CFL - FLAG TO CONTROL PRINT OUT
CC    IFL - HOW MUCH TO PRINT
CC
CC    THIS BANK CAN BE PLACED ANYWHERE; ONE MUST GIVE LECSS  
CC    NECSS ONLY USED IN PRINT STATEMENT
CC    DH 2-87                                          
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT,LECSS,NECSS,IFL,LSCPH,I       
      CHARACTER CFL*(*)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(CFL.EQ.'ALL') THEN               
        IF(LECSS.EQ.0) THEN
          WRITE(PRUNIT,100) NECSS
 100      FORMAT('0   NO ECSS POINTER GIVEN TO PRECSS FOR MODULE ',I8//)
        ELSE
          WRITE(PRUNIT,101) (IC(LECSS+I),I=1,13)                     
 101  FORMAT('0         BANK ECSS -- CALIBRATION SAVE SUMMARY     '//
     A ' Type ',I8,'      Status ',I5,'      Spares ',2I5/
     A ' Generated for run type                           ',I6/
     A ' Run generated                                  = ', I6/     
     A ' Date and Time generated   ',2I12//
     A ' Quality Mask   (bits used)                     = ', O6/     
     A ' Old/New Mask   (bits used)                     = ', O6/     
     A ' Module number                                  = ', I6/     
     A ' Number of quality failures                     = ',I6/
     A ' Number of old/new failures                     = ',I6//)
        ENDIF
      ENDIF
      RETURN
      END
