      SUBROUTINE PREQLC(PRUNIT,LEQLC,NEQLC,CFL,IFL)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    PRINT OUT EQLC - CALIBRATION END TASK QUALITY CRITERIA
CC
CC    PRUNIT - UNIT NUMBER FOR PRINTOUT
CC    LEQLC - BANK ADDRESS
CC    NEQLC - BANK NUMBER
CC    CFL - FLAG TO CONTROL PRINT OUT
CC    IFL - HOW MUCH TO PRINT
CC
CC               SHOULD ONLY BE ONE EQLC BANK
CC    DH 1-87                                          
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSCPH.LINK'
      INCLUDE 'D0$LINKS:IZEQLC.LINK'
      INTEGER PRUNIT,LEQLC,NEQLC,IFL,LSCPH,I,KEQLC
      CHARACTER CFL*(*)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IF(CFL.EQ.'ALL') THEN               
        KEQLC=0
        LSCPH=LC(LSTPH-IZSCPH)
        IF(LSCPH.NE.0) THEN
          KEQLC=LC(LSCPH-IZEQLC)
        ENDIF
        IF(KEQLC.EQ.0) THEN
          WRITE(PRUNIT,100)
 100      FORMAT('0   NO EQLC BANK  '/)
        ELSE
          WRITE(PRUNIT,101) (IC(KEQLC+I),I=1,8),(C(KEQLC+I),I=9,21)
 101  FORMAT('0 BANK EQLC -- CALIBRATION QUALITY CRITERIA '//
     A ' Type ',I8,'      Status ',I5,'      Spare ',I5/
     A ' Minimum and Maximum Runs this set is valid for = ',2I6/     
     A ' Generated for run type   ',I6/
     A ' Date and Time generated   ',2I12//
     A ' Constant quality high value         = ',F10.2/
     A ' Constant quality low value          = ',F10.2/
     A ' Constant quality high width value   = ',F10.2/
     A '                             Spare     ', F5.1//
     A '                   LIMITS    '//
     A ' Average value low limit             ',F8.2/
     A ' Average value high limit            ',F8.2/   
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
