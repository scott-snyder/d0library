      SUBROUTINE PRCGNH(PRUNIT,LCGNHI,NCGNH,CFL,IFL)
C---------------------------------------------------------
C-
C-  Print out for CGNH (Calorimeter gains HEADER bank)
C-
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LCGNH = bank address 
C-  NCGNH = not used
C-  CFL   = not used
C-  IFL   = not used
C-
C---------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT,NCGNH,LCGNHI,IFL,I
      CHARACTER CFL*(*)
C                                                          
      LCGNH=LCGNHI
      IF(LCGNH.LE.0) THEN
        WRITE(PRUNIT,100) LCGNH
        RETURN
      ELSE
        WRITE(PRUNIT,101)
        WRITE(PRUNIT,102) (IC(LCGNH+I),I=1,10)
      ENDIF
C
 100  FORMAT(/' Wrong Address for a CGNH bank: LCGNH=',I8/)
 101  FORMAT(/
     $' =========================================================='/
     $'       CGNH: Header bank for Calorimeter gains '/
     $' =========================================================='/)
 102  FORMAT(
     $' Type             = ',I8/
     $' Status           = ',I8/
     $' Controller word  = ',Z8/
     $' MIN_RUN_NUM      = ',I8/
     $' MAX_RUN_NUM      = ',I8/
     $' ACT_RUN_NUM      = ',I8/
     $' DATE             = ',I8/
     $' TIME             = ',I8/
     $' Crate(8 bits-oct)= ',O8/
     $' N_ADC_CARDS      = ',I8)
C
      RETURN      
      END
