      SUBROUTINE PRCPDH(PRUNIT,LCPDHI,NCPDH,CFL,IFL)
C---------------------------------------------------------
C-
C-  Print out for CPDH (Calorimeter pedestals HEADER bank)
C-
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LCPDH = bank address 
C-  NCPDH = not used
C-  CFL   = not used
C-  IFL   = not used
C-
C---------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT,NCPDH,LCPDHI,IFL,I
      CHARACTER CFL*(*)
C                                                          
      LCPDH=LCPDHI
      IF(LCPDH.LE.0) THEN
        WRITE(PRUNIT,100) LCPDH
        RETURN
      ELSE
        WRITE(PRUNIT,101)
        WRITE(PRUNIT,102) (IC(LCPDH+I),I=1,10)
      ENDIF
C
 100  FORMAT(/' Wrong Address for a CPDH bank: LCPDH=',I8/)
 101  FORMAT(/
     $' =========================================================='/
     $'       CPDH: Header bank for Calorimeter Pedestals '/
     $' =========================================================='/)
 102  FORMAT(
     $' Type              = ',I8/
     $' Status            = ',I8/
     $' Controller word   = ',Z8/
     $' MIN_RUN_NUM       = ',I8/
     $' MAX_RUN_NUM       = ',I8/
     $' ACT_RUN_NUM       = ',I8/
     $' DATE              = ',I8/
     $' TIME              = ',I8/
     $' CRATE (8 bits-oct)= ',O8/
     $' N_ADC_CARDS       = ',I8)
C
      RETURN      
      END
