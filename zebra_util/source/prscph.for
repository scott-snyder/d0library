      SUBROUTINE PRSCPH(PRUNIT,LSCPH,NSCPH,CFL,IFL)
C---------------------------------------------------------
C-
C-  Print out for SCPH (Calibration Run Parameters Header)
C-
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LSCPH = bank address 
C-  NSCPH = bank number; not used
C-  CFL   = not used
C-  IFL   = not used
C-
C---------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT,LSCPH,NSCPH,IFL,I
      CHARACTER CFL*(*)
C                                                          
      IF(LSCPH.LE.0) THEN
        WRITE(PRUNIT,100) LSCPH
        GO TO 99
      ELSE
        WRITE(PRUNIT,101)
        WRITE(PRUNIT,102) (IC(LSCPH+I),I=1,10)
      ENDIF
C
 100  FORMAT(/' Wrong Address for SCPH; LSCPH =',I8/)
 101  FORMAT(/' Header bank for Calib.Run.Param.:  SCPH'/)
 102  FORMAT(
     $' Spare       = ',I6,' ! Spare variable                       '/
     $' Spare       = ',I6,' ! Spare variable                       '/
     $' Spare       = ',I6,' ! Spare variable                       '/
     $' MIN_RUN_NUM = ',I8/
     $' MAX_RUN_NUM = ',I8/
     $' ACT_RUN_NUM = ',I8/
     $' DATE        = ',I8/
     $' TIME        = ',I8/
     $' Spare       = ',I6,' ! Spare variable                       '/
     $' Spare       = ',I6,' ! Spare variable                       ')
C
 99   RETURN      
      END
