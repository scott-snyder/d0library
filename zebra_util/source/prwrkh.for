
      SUBROUTINE PRWRKH(PRUNIT,LWRKHI,NWRKH,CFL,IFL)
C-------------------------------------------------------------
C-
C-  Print out for WRKH (Calibration working space HEADER bank)
C-
C-
C-  INPUT:
C-  PRUNIT= unit number for printout
C-  LWRKH  = bank address 
C-  NWRKH  = not used
C-  CFL   = not used
C-  IFL   = not used
C-
C--------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBWRK.INC'
      INTEGER PRUNIT,NWRKH,LWRKHI,IFL,I
      CHARACTER CFL*(*)
C                                                          
      LWRKH=LWRKHI
      IF(LWRKH.LE.0) THEN
        WRITE(PRUNIT,100) LWRKH,NWRKH
        RETURN
      ELSE
        WRITE(PRUNIT,101)
        WRITE(PRUNIT,102) (IW(LWRKH+I),I=1,5)
      ENDIF
C
 100  FORMAT(/,' Wrong Adress for a WRKH bank: LWRKH=',I8
     $,' NWRKH =',I8/)
 101  FORMAT(/,' Header bank for Calibration working space: WRKH'/)
 102  FORMAT(
     $' TASKNO      = ',I8,'   ! Task=1 for Pedestals '/
     $' ACT_RUN_NUM = ',I8/
     $' DATE        = ',I8/
     $' TIME        = ',I8/
     $' NSTEPS      = ',I8///)
C
      RETURN      
      END
