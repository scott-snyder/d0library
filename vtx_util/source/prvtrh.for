      SUBROUTINE PRVTRH(PRUNIT,LOC,NUM,CFL,IFL)                 
C------------------------------------------------------------------
C 
C     Print out bank VTRH (header bank for VTX tracks)
C 
C     Daria Zieminska May 1988
C                            
C------------------------------------------------------------------
      IMPLICIT NONE 
      INTEGER PRUNIT,LOC,NUM,IFL
      CHARACTER*3 CFL
      INTEGER NCAND,NTRACK,ICONT(10),IWORD
      CALL GTVTRH(ICONT)
      NTRACK=ICONT(2)
      WRITE (PRUNIT,101) NTRACK
  101 FORMAT(/' # VTX tracks  =',I5) 
 1000 CONTINUE 
      RETURN
      END       
