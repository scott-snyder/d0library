      SUBROUTINE PRVERH(PRUNIT,LOC,NUM,FLAG,IFLAG)                 
C------------------------------------------------------------------
C 
C     Print out vertex header bank 
C 
C     Daria Zieminska  Feb. 1989
C                            
C------------------------------------------------------------------
      IMPLICIT NONE 
      INTEGER PRUNIT,NPRIM,NSEC,NUM,IWORD,ICONT(10),LOC,IFLAG
      CHARACTER*3 FLAG
      CALL GTVERH(ICONT)
      NPRIM=ICONT(2)
      NSEC=ICONT(3)
      WRITE (PRUNIT,101) NPRIM,NSEC
  101 FORMAT(' # primary vertices, # secondary vertices =',2I5) 
 1000 RETURN
      END       
