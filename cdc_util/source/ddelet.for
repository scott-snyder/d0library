      SUBROUTINE DDELET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Delete user links and mirrors in 
C-                         CDC cell (sector,layer)
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created   6-NOV-1989   joey thompson: Based on VDELET
C-
C----------------------------------------------------------------------
      IMPLICIT NONE                           
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:CDCLNK.INC'
      INCLUDE 'D0$INC:CDLOCA.INC'
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'                             
      INTEGER LLINK,NLINK,ILINK,LOC,I1,I2,IBSET 
      INTEGER JLOC1,JLOC2,LUSER 
      LOGICAL BTEST
C------------------------------------------------------------------
      LUSER=LQ(LHEAD-IZUSER) 
      LLINK=LQ(LUSER-1)             
      NLINK=IQ(LUSER+1)
      DO 200 ILINK=1,NLINK          ! loop over links
        LOC=(LLINK + (ILINK-1)*6)                
        IF (IQ(LOC+6).EQ.0) THEN      !If link is deleted, git outta town
          JLOC1=IQ(LOC+1)/2         ! pointer to 1-st hit on link ILINK
          I1=IQ(JLOC1+9)
          IF (BTEST(I1,2)) THEN     ! check if 1-st hit used
            IQ(LOC+6) = 1    
            GO TO 200
          END IF
          JLOC2=IQ(LOC+2)/2         ! pointer to 2-nd hit on link ILINK
          I2=IQ(JLOC2+9)
          IF (BTEST(I2,2)) IQ(LOC+6)=1    
        END IF
  200   CONTINUE
  999 RETURN    
      END    
