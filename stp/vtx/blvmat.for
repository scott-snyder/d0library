      SUBROUTINE BLVMAT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Fills VMAT with materials to describe the 
C-   Vertex Chamber in the Monte Carlo 
C-
C-   Inputs  :  none
C-   Outputs :  none
C-   Controls:  none
C-
C-   Created  26-SEP-1988   Ghita Rahal-Callot
C-   Modified 18-JUN-1989   Tom Trippe - new geometry, see D0 note #808
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER NMAT, LVMAT
      PARAMETER (NMAT = 8)
C----------------------------------------------------------------------
C
C ****  Book VMAT
      CALL BKVMAT(NMAT,LVMAT)
C
C ****  Fill for each material
C
      LVMAT = LVMAT + 2
      IC ( LVMAT + 1 ) = 60
      CALL UCTOH('VTX WALL 0$         ', C(LVMAT+2), 4, 20 )
      C  ( LVMAT + 7 ) = 15.0                                   
      C  ( LVMAT + 8 ) =  7.5 
      C  ( LVMAT + 9 ) =  1.41 
      C  ( LVMAT + 10) = 26.0   
      C  ( LVMAT + 11) = 64.  
C
      LVMAT = LVMAT + 11
      IC ( LVMAT + 1 ) = 61
      CALL UCTOH('VTX WALL 1$         ', C(LVMAT+2), 4, 20 )
      C  ( LVMAT + 7 ) = 15.0                                   
      C  ( LVMAT + 8 ) =  7.5 
      C  ( LVMAT + 9 ) =  1.99 
      C  ( LVMAT + 10) = 18.4   
      C  ( LVMAT + 11) = 45.  
C
      LVMAT = LVMAT + 11
      IC ( LVMAT + 1 ) = 62
      CALL UCTOH('VTX WALL 2$         ', C(LVMAT+2), 4, 20 )
      C  ( LVMAT + 7 ) = 14.4                                   
      C  ( LVMAT + 8 ) =  7.1 
      C  ( LVMAT + 9 ) =  2.46 
      C  ( LVMAT + 10) = 15.4   
      C  ( LVMAT + 11) = 36.  
C
      LVMAT = LVMAT + 11
      IC ( LVMAT + 1 ) = 63
      CALL UCTOH('VTX WALL 3$         ', C(LVMAT+2), 4, 20 )
      C  ( LVMAT + 7 ) = 13.7                                   
      C  ( LVMAT + 8 ) =  6.8 
      C  ( LVMAT + 9 ) =  1.39 
      C  ( LVMAT + 10) = 27.9   
      C  ( LVMAT + 11) = 63.  
C
      LVMAT = LVMAT + 11
      IC ( LVMAT + 1 ) = 64
      CALL UCTOH('END CAP 0$          ', C(LVMAT+2), 4, 20 )
      C  ( LVMAT + 7 ) = 35.7
      C  ( LVMAT + 8 ) = 17.1
      C  ( LVMAT + 9 ) = 0.293
      C  ( LVMAT + 10) = 64.9
      C  ( LVMAT + 11) = 396.
C
      LVMAT = LVMAT + 11
      IC ( LVMAT + 1 ) = 65
      CALL UCTOH('END CAP 1$          ', C(LVMAT+2), 4, 20 )
      C  ( LVMAT + 7 ) = 34.7
      C  ( LVMAT + 8 ) = 16.5
      C  ( LVMAT + 9 ) = 0.423
      C  ( LVMAT + 10) = 45.8
      C  ( LVMAT + 11) = 270.
C
      LVMAT = LVMAT + 11
      IC ( LVMAT + 1 ) = 66
      CALL UCTOH('END CAP 2$          ', C(LVMAT+2), 4, 20 )
      C  ( LVMAT + 7 ) = 35.7
      C  ( LVMAT + 8 ) = 17.1
      C  ( LVMAT + 9 ) = 0.386
      C  ( LVMAT + 10) = 49.2
      C  ( LVMAT + 11) = 301.
C
      LVMAT = LVMAT + 11
      IC ( LVMAT + 1 ) = 67
      CALL UCTOH('VTX GAS 95CO2 5ETH$ ', C(LVMAT+2), 4, 20 )
      C  ( LVMAT + 7 ) = 13.33
      C  ( LVMAT + 8 ) = 6.71 
      C  ( LVMAT + 9 ) = 0.001813
      C  ( LVMAT + 10) = 20110.
      C  ( LVMAT + 11) = 49600.
C
  999 RETURN
      END
