      SUBROUTINE SMUOVSN( IVSN, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get Geometry type constant version number
C-                         from MUD1 bank.
C-                         This works only MC. 
C-
C-   Inputs  : None
C-   Outputs : IVSN(1)   : MGEH version number
C-             IVSN(2)   : SSTH version nmuber
C-             IVSN(3)   : SMAH version nmuber
C-             IVSN(4)   : Beam pipe version nmuber
C-             IVSN(5)   : MMAH version number
C-             IVSN(6)   : MSGH version nmuber
C-             IVSN(7)   : Reserved 
C-             IVSN(8)   : Reserved 
C-             IER       : 0 is ok, 1 not exist MUD1
C-            
C-   Controls: None
C-
C-   Created   6-JAN-1994   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER*4 BANKNAME
      INTEGER IVSN(8), IER
      INTEGER LM, GZMUD1, IW5, IW6
C----------------------------------------------------------------------
      IER = 1
C
      LM = GZMUD1()
      IF ( LM.EQ.0 ) GOTO 999  ! MUD1 not exist
C 
      IW5 = IQ(LM+5)
      IW6 = IQ(LM+6)
C
      IVSN(1) = MOD(IW5,255)
      IW5 = ISHFT( IW5, -8 )
      IVSN(2) = MOD(IW5,255)
      IW5 = ISHFT( IW5, -8 )
      IVSN(3) = MOD(IW5,-8 )
      IW5 = ISHFT( IW5, -8 )
      IVSN(4) = MOD(IW5,-8 )
C
      IVSN(5) = MOD(IW6,255)
      IW6 = ISHFT( IW6, -8 )
      IVSN(6) = MOD(IW6,255)
      IW6 = ISHFT( IW6, -8 )
      IVSN(7) = MOD(IW6,-8 )
      IW6 = ISHFT( IW6, -8 )
      IVSN(8) = MOD(IW6,-8 )
C
      IER = 0
  999 RETURN
      END
