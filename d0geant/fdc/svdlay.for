      SUBROUTINE SVDLAY(TIME,PULPH,PULWID,ZPLUS,ZMINUS,
     &           WIRE,NDELAY,HDLAYR,HDLAYL)
C---------------------------------------------------------------------- 
C  Calculate delay line pulse time,hight and width for end 0 (right)
C  and 1 (left) from hit HITSV on adjacent wire (WIRE=0 or 1)
C             
C   Input:       
C       TIME
C       PULPH
C       PULWID
C       ZPLUS
C       ZMINUS
C       WIRE
C       NDELAY
C    Output:
C       HDLAYR      delay line hit, end 0  
C       HDLAYL      delay line hit, end 1
C       HDLAYR(1) = delay line time
C       HDLAYR(2) = pulse hight
C       HDLAYR(3) = pulse width
C  
C  K. Ng,    Mar. 19, 1986
C  D.Z. Apr., 1987
C---------------------------------------------------------------------- 
C
      IMPLICIT NONE
      INTEGER WIRE,NDELAY
      REAL TIME,PULPH,PULWID,ZPLUS,ZMINUS
      REAL DELPH,CONST1,CONST2,COEFF,DELVEL
      REAL HDLAYR(3),HDLAYL(3),DFTVEL                                
      DATA DFTVEL/0.0033/
      DATA DELVEL/0.22/      ! velocity of light in delay line (cm/nsec)
      DATA COEFF/0.0019/    ! attenuation coefficient for delay line
      DATA CONST1,CONST2/1.5,0.3/ ! amplitude of 
C      DATA CONST1,CONST2/0.744663,0.157923/ ! amplitude of 
C                                       delay pulse height/sense wire
      HDLAYR(1)=ZPLUS/DELVEL+TIME 
      HDLAYL(1)=ZMINUS/DELVEL+TIME 
      IF (WIRE.EQ.0.OR.WIRE.EQ.7) DELPH=CONST1*PULPH
      IF (WIRE.EQ.1.OR.WIRE.EQ.6) DELPH=CONST2*PULPH
      HDLAYR(2)=EXP(-COEFF*ZPLUS)*DELPH
      HDLAYL(2)=EXP(-COEFF*ZPLUS)*DELPH
      HDLAYR(3)=PULWID 
      HDLAYL(3)=PULWID 
      RETURN
      END        
