

      SUBROUTINE PTCHAN(NTFADC,TLAY,TWIR,TFADC,NUMLAY)
C===================================================================
C
C  Description:  Plots a histogram of the FADC traces for the TRD's
C             layers per wire.  It will display a specify layer in 
C             NUMLAY is set, otherwise all layers hit will be display.
C                   
C  Author:
C  =======
C    S. Hagopian Nov. 11, 1988 (based on PDCHAN by O. Callot)
C  
C  Modify:
C  =======
C    Lupe Rosas  Dec. 20, 1988
C    Lupe Howell Jan. 10, 1990
C====================================================================
      IMPLICIT NONE
C
C  Include Statements:
C  ===================
      INCLUDE 'D0$INC:GRAPHF77.INC'
C
C  Local Declarations:
C  ====================
C
      REAL  VYMIN, VYMAX, VXMIN, VXMAX  
      REAL  Y(124)
      CHARACTER*3 CWIR    ! LABEL VARIABLES
      CHARACTER*2 CLAY
      CHARACTER*23 TITLE
      INTEGER WIRE,IP,K,ISEG,J,NUMLAY
      INTEGER LFADC
      INTEGER NTFADC,TMAX
      PARAMETER(TMAX=15)
      INTEGER TLAY(TMAX),TWIR(TMAX)
      INTEGER ILAY,IWIR
      INTEGER FOUND(10),WR  ! ARRAY TO STORE WIRES FOUND
      REAL TFADC(128,TMAX)
      REAL DISP,SPACE
      INTEGER I
C--------------------------------------------------------------------
      DATA DISP/.50/
      DATA SPACE/.14/
      DATA LFADC/124/
      DATA TITLE/'FADC TRACES FOR THE TRD'/
C--------------------------------------------------------------------
C
C  Executable Code:
C  ================
C  Checking wires to display 
      DO 25 IP=1,10
       FOUND(IP)=0
  25  CONTINUE
      DO 300 IP=1,NTFADC  
        DO 140 WR=1,10                    ! CHECKING IF FOUND EARLIER
         IF (IP.EQ.FOUND(WR)) THEN
           GO TO 300                ! FOUND EARLIER
         ENDIF
  140   CONTINUE  
C   Writing title
          DO 200 J=IP,NTFADC  !  
            IF(TWIR(IP).EQ.TWIR(J)) THEN  ! CHECKING BY WIRE NUMBER
              IF (NUMLAY.NE.0) THEN
                 IF(NUMLAY.NE.TLAY(J)) THEN
                  FOUND(J)=J              ! STORING WIRE NUM FOUND
                  GO TO 200               ! CHECKING THE LAYER DESIRED
                ENDIF
              ENDIF
              FOUND(J)=J                  ! STORING WIRE NUM FOUND
              ILAY=TLAY(J)
              IWIR=TWIR(IP)
              WRITE(CLAY,5554)ILAY
 5554         FORMAT(I2)
              WRITE(CWIR,5555) IWIR
 5555         FORMAT(I3)
C-       SETTING WINDOW
              VXMAX = 1.
              VXMIN = -1.
            VYMAX = 0.78 - (SPACE * (ILAY-1)) - (DISP * (ILAY-1)) + 0.1
              VYMIN = VYMAX - 0.1
              CALL PUVPRT(VXMIN,VXMAX,VYMIN,VYMAX)
              CALL JWINDO(-50.,50.,-50.,50.)
              CALL PUOPEN 
              CALL PXCOLR('BLU')
              CALL JJUST(2,2)
              CALL JSIZE(1.4560,25.5660)
              CALL JMOVE(0.,40.)
              CALL J1STRG('LAYER   '//CLAY )
              CALL JMOVE(0.,0.)
              CALL J1STRG('WIRE    '//CWIR )
              CALL JRCLOS
              CALL VZERO(  Y, LFADC)
              DO 30 K=1,124 
              Y(K)=TFADC(K,J)    ! SETTING DATA FOR WIRE IP LAYER J
   30         CONTINUE
              VYMAX = 0.78 - (SPACE * (ILAY-1)) - (DISP * (ILAY-1))
              VYMIN = VYMAX - (DISP - 0.01)
              CALL PUVPRT(VXMIN,VXMAX,VYMIN,VYMAX)
              CALL PXHIST( LFADC, Y, 'RED' )
            ENDIF
  200     CONTINUE
  300   CONTINUE
  400 CONTINUE
      RETURN
      END
