      LOGICAL FUNCTION INICDC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Various initializations for CDC. 
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  ??-???-19??      
C-   Updated  12-FEB-1988   Ghita Rahal-Callot  : Change the constants for
C-                                                the 0-suppression
C-   Updated  21-OCT-1988   Ghita Rahal-Callot  : suppress VZERO of the
C-                                              variables of the 0_suppression
C    Updated  14-JUN-1989   Qizhong Li-Demarteau: add parameters for pulse
C                                         shape simulation and use SCDC(5)
C                                         as a switch for 0_suppression 
C-   Updated  17-JUL-1989   Harrison B. Prosper   
C-   Made into pbd inteface function
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:CDCPAR.INC'
      INCLUDE 'D0$INC:ZPULPR.INC/LIST'
      INCLUDE 'D0$INC:ZZEROP.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INTEGER  IERR
C----------------------------------------------------------------------
      INICDC = .TRUE.
      IF ( DCDC .LE. 0 ) GOTO 999
C
C ****  Initialize the geometry of the CDC in the store /ZEBSTP/
C
      CALL CDISTP ( 'CDC_STPFILE', IERR )
      IF (IERR .NE. 0 ) THEN
        WRITE(LOUT,*)' ****INICDC: Error initalization geometry'
        CALL EXIT(1)
      ENDIF
C
C
C ****  Initialize the common CDCPAR
C ****  # of volumes which identify each cell
C
      NUCDIM(1) = 1
      NUCDIM(2) = 2
      NUCDIM(3) = 2
      NUCDIM(4) = 2
      NUCDIM(5) = 2
      NUCDIM(6) = 2
      NUCDIM(7) = 1
C
C ****  Name of the cells in each layer
C
C ****  LAYER 1
C
C
      NAMESW (1,1) = 'DF11'
      NAMESW (2,1) = 'DFT1'
      NAMESW (3,1) = 'DFT1'
      NAMESW (4,1) = 'DFT1'
      NAMESW (5,1) = 'DFT1'
      NAMESW (6,1) = 'DFT1'
      NAMESW (7,1) = 'DF13'
C
C ****  LAYER 2
C
      NAMESW (1,2) = 'DF21'
      NAMESW (2,2) = 'DFT2'
      NAMESW (3,2) = 'DFT2'
      NAMESW (4,2) = 'DFT2'
      NAMESW (5,2) = 'DFT2'
      NAMESW (6,2) = 'DFT2'
      NAMESW (7,2) = 'DF23'
C
C ****  LAYER 3
C
      NAMESW (1,3) = 'DF31'
      NAMESW (2,3) = 'DFT3'
      NAMESW (3,3) = 'DFT3'
      NAMESW (4,3) = 'DFT3'
      NAMESW (5,3) = 'DFT3'
      NAMESW (6,3) = 'DFT3'
      NAMESW (7,3) = 'DF33'
C
C ****  LAYER 4
C
      NAMESW (1,4) = 'DF41'
      NAMESW (2,4) = 'DFT4'
      NAMESW (3,4) = 'DFT4'
      NAMESW (4,4) = 'DFT4'
      NAMESW (5,4) = 'DFT4'
      NAMESW (6,4) = 'DFT4'
      NAMESW (7,4) = 'DF43'

C 
C Parameters for pulse shape simulation: 
C                 (x=0: VTX; x=1: CDC; x=2: FDC)
C 
C PARTR and PARTW are fitted parameters from real data
C
C   Rise time = PARTR(1,x) * SQRT(Tdrift) + PARTR(2,x)  
C               (for uniform drift region: Tdrift > TRLIMT)
C             = PARTR(3,x) * Tdrift ** 2 + PARTR(4,x) * Tdrift + PARTR(5,x)
C               (for the non-uniform drift region: 0 < Tdrift < TRLIMT)
C
C Total width = PARTW(1,x) * SQRT(Tdrift) + PARTW(2,x) 
C               (for uniform drift region: Tdrift > TWLIMT)
C             = PARTW(3,x) * Tdrift ** 2 + PARTW(4,x) * Tdrift + PARTW(5,x)
C               (for the non-uniform drift region: 0 < Tdrift < TWLIMT)
C
      PARTR(1,1) = 0.476
      PARTR(2,1) = 31.9
      PARTR(3,1) = -0.00097
      PARTR(4,1) = 0.45
      PARTR(5,1) = -7.39
C
      PARTW(1,1) = 0.029
      PARTW(2,1) = 11.3
      PARTW(3,1) = -0.000097
      PARTW(4,1) = 0.083
      PARTW(5,1) = -2.2
C
      TRLIMT(1) = 300.0                 ! ns
      TWLIMT(1) = 220.0                 ! ns
      MINRS(1) = 20.0                   ! minimum rise time (ns)
      MINWID(1) = 10.0                  ! minimun width (FADC channels)
      SCALE(1)  = 3.0                   ! scale factor for pulse height
      PULWEI(1) = 1.2                   ! weight for center of gravity
      DLSCAL(1) = 1.2                   ! Delay line / Sense wire (width)
C
C ****  Fill the common ZZEROP
C ****  contains the thresholds for the Zero-suppression in the FADC for
C ****  the 3 chambers VTX, CDC, FDC.  DIFTHn(x) and BINTHn(x)
C                 (x=0: VTX; x=1: CDC; x=2: FDC)
C
      IF( SCDC(5) .EQ. 0. ) THEN
        DIFTH1(1) = 5
        DIFTH2(1) = 5
        DIFTH3(1) = 5
        BINTH1(1) = 27
        BINTH2(1) = 27
        BINTH3(1) = 27
        BINTH4(1) = 27
      ELSE
        DIFTH1(1) = 0
        DIFTH2(1) = 0
        DIFTH3(1) = 0
        BINTH1(1) = 0
        BINTH2(1) = 0
        BINTH3(1) = 0
        BINTH4(1) = 0
      ENDIF
C
  999 CONTINUE
      RETURN
      END
