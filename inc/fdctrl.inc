C     FDCTRL.INC
C---------------------------------------------------------------------------
C-
C-    Created                 by Jeffrey Bantly
C-
C---------------------------------------------------------------------------
      COMMON /FDCTRL/ LUNDBG, 
     &                  NEVDBG, 
     &                  LVLDBG(10), 
     &                  DBGFLG, 
     &                  MXHALF, MXUNIT, MXQUAD,         
     &                  MXSECT, MXSECP, 
     &                  MXWIRT, MXWIRP, 
     &                  TESTBM,                         
     &                  PULTH1(2),PULTH2(2),PULTH3(2),
     &                  PULMAX(2),PULWEI(2),            
     &                  FDINFM, 
     &                  FDTRTO, 
     &                  FDMXPH, 
     &                  FDMXCH,                
     &                  FDMINL, 
     &                  FDTDPH, 
     &                  FDTDIS, 
     &                  FDTCH2,                
     &                  FDLTOL,                
     &                  HISTON(8), 
     &                  HISPAR(3,8),           
     &                  FDSURV, 
     &                  FDNSTP                 
      INTEGER LUNDBG, NEVDBG, LVLDBG
      LOGICAL DBGFLG, TESTBM
      INTEGER MXHALF, MXUNIT, MXQUAD, MXSECT, MXSECP, MXWIRT, MXWIRP
      INTEGER PULTH1, PULTH2, PULTH3, PULMAX
      REAL    PULWEI
      REAL    FDTRTO, FDMXPH, FDMXCH, FDTDPH, FDTDIS, FDTCH2, FDLTOL
      INTEGER FDINFM, FDMINL
      INTEGER HISTON
      REAL    HISPAR
      INTEGER FDSURV, FDNSTP
C
C       LVLDBG(1) : Output constants ( FDINIT )
C       LVLDBG(2) : Display event    ( USREVT )
C       LVLDBG(3) : Pulse finding    ( FDPULS )
C       LVLDBG(4) : Hits             ( FDHITS )
C       LVLDBG(5) : Delay line       ( FDGETD )
C       LVLDBG(6) : Track segment    ( FDTFIT )
C       LVLDBG(7) : Space track      ( CRTRAK )
C       LVLDBG(8) : 
C       LVLDBG(9) : GEANT hit banks  ( FDHITS )
C       LVLDBG(10): Alignment        ( FDALGN )
