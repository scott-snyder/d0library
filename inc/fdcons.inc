      COMMON /FDCONS/ LUNDBG, NEVDBG, LVLDBG(10), DBGFLG, 
     &                MAXLAY, MAXSEC, MAXEVT,             
     &                NPKBYT, NPKSUP, LDATOF,             
     &                PULTH1(2),PULTH2(2),PULTH3(2),
     &                PULMAX(2),PULWEI(2),                
     &                CDPEDS(0:MXFADC,0:MXSECT,0:MXLAYR), 
     &                CDTZER(0:MXFADC,0:MXSECT,0:MXLAYR), 
     &                CDGAIN(0:MXFADC,0:MXSECT,0:MXLAYR), 
     &                CDVELO(0:MXFADC,0:MXSECT,0:MXLAYR), 
     &                CDSOFF(0:MXFADC,0:MXSECT,0:MXLAYR), 
     &                CDLNUM(3,0:NBDELY),                 
     &                CDLSUM(0:NBDELY,0:MXSECT,0:MXLAYR), 
     &                CDLVEL(0:NBDELY,0:MXSECT,0:MXLAYR), 
     &                CDLTOL(0:NBDELY,0:MXSECT,0:MXLAYR), 
     &                CDSRAY(0:MXSECT,0:MXLAYR ),         
     &                CDPHI0(0:MXSECT,0:MXLAYR ),         
     &                CDPHI1(0:MXSECT,0:MXLAYR ),         
     &                CDYPOS(NBSENS), CDTRTO, CDINFM,     
     &                HISTON(NBHIST,0:MXSECT,0:MXLAYR),   
     &                HISPAR(3,NBHIST,0:MXSECT,0:MXLAYR)  
C
      INTEGER LUNDBG, NEVDBG, LVLDBG
      LOGICAL DBGFLG
      INTEGER MAXLAY, MAXSEC, MAXEVT
      INTEGER NPKBYT, NPKSUP, LDATOF, PULTH1, PULTH2, PULTH3, PULMAX
      REAL    PULWEI, CDPEDS, CDTZER, CDGAIN, CDVELO, CDSOFF
      INTEGER CDLNUM
      REAL    CDLSUM, CDLVEL, CDLTOL, CDYPOS, CDTRTO
      REAL    CDSRAY, CDPHI0, CDPHI1
      INTEGER CDINFM, HISTON
      REAL    HISPAR
C
C       LVLDBG(1) : OUTPUTS CDCONS.INP ONTO DEBUG UNIT
C       LVLDBG(2) : 
C       LVLDBG(3) : Pulse finding ( CDPULS )
C       LVLDBG(4) : Hits ( CDHITS )
C       LVLDBG(5) : Delay line ( CDELAY )
C       LVLDBG(6) : Track segment ( CDTFIT )
C       LVLDBG(7) : 
C       LVLDBG(8) : 
C       LVLDBG(9) : 
C       LVLDBG(10): 
C
      REAL    PI, TWOPI, PIOV16
      PARAMETER( PI     = 3.1415926536)
      PARAMETER( TWOPI  = 2.*PI)
      PARAMETER( PIOV16 = PI/16.)
