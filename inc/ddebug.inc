C----------------------------------------------------------------------
C-
C-   Created  21-APR-1989   Qizhong Li-Demarteau
C-
C-   for CDC debug control 
C----------------------------------------------------------------------
      COMMON /DDEBUG/ LUNDBG, NEVDBG, LVLDBG(10), DBGFLG
      INTEGER LUNDBG, NEVDBG, LVLDBG
      LOGICAL DBGFLG
C
C       LVLDBG(1) : Output constants ( CDINIT )
C       LVLDBG(2) : 
C       LVLDBG(3) : Pulse finding ( CDPULS )
C       LVLDBG(4) : Hits          ( CDHITS )
C       LVLDBG(5) : Delay line    ( CDGETZ )
C       LVLDBG(6) : Track segment ( CDTFIT )
C       LVLDBG(7) : Space track      ( CDTFIT,CDTRAK )
C       LVLDBG(8) : SRCP bank        ( DRDDBG )
C   following flags for Monte Carlo only
C       LVLDBG(9) : use GEAN-DSEC banks instead of processing FADC datas
C       LVLDBG(10): DITR bank        ( ISAJET tracks)   
C
