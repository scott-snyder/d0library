C----------------------------------------------------------------------
C-
C-   Created  20-JAN-1988   Olivier Callot
C-
C----------------------------------------------------------------------
      INTEGER NMXSEG, NMXCLA
      PARAMETER( NMXSEG= 2000)
      PARAMETER( NMXCLA= 100)
      COMMON /PZCOMN/ MAXCLA, CURCLA, SEGLIS( NMXSEG )
      INTEGER MAXCLA, CURCLA, SEGLIS
C
C  MAXCLA is the highest used class number
C  CURCLA is the curent class number ( from last call to PZCLAS )
C  SEGLIS is the list of status of segment number. Contain either 0
C         if not used or the class number to whom it belongs
C
