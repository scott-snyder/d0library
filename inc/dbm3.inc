C----------------------------------------------------------------------
C- DBL3 stuff
C
      INTEGER         MAXCRT 
      PARAMETER      (MAXCRT = 310)          
      INTEGER         LTDIR, LKEYS(0:MAXCRT), LDATA(0:MAXCRT)
      INTEGER         NKYS,NXKEY
      PARAMETER      (NKYS = 15)             
      PARAMETER      (NXKEY = 8)             
      INTEGER        DBUNIT
      LOGICAL        LDBEND
      COMMON /DBM3/  LTDIR,LKEYS,LDATA,DBUNIT,LDBEND
C-
C-      LKEYS(*) = Address of the Key bank for a Crate
C-      LDATA(*) = Address of the data bank for a Crate
C-      LTDIR    = Address of the Top Directory
C-      LDBEND   = Set true if DBEND is called. set false during db-init.
C
C----------------------------------------------------------------------
C-
C-   Name      LKMONIT.INC
C-   Purpose   Permanent link area addresses of MONITOR banks
C-   Created   3-NOV-1990  Lars Rasmussen
C-
C----------------------------------------------------------------------
      INTEGER MXMONIT
      PARAMETER( MXMONIT = 25 )
      INTEGER MONIT_LNK(MXMONIT)          
      COMMON /LKMONIT/ MONIT_LNK
C----------------------------------------------------------------------
