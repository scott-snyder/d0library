C----------------------------------------------------------------------
C-
C-   Created  18-NOV-1990   Lars O. Rasmussen
C-
C----------------------------------------------------------------------
C- CDAQ, HdB, RCP stuff
C----------------------------------------------------------------------
      INTEGER         LHCB
      PARAMETER      (LHCB = 10)
      INTEGER         UERR
      PARAMETER      (UERR = 21)
      INTEGER         MAXREQ
      PARAMETER      (MAXREQ = 100)
      INTEGER         MAXITM 
      PARAMETER      (MAXITM = MAXATT*MAXDEV)
      INTEGER         MAXTYPAR
      PARAMETER      (MAXTYPAR=4*MAXTYP)
      INTEGER         LUNLOG
      PARAMETER      (LUNLOG = 6)
      INTEGER         DSCALE
      PARAMETER      (DSCALE = 1000)
C
      INTEGER         N_REQ
      INTEGER         N_ITM  (MAXREQ)
      INTEGER         REQ_ID (MAXREQ)
      INTEGER         ITM_ID (MAXITM,MAXREQ)
C
      COMMON /DBMCDAQ/ N_REQ,N_ITM,REQ_ID,ITM_ID
C
C----------------------------------------------------------------------
C
      LOGICAL       BOOK_ZB (MAXTYP),FILL_DB(MAXTYP)
      REAL          EXVAL(MAXITM),EXSTA(MAXITM)
      CHARACTER*24  F_DBFILE
      COMMON /DBMSTATUS/ F_DBFILE,EXVAL,EXSTA,BOOK_ZB,FILL_DB
C
      CHARACTER*11  D_TIME
      INTEGER       N_TIME
      INTEGER       N_MEAN
      COMMON /DBMRUNPAR/ N_TIME,N_MEAN,D_TIME
C
      CHARACTER *48 PRC_NODE
      CHARACTER *48 PRC_NAM
      INTEGER       PRC_ID
      COMMON /DBMPRCPAR/ PRC_ID,PRC_NAM,PRC_NODE
C----------------------------------------------------------------------
