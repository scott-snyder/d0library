C++++	This is Concom.Inc
C
C		It contains various constants and variables.
C
      REAL*8            CONST
      INTEGER*4         KONST
      COMMON /CONCOM/   CONST(100),     KONST(100)
C
      INTEGER*4         MA,             MF,             MK,
     1  ML,             MT,             MW,             MCIMPR,
     2  MSCUT,          MSFREE,         MSTEP,          MTCUT
      INTEGER*4           ICUT,           IFAIL,          IFFSTP,
     1  NA,             NB,             NBONE,          NC,
     2  NCIMPR,         NCNSTR,         NF,             NFFSTP,
     3  NK,             NL,             NM,             NSCUT,
     4  NSTEP,          NT,             NTCUT,          NV,
     5  NW
      INTEGER*4           LCUT(10),       IDMFLG,         MEM,
     1  NEM
C
      REAL*8            CINF,           COKONS,         CONMIN,
     1  DCHIOK,         FSCALE,         FSOK,           FSMAX,
     2  FSRAT,          PRAT,           PSCALE,         SCALE,
     3  WRAT
      REAL*8            CHICUT,         CHIOLD,         CHISQ,
     1  FSOLD,          FSUM,           ESOLD,          ESUM,
     2  OVSL,           SMAX,           CNLVNW,         PIMP,
     3  APRANG,         APRREL,         CUTSZE,         CUTLIM,
     4  CUTFF
C
      EQUIVALENCE            (MA    ,KONST( 1)),  (MF    ,KONST( 2)),
     1  (MK    ,KONST( 3)),  (ML    ,KONST( 4)),  (MT    ,KONST( 5)),
     2  (MW    ,KONST( 9)),  (MCIMPR,KONST(10)),  (MSCUT ,KONST(11)),
     3  (MSFREE,KONST(12)),  (MSTEP ,KONST(13)),  (MTCUT ,KONST(14))
      EQUIVALENCE            (ICUT  ,KONST(15)),  (IFAIL ,KONST(16)),
     1  (IFFSTP,KONST(17)),  (NA    ,KONST(18)),  (NB    ,KONST(19)),
     2  (NBONE ,KONST(20)),  (NC    ,KONST(21)),  (NCIMPR,KONST(22)),
     3  (NCNSTR,KONST(23)),  (NF    ,KONST(24)),  (NFFSTP,KONST(25)),
     4  (NK    ,KONST(26)),  (NL    ,KONST(27)),  (NM    ,KONST(28)),
     5  (NSCUT ,KONST(29)),  (NSTEP ,KONST(30)),  (NT    ,KONST(31)),
     6  (NTCUT ,KONST(32)),  (NV    ,KONST(33)),  (NW    ,KONST(34))
      EQUIVALENCE            (LCUT  ,KONST(58)),  (IDMFLG,KONST(70)),
     1  (MEM   ,KONST(71)),  (NEM   ,KONST(72))
C
      EQUIVALENCE            (CINF  ,CONST( 2)),  (COKONS,CONST( 3)),
     1  (CONMIN,CONST( 4)),  (DCHIOK,CONST( 5)),  (FSCALE,CONST(12)),
     2  (FSOK  ,CONST(14)),  (FSMAX ,CONST(15)),  (FSRAT ,CONST(16)),
     3  (PRAT  ,CONST(18)),  (PSCALE,CONST(19)),  (SCALE ,CONST(22)),
     4  (WRAT  ,CONST(24))
      EQUIVALENCE            (CHICUT,CONST(26)),  (CHIOLD,CONST(28)),
     1  (CHISQ ,CONST(29)),  (FSOLD ,CONST(31)),  (FSUM  ,CONST(32)),
     2  (ESOLD ,CONST(33)),  (ESUM  ,CONST(34)),  (OVSL  ,CONST(36)),
     3  (SMAX  ,CONST(37)),  (CNLVNW,CONST(40)),  (PIMP  ,CONST(41)),
     4  (APRANG,CONST(48)),  (APRREL,CONST(49)),  (CUTSZE,CONST(50)),
     5  (CUTLIM,CONST(51)),  (CUTFF ,CONST(52))
C
C     REAL*8            DELK(3),        DPXIMP,         DPYIMP,
C    1  DPZIMP,         DPGIMP
C     EQUIVALENCE            (DELK  ,CONST( 6)),  (DPXIMP,CONST( 9)),
C    1  (DPYIMP,CONST(10)),  (DPZIMP,CONST(11)),  (DPGIMP,CONST(53))
C
C----	End of Concom.Inc
