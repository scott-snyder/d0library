C++++	This is Wamcom.Inc
C
C		The following arrays are input and/or output
C
      REAL*8            BEVERT,        DIST,            EDNEW,
     1  PULL,           TARGET,        VCHISQ,          VFSUM,
     2  XCOMP,          XMEAS,         XNEW,            XVERT,
     3  ESTR
      INTEGER*4         LCON,          LCOPL,           LCOR,
     4  LINC,           LQUAL,         LTYPE,           LVTX
C
      COMMON /WAMCOM/   BEVERT(4,4,18),DIST(6),         EDNEW(4,18),
     1  PULL(4,18),     TARGET(6),     VCHISQ(6),       VFSUM(6),
     2  XCOMP(4,18),    XMEAS(4,18),   XNEW(4,18),      XVERT(4,18),
     3  LCON(6),        LCOPL(3,4),    LCOR(28,2),      LINC(6),
     4  LQUAL(4,18),    LTYPE(18),     LVTX(18),        ESTR(6100)
      REAL*8            EE(4,18,4,18), EINCOR(4,4,28),  EMEAS(4,4,18),
     1  WKST(180)
      EQUIVALENCE            (EE    ,ESTR(   1)), (EINCOR,ESTR(5185)),
     1  (EMEAS ,ESTR(5633)), (WKST  ,ESTR(5921))
C
C		The following arrays are for internal use only
C
      REAL*8            ALPHA(4,12),   BE(54,4,6),      BEFLAT(4,3,4),
     1  BEG(72,54),     CC(54),        CNEW(4,18),      COLD(4,18),
     2  DKDX(4,6),      EEPK(72,72),   EEWS(24,54),     FF(4,6),
     3  FFOLD(4,6),     FLATK(3),      FLATWS(3),       FLIN(18),
     4  GCOR(4,4,28),   GEE(4,4,18),   GSTAR(48),       HH(72,4,12),
     5  HHSCAL(48),     PSCWS(4,5),    PULPK(72),       RR(4,6),
     6  TT(72,48),      UFFVEC(4,6),   FLATCS(3,3)
      INTEGER*4         LNWELL(18),    LSEQ(4,18),      LSEQW(18),
     1  LVWELL(4,18)
      EQUIVALENCE            (ALPHA ,EE  (3457)), (BE    ,EE  (3889)),
     1  (BEFLAT,EE  (3817)), (BEG   ,EE  (  49)), (CC    ,BE  (1243)),
     2  (CNEW  ,EDNEW(  1)), (COLD  ,PULL(   1)), (DKDX  ,WKST(  49)),
     3  (EEPK  ,ESTR( 433)), (EEWS  ,BE  (   1)), (FF    ,EE  (3601)),
     4  (FFOLD ,EE  (3625)), (FLATK ,HH  ( 217)), (FLATWS,HH  ( 289)),
     5  (FLIN  ,EE  (3529)), (GCOR  ,EINCOR( 1)), (GEE   ,EMEAS(  1)),
     6  (GSTAR ,WKST(   1)), (HH    ,EE  (   1)), (HHSCAL,EE  (3745)),
     7  (PSCWS ,XCOMP(  1)), (PULPK ,EDNEW(  1)), (RR    ,EE  (3673)),
     8  (TT    ,HH  (   1)), (UFFVEC,HHSCAL( 1))
      EQUIVALENCE            (LNWELL,WKST(  73)), (LSEQ  ,WKST(  91)),
     1  (LSEQW ,WKST( 163)), (LVWELL,COLD(   1)), (FLATCS,HH  ( 649))
C
C----	End of Wamcom.Inc
