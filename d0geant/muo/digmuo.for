      LOGICAL FUNCTION DIGMUO
C--------------------------------------------------------------------
C-    This digitizes hits in muon PDTs.    Hits have been stored in -
C- Geant hit bank in S/R STPMU.   This routine is called by S/R     -
C- GUDIGI.                                                          -
C-                                                                  -
C-    The detector names have been defined in S/R DETMU.    The     -
C- names are assumed to be ACxx, BCxx and CCxx for modules in       -
C- A-, B- and C-layer respectively.   xx is corresponding to the    -
C- last two digits in P. Martine's module numbering sheme (D0 note  -
C- 416, June-26-1986) except numbers 300-307.   Those are BCA0,     -
C- BCA1...BCA7,  in which second lower digit is defined as like     -
C- hexsadecimal.                                                    -
C-                                                                  -
C- Input:                                                           -
C-  (following is logically input for this routine.  This is        -
C-   obtained from Geant hit bank through S/R GFHITS called         -
C-   in this routine)                                               -
C-    NHITS      I  number of hits in each detector (PDT            -
C-                  module)                                         -
C-    HITS(i,j)  F  j-th hit in detector, ACEL.                     -
C-                    i=1=  module,  =2 plane,  =3 wire nuber       -
C-                      4=  distance from track to wire             -
C-                      5=  distance from read out end in cm.       -
C-                      6=  distance from far end in centi meter.   -
C-                      7=  phi incident angle in local X-Y plane   -
C-                      8=  theta incident angle for z direction    -
C-                      9=  time of fright of particle from origin  -
C-   (for debugging)                                                -
C-    IDEBUG     I  debug flag.   If 1, print out.    /GCFLAG/      -
C-    DHIT       I  print flag for Digi section.      /D0LOG/       -
C-    LOUT       I  logical unit number for printer.  /GCUNIT/      -
C-                                                                  -
C- Output:                                                          -
C-    Digitization bank for Muon system (MUD1) stored in S/R MSMUD1.-
C-                                                                  -
C- S.Kunori, N.Oshima,    3-Apr-87                                  -
C  DH 1-88  ADDRESS FOR PLANE GOES FROM 0-3                         -
C- S.Kunori  07-Dec-88    add crate header word in MUD1             -
C- Updated  28-JUL-1989   A.M.Jonckheere - Make in PBD logical function
C- SK 4-91  include 'call mscrat' to get crate number.
C- S.Igarashi 11-Apr-91   For digitization                          -
C- S.Igarashi 17-Apr-92   Fix bug of no pad hits                    -
C--------------------------------------------------------------------
      IMPLICIT NONE
C  -- (for debugging).
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'     ! to get IDEBUG.
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'     ! to get LOUT.
      INCLUDE 'D0$INC:D0LOG.INC/LIST'      ! to get DDIG.
C  -- local variables...
      INTEGER NVDIM,NHDIM,NHMAX
      PARAMETER (NVDIM=2)
      PARAMETER (NHDIM=9)
      PARAMETER (NHMAX=40)
      INTEGER NMODMX
      PARAMETER (NMODMX=307)
      INTEGER ICRATE,NMODS,MODNUM(NMODMX)
      INTEGER II,NMDS,MODN(NMODMX),MODH(NMODMX)
      INTEGER I,J,IMOD,IPL,ICL
      INTEGER JCRATE
      INTEGER IPL1    ! IPL1 is wire number plus one
      INTEGER ICL1    ! ICL1 is wire number plus one.
      INTEGER ITRA(NHMAX),NUMVS(NVDIM),NUMBV(NVDIM,NHMAX),NHITS
      REAL    HITS(NHDIM,NHMAX)
      CHARACTER*4 AMOD,APLN,ACEL
      INTEGER NINDX(24,4),INDX(NHMAX,24,4)
      INTEGER ITVC(2,2),IPAD(2,2)
      INTEGER IMAC
      LOGICAL DIGMUO_V1
      EXTERNAL DIGMUO_V1
      DATA NUMVS/2*0/
C
********************************************
*     Big Branch to old version,  V1.      *
********************************************
      IF(SMUO(2).LT.1.5) THEN
         DIGMUO=DIGMUO_V1()
         RETURN
      ENDIF

      DIGMUO = .TRUE.
      IF ( DMUO.LT.3 ) GOTO 999
C
C-----------debug(1)--------------------------------------------
      IF(IDEBUG.EQ.1 .AND. DDIG.EQ.1.AND.PMUO.GE.2) THEN
        WRITE(LOUT,*) ' ==S/R DIGMU==   HIT BANK FOR MUON...'
        CALL GPHITS('MPDT',0)
      ENDIF
C-----------end debug(1)----------------------------------------
C
C  Initialize MUD1 bank.
C  =====================
      CALL MSMUD1(1,ICRATE,IMOD,IPL,ICL,ITVC,IPAD,IMAC)
C
C  Loop over crates.
C  =================
C
      JCRATE=0
1000  CONTINUE
C      -- obtain crate number, number of modules in this crate
C         and module numbers.   If NMDS is -1, no more crates.  
      JCRATE=JCRATE+1
      CALL MSCRAT(JCRATE,ICRATE,NMDS,MODNUM)
      IF(NMDS.LE.-1) GO TO 1001
C
C        -- create crate header block.
      CALL MSMUD1(2,ICRATE,IMOD,IPL,ICL,ITVC,IPAD,IMAC)
C
C  Loop over PDT modules.     -- second loop to store hits in MUD1 bank.
C  ======================
C
      DO 100 II=1,NMDS
        IMAC=II
        IMOD=MODNUM(II)
C        -- Get detector name (ACEL) for this module...
        CALL MUMODC(IMOD,AMOD,APLN,ACEL)
C        -- skip non-existing module...
        IF(ACEL.EQ.' ') GO TO 101
C        -- Fetch hits in the module...
        CALL GFHITS('MPDT',ACEL,NVDIM,NHDIM,NHMAX,0,NUMVS,
     +                  ITRA,NUMBV,HITS,NHITS)
C        -- check if number of hists exceeded the limit, NHMAX.
C           if so,  S/R GFHITS returns NHMAX+1 for NHITS.
C           === WARNING ===   need to add error count here to report.
        IF(NHITS.GT.NHMAX) NHITS=NHMAX
C        -- skip module without hit...
        IF(NHITS.LE.0) GO TO 101
C        -- create indeces for hits in planes and cells...
        CALL MSINDX(NHITS,HITS,NINDX,INDX)
C
C        -- loop over planes and cells.
C
        DO 120 IPL1=1,4
          DO 140 ICL1=1,24,2
C           -- stor wire and plane number which start with 0...
            ICL=ICL1-1
            IPL=IPL1-1
C           -- check if at least one hit exist in two adjacent cell.
            IF(NINDX(ICL1,IPL1).NE.0.OR.NINDX(ICL1+1,IPL1).NE.0) THEN
C                 -- simulate TVC signal...
              CALL MSTVC(IPL1,ICL1,HITS,NINDX,INDX,ITVC)
C                 -- simulate PAD signal...
              CALL MSPAD(IPL1,ICL1,HITS,NINDX,INDX,IPAD)
C                 -- fill hit information in MUD1 bank...
              IF(IPAD(1,1).GT.0.OR.IPAD(1,2).GT.0.OR.
     &           IPAD(2,1).GT.0.OR.IPAD(2,2).GT.0)THEN
                CALL MSMUD1(3,ICRATE,IMOD,IPL,ICL,ITVC,IPAD,IMAC)
              ENDIF
            ENDIF
  140     CONTINUE
  120   CONTINUE
C----------debug(2)-------------------------------------------
        IF(IDEBUG.EQ.1.AND.DDIG.EQ.1.AND.PMUO.GE.2) THEN
          IF(NHITS.GT.0) THEN
            WRITE(LOUT,61) IMOD,ACEL,NHITS
            DO 661 I=1,NHITS
              WRITE(LOUT,62) I,ITRA(I),NUMBV(1,I)
     +        ,NUMBV(2,I),(HITS(J,I),J=1,9)
  661       CONTINUE
   61       FORMAT(' =DIGMU= imod=',I3,'   hcel=',A4,'    nhits=',I3)
   62       FORMAT('     i,it',2I4,'   vol',2I4,5X,1P9E10.3)
            WRITE(LOUT,63) NINDX
   63       FORMAT('     nindx---',4(/1X,24I3))
          ENDIF
        ENDIF
C----------end debug(2)---------------------------------------
  101   CONTINUE
  100 CONTINUE
C
      GO TO 1000      ! go to next crate.
 1001 CONTINUE
C
C       --- end of loop over crate ---
C
C  Store data in MUD1 bank.
C  =========================
      CALL MSMUD1(4,ICRATE,IMOD,IPL,ICL,ITVC,IPAD,IMAC)
C
  999 RETURN
      END
