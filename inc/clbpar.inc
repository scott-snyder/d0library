C----------------------------------------------------------------
C-                                                              -
C-    CLBPAR common block with static and current calibration   -
C-    parameters; see INI_CLB for parameters description        -
C-                                                              -
C-     AZ  Nov.16, 1986                                         -
C-                                                              -
C----------------------------------------------------------------
C
      INTEGER NLIST
      PARAMETER (NLIST=144)
C
      COMMON/CLBPAR/TASKNO,TASK_NAM,IUDBG,IUPAR,IUHST,IUCLB,IUFIL,
     1              FNAM_DBG,FNAM_PAR,FNAM_HST,FNAM_CLB,FNAM_FIL,
     2              NDBG,NZEB,NPRT,MREJ,
     3              N1_STEP,N2_STEP,N3_STEP,MAX_NSTEP,MAX_ADCS,
     4              MIN_AVRPH,MAX_AVRPH,MIN_NCHA,MIN_NOENT,MIN_NACC,
     5              IDSTP,XEXTR,NSIG1,NSIG2,MAX_DEL,MAX_SIG,MAX_BAD,
     5              LEVEL1_ID,SYNC_BITS,NUM_CARDS,CRATE_NUM,
     6              MOD_CNTRL,PED_SUB,ZER_SUP,DAT_MOD,GAN_LEV,
     7              IRUN,DATE,NACC,NHST,NRHST,NCHA,CONFID,LST_CHAN,
     8              NFILE,NFMAX,NEVTS,NEVTRD,STPNB
C
      INTEGER TASKNO,IUDBG,IUPAR,IUHST,IUCLB,NDBG,NZEB,NPRT,MREJ
      INTEGER IUFIL
      CHARACTER*8 TASK_NAM
      CHARACTER*80 FNAM_DBG,FNAM_PAR,FNAM_HST,FNAM_CLB,FNAM_FIL
      INTEGER N1_STEP,N2_STEP,N3_STEP,MAX_NSTEP,MAX_ADCS,MAX_BAD
      INTEGER MIN_AVRPH,MAX_AVRPH,MIN_NCHA,MIN_NOENT,MIN_NACC,IDSTP
      REAL    XEXTR,NSIG1,NSIG2,MAX_DEL,MAX_SIG
      INTEGER LEVEL1_ID,SYNC_BITS,NUM_CARDS,CRATE_NUM,MOD_CNTRL
      INTEGER PED_SUB,ZER_SUP,DAT_MOD,GAN_LEV
      INTEGER IRUN,DATE,NACC,NREJ,NHST,NRHST,NCHA,STPNB
      INTEGER NFILE,NFMAX,NEVTS,NEVTRD
      INTEGER CONFID,LST_CHAN(NLIST)
C      
