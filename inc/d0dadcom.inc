C-----------------------------------------------------------------------
C
C  Modified: 16-MAR-1995 JDH Add fortran i/o words for DF files.  
C     (VER_MINOR-->6)
C
C  Modified: 16-JAN-1995 JDH Add JEC_RCHECK to header and JCHECK to run
C     section records.  These implement checksums for the run section and
C     for the events in each run. (VER_MINOR-->5)
C
C  Modified: 21-DEC-1994 JDH Add JEC_EXTRA word to EC header.  This counts
C     extra records at EOF which exist if a corrupted catalog has been
C     repaired.  If the catalog has not been repaired, or has had pushed
C     beyond the repaired extra records, JEC_EXTRA=0. (VER_MINOR-->4)
C
C
C  Include file for d0dad library.  Variables having xx in name refer
C  to specific file types.  
C
C     xx = UE  ==>  unsorted event catalog
C          EC  ==>  event catalog
C          FC  ==>  file catalog
C          DF  ==>  d0dad file
C
C  D0DAD file information is kept in a Zebra division IXDDAD in order
C  to have dynamic' memory allocations during the creation of event
C  catalogs.  During normal readonly use, the store is less than 100
C  words in size. The top bank is called DADH.  The format is:
C  (I should have used C...)
C
C     LQ(LDAD-1) = Down link to UE file chain
C     LQ(LDAD-2)   Down link to EC file chain
C     LQ(LDAD-3)   Down link to DF file chain
C     LQ(LDAD-4)   Down link to FC file chain
C
C     IQ(LDAD+1) = # of UE files currently opened
C     IQ(LDAD+2) = # of EC files currently opened
C     IQ(LDAD+3) = # of DF files currently opened
C     IQ(LDAD+4) = # of FC files currently opened
C
C  Zebra specific variables:
C
C     IXDDAD - Zebra division number for the DAD division.
C     LDADH  - Pointer to top bank. (Protected)
C     LUEHD  - Pointer to UE bank chain. (Protected)
C     LECHD  - Pointer to EC bank chain. (Protected)
C     LDFHD  - Pointer to DF bank chain. (Protected)
C     LFCHD  - Pointer to FC bank chain. (Protected)
C
C     JLUN   - Offset to Logical unit position in file bank (xxHD)
C     JRW    - Offset to read/write flag (.ne.0 ==> Writable file)
C     JKEY   - Offset to file key (eg from D0DAD_GTUNIT)
C     NDxx   - Number of data words in addition to header size for
C       bank xxHD
C
C     TESTBSWAP - Test value  to determine if a byteswap is
C        needed.
C
C  Version numbers:
C      VER_D0DAD_MAJOR - Incremented for changes to file formats
C        or major new features.
C      VER_D0DAD_MINOR - Incremented for bug fixes/minor changes.
C      JXXVER - Offset to version number in header
C
C-----------------------------------------------------------------------
      INTEGER IXDDAD,LDSUP,LINTA(2),LDADH,LUEHD,LECHD,LDFHD,LFCHD
C-----------------------------------------------------------------------
      INTEGER VER_D0DAD_MAJOR,VER_D0DAD_MINOR,JXXVER
      PARAMETER(VER_D0DAD_MAJOR=1,VER_D0DAD_MINOR=6,JXXVER=3)
      CHARACTER D0DADBASE*5,D0DADGEN*7
      PARAMETER(D0DADBASE='D0DAD',D0DADGEN='//D0DAD')
      INTEGER TESTBSWAP
      PARAMETER(TESTBSWAP=305450479)     
C-----------------------------------------------------------------------
C
C  File parameters for file types used in d0dad.
C
C-----------------------------------------------------------------------
      INTEGER NFTYPE
      INTEGER JFUE,JFEC,JFDF,JFFC
      PARAMETER(NFTYPE=4)
      PARAMETER(JFUE=1,JFEC=2,JFDF=3,JFFC=4)
      CHARACTER*2 CFTYPE(NFTYPE)
C-----------------------------------------------------------------------
C
C  Zebra pointer parameters for file control banks (xxHD)
C
C-----------------------------------------------------------------------
      INTEGER JLUN,JRW,JKEY
      PARAMETER(JLUN=1,JRW=2,JKEY=3)
      INTEGER NDDF,NDEC,NDFC,NDUE
      PARAMETER(NDDF=7,NDEC=13,NDFC=7,NDUE=8)
C-----------------------------------------------------------------------
C
C  Tag strings copied from/to catalogs
C
C-----------------------------------------------------------------------
      CHARACTER*20 CDFTAG,CECTAG,CFCTAG,CUETAG
C-----------------------------------------------------------------------
C
C  Field sizes...
C    JSIZE is a default which must be larger than the largest field
C    size (JRECxx or JxxHED) represented here or in xx.inc 
C    (xx=FC,EC,DF,UE).  Adjustable field sizes in catalogs are checked 
C    at xxOPEN against JSIZE.  This is a hack to get around the lack of
C    dynamic allocation in FORTRAN.
C
C-----------------------------------------------------------------------
      INTEGER JSIZE
      PARAMETER(JSIZE=116)
C-----------------------------------------------------------------------
C
C  Overall control and temporary storage
C     LDDBG  - Debug level:  0 ==> No messages
C                        .ge.1 ==> Fatal Error messages
C                            2 ==> non-fatal errors
C                            3 ==> warnings
C                            4 ==> Summary Information
C                            5 ==> Moderate Information
C                            6 ==> Full information from manager code.
C                           10 ==> Full debug output
C     LISDAD - .TRUE. ==> Input file is a D0DAD file.
C     LRECL  - Zebra exchange mode record length.
C
C-----------------------------------------------------------------------
      LOGICAL  LISDAD,LEVT_CHECK
      INTEGER  LDDBG,LRECL
      INTEGER  IDRUN,IDEVNT,IDZRN,IDZBO,ISTMSK(2)
      CHARACTER*132 D0DAD_ERRTXT

C-----------------------------------------------------------------------
C-------------------------  UE specific defns ---------------------------
C-----------------------------------------------------------------------
C
C  Record sizes for unsorted event catalog
C
C-----------------------------------------------------------------------
      INTEGER JRECUE,JUEHED
      PARAMETER( JRECUE=6,JUEHED=11)
C-----------------------------------------------------------------------
C
C  Offsets to fields in control record.
C
C-----------------------------------------------------------------------
      INTEGER JUEFN,JUEGN,JUETAP,JUECOM,JUERUN
      PARAMETER(JUEFN=4,JUEGN=5,JUETAP=6,JUECOM=7,JUERUN=8)

C-----------------------------------------------------------------------
C-------------------------  EC specific defns ---------------------------
C-----------------------------------------------------------------------
C  Catalog format allocation constants...
C     KRECEC - Size of a logical record. (Event entry size)
C     KECHED - Size of header (longwords)
C     KRPREC - Number of logical records/physical record
C-----------------------------------------------------------------------
      INTEGER  KRECEC,KECHED,KRPREC,KRUN0,KECRUN,KECDLT,KECPAG,KECTIM
      INTEGER  KECRD
      PARAMETER(KECHED=29,KRECEC=6,KRPREC=1000,KRUN0=2,KECRUN=10000,
     +  KECDLT=1,KECPAG=0,KECTIM=30,KECRD=1)
C-----------------------------------------------------------------------
C  Catalog header (see ECHRD and ECHWRT and BLOCK DATA ECINIT)
C
C     JEC_RECVER  -Offset to  D0Dad version number used to write catalog.
C     JEC_IECHED - Offset to Header size
C     JEC_IRECEC - Offset to Logical record size (=KRECEC)
C     JEC_IRPREC - Offset to Number of logical records/physical record
C     JEC_IRUN0  - Offset to first physical record # of RUN Section
C     JEC_IRUN1  - Offset to physical record # of last entry in RUN 
C        Section (=IRUN0-1 if no entries)
C     JEC_IECRUN - Offset to Number of runs currently in run section
C     JEC_IECRD  - Offset to Increment if run section is expanded
C     JEC_IECPG0 - Offset to Phys Record # of first entry in PAGE table
C     JEC_IECPG1 - Offset to Phys Record # of last entry in PAGE table
C     JEC_IECPAG - Offset to Number of entries in PAGE table
C     JEC_IEVT0  - Offset to first Phys. record # EVENT Section
C     JEC_IEVT1  - Offset to last physical Record # in EVENT Section
C        (=IEVT0-1 if no entries)
C     JEC_IECEVT - Offset to last used logical record in EVENT section
C     JEC_IECDLT - Offset to increment for EVENT section expansion
C     JEC_IECTIM - Offset to delta(days) before run is considered `permanent'
C     JEC_ISDIRT - Offset to .NE.0 ==> Event
C        catalog not closed after update to run IDIRTY
C     JEC_IORDER - Offset to .EQ.0 ==> catalog run data are 
C        physically ordered
C     JEC_INDRCT - Offset to .EQ.0 ==> No page table indirection.
C     JEC_BSWAP  - Offset to byte swap detection field.
C     JEC_EXTRA  - Offset to counter of excess records beyond those in
C        event section known to header (=0 unless catalog has been patched)
C     JEC_RCHECK - Offset to checksum of run section (format version 1.05)
C
C-----------------------------------------------------------------------
      INTEGER  JEC_RECVER,JEC_CECTAG
      INTEGER JEC_IECHED,JEC_IRUN0,JEC_IRUN1,JEC_IECPG0,JEC_IECPG1
      INTEGER JEC_IECPAG,JEC_IEVT0,JEC_IEVT1,JEC_IECRUN
      INTEGER JEC_IECRD,JEC_IECEVT,JEC_IECDLT,JEC_IECTIM,JEC_IRPREC
      INTEGER JEC_IRECEC,JEC_ISDIRT,JEC_INOORD,JEC_INDRCT,JEC_BSWAP
      INTEGER JEC_EXTRA,JEC_RCHECK
      PARAMETER(JEC_RECVER=3,
     + JEC_IECHED=4,JEC_IRUN0=5,JEC_IRUN1=6,JEC_CECTAG=7,
     + JEC_IECPG0=12,JEC_IECPG1=13,
     + JEC_IECPAG=14,JEC_IEVT0=15,JEC_IEVT1=16,JEC_IECRUN=17,
     + JEC_IECRD=18,JEC_IECEVT=19,JEC_IECDLT=20,JEC_IECTIM=21,
     + JEC_IRPREC=22,JEC_IRECEC=23,JEC_ISDIRT=24,JEC_INOORD=25,
     + JEC_INDRCT=26,JEC_BSWAP=27,JEC_EXTRA=28,JEC_RCHECK=29)
C-----------------------------------------------------------------------
C
C  Link offsets on ECHD:
C     LLSREC  - Offset to link to RUNS bank (Run section of EC)
C     LLPREC  - Offset to link to buffer for a single physical record.
C     LLPTAB  - Offset to link to page table bank (NYU)
C     LLRDAT  - Offset to link to bank for currently loaded run data
C     LLDATA  - Offset to link to bank of data to be inserted into EC
C     LLORDER - Offset to link to ORDE bank (used in sort/insert)
C     LLRANK  - Offset to link to RANK bank (used in sort/insert)
C     LLISOLD - Offset to link to OLDE bank (used in sort/insert)
C     LLDNEW  - Offset to link to DNEW bank containing updated run data.
C     LLFLOK  - Offset to link to FLOK containing lock file name.
C     NLECHD  - Number of ECHD down/structural links.
C
C     FNLOCK  - Temporary copy of lock file name.
C
C-----------------------------------------------------------------------
      INTEGER LLRUNS,LLPREC,LLPTAB,LLRDAT,LLDATA,LLORDER,LLRANK,LLISOLD,
     +  LLDNEW,LLFLOK
      PARAMETER(LLRUNS=1,LLPREC=2,LLPTAB=3,LLRDAT=4,LLDATA=5,LLORDER=6,
     +  LLRANK=7,LLISOLD=8,LLDNEW=9,LLFLOK=10)
      INTEGER NLECHD
      PARAMETER(NLECHD=10)
C
C  Links for above.  Protected in ECOPEN.
C
      INTEGER LINTB(2)
      INTEGER LRUNS,LPREC,LPTAB,LRDAT,LDATA,LORDER,LRANK,LISOLD,LDNEW,
     >        LFLOK
      CHARACTER*240 FNLOCK
C-----------------------------------------------------------------------
C
C  Run record pointers
C     JRUNNO   - Offset to run number
C     JLRBEG   - Offset to first logical record with data for this run
C     JLREND   - Offset to last logical record with data for this run
C     JNEVTS   - Number of events for this run
C     JCHECK   - Checksum for this run's event section
C     JDATTIM  - Timestamp for this run's event section
C
C-----------------------------------------------------------------------
      INTEGER JRUNNO,JLRBEG,JLREND,JNEVTS,JCHECK,JDATTIM
      PARAMETER(JRUNNO=1,JLRBEG=2,JLREND=3,JNEVTS=4,JCHECK=5,JDATTIM=6)
C-----------------------------------------------------------------------
C
C  Event record pointers
C     JEVNT    - Offset to event number within event record
C     JMSK1    - Offset to mask field 1 
C     JMSK2    - Offset to mask field 2 
C     JFID     - Offset to is file identifier field
C     JZRN     - Offset to zebra record number
C     JZBO     - Offset to zebra byte offset
C
C-----------------------------------------------------------------------
      INTEGER JEVNT,JMSK1,JMSK2,JFID,JZRN,JZBO
      PARAMETER(JEVNT=1,JMSK1=2,JMSK2=3,JFID=4,JZRN=5,JZBO=6)
C-----------------------------------------------------------------------
C
C  Bank data pointers
C   *Deleted*  JHDIRT - Offset to Header Dirty flag
C   *Deleted*  JPDIRT - Offset to Page table dirty flag (NYI)
C   *Deleted*  JRDIRT - Offset to Run Section dirty flag
C     JNRUN  - Offset to Run number of data in RDAT bank.
C     JNEVT  - Offset to number of last located event
C     JIRUN  - Offset to Index of current run
C     JIEVT  - Offset to index of last located event
C     JIPREC - Offset to current record in PREC bank.
C     JECBS  - Offset to byteswap flag
C     JTS    - Offset to timestamp of most recently loaded run
C  (maximum must be <= NDEC defined in d0dadcom.inc)
C
C-----------------------------------------------------------------------
      INTEGER JNRUN,JNEVT,JIRUN,JIEVT,JIPREC,JECBS
      INTEGER JTS
      PARAMETER(JTS=7,JNRUN=8,JNEVT=9,
     +    JIRUN=10,JIEVT=11,JIPREC=12,JECBS=13)

C-----------------------------------------------------------------------
C-------------------------  FC specific defns ---------------------------
C-----------------------------------------------------------------------
C
C Field sizes for file catalog(NFCFN+NFCGN+NFCTAP+NFCCOM+4=JNCHFC)
C
C-----------------------------------------------------------------------
      INTEGER NFCFN,NFCGN,NFCTAP,NFCCOM
      PARAMETER(NFCFN=200,NFCGN=200,NFCTAP=20,NFCCOM=40)
      CHARACTER*(*) FCDELF,FCREPL
      PARAMETER(FCDELF='XXDeleXX',FCREPL='XXReplXX')
C-----------------------------------------------------------------------
C
C Record and header sizes and field storage for file catalog
C
C-----------------------------------------------------------------------
      INTEGER JRECFC,JNCHFC
      PARAMETER( JRECFC=116,JNCHFC=464 )
      CHARACTER CFNAME*200,CGNAME*200,CTAPE*20,CFCCOM*40,CLINE*464
      CHARACTER CFNTMP*200,CGNTMP*200,CTPTMP*20,CFCTMP*40,CLTMP*464

C-----------------------------------------------------------------------
C-------------------------  DF specific defns ---------------------------
C-----------------------------------------------------------------------
C
C  JRECDF - Logical record size (longwords)
C  JPRECDF- Physical record size (longwords) MUST BE INTEGER MULT. OF JRECDF
C  JDFHED - Header size (longwords)
C  JDFRUN - Offset to current run number in DFHD Zebra bank.
C  JDFBS  - Offset to byteswap flag in DFHD Zebra bank.
C  JDFDIRT- Offset to flag indicating unwritten buffer.
C  JDFIOFF- Offset to last read/written logical record
C  JDFLREC- Offset to position of number of logical records (in header)
C
C-----------------------------------------------------------------------
      INTEGER JRECDF,JDFHED,JDFRUN,JDFBS,JPRECDF,JDFDIRT,JDFLREC
      INTEGER JDFIOFF
      PARAMETER(JRECDF=4,JDFHED=16,JPRECDF=128,JDfRUN=4,
     >  JDFBS=5,JDFDIRT=6,JDFIOFF=7,JDFLREC=13)
      INTEGER DF_IOREC(JPRECDF)
C
C  Commons to hold all necessary stuff...
C
      COMMON/DADCOM/LISDAD,LDDBG,LRECL,IXDDAD,LDSUP,
     +  LINTA,LDADH,LUEHD,LECHD,LDFHD,LFCHD,
     +  LINTB,LRUNS,LPREC,LPTAB,LRDAT,LDATA,LORDER,LRANK,LISOLD,LDNEW,
     +  LFLOK,
     +  LEVT_CHECK,DF_IOREC
      COMMON/DADCOMC/CFTYPE,CDFTAG,CECTAG,CFCTAG,CUETAG,
     +  CFNAME,CGNAME,CTAPE,CFCCOM,FNLOCK
