C-----------------------------------------------------------------------
C
C  Definitions for standalone management program, D0DAD.
C
C    Author: John D. Hobbs
C    Date:   1-NOV-1993
C    Modified: 19-DEC-1994 JDH Add IMFIX mode to repair catalogs...
C    Modified: 28-DEC-1994 JDH Move streaming variables here to save 
C      space
C
C-----------------------------------------------------------------------
C
C  Function codes...
C
C     IMNULL - unknown option or other error.
C     IMSCAN - scan input Zebra file(s) to create unsorted event list
C     IMUPDT - Sort a unsorted event catalog into event and file catalogs
C     IMEDIT - Edit an existing event or file catalog.
C     IMSTRM - Create virtual streams.
C     IMUSER - Create a d0dad file from an input run/event text file.
C     IMCREA - Create event and file catalogs
C     IMDUMP - Print a listing of the contents of given file.
C     IMTEST - Test a d0dad file/file catalog combination
C     IMCOPY - Create an exchange mode file of the events in a d0dadfile
C     IMXLAT - Translate logical names in an fc into system specific names
C     IMCHEK - Check logical consistancy of D0DADF file
C     IMINFO - Save a summary of an input file (Now only UE files)
C     IMFIX  - Repair a corrupted event catalog if possible.
C     IMTSTR - Stream using a privately defined bit mask
C     IMMERG - Merge files of the same type
C
C     IMRCP  - scan Zebra file using RCP file as input.  (D0 specific)
C
C-----------------------------------------------------------------------
      INTEGER IMNULL,IMSCAN,IMUPDT,IMEDIT,IMSTRM,IMUSER,IMCREA
      INTEGER IMDUMP,IMTEST,IMCOPY,IMRCP,IMXLAT,IMCHEK,IMINFO,IMFIX
      INTEGER IMTSTR,IMMERG
      PARAMETER(IMNULL=101,IMSCAN=1,IMUPDT=2,IMEDIT=3,IMSTRM=4,IMUSER=5,
     +   IMCREA=6,IMCOPY=7,IMDUMP=8,IMTEST=9,IMRCP=10,IMXLAT=11,
     +   IMCHEK=12,IMINFO=13,IMFIX=14,IMTSTR=15,IMMERG=16)
C-----------------------------------------------------------------------
C
C  I/O Units used by d0dad...
C
C-----------------------------------------------------------------------
      INTEGER     IDADFC,IDADDF,IDADZB,IDADEC,IDADUE,IDADOK
C-----------------------------------------------------------------------
C
C  Storage for command line parsing results... 
C
C    FxxNAM - Filename
C    xxCOPT - Open options for file (either FORTRAN STATUS or ZEBRA)
C        xx = ZB - Zebra file
C             UE - unsorted event catalog
C             FC - file catalog
C             DF - d0dad file
C             EC - (sorted) event catalog
C    ISTBIT - Streaming Bit.
C
C-----------------------------------------------------------------------
      CHARACTER*132 FZBNAM,FUENAM,FFCNAM,FDFNAM,FECNAM,FNTMP,FNTEXT
      CHARACTER*10  ZBCOPT,UECOPT,FCCOPT,DFCOPT,ECCOPT
C-----------------------------------------------------------------------
C
C  Storage used to keep parameters derived from command line.
C
C-----------------------------------------------------------------------
      LOGICAL LECDEV,LBLIND,HEADER_ONLY
      INTEGER ISTBIT,ISTTYP,ISELR(2),ISELE(2),ITEST,IROFF,IEOFF
      INTEGER DATTIM_STAMP(2)
      CHARACTER*4 CZBTYP,EDSTRING*132,PREFIX*132
C
      COMMON/D0DADC/
C- I/O Units used by management code
     +   IDADUE,IDADDF,IDADEC,IDADFC,IDADZB,IDADOK,
C- Control variables
     +   LECDEV,ISTBIT,ISTTYP,ISELR,ISELE,ITEST,LBLIND,
     +   IROFF,IEOFF,DATTIM_STAMP,HEADER_ONLY
C- Character commons
      COMMON/D0DADCC/
     +   FZBNAM,FUENAM,FFCNAM,FDFNAM,FECNAM,FNTMP,
     +   ZBCOPT,UECOPT,FCCOPT,DFCOPT,ECCOPT,
     +   CZBTYP,EDSTRING,PREFIX,FNTEXT
C----------------------------------------------------------------------
C  Temporary storage for streaming
C----------------------------------------------------------------------
      INTEGER NSMAX
      PARAMETER(NSMAX=160)
      INTEGER ISTLUN(NSMAX),MSKBITS(2,NSMAX),NSEL(NSMAX)
      CHARACTER*128 CDFILE(NSMAX),COMMENT(NSMAX)
      CHARACTER*1 SDFOPT(NSMAX)
      COMMON/CSTREAM/CDFILE,COMMENT,SDFOPT
      COMMON/ISTREAM/ISTLUN,MSKBITS,NSEL
