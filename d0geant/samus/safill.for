      SUBROUTINE SAFILL(IGO, ICRATE, ISECT, NWD, IHITS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine fills raw data ZEBRA bank MUD1
C-                         with ADC signals from SAMUS drift tubes.
C-   MUD1 format correspond to M.Fortner's format from 3-7-91
C-   This routine is called from four places in S/R DIGSAM.
C-
C-   Inputs  :
C-     IGO              I  control flag
C-     ICRATE           I  crate number
C-     ISECT            I  section (MAC) number
C-     NWD              I  number of words in hits array
C-     IHITS(i)         I  hits array for one section
C-                         (odd - tube number, even - ADC count)
C-   Outputs :
C-     D0 Zebra output bank MUD1.
C-
C-   Controls:
C-     IGO=1   Initialization.
C-        =2   Initialization for each crate.
C-        =3   For each section (MAC) to store hits in local array.
C-        =4   Copy crate headers and hits from local arrays to
C              MUD1 bank after all crates processed.
C-
C-
C-   Created  18-OCT-1990   V. Glebov & V. Podstavkov
C-   Updated  20-MAR-1991   V. GLEBOV ! Change format to 3-7-91 version
C-   Updated   2-APR-1991   V. GLEBOV ! Check if bank MUD1 exist
C-   Updated   5-APR-1991   V. GLEBOV ! Polish to D0 standarts
C-   Updated  29-APR-1991   V. Glebov ! Add S/R SATOPM
C-   Updated  24-JAN-1994   Alexander Efimov & S.Igarashi ! add STPFILE version
C-                          numbers to the 5,6-th word of MUD1 crate header
C-   Updated  16-FEB-1994   Yu. Gutnikov Reading STP file version into MUD1
C-----------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'     ! to get IDEBUG.
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'     ! to get LOUT.
      INCLUDE 'D0$INC:D0LOG.INC/LIST'      ! to get DDIG.
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'     ! D0 STP ZEBRA bank.
      INCLUDE 'D0$INC:MU_ZEBSTP.INC/LIST'  ! D0 STP ZEBRA bank.
      INCLUDE 'D0$LINKS:IZMUD1.LINK/LIST'  ! link pointer to MUD1.
C
C  -- local variables.
      INTEGER MXSECT, MXTUBES
      PARAMETER( MXTUBES = 300 )
      INTEGER IGO,ICRATE,ISECT,NWD,IHITS(MXTUBES)
      INTEGER MXCRAT,MXBLK1,MXBLK3
      PARAMETER (MXSECT=6)
      PARAMETER (MXCRAT=6)
      PARAMETER (MXBLK1=320)
      PARAMETER (MXBLK3=9000)
      INTEGER GZMUD1
      INTEGER NCRATE
      INTEGER ND(MXCRAT)
      INTEGER CONWD                       ! Bits for "Controller Word"
      INTEGER CRID(MXCRAT)                ! Crate ID
      INTEGER NBLK1,BLK1(MXBLK1,MXCRAT)   ! crate header
      INTEGER NBLK3,BLK3(MXBLK3,MXCRAT)   ! hits.
      INTEGER NUMVER                      ! "Version Number"
      INTEGER NTRIG, NDATA, PM
      INTEGER I,J,L,N,NWDS,PRVMOD,LMUD1
      INTEGER LDUMMY,NDUMMY,IFL
      DATA LDUMMY,NDUMMY,IFL/0,0,0/
      DATA NTRIG/65536/, NDATA/0/
      DATA NUMVER/536875808/               ! '20001320'X
      DATA CONWD/397375/                   ! '6103F'X
      DATA CRID/ 182, 192, 202, 212, 222, 232/
      INTEGER  GZMGEH,GZMMAH,GZSBPH,GZSMAH,GZSSTH
      EXTERNAL GZMGEH,GZMMAH,GZSBPH,GZSMAH,GZSSTH
      INTEGER NVER_MGEH,NVER_MMAH         ! STP Ver. Number WAMUS GEO & MAG
      INTEGER LSBPH,LSMAH,LSSTH
      INTEGER NVER_SBPH,NVER_SMAH,NVER_SSTH ! STP Ver. Number for SAMUS
C
      INTEGER STP_VER,IER
      CHARACTER*80 MSG
C
C
      IF(IGO.EQ.1) THEN
C
C  first call to initialize this routine for each event.
C  ====================================================
C
C        -- number of crates.
        NCRATE=0
C
      ELSE IF (IGO .EQ. 2) THEN
C
C  second call to store crate header block.
C  =========================================
C
        NCRATE = NCRATE + 1
        BLK1(1,NCRATE) = 12                    ! "Header Length Count"
        BLK1(2,NCRATE) = NTRIG + 65535         ! "SYNC word" (65535=FFFF)
        I = CRID(ICRATE)                       ! Get Crate ID
        CALL SETBYT(J,1,8,I)                   ! Put it in left byte
        CALL SETBYT(J,13,20,CONWD)             ! Put CONWD in right 20 bits
        BLK1(3,NCRATE) = J                     ! "Controller Word"
        BLK1(4,NCRATE) = NUMVER                ! "Version Number" here MC
        LMGEH=GZMGEH(0)
        NVER_MGEH=IC(LMGEH+1)          ! WAMUS Geometry version number
        LMMAH=GZMMAH(0)
        NVER_MMAH=IC(LMMAH+1)          ! WAMUS Magnet version number
        LSBPH=GZSBPH(0)
        NVER_SBPH=IC(LSBPH+1)          ! SAMUS Beam pipe version number
        LSMAH=GZSMAH(0)
        NVER_SMAH=IC(LSMAH+1)          ! SAMUS Magnet version number
        LSSTH=GZSSTH(0)
        NVER_SSTH=IC(LSSTH+1)          ! SAMUS Station version number
        BLK1(5,NCRATE)=NVER_SBPH*2**24+NVER_SMAH*2**16
     &                +NVER_SSTH*2**8 +NVER_MGEH
        IF(SMUO(3).EQ.0.) THEN
          BLK1(6,NCRATE)=0             ! Old uniform field version
        ELSE
          BLK1(6,NCRATE)=NVER_MMAH     ! MMAH version number
        ENDIF
C
C ****  Read the version of STP file *********************
C
        CALL INRCP('SAMRECO_RCP',IER)
        CALL EZPICK('SAMRECO_RCP')
        CALL EZGET('STP_VER',STP_VER,IER)
        IF(IER.NE.0) THEN
          WRITE(MSG,'('' SAFILL: CANNOT READ FROM SAMRECO_RCP '',I5)')
     &          IER
          CALL INTMSG(MSG)
        END IF
        CALL MVBITS(STP_VER,0,8,BLK1(6,NCRATE),16) ! write STP version number
        CALL EZRSET                                ! into 6th word of crate
                                                   ! header ( byte #3 )
C ****  *************************************************
C

        CALL SATOPM(ICRATE,1,PM)
        BLK1(7,NCRATE) = PM*65536              ! Form MAC ID for MAC # 1
        CALL SATOPM(ICRATE,2,PM)
        BLK1(8,NCRATE) = PM*65536              ! Form MAC ID for MAC # 2
        CALL SATOPM(ICRATE,3,PM)
        BLK1(9,NCRATE) = PM*65536              ! Form MAC ID for MAC # 3
        CALL SATOPM(ICRATE,4,PM)
        BLK1(10,NCRATE)= PM*65536              ! Form MAC ID for MAC # 4
        CALL SATOPM(ICRATE,5,PM)
        BLK1(11,NCRATE)= PM*65536              ! Form MAC ID for MAC # 5
        CALL SATOPM(ICRATE,6,PM)
        BLK1(12,NCRATE)= PM*65536              ! Form MAC ID for MAC # 6
        ND(NCRATE)=0                  ! number of data words (initially)
        NBLK3=0
      ELSE IF(IGO .EQ. 3) THEN
C
C  third call to fill data from SAMUS drift tubes.
C  ================================================
C
C        -- fill data for one section
C
        L=ND(NCRATE)
        DO 10 I = 1, NWD
          BLK3(L+I,NCRATE) = IHITS(I)
   10   CONTINUE
C        -- put number of words into header
        BLK1(6+ISECT,NCRATE) = BLK1(6+ISECT,NCRATE) + NWD
C        -- count up number of words...
        NBLK3 = NBLK3 + NWD
        ND(NCRATE) = ND(NCRATE) + NWD
C
      ELSE IF(IGO.EQ.4) THEN
C
C  store in MUD1 bank.
C  ===================
C
C        -- number of data words.
        NWDS=0
        DO 100 I=1,NCRATE
          NWDS=NWDS+BLK1(1,I)+1+ND(I)+5
  100   CONTINUE
C    -- Check if bank MUD1 exist
        LMUD1 = GZMUD1()
        IF (LMUD1 .NE. 0 ) GO TO 105
C    -- Create ZEBRA bank MUD1
        CALL MZBOOK(IXMAIN,LMUD1,LHEAD,-IZMUD1,'MUD1'
     +              ,0,0,NWDS,2,-1)
C
        NDATA = 0
        GO TO 106
  105   CONTINUE
        LMUD1 = GZMUD1()
        NDATA = IQ(LMUD1 - 1)
        CALL MZPUSH(IXMAIN,LMUD1,0,NWDS,' ')
  106   CONTINUE
        L = LMUD1 + NDATA
        DO 200 I=1,NCRATE
          DO 210 J=1,12               ! Copy
            L=L+1                    !      crate
            IQ(L)=BLK1(J,I)          !            header
  210     CONTINUE
          L=L+1                    ! Copy number
          IQ(L)=ND(I)              !      of data words
          N=ND(I)
          DO 230 J=1,N                ! Copy
            L=L+1                    !      data
            IQ(L)=BLK3(J,I)          !           words
  230     CONTINUE
C        -- fill Crate Trailer
          L=L+1
          IQ(L)=0                  ! "non physical address"
          L=L+1
          IQ(L)=12+1+ND(I)+5       ! Total word count = NH + ND + 5
          L=L+1
          IQ(L)=NTRIG + CRID(I)    ! put Trigger # | Crate ID
          L=L+1
          IQ(L)=0                  ! Token Pass Status
          L=L+1
          IQ(L)=0                  ! Checksum
  200   CONTINUE
C-------------------debug----------------------------------------
        IF(IDEBUG.EQ.1.AND.DDIG.EQ.1.AND.PSAM.GE.2) THEN
          CALL PRMUD1(LOUT,LDUMMY,NDUMMY,' ',IFL)
        ENDIF
C-------------------end debug------------------------------------
C
      ENDIF
C
  999 RETURN
      END
