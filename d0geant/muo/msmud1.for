      SUBROUTINE MSMUD1(IGO,ICRATE,IMOD,IPL,ICL,ITVC,IPAD,IMAC)
C-----------------------------------------------------------------
C-     This fills TVC and ADC signal for muon PDT in D0 Zebra    -
C- bank, MUD1.    This routine is called from four  places in    -
C- S/R DIGMU-                                                    -
C-     IGO=1   initialization.                                   -
C-        =2   initialization for each crate.   called from      -
C-             inside loop for crates before loop for modules    -
C-             in the crate.                                     -
C-        =3   for each module to store hits in local array.     -
C-        =4   copy crate headers and hits in local arrays to    -
C              MUD1 bank after all crates processed.             -
C-                                                               -
C-  Input:                                                       -
C-     IGO       I  control flag                                 -
C-     ICRATE    I  crate number                                 -
C-     IMOD      I  module number                                -
C-     IPL       I  plane number                                 -
C-     ICL       I  cell (wire) number                           -
C-     ITVC(i,j) I  TVC output for i-th hit.                     -
C-                     j=1  drfift time in nsec.                 -
C-                      =2  delta time  in nsec.                 -
C-     IPAD(k,l) I  PAD output for l-th cell.                    -
C-                     k=1 center pad,  =2 outside pad           -
C-                     l=1 first cell,  =2 second cell.          -
C-     IMAC      I  MAC slot number 1 - 12                       -
C-  Output:                                                      -
C-     D0 Zebra output bank.   (MUD1)                            -
C-                                                               -
C-  -- VERSION 1 --
C-  S.Kunori   3-Apr-87                                          -
C-  DJ 8-87 Change output format
C   -- VERSION 2 --
C-  SK 4-91 change output format correcponding to MUD1 format
C           by M.Fortner- 3-7-1991
C-  SI 8-91 Store STPFILE version number in Create header
C-  SI 1-92 Set field map version number to 0 if SMUO(3)=0.
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
      INTEGER IGO,ICRATE,IMOD,IPL,ICL,ITVC(2,2),IPAD(2,2)
C  -- local variables.         
      INTEGER MXCRAT,MXBLK1,MXBLK2,MXBLK3,MXBLK4
      PARAMETER (MXCRAT=18)
      PARAMETER (MXBLK1=330)
      PARAMETER (MXBLK2=3)
      PARAMETER (MXBLK3=9000)
      INTEGER NCRATE                      ! number of crates
      INTEGER NBLK1,BLK1(MXBLK1,MXCRAT)   ! crate header(1)
      INTEGER NBLK2,BLK2(MXBLK2,MXCRAT)   ! crate header(2)
      INTEGER NBLK3,BLK3(MXBLK3,MXCRAT)   ! hits.
      INTEGER NEVNT
      INTEGER I,J,L,N,NWDS,PRVMOD,NMOD,LMUD1
      INTEGER LDUMMY,NDUMMY,IFL
      INTEGER IPAD_EVEN,IPAD_ODD          ! Pad latch bits
      INTEGER IMAC
      INTEGER  GZMUD1,GZMGEH,GZMMAH,GZSBPH,GZSMAH,GZSSTH
      EXTERNAL GZMUD1,GZMGEH,GZMMAH,GZSBPH,GZSMAH,GZSSTH
      DATA LDUMMY,NDUMMY,IFL/0,0,0/
      DATA NEVNT/0/
      INTEGER NUMVER                      ! Version Number MC 3/20/91
      DATA NUMVER/536875808/              ! '20001320'X
      INTEGER IDCRAT,NMODUS,IDMODU(12)
      INTEGER NVER_MGEH,NVER_MMAH         ! STP Ver. Number WAMUS GEO & MAG
      INTEGER LSBPH,LSMAH,LSSTH
      INTEGER NVER_SBPH,NVER_SMAH,NVER_SSTH ! STP Ver. Number for SAMUS
C
********************************************
*    Big Branch to the old version (V1)    *
********************************************
      IF(SMUO(2).LT.1.5) THEN
         CALL MSMUD1_V1(IGO,ICRATE,IMOD,IPL,ICL,ITVC,IPAD)
         RETURN
      ENDIF

      IF(IGO.EQ.1) THEN
C
C  first call to initialize this routin for each event. 
C  ====================================================
C        
C        -- number of crates.
         NCRATE=0
C        -- previous module number.
         PRVMOD=0
C
      ELSE IF(IGO.EQ.2) THEN
C
C  second call to store crate header block.
C  =========================================
C
         NCRATE=NCRATE+1
         CALL MSCRAT(NCRATE,IDCRAT,NMODUS,IDMODU)
         BLK1(1,NCRATE)=18
         BLK1(2,NCRATE)=NEVNT+65535     ! 65535 = FFFF in HEX.
         CALL SETBYT(J,1,8,IDCRAT)
         BLK1(3,NCRATE)=J               ! Crate number
     &                 +NMODUS*2**16    ! Number of MAC cards
         BLK1(4,NCRATE)=NUMVER          ! data type  (=4 for MC)
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
     &                 +NVER_SSTH*2**8 +NVER_MGEH
         IF(SMUO(3).EQ.0.) THEN
           BLK1(6,NCRATE)=0             ! Old uniform field version
         ELSE
           BLK1(6,NCRATE)=NVER_MMAH     ! MMAH version number
         ENDIF
         DO 10 I = 1 ,12
   10    BLK1(6+I,NCRATE)=IDMODU(I)*65536 ! Bits16-31 : MAC ID = Module ID
         NBLK1=18
         BLK2(1,NCRATE)=0               ! number of data words 
         NBLK2=1
         NBLK3=0
      ELSE IF(IGO.EQ.3) THEN
C
C  second call to fill TVC and ADC signal for each pair of cells.
C  ==============================================================
C                              
C        -- MAC word count --
         BLK1(6+IMAC,NCRATE)=BLK1(6+IMAC,NCRATE)+9
C
C        -- fill data...
         L=BLK2(1,NCRATE)
C             --NB. plane number in raw data is descending order along
C                   coordinate axisis.
C       Pad latch bits
         IPAD_EVEN=0
         IPAD_ODD =0
         IF(IPAD(1,1).GT.0.OR.IPAD(2,1).GT.0) IPAD_EVEN=1
         IF(IPAD(1,2).GT.0.OR.IPAD(2,2).GT.0) IPAD_ODD =1
C
         BLK3(L+1,NCRATE)=IMOD*256+IPL+ICL*4   ! module,plane,wire no.
         IF(IPAD_EVEN.EQ.1)BLK3(L+1,NCRATE)=IBSET(BLK3(L+1,NCRATE),30)
         IF(IPAD_ODD .EQ.1)BLK3(L+1,NCRATE)=IBSET(BLK3(L+1,NCRATE),31)
           BLK3(L+2,NCRATE)=ITVC(1,1)          ! drift time 1.
           BLK3(L+3,NCRATE)=ITVC(2,1)          ! drfit time 2.
         BLK3(L+4,NCRATE)=IPAD(1,1)          ! central pad, even wire.
         BLK3(L+5,NCRATE)=IPAD(2,1)          ! outside pad, even wire.
           BLK3(L+6,NCRATE)=ITVC(2,2)          ! delta time 2.
           BLK3(L+7,NCRATE)=ITVC(1,2)          ! delta time 1.
         BLK3(L+8,NCRATE)=IPAD(1,2)          ! central pad, odd wire.
         BLK3(L+9,NCRATE)=IPAD(2,2)          ! outside pad, odd wire.
C        -- count up number of words...
         NBLK3=NBLK3+9
         BLK2(1,NCRATE)=BLK2(1,NCRATE)+9 
C
      ELSE IF(IGO.EQ.4) THEN
C
C  store in MUD1 bank. 
C  =================== 
C                                                             
C        -- number of data words.
         NWDS=0
         DO 100 I=1,NCRATE
            NWDS=NWDS+BLK1(1,I)+1+BLK2(1,I)+5
100      CONTINUE
C        
C        -- check if MUD1 bank already exist or not.
         LMUD1=GZMUD1(0)
         IF(LMUD1.EQ.0) THEN
C           -- lift zebra bank...
            CALL MZBOOK(IXMAIN,LMUD1,LHEAD,-IZMUD1,'MUD1'
     +                 ,0,0,NWDS,2,-1)
         ELSE
C           -- push the exisiting bank.
            CALL MZPUSH(IXMAIN,LMUD1,0,NWDS,'I')
         ENDIF
C
         L=LMUD1
         DO 200 I=1,NCRATE
            N=BLK1(1,I)+1-NBLK2
            DO 210 J=1,N
               L=L+1
               IQ(L)=BLK1(J,I)
210         CONTINUE           
            N=NBLK2
            DO 220 J=1,N
               L=L+1
               IQ(L)=BLK2(J,I)
220         CONTINUE
            N=BLK2(1,I)
            DO 230 J=1,N
               L=L+1
               IQ(L)=BLK3(J,I)
230         CONTINUE
C        -- fill Crate Trailer
         L=L+1
         IQ(L)=0                        ! "non physical address'
         L=L+1
         IQ(L)=BLK1(1,I)+1+BLK2(1,I)+5  ! Total word count =NH+HD+5
         L=L+1
         CALL MSCRAT(I,IDCRAT,NMODUS,IDMODU)
         IQ(L)=IDCRAT                   ! Crate ID
         L=L+1
         IQ(L)=0                        ! Token Pass Status
         L=L+1
         IQ(L)=0                        ! Checksum
200      CONTINUE
C-------------------debug----------------------------------------
         IF(IDEBUG.EQ.1.AND.DDIG.EQ.1.AND.PMUO.GE.2) THEN 
            CALL PRMUD1(LOUT,LDUMMY,NDUMMY,' ',IFL)
         ENDIF
C-------------------end debug------------------------------------
C
      ENDIF
C
      RETURN
      END
