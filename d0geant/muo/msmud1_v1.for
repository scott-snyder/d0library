      SUBROUTINE MSMUD1_V1(IGO,ICRATE,IMOD,IPL,ICL,ITVC,IPAD)
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
C-  Output:                                                      -
C-     D0 Zebra output bank.   (MUD1)                            -
C-                                                               -
C-  S.Kunori   3-Apr-87                                          -
C     DJ 8-87 Change output format                               -
C-  S.Kunori  18-Apr-91   Change subroutine name to MSMUD1_v1    -
C-                        from MSMUD1.                           -
C-----------------------------------------------------------------
      IMPLICIT NONE
C                              
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'     ! to get IDEBUG.
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'     ! to get LOUT.
      INCLUDE 'D0$INC:D0LOG.INC/LIST'      ! to get DDIG.
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
      INCLUDE 'D0$LINKS:IZMUD1.LINK/LIST'  ! link pointer to MUD1.
C
      INTEGER IGO,ICRATE,IMOD,IPL,ICL,ITVC(2,2),IPAD(2,2)
C  -- local variables.         
      INTEGER MXCRAT,MXBLK1,MXBLK2,MXBLK3,MXBLK4
      PARAMETER (MXCRAT=1)
      PARAMETER (MXBLK1=320)
      PARAMETER (MXBLK2=2)
      PARAMETER (MXBLK3=9000)
      INTEGER NCRATE                      ! number of crates
      INTEGER NBLK1,BLK1(MXBLK1,MXCRAT)   ! crate header(1)
      INTEGER NBLK2,BLK2(MXBLK2,MXCRAT)   ! crate header(2)
      INTEGER NBLK3,BLK3(MXBLK3,MXCRAT)   ! hits.
      INTEGER NEVNT
      INTEGER I,J,L,N,NWDS,PRVMOD,NMOD,LMUD1
      INTEGER LDUMMY,NDUMMY,IFL
      DATA LDUMMY,NDUMMY,IFL/0,0,0/
      DATA NEVNT/0/
C
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
         BLK1(1,NCRATE)=6
         BLK1(2,NCRATE)=NEVNT+65535     ! 65535 = FFFF in HEX.
         BLK1(3,NCRATE)=ICRATE          ! crate number
         BLK1(4,NCRATE)=1               ! data type  (=1 for MC)
         BLK1(5,NCRATE)=0               ! delay/gain for pulsing data
         NBLK1=5
         BLK2(1,NCRATE)=0               ! number of data words (init.)
         BLK2(2,NCRATE)=0               ! trigger type
         NBLK2=2
         NBLK3=0
      ELSE IF(IGO.EQ.3) THEN
C
C  second call to fill TVC and ADC signal for each pair of cells.
C  ==============================================================
C                              
C        -- check if new module or not.
         IF(PRVMOD.NE.IMOD) THEN
            NBLK1=NBLK1+1
            BLK1(1,NCRATE)=BLK1(1,NCRATE)+1
            PRVMOD=IMOD
            NMOD=BLK1(1,NCRATE)-6
            BLK1(NMOD+5,NCRATE)=IMOD*65536
         ENDIF
C
C        -- fill data...
         L=BLK2(1,NCRATE)
C             --NB. plane number in raw data is descending order along
C                   coordinate axisis.
         BLK3(L+1,NCRATE)=IMOD*128+IPL*32+ICL  ! module,plane,wire no.
           BLK3(L+4,NCRATE)=ITVC(1,1)          ! drift time 1.
           BLK3(L+5,NCRATE)=ITVC(2,1)          ! drfit time 2.
         BLK3(L+2,NCRATE)=IPAD(1,1)          ! central pad, even wire.
         BLK3(L+3,NCRATE)=IPAD(2,1)          ! outside pad, even wire.
           BLK3(L+8,NCRATE)=ITVC(1,2)          ! delta time 1.
           BLK3(L+9,NCRATE)=ITVC(2,2)          ! delta time 2.
         BLK3(L+6,NCRATE)=IPAD(1,2)          ! central pad, odd wire.
         BLK3(L+7,NCRATE)=IPAD(2,2)          ! outside pad, odd wire.
C        -- count up number of words...
         NBLK3=NBLK3+9
         BLK1(NMOD+5,NCRATE)=BLK1(NMOD+5,NCRATE)+9
         BLK2(1,NCRATE)=BLK2(1,NCRATE)+9 
C
      ELSE IF(IGO.EQ.4) THEN
C
C  store in MUD1 ban. 
C  =================== 
C                                                             
C        -- number of data words.
         NWDS=0
         DO 100 I=1,NCRATE
            NWDS=NWDS+BLK1(1,I)+1+BLK2(1,I)
100      CONTINUE
C        -- lift zebra bank...
         CALL MZBOOK(IXMAIN,LMUD1,LHEAD,-IZMUD1,'MUD1'
     +              ,0,0,NWDS,2,-1)
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
