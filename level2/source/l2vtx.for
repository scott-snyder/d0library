       SUBROUTINE L2VTX(ok)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find VTX hits in level-2. The raw
C-   FADC data bank CDD1 is dropped, and replaced by hits bank 
C-   CDH1.
C-
C-   Inputs  : Pulse-finding parameters from VTRAKS_RCP bank,
C-             raw data from CDD1 bank.
C-   Outputs : logical ok = .true. if ok
C-             VTX hit data in (new) CDH1 bank.
C-   Controls: none.
C-
C-   Created  11-FEB-1994   Liang-ping Chen after Chris Klopfenstein's
C-                          L2CDC
C-   Updated   2-MAR-1994   Liang-ping Chen do not drop CDD1 if 
C-                          L2CDUNPK encounters data error  
C-   Updated   8-MAR-1994   liang-ping Chen  check Label counts   
C-                          Note: CALL T0RWFL is to be added before drop CDD1
C----------------------------------------------------------------------
       IMPLICIT NONE
       INCLUDE 'D0$INC:ZEBCOM.INC'
       INCLUDE 'D0$LINKS:IZCDD1.LINK'
       INTEGER IQUEST
       COMMON /QUEST/ IQUEST(100)
       LOGICAL OK
       LOGICAL EZERROR
       INTEGER LCDD1, LCDH1
       INTEGER IER, NWORDS
       INTEGER POINT_CDD1, POINT_CDH1, POINT_NEXT
       INTEGER ICRATE
       INTEGER LENGTH_CDD1
       INTEGER CRATE_HEADLEN, HEADER_SYNC, CONTROLLER, VERSION
       INTEGER CRATE_WORD_COUNT, TRAILER_SYNC, TOKEN_PASS, CHECKSUM
       INTEGER NCRATE, LEN_CABLE_TRAILER, DEPTH
       INTEGER CRATE_ID, TRIGNUM, SECID
       INTEGER CHANNEL_ID, ADDR, CHANNEL_LENGTH, LABEL
       INTEGER HIT, NPULSE
       INTEGER MXHTOT
       PARAMETER(MXHTOT = 500)
       INTEGER LAYER, SECTOR, WIRE, END
       INTEGER MAXHIT_SECTOR(0:2, 0:31)
       INTEGER STATUS(MXHTOT), TIME(MXHTOT)
       INTEGER AREA(MXHTOT), RAW(MXHTOT)
       INTEGER NSECTOT
       PARAMETER (NSECTOT=80)
       INTEGER MAXHITS, NVTXHIT
       INTEGER LABCOUNT, LABMAP(0:2,0:31,0:7,0:1)
       PARAMETER (MAXHITS=MXHTOT*NSECTOT)
C
       INTEGER POINT_SECINF, NSECHIT, WORDCOUNT
       INTEGER SEC_END, SEC_END_SAV, SEC_END_MASK
       INTEGER EXPDAT(1024)
       INTEGER GZVPDH, GZL2VPDH, LVPDH
       INTEGER NSEC(0:2)
       INTEGER STAT0, STAT2
       LOGICAL FIRST, INL2
       DATA    FIRST /.TRUE./
       DATA    NSEC/15,31,31/

       integer zffff
       data zffff / z'ffff' /
C----------------------------------------------------------------------
       OK = .FALSE.
       IF (FIRST) THEN
         FIRST = .FALSE.
         SEC_END_MASK=zFFFF
         SEC_END_MASK=IBCLR(SEC_END_MASK,1)
         SEC_END_MASK=IBCLR(SEC_END_MASK,2)
         SEC_END_MASK=IBCLR(SEC_END_MASK,3)
C                              
C **** temporary
C
C ****   CALL EZPICK('VTRAKS_RCP')
         CALL EZPick('L2CDHT_RCP')
         IF (EZERROR(IER)) THEN
           CALL ERRMSG('L2CDHT', 'L2VTX',
     &      'Unable to find bank L2CDHT_RCP', 'W')
           GOTO 999
         ENDIF
         CALL EZGET('INL2', INL2, IER)
C ****        CALL EZGET('NCRATE', NCRATE, IER)
C ****        IF (IER .NE. 0) NCRATE = 10
         NCRATE = 10
C ****         CALL EZGET('LEN_CABLE_TRAILER', LEN_CABLE_TRAILER, IER)
C ****         IF (IER .NE. 0) LEN_CABLE_TRAILER = 16
         LEN_CABLE_TRAILER = 16
C ****         CALL EZGET('MAX_DEPTH', DEPTH, IER)
C ****         IF (IER .NE. 0) DEPTH = 512
         DEPTH = 512
C
C               .
C               .
C               .
C
         call EZRSET
       ENDIF
       IF (INL2) THEN
         LVPDH = GZL2VPDH()
       ELSE
         LVPDH = GZVPDH()
       ENDIF
       IF (LVPDH .LE. 0) GOTO 999
C  check for CDD1
       LCDD1 = LQ(LHEAD-IZCDD1)
       IF (LCDD1 .LE. 0) goto 999
C  book result bank CDH1
       CALL BKCDH1(MAXHITS,LCDH1)
       SEC_END_SAV = -1
       POINT_SECINF= -1
       
C
C  init. max hits/sector array
C
       DO LAYER = 0, 2
         DO SECTOR = 0, NSEC(LAYER)
           MAXHIT_SECTOR(LAYER, SECTOR) = MXHTOT
         ENDDO
       ENDDO
C
C  initialize the label map and label counter
C
       CALL VZERO(LABMAP, 1536)
       LABCOUNT=0 
C
C  initialize pointers to position within CDD1, CDH1 banks
C
       LCDD1 = LQ(LHEAD-IZCDD1)
       LENGTH_CDD1 = IQ(LCDD1 - 1)
       POINT_CDD1 = LCDD1 + LENGTH_CDD1 - LEN_CABLE_TRAILER
       NVTXHIT=0
           
       POINT_CDH1 = LCDH1 + IQ(LCDH1+2)+1
C
C  loop over crates
C
       DO ICRATE = 0, NCRATE - 1
C
C  read crate trailer and header words
C
         CHECKSUM = IQ(POINT_CDD1)
         TOKEN_PASS = IQ(POINT_CDD1 - 1)
         TRAILER_SYNC = IQ(POINT_CDD1 - 2)
         CRATE_WORD_COUNT = IQ(POINT_CDD1 - 3)
C
         CRATE_ID = IBITS(TRAILER_SYNC, 0, 16)
         TRIGNUM = IBITS(TRAILER_SYNC, 16, 16)
C
         POINT_NEXT = POINT_CDD1 - CRATE_WORD_COUNT
         CRATE_HEADLEN = IQ(POINT_NEXT + 1)
         HEADER_SYNC = IQ(POINT_NEXT + 2)
         CONTROLLER = IQ(POINT_NEXT + 3)
         VERSION = IQ(POINT_NEXT + 4)
C
         POINT_CDD1 = POINT_CDD1 - 4
C
C  check trailer and header for consistency
C
         IF ((IBITS(CONTROLLER, 24, 8) .NE. CRATE_ID) .OR.
     &      (IBITS(HEADER_SYNC, 16, 16) .NE. TRIGNUM)) THEN
           CALL ERRMSG('L2HITS', 'L2VTX',
     &      'crate header/trailer discrepancy', 'W')
           GOTO 666
         ENDIF
C
C  find hits
C
         DO WHILE (POINT_CDD1 .GT.
     &            POINT_NEXT + CRATE_HEADLEN + 1)
           CHANNEL_ID = IBITS(IQ(POINT_CDD1), 16, 16)
           LAYER  = IBITS(CHANNEL_ID, 9, 2)
           SECTOR = IBITS(CHANNEL_ID, 4, 5)
           WIRE   = IBITS(CHANNEL_ID, 1, 3)
           END    = IBITS(CHANNEL_ID, 0, 1)
           LABEL  = IBITS(CHANNEL_ID, 0,11)
           SEC_END= IAND(LABEL, SEC_END_MASK)      ! contain LAYER, SECTOR, END
           CHANNEL_LENGTH = IBITS(IQ(POINT_CDD1), 0, 16) / 4
C
C ****  stringent check. All FADC channels should show up and show up once only
C
           IF (CHANNEL_LENGTH .GT. 0 .and.
     &         .NOT.BTEST(CHANNEL_ID,12)  ) THEN   ! not for T0 Detector  
             LABCOUNT=LABCOUNT+1              
             IF (LAYER.GT.2) GOTO 996
             IF (LAYER.EQ. 0 .AND. SECTOR.GT. 15 ) GOTO 996
             IF (LABMAP(LAYER, SECTOR, WIRE,END).GT.0) GOTO 997
             LABMAP(LAYER, SECTOR, WIRE,END)= 
     &       LABMAP(LAYER, SECTOR, WIRE,END)+1
           ENDIF
           IF (CHANNEL_LENGTH .GT. 1 .and.
     &         .NOT.BTEST(CHANNEL_ID,12)  ) THEN  ! channel with data, and 
                                                  ! not for T0 Detector  
             CALL L2CDUNPK(POINT_CDD1, DEPTH, EXPDAT, IER)
             IF (IER.NE.0) GOTO 999 
C
             CALL L2VTPULS(LABEL, EXPDAT,
     &                     MAXHIT_SECTOR(LAYER, SECTOR), NPULSE, TIME,
     &                     AREA, RAW, STATUS)
C
             MAXHIT_SECTOR(LAYER, SECTOR) =
     &                 MAXHIT_SECTOR(LAYER, SECTOR) - NPULSE
C
             IF (SEC_END .NE. SEC_END_SAV) THEN 
                 SEC_END_SAV = SEC_END
C
C ****  first, update the total number of hits in previous sector
C ****  for the first sector encountered, skip over this section
C  
               IF (POINT_SECINF.GT.0) THEN
                 NSECHIT=MIN(2**11-1, NSECHIT)   
                 WORDCOUNT= IQ(LCDH1+4)*NSECHIT +1  
                 CALL MVBITS(WORDCOUNT, 0, 13, IQ(POINT_SECINF), 0)
                 CALL MVBITS(NSECHIT,   0, 11, IQ(POINT_SECINF),13)
               ENDIF
C
C ****  then work on the new sector    
C
               NSECHIT=0
               POINT_SECINF= POINT_CDH1
               SECID = SECTOR + LAYER * 32 +END * 128
               CALL MVBITS(SECID, 0, 8,IQ(POINT_SECINF),24)
               POINT_CDH1 = POINT_CDH1 + 1             
             ENDIF   
             DO HIT = 1, NPULSE
               AREA(HIT) = MAX0(0, MIN0(2**10-1, AREA(HIT) ))
               TIME(HIT) = MAX0(0, MIN0(2**15-1, TIME(HIT) ))

               ADDR =IBITS(CHANNEL_ID, 0,4)
               STAT2=IBITS(STATUS(HIT),2,2) 
               STAT0=IBITS(STATUS(HIT),0,1)
                     
               IQ(POINT_CDH1)= ISHFT(ADDR,      13) +
     &                         ISHFT(STAT2,     11) +
     &                         ISHFT(STAT0,     10) + 
     &                         AREA(HIT)
               IQ(POINT_CDH1)=IOR( IQ(POINT_CDH1),ISHFT(TIME(HIT),17) )
 
C
               IQ(POINT_CDH1+1) = RAW(HIT)                      
C
               POINT_CDH1 = POINT_CDH1 + IQ(LCDH1+4)
             ENDDO
             NSECHIT=NSECHIT+NPULSE  
             NVTXHIT = NVTXHIT+NPULSE 
           ENDIF
           POINT_CDD1 = POINT_CDD1 - CHANNEL_LENGTH
         ENDDO
C
C  update the crate mask and total number of hits in CDH1 header
C         
         IQ(LCDH1+9) = IBSET (IQ(LCDH1+9), ICRATE) 
C
C  update pointers
C
         POINT_CDD1 = POINT_NEXT
666      CONTINUE 
       ENDDO
C
C ****  update the total number of hits in the very last sector
C
       IF (POINT_SECINF.GT.0) THEN
         NSECHIT=MIN(2**11-1, NSECHIT)   
         WORDCOUNT= IQ(LCDH1+4)*NSECHIT +1  
         CALL MVBITS(WORDCOUNT, 0, 13, IQ(POINT_SECINF), 0)
         CALL MVBITS(NSECHIT,   0, 11, IQ(POINT_SECINF),13)
       ENDIF
       IQ(LCDH1+3) = NVTXHIT

C
C  release empty space in CDH1 bank
C
       NWORDS = POINT_CDH1 -1 - LCDH1 - IQ(LCDH1 - 1)
       CALL MZPUSH(IXCOM, LCDH1, 0, NWORDS, 'R')

C
C  pack T0D and  drop raw data bank if all VTX FADC channel showed up 
C
       IF (LABCOUNT.EQ.1280) THEN 
C         CALL MZDROP(IXMAIN, LCDD1, ' ')
         OK = .TRUE.
         GOTO 999
       ENDIF
  996  CALL ERRMSG('L2HITS', 'L2VTX',
     &      'Invalid Layer or Sector #, data error', 'W')
       GOTO 999  
  997  CALL ERRMSG('L2HITS', 'L2VTX',
     &      'FADC channel ID conflict, data error', 'W')
  999  RETURN
       END
