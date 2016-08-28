      SUBROUTINE HS_COMPARE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Compare hardware and simulator results,
C-                          Printout info and strip off bad events
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  15-FEB-1992   Jussara Miranda (from TRGR_FILL GL's original)
C-   Updated  22-MAR-2004   sss - compile with g77
C----------------------------------------------------------------------

      IMPLICIT NONE
      SAVE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C-- Hardware and simulator info 
      INTEGER H_CCT_LATCH(7),S_CCT_LATCH(7)
      LOGICAL S_L1BIT(16),H_L1BIT(16),HFW_L1BIT(0:31)
C-- MUSIM_RCP flags
      LOGICAL IPRLATCH,IPRL1
C-- MUSIM.RCP strip file mode (0 = no strip; 1=create E_RUN#_EVT#.DAT 
C   file/stripped event; 2=strip all events to a .sta file, 
C   3=list run# and evt# to thefile ERR_EVT.DAT)
      INTEGER OUTMODE
C-- MUSIM_RCP strip mode (1=strip based on l1 bits; 2=based on cct_latch,
C                        0= either)
      INTEGER STRIPMODE
C-- D0 run and event number
      INTEGER NUMRUN,D0_EVENT_NUM
C-- Mask to cct latch comparisson
      INTEGER LATCH_MASK(7)
C>>      DATA LATCH_MASK/'1FF003'X,'10F003'X,'10F003'X,
C>>     &                '10F3FF'X,'10F3FF'X,'13F03F'X,'13F03F'X/
C>> For now compare only octant and l1 bits
      DATA LATCH_MASK/'1FF003'X,'10F003'X,'10F003'X,
     &                '10F003'X,'10F003'X,'00003F'X,'00003F'X/
C-- Region to compare/printout, word in MUSIM.RCP
      INTEGER TRGREGMSKWORD
      LOGICAL PRREG(7)
      DATA PRREG /.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     &            .FALSE.,.FALSE./
C-- Working variables
      LOGICAL H_CCTL(7,0:31),S_CCTL(7,0:31)
      LOGICAL ISAJET_DATA,FIRST_WRT,FLGVAL,LATCH_PROBL,FIRST
      INTEGER GZISAE, LISAE
      INTEGER TEMP,I,II,ISIM,IHDW,IER
      CHARACTER*8 REGION(7)
      DATA FIRST / .TRUE. /
      DATA REGION /'>> CF >>','>> WN >>','>> WS >>',
     &             '>> ON >>','>> OS >>','>> SN >>','>> SS >>'/

C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('MUSIM_RCP')
        CALL EZGET('TRG_REG_MSK_WORD',TRGREGMSKWORD,IER)
        CALL EZGET('IPR_L1',IPRL1,IER)
        CALL EZGET('IPR_LATCH',IPRLATCH,IER)
        CALL EZGET('OUTPUT_MODE',OUTMODE,IER)
        CALL EZGET('STRIP_MODE',STRIPMODE,IER)
        CALL EZRSET()

        IF (OUTMODE.EQ.3) THEN
          OPEN(79,FILE='ERR_EVT.DAT',STATUS='NEW')
          WRITE(6,100)
  100     FORMAT(/'  LISTING D0_EVENT_NUM OF BAD EVENTS TO ERR_EVT.DAT')
        ELSE IF (OUTMODE.EQ.2)THEN
          WRITE (6,101)
  101     FORMAT(/'  STRIPPING BAD EVENTS TO ONE FILE .STA')
        ELSE IF (OUTMODE.EQ.1)THEN
          WRITE(6,102)
  102     FORMAT(/'  SRIPPING BAD EVENT EACH TO A FILE E_RUN#EVT#.DAT')
        ENDIF
        IF(MOD(TRGREGMSKWORD/1000000,   2).NE.0) PRREG(1)=.TRUE.   ! CF
        IF(MOD(TRGREGMSKWORD/100000,    2).NE.0) PRREG(2)=.TRUE.   ! WN
        IF(MOD(TRGREGMSKWORD/10000,     2).NE.0) PRREG(3)=.TRUE.   ! WS
        IF(MOD(TRGREGMSKWORD/1000,      2).NE.0) PRREG(4)=.TRUE.   ! ON
        IF(MOD(TRGREGMSKWORD/100,       2).NE.0) PRREG(5)=.TRUE.   ! OS
        IF(MOD(TRGREGMSKWORD/10,        2).NE.0) PRREG(6)=.TRUE.   ! SN
        IF(MOD(TRGREGMSKWORD,           2).NE.0) PRREG(7)=.TRUE.   ! SS

C-- Find out if isajet data
        ISAJET_DATA = .FALSE.
        LISAE = GZISAE()
        IF(LISAE.GT.0)ISAJET_DATA = .TRUE.
      ENDIF


C.. Get simulator info, CCT_LATCH and L1_BITS
      CALL MU_CCT_LATCH(S_CCT_LATCH)
      CALL MU_L1_RAW_BITS(S_L1BIT)

C.. Get hardware info, CCT_LATCH and L1_BITS
      CALL HARD_INFO(H_CCT_LATCH,H_L1BIT,HFW_L1BIT)

C.. Dump raw bits
      IF(IPRL1) THEN
        WRITE(6,*)
        WRITE(6,*) '   -- RAW BITS FROM HARDWARE AND SIMULATOR --'
        WRITE(6,110) (H_L1BIT(I),I=1,16)
        WRITE(6,111) (S_L1BIT(I),I=1,16)
      ENDIF
  110 FORMAT(' L1 MUON STATES   (HARDWARE)  :',L2,7(1X,2L2),1X,L2)
  111 FORMAT(' L1 MUON STATES   (SIMULATOR) :',L2,7(1X,2L2),1X,L2)


C.. Dumps latch bits
      IF(IPRLATCH) THEN
        DO II=1,7
          TEMP = H_CCT_LATCH(II)
          DO I=31,0,-1
            TEMP = ISHFTC( TEMP , 1 , 32)
            H_CCTL(II,I) = IAND  ( TEMP , 1 )
          ENDDO
          TEMP = S_CCT_LATCH(II)
          DO I=31,0,-1
            TEMP = ISHFTC( TEMP , 1 , 32)
            S_CCTL(II,I) = IAND  ( TEMP , 1 )
          ENDDO
        END DO          
        WRITE(6,*)
        WRITE(6,*) '  -- CCT LATCH HARDWARE -- '
        IF(PRREG(1)) WRITE(6,120) REGION(1),(H_CCTL(1,I),I=12,19)
        IF(PRREG(2)) WRITE(6,121) REGION(2),(H_CCTL(2,I),I=12,15)
        IF(PRREG(3)) WRITE(6,121) REGION(3),(H_CCTL(3,I),I=12,15)
        IF(PRREG(4)) WRITE(6,121) REGION(4),(H_CCTL(4,I),I=12,15)
        IF(PRREG(5)) WRITE(6,121) REGION(5),(H_CCTL(5,I),I=12,15)
        IF(PRREG(6)) WRITE(6,121) REGION(6),(H_CCTL(6,I),I=2,5)
        IF(PRREG(7)) WRITE(6,121) REGION(7),(H_CCTL(7,I),I=2,5)
        WRITE(6,*)
        WRITE(6,*) '  -- CCT LATCH SIMULATOR -- '
        IF(PRREG(1)) WRITE(6,120) REGION(1), (S_CCTL(1,I),I=12,19)
        IF(PRREG(2)) WRITE(6,121) REGION(2), (S_CCTL(2,I),I=12,15)
        IF(PRREG(3)) WRITE(6,121) REGION(3), (S_CCTL(3,I),I=12,15)
        IF(PRREG(4)) WRITE(6,121) REGION(4), (S_CCTL(4,I),I=12,15)
        IF(PRREG(5)) WRITE(6,121) REGION(5), (S_CCTL(5,I),I=12,15)
        IF(PRREG(6)) WRITE(6,121) REGION(6), (S_CCTL(6,I),I=2,5)
        IF(PRREG(7)) WRITE(6,121) REGION(7), (S_CCTL(7,I),I=2,5)
      ENDIF

  120 FORMAT(A6,8L3)
  121 FORMAT(A6,4L3)


C-- Compare hardware and simulator
C   Strip of bad events
      IF (.NOT.ISAJET_DATA.AND.OUTMODE.NE.0)THEN
        CALL EVNTID(NUMRUN,D0_EVENT_NUM)      

C-- Initialize Flags
        CALL FLGSET('WRITE_STREAM_STA',.FALSE.)
        CALL FLGSET('WRITE_EVENT',.FALSE.)

C-- Use L1BITS to compare
        IF (STRIPMODE.LE.1)THEN
          FIRST_WRT = .TRUE.
          DO I = 2, 15
            IF(H_L1BIT(I).NEQV.S_L1BIT(I))THEN
              WRITE(6,130)H_L1BIT,S_L1BIT
              IF(OUTMODE.EQ.1) CALL FLGSET('WRITE_STREAM_STA',.TRUE.)
              IF(OUTMODE.EQ.2) CALL FLGSET('WRITE_EVENT',.TRUE.)
              IF(OUTMODE.EQ.3) THEN
                FIRST_WRT=.FALSE.
                WRITE(79,*)NUMRUN,D0_EVENT_NUM
              ENDIF
              GO TO 10
            ENDIF
          ENDDO
        ENDIF
   10   CONTINUE
  130   FORMAT(1X ,' >>>> BAD L1_BITS EVENT STRIPPED ',/,
     &    1X,'L1BITS SIM: ',16L1,/,1X,'LIBITS HDW: ',16L1)

C-- Compare hardware and simulator through relevant information on CCT_LATCH
        IF ( MOD(STRIPMODE,2).EQ.0)THEN
          LATCH_PROBL=.FALSE.
          DO I=1,7
            IF (PRREG(I)) THEN
              ISIM=IAND(S_CCT_LATCH(I),LATCH_MASK(I))
              IHDW=IAND(H_CCT_LATCH(I),LATCH_MASK(I))
              
              IF(ISIM.NE.IHDW) THEN
                LATCH_PROBL=.TRUE.
                WRITE(6,140) REGION(I),ISIM,IHDW
  140           FORMAT('>>>> CCT LATCH PROBLEMS ',/,
     &            ' CCT_LATCH_SIM,CCT_LATCH_HDW: ', A6,2(3X,Z6))
                IF(OUTMODE.EQ.1) 
     &            CALL FLGSET('WRITE_STREAM_STA',.TRUE.)
                IF(OUTMODE.EQ.2) 
     &            CALL FLGSET('WRITE_EVENT',.TRUE.)
                IF(OUTMODE.EQ.3.AND.FIRST_WRT) 
     &            WRITE(79,*)NUMRUN,D0_EVENT_NUM
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        IF(OUTMODE.EQ.2.AND.FLGVAL('WRITE_EVENT'))
     &     CALL EVWRIT
      ENDIF

      RETURN

      END
