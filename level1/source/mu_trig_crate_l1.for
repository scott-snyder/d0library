      SUBROUTINE MU_TRIG_CRATE_L1(IREG,JBCCT,CCT_LATCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill muon trigger information for one region
C-
C-   Inputs  : IREG   - Region number 1-7
C-
C-   Outputs : JBCCT  - 2-bit CCT output to Trigger monitor
C-             CCT_LATCH - CCT latch word for region IREG
C-   Controls:
C-
C-   Created  28-SEP-1991   K. Bazizi
C-   Updated  26-APR-1992   K. Bazizi
C-                          Add SAMUS-WAMUS overlap CCT trigger region
C-   Updated  25-JUN-1992   K. Bazizi
C-                          Add SAMUS CCT Trigger
C-   Updated  10-OCT-1992   K. Bazizi
C-                          Implementation of the 7 Trigger Region Scheme
C-   Updated  25-JAN-1993   K. Bazizi
C-                          New last stage CCTs for CF,WN,WS
C-   Updated  26-FEB-1993   Guilherme Lima
C-                          Add RCP switch to control above last stage CCT's
C-   Updated  31-MAR-1993   Guilherme Lima  
C-                          Update CCT Latch bits for D0note #1587
C    Updated  01-MAR-1994   Remove all L1.5 and use new centroids code (JM)
C----------------------------------------------------------------------
      IMPLICIT NONE
C-- I/O parameters
      INTEGER IREG,CCT_LATCH
      LOGICAL JBCCT(2)
C-- MUSIM.RCP print parameters
      LOGICAL IPRWCCT,IPRSWCCT,IPRSCCT,IPRSWTRPL,IPRSTRPL
      LOGICAL IPRCC
      LOGICAL PRINTANY
      CHARACTER*72 STRING
C-- Centroid variables
      INTEGER ICRS(16),ICSAM(0:15),ICENFL,ITRUNC,TEMP
      INTEGER TEMPICRS(16),TEMPICSAM(0:15)
      INTEGER CCRS(3,8)
      INTEGER ACC(16,3),BCC(16,5),CCC(16,5)
      INTEGER SACC(0:15,4),SBCC(0:15,4),SWCC(0:15,6)
      INTEGER SCC(0:15,6),SAMCC(0:15,6,3)
C-- Counters
      INTEGER I,II,III,IV,V,IND,CCTCNT,IER
C-- Variables to address the modules
      INTEGER MODNO,MODID
      INTEGER NCCT(7),MCCT(18),MDIR(18),ICCT,JCCT
      DATA NCCT/8,4,4,4,4,4,4/
C-- Axiliar variables
      CHARACTER*3 STRING3
      CHARACTER*20 STRING0
      INTEGER JTRIG,JB(12)
      INTEGER QQ(0:15,6),SWCUT(2)
      INTEGER SWTRIG,SAMTRIG
      LOGICAL CCT_BITS(0:31)
      LOGICAL SW_SEG(8)
      INTEGER SUMSWCC,IOCT
      INTEGER IVERS
      LOGICAL FIRST
      DATA IVERS/2/
      DATA FIRST/.TRUE./

C---------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST=.FALSE.

C.. Get coarse centroid cuts for SAMUS-WAMUS CCT Trigger
        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IER)     ! Check if error
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' EZPICK ERR','MU_TRIG_CRATE_L1',STRING,'F')
          GOTO 999
        ENDIF

        CALL EZGET('SAM_WAM_MULT_CUT_AX',SWCUT(1),IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' SAM_WAM_MULT_CUT_AX','MU_TRIG_CRATE_L1',
     &      STRING,'F')
          GOTO 999
        ENDIF

        CALL EZGET('SAM_WAM_MULT_CUT_BX',SWCUT(2),IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' SAM_WAM_MULT_CUT_BX','MU_TRIG_CRATE_L1',
     &      STRING,'F')
          GOTO 999
        ENDIF

        CALL EZGET_i('IPR_WCCT',IPRWCCT,IER)
        CALL EZGET_i('IPR_SWCCT',IPRSWCCT,IER)
        CALL EZGET_i('IPR_SCCT',IPRSCCT,IER)
        CALL EZGET_i('IPR_SWTRPL',IPRSWTRPL,IER)
        CALL EZGET_i('IPR_STRPL',IPRSTRPL,IER)
        CALL EZGET_i('IPR_CC',IPRCC,IER)
        PRINTANY = IPRWCCT.OR.IPRSWCCT.OR.IPRSCCT.OR.
     &             IPRSWTRPL.OR.IPRSTRPL.OR.IPRCC

        CALL EZRSET()

      ENDIF

C.. Initialize CCT array

      DO I=1,2
        JBCCT(I)=.FALSE.
      ENDDO

      DO I=0,31
        CCT_BITS(I)=.FALSE.
      ENDDO

      DO I=1,3
        DO II=1,8
          CCRS(I,II)=0
        ENDDO
      ENDDO

      DO I=0,15
        DO II=1,4
          SACC(I,II)=0
          SBCC(I,II)=0
        ENDDO
      ENDDO


      DO I = 0 ,15
        DO II = 1 ,6
          DO III = 1 , 3
            SAMCC(I,II,III) = 0
          ENDDO
        ENDDO
      ENDDO

C-- Printout
      IF(PRINTANY) THEN
        IF( IREG.EQ.1 ) PRINT 100
        IF( IREG.EQ.2 ) PRINT 101
        IF( IREG.EQ.3 ) PRINT 102
        IF( IREG.EQ.4 ) PRINT 103
        IF( IREG.EQ.5 ) PRINT 104
        IF( IREG.EQ.6 ) PRINT 105
        IF( IREG.EQ.7 ) PRINT 106
      ENDIF

  100 FORMAT(//' --- PROCESSING CENTRAL TRIGGER  -- ')
  101 FORMAT(//' --- PROCESSING END WAMUS NORTH TRIGGER -- ')
  102 FORMAT(//' --- PROCESSING END WAMUS SOUTH TRIGGER -- ')
  103 FORMAT(//' --- PROCESSING SAMUS-WAMUS NORTH TRIGGER -- ')
  104 FORMAT(//' --- PROCESSING SAMUS-WAMUS SOUTH TRIGGER -- ')
  105 FORMAT(//' --- PROCESSING PURE SAMUS NORTH TRIGGER -- ')
  106 FORMAT(//' --- PROCESSING PURE SAMUS SOUTH TRIGGER -- ')

C-- ******** PROCESS PURE WAMUS REGIONS CF+WN+WS *********
C
      IF (IREG.LE.3) THEN               ! start pure WAMUS processing
        DO III=1,NCCT(IREG)

C.. Reset CCT and OTC trigs
          JTRIG=0

C-- Compute octant number for WN and WS regions
C-- and the corresponding OTC numbers
          ICCT=III
          JCCT=III
          IF (IREG.EQ.2.OR.IREG.EQ.3) ICCT=7+III*2+IREG

C.. Zero out Centroids for next octant
          DO I=1,16
            DO II=1,5
              IF (II.LE.3) ACC(I,II)=0
              BCC(I,II)=0
              CCC(I,II)=0
            ENDDO
          ENDDO

C-- get muon module numbers for this octant
          CALL MU_MOD_NUM(ICCT,MCCT,MDIR)

C.. Loop over modules for this octant
          DO I=1,18

C-- Fill MAC array and find centroids
            MODNO=MCCT(I)
            MODID=MDIR(I)
            IF (MODNO.NE.0.AND.MODNO.LT.900) THEN
              CALL MUMCRS(MODNO,ITRUNC,ICENFL,ICRS)
              IF(ICENFL.GT.0) THEN
C-- Flip ICRS according to modid
                IF(ABS(MODID).NE.MODID) THEN
                  DO IV = 1,8
                    TEMP = ICRS(IV)
                    ICRS(IV)= ICRS(17-IV)
                    ICRS(17-IV)=TEMP
                  ENDDO
                ENDIF
                IF(IPRCC) THEN
                  WRITE (STRING3,'(I3.3)')MODNO
                  STRING0='MODNO,CC:  '//STRING3
                  CALL DUMPV(ICRS,1,16,STRING0,.TRUE.,1)
                ENDIF

C-- fill centroid arrays for CCT
                DO II=1,16
                  IF (I.LE.3) ACC(II,I)=ICRS(II)
                  IF (I.GE.7.AND.I.LE.11) BCC(II,I-6)=ICRS(II)
                  IF (I.GE.13) CCC(II,I-12)=ICRS(II)
                  IF ((IREG.EQ.2.OR.IREG.EQ.3).AND.(I.EQ.9))
     &              BCC(II,I-6)=0
                ENDDO
              ENDIF
            ENDIF
          ENDDO   ! End loop over modules


C.. Compute the COARSE CENTROID (CCT) Trigger for pure WAMUS
          CALL MU_WAM_CCT(ICCT,ACC,BCC,CCC,JTRIG,JB)

C.. Fill CCT Latch Bits
          IF(JTRIG.NE.0) THEN
            CCT_BITS(20)=.TRUE.         ! Logical OR of octants
            IF (IREG.EQ.1) THEN
              CCT_BITS(11+ICCT)=.TRUE.
            ELSE
              IOCT=MOD(ICCT,10)
              IOCT=(IOCT+1)/2
              CCT_BITS(11+IOCT)=.TRUE.
            ENDIF
          ENDIF


C.. OR CCT output bits by 4 for counting
          IF (JTRIG.NE.0) THEN
            DO II=1,3
              IND=(II-1)*4
              CCRS(II,JCCT)=JB(IND+1)+JB(IND+2)+JB(IND+3)+JB(IND+4)
              IF (IVERS.EQ.1) THEN
                IF (CCRS(II,JCCT).NE.0) CCRS(II,JCCT)=1
              ENDIF
            ENDDO
          ENDIF

        ENDDO       ! Quadrant loop  (ICCT)


C.. Count CCT trigger candidates
        CCTCNT=0
        DO I=1,NCCT(IREG)
          CCTCNT=CCTCNT+CCRS(1,I)+CCRS(2,I)+CCRS(3,I)
        ENDDO
        IF (CCTCNT.GT.3) CCTCNT=3
        IF (MOD(CCTCNT,2).EQ.1)   JBCCT(1)=.TRUE.
        IF (MOD(CCTCNT/2,2).EQ.1) JBCCT(2)=.TRUE.
        CCT_BITS(0)=JBCCT(1)
        CCT_BITS(1)=JBCCT(2)

      ENDIF         ! end pure WAMUS processing


C----------------------------------------------------------------------
C-- ******** PROCESS SAMUS-WAMUS OVERLAP REGION ON+OS *********
C--
      IF (IREG.EQ.4.OR.IREG.EQ.5) THEN  ! start overlap processing
C
C-- calculate SAMUS triplets for A and B stations
        CALL MU_MOD_NUM(15+IREG,MCCT,MDIR)
        DO I=1,2
          DO V = 1, 6
            DO IV = 0, 15
              SCC(IV,V) = 0
            ENDDO
          ENDDO

          DO II=1,6
            IND=6*(I-1)+II
            MODNO=MCCT(IND)
            MODID=MDIR(IND)
            IF (MODNO.GE.400.AND.MODNO.LE.457) THEN
              CALL MUMCRS(MODNO,ITRUNC,ICENFL,ICRS)
              IF(ICENFL.GT.0) THEN
C-- Flip ICRS according to modid
                IF(ABS(MODID).NE.MODID) THEN
                  DO IV = 1,8
                    TEMP = ICRS(IV)
                    ICRS(IV)= ICRS(17-IV)
                    ICRS(17-IV)=TEMP
                  ENDDO
                ENDIF

                DO III=0,15
                  ICSAM(III)=ICRS(III+1)
                  SAMCC(III,II,I)=ICSAM(III)
                  SCC(III,II)=ICSAM(III)
                ENDDO

                IF(IPRCC) THEN
                  WRITE (STRING3,'(I3.3)')MODNO
                  STRING0='MODNO,CC:  '//STRING3
                  CALL DUMPV(ICSAM,1,16,STRING0,.TRUE.,0)
                ENDIF

              ENDIF
            ENDIF
          ENDDO

          CALL MU_SAM_CCT_XY(SCC,SWCUT(I),QQ)
          IF(I.EQ.1) CALL MU_SAM_WAM_CABLES(IREG,I,QQ,SACC)
          IF(I.EQ.2) CALL MU_SAM_WAM_CABLES(IREG,I,QQ,SBCC)
        ENDDO

C.. Find CCT roads
        DO III=1,NCCT(IREG)
          ICCT=15+III*2+IREG
          JCCT=III

C.. Zero out Centroids for next octant
          DO I=0,15
            DO II=1,6
              SWCC(I,II)=0
            ENDDO
          ENDDO

C.. Calculate WAMUS Centroids for B and C layers
          CALL MU_MOD_NUM(ICCT,MCCT,MDIR)
          DO I=1,18
            MODNO=MCCT(I)
            MODID=MDIR(I)
            IF (MODNO.NE.0.AND.MODNO.LT.900) THEN
              CALL MUMCRS(MODNO,ITRUNC,ICENFL,ICRS)
C-- Flip ICRS according to modid

              IF(ICENFL.GT.0) THEN
                IF(ABS(MODID).NE.MODID) THEN
                  DO IV = 1,8
                    TEMP = ICRS(IV)
                    ICRS(IV)= ICRS(17-IV)
                    ICRS(17-IV)=TEMP
                  ENDDO
                ENDIF
                IF(IPRCC) THEN
                  WRITE (STRING3,'(I3.3)')MODNO
                  STRING0='MODNO,CC:  '//STRING3
                  CALL DUMPV(tempICRS,1,16,STRING0,.TRUE.,1)
                ENDIF

                DO II=0,15
                  IF (I.EQ.8) SWCC(II,3)=ICRS(II+1)
                  IF (I.EQ.9) SWCC(II,4)=ICRS(II+1)
                  IF (I.GE.13.AND.I.LE.14) SWCC(II,I-8)=ICRS(II+1)
                ENDDO
              ENDIF
            ENDIF
          ENDDO

          DO I=0,15
            SWCC(I,1)=SACC(I,JCCT)
            IF(ICCT.GE.21.AND.ICCT.LE.24) SWCC(I,4)=SBCC(I,JCCT)  ! TOP SWCCT
            IF(ICCT.GE.25.AND.ICCT.LE.28) SWCC(I,2)=SBCC(I,JCCT)  ! BOT SWCCT
          ENDDO

          CALL MU_SAM_WAM_CCT(ICCT,SWCC,QQ,SWTRIG,SW_SEG(2*JCCT-1))

          DO I=0,15
            ACC(I+1,1)=QQ(I,1)
          ENDDO

C.. Fill CCT Latch Bits
          IF(SWTRIG.NE.0) THEN
            CCT_BITS(20)=.TRUE.         ! Logical OR of octants
            IOCT=MOD(ICCT,10)
            IOCT=(IOCT+1)/2
            CCT_BITS(11+IOCT)=.TRUE.
          ENDIF

C.. OR CCT output bits by (4 then 2)= 8 for counting
          IF (SWTRIG.NE.0) THEN
            DO II=1,2
              DO I=1,8
                CCRS(II,JCCT)=CCRS(II,JCCT) + ACC((II-1)*8+I,1)
              ENDDO
              IF (CCRS(II,JCCT).NE.0) CCRS(II,JCCT)=1
            ENDDO
          ENDIF


          SUMSWCC=0
          DO I=1,6
            DO II=0,15
              SUMSWCC = SUMSWCC + SWCC(II,I)
              IF(SWCC(II,I).GT.0) THEN
                SWCC(II,I)=16*MOD(I-1,2)+II
                IF(MOD(I,2).EQ.1.AND.II.EQ.0) SWCC(II,I)=99
              ENDIF
            ENDDO
          ENDDO
        ENDDO

C.. Count CCT trigger candidates
        CCTCNT=0
        DO I=1,NCCT(IREG)
          CCTCNT=CCTCNT+CCRS(1,I)+CCRS(2,I)
        ENDDO
        IF (CCTCNT.GT.3) CCTCNT=3
        IF (MOD(CCTCNT,2).EQ.1)   JBCCT(1)=.TRUE.
        IF (MOD(CCTCNT/2,2).EQ.1) JBCCT(2)=.TRUE.
        CCT_BITS(0)=JBCCT(1)
        CCT_BITS(1)=JBCCT(2)
        DO I=1,8
          CCT_BITS(I+1)=SW_SEG(I)
        ENDDO

      ENDIF       ! end overlap proceesing

C----------------------------------------------------------------------
C-- ******** PROCESS SAMUS REGION SN+SS *********
C--

      IF( IREG.EQ.6 .OR. IREG.EQ.7 ) THEN ! start pure SAMUS processing

        CALL MU_MOD_NUM(13+IREG,MCCT,MDIR)
        DO I=1,3
          DO II=1,6
            IND=6*(I-1)+II
            MODNO=MCCT(IND)
            MODID=MDIR(IND)
            IF (MODNO.GE.400.AND.MODNO.LE.457) THEN
              CALL MUMCRS(MODNO,ITRUNC,ICENFL,ICRS)
              IF(ICENFL.GT.0) THEN

C-- Flip ICRS according to modid
                IF(ABS(MODID).NE.MODID) THEN
                  DO IV = 1,8
                    TEMP = ICRS(IV)
                    ICRS(IV)= ICRS(17-IV)
                    ICRS(17-IV)=TEMP
                  ENDDO
                ENDIF
                DO III=0,15
                  ICSAM(III) = ICRS(III+1)
                  SAMCC(III,II,I)=ICSAM(III)
                ENDDO

                IF(IPRCC) THEN
                  WRITE (STRING3,'(I3.3)')MODNO
                  STRING0='MODNO,CC:  '//STRING3
                  CALL DUMPV(ICSAM,1,16,STRING0,.TRUE.,0)
                ENDIF

              ENDIF
            ENDIF
          ENDDO
        ENDDO
        CALL MU_SAM_CCT(IREG,SAMCC,CCT_BITS,SAMTRIG)


C.. Fill CCT Latch Bits
        IF (SAMTRIG.NE.0) THEN
          DO IOCT=1,4
            IF(CCT_BITS(IOCT+1)) THEN
            ENDIF
          ENDDO
        ENDIF

C.. Fill SAMUS bits
        JBCCT(1)=CCT_BITS(0)
        JBCCT(2)=CCT_BITS(1)

      ENDIF     ! end pure SAMUS processing


C.. Pack CCT Latch bits into one WORD
      CCT_LATCH=0
      DO I=0,31
        IF(CCT_BITS(I)) CCT_LATCH=IBSET(CCT_LATCH,I)
      ENDDO


  999 CONTINUE
      RETURN
C-----------------------------------------------------------
      END
