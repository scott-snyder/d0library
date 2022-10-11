      SUBROUTINE MU_SAM_CCT(IREG,SAMCC,SAMBITS,SAMTRIG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the SAMUS CCT trigger for one
C-                         trigger sector North or South
C-
C-   Inputs  :
C-              IREG = Trigger region SAMUS North (6)/ South (7)
C-              SAMCC (0:15,6,3) = Coarse centroids from X,Y,U planes
C-                                   for A,B,C stations
C-
C-   Outputs : SAMBITS = 28 bit output to CCT Latch card
C-             SAMTRIG = Trigger Flag (1= at least one candidate,0=none)
C-
C-   Controls: None
C-
C-   Created  17-JUN-1992   Kamel A. Bazizi
C-   Updated  31-MAR-1993   Guilherme Lima   Update bits to CCT Latch cards
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,II,III,IND,IREG
      INTEGER SAMCC_FILT(0:15,6,3)
      INTEGER SCC_FILT_X(0:15,6),SCC_FILT_Y(0:15,6),SCC_FILT_U(0:15,6)
      INTEGER XXX(0:15,2),YYY(0:15,2),UUU(0:15,2),SCC(0:15,6)
      INTEGER XXXFLG,YYYFLG,UUUFLG
      INTEGER SAMCC(0:15,6,3),QQ(0:15,6),SAMTRIG
      LOGICAL SAMBITS(0:31)
      integer sambits_i(0:31)
      INTEGER MULT(6),IABC(3),IA,IB,IC
      INTEGER FLIPSTAT(3,2),MULT_CUT(3),IER
      CHARACTER*72 STRING
      LOGICAL FIRST
      INTEGER IERS1,IERS2,IERS3
      DATA FLIPSTAT/
     &  1, -1,  1,       ! SNA,SNB,SNC
     &  -1,  1, -1 /     ! SSA,SSB,SSC
      LOGICAL IPRSCCT
      DATA FIRST/.TRUE./

C      CALL TRIPL_STUDY(IREG,SAMCC)
C
C
C-- get coarse centroid cuts for SAMUS CCT Trigger
      IF (FIRST) THEN
        FIRST=.FALSE.

        CALL EZPICK('MUSIM_RCP')
        CALL EZERR(IER)     ! Check if error
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' EZPICK ERR','MU_SAM_CCT',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('SAM_MULT_CUT_AX',MULT_CUT(1),IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' SAM_MULT_CUT_AX','MU_SAM_CCT',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('SAM_MULT_CUT_BX',MULT_CUT(2),IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' SAM_MULT_CUT_BX','MU_SAM_CCT',STRING,'F')
          GOTO 999
        ENDIF
C
        CALL EZGET('SAM_MULT_CUT_CX',MULT_CUT(3),IER)
        IF(IER.NE.0) THEN
          CALL EZGET_ERROR_TEXT(IER,STRING)
          CALL ERRMSG(' SAM_MULT_CUT_CX','MU_SAM_CCT',STRING,'F')
          GOTO 999
        ENDIF
        CALL EZGET_i('IPR_SCCT',IPRSCCT,IER)

        CALL EZRSET()

      ENDIF


C.. Filter coarse centroids based on XYU triplets
      DO I=1,3
        IABC(I)=0
        DO II=1,6
          DO III=0,15
            SCC(III,II)=SAMCC(III,II,I)
          ENDDO
        ENDDO

        CALL MU_SAM_CCT_XYU(SCC,MULT_CUT(I),QQ)
        DO II=1,6
          DO III=0,15
            SAMCC_FILT(III,II,I)=QQ(III,II)
          ENDDO
        ENDDO
        DO III=0,15
          IF(QQ(III,1).NE.0) IABC(I)=1
          IF(QQ(III,2).NE.0) IABC(I)=1
        ENDDO
      ENDDO
      IA=IABC(1)
      IB=IABC(2)
      IC=IABC(3)

C.. Fills the input connectors for the road finder stage
      DO I=1,3
        DO II=1,6
          IND=I*2-MOD(II,2)
          DO III=0,15
            IF(II.EQ.1.OR.II.EQ.2)
     &        SCC_FILT_X(III,IND)=SAMCC_FILT(III,II,I)
            IF(II.EQ.3.OR.II.EQ.4)
     &        SCC_FILT_Y(III,IND)=SAMCC_FILT(III,II,I)
            IF(II.EQ.5.OR.II.EQ.6)
     &        SCC_FILT_U(III,IND)=SAMCC_FILT(III,II,I)
          ENDDO
        ENDDO
      ENDDO

C.. Actually the x-plane connectors in B-stations are exchanged in hardware,
C   because these stations are flipped with respect to A and C.  Note that this
C   flip does not affect most of the Y-centroids, and the U-centroids are not
C   in the trigger.
      I=2           ! B-stations
      DO III=0,15
        SCC_FILT_X(III,4)=SAMCC_FILT(III,1,I)   ! X1 from B goes to STC's Y2
        SCC_FILT_X(III,3)=SAMCC_FILT(III,2,I)   ! X2 from B goes to STC's Y1
      ENDDO

C.. Calculates the XXX roads
      XXXFLG=0
      CALL MU_SAM_CCT_XXX(SCC_FILT_X,QQ)
      DO III=0,15
        DO II=1,2
          XXX(III,II)=QQ(III,II)
          IF(XXX(III,II).NE.0) XXXFLG=1
        ENDDO
      ENDDO
      IF(IPRSCCT) THEN
        PRINT *,'*** Looking for an XXX road ***'
        CALL DUMPV(SCC_FILT_X,6,16,'SCC :',.TRUE.,0)
        CALL DUMPV(XXX,2,16,'XXX :',.TRUE.,0)
      ENDIF


C.. Calculates the YYY roads
      YYYFLG=0
      CALL MU_SAM_CCT_YYY(SCC_FILT_Y,QQ)
      DO III=0,15
        DO II=1,2
          YYY(III,II)=QQ(III,II)
          IF(YYY(III,II).NE.0) YYYFLG=1
        ENDDO
      ENDDO
      IF(IPRSCCT) THEN
        PRINT *,'*** Looking for an YYY road ***'
        CALL DUMPV(SCC_FILT_Y,6,16,'SCC :',.TRUE.,0)
        CALL DUMPV(YYY,2,16,'YYY :',.TRUE.,0)
      ENDIF

C.. Calculates the UUU roads
      UUUFLG=0
      CALL MU_SAM_CCT_UUU(SCC_FILT_U,QQ)
      DO III=0,15
        DO II=1,2
          UUU(III,II)=QQ(III,II)
          IF(UUU(III,II).NE.0) UUUFLG=1
        ENDDO
      ENDDO
      IF(IPRSCCT) THEN
        PRINT *,'*** Looking for an UUU road ***'
        CALL DUMPV(SCC_FILT_U,6,16,'SCC :',.TRUE.,0)
        CALL DUMPV(UUU,2,16,'UUU :',.TRUE.,0)
      ENDIF

C.. Fill TRIG_OR output bits from Triplet finders
      IF(IABC(1).NE.0) SAMBITS(12)=.TRUE.
      IF(IABC(2).NE.0) SAMBITS(13)=.TRUE.
      IF(IABC(3).NE.0) SAMBITS(14)=.TRUE.
C.. Fill TRIG_OR output bits from Road finders
      IF(XXXFLG.NE.0) SAMBITS(15)=.TRUE.
      IF(YYYFLG.NE.0) SAMBITS(16)=.TRUE.
      IF(UUUFLG.NE.0) SAMBITS(17)=.TRUE.

C.. Fill roads statistics histogram
      IF(IREG.EQ.6) THEN
        IF(XXXFLG.NE.0) CALL HFILL(659,1.,0.,1.)
        IF(YYYFLG.NE.0) CALL HFILL(659,2.,0.,1.)
        IF(UUUFLG.NE.0) CALL HFILL(659,3.,0.,1.)
        IF(XXXFLG*YYYFLG*UUUFLG .NE.0 ) CALL HFILL(659,4.,0.,1.)
      ENDIF
      IF(IREG.EQ.7) THEN
        IF(XXXFLG.NE.0) CALL HFILL(659,6.,0.,1.)
        IF(YYYFLG.NE.0) CALL HFILL(659,7.,0.,1.)
        IF(UUUFLG.NE.0) CALL HFILL(659,8.,0.,1.)
        IF(XXXFLG*YYYFLG*UUUFLG .NE.0 ) CALL HFILL(659,9.,0.,1.)
      ENDIF


C.. Fills the input connectors for the last stage in pure SAMUS logic
      DO II=1,2
        DO III=0,15
          SCC(III,II)=XXX(III,II)
          SCC(III,II+2)=YYY(III,II)
          SCC(III,II+4)=UUU(III,II)
        ENDDO
      ENDDO

C.. Find candidate triggers
      CALL MU_SAM_CCT_SUM(SCC,SAMBITS)
      IF(IPRSCCT) THEN
        PRINT *,'*** Looking for a muon candidate ***'
        CALL DUMPV(SCC,6,16,'SCC :',.TRUE.,0)
        do ii=0, 31
          sambits_i(ii)=sambits(ii)
        enddo
        CALL DUMPV(SAMBITS_i,1,12,'SAM_OUT:',.TRUE.,1)
      ENDIF
      SAMTRIG=0
      IF(SAMBITS(0).OR.SAMBITS(1)) THEN
        SAMTRIG=1
        SAMBITS(20)=.TRUE.
      ENDIF

C----------------------------------------------------------------------
  999 RETURN
      END
