      SUBROUTINE MSGMUH(GOGO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Punch Through Study
C-
C-   Inputs  :   
C-      GOGO    A4    flow control flag.
C                       'INIT', 'UPDT', 'FILL' or 'EVNT'
C-   Outputs :   (banks)
C-   Controls: 
C-
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  -- D0 common blocks...
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C  -- Geant common blocks...
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:GCVOLU.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C  -- variable in argument...
      CHARACTER*4 GOGO
C  -- local variables...
      INTEGER IFIRST,I,J
      INTEGER LGMUH,NDATA
      INTEGER INMVOL,INMCEN,INMCAL,INMMUO
      INTEGER OUTMCXX,OUTMETD,OUTMCAL
      INTEGER HMCEN,HMCAL,HMMUO
      CHARACTER*4 ANAME
      CHARACTER*5 CHTAG(11)
      INTEGER NUMB
      INTEGER NWDS
      PARAMETER (NWDS=12)
      INTEGER MXNTRJ,NNTRJ
      PARAMETER (MXNTRJ=100)
      INTEGER MMMUO,MCFEX,MEFEX
      REAL XYZTRJ(NWDS,MXNTRJ)
      REAL RADIUS,RAD_VERT,ETAD,PHID
      INTEGER ISTAT
C     -- data statements...
      DATA IFIRST/0/
      DATA CHTAG/'X','Y','Z','PX','PY','PZ','P','SLENG','IPART',
     & 'ETAD','PHID'/
C----------------------------------------------------------------------

      IF(GOGO.EQ.'UPDT') THEN
C       -- process only charged particles and skip neutrals ---
        IF(CHARGE.EQ.0.) GO TO 999
C
        RADIUS=VECT(1)**2+VECT(2)**2
        IF(RADIUS.GT.0.) RADIUS=SQRT(RADIUS)
        RAD_VERT=VERT(1)**2+VERT(2)**2
        IF(RAD_VERT.GT.0.) RAD_VERT=SQRT(RAD_VERT)

C       -- leaving beam pipe --
        IF(RAD_VERT.GT.2.7) INMCEN=1
        IF(INMCEN.EQ.0)THEN
          IF(RADIUS.GT.2.7)THEN
            INMCEN=1
            IF(NNTRJ.LT.MXNTRJ) THEN
              NNTRJ=NNTRJ+1
              XYZTRJ(1,NNTRJ)=100000.
              CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
              XYZTRJ(9,NNTRJ)=SLENG
              XYZTRJ(10,NNTRJ)=FLOAT(IPART)
              CALL DET_ETA(VECT,ETAD,PHID)
              XYZTRJ(11,NNTRJ)=ETAD
              XYZTRJ(12,NNTRJ)=PHID
            ENDIF
          ENDIF
        ENDIF

C       -- leaving central tracking detectors --
        IF(RAD_VERT.GT.74.7.OR.
     &      (ABS(VERT(3)).GT.138.5.AND.RAD_VERT.GT.2.7)) INMCAL=1
        IF(INMCAL.EQ.0)THEN
          IF(RADIUS.GT.74.7.OR.
     &      (ABS(VECT(3)).GT.138.5.AND.RADIUS.GT.2.7))THEN
            INMCAL=1
            IF(NNTRJ.LT.MXNTRJ) THEN
              NNTRJ=NNTRJ+1
              XYZTRJ(1,NNTRJ)=200000.
              CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
              XYZTRJ(9,NNTRJ)=SLENG
              XYZTRJ(10,NNTRJ)=FLOAT(IPART)
              CALL DET_ETA(VECT,ETAD,PHID)
              XYZTRJ(11,NNTRJ)=ETAD
              XYZTRJ(12,NNTRJ)=PHID
             ENDIF
          ENDIF
        ENDIF

C       -- leaving calorimeter --
        IF(RAD_VERT.GT.266.8.OR.
     &    (ABS(VERT(3)).GT.398.1.AND.RAD_VERT.GT.2.7)) INMMUO=-1
        IF(INMMUO.EQ.0)THEN
          IF(RADIUS.GT.266.8.OR.
     &      (ABS(VECT(3)).GT.398.1.AND.RADIUS.GT.2.7))THEN
            INMMUO=1
            IF(NNTRJ.LT.MXNTRJ) THEN
              NNTRJ=NNTRJ+1
              XYZTRJ(1,NNTRJ)=300000.
              CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
              XYZTRJ(9,NNTRJ)=SLENG
              XYZTRJ(10,NNTRJ)=FLOAT(IPART)
              CALL DET_ETA(VECT,ETAD,PHID)
              XYZTRJ(11,NNTRJ)=ETAD
              XYZTRJ(12,NNTRJ)=PHID
              CALL HFN(10,XYZTRJ(2,NNTRJ))
              MMMUO=MMMUO+1
            ENDIF
          ENDIF
        ENDIF

C        -- leaving a volume...
C                                        (( inwvol in /GCTRAK/ ))
         IF(INWVOL.EQ.2) THEN
C                                        (( nlevel in /GCVOLU/ ))
         IF(NLEVEL.GE.2) THEN
C
C          -- inside muon mother volume, 'MMUO'...
C
           IF(NAMES(2).EQ.HMMUO) THEN
C             -- check if we are lower level or not...
              IF(NLEVEL.GT.2) THEN
C             -- leaving central toroids...
              CALL MSGNUM(NAMES(NLEVEL),ANAME,NUMB)
              IF(ANAME(1:2).EQ.'MC') THEN
                 IF(OUTMCXX.EQ.0) THEN
                    OUTMCXX=1
                    NNTRJ=NNTRJ+1
                 ENDIF
                 IF(ABS(VECT(1)).GT.410.OR.ABS(VECT(2)).GT.410.OR.
     &              ABS(VECT(3)).GT.370.)THEN
                    IF(NNTRJ.LT.MXNTRJ) THEN
                       XYZTRJ(1,NNTRJ)=410001.
                       CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
                       XYZTRJ(9,NNTRJ)=SLENG
                       XYZTRJ(10,NNTRJ)=FLOAT(IPART)
                       CALL DET_ETA(VECT,ETAD,PHID)
                       XYZTRJ(11,NNTRJ)=ETAD
                       XYZTRJ(12,NNTRJ)=PHID
                       CALL HFN(11,XYZTRJ(2,NNTRJ))
                       MCFEX=MCFEX+1
                    ENDIF
                 ENDIF
              ENDIF
C             -- leaving end toroids...
              IF(ANAME(1:2).EQ.'ME') THEN
                 IF(OUTMETD.EQ.0) THEN
                    OUTMETD=1
                    NNTRJ=NNTRJ+1
                 ENDIF
                 IF(ABS(VECT(1)).GT.410.OR.ABS(VECT(2)).GT.410.OR.
     &              ABS(VECT(3)).GT.550.)THEN
                    IF(NNTRJ.LT.MXNTRJ) THEN
                       XYZTRJ(1,NNTRJ)=420001. 
                       CALL UCOPY(VECT,XYZTRJ(2,NNTRJ),7)
                       XYZTRJ(9,NNTRJ)=SLENG
                       XYZTRJ(10,NNTRJ)=FLOAT(IPART)
                       CALL DET_ETA(VECT,ETAD,PHID)
                       XYZTRJ(11,NNTRJ)=ETAD
                       XYZTRJ(12,NNTRJ)=PHID
                       CALL HFN(12,XYZTRJ(2,NNTRJ))
                       MEFEX=MEFEX+1
                    ENDIF
                 ENDIF
              ENDIF
              ENDIF
            ENDIF       ! ((( if(names(2).eq.hmmuo) )))            
C
         ENDIF          ! ((( if(nlevel.ge.2) )))
         ENDIF          ! ((( if(inwvol.eq.2) )))
         GO TO 999
      ENDIF

      IF(GOGO.EQ.'INIT') THEN
C        -- initialize some variables at the first call of this job...
         IF(IFIRST.EQ.0) THEN
            IFIRST=1
            CALL UCTOH('MCEN',HMCEN,4,4)
            CALL UCTOH('MCAL',HMCAL,4,4)
            CALL UCTOH('MMUO',HMMUO,4,4)
            CALL HBOOK1(5,'MMUO ENTRY MULTI',20,0.,20.,0.)
            CALL HBOOK1(6,'CF EXIT MULTI',20,0.,20.,0.)
            CALL HBOOK1(7,'EF EXIT MULTI',20,0.,20.,0.)
C            CALL HROPEN(1,'NTUPLES','D0GEANT.NTUP','N',1020,ISTAT)
C            CALL HMDIR('NTUPLES','S')
            CALL HBOOKN(10,'MMUO ENTERY',11,' ',1000,CHTAG)
            CALL HBOOKN(11,'CF EXIT',11,' ',1000,CHTAG)
            CALL HBOOKN(12,'EF EXIT',11,' ',1000,CHTAG)
         ENDIF
C
C        -- initialize at the beginning of each track...
C
         INMVOL=0
         INMCEN=0
         INMCAL=0
         INMMUO=0
         OUTMCXX=0
         OUTMETD=0
C
         NNTRJ=1
C
         GO TO 999
      ENDIF

      IF(GOGO.EQ.'FILL') THEN
C        -- do not fill, if track does not reach to the muon system...
         IF(INMMUO.EQ.1.OR.OUTMCXX.EQ.1.OR.OUTMETD.EQ.1) THEN
C           -- book MTRJ bank...
C           NDATA=5+NNTRJ*NWDS
C           CALL BKGMUH(NDATA,LMTRJ)
C           IQ(LMTRJ+1)=2     ! version number
C           IQ(LMTRJ+2)=NWDS 
C           IQ(LMTRJ+3)=NNTRJ
C           IQ(LMTRJ+4)=0
C           IQ(LMTRJ+5)=0
C           -- add vertex point...
            CALL UCOPY(VERT,XYZTRJ(2,1),3)
            XYZTRJ(1,1)=1.0
            XYZTRJ(8,1)=SQRT(PVERT(1)**2+PVERT(2)**2+PVERT(3)**2)
            XYZTRJ(5,1)=PVERT(1)/XYZTRJ(8,1)
            XYZTRJ(6,1)=PVERT(2)/XYZTRJ(8,1)
            XYZTRJ(7,1)=PVERT(3)/XYZTRJ(8,1)
            XYZTRJ(9,1)=0.0
            XYZTRJ(10,1)=FLOAT(IPART)
              CALL DET_ETA(VECT,ETAD,PHID)
              XYZTRJ(12,1)=PHID
              XYZTRJ(11,1)=ETAD
C
C           CALL UCOPY(XYZTRJ(1,1),Q(LMTRJ+6),NNTRJ*NWDS)
C
C            WRITE(LOUT,60) NNTRJ
C            IF(NNTRJ.NE.0) THEN
C               DO 390 I=1,NNTRJ
C                  WRITE(LOUT,61) I,(XYZTRJ(J,I),J=1,10)
C  390          CONTINUE
C            ENDIF
C  60        FORMAT(///' DEBUG IN MSGMUH...    NNTRJ=',I5)
C  61        FORMAT(5X,I5,3X,F8.0,3X,3F8.2,3X,4F9.3,2X,2F10.1)
         ENDIF
         GOTO 999
      ENDIF

      IF(GOGO.EQ.'EVNT') THEN
         CALL HF1(5,FLOAT(MMMUO),1.)
         CALL HF1(6,FLOAT(MCFEX),1.)
         CALL HF1(7,FLOAT(MEFEX),1.)
         MMMUO=0
         MCFEX=0
         MEFEX=0
      ENDIF

  999 RETURN
      END
