      LOGICAL FUNCTION MUSUMR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : end of job hook for MUON-RECO
C-
C-   Created  10-oct-1991 D. Hedin
C    DH 1/92 USE VERIFY FLAG
C    DH 4/92 add efficiency calculations
C    DH 6/92 more eff like calculation, fix for UNIX
C    DH 1/94 fix EFF indexing problem
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IERR,MUCALIB,SSUNIT,LUN,N1,N2,N3,N4,N5,N6,I,N7,N8,N9,N10
      INTEGER IMIN,ISET1,N11,N12,N13,N14,N15,N16,III,ID,NN(164),MUNMOD2
      INTEGER MOD,JD,KD,J,PREFF,ON(164),OFF(164),NMUD1,NMUOH,NMOD,NACT
      REAL HI,R1,R2,R3,R4,R5,R6,E1(164),E2(164),E3(164),E4(164),E5(165),
     A HSTATI
      INTEGER LD
      LOGICAL FLGVAL
C----------------------------------------------------------------------
C
      MUSUMR=.TRUE.
      CALL DHDIR('MURECO_RCP','HBOOK_DIRECTORY',IERR,' ')
      IF(IERR.NE.0) THEN
        CALL ERRMSG('MURECO','MUSUMR',
     +     'ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
      CALL EZPICK('MURECO_RCP')
CCCCCCCC     CALL CALIB ROUTINES
      CALL EZGET('MUCALIB',MUCALIB,IERR)
      IF(MUCALIB.GE.0) THEN
        CALL MUCALIB_END(MUCALIB)
      ENDIF
C
      LUN=SSUNIT()             ! STANDARD SUMMARY; MUHIST_MINI HISTOS
      CALL EZGET('HIST_MINIMUM',IMIN,IERR)
      IF(IMIN.LT.0.) THEN
        IF(FLGVAL('VERIFY')) IMIN=0
      ENDIF
      IF(IMIN.GE.0) THEN
        CALL HNOENT(IMIN+3,N1)        ! NO. MUOT TRACKS ALL
        CALL HNOENT(IMIN+7,N2)        ! NO. MUOT TRACKS GOOD
        CALL HNOENT(IMIN+13,N5)        ! NO. PMUO TRACKS
        CALL HNOENT(IMIN+15,N7)        ! NO. PMUO TRACKS; GLOBAL FIT
        N3=0
        N4=0
        N6=0
        N8=0
        DO I=3,16
          N3=N3+HI(IMIN+3,I)          ! MUOT ALL THETA>20
          N4=N4+HI(IMIN+7,I)          ! MUOT GOOD THETA>20
          N6=N6+HI(IMIN+13,I)         ! PMUO THETA>20
          N8=N8+HI(IMIN+15,I)         ! PMUO THETA>20; GLOBAL FIT
        ENDDO
        WRITE(LUN,530)
  530   FORMAT(//15X,' MURECO Summary '/15X,'************************')
        WRITE(LUN,531)
  531   FORMAT(/' Muon Tracks',4X,
     &    ' All MUOT   Good MUOT    All PMUO',2X,' PMUO global ')
        WRITE(LUN,532) N1,N2,N5,N7
  532   FORMAT(' All theta       ',4(I8,3X))
        WRITE(LUN,533) N3,N4,N6,N8
  533   FORMAT(' Theta > 20 deg  ',4(I8,3X))
      ENDIF
      CALL EZGET('HIST_SET1',ISET1,IERR)
      IF(ISET1.LT.0.) THEN
        IF(FLGVAL('VERIFY')) ISET1=0
      ENDIF
      IF(ISET1.GE.0) THEN
        N9=0
        N10=0
        DO I=3,20
          N9=N9+HI(ISET1+9,I)
          N10=N10+HI(ISET1+11,I)
        ENDDO
        CALL HNOENT(ISET1+106,N11)        ! NO. MUOT NO ISA
        CALL HNOENT(ISET1+132,N12)        ! NO. PMUO NO ISA
        CALL HNOENT(ISET1+133,N13)        ! NO. PMUO NO CD
        N14=0
        N16=0
        N15=0
        DO I=3,16
          N14=N14+HI(ISET1+106,I)          ! MUOT NO ISA THETA>20
          N15=N15+HI(ISET1+132,I)         ! PMUO NO ISA THETA>20
          N16=N16+HI(ISET1+133,I)         ! PMUO NO CD THETA>20
        ENDDO
        WRITE(LUN,534) N9,N10
  534   FORMAT(' No. events >1 MUOT =',I7,'   >1 PMUO =',I7)
        NMUD1=HSTATI(ISET1+85,1,'HIST',1)
        NMUOH=HSTATI(ISET1+86,1,'HIST',1)
        NMOD=HSTATI(ISET1+87,1,'HIST',1)
        NACT=HSTATI(ISET1+71,1,'HIST',1)
        WRITE(LUN,537) NMUD1,NMUOH,NMOD,NACT
 537    FORMAT(' Average MUD1, MUOH, Modules, A-Ctop/event ',4I6)
        WRITE(LUN,631)
  631   FORMAT(' Muon Tracks',4X,' MUOT no ISA   PMUO no ISA',2X,
     A' PMUO no CD ')
        WRITE(LUN,632) N11,N12,N13
  632   FORMAT(' All theta       ',3(I8,6X))
        WRITE(LUN,633) N14,N15,N16
  633   FORMAT(' Theta > 20 deg  ',3(I8,6X))
        CALL EZGET('PREFF',PREFF,IERR)
        IF(PREFF.GT.0) THEN
CC   DO CHAMBER EFFICIENCIES
          ID=ISET1+25
          R1=HI(ID,1)+HI(ID,2)+HI(ID,3)+HI(ID,4)+HI(ID,5)
          R2=HI(ID,2)+2.*HI(ID,3)+3.*HI(ID,4)+4.*HI(ID,5)
          R3=R2/(R1*4.+.0001)
          R1=HI(ID,6)+HI(ID,7)+HI(ID,8)+HI(ID,9)
          R2=HI(ID,7)+2.*HI(ID,8)+3.*HI(ID,9)
          R4=R2/(R1*3.+.0001)
          R1=HI(ID,11)+HI(ID,12)+HI(ID,13)+HI(ID,14)
          R2=HI(ID,12)+2.*HI(ID,13)+3.*HI(ID,14)
          R5=R2/(R1*3.+.0001)
       WRITE(LUN,*)' Chamber efficiencies   A-layer   B-layer C-layer'
          WRITE(LUN,634) R3,R4,R5
  634     FORMAT(' Drift time            ',3F9.3)
          ID=ISET1+26
          R1=HI(ID,1)+HI(ID,2)+HI(ID,3)+HI(ID,4)+HI(ID,5)
          R2=HI(ID,2)+2.*HI(ID,3)+3.*HI(ID,4)+4.*HI(ID,5)
          R3=R2/(R1*4.+.0001)
          R1=HI(ID,6)+HI(ID,7)+HI(ID,8)+HI(ID,9)
          R2=HI(ID,7)+2.*HI(ID,8)+3.*HI(ID,9)
          R4=R2/(R1*3.+.0001)
          R1=HI(ID,11)+HI(ID,12)+HI(ID,13)+HI(ID,14)
          R2=HI(ID,12)+2.*HI(ID,13)+3.*HI(ID,14)
          R5=R2/(R1*3.+.0001)
          WRITE(LUN,635) R3,R4,R5
  635     FORMAT(' Time division         ',3F9.3)
          ID=ISET1+27
          R1=HI(ID,1)+HI(ID,2)+HI(ID,3)+HI(ID,4)+HI(ID,5)
          R2=HI(ID,2)+2.*HI(ID,3)+3.*HI(ID,4)+4.*HI(ID,5)
          R3=R2/(R1*4.+.0001)
          R1=HI(ID,6)+HI(ID,7)+HI(ID,8)+HI(ID,9)
          R2=HI(ID,7)+2.*HI(ID,8)+3.*HI(ID,9)
          R4=R2/(R1*3.+.0001)
          R1=HI(ID,11)+HI(ID,12)+HI(ID,13)+HI(ID,14)
          R2=HI(ID,12)+2.*HI(ID,13)+3.*HI(ID,14)
          R5=R2/(R1*3.+.0001)
          WRITE(LUN,636) R3,R4,R5
  636     FORMAT(' Vernier pads          ',3F9.3)
          DO I=1,164  
            NN(I)=MUNMOD2(1,I)
            ID=ISET1+400+(I-1)/12
            KD=ISET1+420+(I-1)/12
            LD=ISET1+440+(I-1)/12
            JD=(MOD(I-1,12))*5+1
            IF(NN(I).LE.99) THEN
              R1=4*(HI(ID,JD+1)+HI(ID,JD+2)+HI(ID,JD+3)+HI(ID,JD+4))
              R2=HI(ID,JD+1)+2*HI(ID,JD+2)+3*HI(ID,JD+3)+4*HI(ID,JD+4)
              R3=HI(KD,JD+1)+2*HI(KD,JD+2)+3*HI(KD,JD+3)+4*HI(KD,JD+4)
              R4=HI(LD,JD+1)+2*HI(LD,JD+2)+3*HI(LD,JD+3)+4*HI(LD,JD+4)
            ELSE                                               
              R1=3*(HI(ID,JD+1)+HI(ID,JD+2)+HI(ID,JD+3))
              R2=HI(ID,JD+1)+2*HI(ID,JD+2)+3*HI(ID,JD+3)
              R3=HI(KD,JD+1)+2*HI(KD,JD+2)+3*HI(KD,JD+3)
              R4=HI(LD,JD+1)+2*HI(LD,JD+2)+3*HI(LD,JD+3)
            ENDIF                                
            E1(I)=R2/(R1+.001)
            E2(I)=R3/(R1+.001)
            E3(I)=R4/(R1+.001)
CC   COMPARE RAW,MUOH,T2 HITS
            E4(I)=HI(ISET1+91,I)/(HI(ISET1+90,I)+.001)
            E5(I)=HI(ISET1+92,I)/(HI(ISET1+91,I)+.001)
          ENDDO
          WRITE(LUN,*)
     A'Modul Time dTim Pad Muoh% 2Tim% Modul Time dTim Pad Muoh% 2Tim%'
          DO I=1,82
            WRITE(LUN,638) (NN(I+82*(J-1)),E1(I+82*(J-1))
     A          ,E2(I+82*(J-1)),E3(I+82*(J-1)),E4(I+82*(J-1)),
     A    E5(I+82*(J-1)),J=1,2)
  638     FORMAT(2(I5,1X,5F5.2))
          ENDDO
          CALL HBOOK1(ISET1+514,' TiEFF VS MODULE',164,.5,164.5,0.)
          CALL HBOOK1(ISET1+515,' dTEFF VS MODULE',164,.5,164.5,0.)
          CALL HBOOK1(ISET1+516,' padEFF VS MODULE',164,.5,164.5,0.)
          CALL HBOOK1(ISET1+517,' MUOH/MUD1 VS MODULE',164,.5,164.5,0.)
          CALL HBOOK1(ISET1+518,' 2TIME% VS MODULE',164,.5,164.5,0.)
          DO I=1,164
            CALL HFILL(ISET1+514,FLOAT(I),0.,E1(I))
            CALL HFILL(ISET1+515,FLOAT(I),0.,E2(I))
            CALL HFILL(ISET1+516,FLOAT(I),0.,E3(I))
            CALL HFILL(ISET1+517,FLOAT(I),0.,E4(I))
            CALL HFILL(ISET1+518,FLOAT(I),0.,E5(I))
          ENDDO
          DO I=1,164
            ON(I)=HI(ISET1+416,I)
            OFF(I)=HI(ISET1+417,I)
          ENDDO
        WRITE(LUN,*) '****   Modules either on or missing from tracks '
        WRITE(LUN,*)
     A' Modul  ON  OFF  Modul  ON  OFF  Modul  ON  OFF  Modul  ON  OFF '
        DO I=1,41
          WRITE(LUN,639) (NN(I+41*(J-1)),ON(I+41*(J-1))
     A          ,OFF(I+41*(J-1)),J=1,4)
        ENDDO
  639   FORMAT(4(I5,1X,2I5))
        ENDIF
        CALL HOPERA(ISET1+417,'/',ISET1+416,ISET1+418,1.,1.)
C        DO I=1,14
C          CALL HDELET(ISET1+I+399)
C          CALL HDELET(ISET1+I+419)
C          CALL HDELET(ISET1+I+439)
C        ENDDO
      ENDIF
      CALL EZRSET
  999 RETURN
      END
