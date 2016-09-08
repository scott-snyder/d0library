      SUBROUTINE PACK_TPRL (LTPRL,VERSION,REAL_WORD,INTEGER_WORD,IER)
C----------------------------------------------------------------------
C-   Purpose and Methods : pack information into TPRL
C-   Inputs  : LTPRL         integer       link to TPRL bank to be packed
C-             VERSION       real          >=2.0
C-             REAL_WORD     real(200)     see below
C-             INTEGER_WORD  integer(200)  see below
C-   Outputs : IER           integer       error code (0=OK)
C-   Created  18-DEC-1992   Alain PLUQUET
C-   Updated  23-JUN-1993   Alain PLUQUET changes bandwidth for cluster
C-                                        energies
C-   Updated   5-JUL-1993   Alain PLUQUET moves documentation to TPRL.ZEB
C-   Updated  15-OCT-1993   Alain PLUQUET changes packing for 512 wires
C-   Updated   4-DEC-1993   A. Zylberstejn
C-   Updated   1-JUN-1994   A. Zylberstejn
C-   Updated   6-NOV-1995   L.T. Goss fixed packing of cluster energies
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER JS,KS,LOUT,ND,TRUNIT,K
      INTEGER RUN1A
      INTEGER LTPRL,IER,I,NA,NC,NCLA,NCLC,LSTART,LNEXT,SIZE
      REAL VERSION
      INCLUDE 'D0$INC:TRD_NWORD.INC'
      LOGICAL DOPRINT,TRD_DO_PRINT,FIRST
      INCLUDE 'D0$INC:WORKSP.INC'
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
C        lout=6
      END IF
      DOPRINT=TRD_DO_PRINT().AND.LOUT.NE.0
      IF(DOPRINT) WRITE(LOUT,2002)IWS(4003)
 2002 FORMAT(' Enter PACK_TPRL track:',I3,/, ' ---------------')
      Q(LTPRL+1)=VERSION
      IF (VERSION.GE.2.0) THEN
        NA=INTEGER_WORD(4)
        NC=INTEGER_WORD(5)
        NCLA=INTEGER_WORD(6)
        NCLC=INTEGER_WORD(7)
C-------------------------------------------------------------------------------
C EPICOR*100. <--> bits 1 to 17            EPICOR in [0000.00,1310.71]
C B*1000 <---> bits 18 to 32               B in [00.000,32.767]
C-------------------------------------------------------------------------------
        IQ(LTPRL+5)=0
        IF(REAL_WORD(1).GT.0.)
     +    CALL SBYT(NINT(REAL_WORD(1)*100.),IQ(LTPRL+5),1,17)
        IF(REAL_WORD(2).GT.0.)
     +    CALL SBYT(NINT(REAL_WORD(2)*1000.),IQ(LTPRL+5),18,15)
C-------------------------------------------------------------------------------
C (3.+Distance of closest anode)*10000 <--> bits 1 to 16 phi in [-3.0000,3.5535]
C |sin(theta)|*10000 track     <--> bits 17 to 32  sin(theta)in [0.0000,6.5535]
C-------------------------------------------------------------------------------
        CALL SBYT(NINT((REAL_WORD(41)+3.)*10000.),IQ(LTPRL+3),1,16)
        CALL SBYT(NINT(REAL_WORD(42)*10000.),IQ(LTPRL+3),17,16)
C-------------------------------------------------------------------------------
C gas correction*1000          <--> bits 1  to 16 correction in [0.000,65.535]
C high voltage correction*1000 <--> bits 17 to 32 correction in [0.000,65.535]
C-------------------------------------------------------------------------------
        CALL PACK_REAL (REAL_WORD,1000.,43,2,2,LTPRL+15,LNEXT)
        ND=IQ(LTPRL-1)
        SIZE=LNEXT-LTPRL+1
        IF(SIZE.GT.ND)THEN
          IF(DOPRINT)
     +      WRITE(LOUT,*)'  before push1 SIZE',SIZE,' LNEXT',LNEXT,
     +      'LTPRL',LTPRL,' nd',ND
          CALL MZPUSH(IXMAIN,LTPRL,0,-(ND-SIZE),'I')
          ND=IQ(LTPRL-1)
        END IF
C-------------------------------------------------------------------------------
C HV ANODE     for 1st hit wire <--> bits 1 to 11     HV in [0000.,2047.]
C HV WINDOW    for 1st hit wire <--> bits 12 to 22    HV in [0000.,2047.]
C HV POTENTIAL for 1st hit wire <--> bits 23 to 32    HV in [0000.,1023.]
C etc for 2nd,3rd,4th hit wire
C-------------------------------------------------------------------------------
        CALL SBYT(NINT(REAL_WORD(3)),IQ(LTPRL+6),1,11)
        CALL SBYT(NINT(-REAL_WORD(4)),IQ(LTPRL+6),12,11)
        CALL SBYT(NINT(REAL_WORD(5)),IQ(LTPRL+6),23,10)
        CALL SBYT(NINT(REAL_WORD(6)),IQ(LTPRL+7),1,11)
        CALL SBYT(NINT(-REAL_WORD(7)),IQ(LTPRL+7),12,11)
        CALL SBYT(NINT(REAL_WORD(8)),IQ(LTPRL+7),23,10)
        CALL SBYT(NINT(REAL_WORD(9)),IQ(LTPRL+8),1,11)
        CALL SBYT(NINT(-REAL_WORD(10)),IQ(LTPRL+8),12,11)
        CALL SBYT(NINT(REAL_WORD(11)),IQ(LTPRL+8),23,10)
        CALL SBYT(NINT(REAL_WORD(12)),IQ(LTPRL+9),1,11)
        CALL SBYT(NINT(-REAL_WORD(13)),IQ(LTPRL+9),12,11)
        CALL SBYT(NINT(REAL_WORD(14)),IQ(LTPRL+9),23,10)
C-------------------------------------------------------------------------------
C PEDESTAL*1000. 1st hit wire <--> bits 1  to 16    pedestal in [00.000,65.535]
C PEDESTAL*1000. 2nd hit wire <--> bits 17 to 32    pedestal in [00.000,65.535]
C PEDESTAL*1000. 3rd hit wire <--> bits 1  to 16    pedestal in [00.000,65.535]
C PEDESTAL*1000. 4th hit wire <--> bits 17 to 32    pedestal in [00.000,65.535]
C-------------------------------------------------------------------------------
        CALL PACK_REAL (REAL_WORD,1000.,15,4,2,LTPRL+10,LNEXT)
C-------------------------------------------------------------------------------
C ENERGY*100 on adjacent wire inf <--> bits 1  to 16  energy in [00.00,655.35]
C ENERGY*100 on adjacent wire sup <--> bits 17 to 32  energy in [00.00,655.35]
C-------------------------------------------------------------------------------
        CALL PACK_REAL (REAL_WORD,100.,19,2,2,LTPRL+14,LNEXT)
C-------------------------------------------------------------------------------
C coded word for hit anode cells   <--->  bits 1  to 8       word in [0,255]
C coded word for hit cathode cells <--->  bits 9  to 16      word in [0,255]
C-------------------------------------------------------------------------------
        CALL PACK_INTEGER (INTEGER_WORD,1,2,4,LTPRL+16,LNEXT)
C-------------------------------------------------------------------------------
C SECTOR correction 1st hit wire*100<--> bits 1 to 8   correction in [0.00,2.55]
C SECTOR correction 2nd hit wire*100<--> bits 9 to 16  correction in [0.00,2.55]
C SECTOR correction 3rd hit wire*100<--> bits 17 to 24 correction in [0.00,2.55]
C SECTOR correction 4th hit wire*100<--> bits 25 to 32 correction in [0.00,2.55]
C-------------------------------------------------------------------------------
        CALL PACK_REAL (REAL_WORD,100.,21,4,4,LTPRL+18,LNEXT)
C-------------------------------------------------------------------------------
C WIRE correction 1st hit wire*100 <--> bits 1 to 8    correction in [0.00,2.55]
C WIRE correction 2nd hit wire*100 <--> bits 9 to 16   correction in [0.00,2.55]
C WIRE correction 3rd hit wire*100 <--> bits 17 to 24  correction in [0.00,2.55]
C WIRE correction 4th hit wire*100 <--> bits 25 to 32  correction in [0.00,2.55]
C-------------------------------------------------------------------------------
        CALL PACK_REAL (REAL_WORD,100.,25,4,4,LTPRL+19,LNEXT)
C-------------------------------------------------------------------------------
C electronic gain 1st hit wire*100 <--> bits 1 to 8    correction in [0.00,2.55]
C electronic gain 2nd hit wire*100 <--> bits 9 to 16   correction in [0.00,2.55]
C electronic gain 3rd hit wire*100 <--> bits 17 to 24  correction in [0.00,2.55]
C electronic gain 4th hit wire*100 <--> bits 25 to 32  correction in [0.00,2.55]
C-------------------------------------------------------------------------------
        CALL PACK_REAL (REAL_WORD,100.,29,4,4,LTPRL+20,LNEXT)
C-------------------------------------------------------------------------------
C multiplicity (1=1track) for 1st hit anode <--> bits 1 to 8
C multiplicity (1=1track) for 2nd hit anode <--> bits 9 to 16
C multiplicity (1=1track) for 3rd hit anode <--> bits 17 to 24
C multiplicity (1=1track) for 4th hit anode <--> bits 25 to 32
C-------------------------------------------------------------------------------
        CALL PACK_INTEGER (INTEGER_WORD,8,4,4,LTPRL+17,LNEXT)
C-------------------------------------------------------------------------------
C Number of hit anodes NA  <--> bits 1 to 8
C Number of hit cathodes NC <--> bits 9 to 16
C Number of reconstructed anode clusters   NCLA  <--> bits 17 to 24
C Number of reconstructed cathode clusters NCLC  <--> bits 25 to 32
C-------------------------------------------------------------------------------
        LSTART=LTPRL+25
        CALL PACK_INTEGER (INTEGER_WORD,4,4,4,LSTART,LNEXT)
        IF(DOPRINT)
     +WRITE(LOUT,*)' na,nc.ncla,nclc',NA,NC,NCLA,NCLC,
     &    'iq(ltprl+25)',IQ(LTPRL+25),' lstart',LSTART,' lnext',
     &    LNEXT
C-------------------------------------------------------------------------------
C Hit anode-1 number packed 4 by 4               number in   [1,256]
C Energy*100. hit anode packed 2 by 2            energy in   [00.00,655.35]
C Hit cathode number-1 packed 4 by 4             number in   [1,256]
C Energy*100. hit cathode packed 2 by 2          energy in   [00.00,655.35]
C Energy*10000. for 1st anode cluster            energy in   [00.00,655.35]
C Position for 1st anode cluster                 position in [0,255]
C Energy*10000. for 1st cathode cluster          energy in   [00.00,655.35]
C Position for 1st cathode cluster               position in [0,255]
C-------------------------------------------------------------------------------
C        DO I=51,50+NA
C          INTEGER_WORD(I)=INTEGER_WORD(I)-1
C        ENDDO
C        DO I=51+NA,50+NA+NC
C          INTEGER_WORD(I)=INTEGER_WORD(I)-1
C        ENDDO
        LSTART=LNEXT
        SIZE=LNEXT-LTPRL+1
        IF(SIZE.GT.ND)THEN
          IF(DOPRINT)
     +      WRITE(LOUT,*)'  before push3 SIZE',SIZE,' LNEXT',LNEXT,
     +      'LTPRL',LTPRL,' nd',ND
          CALL MZPUSH(IXMAIN,LTPRL,0,SIZE-ND,'I')
          ND=IQ(LTPRL-1)
        END IF
        IF (VERSION.LT.3.) THEN
          CALL PACK_INTEGER (INTEGER_WORD,51,NA,4,LSTART,LNEXT)
        ELSE
          K=2  ! nb. of packed words  in the bank
          IF(RUN1A().ne.0)K=4
          CALL PACK_INTEGER (INTEGER_WORD,51,NA,K,LSTART,LNEXT)! wires nb
        ENDIF
        CALL PACK_REAL (REAL_WORD,100.,51,NA,2,LNEXT,LNEXT)!E anodes
        IF(NC.NE.0)THEN
          CALL PACK_INTEGER (INTEGER_WORD,51+NA,NC,4,LNEXT,LNEXT)
          CALL PACK_REAL (REAL_WORD,100.,51+NA,NC,2,LNEXT,LNEXT)!E cath.
        END IF
C  fill banks for clusters
C  ----------------------
        JS=50+NA+NC
        IF(JS+5*NCLA.GT.NWORD)THEN
          CALL ERRMSG('too many words for TRD info',
     &            'PACK_TPRL',' ','W')
          NCLA=0
        END IF
        SIZE=LNEXT-LTPRL+2*(NCLA+NCLC)+5
        IF(NCLA.NE.0)THEN
          IF(SIZE.GT.ND)THEN
            K=LNEXT-LTPRL
            IF(DOPRINT)
     +        WRITE(LOUT,*)'  before push4 SIZE',SIZE,' LNEXT',LNEXT,
     +        'LTPRL',LTPRL,' nd',ND
            CALL MZPUSH(IXMAIN,LTPRL,0,SIZE-ND,'I')
            ND=IQ(LTPRL-1)
            LNEXT=LTPRL+K
            IF(DOPRINT)
     +        WRITE(LOUT,*)'  After push4, lnext,ltprl',LNEXT,LTPRL
          END IF
C     anodes
C  Fill Eclust cath,Left clust,Center clust,Right clust,Peak value
          DO I=1,NCLA
            JS=JS+1
            IF(DOPRINT)WRITE(LOUT,*)' in pack_tprl cluster nb',I,
     &        'energy',REAL_WORD(JS),' position',INTEGER_WORD(JS),
     &        ' js',JS
            IQ(LNEXT)=NINT(REAL_WORD(JS)*100.)
            LNEXT=LNEXT+1
            CALL PACK_INTEGER(INTEGER_WORD,JS,4,4,LNEXT,LNEXT)!Left clus
            IF(DOPRINT)
     +        WRITE(LOUT,*) 'CLUST anode JS',JS,' left,center,right',
     +        INTEGER_WORD(JS),INTEGER_WORD(JS+1),INTEGER_WORD(JS+2)
            JS=JS+4
          END DO
C     cathodes
          IF(JS+5*NCLC.GT.NWORD)THEN
            CALL ERRMSG('TOO MANY WORDS FOR TRD INFO',
     &            'PACK_TPRL',' ','W')
            NCLC=0
          END IF
          IF(NCLC.NE.0)THEN
C  Eclust cath,Left clust,Center clust,Right clust,Peak value
            SIZE=LNEXT-LTPRL
            DO I=1,NCLC
              JS=JS+1
              IF(DOPRINT)
     +          WRITE(LOUT,4237)I,JS,REAL_WORD(JS),JS+1,
     +          INTEGER_WORD(JS)
 4237         FORMAT(' cluster CATH',I3,' js',I3,' energy',F6.2,
     &          ' js',I3,' LEFT EDGE',I4)
              IQ(LNEXT)=NINT(REAL_WORD(JS)*100.)
              LNEXT=LNEXT+1
C              JS=JS+1
              CALL PACK_INTEGER(INTEGER_WORD,JS,4,4,LNEXT, LNEXT)
              IF(DOPRINT)
     +          WRITE(LOUT,*) 'CLUST CATH. JS',JS,' POSITION= ',
     +          INTEGER_WORD(JS),'LNEXT',LNEXT
              JS=JS+4
            END DO
          END IF
        END IF
      ENDIF
C  pack number of clusters per wire
      LSTART=LNEXT
      JS=JS+1
      CALL    PACK_INTEGER (INTEGER_WORD,JS,MIN0(NA,8),4,
     &    LSTART,LNEXT)
      IF(DOPRINT)
     +        WRITE(LOUT,*)'  JSN',JS,
     +        'NB. OF CLUSTERS ANODE PER CHAN.',
     &        (INTEGER_WORD(I),I=JS,JS+NA-1),'LSTART LNEXT',LSTART,LNEXT
      JS=JS+NA
      LSTART=LNEXT
      CALL    PACK_INTEGER (INTEGER_WORD,JS,MIN0(NC,8),4,
     &    LSTART,LNEXT)
      IF(DOPRINT)
     +        WRITE(LOUT,*)' JS',JS,
     +        'NB. OF CLUSTERS PER CHAN. CATH',
     &        (INTEGER_WORD(I),I=JS,JS+NC-1),'LSTART LNEXT',LSTART,LNEXT
      ND=IQ(LTPRL-1)
      SIZE=LNEXT-LTPRL+1
      IF(SIZE.LT.ND-5)THEN
        IF(DOPRINT)
     +    WRITE(LOUT,*)'  avant push final SIZE',SIZE,' LNEXT',LNEXT,
     +    'LTPRL',LTPRL,' nd',ND
        CALL MZPUSH(IXMAIN,LTPRL,0,-(ND-SIZE),'I')
C        ND=IQ(LTPRL-1)
      END IF
      IF(DOPRINT) WRITE(LOUT,2003)
 2003 FORMAT(' EXIT     PACK_TPRL ',/, ' ---------------')
      RETURN
      END
