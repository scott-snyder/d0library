      SUBROUTINE PRHEAD(PRUNIT,LHEADI,NHEAD,CFL,IFL)
C-------------------------------------------------------------------
C-                                                                 -
C-  Print out for HEAD banks                                       -
C-                                                                 -
C-  INPUT:                                                         -
C-  PRUNIT= unit number for printout                               -
C-  NHEAD = 1 for begin-run header                                 -
C-          2 for event header                                     -
C-          3 for end-run header                                   -
C-              OR                                                 -
C-  CFL   = 'EVENT' printout for event record                      -
C-          'BEGIN'    "      "  beginning-of-run record           -
C-          'END'      "      "  end-of-run record                 -
C-  as default it will do printout for event record header         -
C-  LHEADI and IFL ignored                                         -
C-                                                                 -
C-     SDP  Jan,1987                                               -
C-   Updated  25-NOV-1992   Jan S. Hoftun  (Print value of FLAG word) 
C-                                                                 -
C-------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      CHARACTER CFL*(*)
      CHARACTER*24 DSTRNG
      INTEGER PRUNIT,LHEADI,NHEAD,IFL
      INTEGER IREC,K,NFULL,NEMPTY,DUMMY
      CHARACTER*4 BLIST(18),FULL(18),EMPTY(18)
      CHARACTER*80 MSG
      CHARACTER*8 OBJ(10)
      INTEGER N,I,NOBJ(10),ID_OBJ(40)
      REAL    ET(40)
      LOGICAL BITON(40)
      DATA OBJ/'photon','electron','muon','tau','jet .7','MIS. ET',
     &  'SCAL. ET','jet .5','jet .3','jet NN'/
      DATA BLIST/'TRGR','MUD1','CDD1','CDD2','CDD3','CDD4',
     $ 'CAD1','CAD2','FILT','RECO','ANLS','BLNK','HSUM','BLNK',
     $ 'FAKE','GEAN','ISAE','USER'/
C----------------------------------------------------------------------
C
        WRITE(PRUNIT,100)
C
C        begin of run record
      IF(CFL.EQ.'BEGIN'.OR.NHEAD.EQ.1) THEN
        MSG=' PRINTOUT FOR BEGIN-OF-RUN HEAD NOT IMPLEMENTED'
        WRITE(PRUNIT,90) MSG 
        WRITE(PRUNIT,100)
C
C        end of run record
      ELSE IF(CFL.EQ.'END'.OR.NHEAD.EQ.3) THEN
        MSG=' PRINTOUT FOR END-OF-RUN HEAD NOT IMPLEMENTED'
        WRITE(PRUNIT,90) MSG 
        WRITE(PRUNIT,100)
C                         
C       event record
      ELSE
        IF(LHEAD.GT.0) THEN
          IREC=MOD(IQ(LHEAD+1),1000)
C
          IF(IREC.GT.4) THEN
C
C              Print contents of bank
            WRITE(PRUNIT,105) (IQ(LHEAD+K),K=1,3)
            CALL SYS$ASCTIM (DUMMY,DSTRNG,IQ(LHEAD+4),0)
            WRITE(PRUNIT,106) DSTRNG
C                 filter bits
            IF(IQ(LHEAD+14).GT.1) THEN
              WRITE(PRUNIT,107) IQ(LHEAD+6),IQ(LHEAD+8),IQ(LHEAD+7),
     &          (IQ(LHEAD+K),K=9,14)
C                 unpacked reco bits
              WRITE(PRUNIT,108) (IQ(LHEAD+K),K=18,15,-1)
              IF(IQ(LHEAD+14).GT.2) THEN
                CALL UNPACK_RECO_BITS(NOBJ,ID_OBJ,ET,BITON)
                WRITE(PRUNIT,120)
                N=1
                DO I=1,10
                  IF(NOBJ(I).GT.0) WRITE(PRUNIT,121) 
     &              OBJ(I),(ET(K),BITON(K),K=N,N+NOBJ(I)-1)
                  N=N+NOBJ(I)
                ENDDO
              ENDIF
              IF(IQ(LHEAD+14).GT.4) THEN
                WRITE(PRUNIT,130) IQ(LHEAD+29),IQ(LHEAD+30)
              ENDIF
            ELSE
C                   old head format
              WRITE(PRUNIT,109) (IQ(LHEAD+K),K=6,14)
            ENDIF

C
C              give list of empty banks and banks with data
            NEMPTY=0
            NFULL=0
            DO 18 K=1,18
            IF(BLIST(K).NE.'BLNK') THEN
              IF(LQ(LHEAD-K).EQ.0) THEN
                NEMPTY=NEMPTY+1
                EMPTY(NEMPTY)=BLIST(K)
              ELSE
                NFULL=NFULL+1
                FULL(NFULL)=BLIST(K)
              ENDIF
            ENDIF
   18       CONTINUE  
            IF(NEMPTY.GT.0) WRITE(PRUNIT,205) (EMPTY(K),K=1,NEMPTY)
            IF(NFULL.GT.0) WRITE(PRUNIT,305) (FULL(K),K=1,NFULL)
            WRITE(PRUNIT,100)
          ELSE
            WRITE(PRUNIT,94) IQ(LHEAD+1)
            WRITE(PRUNIT,100)
          ENDIF
        ELSE
C
          MSG=' THERE IS NO EVENT RECORD HEAD BANK'
          WRITE(PRUNIT,90) MSG 
          WRITE(PRUNIT,100)
        ENDIF
      ENDIF
      RETURN
   90 FORMAT(A)
   94 FORMAT(//'  Trying to output record type',I5,
     &  ' as an event record')
  100 FORMAT(/2X,50('*')/)
  105 FORMAT(20X,'EVENT RECORD HEAD BANK',/,
     $'    RECORD TYPE=   ',I8,/,
     $'    MICRONAME      ',2A4)
  106 FORMAT ('    DATE and TIME= ',A24)
  107 FORMAT (
     $'    LOCAL RUN#     ',I8,/,
     $'    INPUT EVENT #  ',2I11,' (Hi Order (+8), Low Order (+7)) ',/, 
     $'    OUTPUT EVENT # ',I8,/,
     $'    EVENT TYPE     ',I8,/,
     $'    TRIGGER BITS   ',Z8.8,/,
     $'    GLOBAL RUN#    ',I8,/,
     $'    CODE VERSION   ',I8,/,
     $'    HEADER VERSION ',I8)
  108   FORMAT(
     &'    FILTER BITS    ',4(Z8.8,1X)) 
  109 FORMAT (
     $'    LOCAL RUN#     ',I8,/,
     $'    INPUT EVENT #  ',2I8,/, 
     $'    OUTPUT EVENT # ',I8,/,
     $'    EVENT TYPE     ',I8,/,
     $'    TRIGGER BITS   ',Z8.8,/,
     $'    SPARE TYPE     ',I8,/,
     $'    CODE VERSION   ',I8,/,
     $'    HEADER VERSION ',I8,//)
  120 FORMAT('    UNPACKED RECO BITS '/,6X
     & ,'  OBJECT',3X,4(' ET(GeV) MATCH '))
  121 FORMAT(8X,A8,1X,4(F7.1,4X,L2,2X))
  130 FORMAT(/' Level-2 result code ',I3,'    FLAG word  ',Z8.8,
     &  ' (see EVENT_HEAD.ZEB)')
  200 FORMAT(/)
  205 FORMAT('  Bank ',A4,' is EMPTY')
  305 FORMAT('  Bank ',A4,' has DATA')
      END
