      SUBROUTINE UDST_GET_JETS(LJETS,XDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return jet parameters
C-
C-   Inputs  : LJETS     - pointer to JETS bank
C-   Outputs : XDATA     - jet variables
C-
C-   Updated  29-SEP-1992   Ulrich Heintz
C-   Updated  14-AUG-1993   Ulrich Heintz - added px,py,pz,link to matched EM
C-   Updated  14-DEC-1993   Ian Adam  - Add NT90, check on divisor nonzero
C-   Updated  17-DEC-1993   Ian Adam  - drop PX,PY,PZ, add PT, drop corrected
C-    jet energy
C-   Updated  21-JAN-1994   Ian Adam  - add another link to PELC/PPHO and use
C-    UDST_GET_PELC_LINK to get links stored in JETS bank
C-   Updated  11-FEB-1994   Ulrich Heintz - add words 15 and 27
C-   Updated  20-OCT-1995   Ian Adam  Add 0.7 forward improved and D0 angles
C-                            and new words for v5 udst
C-   Updated  28-NOV-1995   Ulrich Heintz - add jet py,pz
C-   Updated  28-NOV-1995   Ulrich Heintz - add another two PELC/PPHO links
C---------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LJETS,I,UDST_PELC_LINK,KJET,KJET1,INDX,ICONE,ID_JETS
      INTEGER BANK_VERSION
      PARAMETER( KJET = 34 )
      REAL    XDATA(KJET),XX(KJET),P4(4),VARP3(3),DPIDPJ(3),R(2)

C... NUMBER OF ALGORITHMS STORED IS 7; JET_TAGS=TAGS//JETC

      CHARACTER*8 JETS_TAGS(KJET)
      CHARACTER*2 JETC(7)
      DATA        JETC/'J7','J5','J3','J1','FI','DA','NN'/
      CHARACTER*4 TAGS(KJET)
      DATA TAGS/
     &  'PT'     ,'E'      ,'ET'    ,'PHI'   ,'ETA'   ,'RMSE'
     & ,'RMSP'   ,'FEM'    ,'NCEL'  ,'FICD'  ,'FCH'   ,'RHOT'
     & ,'LEM1'   ,'LEM2'   ,'NT90'  ,'VARE'  ,'MSPL'  ,'TRK_'
     & ,'TRK'    ,'ETNO'   ,'ETUE'  ,'ETSD'  ,'ETUI'  ,'ETPR'
     & ,'PHNC'   ,'ETNI'   ,'NCEM'  ,'NCCH'  ,'NCFH'  ,'NCIC'
     & ,'PY'     ,'PZ'     ,'LEM3'  ,'LEM4'/
      REAL
     &  PT     ,E      ,ET    ,PHI   ,ETA   ,RMSE
     & ,RMSP   ,FEM    ,NCEL  ,FICD  ,FCH   ,RHOT
     & ,LEM1   ,LEM2   ,NT90  ,VARE  ,MSPL  ,TRK_
     & ,TRK    ,ETNO   ,ETUE  ,ETSD  ,ETUI  ,ETPR
     & ,PHNC   ,ETNI   ,NCEM  ,NCCH  ,NCFH  ,NCIC
     & ,PY     ,PZ     ,LEM3  ,LEM4
      COMMON/JETS_OBJECT/
     &  PT     ,E      ,ET    ,PHI   ,ETA   ,RMSE
     & ,RMSP   ,FEM    ,NCEL  ,FICD  ,FCH   ,RHOT
     & ,LEM1   ,LEM2   ,NT90  ,VARE  ,MSPL  ,TRK_
     & ,TRK    ,ETNO   ,ETUE  ,ETSD  ,ETUI  ,ETPR
     & ,PHNC   ,ETNI   ,NCEM  ,NCCH  ,NCFH  ,NCIC
     & ,PY     ,PZ     ,LEM3  ,LEM4
      EQUIVALENCE (XX,PT)
C---------------------------------------------------------------------
      CALL VZERO(XX,KJET)

      IF (LJETS.LE.0) THEN
        CALL ERRMSG('NO JETS','UDST_GET_JETS',' ','W')
        GOTO 999
      ENDIF

      BANK_VERSION = IQ(LJETS+1)

      PT   = Q(LJETS+2)
      PY   = Q(LJETS+3)
      PZ   = Q(LJETS+4)
      E    = Q(LJETS+5)
      ET   = Q(LJETS+6)
      PHI  = Q(LJETS+8)
      ETA  = Q(LJETS+9)
      RMSE = Q(LJETS+12)
      RMSP = Q(LJETS+13)
      FEM  = Q(LJETS+14)
      MSPL = FLOAT(IQ(LJETS+15))
      NCEL = FLOAT(IQ(LJETS+16))
      FICD = Q(LJETS+17)
      FCH  = Q(LJETS+18)
      RHOT = Q(LJETS+19)
      NT90 = FLOAT(IQ(LJETS+21))

C... links to PELC/PPHO banks

      CALL UDST_GET_PELC_LINK(LJETS,1,UDST_PELC_LINK)
      LEM1 = FLOAT(UDST_PELC_LINK)
      CALL UDST_GET_PELC_LINK(LJETS,2,UDST_PELC_LINK)
      LEM2 = FLOAT(UDST_PELC_LINK)
      CALL UDST_GET_PELC_LINK(LJETS,3,UDST_PELC_LINK)
      LEM3 = FLOAT(UDST_PELC_LINK)
      CALL UDST_GET_PELC_LINK(LJETS,4,UDST_PELC_LINK)
      LEM4 = FLOAT(UDST_PELC_LINK)

C... energy variance

      DO I=1,3
        P4(I)=Q(LJETS+I+1)
        DPIDPJ(I)=Q(LJETS+22+I)
      ENDDO
      P4(4)=Q(LJETS+5)
      VARP3(1)=Q(LJETS+10)
      VARP3(2)=Q(LJETS+11)
      VARP3(3)=Q(LJETS+22)
      CALL GET_E_VARIANCE(P4,VARP3,DPIDPJ,VARE)

C... track information

      IF(IQ(LJETS-1).GE.27)THEN
        CALL SPLIT_BITMASK(IQ(LJETS+27),R)
        TRK_ = R(1)
        TRK  = R(2)
      ELSE
        TRK_ = 0.
        TRK  = 0.
      ENDIF

C... stuff for udst v5

      IF (BANK_VERSION.GE.8) THEN
        ETNO     = Q(LJETS + 30)
        ETUE     = Q(LJETS + 31)
        ETSD     = Q(LJETS + 36)
        ETUI     = Q(LJETS + 38)
        ETPR     = Q(LJETS + 39)
        PHNC     = Q(LJETS + 40)
        ETNI     = Q(LJETS + 41)
        NCEM     = FLOAT(IQ(LJETS + 42))
        NCCH     = FLOAT(IQ(LJETS + 43))
        NCFH     = FLOAT(IQ(LJETS + 44))
        NCIC     = FLOAT(IQ(LJETS + 45))
      ENDIF

      DO I=1,KJET
        XDATA(I) = XX(I)
      ENDDO
C----------------------------------------------------------------------
  999 RETURN
C
      ENTRY UDST_JETS_TAGS(ICONE,KJET1,ID_JETS)
      KJET1=KJET

      DO I=1,KJET
        INDX=INDEX(TAGS(I),' ')
        IF(INDX.EQ.0)INDX=5
        IF(TAGS(I)(INDX-1:INDX-1).EQ.'_')THEN
          JETS_TAGS(I)=TAGS(I)(1:INDX-2)//JETC(ICONE)//'_'
        ELSE
          JETS_TAGS(I)=TAGS(I)(1:INDX-1)//JETC(ICONE)
        ENDIF
      ENDDO

      IF (ICONE.EQ.1) THEN
        ID_JETS=5
        CALL UDST_BOOK_GROUP(ID_JETS,'JET7',JETS_TAGS,KJET)
      ELSEIF (ICONE.EQ.2) THEN
        ID_JETS=9
        CALL UDST_BOOK_GROUP(ID_JETS,'JET5',JETS_TAGS,KJET)
      ELSEIF (ICONE.EQ.3) THEN
        ID_JETS=10
        CALL UDST_BOOK_GROUP(ID_JETS,'JET3',JETS_TAGS,KJET)
      ELSEIF (ICONE.EQ.4) THEN
        ID_JETS=16
        CALL UDST_BOOK_GROUP(ID_JETS,'JET1',JETS_TAGS,KJET)
      ELSEIF (ICONE.EQ.5) THEN
        ID_JETS=17
        CALL UDST_BOOK_GROUP(ID_JETS,'JETF',JETS_TAGS,KJET)
      ELSEIF (ICONE.EQ.6) THEN
        ID_JETS=20
        CALL UDST_BOOK_GROUP(ID_JETS,'JETA',JETS_TAGS,KJET)
      ELSEIF (ICONE.EQ.7) THEN
        ID_JETS=6
        CALL UDST_BOOK_GROUP(ID_JETS,'JETN',JETS_TAGS,KJET)
      ENDIF

      RETURN
      END
