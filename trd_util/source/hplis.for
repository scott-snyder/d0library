      SUBROUTINE HPLIS (LOUT,ID0,IPR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print an hitogramme HBOOK dans le format
C-             PLIS (Down the page with percentage of integrated
C-             content over channel_low_edge)
C-
C-   Inputs  : ID: id in HBOOK (ID=0 => Print all one_dim_histo)
C-   Outputs :
C-      PERCEN in common CHPLIS (= Percentage in the last printed histo)
C-      Print on LOUT
C-   Controls: IPR=  0: Percent is not printed
C-                   1: Percent is relative to the total sum
C-                      under and overflows included
C-                   2: percent is relative to the strict histo_content
C-
C-   Created   3-OCT-1989   J.F. DETOEUF ESQ.
C-   Updated   5-OCT-1989   J.F. DETOEUF ESQ.  REAL INTE
C-   Updated  23-SEP-1992   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ID0,IPR
      INTEGER NBCAMA,LOUT
      PARAMETER( NBCAMA=200)
      REAL PERCEN(NBCAMA)
C      COMMON /CHPLIS/PERCEN,LOUT
C
      INTEGER IDVECT(200) ,I,J , IID,ID,NID,IDALL  ,NBENT
      INTEGER NXH,NY,IAD    ,IEXP,KC
      REAL XMI,XMA ,YMI,YMA  ,COEF  ,HI,HSTATI
      REAL OVE,UNDE, CMAX, CMOY, CRMS, CSUM,SUMT
      REAL RAP ,HSUM ,CONT(NBCAMA+2) ,HMAX, SUM(NBCAMA+2)
      REAL XPAS,CPAS ,X,TOT,CONTP
      CHARACTER TITH*80,STR24*24
      LOGICAL HEXIST
C
C      DATA      LOUT /6/
C
C----------------------------------------------------------------------
      ID=ID0
      IDALL=0
      CALL HCDIR('//PAWC/TRD',' ')  ! Go to TRD directory
      IF (ID.EQ.0) THEN !CASE  ID0=0
        CALL HID1(IDVECT,NID)
        IF(NID.LE.0) THEN
          WRITE (LOUT,*) 'HPLIS>> ID=0 ,NO BOOKED HISTO '
          RETURN
        ENDIF
      ELSE
        NID=1
        IDVECT(1)=ID0
      END IF
      DO 998 IID= 1,NID ! Loop on ID
        ID=IDVECT(IID)
        IF (.NOT. HEXIST(ID)) THEN
          WRITE(LOUT,*) 'HPLIS > Non existent ID : ',ID
          GOTO 998
        ENDIF
        CALL HNOENT(ID,NBENT)
        IF (NBENT.EQ.0) THEN
          WRITE(LOUT,*) 'HPLIS > Empty ID : ',ID
          GO TO 998
        ENDIF
        CALL HGIVE(ID,TITH,NXH,XMI,XMA,NY,YMI,YMA,IAD)! GET Hbook_data
        CALL HUNPAK(ID,CONT,'HIST',1)
        UNDE= HI(ID,0)
        OVE= HI(ID,NXH+1)
        CMAX= HMAX(ID)
        CMOY= HSTATI(ID,1,'HIST',1)
        CRMS= HSTATI(ID,2,'HIST',1)
        CSUM= HSUM(ID)
        SUMT= CSUM+UNDE+OVE
C Print Header                                          --  PRINT HEADER
        IEXP=1
        IF(CMAX.GT.9999)IEXP= ALOG10(CMAX)
        WRITE(LOUT,5000) ID, TITH
 5000   FORMAT('11 HISTOGRAMME ',I4,':',A80,/,' -----------')
        IF (SUMT.EQ.0) THEN
          WRITE(LOUT,'(/5X,A50)')
     +           'The TOTAL SUM IS 0 ,NO PERCENTAGE will be given',' '
          IPR= -IPR
        ENDIF
 1000   FORMAT (1X,A4,2X,A8,4X          ,A7,2X    ,74X     ,A12
     +     /19X                   ,A6,I3    ,74X     ,A12)
        IF(IPR.GT.0)THEN
          STR24='Percent over low_edge  '
        ELSE
          STR24=' '
        ENDIF
        WRITE(LOUT,*)
C Init loop                                               --- INIT LOOP
        COEF=10.**(IEXP-1)
        XPAS= (XMA-XMI)/NXH
        CPAS= CMAX/69
        SUM(1)= UNDE
        IF(IPR.EQ.2) SUM(1)=0
C Loop on lines                                        --- LOOP ON LINES
        DO 20 I =1,NXH
          SUM(I+1)=SUM(I)+CONT(I)
          X=XMI+(I-1)*XPAS
          KC= CONT(I)/CPAS +1
          TOT= SUMT
          IF(IPR.EQ.2) TOT= CSUM
          RAP=0.
          IF(TOT.NE.0) RAP= 100.*(1.-SUM(I)/TOT)
          PERCEN(I)= RAP
          CONTP= CONT(I)/COEF
C Print line                                              -- PRINT LINE
          IF (IPR.LE.0)
     +      WRITE(LOUT,'(1X,I4,G12.4,F8.0,2X ,72A1)')
     +      I  ,X   ,CONTP  ,('X',J=1,KC),' ',('.',J=KC+2,72)
          IF(IPR.GE.1)
     +      WRITE(LOUT,'(1X,I4,G12.4,F8.0,2X,72A1,2X,G12.4,A1)')
     +      I  ,X   ,CONTP  ,('X',J=1,KC),' ',('.',J=KC+2,72)
     +      , RAP, '|'
   20   CONTINUE
        WRITE(LOUT,1000)
     +     'Chan'  ,'Low_edge'   ,'Content'       ,STR24(:12)
     +     , '* 10**',IEXP-1    ,STR24(13:)
        IF(IPR.EQ.2) THEN
          WRITE(LOUT,'(98X,A20)') '(sides not included)'
        ELSE
          WRITE(LOUT,'(100X,A16)') '(sides included)'
        ENDIF
C Write summary                                        --- PRINT SUMMARY
        WRITE (LOUT,
     +       '(/(1X,A21,G12.4,A12,G12.4,A12,G12.4))')
     +       'IN histogram=',CSUM,'Mean=',CMOY,'R.m.s.=',CRMS
     +       ,'TOTAL(with outside)=',SUMT,'Underflow=',UNDE,'Overflow=',
     +       OVE
        WRITE (LOUT,*) 'Horizontal step in plot: ',CPAS
  998 CONTINUE   !To next id
      IF(IPR.LT.0) IPR= -IPR
  999 RETURN
      END
