      PROGRAM NEURAL_BUILD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build self-contained NN routine using a
C-   given weight file. The only requirement is that one links to the
C-   JETNET3 object library and JNFEED_FORWARD.
C-
C-   Updated   1-FEB-1995   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
C ****  JNREAD Common Blocks
C
      INTEGER MAXV, MAXM, MAXI, MAXO
      PARAMETER(MAXV=2000,MAXM=150000,MAXI=1000,MAXO=1000)
C
      INTEGER MSTJN, MSTJM, MXNDJM
      REAL    PARJN, PARJM, OIN, OUT
      COMMON /JNDAT1/ MSTJN(40),PARJN(40),MSTJM(20),PARJM(20),
     &                OIN(MAXI),OUT(MAXO),MXNDJM
C
      INTEGER IGFN
      REAL    TINV, ETAL, WIDL, SATM
      COMMON /JNDAT2/ TINV(10),IGFN(10),ETAL(10),WIDL(10),SATM(10)
C
      INTEGER NSELF, NTSELF
      REAL    O, A, D, T, DT, W, DW, G, ODW, ODT, ETAV
      COMMON /JNINT1/ O(MAXV),A(MAXV),D(MAXV),T(MAXV),DT(MAXV),
     &                W(MAXM),DW(MAXM),NSELF(MAXM),NTSELF(MAXV),
     &                G(MAXM+MAXV),ODW(MAXM),ODT(MAXV),ETAV(MAXM+MAXV)
C
      INTEGER M, MV0, MM0, NG, NL, IPOTT, ICPON
      REAL    ER1, ER2, SM
      COMMON /JNINT2/ M(0:10),MV0(11),MM0(11),NG(10),NL,IPOTT,
     &                ER1,ER2,SM(10),ICPON
C----------------------------------------------------------------------
      CHARACTER*(*) PROMPT
      PARAMETER( PROMPT = 'Weights FileName: '  )
      INTEGER LUNINP
      PARAMETER( LUNINP = 80)
      INTEGER LUNOUT
      PARAMETER( LUNOUT = 90)
C----------------------------------------------------------------------
      INTEGER I, J, MAXT, MAXW, II, JJ, LL, LNAME, LENSTR(10), NSTR
      INTEGER LENGTH
      LOGICAL OK, ACTIVE
      CHARACTER*8  STR
      CHARACTER*80 FILENAME, LINE, NAME, OUTSTR(10)
      CHARACTER*20 DAY
C----------------------------------------------------------------------
      INTEGER NSHELL
      PARAMETER( NSHELL = 14 )
      INTEGER LSHELL(NSHELL)
      CHARACTER*80 SHELL(NSHELL)
C----------------------------------------------------------------------
      DATA SHELL  /
     &  '      SUBROUTINE PROGRAM(OIN,OUT)'
     &,'      IMPLICIT NONE'
     &,'C-------------------------------------------------------------'
     &,'C-    Compute NN outputs OUT(*) given inputs OIN(*); JETNET V3'
     &,'C-                NB: Must link to the JETNET3 Object Library'
     &,'C-    Created  DD-MMM-YYYY  NEURAL_BUILD V1.0'
     &,'C-------------------------------------------------------------'
     &,'      REAL OIN(*), OUT(*)'
     &,'C-------------------------------------------------------------'
     &,'      INTEGER MAXV, MAXM'
     &,'      PARAMETER (MAXV = XMAXV, MAXM = XMAXM)'
     &,'      INTEGER M(0:10),MV0(11),MM0(11),NG(10),NL'
     &,'      REAL    DTINV,TINV(10),A(MAXV),T(MAXV),W(MAXM),O(MAXM)'
     &,'C-------------------------------------------------------------'
     &/
C----------------------------------------------------------------------
C
C *********************************************************************
C ****  Get weight file name
C *********************************************************************
C
      CALL LIB$GET_FOREIGN(FILENAME,
     &                     PROMPT,
     &                     LENGTH)
      IF ( LENGTH .LE. 0 ) THEN
        STOP 'Bye!'
      ENDIF
C
C ****  Filename and routine name
C
      FILENAME  = FILENAME(1:LENGTH)
      CALL CHOP(FILENAME(1:LENGTH),OUTSTR,LENSTR,NSTR)
      FILENAME = OUTSTR(1)
      IF     ( NSTR .GE. 2 ) THEN
        NAME = OUTSTR(2)
        LNAME= LENSTR(2)
      ELSE
        WRITE(6,'('' Routine Name: ''$)')
        READ (5,'(A)') OUTSTR(2)
        CALL WORD(OUTSTR(2),I,J,LL)
        NAME = OUTSTR(2)(I:J)
        LNAME= LL
      ENDIF
C
C *********************************************************************
C ****  No setting of parameters or switches is needed.
C ****  All information is present in the file NEURAL_WEIGHTS
C *********************************************************************
C
      CALL D0OPEN(LUNINP, FILENAME, 'IF', OK)
      IF ( OK ) THEN
        CALL JNREAD(LUNINP)
        CLOSE(LUNINP)
      ELSE
        STOP ' Cannot open NEURAL_WEIGHTS'
      ENDIF
C
C ****  Get sizes of arrays
C
      MAXT = MV0(NL+1)
      MAXW = MM0(NL+1)
C
C *********************************************************************
C ****  Write out data statements
C *********************************************************************
C
      CALL D0OPEN(LUNOUT,'DATA.TXT','OFL',OK)
      IF ( .NOT. OK ) THEN
        STOP 'Cannot open Output. Bye!'
      ENDIF
C
C ****  Write out data statements
C
      WRITE(LUNOUT,'(''      DATA NL     /'',I11)') NL
      WRITE(LUNOUT,'(''     &            /'')')
C
      WRITE(LUNOUT,'(''      DATA DTINV  /'',E11.5)') PARJN(3)
      WRITE(LUNOUT,'(''     &            /'')')
C
      WRITE(LUNOUT,'(''      DATA M      /'',I11)') M(0)
      WRITE(LUNOUT,'(''     &'',5(A,I11))') (',',M(I),I=1,5)
      WRITE(LUNOUT,'(''     &'',5(A,I11))') (',',M(I),I=6,10)
      WRITE(LUNOUT,'(''     &            /'')')
C
      WRITE(LUNOUT,'(''      DATA MV0    /'',I11)') MV0(1)
      WRITE(LUNOUT,'(''     &'',5(A,I11))') (',',MV0(I),I=2,6)
      WRITE(LUNOUT,'(''     &'',5(A,I11))') (',',MV0(I),I=7,11)
      WRITE(LUNOUT,'(''     &            /'')')
C
      WRITE(LUNOUT,'(''      DATA MM0    /'',I11)') MM0(1)
      WRITE(LUNOUT,'(''     &'',5(A,I11))') (',',MM0(I),I=2,6)
      WRITE(LUNOUT,'(''     &'',5(A,I11))') (',',MM0(I),I=7,11)
      WRITE(LUNOUT,'(''     &            /'')')
C
      WRITE(LUNOUT,'(''      DATA NG     /'',I11)') NG(1)
      WRITE(LUNOUT,'(''     &'',5(A,I11))') (',',NG(I),I=2,6)
      WRITE(LUNOUT,'(''     &'',5(A,I11))') (',',NG(I),I=7,10)
      WRITE(LUNOUT,'(''     &            /'')')
C
      WRITE(LUNOUT,'(''      DATA TINV   /'',E11.5)') TINV(1)
      WRITE(LUNOUT,'(''     &'',5(A,E11.5))') (',',TINV(I),I=2,6)
      WRITE(LUNOUT,'(''     &'',5(A,E11.5))') (',',TINV(I),I=7,10)
      WRITE(LUNOUT,'(''     &            /'')')
C
      WRITE(LUNOUT,'(''      DATA T      /'',E11.5)') T(1)
      DO J =  2, MAXT, 5
        II = J
        JJ = MIN(J+4,MAXT)
        WRITE(LUNOUT,'(''     &'',5(A,E11.5))') (',',T(I),I=II,JJ)
      ENDDO
      WRITE(LUNOUT,'(''     &            /'')')
C
      WRITE(LUNOUT,'(''      DATA W      /'',E11.5)') W(1)
      DO J =  2, MAXW, 5
        II = J
        JJ = MIN(J+4,MAXW)
        WRITE(LUNOUT,'(''     &'',5(A,E11.5))') (',',W(I),I=II,JJ)
      ENDDO
      WRITE(LUNOUT,'(''     &            /'')')
      CLOSE(LUNOUT)
C
C
C *********************************************************************
C ****  Create routine
C *********************************************************************
C
      CALL UPCASE(NAME(1:LNAME),NAME(1:LNAME))
      LL = INDEX(NAME,'.FOR')
      IF ( LL .LE. 0 ) THEN
        FILENAME = NAME(1:LNAME)//'.FOR'
      ELSE
        FILENAME = NAME(1:LNAME)
        LNAME    = LL-1
        NAME     = NAME(1:LNAME)
      ENDIF
C
      CALL SWAP_TOKEN('PROGRAM',NAME(1:LNAME),NSHELL,SHELL,LSHELL)
C
      CALL CDTIME(DAY)
      DAY = DAY(1:7)//'19'//DAY(8:9)
      CALL SWAP_TOKEN('DD-MMM-YYYY',DAY(1:11),NSHELL,SHELL,LSHELL)
C
      WRITE(STR,'(I8)') MAXT
      CALL WORD(STR,I,J,LL)
      CALL SWAP_TOKEN('XMAXV',STR(I:J),NSHELL,SHELL,LSHELL)
C
      WRITE(STR,'(I8)') MAXW
      CALL WORD(STR,I,J,LL)
      CALL SWAP_TOKEN('XMAXM',STR(I:J),NSHELL,SHELL,LSHELL)
C
C ****  Write out routine
C
      CALL D0OPEN(LUNOUT,FILENAME,'OFL',OK)
      IF ( .NOT. OK ) THEN
        STOP 'Cannot open Output. Bye!'
      ENDIF
C
C ****  OPen DATA.TXT
C
      CALL D0OPEN(LUNINP,'DATA.TXT','IF',OK)
      IF ( .NOT. OK ) THEN
        STOP 'Cannot open DATA.TXT. Bye!'
      ENDIF
C
      DO I =  1, 14
        WRITE(LUNOUT,'(A)') SHELL(I)(1:LSHELL(I))
      ENDDO
C
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        READ (UNIT=LUNINP,FMT='(A)',END=800) LINE
        CALL SWORDS(LINE,II,LL,JJ)
        WRITE(LUNOUT,'(A)') LINE(1:LL)
      ENDDO
C
  800 CONTINUE
      WRITE(LUNOUT,'(A)')
     &'C-------------------------------------------------------------'
      WRITE(LUNOUT,'(A)')
     &'      '//
     &'CALL JNFEED_FORWARD(M,MV0,MM0,NG,NL,DTINV,TINV,T,W,A,O,OIN,OUT)'
      WRITE(LUNOUT,'(A)') ' 999  RETURN'
      WRITE(LUNOUT,'(A)') '      END'
      CLOSE(UNIT=LUNINP)
      CLOSE(UNIT=LUNOUT)
C
C *********************************************************************
      CALL LIB$SPAWN('DELETE/NOCONFIRM/NOLOG DATA.TXT;*')
C
      STOP
      END
