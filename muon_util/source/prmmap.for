C DEC/CMS REPLACEMENT HISTORY, Element PRMMAP.FOR
C *1    28-JAN-1988 12:38:34 HEDIN "prints mmap"
C DEC/CMS REPLACEMENT HISTORY, Element PRMMAP.FOR
      SUBROUTINE PRMMAP(PRUNIT,LMMAPI,NMMAP,CFL,IFL)
C.
C-    PRMMAP  - PRINT MMAP BANK                 NO  1.00 (24/05/87)
C.
C.    INPUT:
C.          PRUNIT - Unit Number for Printout
C.          LMMAPI - Bank Address
C.        * NMMAP  - Bank Number(if 0, all modules)
C.        * CFL    - Flag to control Printout(ONE/ALL/OLD/NEW/CURRENT)
C.        * IFL    - How many data want to print
C.
C.       (*)... Dummy Argument
C.     DH 2/92 ignore all arguements, prints out both MMAP banks
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER PRUNIT,LMMAPI,NMMAP,IFL,I,J,K
      INTEGER LMMAP,LMMAH,LLS,NBNKS,GZMMAP
      INTEGER JP,JPP,NPNTX,NPNTY,IOE,NLINE,ISEC,IL,IXX,JSTA,JEND,JC
      INTEGER NZBANK
      CHARACTER*4 JNAME
      CHARACTER*36 CTITL(20)
      CHARACTER   CFL*(*)
C-
      DATA CTITL /'Type','Status','Quality','Low_Run#','High_Run#'
     6, 'Generated Run','Generated Date','Generated Type of Run'
     9, 'Map Number','Map Type(= dimension)'
     1, 'No. of Points in x Direction','      "       in y Direction'
     3, 'The Origin of x','      "       y'
     5, 'Diff. of x','Diff. of y',3*'Spare'
     +, 'No.of Data Words per Point'/
C-
      DO K=1,2
        LMMAP = GZMMAP(K)
C-
C---  Print First 20 words of MMAP Bank
        CALL UHTOC (IC(LMMAP-4), 4, JNAME, 4)
C-
        WRITE (PRUNIT,1000)
        WRITE (PRUNIT,1005) JNAME
        DO 100 JP=1,10
          JPP = JP + 10
          IF (JP .LE. 2) THEN
            WRITE (PRUNIT,1010) JP, CTITL(JP), IC(LMMAP+JP)
     +,                       JPP,CTITL(JPP),IC(LMMAP+JPP)
          ELSE
            WRITE (PRUNIT,1015) JP, CTITL(JP), IC(LMMAP+JP)
     +,                       JPP,CTITL(JPP), C(LMMAP+JPP)
          ENDIF
  100   CONTINUE
C-
C--- Print Field Map
        NPNTX = IC (LMMAP+11)
        NPNTY = IC (LMMAP+12)
        WRITE (PRUNIT,1020)
C-
        IOE = MOD (NPNTX,4)
        IF (IOE .EQ. 0) THEN
          NLINE = NPNTX/4
        ELSE
          NLINE = NPNTX/4 + 1
        ENDIF
        DO 400 ISEC=1,NPNTY
          WRITE (PRUNIT,1030)
          DO 200 IL=1,NLINE
            IXX  = 4*IL - 3
            JSTA = (8*(IL-1)+1) + (2*NPNTX*(ISEC-1))
            JEND = JSTA + 7
            WRITE (PRUNIT,1025) IXX,ISEC, (C(LMMAP+20+JC),JC=JSTA,JEND)
  200     CONTINUE
  400   CONTINUE
      ENDDO
C-
C---  Format Seq.
 1000 FORMAT(1H1)
 1005 FORMAT(1H0,6X,'==============================',/
     A,          7X,'I  Contents of Bank :  ',A4,'  I',/
     A,          7X,'==============================')
 1010 FORMAT(1X,I3,'.',2X,A36,' :',I10,10X,I3,'.',2X,A36,' :',I10)
 1015 FORMAT(1X,I3,'.',2X,A36,' :',I10,10X,I3,'.',2X,A36,' :',F10.2)
 1020 FORMAT(1H0,3X,'( x, y)',3('/',6X,'Bx',13X,'By',6X),'/',6X,'Bx',
     A  13X,'By'
     A,/,130(1H-))
 1025 FORMAT(4X,'(',I2,',',I2,')',8E15.4)
 1030 FORMAT(1H0)
C-
      RETURN
  888 CONTINUE
      WRITE (PRUNIT,1200)
 1200 FORMAT(1H1,'=== @PRMMAP. NO MMAP BANK !!! ===')
      RETURN
      END
