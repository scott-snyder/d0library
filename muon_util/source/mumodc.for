      SUBROUTINE MUMODC(NMOD,VNAME,VPNAME,VCNAME)
C------------------------------------------------------------------
C     S/R MUMODC returns volume name in Geant for muon PDT module, 
C  number NMOD.    The module numbering scheme is defined in D0 
C  note 416 by P.Martin.   
C
C  Input:
C     NMOD      module number in format of 3 digits integer, LMN.
C                   L=0      for A-layer
C                    =1          B-layer
C                    =2 or 3     C-layer
C  Output:
C     VNAME     volume name (four characters)
C                 AMxx, BMxx, CMxx for A-, B-, C-layer, respectively.
C                 xx is the last two digits of module number(=NM).   
C     VPNAME    plane name in module NMOD.  (four characters)
C                 APxx, BPxx, CPxx.
C     VCNAME    cell name in module NMDO.
C                 ACxx, BCxx, CCxx.
C
C  S.Kunori    23Jan87
C-------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMOD
      CHARACTER*4 VNAME,VPNAME,VCNAME
C  -- external function...
      INTEGER GZMGEO
C  -- local variables...
      INTEGER L,M,N
      CHARACTER*4 AN(11)
      DATA AN/'0','1','2','3','4','5','6','7','8','9','A'/
C                   
C  -- check the bank pointer to NMOD PDT constant bnak to
C     see if the module exists...
      IF(GZMGEO(NMOD).EQ.0) THEN
C        -- module dose not exist.
         VNAME=' '
         VPNAME=' '
         VCNAME=' '
         GO TO 900
      ENDIF
      L=NMOD/100
      M=MOD(NMOD/10,10)
      IF(L.GE.3) THEN
         M=10
      ENDIF
      N=MOD(NMOD,10)
C
      IF(L.EQ.0) THEN   
         VNAME(1:2)='AM'
         VNAME(3:3)=AN(M+1)
         VNAME(4:4)=AN(N+1)
      ELSE IF(L.EQ.1) THEN
         VNAME(1:2)='BM'
         VNAME(3:3)=AN(M+1)
         VNAME(4:4)=AN(N+1)
      ELSE IF(L.EQ.2.OR.L.EQ.3) THEN
         VNAME(1:2)='CM'
         VNAME(3:3)=AN(M+1)
         VNAME(4:4)=AN(N+1)
      ELSE             
         WRITE(6,60) NMOD
60       FORMAT('  *** ILLEGAL MUON PDT MODULE NUMBER,',I5
     +,'WAS DETECTED ***'/'     PROGRAM STOPPED IN S/R MUMODC.')
         STOP
      ENDIF
C
      VPNAME=VNAME
      VPNAME(2:2)='P'
C
      VCNAME=VNAME
      VCNAME(2:2)='C'
C
900   CONTINUE
      RETURN
      END
