      SUBROUTINE GTCDTK(ITRK,NVAR,RESULT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack CDTK bank; get information for i'th 
C-                         CDC track
C-
C-   Inputs  : ITRK [I]     index of track in CDTK
C-             NVAR [I]     number of variables per track to unpack
C-           
C-   Outputs : RESULT(10) [R] result vector
C-              
C-             RESULT(1)  theta
C-             RESULT(2)  phi  
C-             RESULT(3)  x0
C-             RESULT(4)  y0
C-             RESULT(5)  z0
C-             RESULT(6)  nhitxy
C-             RESULT(7)  nhitrz 
C-             RESULT(8)  ionization (mip)
C-             RESULT(9)  chisq probability of xy fit
C-             RESULT(10) chisq probability of rz fit
C-             RESULT(11) error on xy impact parameter
C-             RESULT(12) error on z position of track at beam
C-
C-   Controls: NONE
C-
C-   Created   2-OCT-1995   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDTK.LINK'
      INCLUDE 'D0$INC:CDTK_PACKING.INC'

      INTEGER LCDTK,GZCDTK,NTRK,PCKWRD1,PCKWRD2,JBYT
      INTEGER NHITXY,NHITRZ,IMIP,IOVRFLW,ITRK,NVAR
      INTEGER NVARMAX,POINTER,I
      PARAMETER( NVARMAX = 12 )
      REAL    OUT(NVARMAX),TEMP(4),MIP,RESULT(*)
      EXTERNAL  JBYT
C----------------------------------------------------------------------
      CALL VZERO(OUT,NVARMAX)

      LCDTK = GZCDTK()
      IF (LCDTK.LE.0) THEN
        CALL ERRMSG('NO CDTK','GTCDTK','ZERO RESULT','W')
        GOTO 999
      ENDIF

      NTRK = IQ(LCDTK+3)
      IF (ITRK.GT.NTRK) THEN
        CALL ERRMSG('ITRK TOO BIG','GTCDTK','ZERO RESULT','W')
        GOTO 999
      ENDIF

      IF (IQ(LCDTK+2).NE.NREP) THEN
        CALL ERRMSG('INCONSISTENT NREP','GTCDTK',' ','W')
      ENDIF

      POINTER = 3 + NREP*(ITRK-1)

      OUT( 1)   = Q(POINTER+1)
      OUT( 2)   = Q(POINTER+2)
      OUT( 3)   = Q(POINTER+3)
      OUT( 4)   = Q(POINTER+4)
      OUT( 5)   = Q(POINTER+5)
      PCKWRD1   = IQ(POINTER+6)
      PCKWRD2   = IQ(POINTER+7)

      NHITXY  = JBYT(PCKWRD1, 1,   5)
      NHITRZ  = JBYT(PCKWRD1, 6,   4)
      IMIP    = JBYT(PCKWRD1, 10, 22)
      IOVRFLW = JBYT(PCKWRD1, 32,  1)

      MIP = FLOAT(IMIP)*MIP_CUTOFF/4194303.0

      OUT( 6)   = FLOAT(NHITXY)
      OUT( 7)   = FLOAT(NHITRZ)
      OUT( 8)   = MIP

      CALL UDST_CDTK_ERRORS_UNPACK(PCKWRD2,TEMP)
      OUT( 9)   = TEMP(1)
      OUT(10)   = TEMP(2)
      OUT(11)   = TEMP(3)
      OUT(12)   = TEMP(4)

      IF (NVAR.GT.NVARMAX) NVAR = NVARMAX
      DO I=1,NVAR
        RESULT(I) = OUT(I)
      ENDDO

  999 RETURN
      END
