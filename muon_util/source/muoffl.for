      SUBROUTINE MUOFFL(IGO,MODNUM,NHIT,NLOC)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Fill MUOF bank for one module
C-
C-   Inputs  :  IGO    - control flag
C-                       IGO=1   initialize/book bank
C-                          =2   fill raw information (MUD1)
C-                          =3   fill processed information (MUOH)
C-                          =4   fill pointer information (MUHP)
C-                          =99  compress bank
C-              MODNUM - Module id
C-              NHIT   - Number of hits
C-              NLOC   - Associated location MUD1/MUOH/MUHP
C-
C-    Output :  D0 Zebra output bank.   (MUOF)
C-
C-    Created :  26-AUG-93  M. Fortner
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C                              
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
C
      INTEGER IGO,MODNUM,NHIT,NLOC
C
C  -- local variables.         
      INTEGER LMUOF,IMUOF,NWD,IHIT,IPLN
      INTEGER NMUOF,JMUOF,LMUHT
      INTEGER  GZMUOF
      EXTERNAL GZMUOF
C
C                Initialize and book bank if needed
C
      IF (IGO.EQ.1) THEN
          LMUOF = GZMUOF(0)
          IF (LMUOF.EQ.0) THEN
              CALL BKMUOF(0,0,LMUOF)
          ENDIF
          LMUHT = LQ(LMUOF+1)
C
C                Compress bank
C
      ELSE IF (IGO.EQ.99) THEN
          LMUOF = GZMUOF(0)
          IF (LMUOF.EQ.0) RETURN
          LMUHT = LQ(LMUOF+1)
          NMUOF = IQ(LMUHT+8)
          NWD = NMUOF*10 - IQ(LMUOF-1)
          CALL MZPUSH(IXCOM,LMUOF,0,NWD,'I')
C
C                Get current pointers
C
      ELSE
          LMUOF = GZMUOF(0)
          IF (MODNUM.LE.0.OR.MODNUM.GT.460.OR.LMUOF.EQ.0) RETURN
          LMUHT = LQ(LMUOF+1)
          NMUOF = IQ(LMUHT+8)
          JMUOF = IQ(LMUHT+10+MODNUM)
          IMUOF = LMUOF + (JMUOF-1)*10
C
C                Load count and location of raw module hits
C
          IF (IGO.EQ.2) THEN
              IMUOF = LMUOF + NMUOF*10
              IQ(IMUOF+1) = MODNUM
              IQ(IMUOF+2) = NHIT
              IQ(IMUOF+10) = NLOC
C
C                Load count and location of processed hits
C
          ELSE IF (IGO.EQ.3) THEN
              IHIT = IQ(IMUOF+4)
              IQ(IMUOF+4) = IHIT + 1
              IF (IHIT.EQ.0) IQ(IMUOF+5) = NLOC
              IPLN = ISHFT(1,MOD(NHIT,4))
              IQ(IMUOF+9) = IOR(IPLN,IQ(IMUOF+9))
C
C                Load count of first hit pointer
C
          ELSE IF (IGO.EQ.4) THEN
              IQ(IMUOF+3) = NLOC
C
C                Load count and location of scintillator hits
C
          ELSE IF (IGO.EQ.5) THEN
              IHIT = IQ(IMUOF+6)
              IQ(IMUOF+6) = IHIT + 1
              IF (IHIT.EQ.0) IQ(IMUOF+7) = NLOC
C
C                Update MUHT bank
C
          ENDIF
          CALL MUHTFL(IGO,MODNUM,NHIT)
C
      ENDIF
C
      RETURN
      END
