      SUBROUTINE MUHTFL(IGO,MODNUM,NHIT)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Fill MUHT bank for one module
C-
C-   Inputs  :  IGO    - control flag
C-                       IGO=0   set pointer to bank
C-                          =1   initialize/book bank
C-                          =2   fill raw information
C-                          =3   fill processed information
C-                          =5   set error condition (MODNUM)
C-                          =10  load version number (MODNUM)
C-                          =98  set error condition (MODNUM)
C-                          =99  compress bank on error
C-              MODNUM - Module id
C-              NHIT   - Number of hits
C-
C-    Output :  D0 Zebra output bank.   (MUHT)
C-
C-    Created :  26-AUG-93  M. Fortner
C-    Modified:  12/94  MF  Add IGO=0 for pointer initialization
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C                              
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
C
      INTEGER IGO,MODNUM,NHIT,I
C
C  -- local variables.         
      INTEGER LMUHT,NWD
      INTEGER  GZMUHT
      EXTERNAL GZMUHT
      SAVE LMUHT
C
C                Initialize and book bank if needed
C
      IF (IGO.EQ.0.OR.IGO.EQ.1) THEN
          LMUHT = GZMUHT(0)
          IF (LMUHT.EQ.0.AND.IGO.EQ.1) THEN
              CALL BKMUHT(0,0,LMUHT)
          ENDIF
C
C                Load count and location of raw module
C
      ELSE IF (IGO.EQ.2) THEN
          IQ(LMUHT+7) = IQ(LMUHT+7) + NHIT
          IQ(LMUHT+8) = IQ(LMUHT+8) + 1
          IQ(LMUHT+10+MODNUM) = IQ(LMUHT+8)
          IF (MODNUM .LT. 400) THEN
              IQ(LMUHT+1) = IQ(LMUHT+1) + NHIT
              IQ(LMUHT+3) = IQ(LMUHT+3) + 1
          ELSE
              IQ(LMUHT+4) = IQ(LMUHT+4) + NHIT
              IQ(LMUHT+6) = IQ(LMUHT+6) + 1
          ENDIF
C
C                Load count of processed hits
C
      ELSE IF (IGO.EQ.3) THEN
          IF (MODNUM .LT. 400) THEN
              IQ(LMUHT+2) = IQ(LMUHT+2) + 1
          ELSE
              IQ(LMUHT+5) = IQ(LMUHT+5) + 1
          ENDIF
C
C                Load count of scintillator hits
C
      ELSE IF (IGO.EQ.5) THEN
          IQ(LMUHT+9) = IQ(LMUHT+9) + 1
C
C                Load version number
C
      ELSE IF (IGO.EQ.10) THEN
          IQ(LMUHT+10) = MODNUM
C
C                Set error condition
C
      ELSE IF (IGO.EQ.98) THEN
          LMUHT = GZMUHT(0)
          DO I = 1,9
              IQ(LMUHT+I) = MODNUM
          ENDDO
C
C                Compress bank if error
C
      ELSE IF (IGO.EQ.99) THEN
          LMUHT = GZMUHT(0)
          IF (IQ(LMUHT+1).EQ.-1) THEN
              NWD = 10 - IQ(LMUHT-1)
              CALL MZPUSH(IXCOM,LMUHT,0,NWD,' ')
          ENDIF
C
C
      ENDIF
C
      RETURN
      END
