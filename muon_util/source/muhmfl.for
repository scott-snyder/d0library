      SUBROUTINE MUHMFL(IGO,MODNUM,NHIT,NLOC,JFINE)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Fill MUHM bank for one module
C-
C-   Inputs  :  IGO    - control flag
C-                       IGO=1   initialize/book bank
C-                          =2   fill raw information (MUD1)
C-                          =3   fill processed information (MUOH)
C-                          =4   fill pointer information (MUHP)
C-                          =5   fill scintillator information (MSCT)
C-                          =6   fill trigger information (MOTR)
C-                          =7   fill centroid information
C-                          =99  drop bank
C-              MODNUM - Module id
C-              NHIT   - Number of hits
C-              NLOC   - Associated location in bank
C-              JFINE  - Fine centroid information
C-
C-    Output :  D0 Zebra output bank.   (MUHM)
C-
C-    Created :  13-JAN-94  M. Fortner
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C                              
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
C
      INTEGER IGO,MODNUM,NHIT,NLOC,JFINE(4)
C
C  -- local variables.         
      INTEGER LMUHM,IHIT,IPLN
      INTEGER  GZMUHM
      EXTERNAL GZMUHM
C
C                Initialize and book bank if needed
C
      LMUHM = GZMUHM(MODNUM)
      IF (LMUHM.EQ.0)THEN
          IF (IGO.EQ.1) THEN
              CALL BKMUHM(0,MODNUM,LMUHM)
              IF (LMUHM.EQ.0) RETURN
          ELSE
              RETURN
          ENDIF
      ENDIF
C
C                Drop bank
C
      IF (IGO.EQ.99) THEN
          CALL MZDROP(IXCOM,LMUHM,' ')
C
C                Load count and location of raw module hits
C
      ELSE IF (IGO.EQ.2) THEN
          IQ(LMUHM+1) = MODNUM
          IQ(LMUHM+2) = NLOC
          IQ(LMUHM+3) = JFINE(1)
          IQ(LMUHM+4) = NHIT
          IQ(LMUHM+20) = JFINE(2)
          IQ(LMUHM+21) = JFINE(3)
          IQ(LMUHM+22) = JFINE(4)
C
C                Load count and location of processed hits
C
      ELSE IF (IGO.EQ.3) THEN
          IF (NHIT.EQ.-1) THEN
              IQ(LMUHM+6) = -1
          ELSE
              IHIT = IQ(LMUHM+7)
              IQ(LMUHM+7) = IHIT + 1
              IF (IHIT.EQ.0) IQ(LMUHM+8) = NLOC
              IPLN = ISHFT(1,MOD(NHIT,4))
              IQ(LMUHM+6) = IOR(IPLN,IQ(LMUHM+6))
          ENDIF
C
C                Load count of first hit pointer
C
      ELSE IF (IGO.EQ.4) THEN
          IQ(LMUHM+5) = NLOC
C
C                Load count and location of scintillator hits
C
      ELSE IF (IGO.EQ.5) THEN
          IHIT = IQ(LMUHM+9)
          IQ(LMUHM+9) = IHIT + 1
          IF (IHIT.EQ.0) IQ(LMUHM+10) = NLOC
C
C                Load trigger information
C
      ELSE IF (IGO.EQ.6) THEN
          IQ(LMUHM+13) = NHIT
C
C                Load centroid information
C
      ELSE IF (IGO.EQ.7) THEN
          IQ(LMUHM+14) = NHIT
          IQ(LMUHM+15) = NLOC
          IQ(LMUHM+16) = JFINE(1)
          IQ(LMUHM+17) = JFINE(2)
          IQ(LMUHM+18) = JFINE(3)
          IQ(LMUHM+19) = JFINE(4)
C
C                Update MUHT bank
C
      ENDIF
C      CALL MUHTFL(IGO,MODNUM,NHIT)   ! out while MUOFFL does this
C
      RETURN
      END
