C VAX/DEC CMS REPLACEMENT HISTORY, Element MUD1FL.FOR
C *1    15-SEP-1993 17:31:14 DARIEN "New MF code for 1B MUD1"
C VAX/DEC CMS REPLACEMENT HISTORY, Element MUD1FL.FOR
C&IF VAXVMS
      OPTIONS /CHECK=NOOVERFLOW
C&ENDIF
      SUBROUTINE MUD1FL(IGO,CRATE)
C-----------------------------------------------------------------
C-
C-   Purpose and Methods : Fill MUD1 bank for one crate or trailer
C-
C-   Inputs  :  IGO    - control flag
C-                       IGO=1   initialize/book bank
C-                          =2   copy one device (header or adc)
C-                          =3   create crate trailer
C-                          =4   copy one crate
C-                          =5   create bank trailer and push bank
C-              CRATE  - array of data to be copied to MUD1
C-
C-    Output :  D0 Zebra output bank.   (MUD1)
C-
C-    Created :  26-AUG-93  M. Fortner
C-
C-----------------------------------------------------------------
      IMPLICIT NONE
C                              
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'     ! D0 ZEBRA bank.
C
      INTEGER IGO,CRATE(*)
C
C  -- local variables.         
      INTEGER LMUD1,LM,LMC,I
      INTEGER NWCRATE,NWMUD1,NSCRATE,NSMUD1
      INTEGER IWDEV,ITWORD,ICWORD,MTRIG
      DATA MTRIG /'FFFF0000'X/
      INTEGER  GZMUD1
      EXTERNAL GZMUD1
C
C                Initialize and book bank if needed
C
      IF (IGO.EQ.1) THEN
          LMUD1 = GZMUD1(0)
          IF (LMUD1.EQ.0) THEN
              CALL BKMUD1(LMUD1)
          ENDIF
          LM = LMUD1
          LMC = LMUD1
          NWMUD1 = 0
          NWCRATE = 0
          NSMUD1 = 0
          NSCRATE = 0
C
C                Copy one device into MUD1
C
      ELSE IF (IGO.EQ.2) THEN
          IWDEV = CRATE(1) + 1
          DO I = 1,IWDEV
              LM = LM + 1
              IQ(LM) = CRATE(I)
              NSCRATE = NSCRATE + CRATE(I)
          END DO
          NWCRATE = NWCRATE + IWDEV
C
C                Add trailer to crate in MUD1
C
      ELSE IF (IGO.EQ.3) THEN
          ITWORD = IAND(IQ(LMC+2),MTRIG)
          ICWORD = ISHFT(IQ(LMC+3),-24)
          IQ(LM+1) = NWCRATE
          IQ(LM+2) = ITWORD + ICWORD
          IQ(LM+3) = 0
          NSCRATE = NSCRATE + IQ(LM+1) + IQ(LM+2)
          IQ(LM+4) = NOT(NSCRATE)
          LMC = LM + 4
          NWMUD1 = NWMUD1 + NWCRATE
          NWCRATE = 0
          NSCRATE = 0
C
C                Add whole crate to MUD1
C
      ELSE IF (IGO.EQ.4) THEN
          IWDEV = CRATE(1) + 1
          IWDEV = IWDEV + CRATE(IWDEV+1) + 1
          IWDEV = IWDEV + CRATE(IWDEV+1) + 1 + 4
          DO I = 1,IWDEV
              LM = LM + 1
              IQ(LM) = CRATE(I)
          END DO
          NWMUD1 = NWMUD1 + IWDEV
          LMC = LM
C
C                Add cable trailer to MUD1 and resize
C
      ELSE IF (IGO.EQ.5) THEN
         DO I = 1,16
             IQ(LMC+I) = 0
         END DO
         NWMUD1 = NWMUD1 + 16
         CALL MZPUSH(IXMAIN,LMUD1,0,NWMUD1,'I')
C
C
      ENDIF
C
      RETURN
      END
