C&IF VAXVMS
      OPTIONS /CHECK=NOOVERFLOW
C&ENDIF
      SUBROUTINE GTMUD1(IGO,LADD,IADD,IDEN,
     &                  IW1,IW2,IW3,IW4,IW5,IW6,IW7,IW8)
C====================================================================
C
C-   Purpose and Methods : Extract one block of MUD1 data
C
C-   Inputs  :  IGO    - control flag
C-              LADD   - Offset address in MUD1.
C- 
C-   Output  :  IADD   - Address in data of next block
C-              IDEN   - Data identifier
C-              IW1-IW8 - Raw data values
C-
C-   Functions :
C-       IGO=1  Get crate header information
C-              IADD = location of trailer
C-              IDEN = crate controller word
C-              IW1  = number of header words
C-              IW2  = trigger number
C-              IW3  = number of data words
C-              IW4  = version number
C-              IW5-8 = user header words
C-
C-       IGO=2  Get module header information - run 1A
C-              IADD = location of next module
C-              IDEN = module ID
C-              IW1  = number of module words in data
C-              IW2  = module flag word
C-              IW3-8 = dummy
C-
C-       IGO=3  Get WAMUS hit information - run 1A
C-              IADD = location of next hit
C-              IDEN = cell address word
C-              IW1-2 = drift time 1,2
C-              IW3-4 = pads even B,A
C-              IW5-6 = delta time 2,1
C-              IW7-8 = pads odd B,A
C-
C-       IGO=4  Get SAMUS hit information - run 1A
C-              IADD = location of next hit
C-              IDEN = cell address word
C-              IW1-2 = drift cell 1,2
C-              IW3-8 = dummy
C-
C-       IGO=5  Get crate trailer information
C-              IADD = location of next crate (0 if end)
C-              IDEN = crate ID
C-              IW1  = number of crate words
C-              IW2  = trigger number
C-              IW3  = token number
C-              IW4  = checksum
C-              IW5-8 = dummy
C-
C-       IGO=12 Get module header information - run 1B
C-              IADD = location of next module
C-              IDEN = module ID
C-              IW1  = module address mask
C-              IW2  = number of module hits in data
C-              IW3  = module flag word
C-              IW4  = dummy
C-              IW5-8 = latch words
C-
C-       IGO=13 Get hit information - run 1B
C-              IADD = location of next hit
C-              IDEN = cell number
C-              IW1-4 = ADC values 1-4
C-              IW5  = adc address mask
C-              IW6-8 = dummy
C-
C-
C  Revision History:
C  =================
C  Original Creation - June 29,1987  Tami Kramer
C  Recreated           Aug 1993  Mike Fortner
C=======================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C  Argument Declarations:
C  ======================
C
      INTEGER IGO,LADD
      INTEGER IADD,IDEN,IW1,IW2,IW3,IW4,IW5,IW6,IW7,IW8
C
C  Local Declarations:
C  ===================
C
      INTEGER GZMUD1,LMUD1,IMUD1
      INTEGER IMADC,IMU16
      LOGICAL FIRST
      DATA IMADC,IMU16/0,0/,FIRST/.TRUE./
C
C  Executable Code:
C  ================
C
      IF (FIRST) THEN
          IMADC = IBSET(IMADC,18)
          IMADC = IBSET(IMADC,19)
          IMADC = IBSET(IMADC,20)
          IMADC = IBSET(IMADC,31)
          IMU16 = 2**16 - 1
          IMU16 = ISHFT(IMU16,16)
          FIRST = .FALSE.
      ENDIF
      LMUD1 = GZMUD1(0)
      IF (LMUD1 .EQ. 0) THEN                   ! No bank present
          IADD = -1
          RETURN
      ENDIF
      IF (IQ(LMUD1-1) .LE. 16) THEN            ! No data present
          IADD = -2
          RETURN
      ENDIF
      IMUD1 = LMUD1 + LADD
C
C                Get crate header information
C
      IF (IGO.EQ.1) THEN
          IDEN = IQ(IMUD1+3)                   ! Controller word
          IW1 = IQ(IMUD1+1) + 1                ! Header word count
          IW2 = IBITS(IQ(IMUD1+2),16,16)       ! Trigger number
          IW4 = IQ(IMUD1+4)                    ! Version number
          IW5 = IQ(IMUD1+5)                    ! Pulser word 1
          IW6 = IQ(IMUD1+6)                    ! Pulser word 2
          IF (IBITS(IW4,20,1) .eq. 0) THEN     ! run 1A
              IW3 = IQ(IMUD1+IW1)
              IW7 = 0
              IW8 = 0
              IADD = LADD + IW1 + IW3 + 1
          ELSE                                 ! run 1B
              IW3 = IQ(IMUD1+IW1+1) + 1
              IW7 = IQ(IMUD1+7)
              IW8 = IQ(IMUD1+8)
              IADD = LADD + IW1 + IW3 + IQ(IMUD1+IW1+IW3+1) + 1
          ENDIF
C
C                Get module header information - run 1A
C
      ELSE IF (IGO.EQ.2) THEN
          IADD = LADD + 1
          IDEN = IQ(IMUD1+1)                   ! Module Word
          IW1 = IBITS(IDEN,0,12)               ! Word count
          IW2 = IBITS(IDEN,12,4)               ! Flags
          IDEN = IBITS(IDEN,16,16)             ! Module ID
          IW3 = 0
          IW4 = 0
          IW5 = 0
          IW6 = 0
          IW7 = 0
          IW8 = 0
C
C                Get WAMUS hit information - run 1A
C
      ELSE IF (IGO.EQ.3) THEN
          IADD = LADD + 9
          IDEN = IQ(IMUD1+1)                   ! Cell address word
          IW1 = IBITS(IQ(IMUD1+2),0,12)        ! Drift time 1
          IW2 = IBITS(IQ(IMUD1+3),0,12)        ! Drift time 2
          IW3 = IBITS(IQ(IMUD1+4),0,12)        ! Pad B even
          IW4 = IBITS(IQ(IMUD1+5),0,12)        ! Pad A even
          IW5 = IBITS(IQ(IMUD1+6),0,12)        ! Delta time 2
          IW6 = IBITS(IQ(IMUD1+7),0,12)        ! Delta time 1
          IW7 = IBITS(IQ(IMUD1+8),0,12)        ! Pad B odd
          IW8 = IBITS(IQ(IMUD1+9),0,12)        ! Pad A odd
C
C                Get SAMUS hit information - run 1A
C
      ELSE IF (IGO.EQ.4) THEN
          IADD = LADD + 3
          IDEN = IQ(IMUD1+1)                   ! Cell address word
          IW1 = IBITS(IQ(IMUD1+2),0,12)        ! Drift time 1
          IW2 = IBITS(IQ(IMUD1+3),0,12)        ! Drift time 2
          IW3 = 0
          IW4 = 0
          IW5 = 0
          IW6 = 0
          IW7 = 0
          IW8 = 0
C
C                Get crate trailer information
C
      ELSE IF (IGO.EQ.5) THEN
          IADD = LADD + 4
          IF (IADD+16 .GE. IQ(LMUD1-1)) IADD=0
          IDEN = IBITS(IQ(IMUD1+2),0,8)        ! Crate ID
          IW1 = IQ(IMUD1+1)                    ! Crate word count
          IW2 = IBITS(IQ(IMUD1+2),16,16)       ! Trigger number
          IW3 = IQ(IMUD1+3)                    ! Token number
          IW4 = IQ(IMUD1+4)                    ! Checksum
          IW5 = 0
          IW6 = 0
          IW7 = 0
          IW8 = 0
C
C                Get module header information - run 1B
C
      ELSE IF (IGO.EQ.12) THEN
          IADD = LADD + 8
          IDEN = IQ(IMUD1+1)                   ! Module Word
          IW1 = IQ(IMUD1+2)                    ! ADC address mask
          IW2 = IQ(IMUD1+3)                    ! Word count
          IW3 = IQ(IMUD1+4)                    ! Flags
          IDEN = IBITS(IDEN,16,16)             ! Module ID
          IW4 = 0
          IW5 = IQ(IMUD1+5)                    ! Latch word 1
          IW6 = IQ(IMUD1+6)                    ! Latch word 2
          IW7 = IQ(IMUD1+7)                    ! Latch word 3
          IW8 = IQ(IMUD1+8)                    ! Latch word 4
C
C                Get hit information - run 1B
C
      ELSE IF (IGO.EQ.13) THEN
          IADD = LADD + 4
          IDEN = IBITS(IQ(IMUD1+1),21,7)       ! Cell number
          IW1 = IBITS(IQ(IMUD1+1),0,12)        ! T1 / D1 / TE
          IW2 = IBITS(IQ(IMUD1+2),0,12)        ! T2 / D2 / XX
          IW3 = IBITS(IQ(IMUD1+3),0,12)        ! BE / BO / TO
          IW4 = IBITS(IQ(IMUD1+4),0,12)        ! AE / AO / XX
          IW5 = IAND(IQ(IMUD1+1),IMADC)        ! ADC address mask
          IW6 = 0
          IW7 = 0
          IW8 = 0
C
C
      ENDIF
C
      RETURN
      END
