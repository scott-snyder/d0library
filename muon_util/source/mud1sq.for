      SUBROUTINE MUD1SQ(NMOD,IMOD,IERR)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCC   THIS ROUTINE SQUEEZES THE MUD1 BANK. NMOD IS THE NUMBER OF
CCC   WAMUS MODULES KEPT AND IMOD IS THE MODULE NUMBER
CCC
CCC    D. HEDIN    NOV-2-1991....update with trailer
CCC    DH set bit 22 word 4 =1
CC     DH 5/22 12 bits for module word count
CC     DH 12/92 word 3 wrong
CC     DW 12/93 call MUD1SQ1B for 1B MUD1 format
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER LMUD1,NMOD,IMOD(200),BUF(2000),GZMUD1,I1,I3,I,J
      INTEGER B8,B16,NHEADW,IVERS,ICONT,ICRID,IERR
      INTEGER LMOD,LHIT,NRMOD,I4,I5,INEW,IOLD,L,K,LCRATE,NDATAW
      INTEGER IND,NMODC,IMHEAD,NRHIT,B22,B12
      INTEGER IADD,IDEN,IW1,IW2,IW3,NV,IW5,IW6,IW7,IW8
C
      DATA B8,B12,B16,B22/255,4095,65535,2097152/
C
C can't use mudver here because MUHT might have been dropped
      CALL GTMUD1(1,0,IADD,IDEN,
     &                  IW1,IW2,IW3,NV,IW5,IW6,IW7,IW8)
C call new routine for 1b format data (same arguements)
      IF (IBITS(NV,20,1).EQ.1) THEN
        CALL MUD1SQ1B(NMOD,IMOD,IERR)
        GOTO 999
      ENDIF
C
      IF(NMOD.GT.12) THEN
        IERR=3
        GO TO 999
      ENDIF
      DO I=1,2000
        BUF(I)=0
      ENDDO
      IERR=0
      I1=0
      LMOD=0
      LHIT=0
      LMUD1=GZMUD1(0)
      LCRATE = LMUD1
CC              Loop over crates and see if module
 1000 CONTINUE
C
CC    -- check if crate exists or not...
      IF (IQ(LMUD1-1)-16.LE.(LCRATE-LMUD1)) GO TO 900
      NHEADW = IQ(LCRATE+1)+1
      NDATAW = IQ(LCRATE+NHEADW)
      I3=IAND(IQ(LCRATE+3),B16)
      IVERS = IAND(IQ(LCRATE+4),B16)            ! Software version number
      ICONT = ISHFT(IQ(LCRATE+3),-16)           ! Controller word
      ICRID = ISHFT(ICONT,-8)                   ! Crate ID C
      IF(IVERS.NE.1) THEN
        IF (ICRID.LE.180) THEN                  ! WIDE ANGLE CRATE
          IF(I1.EQ.0) THEN           ! FIRST TIME THROUGH; SET UP BUF HEADER
            BUF(1)=NMOD+6            ! NO. HEADER WORDS -1
            BUF(2)=IQ(LCRATE+2)      ! TRIGGER
            BUF(3)=IOR(ISHFT(NMOD,16),I3) ! CRID=0
            BUF(4)=IOR(IQ(LCRATE+4),B22)   ! SET BIT 22
            BUF(5)=IQ(LCRATE+5)
            BUF(6)=IQ(LCRATE+6)
            I4=IQ(LCRATE+NHEADW+NDATAW+4)
            I5=IQ(LCRATE+NHEADW+NDATAW+5)
            I1=1
          ENDIF
          IND = LCRATE+NHEADW+1           ! POINTER IN MUD1 TO FIRST HIT
          IF (IVERS.EQ.2823) THEN
            IND = LCRATE+NHEADW           ! SPECIAL PATCHED INDEX
          END IF
          NMODC=IAND(ICONT,B8)                  ! NUMBER OF MODULES IN CRATE
          IF (NMODC.EQ.0) NMODC=NHEADW-7        ! SPECIAL PATCHED CRATE COUNT
C               Loop over modules in crate
          DO 20 I=1,NMODC
            IMHEAD = IQ(LCRATE+6+I)         ! HEADER WORD FOR ONE MODULE
            NRMOD = ISHFT(IMHEAD,-16)        ! MODULE ID
            NRHIT = IAND(IMHEAD,B12)        ! MODULE WORD COUNT
CCCCC   SEE IF ON LIST OF SAVED MODULES
            DO J=1,NMOD
              IF(IMOD(J).EQ.NRMOD) THEN
                LMOD=LMOD+1
                BUF(6+LMOD)=IMHEAD
                IF(12+NMOD+LHIT+NRHIT.GT.2000) THEN
                  IERR=2             ! TOO MANY HITS
                  GO TO 999
                ENDIF
                DO K=1,NRHIT
                  BUF(7+NMOD+K+LHIT)=IQ(IND+K-1)
                ENDDO
                LHIT=LHIT+NRHIT
                GO TO 21
              ENDIF
            ENDDO
   21       CONTINUE
            IND = IND+NRHIT
   20     CONTINUE
        END IF
        LCRATE=LCRATE+NHEADW+NDATAW+5     ! ADVANCE POINTER TO NEXT CRATE
        GO TO 1000
      ELSE
        IERR=1          ! OLD FORMAT
        GO TO 999
      ENDIF
900   CONTINUE
      BUF(7+NMOD)=LHIT
      BUF(7+NMOD+LHIT+1)=0                ! TRAILER
      BUF(7+NMOD+LHIT+2)=7+NMOD+LHIT+5    ! TRAILER
      BUF(7+NMOD+LHIT+3)=ISHFT(ISHFT(BUF(2),-16),16)  ! TRAILER
      BUF(7+NMOD+LHIT+4)=I4                ! TRAILER
      BUF(7+NMOD+LHIT+5)=I5               ! TRAILER
CCCCCCCCCCCCCCCCCCCCCC
CCCCCCCC   COPY TO MUD1
      INEW=12+NMOD+LHIT
      IOLD=IQ(LMUD1-1)
      IF(IOLD.LT.INEW) THEN
        IERR=4
        GO TO 999
      ENDIF
      DO I=1,INEW
        IQ(LMUD1+I)=BUF(I)
      ENDDO
      L=IOLD-INEW
      CALL MZPUSH(IXCOM,LMUD1,0,-L,' ')
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  999 CONTINUE                            ! ERROR RETURN
      RETURN
      END
