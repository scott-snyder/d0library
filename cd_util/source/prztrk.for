      SUBROUTINE PRZTRK(PRUNIT,LOC,ITRA,FLAG,IFLAG)                 
C------------------------------------------------------------------
C 
C     Print out banks ZTRK (central detector tracks)
C     IF FLAG='ALL'  all tracks
C     IF FLAG='ONE'  only track ITRA 
C 
C     Daria Zieminska Feb. 1989
C-   Updated  21-SEP-1992   Qizhong Li-Demarteau  changed printout
C-                                         (added mu, e or tau road)
C                            
C------------------------------------------------------------------
      IMPLICIT NONE 
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INTEGER PRUNIT,LOC,IFLAG
      INTEGER ICONT(10),ITRA,NTRACK,ITRACK,IWORD,IB0,IB1,IB2,IB3 
      INTEGER IADD(24),IHIT(24),IADDS(6),IHITS(6)
      INTEGER LBIT,IBITS,NGOODT,NEL,NWORDS
      INTEGER LZTRK,GZZTRK, ISTAT(7) 
      CHARACTER*3 FLAG
C------------------------------------------------------------------
      IF (FLAG.EQ.'ONE') THEN
        LZTRK=GZZTRK(ITRA)
        IF (LZTRK.GT.0) THEN
        IB0=IBITS(IQ(LZTRK+1),0,8)  
        IB1=IBITS(IQ(LZTRK+1),8,8)  
        IB2=IBITS(IQ(LZTRK+1),16,8)  
        IB3=IBITS(IQ(LZTRK+1),24,8)  
          DO 200 LBIT=9,12
            ISTAT(LBIT-8)=IBITS(IQ(LZTRK),LBIT,1)
  200     CONTINUE
        WRITE (PRUNIT,101) 
        WRITE (PRUNIT,102) ITRA,IB3,IB2,IB1,IB0,
     X  IQ(LZTRK+2),IQ(LZTRK+3),IQ(LZTRK+4),IQ(LZTRK+5),Q(LZTRK+6),
     X  Q(LZTRK+8), 
     X  ISTAT(3),ISTAT(2),ISTAT(1),ISTAT(7),ISTAT(6),ISTAT(5),ISTAT(4)
        END IF
        GO TO 1000
      END IF
      CALL GTZTRH(ICONT)
      NTRACK=ICONT(2)
      IF (NTRACK.LE.0) GO TO 1000
      IF (FLAG.EQ.'ALL') THEN 
        WRITE (PRUNIT,101) 
        DO 100 ITRACK=1,NTRACK
          LZTRK=GZZTRK(ITRACK)
          IF (LZTRK.GT.0) THEN
            DO 201 LBIT=6,12
              ISTAT(LBIT-5)=IBITS(IQ(LZTRK),LBIT,1)
  201       CONTINUE
            IB0=IBITS(IQ(LZTRK+1),0,8)  
            IB1=IBITS(IQ(LZTRK+1),8,8)  
            IB2=IBITS(IQ(LZTRK+1),16,8)  
            IB3=IBITS(IQ(LZTRK+1),24,8)  
            WRITE (PRUNIT,102) ITRACK,IB3,IB2,IB1,IB0,
     X      IQ(LZTRK+2),IQ(LZTRK+3),IQ(LZTRK+4),
     X      IQ(LZTRK+5),Q(LZTRK+6),Q(LZTRK+8),
     X      ISTAT(3),ISTAT(2),ISTAT(1),
     X      ISTAT(7),ISTAT(6),ISTAT(5),ISTAT(4)
          END IF
  100   CONTINUE 
      END IF
  101 FORMAT(/,'  ZTRK    b3 b2 b1 b0    IDV    IDC    IDF    IDT',
     X       '    delta_phi  delta_theta  mu   e tau',
     X       '  unique_2D   3D  no_theta_VTX only_VTX')
  102 FORMAT(1X,I4,4X,4I3,4I7,3X,F9.4,2X,F9.4,3X,3I4,4I8)
 1000 CONTINUE 
      RETURN
      END       
