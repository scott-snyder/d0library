      SUBROUTINE PRL0VX(PRUNIT,LJL0VX,BUNCH,CFL,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints out L0VX (Level 0  Detector Vertex) bank
C-
C-   Inputs  : PRUNIT = unit number for printout
C-             LKL0VX = bank address
C-             BUNCH = specific bunch number
C-             CFL    = 'ONE' = one bank determined by bunch number
C-                      'ALL' = all banks, ie all bunches
C-             IFL = level of printout
C-                IFL  = 0 no printout
C-                IFL >= 1 prints general info about L0VX bank
C-                IFL  = 3 prints L0VX banks
C-   Outputs : None
C-   Controls: None
C-
C-   Created  27-JUL-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER BUNCH, IBUNCH
      INTEGER PRUNIT, NL0VX, IFL, LKL0VX, LJL0VX
      INTEGER IVERS, FASTZ_ID, VERTEX_BOARD
      INTEGER WORDS(52)
      INTEGER JBUNCH, K
      INTEGER BEGBUN, ENDBUN
      INTEGER IBOARD, OFFSET
      INTEGER SIGN, GOOD_FZ
      INTEGER GZL0VX
      EXTERNAL GZL0VX
C
      REAL    SLOWZ, FASTZ
C
      CHARACTER*3 CFL

      integer z0f
      data z0f / z'0f' /
      integer z10
      data z10 / z'10' /
      integer z20
      data z20 / z'20' /
      integer zff
      data zff / z'ff' /
      integer z100
      data z100 / z'100' /
C----------------------------------------------------------------------
      IF ( IFL.LE.0 ) GOTO 999
      LKL0VX=LJL0VX
      IF ( LKL0VX.LE.0 ) LKL0VX=GZL0VX()
      IF ( LKL0VX.LE.0 ) THEN
        WRITE (PRUNIT, 901) LKL0VX
        GOTO 999
      ENDIF
C
      BEGBUN=1
      ENDBUN=6
      IF ( CFL.EQ.'ONE' ) THEN
        BEGBUN=BUNCH
        ENDBUN=BUNCH
      ENDIF
      DO JBUNCH=BEGBUN,ENDBUN
        DO 10 IBOARD=1,2
          OFFSET = (IBOARD-1)*26
          IF ( IFL.GE.1 ) THEN
            IVERS=IBITS(IQ(LKL0VX),13,5)
            CALL GTL0VX(BUNCH,WORDS)
            IBUNCH=WORDS(OFFSET+1)
            FASTZ_ID=WORDS(OFFSET+2)
            VERTEX_BOARD=WORDS(OFFSET+3)
            IF ( IBOARD.EQ.1 ) WRITE(PRUNIT,101) IVERS, IBUNCH
            WRITE(PRUNIT,102) FASTZ_ID, VERTEX_BOARD
          ENDIF
          IF ( IFL.LT.3 ) GOTO 10
          IF ( IFL.EQ.3 ) THEN
            WRITE(PRUNIT,200)
            WRITE(PRUNIT,201) WORDS(OFFSET+4),WORDS(OFFSET+9)
            WRITE(PRUNIT,202) WORDS(OFFSET+8),WORDS(OFFSET+13)
            WRITE(PRUNIT,203) WORDS(OFFSET+6),WORDS(OFFSET+11)
            WRITE(PRUNIT,204) WORDS(OFFSET+7),WORDS(OFFSET+12)
            WRITE(PRUNIT,205) WORDS(OFFSET+5)
            WRITE(PRUNIT,206) WORDS(OFFSET+10)
            WRITE(PRUNIT,207) WORDS(OFFSET+15), WORDS(OFFSET+14)
            WRITE(PRUNIT,208) WORDS(OFFSET+16)
            WRITE(PRUNIT,209) WORDS(OFFSET+17), WORDS(OFFSET+18)
          WRITE(PRUNIT,210) WORDS(OFFSET+19), WORDS(OFFSET+20)
          SIGN = IAND(WORDS(OFFSET+19),z100)/z100
          IF ( SIGN.EQ.0 ) THEN
            SLOWZ = 0.75*IAND(WORDS(OFFSET+19),zFF)
          ELSE
            SLOWZ = -0.75*IAND(z100-WORDS(OFFSET+19),zFF)
          ENDIF
          SIGN = IAND(WORDS(OFFSET+20),z10)/z10
          IF ( SIGN.EQ.0 ) THEN
            FASTZ = 6.25*IAND(WORDS(OFFSET+20),z0F)
          ELSE
            FASTZ = -6.25*IAND(z10-WORDS(OFFSET+20),z0F)
          ENDIF
          GOOD_FZ = IAND(WORDS(OFFSET+20),z20)/z20
          WRITE(PRUNIT,310) SLOWZ,FASTZ,GOOD_FZ
          WRITE(PRUNIT,211) WORDS(OFFSET+21), WORDS(OFFSET+22)
          WRITE(PRUNIT,212)
          WRITE(PRUNIT,213) (WORDS(OFFSET+K),K=23,26)
          ENDIF
   10   CONTINUE
        IF ( IFL.EQ.3 ) THEN
          OFFSET = 26
          IF ( WORDS(3).EQ.1 ) OFFSET = 0
        ENDIF
      ENDDO
C
  101 FORMAT(/' VERTEX banks for LV0 detector - Version ',I3,
     &  ' Bunch  ',I3)
  102 FORMAT(/' The Fast Z id is ',I4,
     &  ' and the Vertex board number is ',I4)
  200 FORMAT(/,40X,' SHORT COUNTERS ',2X,' LONG COUNTERS ')
  201 FORMAT(/,' The Sum of (Corrected Time)**2 is ',I15,2X,I15)
  202 FORMAT(/,' The Sum of Corrected Time is      ',I15,2X,I15)
  203 FORMAT(/,' The Minimum Corrected Time is     ',I15,2X,I15)
  204 FORMAT(/,' The Maximum Corrected Time is     ',I15,2X,I15)
  205 FORMAT(/,' The Number of Hits for Short Counters is      ',I8)
  206 FORMAT(/,' The Number of Valid Hits for Long Counters is ',I8)
  207 FORMAT(/,' The average time is ',I4,
     &  ' and the the standard deviation of time is ',I4)
  208 FORMAT(/,' The Number of Hits used in statistics calculation is ',
     &  I4)
  209 FORMAT(/,' The two statistics calculation values are ',I4,' and ',
     &  I4)
  210 FORMAT(/,' The Vertex Position is ',I6,' and the data from FASTZ',
     &           ' module is ',I4)
  310 FORMAT(/,' The Vertex Position = ',F8.2,' cm and FASTZ =',
     &           F8.2,' cm and Good_FZ =',I2)
  211 FORMAT(/,' The Interaction Flag is ',I4,' and the Good Vertex',
     &           ' Flag is ',I4)
  212 FORMAT(/,8X,' MI Flag 4 ',2X,' MI Flag 3 ',2X,' MI Flag 2 ',2X,
     &           ' MI Flag 1 ')
  213 FORMAT(5X,I10,3X,I10,3X,I10,3X,I10)
  901 FORMAT(/,'WRONG ADDRESS, LKL0VX = ', I10)
C----------------------------------------------------------------------
  999 RETURN
      END
