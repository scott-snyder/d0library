      SUBROUTINE VTX_AVE_PH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill average pulse height histos (one channel oer
C-                         sense hv channel).
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-OCT-1992   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL MEAN(0:31,0:2), ERR(0:31,0:2), HSTATI, SIGMA, SUM
      INTEGER MXBIN
      PARAMETER ( MXBIN = 250 )
      REAL BINCENTER(MXBIN), BINCENTSQ(MXBIN), BINSIZ
      REAL XMI, XMA, YMI, YMA, CONTENTS(MXBIN), TSUM, TSUMSQ
      INTEGER GRP, ID, IER, NX, NY, NWT, LOC, I, LAY, MAXBIN, N
      CHARACTER*80 TITLE
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C
      CALL DHDIR(' ','//PAWC/VTX',IER,' ')
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Get histogram parameters
C
        CALL HGIVE(1000,TITLE,NX,XMI,XMA,NY,YMI,YMA,NWT,LOC)
        BINSIZ = (XMA-XMI)/FLOAT(NX)
        DO I = 1, NX
          BINCENTER(I) = XMI + (FLOAT(I)-0.5)*BINSIZ
          BINCENTSQ(I) = BINCENTER(I)**2
        ENDDO
      ENDIF
C
      DO LAY = 0, 2
        ID = 1000 + LAY*100
        DO GRP = 0, 31
          MEAN(GRP,LAY) = HSTATI(ID,1,'HIST',0)
          SIGMA = HSTATI(ID,2,'HIST',0)
          SUM = HSTATI(ID,3,'HIST',0)
          IF ( SUM .GT. 1 ) THEN
C
C ****   Perform truncated average (truncate at 2 times average)
C
            MAXBIN = NINT((2*MEAN(GRP,LAY)-XMI)/BINSIZ)
            IF ( MAXBIN .LT. NX ) THEN
              CALL HUNPAK(ID,CONTENTS,'HIST',0)
              N = 0
              TSUM = 0.
              TSUMSQ = 0.
              DO I = 1, MAXBIN
                N = N + CONTENTS(I)
                TSUM = TSUM + BINCENTER(I)*CONTENTS(I)
                TSUMSQ = TSUMSQ + BINCENTSQ(I)*CONTENTS(I)
              ENDDO
              MEAN(GRP,LAY) = TSUM / FLOAT(N)
              SIGMA = SQRT(TSUMSQ/N - MEAN(GRP,LAY)**2)
            ENDIF
            ERR(GRP,LAY) = SIGMA/SQRT(SUM)
          ELSE
            MEAN(GRP,LAY) = 0.
            ERR(GRP,LAY) = 0.
          ENDIF
          ID = ID + 1
        ENDDO
      ENDDO
C
      CALL DHDIR(' ','//PAWC/CD',IER,' ')
      DO LAY = 0, 2
        CALL HPAK(112+LAY,MEAN(0,LAY))
        CALL HPAKE(112+LAY,ERR(0,LAY))
      ENDDO
C
  999 RETURN
      END
