      SUBROUTINE VTXHST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   4-NOV-1992   Peter M. Grudberg
C_   updated  29-DEC-1992   Liang-ping Chen   Use single precision 
C_                          SPI, STWOPI, SHALFPI in HBOOK (Re. Greenlee)
C-   Updated   1-MAY-1993   Ed Oltman   ACCOMADATE CHANGE IN VTTH
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      REAL ZEE, TIME, AREA, PHI, XG, YG, PHIG, THETA, CHISQ, CHISQZ
      REAL SINTHE, DIFF, CATHPHI(0:2), HALF(0:2), PHICEN, PHIINSEC
      REAL MEAN(0:31,0:2), ERR(0:31,0:2), HSTATI, SIGMA, SUM, DEDX
      REAL SPI, STWOPI, SHALFPI
      INTEGER MXBIN
      PARAMETER ( MXBIN = 250 )
      REAL BINCENTER(MXBIN), BINCENTSQ(MXBIN), BINSIZ
      REAL XMI, XMA, YMI, YMA, CONTENTS(MXBIN), TSUM, TSUMSQ
      INTEGER ID, NX, NY, NWT, LOC, I, MAXBIN, N
      INTEGER IER, LAY, NCH(0:2), LVSEC, GZVSEC, LVWDA, GZVWDA, WIRE
      INTEGER LVTRH, GZVTRH, ICH, SEC, NSEC(0:2), IFADC, NWORDS, NH
      INTEGER PNT, LSEG, NTRACK, LVTXT, OLDLAY, OLDSEC, HITNUM, IADD
      INTEGER IH, LVTTH, LVRFT, GZVRFT, STAT, NZH, SINCOR
      INTEGER GRPID, HVID, LOSEC, HISEC, GRP
      CHARACTER*6 INOUT
      CHARACTER*7 LAYR
      CHARACTER*10 SECT
      CHARACTER*40 TITLE
      CHARACTER*80 TITL
      LOGICAL FIRST, CLNHST
      DATA NSEC / 16, 32, 32 /
      DATA NCH / 256, 512, 512 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C
      CALL DHDIR('VTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        SPI=PI
        STWOPI=TWOPI
        SHALFPI=HALFPI
C
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('CLNHST',CLNHST,IER)
        IF ( IER .NE. 0 ) CLNHST = .TRUE.
        CALL EZRSET
C
        DO LAY = 0, 2
C
          LVRFT = GZVRFT()
          PHICEN = C(LVRFT+8+7*LAY) * RADIAN
          HALF(LAY) = C(LVRFT+6+7*LAY) * RADIAN
          CATHPHI(LAY) = PHICEN - HALF(LAY)
C
C ****  Hits
C
          WRITE(TITLE,'(A,I1)') 'Wire map, layer ', LAY
          CALL HBOOK1(100+LAY,TITLE,NCH(LAY),
     &      -0.5,FLOAT(NCH(LAY))-0.5,0.)
          WRITE(TITLE,'(A,I1)') 'Drift time, all hits, layer ', LAY
          CALL HBOOK1(110+LAY,TITLE,101,-25.,2500.,0.)
          WRITE(TITLE,'(A,I1)') 'Drift time, track hits, layer ', LAY
          CALL HBOOK1(120+LAY,TITLE,101,-25.,2500.,0.)
          WRITE(TITLE,'(A,I1)') 'Area, all hits, layer ', LAY
          CALL HBOOK1(130+LAY,TITLE,100,0.,2000.,0.)
          WRITE(TITLE,'(A,I1)') 'Area, track hits, layer ', LAY
          CALL HBOOK1(140+LAY,TITLE,100,0.,2000.,0.)
          WRITE(TITLE,'(A,I1)')
     &      'Corrected area, track hits, layer ', LAY
          CALL HBOOK1(150+LAY,TITLE,100,0.,2000.,0.)
          WRITE(TITLE,'(A,I1)') 'Z, all hits, layer ', LAY
          CALL HBOOK1(160+LAY,TITLE,50,-60.,60.,0.)
          WRITE(TITLE,'(A,I1)') 'Z, track hits, layer ', LAY
          CALL HBOOK1(170+LAY,TITLE,50,-60.,60.,0.)
C
C ****  Segments
C
          WRITE(TITLE,'(A,I1)') 'Segment phi, layer ', LAY
          CALL HBOOK1(200+LAY,TITLE,50,0.,STWOPI,0.)
          WRITE(TITLE,'(A,I1)') 'Seg PhiG-Phi vs Phi, layer ', LAY
          CALL HBOOK2(210+LAY,TITLE,50,0.,STWOPI,50,-0.1,0.1,0.)
          WRITE(TITLE,'(A,I1)') 'Seg theta, layer ', LAY
          CALL HBOOK1(220+LAY,TITLE,50,0.,SPI,0.)
          WRITE(TITLE,'(A,I1)') 
     &      'Seg chisq vs Phi-in-Sec, layer ', LAY
          CALL HBOOK2(230+LAY,TITLE,50,0.,HALF(LAY)*2.,50,0.,5.,0.)
          WRITE(TITLE,'(A,I1)') 'Phi-in-Sec, layer ', LAY
          CALL HBOOK1(240+LAY,TITLE,50,0.,HALF(LAY)*2.,0.)
C
C ****  HV groups
C
          WRITE(LAYR,'(A,I1)') 'Layer ', LAY
          DO GRP = 0, 31  ! 32 sense hv channels per layer
            IF ( LAY .EQ. 0 ) THEN
              WRITE(SECT,'(A,I2)') 'Sector ', MOD(GRP,16)
            ELSE
              LOSEC = 2*(GRP/2) + MOD(LAY,2)
              HISEC = MOD(LOSEC+1,32)
              WRITE(SECT,'(A,I2,A,I2)') ' Sec ', LOSEC, '-', HISEC
            ENDIF
            INOUT = ' Inner'
            IF ( GRP .GE. 16  ) INOUT = ' Outer'
            TITLE = ' Area: '//LAYR//SECT//INOUT
            CALL HBOOK1( 1000+100*LAY+GRP,TITLE,100,0.,2000.,0.)
          ENDDO
C
          WRITE(TITLE,'(A,I1,A)')
     &      'Area vs HV group, layer ', LAY, ' in16,out16'
          CALL HBOOK1(400+LAY,TITLE,32,-0.5,31.5,0.)
C
        ENDDO
C
C ****  Tracks
C
        CALL HBOOK1(300,'Number of tracks',100,0.,200.,0.)
        CALL HBOOK1(310,'XY Hits on tracks',25,-0.5,24.5,0.)
        CALL HBOOK1(320,'Z Hits on tracks',25,-0.5,24.5,0.)
        CALL HBOOK1(330,'VTXT Ionization',100,0.,4.,0.)
        CALL HBOOK1(340,'VTXT Phi',50,0.,STWOPI,0.)
        CALL HBOOK1(350,'VTXT Theta',50,0.,SPI,0.)
        CALL HBOOK1(360,'VTXT chisq',50,0.,50.,0.)
        CALL HBOOK1(370,'VTXT chisqz',50,0.,25.,0.)
C
      ENDIF
C
      LVTRH = GZVTRH()
C
      DO LAY = 0, 2
        ICH = 0
        DO SEC = 0, NSEC(LAY) - 1
          LVSEC = GZVSEC(LAY,SEC)
          LVWDA = GZVWDA(LAY,SEC)
          IF ( LVSEC .GT. 0 ) THEN
            DO IFADC = 0, 15
              IF ( IQ(LVWDA+4+IFADC) .GT. 0 ) THEN
                CALL HF1(100+LAY,FLOAT(ICH),1.)
              ENDIF
              ICH = ICH + 1
            ENDDO
C
            NWORDS = IQ(LVSEC+3)
            DO WIRE = 0, 7
              NH = IQ(LVSEC+4+WIRE)
              PNT = LVSEC + IQ(LVSEC+12+WIRE)
              DO IH = 1, NH
                STAT = IBITS(IQ(PNT+9),0,2)
                IF ( STAT .EQ. 3 ) THEN
                  ZEE  = Q(PNT+3)
                  AREA = Q(PNT+6)
                  TIME = Q(PNT+8)
                  CALL HF1(110+LAY,TIME,1.)
                  CALL HF1(130+LAY,AREA,1.)
                  CALL HF1(160+LAY,ZEE,1.)
                ENDIF
                PNT = PNT + NWORDS
              ENDDO
            ENDDO
          ELSE
            ICH = ICH + 16
          ENDIF
        ENDDO
C
        IF ( LVTRH .GT. 0 ) THEN
          LSEG = LQ(LVTRH-3-LAY)
          DO WHILE ( LSEG .GT. 0 )
            PHI = Q(LSEG+20)
            XG  = Q(LSEG+21)
            YG  = Q(LSEG+22)
            PHIG = ATAN2(YG,XG)
            DIFF = PHIG - PHI
            DIFF = DIFF - NINT(DIFF/PI)*PI
            THETA = Q(LSEG+23)
            CHISQ = Q(LSEG+24)
            CALL HF1(200+LAY,PHI,1.)
            CALL HF2(210+LAY,PHI,DIFF,1.)
            CALL HF1(220+LAY,THETA,1.)
            PHIINSEC = MOD(PHI+DIFF-CATHPHI(LAY),2.*HALF(LAY)) ! Use PHIG
            CALL HF2(230+LAY,PHIINSEC,CHISQ,1.)
            CALL HF1(240+LAY,PHIINSEC,1.)
            LSEG = LQ(LSEG)
          ENDDO
        ENDIF
      ENDDO
C
      IF ( LVTRH .GT. 0 ) THEN
        NTRACK = IQ(LVTRH+2)
        CALL HF1(300,FLOAT(NTRACK),1.)
        LVTXT = LQ(LVTRH-1)
        DO WHILE ( LVTXT .GT. 0 )
          NH = IQ(LVTXT+2)
          NZH = IQ(LVTXT+5)
          PHI = Q(LVTXT+6)
          THETA = Q(LVTXT+9)
          SINTHE = SIN(THETA)
          CHISQ = Q(LVTXT+12)
          CHISQZ = Q(LVTXT+13)
          DEDX = Q(LVTXT+20)
          CALL HF1(310,FLOAT(NH),1.)
          CALL HF1(340,PHI,1.)
          CALL HF1(360,CHISQ,1.)
          IF ( CHISQZ .GT. 0. ) THEN ! Take out tracks w/o theta info
            CALL HF1(320,FLOAT(NZH),1.)
            CALL HF1(350,THETA,1.)
            CALL HF1(370,CHISQZ,1.)
C
C ****  Check to see if the sin(theta) correction was applied to the DEDX.  If
C ****  not, apply it here (and only fill the histogram if there is a theta
C ****  measurement for the track in question)
C
            SINCOR = IBITS(IQ(LVTXT+1),0,1)
            IF ( SINCOR .NE. 1 ) DEDX = DEDX * SINTHE
            CALL HF1(330,DEDX,1.)
          ENDIF
          LVTTH = LQ(LVTXT-1)
          OLDLAY = 3
          OLDSEC = 32
          IF ( LVTTH .GT. 0 ) THEN
            DO IH = 1, NH
              IADD = IQ(LVTTH+6+4*(IH-1))
              LAY = IBITS(IADD,9,2)
              SEC = IBITS(IADD,4,5)
              WIRE = IBITS(IADD,1,3)
              HITNUM = IQ(LVTTH+7+4*(IH-1))
              IF ( LAY .NE. OLDLAY .OR. SEC .NE. OLDSEC ) THEN
                LVSEC = GZVSEC(LAY,SEC)
                IF ( LAY .EQ. 0 ) THEN
                  GRPID = 1000 + SEC
                ELSEIF ( LAY .EQ. 1 ) THEN
                  GRPID = 1100 + MOD(SEC+31,32)/2
                ELSE
                  GRPID = 1200 + SEC/2
                ENDIF
              ENDIF
              HVID = GRPID
              IF ( WIRE .EQ. 0 .OR. WIRE .EQ. 7 ) HVID = HVID + 16
              IF ( LVSEC .GT. 0 ) THEN
                PNT = LVSEC + IQ(LVSEC+12+WIRE) + NWORDS*(HITNUM-1)
                STAT = IBITS(IQ(PNT+9),0,2)
                IF ( STAT .EQ. 3 ) THEN
                  ZEE  = Q(PNT+3)
                  AREA = Q(PNT+6)
                  TIME = Q(PNT+8)
                  CALL HF1(120+LAY,TIME,1.)
                  CALL HF1(140+LAY,AREA,1.)
                  CALL HF1(170+LAY,ZEE,1.)
                  IF ( SINTHE .GT. 0. ) THEN
                    CALL HF1(150+LAY,AREA*SINTHE,1.)
                    CALL HF1(HVID,AREA*SINTHE,1.)
                  ENDIF
                ENDIF
              ENDIF
              OLDLAY = LAY
              OLDSEC = SEC
            ENDDO
          ENDIF
          LVTXT = LQ(LVTXT)
        ENDDO
      ENDIF
  999 RETURN
C
      ENTRY VTXHST_END
C
      CALL DHDIR('VTRAKS_RCP','HBOOK_DIRECTORY',IER,' ')
C
C ****  Get histogram parameters
C
      CALL HGIVE(1000,TITL,NX,XMI,XMA,NY,YMI,YMA,NWT,LOC)
      BINSIZ = (XMA-XMI)/FLOAT(NX)
      DO I = 1, NX
        BINCENTER(I) = XMI + (FLOAT(I)-0.5)*BINSIZ
        BINCENTSQ(I) = BINCENTER(I)**2
      ENDDO
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
          IF ( CLNHST ) CALL HDELET(ID)
          ID = ID + 1
        ENDDO
      ENDDO
C
      DO LAY = 0, 2
        CALL HPAK(400+LAY,MEAN(0,LAY))
        CALL HPAKE(400+LAY,ERR(0,LAY))
      ENDDO
C
      RETURN
      END
