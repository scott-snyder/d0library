      SUBROUTINE PSAMUD1_SV(IVIEW,IFLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To display the raw hit SAMUS cells
C
C  Argument Declarations:
C  ========================
C  IVIEW - Input - INTEGER - View number
C         1  Z-Y
C         2  X-Y  <--NOT VALID in the routine
C         3  Z-X
C- IFLAG =1 for +Z ,-1 for -Z
C-
C-
C-   Created  23-May-1991, C. Yoshikawa, S. Hagopian
C-            Based on PMCELL
C-   Updated  27-JAN-1993   Vladimir Glebov  ! Updated SACELL2 
C-   Updated   9-FEB-1993   Vladimir Glebov  ! Inserted adjacent tube check 
C-   Updated  13-FEB-1994   Alexander Kozelov  Inserted MUDPAK etc. to avoid 
C-          problems with Run1A/Run1b formats, moved cycles on stations,
C-          sections and hits here from GTSAM1 for time efficiency
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IVIEW,IFLAG
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUHT.LINK'
C    Variable declarations
C    =====================
      INTEGER NHIT,XYHITFLG,UVHITFLG
      INTEGER IERR,K,NST,NSC,IWIR,ITIME,NUMT
      INTEGER LMUHT,GZMUHT,NMURAW,NSARAW,IDUM
      INTEGER GZSAHH,LSAHH
      INTEGER LMUD1,GZMUD1,NMUD1,MAXMUD1
      REAL    SPAR1(3),SPAR2(3),XPAR1(3),XPAR2(3) 
      INTEGER NBUF,IBUF(100)
      INTEGER SHAPE_TYPE, ST1, ST2, ST3
      INTEGER LHITS, GZHITS, OLDMUHT
      REAL XDCEN,YDCEN
      REAL X1,Y1,X2,Y2,X3,Y3
      CHARACTER*3 COLORS(2)
      INTEGER MARKS(2)
      CHARACTER*15 LABELS(2)
      LOGICAL FIRST,EZERROR
      LOGICAL SAMONLY
      INTEGER MAXSTA, MAXSEC
      PARAMETER (MAXSTA=6, MAXSEC=6)
      INTEGER IHEAD
      INTEGER JERR, NCEL, JHIT
      INTEGER NLAT, IADC(2), NMOD
      INTEGER JTUBE, IHIT, NHITS, I, J
      INTEGER PED_CUT, MAX_ADC 
      DATA    PED_CUT/400/, MAX_ADC/4000/
C
      DATA COLORS/'MAG','MAG'/
      DATA MARKS/2,4/
      DATA LABELS/' X or Y hit',' U or V hit'/
C
      DATA FIRST/.TRUE./
C
C    Executable code
C    ===============
      IF (.NOT.(IFLAG.EQ.1 .OR. IFLAG.EQ.-1)) THEN  
        WRITE(6,*)'IFLAG ERROR IN PSAMUD1_SV'
        GOTO 999
      ENDIF                                  
      LMUD1 = GZMUD1()
      IF (LMUD1.LE.0) THEN
        CALL INTMSG( 'No MUD1 bank' )
        GO TO 999
      ENDIF
      X2 = -50000.
      X3 = -45000.
      NHIT=0
      CALL PUGETV('SAMUS ONLY',SAMONLY)
      CALL PUGETV('SAMUS MAX HITS',MAXMUD1)
      CALL PUGETV('SAMUS DRAW XY HITS',XYHITFLG)
      CALL PUGETV('SAMUS DRAW UV HITS',UVHITFLG)
      IF(XYHITFLG.LT.1.AND.UVHITFLG.LT.1)GO TO 999
      IF(XYHITFLG.GT.3.OR.UVHITFLG.GT.3)GO TO 999
      IF(MAXMUD1.LE.0)MAXMUD1=9999
C
C **** Create new MUHT structure
C
      LHITS=GZHITS()  
      IF(LHITS.NE.0) THEN
        OLDMUHT = LQ(LHITS-IZMUHT) 
        LQ(LHITS-IZMUHT) = 0
      ENDIF
      CALL BKMUHT(0, 0, LMUHT)
      CALL MUDPAK(IERR)
      LMUHT=GZMUHT()
      IF(LMUHT.GT.0)THEN
        NSARAW=IQ(LMUHT+4)                        !    "
      ENDIF
      IF(NSARAW.GT.MAXMUD1)THEN
        CALL INTMSG(' !!!!WARNING -too many SAMUS hits to display')
        GO TO 990
      ENDIF
      CALL PUOPEN
      NHIT=0
C
      CALL MUDMOD(0, NHITS, JHIT, IHEAD)
      CALL SADHIT(0, JHIT, NCEL, NLAT, IADC)
      DO NST = 1, MAXSTA         ! Station loop
C
C ****  Loop over SAMUS sections
C
        DO NSC = 1, MAXSEC             ! Section loop
          CALL SATOPM(NST, NSC, NMOD)
          CALL MUDMOD(NMOD, NHITS, JHIT, IHEAD)
C
C ****  Loop over SAMUS hits
C
          DO I = 1, NHITS                          ! Hit loop
            IHIT = JHIT
            CALL SADHIT(IHIT, JHIT, NCEL, NLAT, IADC)
            IWIR = NCEL * 2 + 1
            IF (NSC .EQ. 2 .OR. NSC .EQ. 5) IWIR = IWIR - 16
            DO J = 1, 2                         ! loop for even/odd tubes
              JTUBE = IWIR - 1 + J              ! even/odd tube number
              ITIME = IADC(J)                   ! drift time
              IF (ITIME .GT. PED_CUT .AND.
     &            ITIME .LT. MAX_ADC) THEN      ! this tube is ON
C
                IF(IVIEW.EQ.1.AND.(NSC.EQ.1.OR.NSC.EQ.4)) GOTO 100
                IF(IVIEW.EQ.3.AND.(NSC.EQ.3.OR.NSC.EQ.6)) GOTO 100
                CALL SACELL2(NST,NSC,IWIR,NUMT,SPAR1,XPAR1,SPAR2,XPAR2)
                IF (.NOT.(NUMT.EQ.1 .OR. NUMT.EQ.2)) THEN    
                  WRITE(6,*)'NUMT ERROR IN PSAMUD1_SV'
                  GOTO 900                            
                ENDIF                                 
C
C     TAKE CARE OF SIDE & TOP VIEWS
C
                IF (IVIEW.EQ.1) THEN            ! SIDE VIEW
                  IF (NSC.EQ.2 .OR. NSC.EQ.5) SHAPE_TYPE=3  ! "O"
                  IF (NSC.EQ.3 .OR. NSC.EQ.6) SHAPE_TYPE=2  ! "+"
                  XDCEN=XPAR1(3)
                  YDCEN=XPAR1(2)
                ELSEIF (IVIEW.EQ.3) THEN        ! TOP VIEW
                  IF (NSC.EQ.2 .OR. NSC.EQ.5) SHAPE_TYPE=3  ! "0"
                  IF (NSC.EQ.1 .OR. NSC.EQ.4) SHAPE_TYPE=2  ! "+"
                  XDCEN=XPAR1(3)
                  YDCEN=XPAR1(1)
                ENDIF
C
C ****  Check for adjacent tubes
C
                IF(XYHITFLG.EQ.1.AND.(NSC.EQ.1.OR.NSC.EQ.3
     &            .OR.NSC.EQ.4.OR.NSC.EQ.6)) GO TO 33   
                IF(UVHITFLG.EQ.1.AND.(NSC.EQ.2.OR.NSC.EQ.5)) GOTO 33
                GOTO 60
C
   33           X1 = X2
                Y1 = Y2
                ST1 = ST2
                X2 = X3
                Y2 = Y3
                ST2 = ST3
                X3 = XDCEN
                Y3 = YDCEN
                ST3 = SHAPE_TYPE
                IF(ABS(X2-X1).LT.15..AND.ABS(Y2-Y1).LT.3.5
     &            .AND.ST1.EQ.ST2) GO TO 50
                IF(ABS(X3-X2).LT.15..AND.ABS(Y3-Y2).LT.3.5
     &            .AND.ST3.EQ.ST2) GO TO 50
                GOTO 100
C
   50           CONTINUE
                XDCEN = X2
                YDCEN = Y2
                SHAPE_TYPE = ST2
C
C     DRAW THE HITS
C
   60           CALL PXCOLR('MAG')
                NHIT = NHIT + 1
                IF (SHAPE_TYPE.EQ.2.AND.XYHITFLG.GT.0) THEN     ! X,Y - mark=+
                  CALL PXMARK('MAG',2,XDCEN,YDCEN,0)
                ELSEIF (SHAPE_TYPE.EQ.3.AND.UVHITFLG.GT.0) THEN ! U,V - mark=0
                  CALL PSCELL(IVIEW,XPAR1) 
                ENDIF
              ENDIF
  100       ENDDO
          ENDDO ! hit loop 
        ENDDO ! NSC loop
      ENDDO  ! NST loop
C
  900 CALL JRCLOS
      IF(NHIT.LE.1)GO TO 990
      IF(SAMONLY.AND.UVHITFLG.GT.0)THEN
       CALL LEGEND_MARKS(COLORS,MARKS,LABELS,2)
      ENDIF
C
  990 LMUHT = GZMUHT(IDUM)
      CALL MZDROP( IXMAIN, LMUHT, 'V' )
      CALL MZDROP( IXMAIN, LMUHT, 'L' )
      LQ(LHITS-IZMUHT) = OLDMUHT
  999 RETURN
      END
