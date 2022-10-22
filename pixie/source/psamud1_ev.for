      SUBROUTINE PSAMUD1_EV(IVIEW,IFLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To display the raw hit SAMUS cells
C-
C  Argument Declarations:
C  ========================
C  IVIEW - Input - INTEGER - View number
C        = 1  Z-Y   3  Z-X

C-          IFLAG = -1  To draw -Z (NORTH) A,B,&C stations tubes hit
C-          IFLAG = +1  To draw +Z (SOUTH) A,B,&C stations tubes hit
C-                  +2  To draw -Z (NORTH) A station tubes hit
C-                  +3  To draw -Z (NORTH) B station tubes hit
C-                  +4  To draw -Z (NORTH) C station tubes hit
C-                  +5  To draw +Z (SOUTH) A station tubes hit
C-                  +6  To draw +Z (SOUTH) B station tubes hit
C-                  +7  To draw +Z (SOUTH) C station tubes hit
C        = 2  X-Y
C
C-          IFLAG = 1  +Z
C-                 -1  -Z
C-
C-   Created  22-MAY-1991   Cary Y. Yoshikawa
C-   Updated  30-JAN-1993   Lupe Howell Modified GOTO when skipping hits
C-       when no desired stations
C-   Updated  13-FEB-1994   Alexander Kozelov  Inserted MUDPAK etc. to avoid
C-          problems with Run1A/Run1b formats, moved cycles on stations,
C-          sections and hits here from GTSAM1 for time efficiency
C-   Updated   4-NOV-1994   Andrei Mayorov  move drawing in subroutine drsamhit
C-                                          fix U plane & multihit (**HITFLAG=1)
C-                                          combine all views in this rootine
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IVIEW,IFLAG
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXMHTK.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMUHT.LINK'
C    Variable declarations
C    =====================
      INTEGER LSAHH,GZSAHH,K
      INTEGER NHIT,NST,NSC,IWIR,ITIME,IERR                      ! GTSAM1
      INTEGER NBUF,IBUF(100),NUMT
      INTEGER NMURAW,NMAX,IDUM
      INTEGER LMUD1,GZMUD1,NMUD1,MAXMUD1
      INTEGER LMUHT,GZMUHT,NSARAW,ITUBE
      INTEGER LHITS, GZHITS, OLDMUHT
      INTEGER INX, XYHITFLG,UVHITFLG
      INTEGER NT1, NT2, NT3
      INTEGER NSC1,NSC2,NSC3,NST1,NST2,NST3
      LOGICAL dfl1,dfl2,dfl3
      REAL SPAR1(3),SPAR2(3),XPAR1(3),XPAR2(3)
      REAL JBUFF(85)
      LOGICAL SAMONLY
      CHARACTER*4 HSHAPE1,HSHAPE2
      CHARACTER*12 SAMSTATION(8)
      INTEGER MAXSTA, MAXSEC
      PARAMETER (MAXSTA=6, MAXSEC=6)
      INTEGER IHEAD
      INTEGER JERR, NCEL, JHIT
      INTEGER NLAT, IADC(2), NMOD
      INTEGER JTUBE, IHIT, NHITS, I, J
      INTEGER PED_CUT, MAX_ADC
      DATA    PED_CUT/400/, MAX_ADC/4000/
      CHARACTER*3 colors(2)
      INTEGER marks(2)
      CHARACTER*15 labels(2)
      DATA colors/'MAG','MAG'/
      DATA marks/2,4/
      DATA labels/'   X or Y hit  ','   U or V hit  '/
C
      DATA SAMSTATION/' ALL SOUTH  ', 'STAT A NORTH','STAT B NORTH',
     &  'STAT C NORTH',
     &  'STAT A SOUTH','STAT B SOUTH','STAT C SOUTH',' ALL NORTH  '/
C
C    Executable code
C    ===============
      IF(iview.EQ.2) THEN
        IF (IFLAG.LT.-1.OR.IFLAG.GT.7) THEN
          WRITE(6,*)'IFLAG ERROR IN PSAMUD1_EV'
          GOTO 999
        ENDIF
      ELSE
        IF (.NOT.(iflag.EQ.1 .OR. iflag.EQ.-1)) then
          WRITE(6,*)'IFLAG ERROR IN PSAMUD1_SV'
          GOTO 999
        ENDIF
      END IF
      LMUD1 = GZMUD1()
      IF (LMUD1.LE.0) THEN
        CALL INTMSG( 'No MUD1 bank' )
        GO TO 999
      ENDIF
      NSC1 = -5
      NSC2 = -4
      NSC3 = -3
      dfl1=.false.
      dfl2=.false.
      dfl3=.false.
      CALL PUGET_l('SAMUS ONLY',SAMONLY)
      CALL PUGET_i('SAMUS MAX HITS',MAXMUD1)
      CALL PUGET_i('SAMUS DRAW XY HITS',XYHITFLG)
      CALL PUGET_i('SAMUS DRAW UV HITS',UVHITFLG)
      IF(XYHITFLG.LT.1.AND.UVHITFLG.LT.1) GOTO 999
      IF(XYHITFLG.GT.2.OR.UVHITFLG.GT.2) GOTO 999
      IF(MAXMUD1.LE.0)MAXMUD1=9999
C
C **** Create new MUHT structure
C
      LHITS=GZHITS()
      IF(LHITS.GT.0) THEN
        OLDMUHT = LQ(LHITS-IZMUHT)
        LQ(LHITS-IZMUHT) = 0
      ENDIF
      CALL BKMUHT(0, 0, LMUHT)
      CALL MUDPAK(IERR)
      NSARAW=IQ(LMUHT+4)
      IF(NSARAW.GT.MAXMUD1)THEN
        CALL INTMSG(' !!!!WARNING -too many SAMUS hits to display')
        GOTO 990
      ENDIF
      CALL PUOPEN
      CALL MUDMOD(0, NHITS, JHIT, IHEAD)
      CALL SADHIT(0, JHIT, NCEL, NLAT, IADC)
      DO NST = 1, MAXSTA         ! Station loop
C
C ****  Loop over SAMUS sections
C
        DO NSC = 1, MAXSEC             ! Section loop
          IF(iview.EQ.1.AND.(nsc.EQ.1.OR.nsc.EQ.4)) GOTO 300
          IF(iview.EQ.3.AND.(nsc.EQ.3.OR.nsc.EQ.6)) GOTO 300
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
C ****  Check for adjacent tubes
C
                IF(XYHITFLG.EQ.1.AND.(NSC.EQ.1.OR.NSC.EQ.3.OR.NSC.EQ.4.
     X            OR.NSC.EQ.6)) GOTO 33
                IF(UVHITFLG.EQ.1.AND.(NSC.EQ.2.OR.NSC.EQ.5)) GOTO 33
                GOTO 70
C
   33           NT1 = NT2
                NSC1 = NSC2
                NST1 = NST2
                dfl1=dfl2
                NSC2 = NSC3
                NST2 = NST3
                dfl2 = dfl3
                NT2 = NT3
                NSC3 = NSC
                NST3 = NST
                NT3 = jtube
                dfl3 = .false.
                IF (NT2-NT1.LE.2.AND.NST1.EQ.NST2
     &            .AND.NSC1.EQ.NSC2) THEN
                  IF (.NOT.dfl1) THEN
                    CALL drsamhit(nst1,nsc1,nt1,iview,iflag)
                    dfl1=.true.
                  END IF
                  IF (.NOT.dfl2) THEN
                    CALL drsamhit(nst2,nsc2,nt2,iview,iflag)
                    dfl2=.true.
                  END IF
                END IF
                IF (NT3-NT2.LE.2.AND.NST3.EQ.NST2
     &              .AND.NSC3.EQ.NSC2) THEN
                  IF (.NOT.dfl2) THEN
                    CALL drsamhit(nst2,nsc2,nt2,iview,iflag)
                    dfl2=.true.
                  END IF
                  IF (.NOT.dfl3) THEN
                    CALL drsamhit(nst3,nsc3,nt3,iview,iflag)
                    dfl3=.true.
                  END IF
                END IF
C
                go to 200
   70           CONTINUE
C Don't draw U or V if UVHITFLG.EQ.0
                IF((NSC.EQ.2.OR.NSC.EQ.5).AND.UVHITFLG.EQ.0) GOTO
     &                200 
C Don't draw X or Y if XYHITFLG.EQ.0
                IF((NSC.EQ.1.OR.NSC.EQ.3.OR.NSC.EQ.4.OR.NSC.EQ.6)
     X                .AND.XYHITFLG.EQ.0) GOTO 200
C
C    DON'T DRAW HITS IF NOT IN DESIRED STATIONS
C
                CALL drsamhit(nst,nsc,jtube,iview,iflag)
              END IF
  200       ENDDO ! even, odd tube
          ENDDO ! hit loop
  300   ENDDO ! NSC loop
      ENDDO  ! NST loop
C
  900 CALL JRCLOS
      IF(SAMONLY)THEN
        IF(iview.EQ.2) THEN
          INX=IFLAG
          IF(IFLAG.EQ.-1)INX=8
          CALL JVSAVE(JBUFF)
          CALL PXTITL(SAMSTATION(INX))
          CALL JVLOAD(JBUFF)
        ELSE
          IF(uvhitflg.GT.0) CALL legend_marks(colors,marks,labels,2)
        ENDIF
      END IF
C
C  Restore old MUHT structure
C
  990 LMUHT = GZMUHT(IDUM)
      CALL MZDROP( IXMAIN, LMUHT, 'V' )
      CALL MZDROP( IXMAIN, LMUHT, 'L' )
      LQ(LHITS-IZMUHT) = OLDMUHT
  999 RETURN
      END
