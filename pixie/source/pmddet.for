C======================================================================
      SUBROUTINE PMDDET (JVIEW)
C======================================================================
C
C  Description:
C  ============
C  Draws the Muon Cosmic Ray Detector in different views
C
C  Output Arguements:
C  ==================
C  JVIEW - Tells which view to display---SEE PMEVNT FOR EXPLANATION
C          if JVIEW negative, then blownup view
C  Author:
C  =======
C  Tami Kramer
C
C  Revision History:
C  =================
C  Original Creation - October 27,1986
C  dh 7/89 new muof format
C  DH 4/90 plot toroids if PXMAG=1, add views 7-12
C- T. McKibben 3-NOV-93 Changed call to GTMUHT for run 1B compatibility
C- Updated  15-NOV-1993   BH Added variable NMSCT to call GTMUHT
C- TMcK 23-NOV-1993 Changed call to GTMUOF for run 1B compatibility
C-
C=====================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXMHTK.INC'
C
C  Local Declarations:
C  ===================
C
      INTEGER IVIEW                     ! IVIEW - the present view
      INTEGER M,NMOD,IMOD               ! M - LOOP VARIABLE
      INTEGER I,J,IMUD1
      INTEGER NHPLP,NRAW,IMUHP,NPROC,IMUOH,NSCNT,IMSCT,NHPLR
      INTEGER NWRAW,NWPRO,NWMOD,NSRAW,NSPRO,NSMOD !Run 1B GTMUHT
      INTEGER NMUHP,NMUOF,NVERS,LPMUOF(460),NMSCT !Run 1B GTMUHT
      REAL SPAR(3),XPAR(3),ROTM(3,3)
      REAL SPARM(3)
      INTEGER NBUF,IBUF,NSPAR,JVIEW,ICHNUM
      REAL MXC,MYC,MZC
      REAL MXD,MYD,MZD
      REAL XWC,YWC,ZWC
      REAL MODXC,MODYC,MODZC
      REAL MODXD,MODYD,MODZD
      CHARACTER*4 HSHAPE
      ICHNUM=CHNUM
      IF(JVIEW.LT.0) ICHNUM=0     ! NO CHAMBER NUMBERS FOR BLOWUP VIEW
      IVIEW=IABS(JVIEW)
      IF ((IVIEW .GE. 1) .AND. (IVIEW .LE. 12))  THEN
        CALL PUOPEN
C
C  Get information from STP bank and draw iron toroid...
C  =======================================================
C
        IF(PXMAG.EQ.1) THEN
          CALL MUMAGS(1,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
          IF ((NSPAR .NE. 0) .AND. (HSHAPE .NE. '    ')) THEN
            DO 20 I = 1,3
              DO 30 J = 1,3
                SPARM(I) = SPARM(I) + SPAR(J)*ROTM(I,J)
   30         CONTINUE
   20       CONTINUE
            MXC = XPAR(1)
            MYC = XPAR(2)
            MZC = XPAR(3)
            MXD = ABS(SPARM(1))
            MYD = ABS(SPARM(2))
            MZD = ABS(SPARM(3))
            CALL PXCOLR('GRE')
            IF (IVIEW .EQ. 1)
     X            CALL PXBOX(MZC,MYC,MXC,MZD,MYD,MXD)
            IF (IVIEW .EQ. 2)
     X            CALL PXBOX(MXC,MYC,MZC,MXD,MYD,MZD)
            IF (IVIEW .EQ. 3)
     X            CALL PXBOX(MXC,MZC,MYC,MXD,MZD,MYD)
            IF (IVIEW .EQ. 4)
     X            CALL PXBOX(MYC,MZC,MXC,MYD,MZD,MXD)
            IF (IVIEW .EQ. 5)
     X            CALL PXBOX(MYC,MXC,MZC,MYD,MXD,MZD)
            IF (IVIEW .EQ. 6)
     X            CALL PXBOX(MZC,MXC,MYC,MZD,MXD,MYD)
            IF (IVIEW .EQ. 7)
     X            CALL PXBOX(MZC,-MYC,MXC,MZD,MYD,MXD)
            IF (IVIEW .EQ. 8)
     X            CALL PXBOX(MXC,-MYC,MZC,MXD,MYD,MZD)
            IF (IVIEW .EQ. 9)
     X            CALL PXBOX(MXC,-MZC,MYC,MXD,MZD,MYD)
            IF (IVIEW .EQ. 10)
     X            CALL PXBOX(MYC,-MZC,MXC,MYD,MZD,MXD)
            IF (IVIEW .EQ. 11)
     X            CALL PXBOX(MYC,-MXC,MZC,MYD,MXD,MZD)
            IF (IVIEW .EQ. 12)
     X            CALL PXBOX(MZC,-MXC,MYC,MZD,MXD,MYD)
          ENDIF
        ENDIF
C
C  Draw hit muon modules...
C  ==========================================================
C
        CALL GTMUHT(NWRAW,NWPRO,NWMOD,NSRAW,NSPRO,NSMOD,
     &            NMUHP,NMUOF,NMSCT,NVERS,LPMUOF)
        IF (NWMOD .EQ. 0) THEN
          CALL JCONVW(0.,0.,XWC,YWC,ZWC)
          CALL JMOVE(XWC,YWC)
          CALL J1STRG(' NO MUON MODULES HIT IN THIS EVENT')
          GO TO 999
        ENDIF
        DO 10 IMOD = 1,NWMOD
          CALL GTMUOF(IMOD,NMOD,NRAW,IMUHP,NPROC,IMUOH,NSCNT,
     X              IMSCT,NHPLR,NHPLP,IMUD1)
          CALL MUMODU(NMOD,HSHAPE,NSPAR,SPAR,XPAR,ROTM,
     X                        NBUF,IBUF)
          IF ((NSPAR .NE. 0) .AND. (HSHAPE .NE. '    ')) THEN
            DO 25 I = 1,3
              SPARM(I) = 0.
              DO 35 J = 1,3
                SPARM(I) = SPARM(I) + SPAR(J)*ROTM(I,J)
   35         CONTINUE
   25       CONTINUE
            MODXC = (XPAR(1))
            MODYC = (XPAR(2))
            MODZC = (XPAR(3))
            MODXD = ABS(SPARM(1))
            MODYD = ABS(SPARM(2))
            MODZD = ABS(SPARM(3))
C
C  If chnum = 0 then the chamber numbers are not put on the display
C
            IF (ICHNUM.EQ.0) THEN
              IF (IVIEW.EQ.1) CALL PXBOX(MODZC,MODYC,
     X                     MODXC,MODZD,MODYD,MODXD)
              IF (IVIEW.EQ.2) CALL PXBOX(MODXC,MODYC,
     X                     MODZC,MODXD,MODYD,MODZD)
              IF (IVIEW.EQ.3) CALL PXBOX(MODXC,MODZC,
     X                     MODYC,MODXD,MODZD,MODYD)
              IF (IVIEW.EQ.4) CALL PXBOX(MODYC,MODZC,
     X                     MODXC,MODYD,MODZD,MODXD)
              IF (IVIEW.EQ.5) CALL PXBOX(MODYC,MODXC,
     X                     MODZC,MODYD,MODXD,MODZD)
              IF (IVIEW.EQ.6) CALL PXBOX(MODZC,MODXC,
     X                     MODYC,MODZD,MODXD,MODYD)
              IF (IVIEW.EQ.7) CALL PXBOX(MODZC,-MODYC,
     X                     MODXC,MODZD,MODYD,MODXD)
              IF (IVIEW.EQ.8) CALL PXBOX(MODXC,-MODYC,
     X                     MODZC,MODXD,MODYD,MODZD)
              IF (IVIEW.EQ.9) CALL PXBOX(MODXC,-MODZC,
     X                     MODYC,MODXD,MODZD,MODYD)
              IF (IVIEW.EQ.10) CALL PXBOX(MODYC,-MODZC,
     X                     MODXC,MODYD,MODZD,MODXD)
              IF (IVIEW.EQ.11) CALL PXBOX(MODYC,-MODXC,
     X                     MODZC,MODYD,MODXD,MODZD)
              IF (IVIEW.EQ.12) CALL PXBOX(MODZC,-MODXC,
     X                     MODYC,MODZD,MODXD,MODYD)
            ENDIF
            IF (ICHNUM.NE.0) THEN
              IF (IVIEW .EQ. 1) THEN
                CALL PXBOX(MODZC,MODYC,MODXC,MODZD,MODYD,MODXD)
                CALL PMCHNM(NMOD,MODZC,MODYC,
     X                       MODXC,MODZD,MODYD,MODXD)
              ENDIF
              IF (IVIEW .EQ. 2) THEN
                CALL PXBOX(MODXC,MODYC,MODZC,MODXD,MODYD,MODZD)
                CALL PMCHNM(NMOD,MODXC,MODYC,
     X                       MODZC,MODXD,MODYD,MODZD)
              ENDIF
              IF (IVIEW .EQ. 3) THEN
                CALL PXBOX(MODXC,MODZC,MODYC,MODXD,MODZD,MODYD)
                CALL PMCHNM(NMOD,MODXC,MODZC,
     X                     MODYC,MODXD,MODZD,MODYD)
              ENDIF
              IF (IVIEW .EQ. 4) THEN
                CALL PXBOX(MODYC,MODZC,MODXC,MODYD,MODZD,MODXD)
                CALL PMCHNM(NMOD,MODYC,MODZC,
     X                       MODXC,MODYD,MODZD,MODXD)
              ENDIF
              IF (IVIEW .EQ. 5) THEN
                CALL PXBOX(MODYC,MODXC,MODZC,MODYD,MODXD,MODZD)
                CALL PMCHNM(NMOD,MODYC,MODXC,
     X                       MODZC,MODYD,MODXD,MODZD)
              ENDIF
              IF (IVIEW .EQ. 6) THEN
                CALL PXBOX(MODZC,MODXC,MODYC,MODZD,MODXD,MODYD)
                CALL PMCHNM(NMOD,MODZC,MODXC,
     X                     MODYC,MODZD,MODXD,MODYD)
              ENDIF
              IF (IVIEW .EQ. 7) THEN
                CALL PXBOX(MODZC,-MODYC,MODXC,MODZD,MODYD,MODXD)
                CALL PMCHNM(NMOD,MODZC,-MODYC,
     X                       MODXC,MODZD,MODYD,MODXD)
              ENDIF
              IF (IVIEW .EQ. 8) THEN
                CALL PXBOX(MODXC,-MODYC,MODZC,MODXD,MODYD,MODZD)
                CALL PMCHNM(NMOD,MODXC,-MODYC,
     X                       MODZC,MODXD,MODYD,MODZD)
              ENDIF
              IF (IVIEW .EQ. 9) THEN
                CALL PXBOX(MODXC,-MODZC,MODYC,MODXD,MODZD,MODYD)
                CALL PMCHNM(NMOD,MODXC,-MODZC,
     X                     MODYC,MODXD,MODZD,MODYD)
              ENDIF
              IF (IVIEW .EQ. 10) THEN
                CALL PXBOX(MODYC,-MODZC,MODXC,MODYD,MODZD,MODXD)
                CALL PMCHNM(NMOD,MODYC,-MODZC,
     X                       MODXC,MODYD,MODZD,MODXD)
              ENDIF
              IF (IVIEW .EQ. 11) THEN
                CALL PXBOX(MODYC,-MODXC,MODZC,MODYD,MODXD,MODZD)
                CALL PMCHNM(NMOD,MODYC,-MODXC,
     X                       MODZC,MODYD,MODXD,MODZD)
              ENDIF
              IF (IVIEW .EQ. 12) THEN
                CALL PXBOX(MODZC,-MODXC,MODYC,MODZD,MODXD,MODYD)
                CALL PMCHNM(NMOD,MODZC,-MODXC,
     X                     MODYC,MODZD,MODXD,MODYD)
              ENDIF
            ENDIF
          ENDIF
C
   10   CONTINUE
C
  999   CONTINUE
        CALL JRCLOS        !(SEGMENT 100+IVIEW)
      ENDIF
      RETURN
      END
