      SUBROUTINE PMMUD1(IVIEW,IFLAG,ICUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To display the raw hit cells
C
C  Argument Declarations:
C  ========================
C  IVIEW - Input - INTEGER - View number
C         1  Z-Y
C         2  X-Y
C         3  Z-X
C        12  X-Z
C- IFLAG- =1 IF ONLY A-LAYER
C - ICUT-1 FOR CUT VIEW
C-
C-
C-   Created  10-APR-1991 D. Hedin, S. Hagopian
C-            Based on PMCELL
C-   Updated  29-OCT-1993 T. McKibben changed GTMUHT and GTMUD1 calls
C-                        for run 1B compatibility
C-   Updated  15-NOV-1993   BH Added variable NMSCT to call GTMUHT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IVIEW,IFLAG,ICUT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXMHTK.INC/LIST'
C    Variable declarations
C    =====================
      CHARACTER*40 CALLER,MESSID,MESSAGE
      INTEGER I,K,II,JJ
      INTEGER IWADDO,DRCELLS
!!!      INTEGER IWADD,IEPHA,IEPHB,IDR1,IDR2,IOPHA,IOPHB   !GTMUD1
!!!      INTEGER IDELT1,IDELT2             ! GTMUD1
!!!      INTEGER NMOD,NPLN,NCEL,JERR       ! MUADD
      INTEGER NSPAR,NBUF,IBUF(10)        ! MUCELL
      INTEGER NMOD,NMODX,NPLNX,NWIRX,NPLN,JERR,MUNMOD
      INTEGER NHIT,JHIT,IHIT,NCEL,NLAT,LMUD1,MUNMOD2
      INTEGER NWRAW,NWPRO,NWMOD,NSRAW,NSPRO,NSMOD ! Run 1B GTMUHT
      INTEGER NMUHP,NMUOF,NVERS,LPMUOF(460),NMSCT ! Run 1B GTMUHT
      INTEGER KCOL,KINT,KFIL,KSTY,ICOL
      INTEGER IODDL,IEVENL,NRAW,NWIR,IADC(8)
      REAL MINLEN,LEN0,LEN1
      REAL SPAR(3),XPAR(3),ROTM(3,3),VOFF   ! MUCELL
      REAL SPARM(3)
      REAL XCWIR,YCWIR,ZCWIR,DX,DY,DZ
      CHARACTER*4 HSHAPE                ! MUCELL
      DATA LEN0,LEN1/100000.,90./
C
C    Executable code
C    ===============
C if MUON DRAW CELL parameter is 0, do not draw MUD1 CELLS
      CALL PUGETV('MUON DRAW CELLS',DRCELLS)
      IF(DRCELLS.EQ.0) GOTO 999
C THIS CALL TO GTMUHT HAS BEEN UPDATED FOR RUN 1B FORMAT
C TMCK
      CALL GTMUHT(NWRAW,NWPRO,NWMOD,NSRAW,NSPRO,NSMOD,
     &            NMUHP,NMUOF,NMSCT,NVERS,LPMUOF)
      IF(NWRAW.LE.0) GOTO 999
      CALL PXCOLR('RED')
C SET minimum length for cut or regular view
C this keeps cells in non-bend view from being plotted in the cut view
      IF(ICUT.EQ.1)THEN
        MINLEN=LEN1
      ELSE
        MINLEN=LEN0
      ENDIF
C LOOP OVER ALL MODULES AND LOOP OVER HITS WITHIN EACH MODULE
C
C      DO 81 K = 1,164
C        NMOD=0
C THIS CALL TO GTMUD1 NEEDS TO BE CHANGED FOR THE NEW RUN 1B FORMAT
C TMCK
C        CALL GTMUD1(K,IWADD,IEPHA,IEPHB,IDR1,IDR2,IOPHA,IOPHB,
C     X                 IDELT1,IDELT2)
C        CALL MULTCH(IWADD,IODDL,IEVENL)
C        CALL MUADD(IWADD,NMOD,NPLN,NCEL,JERR)
C NEW STUFF
C
C MODIFICATIONS FOR RUN 1B BEGIN HERE TMcK
C
C
      CALL MUDMOD(0,NRAW,JHIT,LMUD1) !INITIALIZE MUDMOD
      IF(NRAW .EQ. -1) RETURN !MUHP corrupted
      CALL MUDHIT(0,JHIT,NCEL,NLAT,IADC) !INITIALIZE MUDHIT
      DO 81  K = 1,164
        NMOD = MUNMOD(1,K)  ! index -> module number
        IF (NMOD .EQ. -1) RETURN !out of range
        CALL MUDMOD(NMOD,NRAW,JHIT,LMUD1)
        DO 70 I = 1,NRAW  !Loop over hits in module
          IHIT=JHIT
          CALL MUDHIT(IHIT,JHIT,NCEL,NLAT,IADC)
C
C I GOT THE IODDL AND IEVENL OUT OF THE MUMLAT ROUTINE CODE
C TMcK
          IODDL = NLAT/2
          IEVENL = NLAT - IODDL*2
C
C I GOT THE NWIR,NPLN, AND NCEL STUFF OUT OF THE MUHITS ROUTINE CODE
C TMcK
          CALL MUMDAT(NMOD,NMODX,NPLNX,NWIRX)
          NWIR = NCEL/4
          IF (NWIR .GE. NWIRX .OR. MOD(NWIR,2) .EQ. 1) THEN
            GOTO 70  !Goto next hit
          ENDIF
          NPLN = NCEL - NWIR*4
          IF (NPLN .GE. NPLNX) GOTO 70 !Goto next hit
C

C END OF NEW STUFF
C
!!!          IF (JERR .EQ. 0) THEN
          IF(DRCELLS.EQ.2.OR.(DRCELLS.EQ.1.AND.IEVENL.EQ.1))THEN
            CALL MUCELL(NMOD,NPLN,NCEL,HSHAPE,NSPAR,SPAR,
     X                   XPAR,ROTM,VOFF,NBUF,IBUF)
            IF ((NSPAR .NE. 0) .AND. (HSHAPE .NE. '    ')) THEN
              DO 25 II = 1,3
                SPARM(II) = 0.
                DO 40 JJ = 1,3
                  SPARM(II) = SPARM(II) + SPAR(JJ)*ROTM(II,JJ)
   40           CONTINUE
   25         CONTINUE
              XCWIR = XPAR(1)
              YCWIR = XPAR(2)
              ZCWIR = XPAR(3)
              DX = ABS(SPARM(1))
              DY = ABS(SPARM(2))
              DZ = ABS(SPARM(3))
            ENDIF
            IF(IVIEW.EQ.1)THEN
              IF(DZ.LT.MINLEN.AND.DY.LT.MINLEN)THEN
                CALL PXBOX(ZCWIR,YCWIR,XCWIR,DZ,DY,DX)
              ENDIF
            ELSE IF (IVIEW.EQ.2)THEN
              IF(DX.LT.MINLEN.AND.DY.LT.MINLEN)THEN
                CALL PXBOX(XCWIR,YCWIR,ZCWIR,DX,DY,DZ)
              ENDIF
            ELSE IF (IVIEW.EQ.3)THEN
              IF(DX.LT.MINLEN.AND.DZ.LT.MINLEN)THEN
                CALL PXBOX(ZCWIR,XCWIR,YCWIR,DZ,DX,DY)
              ENDIF
            ELSE IF (IVIEW.EQ.12)THEN
              IF(DX.LT.MINLEN.AND.DZ.LT.MINLEN)THEN
                CALL PXBOX(XCWIR,ZCWIR,YCWIR,DX,DZ,DY)
              ENDIF
            ENDIF
          ENDIF
!!!          ENDIF
          CALL MUPACK(NMOD,NPLN,NCEL+1,IWADDO,JERR)
          CALL MUADD(IWADDO,NMOD,NPLN,NCEL,JERR)
          IF (JERR .EQ. 0) THEN
            IF(DRCELLS.EQ.2.OR.(DRCELLS.EQ.1.AND.IODDL.EQ.1))THEN
              CALL MUCELL(NMOD,NPLN,NCEL,HSHAPE,NSPAR,SPAR,
     X                   XPAR,ROTM,VOFF,NBUF,IBUF)
              IF ((NSPAR .NE. 0) .AND. (HSHAPE .NE. '    ')) THEN
                DO 26 II = 1,3
                  SPARM(II) = 0.
                  DO 41 JJ = 1,3
                    SPARM(II) = SPARM(II) + SPAR(JJ)*ROTM(II,JJ)
   41             CONTINUE
   26           CONTINUE
                XCWIR = XPAR(1)
                YCWIR = XPAR(2)
                ZCWIR = XPAR(3)
                DX = ABS(SPARM(1))
                DY = ABS(SPARM(2))
                DZ = ABS(SPARM(3))
              ENDIF
              IF(IVIEW.EQ.1)THEN
                IF(DZ.LT.MINLEN.AND.DY.LT.MINLEN)THEN
                  CALL PXBOX(ZCWIR,YCWIR,XCWIR,DZ,DY,DX)
                ENDIF
              ELSE IF (IVIEW.EQ.2)THEN
                IF(DX.LT.MINLEN.AND.DY.LT.MINLEN)THEN
                  CALL PXBOX(XCWIR,YCWIR,ZCWIR,DX,DY,DZ)
                ENDIF
              ELSE IF (IVIEW.EQ.3)THEN
                IF(DX.LT.MINLEN.AND.DZ.LT.MINLEN)THEN
                  CALL PXBOX(ZCWIR,XCWIR,YCWIR,DZ,DX,DY)
                ENDIF
              ELSE IF (IVIEW.EQ.12)THEN
                IF(DX.LT.MINLEN.AND.DZ.LT.MINLEN)THEN
                  CALL PXBOX(XCWIR,ZCWIR,YCWIR,DX,DZ,DY)
                ENDIF
              ENDIF
            ENDIF
          ENDIF
   70   CONTINUE
   81 CONTINUE

  999 RETURN
      END
