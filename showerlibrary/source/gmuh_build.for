      SUBROUTINE GMUH_BUILD (HITSM,IHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  STUFF MUON HITS information into
C-                          GMUH (ZEBRA data structure like JHITS)
C-
C-   Inputs  : HITSM(9)  [R]  - MUON HITS ARAY
C-             IHIT      [I]  - HIT COUNT FOR THIS ISET,IDET,ITRA
C-   Outputs :
C-   Controls: SMUO(4) - minimum distance (cm) between stored hits
C-
C-   Created   1-APR-1993   Chip Stewart AND Jasbir Singh
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:GCTRAK.INC/LIST'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCSETS.INC/LIST'
      INCLUDE 'D0$INC:GCVOLU.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:CALTRK.INC/LIST'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INTEGER IHIT,NWIRE,NWHIT,I,J,K,L,M,N,NR,LGMUH,GZGMUH,NVERT,NPART
      INTEGER LISV1,LISP1,IGMUH,GZISV1,LZFIND,MAXPUNCH,ITRA1,NPUNCH
      PARAMETER( NWHIT = 9)
      PARAMETER( MAXPUNCH = 500)
      REAL HITSM(NWHIT),XYZLAST(3),DIST,PEXIT(7,MAXPUNCH)
      CHARACTER*4 ANAME
      SAVE XYZLAST
C----------------------------------------------------------------------
C
C ****  Make sure we've gone at least SMUO(4) (cm) before storing next hit
C
      DIST = (XYZLAST(1)-VECT(1))**2+(XYZLAST(2)-VECT(2))**2+
     &       (XYZLAST(3)-VECT(3))**2
      DIST = SQRT(ABS(DIST))
      IF (DIST.LT.SMUO(4) ) GO TO 999
      
C
C ****  FIGURE OUT MODULE,PLANE AND WIRE HIT
C
      CALL MSGNUM(NAMES(NLEVEL),ANAME,NWIRE)
C
C
C ****  write into GMUH if in A B or C layers
C
      IF(ANAME(1:1).NE.'A'.AND.ANAME(1:1).NE.'B'.
     +                              AND.ANAME(1:1).NE.'C') GOTO 999
      CALL UCOPY(VECT,XYZLAST,3) 
C
C ****  IS this a new track?
C
      LGMUH = GZGMUH(ITRA)
      IF(IHIT.EQ.1.AND.LGMUH.LE.0) THEN ! PREPARE GMUH STRUCTURE HEADER
        CALL BKGMUH(ITRA,LGMUH)
        IF(LGMUH.LE.0) THEN
          CALL ERRMSG('NO GMUH','GMUH_BUILD',' NOT BOOKED FOR ITRA','W')
          GOTO 999
        END IF
C
        CALL FLGMUH(ITRA,LGMUH)
C
C FIND ISV1, ISP1 for reference link
C
        NVERT = IQ(LGMUH+16)
        LISV1 = GZISV1()
        LISV1 = LZFIND(IXCOM,LISV1,NVERT,-5)
        IF(LISV1.LE.0) THEN
          CALL ERRMSG('BAD VERTEX NUMBER','GMUH_BUILD','UBUF?','W')
          GOTO 999
        END IF
        NPART = IQ(LGMUH+17)
        LISP1 = LQ(LISV1-IZISP1)
        LISP1 = LZFIND(IXCOM,LISP1,NPART,-5)
        IF(LISP1.LE.0) THEN
          CALL ERRMSG('BAD PARTICLE NUMBER','GMUH_BUILD','UBUF?','W')
          GOTO 999
        END IF
        LQ(LGMUH-2) = LISP1
C
C ****  WRITE PUCHTHROGH FROM MCAL (STORED FROM MCALEXIT) 
C
        IF ( (NPUNCH.EQ.0).OR.(NPUNCH.GT.MAXPUNCH) ) THEN
          CALL ERRMSG('NPUNCH EXCEEDED','GMUH_BUILD','OOPS','W')
        ELSE
          CALL UCOPY(PEXIT(1,NPUNCH),Q(LGMUH+18),7)  !NPUNCH MAY NOT BE RIGHT???
        END IF
C
      END IF
      LGMUH = GZGMUH(ITRA)
      IF(LGMUH.LE.0) THEN
        CALL ERRMSG('GMUH LOST','GMUH_BUILD',' MISSING FOR ITRA','W')
        GOTO 999
      END IF
      NR = IQ(LGMUH+3)
      IGMUH = IQ(LGMUH-1)
      CALL MZPUSH(IXMAIN,LGMUH,0,NR,'I') ! Make room
C
C ****  Fill in JHITS info for this HIT
C
      IF(NVNAME.NE.2) CALL ERRMSG('BAD NVNAME','GMUH_BUILD','BAD','W')
      IQ(LGMUH+IGMUH+1) = ISET
      IQ(LGMUH+IGMUH+2) = IDET
      IQ(LGMUH+IGMUH+3) = ITRA
      IQ(LGMUH+IGMUH+4) = NUMBV(1)
      IQ(LGMUH+IGMUH+5) = NUMBV(2)
      CALL UCOPY(HITSM,Q(LGMUH+IGMUH+6),NWHIT)
      IQ(LGMUH+IGMUH+NR) = IHIT
      IQ(LGMUH+25) = IQ(LGMUH+25)+1
C      
  999 RETURN
C#######################################################################
      ENTRY GMUH_BUILD_EXIT(ITRA1)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  STORE TRACK PARAMETERS LEAVING MCAL
C-
C-   Inputs  : ITRA1 - track number
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  23-APR-1993   Jasbir Singh
C-
C----------------------------------------------------------------------
      NPUNCH = NPUNCH + 1
      IF(NPUNCH.GT.MAXPUNCH) THEN
        CALL ERRMSG('MAXPUNCH EXCEEDED','GMUH_BUILD','WOW','W')
        GOTO 1999
      END IF
      CALL UCOPY(VECT(1),PEXIT(1,NPUNCH),3)
C            CALL UCOPY(VECT(1),CAL_XYZ_EXIT(1),3)
      PEXIT(4,NPUNCH) = VECT(4)*VECT(7)
      PEXIT(5,NPUNCH) = VECT(5)*VECT(7)
      PEXIT(6,NPUNCH) = VECT(6)*VECT(7)
      PEXIT(7,NPUNCH) = GETOT
C            CAL_P_EXIT(5) = Ipart
c-----------------------------------------------------------------

C----------------------------------------------------------------------
 1999 RETURN
      ENTRY GMUH_RESET_EXIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  RESET TRACK PARAMETERS LEAVING MCAL
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  23-APR-1993   Jasbir Singh
C-
C----------------------------------------------------------------------
      NPUNCH = 0
C----------------------------------------------------------------------
      RETURN
      END
