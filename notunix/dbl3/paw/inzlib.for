C
C - INZLIB160A.FOR  to go with dbl3 version 308
C
      SUBROUTINE INZADZ
*
************************************************************************
*
*        SUBR. INZADZ
*
*   Action Routines for menu /ZEBRA/DZ (interactive)
*
*   Allowed Actions :
*
*     DZSHOW, DZFORM, DZSTOR, DZAREA, DZSURV, DZSNAP, DZVERI
*
*   Called by KUIP routine
*
************************************************************************
*
      COMMON /ZUNIT/  IQREAD, IQPRNT, IQPR2, IQLOG, IQPNCH, IQTTIN,
     +                IQTYPE
      CHARACTER       CPATL*32, CHOPT*32, CHIDH*4, CTEXT*16, CHLA*16
*
*     ------------------------------------------------------------------
*
      CALL KUPATL (CPATL, NPAR)
*
      IF (CPATL.EQ.'DZSHOW') THEN
*
*  **   DZSHOW
*
        IXDIV = 2
        CHOPT = 'BHV'
        IL1   = 0
        IL2   = 0
        ID1   = 0
        ID2   = 0
        CTEXT = 'DZSHOW'
        CALL KUGETC (CHIDH, NCH)
        CALL KUGETI (IDN)
        CALL KUGETI (IXDIV)
        CALL KUGETC (CHOPT, NCH)
        CALL KUGETI (IL1)
        CALL KUGETI (IL2)
        CALL KUGETI (ID1)
        CALL KUGETI (ID2)
        CALL KUGETC (CTEXT, NCH)
        CALL UCTOH  (CHIDH, IDH, 4, 4)
        LBANK = LZFID (IXDIV, IDH, IDN, 0)
        IF (LBANK.EQ.0) THEN
          WRITE (IQPRNT, 1001) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        CALL DZSHOW (CTEXT, IXDIV, LBANK, CHOPT, IL1, IL2, ID1, ID2)
*
      ELSE IF (CPATL.EQ.'DZFORM') THEN
*
*  **   DZFORM
*
        IXDIV = 2
        CALL KUGETC (CHIDH, NCH)
        CALL KUGETI (IDN)
        CALL KUGETI (IXDIV)
        CALL UCTOH  (CHIDH, IDH, 4, 4)
        LBANK = LZFID (IXDIV, IDH, IDN, 0)
        IF (LBANK.EQ.0) THEN
          WRITE (IQPRNT, 1001) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        CALL DZFORM (IXDIV, LBANK)
*
      ELSE IF (CPATL.EQ.'DZSTOR') THEN
*
*  **   DZSTOR
*
        ISTOR = 0
        CTEXT = 'DZSTOR'
        CALL KUGETC (CTEXT, NCH)
        CALL KUGETI (ISTOR)
        CALL DZSTOR (CTEXT, ISTOR)
*
      ELSE IF (CPATL.EQ.'DZAREA') THEN
*
*  **   DZAREA
*
        ISTOR = 0
        CTEXT = 'DZAREA'
        CALL KUGETC (CHLA, NCH)
        CALL KUGETC (CTEXT, NCH)
        CALL KUGETI (ISTOR)
        CALL DZAREA (CTEXT, ISTOR, CHLA, 0, 'N')
*
      ELSE IF (CPATL.EQ.'DZSURV') THEN
*
*  **   DZSURV
*
        IXDIV = 2
        CTEXT = 'DZSURV'
        CALL KUGETC (CHIDH, NCH)
        CALL KUGETI (IDN)
        CALL KUGETI (IXDIV)
        CALL KUGETC (CTEXT, NCH)
        CALL UCTOH  (CHIDH, IDH, 4, 4)
        LBANK = LZFID (IXDIV, IDH, IDN, 0)
        IF (LBANK.EQ.0) THEN
          WRITE (IQPRNT, 1001) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        CALL DZSURV (CTEXT, IXDIV, LBANK)
*
      ELSE IF (CPATL.EQ.'DZSNAP') THEN
*
*  **   DZSNAP
*
        IXDIV = 2
        CTEXT = 'DZSNAP'
        CHOPT = 'M'
        CALL KUGETI (IXDIV)
        CALL KUGETC (CHOPT, NCH)
        CALL KUGETC (CTEXT, NCH)
        CALL DZSNAP (CTEXT, IXDIV, CHOPT)
*
      ELSE IF (CPATL.EQ.'DZVERI') THEN
*
*  **   DZVERI
*
        IXDIV = 2
        CTEXT = 'DZVERI'
        CHOPT = 'CLSU'
        CALL KUGETI (IXDIV)
        CALL KUGETC (CHOPT, NCH)
        CALL KUGETC (CTEXT, NCH)
        CALL DZVERI (CTEXT, IXDIV, CHOPT)
*
      ENDIF
*
 1001 FORMAT (  ' INZADZ : Cannot find bank ',A4,I12,' in division ',
     +          I10)
*                                                             END INZADZ
  999 END
      SUBROUTINE INZAFZ
*
************************************************************************
*
*        SUBR. INZAFZ
*
*   Action Routines for menu /ZEBRA/FZ (interactive)
*
*   Allowed Actions :
*
*     FZFILE, FZLOGL, FZLIMI, FZENDI, FZENDO
*
*   Called by KUIP routine
*
************************************************************************
*
      COMMON /ZUNIT/  IQREAD, IQPRNT, IQPR2, IQLOG, IQPNCH, IQTTIN,
     +                IQTYPE
      COMMON /QUEST/  IQUEST(100)
      CHARACTER       CPATL*32, CHOPT*32
*
*     ------------------------------------------------------------------
*
      CALL KUPATL (CPATL, NPAR)
*
      IF (CPATL.EQ.'FZFILE') THEN
*
*  **   FZFILE
*
        CALL INZFOP (LUN)
*
      ELSE IF (CPATL.EQ.'FZLOGL') THEN
*
*  **   FZLOGL
*
        LOGL   = 0
        CALL KUGETI (LUN)
        CALL KUGETI (LOGL)
        CALL FZLOGL (LUN, LOGL)
*
      ELSE IF (CPATL.EQ.'FZLIMI') THEN
*
*  **   FZLIMI
*
        ALIM   = -1.
        CALL KUGETI (LUN)
        CALL KUGETR (ALIM)
        CALL FZLIMI (LUN, ALIM)
*
      ELSE IF (CPATL.EQ.'FZENDI') THEN
*
*  **   FZENDI
*
        CHOPT  = 'T'
        CALL KUGETI (LUN)
        CALL KUGETC (CHOPT, NCH)
        CALL FZENDI (LUN, CHOPT)
        IF (LUN.GT.0) THEN
          CLOSE (LUN)
        ENDIF
*
      ELSE IF (CPATL.EQ.'FZENDO') THEN
*
*  **   FZENDO
*
        CHOPT  = 'TE'
        CALL KUGETI (LUN)
        CALL KUGETC (CHOPT, NCH)
        CALL FZENDO (LUN, CHOPT)
        IF (LUN.GT.0) THEN
          CLOSE (LUN)
        ENDIF
*
      ENDIF
*                                                             END INZAFZ
  999 END
      SUBROUTINE INZAMZ
*
************************************************************************
*
*        SUBR. INZAMZ
*
*   Action Routines for menu /ZEBRA/MZ (interactive)
*
*   Allowed Actions :
*
*     MZDIV, MZLOGL, MZBOOK, MZPUSH, MZDROP, MZWIPE, LZFIDH, LZFIDN,
*     LZLAST, LZFIND, LZBYT, LZFVAL, NZBANK, NZFIND
*
*   Called by KUIP routine
*
************************************************************************
*
      COMMON /ZUNIT/  IQREAD, IQPRNT, IQPR2, IQLOG, IQPNCH, IQTTIN,
     +                IQTYPE
      COMMON /QUEST/  IQUEST(100)
      CHARACTER       CPATL*32, CHOPT*32, CHIDH*4, CHIO*80, CNAME*16
      PARAMETER       (NWIO=16)
      DIMENSION       NIO(NWIO)
*
*     ------------------------------------------------------------------
*
      CALL KUPATL (CPATL, NPAR)
*
      IF (CPATL.EQ.'MZDIV') THEN
*
*  **   MZDIV
*
        CALL KUGETI (ISTOR)
        CALL KUGETC (CNAME, NCH)
        CALL KUGETI (NWMIN)
        CALL KUGETI (NWMAX)
        CALL KUGETC (CHOPT, NCH)
        NWMAX = MAX (NWMIN, 10000)
        CALL MZDIV (ISTOR, IXDIV, CNAME, NWMIN, NWMAX, CHOPT)
        WRITE (IQPRNT, 1001) IXDIV, CNAME, CHOPT
*
      ELSE IF (CPATL.EQ.'MZLOGL') THEN
*
*  **   MZLOGL
*
        CALL KUGETI (ISTOR)
        CALL KUGETI (LOGL)
        CALL MZLOGL (ISTOR, LOGL)
*
      ELSE IF (CPATL.EQ.'MZBOOK') THEN
*
*  **   MZBOOK
*
        CALL KUGETI (IXDIV)
        CALL KUGETI (JBIAS)
        CALL KUGETC (CHIDH, NCH)
        CALL KUGETI (NL)
        CALL KUGETI (NS)
        CALL KUGETI (ND)
        NIO(1)= 2
        NS    = MIN (NS, NL)
        NZ    = -1
        CALL KUGETC (CHIO, NCH)
        IF (NCH.EQ.1) THEN
          IF (CHIO(1:1).EQ.'0') THEN
            NIO(1) = 0
          ELSE IF (CHIO(1:1).EQ.'1') THEN
            NIO(1) = 1
          ELSE IF (CHIO(1:1).EQ.'2') THEN
            NIO(1) = 2
          ELSE IF (CHIO(1:1).EQ.'3') THEN
            NIO(1) = 3
          ELSE IF (CHIO(1:1).EQ.'4') THEN
            NIO(1) = 4
          ELSE IF (CHIO(1:1).EQ.'5') THEN
            NIO(1) = 5
          ELSE IF (CHIO(1:1).EQ.'7') THEN
            NIO(1) = 7
          ELSE
            CALL MZIOCH (NIO, NWIO, CHIO)
          ENDIF
        ELSE
          CALL MZIOCH (NIO, NWIO, CHIO)
        ENDIF
        CALL KUGETI (NZ)
        IF (JBIAS.LE.0) THEN
          CALL KUPROC ('Name   of the Supporting Bank', CHOPT, NCH)
          CALL KUPROI ('Number of the Supporting Bank', IDN)
          CALL UCTOH (CHOPT, IDH, 4, 4)
          LSUP  = LZFID (IXDIV, IDH, IDN, 0)
          IF (LSUP.EQ.0) THEN
            WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
            GO TO 999
          ENDIF
        ELSE IF (JBIAS.EQ.2) THEN
          LSUP  = 0
        ELSE
          WRITE (IQPRNT, 1003) JBIAS
        ENDIF
        CALL MZBOOK (IXDIV, L, LSUP, JBIAS, CHIDH, NL, NS, ND, NIO, NZ)
        WRITE (IQPRNT, 1004) CHIDH, L, JBIAS, LSUP
*
      ELSE IF (CPATL.EQ.'MZPUSH') THEN
*
*  **   MZPUSH
*
        CALL KUGETC (CHIDH, NCH)
        CALL KUGETI (IDN)
        CALL UCTOH  (CHIDH, IDH, 4, 4)
        CALL KUGETI (IXDIV)
        L     = LZFID (IXDIV, IDH, IDN, 0)
        IF (L.EQ.0) THEN
          WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        CALL KUGETI (INCNL)
        CALL KUGETI (INCND)
        CALL KUGETC (CHOPT, NCH)
        CALL MZPUSH (IXDIV, L, INCNL, INCND, CHOPT)
*
      ELSE IF (CPATL.EQ.'MZDROP') THEN
*
*  **   MZDROP
*
        CALL KUGETC (CHIDH, NCH)
        CALL KUGETI (IDN)
        CALL UCTOH  (CHIDH, IDH, 4, 4)
        CALL KUGETI (IXDIV)
        L     = LZFID (IXDIV, IDH, IDN, 0)
        IF (L.EQ.0) THEN
          WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        CALL KUGETC (CHOPT, NCH)
        CALL MZDROP (IXDIV, L, CHOPT)
*
      ELSE IF (CPATL.EQ.'MZWIPE') THEN
*
*  **   MZWIPE
*
        CALL KUGETI (IXDIV)
        CALL MZWIPE (IXIDV)
*
      ELSE IF (CPATL.EQ.'MZNEED') THEN
*
*  **   MZNEED
*
        CALL KUGETI (NEEDW)
        CALL KUGETI (IXDIV)
        CALL KUGETC (CHOPT, NCH)
        CALL MZNEED (IXDIV, NEEDW, CHOPT)
*
      ELSE IF (CPATL.EQ.'LZFIDH') THEN
*
*  **   LZFIDH
*
        CALL KUGETC (CHIDH, NCH)
        CALL UCTOH  (CHIDH, IDH, 4, 4)
        CALL KUGETI (IXDIV)
        CALL KUGETI (LGO)
        LF    = LZFIDH (IXDIV, IDH, LGO)
        WRITE (IQPRNT, 1005) IDH, IXDIV, LF
*
      ELSE IF (CPATL.EQ.'LZFIDN') THEN
*
*  **   LZFIDN
*
        CALL KUGETC (CHIDH, NCH)
        CALL UCTOH  (CHIDH, IDH, 4, 4)
        CALL KUGETI (IDN)
        CALL KUGETI (IXDIV)
        CALL KUGETI (LGO)
        LF    = LZFID  (IXDIV, IDH, IDN, LGO)
        WRITE (IQPRNT, 1006) IDH, IDN, IXDIV, LF
*
      ELSE IF (CPATL.EQ.'LZLAST') THEN
*
*  **   LZLAST
*
        CALL KUGETC (CHIDH, NCH)
        CALL KUGETI (IDN)
        CALL UCTOH  (CHIDH, IDH, 4, 4)
        CALL KUGETI (IXDIV)
        L     = LZFID (IXDIV, IDH, IDN, 0)
        IF (L.EQ.0) THEN
          WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        LF    = LZLAST (IXDIV, L)
        WRITE (IQPRNT, 1007) IDH, IDN, IXDIV, LF
*
      ELSE IF (CPATL.EQ.'LZFIND') THEN
*
*  **   LZFIND
*
        CALL KUGETC (CHIDH, NCH)
        CALL KUGETI (IDN)
        CALL UCTOH  (CHIDH, IDH, 4, 4)
        CALL KUGETI (IT)
        CALL KUGETI (JW)
        CALL KUGETI (IXDIV)
        L     = LZFID (IXDIV, IDH, IDN, 0)
        IF (L.EQ.0) THEN
          WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        LLS   = LZHEAD (IXDIV, L)
        IF (LLS.EQ.0) THEN
          WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        LF    = LZFIND (IXDIV, LLS, IT, JW)
        WRITE (IQPRNT, 1008) IDH, IDN, IXDIV, JW, IT, LF
*
      ELSE IF (CPATL.EQ.'LZBYT') THEN
*
*  **   LZBYT
*
        CALL KUGETC (CHIDH, NCH)
        CALL KUGETI (IDN)
        CALL UCTOH  (CHIDH, IDH, 4, 4)
        CALL KUGETI (IT)
        CALL KUGETI (JB)
        CALL KUGETI (NB)
        CALL KUGETI (IXDIV)
        L     = LZFID (IXDIV, IDH, IDN, 0)
        IF (L.EQ.0) THEN
          WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        LLS   = LZHEAD (IXDIV, L)
        IF (LLS.EQ.0) THEN
          WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        LF    = LZBYT  (IXDIV, LLS, IT, JB, NB)
        WRITE (IQPRNT, 1009) IDH, IDN, IXDIV, JB, NB, IT, LF
*
      ELSE IF (CPATL.EQ.'LZFVAL') THEN
*
*  **   LZFVAL
*
        CALL KUGETC (CHIDH, NCH)
        CALL KUGETI (IDN)
        CALL UCTOH  (CHIDH, IDH, 4, 4)
        CALL KUGETR (VAL)
        CALL KUGETR (TOL)
        CALL KUGETI (JW)
        CALL KUGETI (IXDIV)
        L     = LZFID (IXDIV, IDH, IDN, 0)
        IF (L.EQ.0) THEN
          WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        LLS   = LZHEAD (IXDIV, L)
        IF (LLS.EQ.0) THEN
          WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        LF    = LZFVAL (IXDIV, LLS, VAL, TOL, JW)
        WRITE (IQPRNT, 1010) IDH, IDN, IXDIV, JW, VAL, TOL, LF
*
      ELSE IF (CPATL.EQ.'NZBANK') THEN
*
*  **   NZBANK
*
        CALL KUGETC (CHIDH, NCH)
        CALL KUGETI (IDN)
        CALL UCTOH  (CHIDH, IDH, 4, 4)
        CALL KUGETI (IXDIV)
        L     = LZFID (IXDIV, IDH, IDN, 0)
        IF (L.EQ.0) THEN
          WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        LLS   = LZHEAD (IXDIV, L)
        IF (LLS.EQ.0) THEN
          WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        N     = NZBANK (IXDIV, LLS)
        WRITE (IQPRNT, 1011) IDH, IDN, IXDIV, N
*
      ELSE IF (CPATL.EQ.'NZFIND') THEN
*
*  **   NZFIND
*
        CALL KUGETC (CHIDH, NCH)
        CALL KUGETI (IDN)
        CALL UCTOH  (CHIDH, IDH, 4, 4)
        CALL KUGETI (IT)
        CALL KUGETI (JW)
        CALL KUGETI (IXDIV)
        L     = LZFID (IXDIV, IDH, IDN, 0)
        IF (L.EQ.0) THEN
          WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        LLS   = LZHEAD (IXDIV, L)
        IF (LLS.EQ.0) THEN
          WRITE (IQPRNT, 1002) IDH, IDN, IXDIV
          GO TO 999
        ENDIF
        N     = NZFIND (IXDIV, LLS, IT, JW)
        WRITE (IQPRNT, 1012) IDH, IDN, IXDIV, JW, IT, N
        IF (N.GT.0) THEN
          NMAX  = MIN (N, 100)
          WRITE (IQPRNT, 1013) (IQUEST(I), I = 1, NMAX)
        ENDIF
*
      ENDIF
*
 1001 FORMAT (/,' INZAMZ : Division ',A8,I12,' created with CHOPT ',A)
 1002 FORMAT (  ' INZAMZ : Cannot find bank ',A4,I12,' in division ',
     +          I10)
 1003 FORMAT (  ' INZAMZ : JBIAS = ',I6,' not supported')
 1004 FORMAT (/,' INZAMZ : Bank ',A4,' created at address ',I12,
     +          ' JBIAS = ',I6,' LSUP = ',I12)
 1005 FORMAT (/,' INZAMZ : Bank ',A4,' in division ',I10,
     +          ' is at address ',I12)
 1006 FORMAT (/,' INZAMZ : Bank ',A4,I12,' in division ',I10,
     +          ' is at address ',I12)
 1007 FORMAT (/,' INZAMZ : Last bank in the linear structure with ',A4,
     +          I12,' in division ',I10,' is at address ',I12)
 1008 FORMAT (/,' INZAMZ : First bank in the linear structure with ',A4,
     +          I12,' in division ',I10,' with word ',I5,' = ',I12,
     +          ' is at address ',I12)
 1009 FORMAT (/,' INZAMZ : First bank in the linear structure with ',A4,
     +          I12,' in division ',I10,' with status byte ',2I3,' = ',
     +          Z6,' is at address ',I12)
 1010 FORMAT (/,' INZAMZ : First bank in the linear structure with ',A4,
     +          I12,' in division ',I10,' with word ',I5,' = ',G12.4,
     +          ' within ',G12.4,' is at address ',I12)
 1011 FORMAT (/,' INZAMZ : Numb. of banks in the linear structure with '
     +         ,A4,I12,' in division ',I10,' is ',I6)
 1012 FORMAT (/,' INZAMZ : Numb. of banks in the linear structure with '
     +         ,A4,I12,' in division ',I10,' with word ',I5,' = ',I12,
     +          ' is ',I6,' Addresses ')
 1013 FORMAT (8X,10I12)
*                                                             END INZAMZ
  999 END
      SUBROUTINE INZARZ
*
************************************************************************
*
*        SUBR. INZARZ
*
*   Action Routines for menu /ZEBRA/RZ (interactive)
*
*   Allowed Actions :
*
*     RZMAKE, RZFILE, RZLOGL, RZEND,  RZSAVE, RZQUOT, RZLOCK, RZFREE,
*     RZLLOK, RZNDIR, RZCDIR, RZLDIR, RZMDIR, RZPURG, RZDELT, RZSTAT,
*     RZKEYD, RZTOFZ, RZFRFZ, RZTOALPHA, RZFRALPHA
*
*   Called by KUIP routine
*
************************************************************************
*
      COMMON /ZUNIT/  IQREAD, IQPRNT, IQPR2, IQLOG, IQPNCH, IQTTIN,
     +                IQTYPE
      COMMON /QUEST/  IQUEST(100)
      CHARACTER       CPATL*32, CHOPT*32, CHDIR*80, CFORM*100, CPATH*80
      CHARACTER       CHTAG(100)*8, CHPRO*8, CLOCK*16, CHOP*8
*
*     ------------------------------------------------------------------
*
      CALL KUPATL (CPATL, NPAR)
*
      IF (CPATL.EQ.'RZMAKE') THEN
*
*  **   RZMAKE
*
        CALL INZROP ('MAKE', LUN, CHOPT)
        IF (IQUEST(1).NE.0)           GO TO 999
        NREC   = 4000
        NWKEY  = 9
        CFORM  = 'IIIIIIIII'
        CALL KUGETC (CHDIR, NCH)
        CALL KUGETI (NREC)
        CALL KUGETI (NWKEY)
        CALL KUGETC (CFORM)
        DO 10 I = 1, NWKEY
          WRITE (CHPRO, 1000) I
          CALL KUPROC (CHPRO, CHTAG(I), NCH)
   10   CONTINUE
        CALL RZMAKE (LUN, CHDIR, NWKEY, CFORM, CHTAG, NREC, CHOPT)
        IF (IQUEST(1).NE.0) WRITE (IQPRNT, 1001) 'RZMAKE', CHDIR
*
      ELSE IF (CPATL.EQ.'RZFILE') THEN
*
*  **   RZFILE
*
        CALL INZROP ('FILE', LUN, CHOPT)
        IF (IQUEST(1).NE.0)           GO TO 999
        CALL KUGETC (CHDIR, NCH)
        CALL RZFILE (LUN, CHDIR, CHOPT)
        IF (IQUEST(1).NE.0) WRITE (IQPRNT, 1001) 'RZFILE', CHDIR
*
      ELSE IF (CPATL.EQ.'RZLOGL') THEN
*
*  **   RZLOGL
*
        LOGL   = 0
        CALL KUGETI (LUN)
        CALL KUGETI (LOGL)
        CALL RZLOGL (LUN, LOGL)
*
      ELSE IF (CPATL.EQ.'RZEND') THEN
*
*  **   RZEND
*
        CALL KUGETC (CHDIR, NCH)
        CALL RZEND  (CHDIR)
*
      ELSE IF (CPATL.EQ.'RZSAVE') THEN
*
*  **   RZSAVE
*
        CALL RZSAVE
*
      ELSE IF (CPATL.EQ.'RZQUOT') THEN
*
*  **   RZQUOT
*
        NQUOT  = 1000
        CALL KUGETC (CHDIR, NCH)
        CALL KUGETI (NQUOT)
        CALL RZCDIR (CHDIR, ' ')
        IF (IQUEST(1).NE.0)           GO TO 999
        CALL RZQUOT (NQUOT)
*
      ELSE IF (CPATL.EQ.'RZLOCK') THEN
*
*  **   RZLOCK
*
        CALL KUGETC (CPATH, NCH)
        CALL KUGETC (CLOCK, NCH)
        CALL RZCDIR (CHDIR, ' ')
        IF (IQUEST(1).NE.0)           GO TO 999
        CALL RZLOCK (CLOCK)
*
      ELSE IF (CPATL.EQ.'RZFREE') THEN
*
*  **   RZFREE
*
        CALL KUGETC (CLOCK, NCH)
        CALL RZFREE (CLOCK)
*
      ELSE IF (CPATL.EQ.'RZLLOK') THEN
*
*  **   RZLLOK
*
        CALL RZLLOK
*
      ELSE IF (CPATL.EQ.'RZNDIR') THEN
*
*  **   RZNDIR
*
        CHOPT  = ' '
        CALL KUGETC (CPATH, NCH)
        CALL KUGETC (CHOPT, NCH)
        CALL RZNDIR (CPATH, CHOPT)
        IF (IQUEST(1).NE.0) WRITE (IQPRNT, 1001) 'RZNDIR', CPATH
*
      ELSE IF (CPATL.EQ.'RZCDIR') THEN
*
*  **   RZCDIR
*
        CHOPT  = ' '
        CALL KUGETC (CPATH, NCH)
        CALL KUGETC (CHOPT, NCH)
        CALL RZCDIR (CPATH, CHOPT)
        IF (IQUEST(1).NE.0) WRITE (IQPRNT, 1001) 'RZCDIR', CPATH
*
      ELSE IF (CPATL.EQ.'RZLDIR') THEN
*
*  **   RZLDIR
*
        CHOPT  = ' '
        CALL KUGETC (CPATH, NCH)
        CALL KUGETC (CHOPT, NCH)
        CALL RZLDIR (CPATH, CHOPT)
*
      ELSE IF (CPATL.EQ.'RZMDIR') THEN
*
*  **   RZMDIR
*
        NWKEY  = 9
        CFORM  = 'IIIIIIIII'
        CALL KUGETC (CHDIR, NCH)
        CALL KUGETI (NWKEY)
        CALL KUGETC (CFORM, NCH)
        DO 20 I = 1, NWKEY
          WRITE (CHPRO, 1000) I
          CALL KUPROC (CHPRO, CHTAG(I), NCH)
   20   CONTINUE
        CALL RZMDIR (CHDIR, NWKEY, CFORM, CHTAG)
*
      ELSE IF (CPATL.EQ.'RZPURG') THEN
*
*  **   RZPURG
*
        CALL KUGETI (NKEEP)
        CALL RZPURG (NKEEP)
*
      ELSE IF (CPATL.EQ.'RZDELT') THEN
*
*  **   RZDELT
*
        CALL KUGETC (CHDIR, NCH)
        CALL RZDELT (CHDIR)
        IF (IQUEST(1).NE.0) WRITE (IQPRNT, 1001) 'RZDELT', CHDIR
*
      ELSE IF (CPATL.EQ.'RZSTAT') THEN
*
*  **   RZSTAT
*
        NLEV   = 16
        CALL KUGETC (CHDIR, NCH)
        CALL KUGETI (NLEV)
        CALL RZSTAT (CHDIR, NLEV, ' ')
*
      ELSE IF (CPATL.EQ.'RZKEYD') THEN
*
*  **   RZKEYD
*
        CALL KUGETC (CHDIR, NCH)
        CALL RZCDIR (CHDIR, ' ')
        IF (IQUEST(1).NE.0) THEN
          WRITE (IQPRNT, 1001) 'RZKEYD', CHDIR
          GO TO 999
        ENDIF
        CALL RZKEYD (NWKEY, CFORM, CHTAG)
        DO 30 IK = 1, NWKEY
          WRITE (IQPRNT, 1002) IK, CFORM(IK:IK), CHTAG(IK)
   30   CONTINUE
*
      ELSE IF (CPATL.EQ.'RZTOFZ') THEN
*
*  **   RZTOFZ
*
        CALL INZFOP (LUN)
        IF (IQUEST(1).NE.0)           GO TO 999
        CHOP   = ' '
        CALL KUGETC (CHOP, NCH)
        CALL RZTOFZ (LUN, CHOP)
*
      ELSE IF (CPATL.EQ.'RZFRFZ') THEN
*
*  **   RZFRFZ
*
        CALL INZFOP (LUN)
        IF (IQUEST(1).NE.0)           GO TO 999
        CHOP   = ' '
        CALL KUGETC (CHOP, NCH)
        CALL RZFRFZ (LUN, CHOP)
*
      ELSE IF (CPATL.EQ.'RZTOALPHA') THEN
*
*  **   RZTOALPHA
*
        CALL INZFOP (LUN)
        IF (IQUEST(1).NE.0)           GO TO 999
        CHOP   = ' '
        CALL KUGETC (CHOP, NCH)
        CALL RZTOFZ (LUN, CHOP)
*
      ELSE IF (CPATL.EQ.'RZFRALPHA') THEN
*
*  **   RZFRALPHA
*
        CALL INZFOP (LUN)
        IF (IQUEST(1).NE.0)           GO TO 999
        CHOP   = ' '
        CALL KUGETC (CHOP, NCH)
        CALL RZFRFZ (LUN, CHOP)
*
      ENDIF
*
 1000 FORMAT ('Key ',I4)
 1001 FORMAT (' INZARZ : Error in ',A,' for ',A)
 1002 FORMAT (' Key ',I5,1X,A1,1X,A8)
*                                                             END INZARZ
  999 END
      SUBROUTINE INZEIN
*
************************************************************************
*
*        SUBR. INZEIN
*
*   Defines interactive Menus and Commands for ZEBRA
*
*   Called by INTINI
*
************************************************************************
*
      CALL INZEMU
      CALL INZEMZ
      CALL INZEDZ
      CALL INZEFZ
      CALL INZERZ
*                                                             END INZEIN
      END
      SUBROUTINE INZFOP (LUN)
*
************************************************************************
*
*        SUBR. INZFOP (LUN*)
*
*   Opens a FZ file interactively
*
*   Called by INZAFZ, INZARZ, ...
*
************************************************************************
*
      COMMON /ZUNIT/  IQREAD, IQPRNT, IQPR2, IQLOG, IQPNCH, IQTTIN,
     +                IQTYPE
      COMMON /QUEST/  IQUEST(100)
      DIMENSION       IOPT(5)
      CHARACTER       NAME*80, CHOPT*8, NAMEO*80, STATE*10, FORMT*12
      EQUIVALENCE     (IOPTA, IOPT(1)), (IOPTI, IOPT(2))
     +              , (IOPTO, IOPT(3)), (IOPTX, IOPT(4))
     +              , (IOPTZ, IOPT(5))
      LOGICAL         EXIST, OPEN
*
*     ------------------------------------------------------------------
*
* *** Access the file name, etc.
*
      IQUEST(1) = 0
      CALL KUGETI (LUN)
      CALL KUGETC (NAME, NCH)
      CALL KUGETI (LRECL)
      CALL KUGETC (CHOPT, NCH)
      NMCH   = LENOCC (NAME) + 1
      IF (NMCH.GT.80) NMCH = 80
*
* *** Analyse the option
*
      CALL UOPTC  (CHOPT, 'AIOXZ', IOPT)
*
* *** Decide on the record length
*
      IF (IOPTX.NE.0) THEN
        LREC = 900
      ELSE IF (LRECL.GT.0) THEN
        LREC = LRECL
      ELSE IF (IOPTA.NE.0) THEN
        LREC = 20
      ELSE
        LREC = 2500
      ENDIF
*
* *** Decide on state and form
*
      IF (IOPTO.EQ.0) THEN
        STATE  = 'OLD'
      ELSE
        STATE  = 'NEW'
      ENDIF
      IF (IOPTA.NE.0) THEN
        FORMT = 'FORMATTED'
      ELSE
        FORMT = 'UNFORMATTED'
      ENDIF
*
* *** Check that file has not already been opened
*
      INQUIRE (LUN, OPENED=OPEN, NAME=NAMEO)
      IF (OPEN)  THEN
        WRITE (IQPRNT, 1001) LUN, NAMEO
        IQUEST(1) = 1
        GO TO 999
      ENDIF
*
* *** Check if the file exists
*
      INQUIRE (FILE=NAME, EXIST=EXIST)
      IF (EXIST) THEN
*
*  **   For input files, give warning if not existing
*
      ELSE IF (IOPTO.EQ.0) THEN
        WRITE (IQPRNT, 1003) NAME(1:NMCH), LUN
        IQUEST(1) = 1
        GO TO 999
      ENDIF
*
* *** Now open the file
*
      IF (IOPTX.NE.0) THEN
        IF (STATE.EQ.'OLD') THEN
          OPEN (UNIT=LUN, FILE=NAME, FORM=FORMT, STATUS=STATE,
     +          READONLY, SHARED, ERR=50, IOSTAT=IOERR)
        ELSE
          OPEN (UNIT=LUN, FILE=NAME, FORM=FORMT, STATUS=STATE,
     +          RECORDTYPE='FIXED', RECL=LREC, BLOCKSIZE=4*LREC, ERR=50,
     +          IOSTAT=IOERR)
        ENDIF
*
      ELSE
        IF (STATE.EQ.'NEW') THEN
          OPEN (UNIT=LUN, FILE=NAME, FORM=FORMT, STATUS=STATE,
     +          ERR=50, IOSTAT=IOERR)
        ELSE
          OPEN (UNIT=LUN, FILE=NAME, FORM=FORMT, STATUS=STATE,
     +          READONLY, SHARED, ERR=50, IOSTAT=IOERR)
        ENDIF
      ENDIF
      WRITE (IQPRNT, 1004) NAME(1:NMCH), LUN
      CALL FZFILE (LUN, LREC, CHOPT)
      GO TO 999
*
   50 WRITE (IQPRNT, 1005) IOERR, LUN, NAME(1:NMCH)
      IQUEST(1) = 1
*
 1001 FORMAT (' INZFOP : Request for OPEN on Unit ',I3,/,
     +          10X,' Unit already in use for file ',A,/,
     +          10X,' Request ignored')
 1002 FORMAT (/,' INZFOP : Request for file ',A,' on Unit ',I4,
     +        /,10X,'File already exits - request ignored ',/)
 1003 FORMAT (/,' INZFOP : Request for file ',A,' on Unit ',I4,
     +        /,10X,'File does not exist - request ignored ',/)
 1004 FORMAT (' INZFOP : Opened File ',A,' on Unit ',I4)
 1005 FORMAT (' INZFOP Error : IOSTAT = ',I10,' LUN = ',I4,/
     +        '                FILE-NAME = ',A)
*                                                             END INZFOP
  999 END
      SUBROUTINE INZROP (CASE, LUN, CHOPT)
*
************************************************************************
*
*        SUBR. INZROP (CASE, LUN*, CHOPT*)
*
*   Opens a RZ file interactively
*
*   Called by INZARZ, INDBL3, ...
*
************************************************************************
*
      COMMON /ZUNIT/  IQREAD, IQPRNT, IQPR2, IQLOG, IQPNCH, IQTTIN,
     +                IQTYPE
      COMMON /QUEST/  IQUEST(100)
      DIMENSION       IOPT(2)
      CHARACTER       NAME*80, NAMEO*80, STATE*10, FORMT*12
      CHARACTER       CASE*(*), CHOPT*(*)
      EQUIVALENCE     (IOPT1, IOPT(1)), (IOPTU, IOPT(2))
      LOGICAL         EXIST, OPEN
*
*     ------------------------------------------------------------------
*
* *** Access the file name, etc.
*
      IQUEST(1) = 0
      LRECL  = 1000
      CHOPT  = ' '
      CALL KUGETI (LUN)
      CALL KUGETC (NAME, NCH)
      CALL KUGETI (LRECL)
      CALL KUGETC (CHOPT, NCH)
      NMCH   = LENOCC (NAME) + 1
      IF (NMCH.GT.80) NMCH = 80
*
* *** Analyse the option
*
      CALL UOPTC  (CHOPT, '1U', IOPT)
*
* *** Decide on state and form
*
      IF (CASE.EQ.'MAKE') THEN
        IOPTO  = 1
      ELSE IF (IOPT1.NE.0.OR.IOPTU.NE.0) THEN
        IOPTO = 1
      ELSE
        IOPTO = 0
      ENDIF
      IF (IOPTO.EQ.0) THEN
        STATE  = 'OLD'
      ELSE
        STATE  = 'UNKNOWN'
        IF (CASE.EQ.'MAKE') STATE = 'NEW'
      ENDIF
      FORMT = 'UNFORMATTED'
*
* *** Check that file has not already been opened
*
      INQUIRE (LUN, OPENED=OPEN, NAME=NAMEO)
      IF (OPEN)  THEN
        WRITE (IQPRNT, 1001) LUN, NAMEO
        IQUEST(1) = 1
        GO TO 999
      ENDIF
      INQUIRE (FILE=NAME, EXIST=EXIST)
      IF (.NOT.EXIST.AND.CASE.NE.'MAKE') THEN
*
*  **   Files assumed to exist
*
        WRITE (IQPRNT, 1003) NAME(1:NMCH), LUN
        IQUEST(1) = 1
        GO TO 999
      ENDIF
*
* *** Now open the file
*
      IF (IOPTO.EQ.0) THEN
        OPEN (UNIT=LUN, FILE=NAME, FORM=FORMT, STATUS=STATE, RECL=LRECL,
     +        ACCESS='DIRECT', SHARED, READONLY, ERR=50, IOSTAT=IOERR)
      ELSE
        OPEN (UNIT=LUN, FILE=NAME, FORM=FORMT, STATUS=STATE, RECL=LRECL,
     +        ACCESS='DIRECT', SHARED, ERR=50, IOSTAT=IOERR)
      ENDIF
      WRITE (IQPRNT, 1004) NAME(1:NMCH), LUN
      GO TO 999
*
   50 WRITE (IQPRNT, 1005) IOERR, LUN, NAME(1:NMCH)
      IQUEST(1) = 1
*
 1001 FORMAT (' INZROP : Request for OPEN on Unit ',I3,/,
     +          10X,' Unit already in use for file ',A,/,
     +          10X,' Request ignored')
 1003 FORMAT (' INZROP : Request for File ',A,' on unit ',I4,
     +        /,10X,'File does not exist - request ignored ',/)
 1004 FORMAT (' INZROP : Opened File ',A,' on Unit ',I4)
 1005 FORMAT (' INZROP Error : IOSTAT = ',I10,' LUN = ',I4,/
     +        '                FILE-NAME = ',A)
*                                                             END INZROP
  999 END

      SUBROUTINE INDBIN
*
************************************************************************
*
*        SUBR. INDBIN
*
*   Defines interactive Menus and Commands for DBL3
*
*   Called by INTINI
*
************************************************************************
*
      CALL INDBME
      CALL INDBIC
      CALL INDBAU
      CALL INDBIP
*                                                             END INDBIN
      END
      SUBROUTINE INDBPL
*
************************************************************************
*
*        SUBR. INDBPL
*
*   Action Routines for menu /DBL3/PLOT
*
*   Allowed Actions :
*
*     DBHELP, DBPLOB, DBPLOT, DBPLOV, DBPLTI, DBREAD, DBTREE
*
*   Called by KUIP routine
*
************************************************************************
*
      PARAMETER       (L3WKST=1, NARGL3=100, NLUNL3=20)
      COMMON /L3UNIT/ L3DEBU, L3DRAW, L3GETG, L3GETS, L3GETX, L3GETY
     +              , L3GETZ, L3HIST, L3HSTO, L3LUMI, L3PRNT, L3SAVG
     +              , L3SAVS, L3SAVX, L3SAVY, L3SAVZ, L3SCAN, L3SMRY
     +              , L3SPIN, L3UOUT
      INTEGER         L3DEBU, L3DRAW, L3GETG, L3GETS, L3GETX, L3GETY
     +              , L3GETZ, L3HIST, L3HSTO, L3LUMI, L3PRNT, L3SAVG
     +              , L3SAVS, L3SAVX, L3SAVY, L3SAVZ, L3SCAN, L3SMRY
     +              , L3SPIN, L3UOUT, LUNSL3(NLUNL3)
      COMMON /L3FLAG/ IFTML3, IGYFL3, IHFIL3, IHISL3, INTXL3, IPRTL3
     +              , IPR2L3, IWKTL3, IXERL3, MOP3L3, ARGSL3(NARGL3)
      INTEGER         IARGL3(NARGL3)
      INTEGER         IFTML3, IGYFL3, IHFIL3, IHISL3, INTXL3, IPRTL3
     +              , IPR2L3, IWKTL3, IXERL3, MOP3L3
      REAL            ARGSL3
      EQUIVALENCE     (IARGL3(1), ARGSL3(1)), (LUNSL3(1), L3DEBU)
*
*
      IF (IWKTL3.NE.0) THEN
        CALL DBACPL
      ENDIF
*                                                             END INDBPL
      END
      SUBROUTINE INPAIN
*
************************************************************************
*
*        SUBR. INPAIN
*
*   Create command structure from definition files for PAW menus
*
*   Called by user
*
************************************************************************
*
      CALL HISDEF
      CALL FUNDEF
      CALL NTUDEF
      CALL GRADEF
      CALL PICDEF
      CALL FORDEF
      CALL NETDEF
      CALL PVEDEF
      CALL INPAHI
*                                                             END INPAIN
      END
      SUBROUTINE INPAW
*
************************************************************************
*
*        SUBR. INPAW
*
*   Initialization routine for PAW session
*
*   Called by user
*
************************************************************************
*
      COMMON/PAWC/NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,HCV(9989)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      COMMON/HCBOOK/HVERSN,IHWORK,LHBOOK,LHPLOT,LGTIT,LHWORK,
     +LCDIR,LSDIR,LIDS,LTAB,LCID,LCONT,LSCAT,LPROX,LPROY,LSLIX,
     +LSLIY,LBANX,LBANY,LPRX,LPRY,LFIX,LLID,LR1,LR2,LCIDN
*
      PARAMETER(KNCX=3,KXMIN=4,KXMAX=5,KBWIDX=6,KMIN=7,KMAX=8,KNORM=9,
     +          KTIT1=10,KNCY=7,KYMIN=8,KYMAX=9,KBWIDY=10,KSCAL2=11,
     +          KTIT2=12,KNBIT=1,KNOENT=2,KSTAT1=3,KNSDIR=5,KNRH=6,
     +          KCON1=9,KCON2=3,KBITS=1,KNTOT=2)
*
      COMMON/PAWCOM/ID,NUM,JOFSET,IDOLD,KHRIN
      COMMON/PAWCID/CHID,CHCASE
      CHARACTER*64 CHID
      CHARACTER*4 CHCASE
*
      COMMON/PAWLUN/LUNIT(128)
*
      COMMON/PAWCUT/ICUTYP(100),XPCUT(20,100),YPCUT(20,100),
     +              IVCUTX(100),CUTXL(100),CUTXU(100),
     +              IVCUTY(100),CUTYL(100),CUTYU(100)
      COMMON/PAWCUE/CHCUTS
      CHARACTER*64 CHCUTS(100)
*

      COMMON/PAWNPU/NPFUNC,NPFUNX,NPFUNY,NPFUNZ,ANGLE1,ANGLE2
     +,             RANGX1,RANGX2,RANGY1,RANGY2,RANGZ1,RANGZ2
*
      COMMON/PAWBGN/ICOMIS,ISIGMA
*
      PARAMETER (MASDIM=9)
      COMMON/PCMASK/LMASK,LMBUF,LM,IMSK(MASDIM),IMRD,MBIT(MASDIM),MBITD,
     +              NUMD,NBUFFD,NBUFFR,NBUFF(MASDIM),IDEC,LREC,NBMASK
      COMMON/PCMAS2/CHMASK
      CHARACTER*32 CHMASK(20)
*
      COMMON/PCCSEL/IOPTCS(5),CSIZE
      EQUIVALENCE (IOPTCS(1),IOPTCR),(IOPTCS(2),IOPTCB)
      EQUIVALENCE (IOPTCS(3),IOPTCM),(IOPTCS(4),IOPTCC)
      EQUIVALENCE (IOPTCS(5),IOPTCN)
      COMMON/PCCSE2/CHCSEL
      CHARACTER*80 CHCSEL
*
      COMMON/PCARGS/NOLOG,BATCH,CERN
      LOGICAL NOLOG,BATCH,CERN
*
      COMMON/CZSOCK/LUNCZ,IADTCP,LBUF,ISKIN,ISKOUT
*
      COMMON/PCSLAS/BSLASH
      CHARACTER*1 BSLASH
*
      PARAMETER       (L3WKST=1, NARGL3=100, NLUNL3=20)
      COMMON /L3UNIT/ L3DEBU, L3DRAW, L3GETG, L3GETS, L3GETX, L3GETY
     +              , L3GETZ, L3HIST, L3HSTO, L3LUMI, L3PRNT, L3SAVG
     +              , L3SAVS, L3SAVX, L3SAVY, L3SAVZ, L3SCAN, L3SMRY
     +              , L3SPIN, L3UOUT
      INTEGER         L3DEBU, L3DRAW, L3GETG, L3GETS, L3GETX, L3GETY
     +              , L3GETZ, L3HIST, L3HSTO, L3LUMI, L3PRNT, L3SAVG
     +              , L3SAVS, L3SAVX, L3SAVY, L3SAVZ, L3SCAN, L3SMRY
     +              , L3SPIN, L3UOUT, LUNSL3(NLUNL3)
      COMMON /L3FLAG/ IFTML3, IGYFL3, IHFIL3, IHISL3, INTXL3, IPRTL3
     +              , IPR2L3, IWKTL3, IXERL3, MOP3L3, ARGSL3(NARGL3)
      INTEGER         IARGL3(NARGL3)
      INTEGER         IFTML3, IGYFL3, IHFIL3, IHISL3, INTXL3, IPRTL3
     +              , IPR2L3, IWKTL3, IXERL3, MOP3L3
      REAL            ARGSL3
      EQUIVALENCE     (IARGL3(1), ARGSL3(1)), (LUNSL3(1), L3DEBU)
*
      COMMON /CWK/    IWK
*
*
*     ------------------------------------------------------------------
*
      BSLASH = '\'
      IADTCP = 0
      ISKIN  = 0
      LMASK  = 0
      IDOLD  = 0
      JOFSET = 0
      CALL VZERO (LUNIT, 128)
      LUNIT( 5) = 6
      LUNIT( 6) = 6
      LUNIT( 7) = 6
      LUNIT(10) = 8
      DO 5 I=11,18
        LUNIT(I) = 6
   5  CONTINUE
      LUNIT(19) = 8
      LUNIT(81) = 7
      LUNIT(82) = 7
      LUNIT(83) = 7
      LUNIT(84) = 7
      LUNIT(91) = 8
      LUNIT(97) = 7
      CALL VZERO (IOPTCS, 5)
      CSIZE = 0.28
*
* *** Set the No-Reference bit in ZEBRA matrix
*
      CALL MZXREF (IXKU  , IXPAWC+21, 'C')
      CALL MZXREF (IXKU  , IXPAWC+22, 'C')
      CALL MZXREF (IXKU  , IXPAWC+23, 'C')
      CALL MZXREF (IXHIGZ, IXPAWC+21, 'C')
      CALL MZXREF (IXHIGZ, IXPAWC+22, 'C')
      CALL MZXREF (IXHIGZ, IXPAWC+23, 'C')
*
* *** Initialize COMIS
*
      ICOMIS = 0
      ISIGMA = 0
*
* *** Initialize /PAWCUT/
*
      NPFUNC = 100
      CALL VZERO (IVCUTX, 100)
      CALL VZERO (IVCUTY, 100)
      CALL VZERO (ICUTYP, 100)
*
      IWK    = IWKTL3
      CALL ISCHH (CSIZE)
*                                                              END INPAW
      END
C
C - INZLIB160B.FOR  to go with dbl3 version 308
C
      SUBROUTINE INZEDZ          
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL INZADZ
 
      CALL KUNWG(   0)
 
      CALL KUCMD('/ZEBRA',' ','SW')
 
      CALL KUNWG(  15)
      CALL KUCMD(' ','DZ','C')
      GUID(  1)='Interfaces to the DZ (Debug and dump) pa'//
     +'ckage of ZEBRA'
      CALL KUGUID('DZ',GUID,  1,'S')
 
      CALL KUCMD('DZ',' ','SW')
 
      CALL KUNWG( 217)
      CALL KUCMD(' ','DZSHOW','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSHOW','CHIDH','Bank name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSHOW','IDN','Bank number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSHOW','IXDIV','Division index','IO','S')
      CALL KUPVAL('DZSHOW','IXDIV',2,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSHOW','CHOPT','Character option','CO','S')
      CALL KUPVAL('DZSHOW','CHOPT',0,0.,'BHV','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSHOW','IL1','Index of the first link to be printed',
     +'IO','S')
      CALL KUPVAL('DZSHOW','IL1',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSHOW','IL2','Index of the last  link to be printed',
     +'IO','S')
      CALL KUPVAL('DZSHOW','IL2',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSHOW','ID1','Index of the first data to be printed',
     +'IO','S')
      CALL KUPVAL('DZSHOW','ID1',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSHOW','ID2','Index of the last  data to be printed',
     +'IO','S')
      CALL KUPVAL('DZSHOW','ID2',0,0.,' ','D')
      CALL KUNDPV(   2,   1,   1,   0,   1)
      CALL KUPAR('DZSHOW','CTEXT','Text tp be printed together with dump
     +','CO','S')
      CALL KUPVAL('DZSHOW','CTEXT',0,0.,'DZSHOW','D')
      GUID(  1)='Print the data structure identified by B'//
     +'ank name and number through'
      GUID(  2)='the calls'
      GUID(  3)='  LBANK = LZFID (IXDIV, IDH, IDN, 0)  an'//
     +'d'
      GUID(  4)='  CALL DZSHOW (CTEXT, IXDIV, LBANK, CHOP'//
     +'T, IL1, IL2, ID1, ID2)'
      GUID(  5)='The output format of the data part is co'//
     +'ntrolled by the internal'
      GUID(  6)='or external I/O characteristic.'
      GUID(  7)=' CHOPT = ''B''  : Print the single bank '//
     +'at LBANK'
      GUID(  8)='       = ''D''  : Print the bank content'//
     +'s from top to bottom Downwards'
      GUID(  9)='                with five elements per l'//
     +'ine.'
      GUID( 10)='       = ''S''  : Print the bank content'//
     +'s from left to right Sideways'
      GUID( 11)='                with up to ten elements '//
     +'per line'
      GUID( 12)='       = ''L''  : Print the linear struc'//
     +'ture supported by LBANK'
      GUID( 13)='       = ''V''  : Print the vertical str'//
     +'ucture supported by LBANK'
      GUID( 14)='       = ''Z''  : print the data part of'//
     +' each bank in hexadecimal'
      GUID( 15)='                format (ignoring the I/O'//
     +' characteristic)'
      CALL KUGUID('DZSHOW',GUID, 15,'S')
      CALL KUACT('DZSHOW',INZADZ)
 
      CALL KUNWG(  99)
      CALL KUCMD(' ','DZFORM','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZFORM','CHIDH','Bank name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZFORM','IDN','Bank number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZFORM','IXDIV','Division index','IO','S')
      CALL KUPVAL('DZFORM','IXDIV',2,0.,' ','D')
      GUID(  1)='Print the format of the data part of the'//
     +' bank identified by name and'
      GUID(  2)='number through the calls'
      GUID(  3)='  LBANK = LZFID (IXDIV, IDH, IDN, 0)  an'//
     +'d'
      GUID(  4)='  CALL DZFORM (IXDIV, LBANK)'
      GUID(  5)='It uses the I/O characteristic stored in'//
     +' the bank, decodes the'
      GUID(  6)='information and prints it in a format wh'//
     +'ich is compatible to MZFORM.'
      GUID(  7)='If LBANK = 0, all I/O characteristics de'//
     +'clared with MZFORM are printed.'
      CALL KUGUID('DZFORM',GUID,  7,'S')
      CALL KUACT('DZFORM',INZADZ)
 
      CALL KUNWG(  72)
      CALL KUCMD(' ','DZSTOR','C')
      CALL KUNDPV(   2,   1,   1,   0,   1)
      CALL KUPAR('DZSTOR','CTEXT','Text to be printed with the dump','CO
     +','S')
      CALL KUPVAL('DZSTOR','CTEXT',0,0.,'DZSTOR','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSTOR','ISTOR','Store index','IO','S')
      CALL KUPVAL('DZSTOR','ISTOR',0,0.,' ','D')
      GUID(  1)='Print the structure of the ZEBRA store i'//
     +'dentified by ISTOR through'
      GUID(  2)='the call'
      GUID(  3)='  CALL DZSTOR (CTEXT, ISTOR)'
      GUID(  4)='The routine outputs the parameters chara'//
     +'cterizing the store, followed'
      GUID(  5)='by a list of all divisions and all link '//
     +'areas associated with the'
      GUID(  6)='store in question.'
      CALL KUGUID('DZSTOR',GUID,  6,'S')
      CALL KUACT('DZSTOR',INZADZ)
 
      CALL KUNWG(  50)
      CALL KUCMD(' ','DZAREA','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZAREA','CHLA','Name of the link area','C','S')
      CALL KUNDPV(   2,   1,   1,   0,   1)
      CALL KUPAR('DZAREA','CTEXT','Text to be printed with the dump','CO
     +','S')
      CALL KUPVAL('DZAREA','CTEXT',0,0.,'DZAREA','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZAREA','ISTOR','Store index','IO','S')
      CALL KUPVAL('DZAREA','ISTOR',0,0.,' ','D')
      GUID(  1)='Print the content of a ZEBRA link area i'//
     +'dentified by CHLA associated'
      GUID(  2)='with the store ISTOR through the call'
      GUID(  3)='  CALL DZAREA (CTEXT, ISTOR, CHLA, 0, '''//
     +'N'')'
      GUID(  4)='See ZEBRA user guide for DZAREA'
      CALL KUGUID('DZAREA',GUID,  4,'S')
      CALL KUACT('DZAREA',INZADZ)
 
      CALL KUNWG( 101)
      CALL KUCMD(' ','DZSURV','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSURV','CHIDH','Bank name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSURV','IDN','Bank number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSURV','IXDIV','Division index','IO','S')
      CALL KUPVAL('DZSURV','IXDIV',2,0.,' ','D')
      CALL KUNDPV(   2,   1,   1,   0,   1)
      CALL KUPAR('DZSURV','CTEXT','Text to be printed with the dump','CO
     +','S')
      CALL KUPVAL('DZSURV','CTEXT',0,0.,'DZSURV','D')
      GUID(  1)='Print a survey of the data structure ide'//
     +'ntified by Bank name and number'
      GUID(  2)='through the calls'
      GUID(  3)='  LBANK = LZFID (IXDIV, IDH, IDN, 0)  an'//
     +'d'
      GUID(  4)='  CALL DZSURV (CTEXT, IXDIV, LBANK)'
      GUID(  5)='All horizontal (Next) as well as vertica'//
     +'l (Down) structural links of'
      GUID(  6)='the ZEBRA structure are followed startin'//
     +'g from the bank supporting the'
      GUID(  7)='sub(structure) in question.'
      GUID(  8)='See ZEBRA user guide for DZSURV'
      CALL KUGUID('DZSURV',GUID,  8,'S')
      CALL KUACT('DZSURV',INZADZ)
 
      CALL KUNWG( 315)
      CALL KUCMD(' ','DZSNAP','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSNAP','IXDIV','Division index','IO','S')
      CALL KUPVAL('DZSNAP','IXDIV',2,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZSNAP','CHOPT','Character option','CO','S')
      CALL KUPVAL('DZSNAP','CHOPT',0,0.,'M','D')
      CALL KUNDPV(   2,   1,   1,   0,   1)
      CALL KUPAR('DZSNAP','CTEXT','Text to be printed with the dump','CO
     +','S')
      CALL KUPVAL('DZSNAP','CTEXT',0,0.,'DZSNAP','D')
      GUID(  1)='Provide a snapshot of one or more divisi'//
     +'ons in a ZEBRA store through'
      GUID(  2)='the call'
      GUID(  3)='  CALL DZSNAP (CTEXT, IXDIV, CHOPT)'
      GUID(  4)='The kind of information provided is cont'//
     +'rolled by CHOPT.'
      GUID(  5)=' CHOPT = ''C''  : Dump any active bank w'//
     +'ith status bit IQCRIT set;'
      GUID(  6)='                bit IQCRIT will be reset'//
     +' to zero in each bank.'
      GUID(  7)='                (option C is implied by '//
     +'option T)'
      GUID(  8)='       = ''D''  : Dump any active bank w'//
     +'ith status bit IQMARK set;'
      GUID(  9)='                bit IQMARK will be reset'//
     +' to zero in each bank.'
      GUID( 10)='       = ''E''  : Extend map entry to du'//
     +'mp all links of each bank.'
      GUID( 11)='                (otherwise only as many '//
     +'links as fits on a line)'
      GUID( 12)='       = ''F''  : Dump all active banks,'//
     +' links and data'
      GUID( 13)='       = ''K''  : Dropped banks to be tr'//
     +'eated as active. (Dropped'
      GUID( 14)='                banks are not normally d'//
     +'umped under D or F option)'
      GUID( 15)='       = ''L''  : Dump all link areas as'//
     +'sociated with the store.'
      GUID( 16)='       = ''M''  : Print map entry for ea'//
     +'ch bank.'
      GUID( 17)='       = ''T''  : Terminal type dump, us'//
     +'ed for post-mortem dump'
      GUID( 18)='                mainly to mark "critical'//
     +'" directly accessible banks.'
      GUID( 19)='       = ''W''  : Dump the working space'//
     +', links and data.'
      GUID( 20)='       = ''Z''  : Dump the information i'//
     +'n hexadecimal.'
      GUID( 21)='See ZEBRA user guide for DZSNAP'
      CALL KUGUID('DZSNAP',GUID, 21,'S')
      CALL KUACT('DZSNAP',INZADZ)
 
      CALL KUNWG( 150)
      CALL KUCMD(' ','DZVERI','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZVERI','IXDIV','Division index','IO','S')
      CALL KUPVAL('DZVERI','IXDIV',2,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DZVERI','CHOPT','Character option','CO','S')
      CALL KUPVAL('DZVERI','CHOPT',0,0.,'CLSU','D')
      CALL KUNDPV(   2,   1,   1,   0,   1)
      CALL KUPAR('DZVERI','CTEXT','Text to be printed with the dump','CO
     +','S')
      CALL KUPVAL('DZVERI','CTEXT',0,0.,'DZVERI','D')
      GUID(  1)='Checks the structure of one or more divi'//
     +'sions in a ZEBRA store through'
      GUID(  2)='the call'
      GUID(  3)='  CALL DZVERI (CTEXT, IXDIV, CHOPT)'
      GUID(  4)='The verification detail depends  on the '//
     +'settings in CHOPT.'
      GUID(  5)=' CHOPT = ''C''  : Check chaining of the '//
     +'bank only.'
      GUID(  6)='       = ''F''  : Errors are considered '//
     +'fatal and generate a call to'
      GUID(  7)='                ZFATAL.'
      GUID(  8)='       = ''L''  : Check validity of the '//
     +'structural links in the banks.'
      GUID(  9)='       = ''S''  : Check the store parame'//
     +'ters.'
      GUID( 10)='       = ''U''  : Check the validity of '//
     +'the up and origin links in the'
      GUID( 11)='                banks.'
      GUID( 12)='See ZEBRA user guide for DZVERI'
      CALL KUGUID('DZVERI',GUID, 12,'S')
      CALL KUACT('DZVERI',INZADZ)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
      SUBROUTINE INZEFZ          
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL INZAFZ
 
      CALL KUNWG(   0)
 
      CALL KUCMD('/ZEBRA',' ','SW')
 
      CALL KUNWG(  15)
      CALL KUCMD(' ','FZ','C')
      GUID(  1)='Interfaces to the FZ (Sequential I/O) pa'//
     +'ckage of ZEBRA'
      CALL KUGUID('FZ',GUID,  1,'S')
 
      CALL KUCMD('FZ',' ','SW')
 
      CALL KUNWG( 113)
      CALL KUCMD(' ','FZFILE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZFILE','LUN','Logical unit number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZFILE','FNAME','File name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZFILE','LRECL','Record length in words','IO','S')
      CALL KUPVAL('FZFILE','LRECL',900,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZFILE','CHOPT','Character option','CO','S')
      CALL KUPVAL('FZFILE','CHOPT',0,0.,'IX','D')
      GUID(  1)='Open an FZ sequential formatted or unfor'//
     +'matted file.'
      GUID(  2)='For LRECL = 0, FZFILE automatically sets'//
     +' its own defaults;'
      GUID(  3)='for Native mode this value is 2440 words'//
     +'; for binary exchange'
      GUID(  4)='mode it is 900 words. For binary exchang'//
     +'e mode, user supplied'
      GUID(  5)='LRECL must be a multiple of 90 words.'
      GUID(  6)='  CHOPT = ''I''  Input file'
      GUID(  7)='        = ''O''  Output file'
      GUID(  8)='        = ''X''  Binary exchange mode'
      GUID(  9)='        = ''A''  Alphanumeric exchange m'//
     +'ode'
      CALL KUGUID('FZFILE',GUID,  9,'S')
      CALL KUACT('FZFILE',INZAFZ)
 
      CALL KUNWG(  99)
      CALL KUCMD(' ','FZLOGL','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZLOGL','LUN','Logical unit number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZLOGL','LOGL','Logging level','IO','S')
      CALL KUPVAL('FZLOGL','LOGL',0,0.,' ','D')
      GUID(  1)='Set the logging level of an FZ sequentia'//
     +'l file.'
      GUID(  2)='  LOGL  = -3   Suppress all messages'
      GUID(  3)='        = -2   Print error messages only'
      GUID(  4)='        = -1   Terse mode'
      GUID(  5)='        =  0   Normal mode'
      GUID(  6)='        =  1   Normal mode and details o'//
     +'f conversion problem'
      GUID(  7)='        =  2   Print to monitor calls to'//
     +' the FZ routines'
      GUID(  8)='        =  3   Short diagnostics'
      GUID(  9)='        =  4   Full diagnostics'
      CALL KUGUID('FZLOGL',GUID,  9,'S')
      CALL KUACT('FZLOGL',INZAFZ)
 
      CALL KUNWG(  55)
      CALL KUCMD(' ','FZLIMI','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZLIMI','LUN','Logical unit number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZLIMI','ALIM','The maximum size of o/p file in MWords
     +','RO','S')
      CALL KUPVAL('FZLIMI','ALIM',0,-1.,' ','D')
      GUID(  1)='Limit the size of a output FZ file.'
      GUID(  2)='  ALIM  > 0.   User defined limit in meg'//
     +'a words'
      GUID(  3)='        = 0.   Increase the limit by the'//
     +' amount specified previously'
      GUID(  4)='        < 0.   Unlimited (as initialized'//
     +' by FZFILE)'
      CALL KUGUID('FZLIMI',GUID,  4,'S')
      CALL KUACT('FZLIMI',INZAFZ)
 
      CALL KUNWG( 135)
      CALL KUCMD(' ','FZENDI','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZENDI','LUN','Logical unit number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZENDI','CHOPT','Character option','CO','S')
      CALL KUPVAL('FZENDI','CHOPT',0,0.,'T','D')
      GUID(  1)='Close an FZ input file through the call'
      GUID(  2)='  CALL FZENDI (LUN, CHOPT)'
      GUID(  3)='Selcted option is specified by CHOPT.'
      GUID(  4)=' CHOPT = ''O''  : Switch to output, perm'//
     +'it write after read.'
      GUID(  5)='                Needed in the case that '//
     +'the file is positioned for'
      GUID(  6)='                output by reading'
      GUID(  7)='       = ''Q''  : Quiet, suppress printi'//
     +'ng of file statistics'
      GUID(  8)='       = ''R''  : Final rewind'
      GUID(  9)='       = ''T''  : Terminate, drop contro'//
     +'l-bank for this file and'
      GUID( 10)='                print file statistics'
      GUID( 11)='       = ''U''  : Unload the file'
      CALL KUGUID('FZENDI',GUID, 11,'S')
      CALL KUACT('FZENDI',INZAFZ)
 
      CALL KUNWG( 183)
      CALL KUCMD(' ','FZENDO','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZENDO','LUN','Logical unit number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('FZENDO','CHOPT','Character option','CO','S')
      CALL KUPVAL('FZENDO','CHOPT',0,0.,'TE','D')
      GUID(  1)='Write the output buffer and close an FZ '//
     +'file through the call'
      GUID(  2)='  CALL FZENDO (LUN, CHOPT)'
      GUID(  3)='Selcted option is specified by CHOPT.'
      GUID(  4)=' CHOPT = ''E''  : Write end-of-file (unl'//
     +'ess done)'
      GUID(  5)='       = ''E2'' : Write end-of-data (unl'//
     +'ess done)'
      GUID(  6)='       = ''F''  : Flush the buffer only'
      GUID(  7)='       = ''I''  : Switch to input, write'//
     +' end-of-data and rewind'
      GUID(  8)='                (if not yet done), cance'//
     +'l the output permission'
      GUID(  9)='       = ''Q''  : Quiet, suppress printi'//
     +'ng of file statistics'
      GUID( 10)='       = ''R''  : Final rewind'
      GUID( 11)='       = ''T''  : Terminate, write end-o'//
     +'f-run (if not yet done);'
      GUID( 12)='                drop control-bank for th'//
     +'is file and print the'
      GUID( 13)='                file statistics'
      GUID( 14)='       = ''U''  : Unload the file'
      CALL KUGUID('FZENDO',GUID, 14,'S')
      CALL KUACT('FZENDO',INZAFZ)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
      SUBROUTINE INZEMU          
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
 
      CALL KUNWG(   9)
      CALL KUCMD(' ','ZEBRA','C')
      GUID(  1)='Interfaces to the ZEBRA packages'
      CALL KUGUID('ZEBRA',GUID,  1,'S')
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
      SUBROUTINE INZEMZ          
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL INZAMZ
 
      CALL KUNWG(   0)
 
      CALL KUCMD('/ZEBRA',' ','SW')
 
      CALL KUNWG(  15)
      CALL KUCMD(' ','MZ','C')
      GUID(  1)='Interfaces to the MZ (Memory manager) pa'//
     +'ckage of ZEBRA'
      CALL KUGUID('MZ',GUID,  1,'S')
 
      CALL KUCMD('MZ',' ','SW')
 
      CALL KUNWG(  77)
      CALL KUCMD(' ','MZDIV','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZDIV','ISTOR','Store index (zero for primary store)',
     +'I','S')
      CALL KUPVAL('MZDIV','ISTOR',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZDIV','CNAME','Name of the division (upto 8 char.)'
     +,'C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZDIV','NWMIN','Minimum number of words in division'
     +,'IO','S')
      CALL KUPVAL('MZDIV','NWMIN',2000,0.,' ','D')
      CALL KUPVAL('MZDIV','NWMIN',100,0.,' ','L')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZDIV','NWMAX','Maximum size of the division','IO','S'
     +)
      CALL KUPVAL('MZDIV','NWMAX',10000,0.,' ','D')
      CALL KUPVAL('MZDIV','NWMAX',100,0.,' ','L')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZDIV','CHOPT','Character option','CO','S')
      CALL KUPVAL('MZDIV','CHOPT',0,0.,' ','D')
      GUID(  1)='Creates a new division inside a dynamic '//
     +'store'
      GUID(  2)='  CHOPT = ''R''  Reverse division (defau'//
     +'lt is Forward)'
      GUID(  3)='        = ''L''  Long term division (def'//
     +'ault is user short term)'
      GUID(  4)='        = ''P''  Package division (P imp'//
     +'lies C, overrides L)'
      GUID(  5)='        = ''C''  Division is contained ('//
     +'no links pointing outside)'
      CALL KUGUID('MZDIV',GUID,  5,'S')
      CALL KUACT('MZDIV',INZAMZ)
 
      CALL KUNWG(  99)
      CALL KUCMD(' ','MZLOGL','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZLOGL','ISTOR','Store index (zero for primary store)'
     +,'I','S')
      CALL KUPVAL('MZLOGL','ISTOR',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZLOGL','LOGL','Logging level','IO','S')
      CALL KUPVAL('MZLOGL','LOGL',0,0.,' ','D')
      GUID(  1)='Changes the log level for a store'
      GUID(  2)='  LOGL  = -3   Suppresses all log messag'//
     +'es'
      GUID(  3)='        = -2   Error messages only (ZFAT'//
     +'AL,ZPHASE)'
      GUID(  4)='        = -1   Terse logging (MZEBRA,MZS'//
     +'TOR,ZPHASE)'
      GUID(  5)='        =  0   Normal logging (MZDIV,MZL'//
     +'INK)'
      GUID(  6)='        =  1   Log to watch (MZLINT,MZGA'//
     +'RB,MZPUSH,MZFORM,MZIOBK,MZIOCH)'
      GUID(  7)='        =  2   Log to monitor (MZWORK,MZ'//
     +'BOOK,MZLIFT,MZDROP,MZLOGL)'
      CALL KUGUID('MZLOGL',GUID,  7,'S')
      CALL KUACT('MZLOGL',INZAMZ)
 
      CALL KUNWG( 217)
      CALL KUCMD(' ','MZBOOK','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZBOOK','IXDIV','Division index','I','S')
      CALL KUPVAL('MZBOOK','IXDIV',2,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZBOOK','JBIAS','Link bias in the supporting link','I'
     +,'S')
      CALL KUPVAL('MZBOOK','JBIAS',2,0.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZBOOK','CHIDH','The bank hollerith identifier','C','S
     +')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZBOOK','NL','Total number of links','I','S')
      CALL KUPVAL('MZBOOK','NL',0,0.,' ','L')
      CALL KUPVAL('MZBOOK','NL',6400,0.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZBOOK','NS','Number of structural links','I','S')
      CALL KUPVAL('MZBOOK','NS',0,0.,' ','L')
      CALL KUPVAL('MZBOOK','NS',6400,0.,' ','H')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZBOOK','ND','Total number of data words','I','S')
      CALL KUPVAL('MZBOOK','ND',0,0.,' ','L')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZBOOK','CHIO','IO characteristics of the data words',
     +'CO','S')
      CALL KUPVAL('MZBOOK','CHIO',0,0.,'2','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZBOOK','NZ','How much of the data is preset to zero',
     +'IO','S')
      CALL KUPVAL('MZBOOK','NZ',-1,0.,' ','D')
      CALL KUPVAL('MZBOOK','NZ',-1,0.,' ','L')
      GUID(  1)='Creates a bank through a call'
      GUID(  2)='  CALL MZBOOK (IXDIV, L, LSUP, JBIAS, CH'//
     +'IDH, NL, NS, ND, NIO, NZ)'
      GUID(  3)='If JBIAS = 2, a stand-alone bank is crea'//
     +'ted, otherwise JBIAS should'
      GUID(  4)='be less than or equal to 0 (JBIAS = 1 is'//
     +' not supported). For JBIAS'
      GUID(  5)='value .LE. 0, the user has to supply the'//
     +' identifier of the supporting'
      GUID(  6)='bank (both hollerith and numeric). In th'//
     +'is case, the address of the'
      GUID(  7)='supporting bank is computed though'
      GUID(  8)='   LSUP = LZFID  (IXDIV, IDH, IDN, 0)'
      GUID(  9)='   NIO  =  0   Undefined data type'
      GUID( 10)='        =  1   The whole bank is of type'//
     +' bit string'
      GUID( 11)='        =  2   The whole bank is of type'//
     +' integer'
      GUID( 12)='        =  3   The whole bank is of type'//
     +' floating point'
      GUID( 13)='        =  4   The whole bank is of type'//
     +' double precision'
      GUID( 14)='        =  5   The whole bank is of type'//
     +' Hollerith'
      GUID( 15)='        =  7   The whole bank is of self'//
     +' describing'
      CALL KUGUID('MZBOOK',GUID, 15,'S')
      CALL KUACT('MZBOOK',INZAMZ)
 
      CALL KUNWG(  94)
      CALL KUCMD(' ','MZPUSH','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZPUSH','CHIDH','Hollerith identifier of the bank','C'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZPUSH','IDN','Numeric identifier of the bank','I','S'
     +)
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZPUSH','IXDIV','Division index','IO','S')
      CALL KUPVAL('MZPUSH','IXDIV',2,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZPUSH','INCNL','Number of additional links','IO','S')
      CALL KUPVAL('MZPUSH','INCNL',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZPUSH','INCND','Number of additional data words','IO'
     +,'S')
      CALL KUPVAL('MZPUSH','INCND',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZPUSH','CHOPT','Character option','CO','S')
      CALL KUPVAL('MZPUSH','CHOPT',0,0.,' ','D')
      GUID(  1)='Alters the size of a bank through a call'
      GUID(  2)='  CALL MZPUSH (IXDIV, L, INCNL, INCND, C'//
     +'HOPT)'
      GUID(  3)='The address of the bank is computed thou'//
     +'gh'
      GUID(  4)='  L  = LZFID  (IXDIV, IDH, IDN, 0)'
      GUID(  5)='  CHOPT = '' ''  Any link may point to t'//
     +'he bank'
      GUID(  6)='        = ''R''  No link points to the a'//
     +'bandoned bank region'
      GUID(  7)='        = ''I''  Isolated case (no refer'//
     +'ence link pointing to the bank)'
      CALL KUGUID('MZPUSH',GUID,  7,'S')
      CALL KUACT('MZPUSH',INZAMZ)
 
      CALL KUNWG(  89)
      CALL KUCMD(' ','MZDROP','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZDROP','CHIDH','Hollerith identifier of the bank','C'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZDROP','IDN','Numeric identifier of the bank','I','S'
     +)
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZDROP','IXDIV','Division index','IO','S')
      CALL KUPVAL('MZDROP','IXDIV',2,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZDROP','CHOPT','Character option','CO','S')
      CALL KUPVAL('MZDROP','CHOPT',0,0.,' ','D')
      GUID(  1)='Drops a bank and its dependents through '//
     +'a call'
      GUID(  2)='  CALL MZDROP (IXDIV, L, CHOPT)'
      GUID(  3)='The address of the bank is computed thou'//
     +'gh'
      GUID(  4)='  L  = LZFID  (IXDIV, IDH, IDN, 0)'
      GUID(  5)='  CHOPT = '' ''  Drop the bank and its v'//
     +'ertical dependents'
      GUID(  6)='        = ''L''  Drop the linear structu'//
     +'re'
      GUID(  7)='        = ''V''  Drop only the vertical '//
     +'dependents, not the bank itself'
      CALL KUGUID('MZDROP',GUID,  7,'S')
      CALL KUACT('MZDROP',INZAMZ)
 
      CALL KUNWG(  24)
      CALL KUCMD(' ','MZWIPE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZWIPE','IXDIV','Division index','I','S')
      CALL KUPVAL('MZWIPE','IXDIV',2,0.,' ','D')
      GUID(  1)='The contents of a complete division are '//
     +'deleted through a call'
      GUID(  2)='  CALL MZWIPE (IXDIV)'
      CALL KUGUID('MZWIPE',GUID,  2,'S')
      CALL KUACT('MZWIPE',INZAMZ)
 
      CALL KUNWG(  63)
      CALL KUCMD(' ','MZNEED','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZNEED','NEEDW','Number of words needed in the divisio
     +n','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZNEED','IXDIV','Division index','IO','S')
      CALL KUPVAL('MZNEED','IXDIV',2,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MZNEED','CHOPT','Character option','CO','S')
      CALL KUPVAL('MZNEED','CHOPT',0,0.,' ','D')
      GUID(  1)='Finds if NEEDW words are available in th'//
     +'e division through a call'
      GUID(  2)='  CALL MZNEED (IXDIV, NEEDW, CHOPT)'
      GUID(  3)='  CHOPT = '' ''  Do not grabage collect '//
     +'the division to free space'
      GUID(  4)='        = ''G''  Garbage collect the div'//
     +'ision to increase the space'
      CALL KUGUID('MZNEED',GUID,  4,'S')
      CALL KUACT('MZNEED',INZAMZ)
 
      CALL KUNWG(  43)
      CALL KUCMD(' ','LZFIDH','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFIDH','CHIDH','Hollerith name of the bank','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFIDH','IXDIV','Division index','IO','S')
      CALL KUPVAL('LZFIDH','IXDIV',2,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFIDH','LGO','The address after which the scan start'
     +,'IO','S')
      CALL KUPVAL('LZFIDH','LGO',0,0.,' ','D')
      GUID(  1)='Returns the address of the first bank wi'//
     +'th the specified identifier'
      GUID(  2)='  LF = LZFIDH (IXDIV, IDH, LGO)'
      GUID(  3)='  LGO = 0 means start with the first ban'//
     +'k in the division'
      CALL KUGUID('LZFIDH',GUID,  3,'S')
      CALL KUACT('LZFIDH',INZAMZ)
 
      CALL KUNWG(  44)
      CALL KUCMD(' ','LZFIDN','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFIDN','CHIDH','Hollerith name of the bank','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFIDN','IDN','Numeric identifier of the bank','I','S'
     +)
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFIDN','IXDIV','Division index','IO','S')
      CALL KUPVAL('LZFIDN','IXDIV',2,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFIDN','LGO','The address after which the scan start'
     +,'IO','S')
      CALL KUPVAL('LZFIDN','LGO',0,0.,' ','D')
      GUID(  1)='Returns the address of the first bank wi'//
     +'th the specified identifier'
      GUID(  2)='  LF = LZFID  (IXDIV, IDH, IDN, LGO)'
      GUID(  3)='  LGO = 0 means start with the first ban'//
     +'k in the division'
      CALL KUGUID('LZFIDN',GUID,  3,'S')
      CALL KUACT('LZFIDN',INZAMZ)
 
      CALL KUNWG(  54)
      CALL KUCMD(' ','LZLAST','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZLAST','CHIDH','Hollerith name of the bank','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZLAST','IDN','Numeric identifier of the bank','I','S'
     +)
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZLAST','IXDIV','Division index','IO','S')
      CALL KUPVAL('LZLAST','IXDIV',2,0.,' ','D')
      GUID(  1)='Returns the address of the last bank in '//
     +'a linear structure pointed to'
      GUID(  2)='by a bank with the given identifier thro'//
     +'ugh the calls'
      GUID(  3)='  LLS = LZFID  (IXDIV, IDH, IDN, 0)     '//
     +'and'
      GUID(  4)='  LF  = LZLAST (IXDIV, LLS)'
      CALL KUGUID('LZLAST',GUID,  4,'S')
      CALL KUACT('LZLAST',INZAMZ)
 
      CALL KUNWG(  63)
      CALL KUCMD(' ','LZFIND','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFIND','CHIDH','Hollerith name of the bank','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFIND','IDN','Numeric identifier of the bank','I','S'
     +)
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFIND','IT','Given value of the data word','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFIND','JW','Data word','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFIND','IXDIV','Division index','IO','S')
      CALL KUPVAL('LZFIND','IXDIV',2,0.,' ','D')
      GUID(  1)='Returns the address of the first bank in'//
     +' a linear structure pointed to'
      GUID(  2)='by a bank with the given identifier cont'//
     +'aining IT in word JW through'
      GUID(  3)='the calls'
      GUID(  4)='  LLS = LZFID  (IXDIV, IDH, IDN, 0)     '//
     +'and'
      GUID(  5)='  LF  = LZFIND (IXDIV, LLS, IT, JW)'
      CALL KUGUID('LZFIND',GUID,  5,'S')
      CALL KUACT('LZFIND',INZAMZ)
 
      CALL KUNWG(  82)
      CALL KUCMD(' ','LZBYT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZBYT','CHIDH','Hollerith name of the bank','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZBYT','IDN','Numeric identifier of the bank','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZBYT','IT','Given value of the byte','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZBYT','JB','Starting bit of the status word','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZBYT','NB','Number of bits in the status word','I','S
     +')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZBYT','IXDIV','Division index','IO','S')
      CALL KUPVAL('LZBYT','IXDIV',2,0.,' ','D')
      GUID(  1)='Returns the address of the first bank in'//
     +' a linear structure pointed to'
      GUID(  2)='by a bank with the given identifier cont'//
     +'aining IT in the byte of the'
      GUID(  3)='status word starting at JBIT and with a '//
     +'width of NBIT bits through'
      GUID(  4)='the calls'
      GUID(  5)='  LLS = LZFID  (IXDIV, IDH, IDN, 0)     '//
     +'and'
      GUID(  6)='  LF  = LZBYT  (IXDIV, LLS, IT, JB, NB)'
      CALL KUGUID('LZBYT',GUID,  6,'S')
      CALL KUACT('LZBYT',INZAMZ)
 
      CALL KUNWG(  83)
      CALL KUCMD(' ','LZFVAL','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFVAL','CHIDH','Hollerith name of the bank','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFVAL','IDN','Numeric identifier of the bank','I','S'
     +)
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFVAL','VAL','Given value of the data word','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFVAL','TOL','Tolerance for checking','R','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFVAL','JW','Data word','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LZFVAL','IXDIV','Division index','IO','S')
      CALL KUPVAL('LZFVAL','IXDIV',2,0.,' ','D')
      GUID(  1)='Returns the address of the first bank in'//
     +' a linear structure pointed'
      GUID(  2)='to by a bank with the given identifier c'//
     +'ontaining in word JW a floating'
      GUID(  3)='point number which is equal to VAL withi'//
     +'n the tolerance TOL through'
      GUID(  4)='the calls'
      GUID(  5)='  LLS = LZFID  (IXDIV, IDH, IDN, 0)     '//
     +'and'
      GUID(  6)='  LF  = LZFVAL (IXDIV, LLS, VAL, TOL, JW'//
     +')'
      CALL KUGUID('LZFVAL',GUID,  6,'S')
      CALL KUACT('LZFVAL',INZAMZ)
 
      CALL KUNWG(  51)
      CALL KUCMD(' ','NZBANK','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('NZBANK','CHIDH','Hollerith name of the bank','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('NZBANK','IDN','Numeric identifier of the bank','I','S'
     +)
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('NZBANK','IXDIV','Division index','IO','S')
      CALL KUPVAL('NZBANK','IXDIV',2,0.,' ','D')
      GUID(  1)='Counts the number of banks in a linear s'//
     +'tructure pointed to by a bank'
      GUID(  2)='with the given identifier through the ca'//
     +'lls'
      GUID(  3)='  LLS = LZFID  (IXDIV, IDH, IDN, 0)     '//
     +'and'
      GUID(  4)='  N   = NZBANK (IXDIV, LLS)'
      CALL KUGUID('NZBANK',GUID,  4,'S')
      CALL KUACT('NZBANK',INZAMZ)
 
      CALL KUNWG(  90)
      CALL KUCMD(' ','NZFIND','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('NZFIND','CHIDH','Hollerith name of the bank','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('NZFIND','IDN','Numeric identifier of the bank','I','S'
     +)
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('NZFIND','IT','Given value of the data word','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('NZFIND','JW','Data word','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('NZFIND','IXDIV','Division index','IO','S')
      CALL KUPVAL('NZFIND','IXDIV',2,0.,' ','D')
      GUID(  1)='Returns the number and addresses of all '//
     +'banks in a linear structure'
      GUID(  2)='pointed to by a bank with the given iden'//
     +'tifier containing IT in word JW'
      GUID(  3)='through the calls'
      GUID(  4)='  LLS = LZFID  (IXDIV, IDH, IDN, 0)     '//
     +'and'
      GUID(  5)='  N   = LZFIND (IXDIV, LLS, IT, JW)'
      GUID(  6)='The addresses of the first 100 such bank'//
     +'s are stored into IQUEST(1..100)'
      GUID(  7)='in common /QUEST/'
      CALL KUGUID('NZFIND',GUID,  7,'S')
      CALL KUACT('NZFIND',INZAMZ)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
      SUBROUTINE INZERZ          
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL INZARZ
 
      CALL KUNWG(   0)
 
      CALL KUCMD('/ZEBRA',' ','SW')
 
      CALL KUNWG(  16)
      CALL KUCMD(' ','RZ','C')
      GUID(  1)='Interfaces to the RZ (Direct access I/O)'//
     +' package of ZEBRA'
      CALL KUGUID('RZ',GUID,  1,'S')
 
      CALL KUCMD('RZ',' ','SW')
 
      CALL KUNWG( 181)
      CALL KUCMD(' ','RZMAKE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZMAKE','LUN','Logical Unit Number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZMAKE','FNAME','Name of the RZ file','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZMAKE','LRECL','Record length in words','I','S')
      CALL KUPVAL('RZMAKE','LRECL',1000,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZMAKE','CHOPT','Character option','C','S')
      CALL KUPVAL('RZMAKE','CHOPT',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZMAKE','CHDIR','Name of the top directory','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZMAKE','NREC','Number of records in the file','IO','S
     +')
      CALL KUPVAL('RZMAKE','NREC',4000,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZMAKE','NWKEY','Number of key elements','IO','S')
      CALL KUPVAL('RZMAKE','NWKEY',9,0.,' ','D')
      CALL KUNDPV(   3,   1,   1,   0,   1)
      CALL KUPAR('RZMAKE','CFORM','Description of each key element','CO'
     +,'S')
      CALL KUPVAL('RZMAKE','CFORM',0,0.,'IIIIIIIII','D')
      GUID(  1)='Open a random access file and create a n'//
     +'ew RZ file through the call'
      GUID(  2)='  CALL RZMAKE (LUN, CHDIR, NWKEY, CFORM,'//
     +' CHTAG, NREC, CHOPT)'
      GUID(  3)='Name of the top directory is limited to '//
     +'16 characters. Number'
      GUID(  4)='of key elements is limited to 100. Descr'//
     +'iption of each key'
      GUID(  5)='element as specified by CFORM has the fo'//
     +'llowing meaning,'
      GUID(  6)='   ''B'' : Bit string but not zero'
      GUID(  7)='   ''H'' : Hollerith (4 characters)'
      GUID(  8)='   ''A'' : Same as H except for printout'//
     +' purpose (where it will be'
      GUID(  9)='         concatenated with the previous '//
     +'key with type H or A)'
      GUID( 10)='   ''I'' : Integer'
      GUID( 11)='Selected option for making the RZ file i'//
     +'s given in CHOPT'
      GUID( 12)=' CHOPT = '' ''  : Native mode'
      GUID( 13)='       = ''X''  : Exchange mode'
      GUID( 14)='See ZEBRA user guide for RZMAKE'
      CALL KUGUID('RZMAKE',GUID, 14,'S')
      CALL KUACT('RZMAKE',INZARZ)
 
      CALL KUNWG(  91)
      CALL KUCMD(' ','RZFILE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFILE','LUN','Logical Unit Number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFILE','FNAME','Name of the RZ file','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFILE','LRECL','Record length in words','I','S')
      CALL KUPVAL('RZFILE','LRECL',1000,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFILE','CHOPT','Character option','C','S')
      CALL KUPVAL('RZFILE','CHOPT',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFILE','CHDIR','Name of the top directory','C','S')
      GUID(  1)='Open an existing random access file and '//
     +'initialize RZ through the call'
      GUID(  2)='  CALL RZFILE (LUN, CHDIR, CHOPT)'
      GUID(  3)='Selected option for opening the RZ file '//
     +'is given in CHOPT'
      GUID(  4)=' CHOPT = '' ''  : Raed mode'
      GUID(  5)='       = ''S''  : Shared mode'
      GUID(  6)='       = ''U''  : Update mode'
      GUID(  7)='       = ''1''  : Update mode and only o'//
     +'ne user'
      GUID(  8)='See ZEBRA user guide for RZFILE'
      CALL KUGUID('RZFILE',GUID,  8,'S')
      CALL KUACT('RZFILE',INZARZ)
 
      CALL KUNWG( 106)
      CALL KUCMD(' ','RZLOGL','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZLOGL','LUN','Logical Unit Number','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZLOGL','LOGL','Logging level','IO','S')
      CALL KUPVAL('RZLOGL','LOGL',0,0.,' ','D')
      GUID(  1)='Set the logging level of a RZ file throu'//
     +'gh the call'
      GUID(  2)='  CALL RZLOGL (LUN, LOGL)'
      GUID(  3)='  LOGL  = -3   Suppress all log messages'
      GUID(  4)='        = -2   Error messages only'
      GUID(  5)='        = -1   Terse logging'
      GUID(  6)='        =  0   Normal logging (RZFILE,RZ'//
     +'MAKE,RZEND)'
      GUID(  7)='        =  1   Log to watch rare events'
      GUID(  8)='        =  2   Log to monitor calls'
      GUID(  9)='        =  3   Short dumps to debug'
      GUID( 10)='        =  4   Full dumps to debug'
      CALL KUGUID('RZLOGL',GUID, 10,'S')
      CALL KUACT('RZLOGL',INZARZ)
 
      CALL KUNWG(  74)
      CALL KUCMD(' ','RZEND','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZEND','CHDIR','Name of the top directory','C','S')
      GUID(  1)='Closes a direct access file, identified '//
     +'through a top directory name'
      GUID(  2)='by the call'
      GUID(  3)='  CALL RZEND (CHDIR)'
      GUID(  4)='The directories present in memory, when '//
     +'they have been changed are'
      GUID(  5)='copied to the file and then deleted from'//
     +' memory, else the directories'
      GUID(  6)='in memory are simply deleted.'
      CALL KUGUID('RZEND',GUID,  6,'S')
      CALL KUACT('RZEND',INZARZ)
 
      CALL KUNWG(  40)
      CALL KUCMD(' ','RZSAVE','C')
      GUID(  1)='All directories which have been modified'//
     +' in memory and the current'
      GUID(  2)='output buffer are written to the output '//
     +'file through the call'
      GUID(  3)='  CALL RZSAVE'
      CALL KUGUID('RZSAVE',GUID,  3,'S')
      CALL KUACT('RZSAVE',INZARZ)
 
      CALL KUNWG(  47)
      CALL KUCMD(' ','RZQUOT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZQUOT','CHDIR','Name of the top directory','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZQUOT','NQUOT','Number of records','I','S')
      CALL KUPVAL('RZQUOT','NQUOT',5000,0.,' ','D')
      GUID(  1)='Change the quota (maximum number of reco'//
     +'rds allowed in a file).'
      GUID(  2)='This is useful when the RZ file reaches '//
     +'the size as specified by'
      GUID(  3)='NREC in the RZMAKE call which creates th'//
     +'e file.'
      CALL KUGUID('RZQUOT',GUID,  3,'S')
      CALL KUACT('RZQUOT',INZARZ)
 
      CALL KUNWG(  47)
      CALL KUCMD(' ','RZLOCK','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZLOCK','CPATH','Directory to be locked','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZLOCK','CLOCK','Lock identifier','C','S')
      GUID(  1)='Lock the directory CHDIR (get exclusive '//
     +'access) through the calls'
      GUID(  2)='  CALL RZCDIR (CPATH, '' '')'
      GUID(  3)='  CALL RZLOCK (CLOCK)'
      GUID(  4)='The lock must be released with the comma'//
     +'nd RZFREE'
      CALL KUGUID('RZLOCK',GUID,  4,'S')
      CALL KUACT('RZLOCK',INZARZ)
 
      CALL KUNWG(  26)
      CALL KUCMD(' ','RZFREE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFREE','CLOCK','Lock identifier','C','S')
      GUID(  1)='Release a lock previously set with the c'//
     +'ommand RZLOCK through the call'
      GUID(  2)='  CALL RZFREE (CLOCK)'
      CALL KUGUID('RZFREE',GUID,  2,'S')
      CALL KUACT('RZFREE',INZARZ)
 
      CALL KUNWG(  38)
      CALL KUCMD(' ','RZLLOK','C')
      GUID(  1)='List all the locks currently active in a'//
     +'ll open RZ files.'
      GUID(  2)='This shows the list of users who are usi'//
     +'ng these files'
      GUID(  3)='concurrently with you.'
      CALL KUGUID('RZLLOK',GUID,  3,'S')
      CALL KUACT('RZLLOK',INZARZ)
 
      CALL KUNWG(  47)
      CALL KUCMD(' ','RZNDIR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZNDIR','CPATH','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZNDIR','CHOPT','Character option','CO','S')
      CALL KUPVAL('RZNDIR','CHOPT',0,0.,' ','D')
      GUID(  1)='Set or print the naming directory throug'//
     +'h the call'
      GUID(  2)='  CALL RZNDIR (CPATH, CHOPT)'
      GUID(  3)='  CHOPT = '' ''  Set the naming director'//
     +'y to CPATH'
      GUID(  4)='        = ''P''  Print the naming direct'//
     +'ory'
      CALL KUGUID('RZNDIR',GUID,  4,'S')
      CALL KUACT('RZNDIR',INZARZ)
 
      CALL KUNWG(  61)
      CALL KUCMD(' ','RZCDIR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZCDIR','CPATH','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZCDIR','CHOPT','Character option','CO','S')
      CALL KUPVAL('RZCDIR','CHOPT',0,0.,' ','D')
      GUID(  1)='Set or print the current directory throu'//
     +'gh the call'
      GUID(  2)='  CALL RZCDIR (CPATH, CHOPT)'
      GUID(  3)='  CHOPT = '' ''  Set the current directo'//
     +'ry to CPATH'
      GUID(  4)='        = ''P''  Print the current direc'//
     +'tory'
      GUID(  5)='See ZEBRA user guide for RZCDIR for more'//
     +' detail'
      CALL KUGUID('RZCDIR',GUID,  5,'S')
      CALL KUACT('RZCDIR',INZARZ)
 
      CALL KUNWG(  41)
      CALL KUCMD(' ','RZLDIR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZLDIR','CPATH','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZLDIR','CHOPT','Character option','CO','S')
      CALL KUPVAL('RZLDIR','CHOPT',0,0.,' ','D')
      GUID(  1)='Print the objects and the subdirectories'//
     +' belonging to a given pathname'
      GUID(  2)='through the call'
      GUID(  3)='  CALL RZLDIR (CPATH, CHOPT)'
      GUID(  4)='See ZEBRA user guide for RZLDIR'
      CALL KUGUID('RZLDIR',GUID,  4,'S')
      CALL KUACT('RZLDIR',INZARZ)
 
      CALL KUNWG( 132)
      CALL KUCMD(' ','RZMDIR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZMDIR','CHDIR','Name of the directory to be created',
     +'C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZMDIR','NWKEY','Number of key elements','IO','S')
      CALL KUPVAL('RZMDIR','NWKEY',9,0.,' ','D')
      CALL KUNDPV(   3,   1,   1,   0,   1)
      CALL KUPAR('RZMDIR','CFORM','Description of each key element','CO'
     +,'S')
      CALL KUPVAL('RZMDIR','CFORM',0,0.,'IIIIIIIII','D')
      GUID(  1)='Create a directory below the current wor'//
     +'king directory'
      GUID(  2)='through the call'
      GUID(  3)='  CALL RZMDIR (CHDIR, NWKEY, CFORM, CHTA'//
     +'G)'
      GUID(  4)='Number of key elements is limited to 100'//
     +'. Description of each key'
      GUID(  5)='element as specified by CFORM has the fo'//
     +'llowing meaning,'
      GUID(  6)='   ''B'' : Bit string but not zero'
      GUID(  7)='   ''H'' : Hollerith (4 characters)'
      GUID(  8)='   ''A'' : Same as H except for printout'//
     +' purpose (where it will be'
      GUID(  9)='         concatenated with the previous '//
     +'key with type H or A)'
      GUID( 10)='   ''I'' : Integer'
      GUID( 11)='See ZEBRA user guide for RZMDIR'
      CALL KUGUID('RZMDIR',GUID, 11,'S')
      CALL KUACT('RZMDIR',INZARZ)
 
      CALL KUNWG(  43)
      CALL KUCMD(' ','RZPURG','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZPURG','NKEEP','Number of cycles to be kept','I','S')
      GUID(  1)='Delete all but the last NKEEP cycles for'//
     +' all the objects in the current'
      GUID(  2)='directory through the call'
      GUID(  3)='  CALL RZPURG (NKEEP)'
      GUID(  4)='See ZEBRA user guide for RZPURG'
      CALL KUGUID('RZPURG',GUID,  4,'S')
      CALL KUACT('RZPURG',INZARZ)
 
      CALL KUNWG(  34)
      CALL KUCMD(' ','RZDELT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZDELT','CHDIR','Name of the directory to be deleted',
     +'C','S')
      GUID(  1)='Delete a subtree of the current working '//
     +'directory through the call'
      GUID(  2)='  CALL RZDELT (CHDIR)'
      GUID(  3)='See ZEBRA user guide for RZDELT'
      CALL KUGUID('RZDELT',GUID,  3,'S')
      CALL KUACT('RZDELT',INZARZ)
 
      CALL KUNWG(  45)
      CALL KUCMD(' ','RZSTAT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZSTAT','CHDIR','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZSTAT','NLEV','Number of levels for which data needed
     +','IO','S')
      CALL KUPVAL('RZSTAT','NLEV',16,0.,' ','D')
      GUID(  1)='Print statistics about space used by sub'//
     +'directories below CHDIR'
      GUID(  2)='(upto NLEV levels) through the call'
      GUID(  3)='  CALL RZSTAT (CHDIR, NLEV, '' '')'
      GUID(  4)='See ZEBRA user guide for RZSTAT'
      CALL KUGUID('RZSTAT',GUID,  4,'S')
      CALL KUACT('RZSTAT',INZARZ)
 
      CALL KUNWG(  67)
      CALL KUCMD(' ','RZKEYD','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZKEYD','CHDIR','Pathname','C','S')
      GUID(  1)='Print information about the key definiti'//
     +'ons for directory CHDIR'
      GUID(  2)='through the calls'
      GUID(  3)='  CALL RZCDIR (CHDIR, '' '')'
      GUID(  4)='  CALL RZKEYD (NWKEY, CFORM, CHTAG)'
      GUID(  5)='IO characteristics and tag of each of th'//
     +'e keys are printed out.'
      GUID(  6)='See ZEBRA user guide for RZKEYD'
      CALL KUGUID('RZKEYD',GUID,  6,'S')
      CALL KUACT('RZKEYD',INZARZ)
 
      CALL KUNWG( 177)
      CALL KUCMD(' ','RZTOFZ','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZTOFZ','LUN','Logical unit number of the FZ file','I'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZTOFZ','FNAME','File name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZTOFZ','LRECL','Record length in words','IO','S')
      CALL KUPVAL('RZTOFZ','LRECL',900,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZTOFZ','CHOPT','Character option for FZ File','CO','S
     +')
      CALL KUPVAL('RZTOFZ','CHOPT',0,0.,'OX','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZTOFZ','CHOP','Character option for RZTOFZ','CO','S')
      CALL KUPVAL('RZTOFZ','CHOP',0,0.,' ','D')
      GUID(  1)='Copy the current directory tree onto an '//
     +'FZ file through the call'
      GUID(  2)='  CALL RZTOFZ (LUN, CHOP)'
      GUID(  3)='The sequntial file is opened first throu'//
     +'gh a FORTRAN OPEN,'
      GUID(  4)='followed by a call to FZFILE (LUN, LRECL'//
     +', CHOPT). All objects of'
      GUID(  5)='the current directory (keys + data struc'//
     +'tures) are first read'
      GUID(  6)='into the system division of the primary '//
     +'store before their'
      GUID(  7)='transfer to the output file. Choice of t'//
     +'he objects is steered'
      GUID(  8)='though the character variable CHOP.'
      GUID(  9)=' CHOP  = '' ''  : Write the highest cycl'//
     +'e of the keys in the CWD'
      GUID( 10)='                to the FZ file'
      GUID( 11)='       = ''C''  : Write all cycles of th'//
     +'e keys in the CWD to the'
      GUID( 12)='                FZ file'
      GUID( 13)='See ZEBRA user guide for RZTOFZ'
      CALL KUGUID('RZTOFZ',GUID, 13,'S')
      CALL KUACT('RZTOFZ',INZARZ)
 
      CALL KUNWG( 169)
      CALL KUCMD(' ','RZFRFZ','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFRFZ','LUN','Logical unit number of the FZ file','I'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFRFZ','FNAME','File name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFRFZ','LRECL','Record length in words','IO','S')
      CALL KUPVAL('RZFRFZ','LRECL',900,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFRFZ','CHOPT','Character option for FZ File','CO','S
     +')
      CALL KUPVAL('RZFRFZ','CHOPT',0,0.,'IX','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFRFZ','CHOP','Character option for RZFRFZ','CO','S')
      CALL KUPVAL('RZFRFZ','CHOP',0,0.,' ','D')
      GUID(  1)='Copy the FZ file into the current direct'//
     +'ory tree through the call'
      GUID(  2)='  CALL RZFRFZ (LUN, CHOP)'
      GUID(  3)='The sequntial file is opened first throu'//
     +'gh a FORTRAN OPEN,'
      GUID(  4)='followed by a call to FZFILE (LUN, LRECL'//
     +', CHOPT). If a sub-directory'
      GUID(  5)='with the same name as the one read in is'//
     +' already present in the CWD,'
      GUID(  6)='then a new cyccle is created for the int'//
     +'roduced objects.'
      GUID(  7)='Choice of the objects is steered though '//
     +'the character variable CHOP.'
      GUID(  8)=' CHOP  = '' ''  : Read all cycles of the'//
     +' keys in the CWD to the'
      GUID(  9)='                FZ file'
      GUID( 10)='       = ''H''  : Read the highest cycle'//
     +' of the keys in the CWD to the'
      GUID( 11)='                FZ file'
      GUID( 12)='See ZEBRA user guide for RZFRFZ'
      CALL KUGUID('RZFRFZ',GUID, 12,'S')
      CALL KUACT('RZFRFZ',INZARZ)
 
      CALL KUNWG( 186)
      CALL KUCMD(' ','RZTOALPHA','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZTOALPHA','LUN','Logical unit number of the FZ file',
     +'I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZTOALPHA','FNAME','File name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZTOALPHA','LRECL','Record length in words','IO','S')
      CALL KUPVAL('RZTOALPHA','LRECL',20,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZTOALPHA','CHOPT','Character option for FZ File','CO'
     +,'S')
      CALL KUPVAL('RZTOALPHA','CHOPT',0,0.,'AO','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZTOALPHA','CHOP','Character option for RZTOFZ','CO'
     +,'S')
      CALL KUPVAL('RZTOALPHA','CHOP',0,0.,' ','D')
      GUID(  1)='Copy the current directory tree onto an '//
     +'FZ file in Alphanumeric format.'
      GUID(  2)='This file FNAME can be exchanged between'//
     +' different machines.'
      GUID(  3)='The sequntial file is opened first throu'//
     +'gh a FORTRAN OPEN,'
      GUID(  4)='followed by a call to FZFILE (LUN, LRECL'//
     +', CHOPT). All objects of'
      GUID(  5)='the current directory (keys + data struc'//
     +'tures) are first read'
      GUID(  6)='into the system division of the primary '//
     +'store before their'
      GUID(  7)='transfer to the output file through the '//
     +'call RZTOFZ (LUN, CHOP).'
      GUID(  8)='Choice of the objects is steered though '//
     +'the character variable CHOP.'
      GUID(  9)=' CHOP  = '' ''  : Write the highest cycl'//
     +'e of the keys in the CWD'
      GUID( 10)='                to the FZ file'
      GUID( 11)='       = ''C''  : Write all cycles of th'//
     +'e keys in the CWD to the'
      GUID( 12)='                FZ file'
      CALL KUGUID('RZTOALPHA',GUID, 12,'S')
      CALL KUACT('RZTOALPHA',INZARZ)
 
      CALL KUNWG( 179)
      CALL KUCMD(' ','RZFRALPHA','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFRALPHA','LUN','Logical unit number of the FZ file',
     +'I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFRALPHA','FNAME','File name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFRALPHA','LRECL','Record length in words','IO','S')
      CALL KUPVAL('RZFRALPHA','LRECL',20,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFRALPHA','CHOPT','Character option for FZ File','CO'
     +,'S')
      CALL KUPVAL('RZFRALPHA','CHOPT',0,0.,'AI','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('RZFRALPHA','CHOP','Character option for RZFRFZ','CO'
     +,'S')
      CALL KUPVAL('RZFRALPHA','CHOP',0,0.,' ','D')
      GUID(  1)='Copy the FZ file in Alphanumeric format '//
     +'into the current directory tree.'
      GUID(  2)='The sequntial file is opened first throu'//
     +'gh a FORTRAN OPEN,'
      GUID(  3)='followed by a call to FZFILE (LUN, LRECL'//
     +', CHOPT). The directory tree'
      GUID(  4)='is then read in from the FZ file into th'//
     +'e CWD using RZFRFZ (LUN, CHOP).'
      GUID(  5)='If a sub-directory with the same name as'//
     +' the one read in is already'
      GUID(  6)='present in the CWD, then a new cyccle is'//
     +' created for the introduced'
      GUID(  7)='objects. Choice of the objects is steere'//
     +'d though the character'
      GUID(  8)='variable CHOP.'
      GUID(  9)=' CHOP  = '' ''  : Read all cycles of the'//
     +' keys in the CWD to the'
      GUID( 10)='                FZ file'
      GUID( 11)='       = ''H''  : Read the highest cycle'//
     +' of the keys in the CWD to the'
      GUID( 12)='                FZ file'
      CALL KUGUID('RZFRALPHA',GUID, 12,'S')
      CALL KUACT('RZFRALPHA',INZARZ)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
      SUBROUTINE INDBAU          
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL DBAUXI
 
      CALL KUNWG(   0)
 
      CALL KUCMD('/DBL3',' ','SW')
 
      CALL KUNWG(   8)
      CALL KUCMD(' ','AUXILIARY','C')
      GUID(  1)='Auxiliary DBL3 Utility menu'
      CALL KUGUID('AUXILIARY',GUID,  1,'S')
 
      CALL KUCMD('AUXILIARY',' ','SW')
 
      CALL KUNWG( 123)
      CALL KUCMD(' ','DBASCI','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBASCI','PATHN','Pathname','C','S')
      CALL KUNDPV(   4,   1,   1,   0,   1)
      CALL KUPAR('DBASCI','CFNAM','Key-file name','C','S')
      CALL KUPVAL('DBASCI','CFNAM',0,0.,'KFNAME.FILEXT','D')
      GUID(  1)='Insert objects from a pre-edited file.'
      GUID(  2)='The file contains only the values of the'//
     +' keys in the standard'
      GUID(  3)='horizontal or vertical mode of display. '//
     +'There should be the'
      GUID(  4)='letter N for every new key to be inserte'//
     +'d in the appropriate'
      GUID(  5)='column; column 1 in the line with the ke'//
     +'ys of a given object for'
      GUID(  6)='Horizontal mode and column 20 in the fir'//
     +'st line describing the'
      GUID(  7)='keys for Vertical mode. Data part can be'//
     +' optionally added when'
      GUID(  8)='the program separately asks for the data'//
     +'.'
      CALL KUGUID('DBASCI',GUID,  8,'S')
      CALL KUACT('DBASCI',DBAUXI)
 
      CALL KUNWG(  31)
      CALL KUCMD(' ','DBDISP','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBDISP','PATHN','Pathname','C','S')
      CALL KUNDPV(   4,   1,   1,   0,   1)
      CALL KUPAR('DBDISP','CFNAM','Display file name (.FILEXT)','C','S')
      CALL KUPVAL('DBDISP','CFNAM',0,0.,'KFNAME.FILEXT','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBDISP','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBDISP','CHOPT',0,0.,'H','D')
      GUID(  1)='Displays all objects (keys only) in a gi'//
     +'ven directory,'
      GUID(  2)='in Horizontal (with CHOPT=''H'') or in V'//
     +'ertical (''V'') mode.'
      CALL KUGUID('DBDISP',GUID,  2,'S')
      CALL KUACT('DBDISP',DBAUXI)
 
      CALL KUNWG(  19)
      CALL KUCMD(' ','DBEALI','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBEALI','PATHN','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBEALI','ALIAS','Alias name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBEALI','IFLAG','Flag (0 if temporary; 1 if permanent)
     +','IO','S')
      CALL KUPVAL('DBEALI','IFLAG',0,0.,' ','D')
      GUID(  1)='Give an alias name (limited to 8 charact'//
     +'ers) to a directory path name'
      CALL KUGUID('DBEALI',GUID,  1,'S')
      CALL KUACT('DBEALI',DBAUXI)
 
      CALL KUNWG( 253)
      CALL KUCMD(' ','DBEDIT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBEDIT','PATHN','Pathname','C','S')
      CALL KUNDPV(   4,   1,   1,   0,   1)
      CALL KUPAR('DBEDIT','CFNAM','Key-file name','C','S')
      CALL KUPVAL('DBEDIT','CFNAM',0,0.,'KFNAME.FILEXT','D')
      GUID(  1)='Creates Directories, inserts or updates '//
     +'keys and data for a given'
      GUID(  2)='path name. There are two possible modes '//
     +'of operation for inserting'
      GUID(  3)='the key elements of the new object. In t'//
     +'he horizontal mode, where'
      GUID(  4)='all the key elements are displayed in a '//
     +'single line, one should'
      GUID(  5)='insert ''N'' in column 1 for every new o'//
     +'bject. To replace an existing'
      GUID(  6)='object (ala DBREPL), one has to put ''R'''//
     +' in front of the existing'
      GUID(  7)='object, followed by a new line with the '//
     +'keys of the new object with'
      GUID(  8)='''N'' in front. In the vertical mode, al'//
     +'l the keys are displayed in'
      GUID(  9)='seperate lines with one header line for '//
     +'every object. As in the'
      GUID( 10)='horizontal mode, one should insert chara'//
     +'cter ''N'' or ''R'' in the header'
      GUID( 11)='line in column 20 (where the letter ''D'''//
     +' appears in the display).'
      GUID( 12)='Data can be optionally added to the obje'//
     +'ct as requested by the'
      GUID( 13)='interactive program. Only vertical mode '//
     +'(one line for each data'
      GUID( 14)='element) is so far envisaged in this pro'//
     +'gram.'
      GUID( 15)='Note : No compression of data is done.'
      CALL KUGUID('DBEDIT',GUID, 15,'S')
      CALL KUACT('DBEDIT',DBAUXI)
 
      CALL KUNWG(  16)
      CALL KUCMD(' ','DBEHLP','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBEHLP','PATHN','Pathname','C','S')
      CALL KUNDPV(   4,   1,   1,   0,   1)
      CALL KUPAR('DBEHLP','CFNAM','File name','C','S')
      CALL KUPVAL('DBEHLP','CFNAM',0,0.,'KFNAME.FILEXT','D')
      GUID(  1)='Enter help information of a given direct'//
     +'ory into data base'
      CALL KUGUID('DBEHLP',GUID,  1,'S')
      CALL KUACT('DBEHLP',DBAUXI)
 
      CALL KUNWG(  17)
      CALL KUCMD(' ','DBENAM','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBENAM','PATHN','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBENAM','NWDS','Number of data words','I','S')
      GUID(  1)='Enter names of data elements of a given '//
     +'directory into data base'
      CALL KUGUID('DBENAM',GUID,  1,'S')
      CALL KUACT('DBENAM',DBAUXI)
 
      CALL KUNWG(  93)
      CALL KUCMD(' ','DBKEPT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBKEPT','NPATH','Number of paths to be kept','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBKEPT','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBKEPT','CHOPT',0,0.,' ','D')
      GUID(  1)='Deletes all directories from the data ba'//
     +'se except the ones specified'
      GUID(  2)='by the user. The program will prompt for'//
     +' the path names. The user'
      GUID(  3)='should type complete path names includin'//
     +'g top directory name. If the'
      GUID(  4)='updates of this session are saved in jou'//
     +'rnal files, the user can'
      GUID(  5)='divert the updates in the special backup'//
     +' file using the character'
      GUID(  6)='option ''B''.'
      CALL KUGUID('DBKEPT',GUID,  6,'S')
      CALL KUACT('DBKEPT',DBAUXI)
 
      CALL KUNWG(  55)
      CALL KUCMD(' ','DBPEEK','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPEEK','PATHN','Pathname','C','S')
      CALL KUNDPV(   4,   1,   1,   0,   1)
      CALL KUPAR('DBPEEK','CFNAM','Display file name (.FILEXT)','C','S')
      CALL KUPVAL('DBPEEK','CFNAM',0,0.,'KFNAME.FILEXT','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPEEK','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBPEEK','CHOPT',0,0.,'H','D')
      GUID(  1)='Displays the User Keys in the directory '//
     +'specified,'
      GUID(  2)='in Horizontal (with CHOPT=''H'') or in V'//
     +'ertical (''V'') mode.'
      GUID(  3)='Displays data when the first column in m'//
     +'ode ''H'' or ''D'' in mode ''V'''
      GUID(  4)='is overwritten by ''*''.'
      CALL KUGUID('DBPEEK',GUID,  4,'S')
      CALL KUACT('DBPEEK',DBAUXI)
 
      CALL KUNWG( 398)
      CALL KUCMD(' ','DBPLNT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLNT','NPATH','Number of directories to be scanned',
     +'I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLNT','CHOPT','Character option for selecting object
     +','C','S')
      CALL KUPVAL('DBPLNT','CHOPT',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLNT','IDN','Numeric identifier of the N-tuple','I',
     +'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLNT','CTITL','Ntuple title','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLNT','CRZPA','RZ path for the N-tuple','CO','S')
      CALL KUPVAL('DBPLNT','CRZPA',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLNT','NPRIM','Primary allocation for the Ntuple','I
     +O','S')
      CALL KUPVAL('DBPLNT','NPRIM',1000,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLNT','LUNI','Logical unit number of auxiliary file'
     +,'IO','S')
      CALL KUPVAL('DBPLNT','LUNI',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLNT','CFNAM','Name of the auxiliary file','CO','S')
      CALL KUPVAL('DBPLNT','CFNAM',0,0.,' ','D')
      GUID(  1)='Prepares an N-tuple from the data elemen'//
     +'t(s) belonging to one or more'
      GUID(  2)='directories. The selection of the data o'//
     +'bjects is steered by CHOPT as'
      GUID(  3)='applied to the first directory. For subs'//
     +'equent directories, it tries'
      GUID(  4)='to match the time slot and selection on '//
     +'user keys is transmitted only'
      GUID(  5)='when the TAG of the appropriate user key'//
     +' in the first path matches'
      GUID(  6)='with the TAG of some user key in the cur'//
     +'rent directory. The following'
      GUID(  7)='assignments of CHOPT have been used.'
      GUID(  8)=' CHOPT = ''T''  transforms the Keys 3,4,'//
     +'7 to seconds/minutes passed since'
      GUID(  9)='              midnight January 1, 1980'
      GUID( 10)='       = ''3''  selects objects with sta'//
     +'rt validity time > KEYS(3)'
      GUID( 11)='       = ''4''  selects objects with sta'//
     +'rt validity time < KEYS(4)'
      GUID( 12)='       = ''5''  specific Program version'//
     +' number required'
      GUID( 13)='       = ''7''  selects objects with ins'//
     +'ertion      time < KEYS(7)'
      GUID( 14)='       = ''n''  consider user key n (whe'//
     +'re 7 < n < 30)'
      GUID( 15)='Creation of the Ntuple is guided by the '//
     +'parameters IDN, CTITL, CRZPA'
      GUID( 16)='and NPRIM. The user should first set the'//
     +' current directory to the'
      GUID( 17)='desired directory in memory by CDIR //pa'//
     +'wc/mydir. One should also'
      GUID( 18)='have a histgram RZ file opened and shoul'//
     +'d give appropriate CRZPA'
      GUID( 19)='to allow automatic overflow to that RZ f'//
     +'ile. The user is advised to'
      GUID( 20)='read HBOOK manual for more information o'//
     +'n Ntuples.'
      GUID( 21)='Detail on data and key elements to be st'//
     +'ored in the ntuples is by'
      GUID( 22)='default obtained from interactive enquir'//
     +'y. Optionally the user can'
      GUID( 23)='supply a file with contents of all these'//
     +' enquiries which are read'
      GUID( 24)='in free format from the program.'
      CALL KUGUID('DBPLNT',GUID, 24,'S')
      CALL KUACT('DBPLNT',DBAUXI)
 
      CALL KUNWG(  19)
      CALL KUCMD(' ','DBPRGD','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPRGD','PATHN','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPRGD','IKEEP','Number of partitions to be kept','IO'
     +,'S')
      CALL KUPVAL('DBPRGD','IKEEP',1,0.,' ','D')
      GUID(  1)='Deletes all but the last IKEEP partition'//
     +'s in a partitioned directory.'
      CALL KUGUID('DBPRGD',GUID,  1,'S')
      CALL KUACT('DBPRGD',DBAUXI)
 
      CALL KUNWG(  23)
      CALL KUCMD(' ','DBPTIM','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPTIM','IDATE','Date in YYMMDD','I','S')
      CALL KUPVAL('DBPTIM','IDATE',810101,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPTIM','ITIME','Time in HHMMSS','I','S')
      CALL KUPVAL('DBPTIM','ITIME',0,0.,' ','D')
      GUID(  1)='Packs date and time into one integer wor'//
     +'d'
      GUID(  2)='   CALL DBPKTS (IDATE, ITIME, IDATIM)'
      CALL KUGUID('DBPTIM',GUID,  2,'S')
      CALL KUACT('DBPTIM',DBAUXI)
 
      CALL KUNWG( 124)
      CALL KUCMD(' ','DBPURG','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPURG','PATHN','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPURG','KYDAT','Key element number','I','S')
      CALL KUPVAL('DBPURG','KYDAT',7,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPURG','KYTIM','Maximum value of the key','I','S')
      CALL KUPVAL('DBPURG','KYTIM',9999999,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPURG','CHOPT','Character option','C','S')
      CALL KUPVAL('DBPURG','CHOPT',0,0.,'K','D')
      GUID(  1)='Purges objects in the given directory ac'//
     +'cording to user specification.'
      GUID(  2)=' CHOPT = ''A'' : Purges all data objects'
      GUID(  3)='       = ''K'' : Purges data objects wit'//
     +'h KEY(kydat) < kytim'
      GUID(  4)='       = ''L'' : Purges all but the last'//
     +' one (one with highest KEY1 value)'
      GUID(  5)='       = ''P'' : Purges all data objects'//
     +' with the same validity period'
      GUID(  6)='               but the one with the high'//
     +'est KEY5 value'
      GUID(  7)='       = ''S'' : Purges all data objects'//
     +' with KEY1 value in the range'
      GUID(  8)='               kydat-kytim'
      CALL KUGUID('DBPURG',GUID,  8,'S')
      CALL KUACT('DBPURG',DBAUXI)
 
      CALL KUNWG(  14)
      CALL KUCMD(' ','DBRALI','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBRALI','ALIAS','Alias name','C','S')
      GUID(  1)='Retrieve the directory path name from th'//
     +'e alias name'
      CALL KUGUID('DBRALI',GUID,  1,'S')
      CALL KUACT('DBRALI',DBAUXI)
 
      CALL KUNWG(  16)
      CALL KUCMD(' ','DBRHLP','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBRHLP','PATHN','Pathname','C','S')
      CALL KUNDPV(   4,   1,   1,   0,   1)
      CALL KUPAR('DBRHLP','CFNAM','File name','C','S')
      CALL KUPVAL('DBRHLP','CFNAM',0,0.,'KFNAME.FILEXT','D')
      GUID(  1)='Display help information of a given dire'//
     +'ctory from data base'
      CALL KUGUID('DBRHLP',GUID,  1,'S')
      CALL KUACT('DBRHLP',DBAUXI)
 
      CALL KUNWG(  18)
      CALL KUCMD(' ','DBRENK','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBRENK','PATHN','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBRENK','KYI','Object number (Serial number)','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBRENK','KYEL','Key element to be changed','I','S')
      GUID(  1)='Rename a given key element of a given ob'//
     +'ject in the specified path'
      CALL KUGUID('DBRENK',GUID,  1,'S')
      CALL KUACT('DBRENK',DBAUXI)
 
      CALL KUNWG(  17)
      CALL KUCMD(' ','DBRNAM','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBRNAM','PATHN','Pathname','C','S')
      GUID(  1)='Read names of data elements of a given d'//
     +'irectory from data base'
      CALL KUGUID('DBRNAM',GUID,  1,'S')
      CALL KUACT('DBRNAM',DBAUXI)
 
      CALL KUNWG( 123)
      CALL KUCMD(' ','DBRTFZ','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBRTFZ','PATHN','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBRTFZ','LUNFZ','Logical unit number of the FZ file'
     +,'I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBRTFZ','K1MIN','Serial number of the first object','I
     +O','S')
      CALL KUPVAL('DBRTFZ','K1MIN',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBRTFZ','K1MAX','Serial number of the last  object','I
     +O','S')
      CALL KUPVAL('DBRTFZ','K1MAX',100,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBRTFZ','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBRTFZ','CHOPT',0,0.,' ','D')
      GUID(  1)='Copies a part of the directory specified'//
     +' by the path name with serial'
      GUID(  2)='number in the range K1MIN to K1MAX into '//
     +'a sequential file already'
      GUID(  3)='opened in output mode. The output is wri'//
     +'tten in the format of the'
      GUID(  4)='journal file, which can then be subseque'//
     +'ntly used for updating a copy'
      GUID(  5)='of the data base. With character option '//
     +'''F'', the update mode of the'
      GUID(  6)='compactification (when the journal file '//
     +'will later be used) will make'
      GUID(  7)='use data objects with matching user keys'//
     +'.'
      CALL KUGUID('DBRTFZ',GUID,  7,'S')
      CALL KUACT('DBRTFZ',DBAUXI)
 
      CALL KUNWG(  31)
      CALL KUCMD(' ','DBSHOW','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBSHOW','PATHN','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBSHOW','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBSHOW','CHOPT',0,0.,'K','D')
      GUID(  1)='Lists keys for all objects in the specif'//
     +'ied directory (CHOPT=''K'')'
      GUID(  2)='      keys and data for all objects (CHO'//
     +'PT=''D'')'
      CALL KUGUID('DBSHOW',GUID,  2,'S')
      CALL KUACT('DBSHOW',DBAUXI)
 
      CALL KUNWG(  23)
      CALL KUCMD(' ','DBUTIM','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBUTIM','IDATM','Date and time in one packed integer',
     +'I','S')
      GUID(  1)='Unpacks date and time from one integer w'//
     +'ord'
      GUID(  2)='   CALL DBUPTS (IDATE, ITIME, IDATIM)'
      CALL KUGUID('DBUTIM',GUID,  2,'S')
      CALL KUACT('DBUTIM',DBAUXI)
 
      CALL KUNWG(  32)
      CALL KUCMD(' ','DBVIEW','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBVIEW','PATHN','Pathname','C','S')
      CALL KUNDPV(   4,   1,   1,   0,   1)
      CALL KUPAR('DBVIEW','CFNAM','Display Filename (.FILEXT)','C','S')
      CALL KUPVAL('DBVIEW','CFNAM',0,0.,'DPNAME.FILEXT','D')
      GUID(  1)='Creates and displays a view a la ORACLE '//
     +'for valid data at a given time.'
      GUID(  2)='Different tables (pathnames) can also be'//
     +' joined.'
      CALL KUGUID('DBVIEW',GUID,  2,'S')
      CALL KUACT('DBVIEW',DBAUXI)
 
      CALL KUNWG( 211)
      CALL KUCMD(' ','DBWRITE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBWRITE','PATHN','Pathname','C','S')
      CALL KUNDPV(   4,   1,   1,   0,   1)
      CALL KUPAR('DBWRITE','CFNAM','Key-file name','C','S')
      CALL KUPVAL('DBWRITE','CFNAM',0,0.,'KFNAME.FILEXT','D')
      GUID(  1)='Creates Directories, inserts or updates '//
     +'ASCII data files.'
      GUID(  2)='There are two possible modes of operatio'//
     +'n for inserting'
      GUID(  3)='the key elements of the new object. In t'//
     +'he horizontal mode, where'
      GUID(  4)='all the key elements are displayed in a '//
     +'single line, one should'
      GUID(  5)='insert ''N'' in column 1 for every new o'//
     +'bject. To replace an existing'
      GUID(  6)='object (ala DBREPL), one has to put ''R'''//
     +' in front of the existing'
      GUID(  7)='object, followed by a new line with the '//
     +'keys of the new object with'
      GUID(  8)='''N'' in front. In the vertical mode, al'//
     +'l the keys are displayed in'
      GUID(  9)='seperate lines with one header line for '//
     +'every object. As in the'
      GUID( 10)='horizontal mode, one should insert chara'//
     +'cter ''N'' or ''R'' in the header'
      GUID( 11)='line in column 20 (where the letter ''D'''//
     +' appears in the display).'
      GUID( 12)='The data part is an ASCII file with a ma'//
     +'ximum of 80 characters per'
      GUID( 13)='line.'
      CALL KUGUID('DBWRITE',GUID, 13,'S')
      CALL KUACT('DBWRITE',DBAUXI)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
      SUBROUTINE INDBIC          
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL DBACTI
 
      CALL KUNWG(   0)
 
      CALL KUCMD('/DBL3',' ','SW')
 
      CALL KUNWG(   9)
      CALL KUCMD(' ','INITCLOSE','C')
      GUID(  1)='DBL3 Initializations or closing'
      CALL KUGUID('INITCLOSE',GUID,  1,'S')
 
      CALL KUCMD('INITCLOSE',' ','SW')
 
      CALL KUNWG(  26)
      CALL KUCMD(' ','DBCRDR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBCRDR','PATHN','Pathname','C','S')
      GUID(  1)='Creates Directories interactively. It as'//
     +'ks for the tags and types'
      GUID(  2)='of user keys interactively.'
      CALL KUGUID('DBCRDR',GUID,  2,'S')
      CALL KUACT('DBCRDR',DBACTI)
 
      CALL KUNWG(  15)
      CALL KUCMD(' ','DBDELT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBDELT','PATHN','Pathname','C','S')
      GUID(  1)='Delete a directory or a directory tree i'//
     +'nteractively.'
      CALL KUGUID('DBDELT',GUID,  1,'S')
      CALL KUACT('DBDELT',DBACTI)
 
      CALL KUNWG(  51)
      CALL KUCMD(' ','DBEFOR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBEFOR','TOPNM','Name of the top directory','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBEFOR','IDATE','Last date of insertion in YYMMDD','I'
     +,'S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBEFOR','ITIME','Last time of insertion in HHMM','I'
     +,'S')
      GUID(  1)='Sets a cutoff on the insertion time for '//
     +'a given RZ file. The RZ file'
      GUID(  2)='is identified by its top directory name.'//
     +' All subsequent DBUSE calls'
      GUID(  3)='will ignore data objects inserted at a l'//
     +'ater date/time.'
      CALL KUGUID('DBEFOR',GUID,  3,'S')
      CALL KUACT('DBEFOR',DBACTI)
 
      CALL KUNWG(  15)
      CALL KUCMD(' ','DBENFL','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBENFL','TOPNM','Top directory name','C','S')
      GUID(  1)='Close one DBL3 file specified by its top'//
     +' directory name.'
      CALL KUGUID('DBENFL',GUID,  1,'S')
      CALL KUACT('DBENFL',DBACTI)
 
      CALL KUNWG(   7)
      CALL KUCMD(' ','DBEND','C')
      GUID(  1)='Close all DBL3 files.'
      CALL KUGUID('DBEND',GUID,  1,'S')
      CALL KUACT('DBEND',DBACTI)
 
      CALL KUNWG(  64)
      CALL KUCMD(' ','DBFZOP','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBFZOP','TOPNM','Top directory name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBFZOP','LUNF','Logical unit number of the FZ file','I
     +','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBFZOP','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBFZOP','CHOPT',0,0.,' ','D')
      GUID(  1)='Associates a journal file for a given da'//
     +'ta base. The journal file'
      GUID(  2)='should be opened before hand using the c'//
     +'ommand FZFILE in write mode.'
      GUID(  3)=' CHOPT = '' '' : Stnadard journal file'
      GUID(  4)='       = ''B'' : Special backup file for'//
     +' objects entered with option B'
      CALL KUGUID('DBFZOP',GUID,  4,'S')
      CALL KUACT('DBFZOP',DBACTI)
 
      CALL KUNWG(  74)
      CALL KUCMD(' ','DBFZUP','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBFZUP','LUNF','Logical unit number of the FZ file','I
     +','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBFZUP','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBFZUP','CHOPT',0,0.,' ','D')
      GUID(  1)='Update a data base from a journal file s'//
     +'tored from a previous'
      GUID(  2)='session. The stored file should be opene'//
     +'d beforehand using the'
      GUID(  3)='command FZFILE in read mode.'
      GUID(  4)=' CHOPT = '' '' : Update keeping KEY(1) v'//
     +'alue unchanged'
      GUID(  5)='       = ''O'' : Update ignoring KEY(1) '//
     +'value which is recalculated'
      CALL KUGUID('DBFZUP',GUID,  5,'S')
      CALL KUACT('DBFZUP',DBACTI)
 
      CALL KUNWG(  62)
      CALL KUCMD(' ','DBILDU','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBILDU','LUNI','Logical unit number of the ASCII file'
     +,'I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBILDU','CFNAM','Name of the file containing the list'
     +,'C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBILDU','TOPNM','Name of the Top Directory','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBILDU','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBILDU','CHOPT',0,0.,' ','D')
      GUID(  1)='Opens an ASCII file containing the list '//
     +'of directories to be updated'
      GUID(  2)='from the journal file. Reads the directo'//
     +'ry names. The user can'
      GUID(  3)='supply the root names of the directories'//
     +' if all directories with that'
      GUID(  4)='root name are acceptable.'
      CALL KUGUID('DBILDU',GUID,  4,'S')
      CALL KUACT('DBILDU',DBACTI)
 
      CALL KUNWG(  47)
      CALL KUCMD(' ','DBINIT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBINIT','NREC','Flag if the file already exists or not
     +','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBINIT','LUN','Logical Unit Number for Data Base file'
     +,'I','S')
      CALL KUPVAL('DBINIT','LUN',1,0.,' ','D')
      CALL KUNDPV(   3,   1,   1,   0,   1)
      CALL KUPAR('DBINIT','FNAME','Name of the Data Base file','C','S')
      CALL KUPVAL('DBINIT','FNAME',0,0.,'DBTEST.DAT','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBINIT','LRECL','Record length in words','I','S')
      CALL KUPVAL('DBINIT','LRECL',1024,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBINIT','CHOPT','Character option for RZFILE/RZMAKE'
     +,'C','S')
      CALL KUPVAL('DBINIT','CHOPT',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBINIT','IDIV','DBL3 user Division Index','I','S')
      CALL KUPVAL('DBINIT','IDIV',2,0.,' ','D')
      CALL KUNDPV(   2,   1,   1,   0,   1)
      CALL KUPAR('DBINIT','TOPNM','Name of the Top Directory','C','S')
      CALL KUPVAL('DBINIT','TOPNM',0,0.,'D0STP','D')
      GUID(  1)='Open the DBL3 file and initialize DBL3 p'//
     +'ackage.'
      GUID(  2)='NREC should be set to zero if the file a'//
     +'lready exists.'
      GUID(  3)='Otherwise, NREC should contain maximum n'//
     +'umber of records in the file.'
      CALL KUGUID('DBINIT',GUID,  3,'S')
      CALL KUACT('DBINIT',DBACTI)
 
      CALL KUNWG(   8)
      CALL KUCMD(' ','DBLOGL','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBLOGL','LUN','Logical Unit Number for Data Base file'
     +,'I','S')
      CALL KUPVAL('DBLOGL','LUN',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBLOGL','LOGL','Log level','I','S')
      CALL KUPVAL('DBLOGL','LOGL',1,0.,' ','D')
      GUID(  1)='Set the log level for DBL3.'
      CALL KUGUID('DBLOGL',GUID,  1,'S')
      CALL KUACT('DBLOGL',DBACTI)
 
      CALL KUNWG(  44)
      CALL KUCMD(' ','DBNTOP','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBNTOP','PATHI','Input Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBNTOP','PATHN','Output Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBNTOP','MXKP','Maximum objects in a partition','IO'
     +,'S')
      CALL KUPVAL('DBNTOP','MXKP',50,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBNTOP','NSAV','Number of objects entered in 1 go','IO
     +','S')
      CALL KUPVAL('DBNTOP','NSAV',20,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBNTOP','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBNTOP','CHOPT',0,0.,' ','D')
      GUID(  1)='Make partiotioned directory from a norma'//
     +'l one.'
      GUID(  2)=' CHOPT = '' '' : Stnadard default for up'//
     +'dating'
      GUID(  3)='       = ''F'' : Updates with a fully ma'//
     +'tched data object (in user keys)'
      CALL KUGUID('DBNTOP',GUID,  3,'S')
      CALL KUACT('DBNTOP',DBACTI)
 
      CALL KUNWG(  42)
      CALL KUCMD(' ','DBOPEN','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBOPEN','NREC','Flag if the file already exists or not
     +','I','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBOPEN','LUN','Logical Unit Number for Data Base file'
     +,'I','S')
      CALL KUPVAL('DBOPEN','LUN',1,0.,' ','D')
      CALL KUNDPV(   3,   1,   1,   0,   1)
      CALL KUPAR('DBOPEN','FNAME','Name of the Data Base file','C','S')
      CALL KUPVAL('DBOPEN','FNAME',0,0.,'DBTEST.DAT','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBOPEN','LRECL','Record length in words','I','S')
      CALL KUPVAL('DBOPEN','LRECL',1000,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBOPEN','CHOPT','Character option for RZFILE/RZMAKE'
     +,'C','S')
      CALL KUPVAL('DBOPEN','CHOPT',0,0.,' ','D')
      GUID(  1)='Open a Direct Access file.'
      GUID(  2)='NREC should be set to zero if the file a'//
     +'lready exists.'
      GUID(  3)='Otherwise, NREC should contain maximum n'//
     +'umber of records in the file.'
      CALL KUGUID('DBOPEN',GUID,  3,'S')
      CALL KUACT('DBOPEN',DBACTI)
 
      CALL KUNWG(  40)
      CALL KUCMD(' ','DBSAVE','C')
      GUID(  1)='Saves the information on to the Random A'//
     +'ccess file through a call to'
      GUID(  2)='DBSAVE. This is useful only when the dat'//
     +'abase is opened with option'
      GUID(  3)='P on IBM.'
      CALL KUGUID('DBSAVE',GUID,  3,'S')
      CALL KUACT('DBSAVE',DBACTI)
 
      CALL KUNWG(  29)
      CALL KUCMD(' ','DBSETD','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBSETD','MXDIS','Horizontal Display range','I','S')
      CALL KUPVAL('DBSETD','MXDIS',80,0.,' ','D')
      GUID(  1)='Set the Horizontal Display range. At ini'//
     +'tialization the display'
      GUID(  2)='range is limited to 80 characters per li'//
     +'ne.'
      CALL KUGUID('DBSETD',GUID,  2,'S')
      CALL KUACT('DBSETD',DBACTI)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
      SUBROUTINE INDBIP          
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL INDBPL
 
      CALL KUNWG(   0)
 
      CALL KUCMD('/DBL3',' ','SW')
 
      CALL KUNWG(   9)
      CALL KUCMD(' ','PLOT','C')
      GUID(  1)='Plotting facilities inside DBL3'
      CALL KUGUID('PLOT',GUID,  1,'S')
 
      CALL KUCMD('PLOT',' ','SW')
 
      CALL KUNWG( 123)
      CALL KUCMD(' ','DBHELP','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBHELP','TOPN','Top directory name','CO','S')
      CALL KUPVAL('DBHELP','TOPN',0,0.,'*','D')
      CALL KUNDPV(   4,   1,   1,   0,   1)
      CALL KUPAR('DBHELP','CFNAM','File name','CO','S')
      CALL KUPVAL('DBHELP','CFNAM',0,0.,'KFNAME.FILEXT','D')
      GUID(  1)='Display help information of a given dire'//
     +'ctory from data base. This'
      GUID(  2)='command will show the tree of directory '//
     +'on the screen. The user is'
      GUID(  3)='supposed to pick the directory for whose'//
     +' the help information is'
      GUID(  4)='required by positioning the cursor on th'//
     +'e appropriate box and then'
      GUID(  5)='pressing the left mouse button (on Apoll'//
     +'o) or space bar on a standard'
      GUID(  6)='terminal. The help information if aviala'//
     +'ble will be put into a file'
      GUID(  7)='and displayed to the user through the st'//
     +'andard editor.'
      CALL KUGUID('DBHELP',GUID,  7,'S')
      CALL KUACT('DBHELP',INDBPL)
 
      CALL KUNWG( 262)
      CALL KUCMD(' ','DBPLOB','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOB','PATHN','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOB','NOBJ','Number of data elements to be plotted'
     +,'IO','S')
      CALL KUPVAL('DBPLOB','NOBJ',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOB','KEX','Key index for the abcissa','IO','S')
      CALL KUPVAL('DBPLOB','KEX',3,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOB','NST','Step size for selection of data object'
     +,'IO','S')
      CALL KUPVAL('DBPLOB','NST',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOB','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBPLOB','CHOPT',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOB','LUNI','Logical unit number of auxiliary file'
     +,'IO','S')
      CALL KUPVAL('DBPLOB','LUNI',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOB','CFNAM','Name of the auxiliary file','CO','S')
      CALL KUPVAL('DBPLOB','CFNAM',0,0.,' ','D')
      GUID(  1)='Plot data element(s) versus a key elemen'//
     +'t for a given directory.'
      GUID(  2)='CHOPT guides the selection of data objec'//
     +'ts in a directory, as well as'
      GUID(  3)='the mode of display of the data objects.'
      GUID(  4)=' CHOPT = ''P''  a symbol to be drawn at '//
     +'each point'
      GUID(  5)='              (Default is a line to be d'//
     +'rawn through the points)'
      GUID(  6)='       = ''L''  a line to be drawn throu'//
     +'gh the points'
      GUID(  7)='              (needed only when symbol a'//
     +'nd line both to be drawn)'
      GUID(  8)='       = ''S''  all elements shown on th'//
     +'e same plot'
      GUID(  9)='              (Default is a seperate plo'//
     +'t for each variable)'
      GUID( 10)='       = ''3''  selects objects with sta'//
     +'rt validity time > KEYS(3)'
      GUID( 11)='       = ''4''  selects objects with sta'//
     +'rt validity time < KEYS(4)'
      GUID( 12)='       = ''5''  specific Program version'//
     +' number required'
      GUID( 13)='       = ''7''  selects objects with ins'//
     +'ertion      time < KEYS(7)'
      GUID( 14)='       = ''n''  consider user key n (whe'//
     +'re 7 < n < 30)'
      GUID( 15)='The list of key and object elements to b'//
     +'e plotted and the specification'
      GUID( 16)='of the selection on key values can be su'//
     +'pplied through prompts or by'
      GUID( 17)='an auxiliary file.'
      CALL KUGUID('DBPLOB',GUID, 17,'S')
      CALL KUACT('DBPLOB',INDBPL)
 
      CALL KUNWG( 226)
      CALL KUCMD(' ','DBPLOT','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOT','ALIAS','Alias name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOT','CHTAG','Name of the data element to be plotte
     +d','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOT','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBPLOT','CHOPT',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOT','LUNI','Logical unit number of auxiliary file'
     +,'IO','S')
      CALL KUPVAL('DBPLOT','LUNI',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOT','CFNAM','Name of the auxiliary file','CO','S')
      CALL KUPVAL('DBPLOT','CFNAM',0,0.,' ','D')
      GUID(  1)='Plot the time evolution of a given data '//
     +'element from a given directory.'
      GUID(  2)='The directory is addressed by its alias '//
     +'name and the data element is'
      GUID(  3)='addressed by a tag uniquely identifying '//
     +'the data element inside the'
      GUID(  4)='directory. CHOPT guides the selection of'//
     +' data objects in a directory,'
      GUID(  5)='as well as the mode of display of the da'//
     +'ta objects.'
      GUID(  6)=' CHOPT = ''P''  a symbol to be drawn at '//
     +'each point'
      GUID(  7)='              (Default is a line to be d'//
     +'rawn through the points)'
      GUID(  8)='       = ''3''  selects objects with sta'//
     +'rt validity time > KEYS(3)'
      GUID(  9)='       = ''4''  selects objects with sta'//
     +'rt validity time < KEYS(4)'
      GUID( 10)='       = ''5''  specific Program version'//
     +' number required'
      GUID( 11)='       = ''7''  selects objects with ins'//
     +'ertion      time < KEYS(7)'
      GUID( 12)='       = ''n''  consider user key n (whe'//
     +'re 7 < n < 30)'
      GUID( 13)='The specification of the selection on ke'//
     +'y values can be supplied through'
      GUID( 14)='prompts or by an auxiliary file.'
      CALL KUGUID('DBPLOT',GUID, 14,'S')
      CALL KUACT('DBPLOT',INDBPL)
 
      CALL KUNWG( 225)
      CALL KUCMD(' ','DBPLOV','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOV','PATHN','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOV','NOBJ','Pair of elements to be plotted','IO'
     +,'S')
      CALL KUPVAL('DBPLOV','NOBJ',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOV','NST','Step size for selection of data object'
     +,'IO','S')
      CALL KUPVAL('DBPLOV','NST',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOV','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBPLOV','CHOPT',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOV','LUNI','Logical unit number of auxiliary file'
     +,'IO','S')
      CALL KUPVAL('DBPLOV','LUNI',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLOV','CFNAM','Name of the auxiliary file','CO','S')
      CALL KUPVAL('DBPLOV','CFNAM',0,0.,' ','D')
      GUID(  1)='Plot data element(s) versus data element'//
     +'(s) for a given directory.'
      GUID(  2)='CHOPT guides the selection of data objec'//
     +'ts in a directory, as well as'
      GUID(  3)='the mode of display of the data objects.'
      GUID(  4)=' CHOPT = ''L''  line to be drawn through'//
     +' the points'
      GUID(  5)='       = ''P''  a symbol to be drawn at '//
     +'each point'
      GUID(  6)='              (If L or P not chosen, a d'//
     +'efault marker to be drawn'
      GUID(  7)='               at each point)'
      GUID(  8)='       = ''3''  selects objects with sta'//
     +'rt validity time > KEYS(3)'
      GUID(  9)='       = ''4''  selects objects with sta'//
     +'rt validity time < KEYS(4)'
      GUID( 10)='       = ''5''  specific Program version'//
     +' number required'
      GUID( 11)='       = ''7''  selects objects with ins'//
     +'ertion      time < KEYS(7)'
      GUID( 12)='       = ''n''  consider user key n (whe'//
     +'re 7 < n < 30)'
      GUID( 13)='The list of pair of object elements to b'//
     +'e plotted and the specification'
      GUID( 14)='of the selection on key values can be su'//
     +'pplied through prompts or by'
      GUID( 15)='an auxiliary file.'
      CALL KUGUID('DBPLOV',GUID, 15,'S')
      CALL KUACT('DBPLOV',INDBPL)
 
      CALL KUNWG( 150)
      CALL KUCMD(' ','DBPLTI','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLTI','PATHN','Pathname','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLTI','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBPLTI','CHOPT',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLTI','LUNI','Logical unit number of auxiliary file'
     +,'IO','S')
      CALL KUPVAL('DBPLTI','LUNI',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBPLTI','CFNAM','Name of the auxiliary file','CO','S')
      CALL KUPVAL('DBPLTI','CFNAM',0,0.,' ','D')
      GUID(  1)='Plot validity time of data objects in a '//
     +'directory.'
      GUID(  2)='CHOPT guides the selection of data objec'//
     +'ts in a directory.'
      GUID(  3)=' CHOPT = ''3''  selects objects with sta'//
     +'rt validity time > KEYS(3)'
      GUID(  4)='       = ''4''  selects objects with sta'//
     +'rt validity time < KEYS(4)'
      GUID(  5)='       = ''5''  specific Program version'//
     +' number required'
      GUID(  6)='       = ''7''  selects objects with ins'//
     +'ertion      time < KEYS(7)'
      GUID(  7)='       = ''n''  consider user key n (whe'//
     +'re 7 < n < 30)'
      GUID(  8)='Detail is given in the DBL3 documentatio'//
     +'n.'
      GUID(  9)='The specification of the selection on ke'//
     +'y values can be supplied through'
      GUID( 10)='prompts or by an auxiliary file.'
      CALL KUGUID('DBPLTI',GUID, 10,'S')
      CALL KUACT('DBPLTI',INDBPL)
 
      CALL KUNWG( 127)
      CALL KUCMD(' ','DBREAD','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBREAD','TOPN','Top Directory name','CO','S')
      CALL KUPVAL('DBREAD','TOPN',0,0.,'*','D')
      CALL KUNDPV(   4,   1,   1,   0,   1)
      CALL KUPAR('DBREAD','CFNAM','Display file name (.FILEXT)','CO','S'
     +)
      CALL KUPVAL('DBREAD','CFNAM',0,0.,'KFNAME.FILEXT','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBREAD','CHOPT','Character option','CO','S')
      CALL KUPVAL('DBREAD','CHOPT',0,0.,'H','D')
      GUID(  1)='Displays the directory structure of the '//
     +'RZ-file as specified by'
      GUID(  2)='the user through the Top directory name '//
     +'(by default it uses the'
      GUID(  3)='first data base file opened). The user c'//
     +'an then pick the wanted'
      GUID(  4)='directory using the mouse button 1 by po'//
     +'sitioning on the last node.'
      GUID(  5)='Displays the User Keys in the directory '//
     +'specified on the screen,'
      GUID(  6)='in Horizontal (with CHOPT=''H'') or in V'//
     +'ertical (''V'') mode.'
      GUID(  7)='Displays data when the first column in m'//
     +'ode ''H'' or ''D'' in mode ''V'''
      GUID(  8)='is overwritten by ''*'''
      CALL KUGUID('DBREAD',GUID,  8,'S')
      CALL KUACT('DBREAD',INDBPL)
 
      CALL KUNWG(  21)
      CALL KUCMD(' ','DBTREE','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('DBTREE','TOPN','Top Directory name','CO','S')
      CALL KUPVAL('DBTREE','TOPN',0,0.,'*','D')
      GUID(  1)='Draw the tree of directories in the file'//
     +' given by the top directory'
      GUID(  2)='name.'
      CALL KUGUID('DBTREE',GUID,  2,'S')
      CALL KUACT('DBTREE',INDBPL)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
      SUBROUTINE INDBME          
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
 
      CALL KUNWG(   8)
      CALL KUCMD(' ','DBL3','C')
      GUID(  1)='DBL3 Interactive commands'
      CALL KUGUID('DBL3',GUID,  1,'S')
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
      SUBROUTINE INPAHI          
      INTEGER MGUIDL
      PARAMETER (MGUIDL=199)
      CHARACTER*80 GUID
      COMMON /KCGUID/ GUID(MGUIDL)
      EXTERNAL PAZRZ
 
      CALL KUNWG(   0)
 
      CALL KUCMD('/HISTOGRAM',' ','SW')
 
      CALL KUNWG(   9)
      CALL KUCMD(' ','ZEBRA','C')
      GUID(  1)='ZEBRA usage for HBOK4 package'
      CALL KUGUID('ZEBRA',GUID,  1,'S')
 
      CALL KUCMD('ZEBRA',' ','SW')
 
      CALL KUNWG(  23)
      CALL KUCMD(' ','MDIR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MDIR','CHDIR','Directory name','C','S')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MDIR','NWKEY','Number of words per Key','IO','S')
      CALL KUPVAL('MDIR','NWKEY',1,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('MDIR','CHFORM','CHFORM','CO','S')
      CALL KUPVAL('MDIR','CHFORM',0,0.,'I','D')
      CALL KUNDPV(   2,   1,   1,   0,   1)
      CALL KUPAR('MDIR','CHTAGS','List of Tags','CO','S')
      CALL KUPVAL('MDIR','CHTAGS',0,0.,'HBOOK-ID','D')
      GUID(  1)='To create a new RZ directory below the c'//
     +'urrent directory.'
      GUID(  2)='See ZEBRA guide RZMDIR.'
      CALL KUGUID('MDIR',GUID,  2,'S')
      CALL KUACT('MDIR',PAZRZ)
 
      CALL KUNWG(  66)
      CALL KUCMD(' ','LDIR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('LDIR','CHPATH','Path name','CO','S')
      CALL KUPVAL('LDIR','CHPATH',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   1,   1)
      CALL KUPAR('LDIR','CHOPT','Options','CO','S')
      CALL KUPVAL('LDIR','CHOPT',0,0.,' ','D')
      CALL KUPVAL('LDIR','CHOPT',0,0.,' ,A','V')
      GUID(  1)='List contents of a directory (memory or '//
     +'disk).'
      GUID(  2)='To list all RZ files currently opened, t'//
     +'ype ''LD //''.'
      GUID(  3)='Note that if the Current Directory is //'//
     +'PAWC, this command'
      GUID(  4)='uses the same format as HISTO/LIST.'
      GUID(  5)=' CHOPT=''A'' to list all the Ntuple exte'//
     +'nsions.'
      CALL KUGUID('LDIR',GUID,  5,'S')
      CALL KUACT('LDIR',PAZRZ)
 
      CALL KUNWG(  92)
      CALL KUCMD(' ','CDIR','C')
      CALL KUNDPV(   1,   1,   1,   0,   1)
      CALL KUPAR('CDIR','CHPATH','Path name','CO','S')
      CALL KUPVAL('CDIR','CHPATH',0,0.,' ','D')
      CALL KUNDPV(   1,   1,   1,   1,   1)
      CALL KUPAR('CDIR','CHOPT','CHOPT','CO','S')
      CALL KUPVAL('CDIR','CHOPT',0,0.,' ','D')
      CALL KUPVAL('CDIR','CHOPT',0,0.,' ,P','V')
      GUID(  1)='Change the current working directory (CW'//
     +'D).'
      GUID(  2)='IF CHPATH is given make it the new CWD.'
      GUID(  3)='Otherwise, print the pathname of the CWD'//
     +'.'
      GUID(  4)=' Ex.  CD dir1         ; make DIR1 the ne'//
     +'w CWD'
      GUID(  5)='      CD //file1/dir2 ; make //FILE1/DIR'//
     +'2 the new CWD (FILE1 must be'
      GUID(  6)='                        connected with F'//
     +'ILE)'
      GUID(  7)='      CD              ; print the name o'//
     +'f the CWD'
      CALL KUGUID('CDIR',GUID,  7,'S')
      CALL KUACT('CDIR',PAZRZ)
 
      CALL KUNWG(   0)
 
      CALL KUNDPV(   1,   1,   1,   0,   1)
 
      CALL KUCMD(' ',' ','E')
 
      CALL KUCMD('/',' ','SW')
 
      END
