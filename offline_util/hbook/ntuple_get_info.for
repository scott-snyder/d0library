      SUBROUTINE NTUPLE_GET_INFO(IDD,MAXDIM,TITLE,RZPATH,
     &                           NENTRY,NDIM,TAG,XMIN,XMAX,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return info for given ntuple.
C-   Uses some code from HBOOK4.
C-
C-   Inputs  : IDD      [I]   Ntuple ID
C-             MAXDIM   [I]   Maximum number of dimensions of ntuple
C-             
C-   Outputs : TITLE    [C*]  Title of ntuple
C-             RZPATH   [C*]  RZ-path in which ntuple resides
C-             NENTRY   [I]   Number of entries
C-             NDIM     [I]   Dimension of ntuple
C-             TAG(*)   [C*8] NDIM Tags
C-             XMIN(*)  [R]   NDIM Minimum value of field
C-             XMAX(*)  [R]   NDIM Maximum value of field
C-             STATUS   [I]   0 - Ok
C-   Controls:
C-
C-   Created  11-JAN-1992   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDD, MAXDIM
      CHARACTER*(*) TITLE, RZPATH
      INTEGER NENTRY,NDIM
      CHARACTER*(*) TAG(*)
      REAL    XMIN(*), XMAX(*)
      INTEGER STATUS
C----------------------------------------------------------------------
C
C-         ============> N-TUPLE  DATA STRUCTURE <============
C-
C-                   I
C-                   I
C-                   I LCID
C-    -3   -2   -1   V
C-  ************************************************************************
C-  *    *    *    ****** Bits,NDIM,NOENT,etc  (See A)                     *
C-  ************************************************************************
C-    .     I     I   LNLAST  pointer to last linear bank
C-    ......I.....I.......................................
C-          I     I                                      .
C-          I     I LCONT                                .
C-          I     I                                      V
C-          I     I       ****************************   ********************
C-          I     I------>* (x1,y1,z1,,),(x2,y2,z2,,)*-->*    next if no RZ *
C-          I             ****************************   ********************
C-          I LLIMS
C-          I
C-          I        *****************************************
C-          I------->*  xlow,xup,ylow,yup,zlow,zup,etc       *
C-                   *****************************************
C-
C-
C-    **********************************************************************
C-    * word *  tag    *               content                             *
C-    **********************************************************************
C-    *      *         *                                                   *
C-    *   1  *  BITS   *     status word                                   *
C-    *   2  *  NDIM   *     Number of variables in the N-Tuple            *
C-    *   3  *  NOENT  *     Total number of entries                       *
C-    *   4  *  NPRIME *     Number of words for primary allocation        *
C-    *   5  *  NMEM   *     Number of memory blocks (only 1 with RZ)      *
C-    *   6  *  NRZB   *     Number of RZ records (If RZ is used)          *
C-    *   7  *  IFIRST *     pointer to 1st free word in LCONT             *
C-    *   8  *  NWTITL *     Number of words in the title                  *
C- (A)*   9  *  ITIT1  *     Internal pointer to the first word of title   *
C-    *  10  *  ITAG1  *     pointer to the first tag                      *
C-    *  11  *  NCHRZ  *     Number of characters in RZ dir (may be 0)     *
C-    *  12  *  CHRZ1  *     1st word of RZ directory                      *
C-    *  13  *  CHRZ2  *     2nd word,etc                                  *
C-    *      *   ...   *                                                   *
C-    * ITAG1*  TAG1   *     1st tag (8 characters on 2 words)             *
C-    *      *   ...   *                                                   *
C-    * ITIT1*  TITLE1 *     1st word of title                             *
C-    *      *   ...   *                                                   *
C-    **********************************************************************
C-
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PAWC.INC'
C----------------------------------------------------------------------
      INTEGER LCIDN, LLIMS, NCHT, ITIT1, ITAG1, NCHRZ, I
      INTEGER GZHIDN
C----------------------------------------------------------------------
      STATUS = 0
C
C ****  Get address of ntuple bank.
C
      LCIDN = GZHIDN(IDD)
      IF ( LCIDN .LE. 0 ) THEN
        STATUS = -1
        GOTO 999
      ENDIF
C
C ****  Get dimension etc.
C
      NDIM    = IP(LCIDN+2)
      NENTRY  = IP(LCIDN+3)
      NCHT    = IP(LCIDN+8)*4
      ITIT1   = IP(LCIDN+9)
      ITAG1   = IP(LCIDN+10)
      NCHRZ   = IP(LCIDN+11)
C
C ****  Get RZ path
C
      RZPATH = ' '
      IF ( NCHRZ.NE.0 ) THEN
        CALL DHTOC(NCHRZ,IP(LCIDN+12),RZPATH)
      ENDIF
C
C ****  Get title
C
      TITLE = ' '
      IF ( NCHT.NE.0 ) THEN
        CALL DHTOC(NCHT,IP(LCIDN+ITIT1),TITLE)
      ENDIF
C
C ****  Get tags
C
      DO I=1,NDIM
        TAG(I) = ' '
        CALL DHTOC(8,IP(LCIDN+ITAG1+2*I-2),TAG(I))
        XMIN(I) = 1.0
        XMAX(I) =-1.0
      ENDDO
C
C ****  Get max and min values
C
      LLIMS = LP(LCIDN-2)
      IF ( LLIMS .LE. 0 ) THEN
        GOTO 999
      ENDIF
      DO I=1,NDIM
        XMIN(I) = P(LLIMS+2*I-1)
        XMAX(I) = P(LLIMS+2*I  )
      ENDDO
C
  999 RETURN
      END
