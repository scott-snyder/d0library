      SUBROUTINE BKCAPH(LADDR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book a CAPH bank.
C-
C-   Inputs  : LADDR    [I]     Address of new CAPH bank
C-
C-   Outputs : None
C-
C-   Created   2-Feb-1989   Z. Wolf
C-   Updated  13-SEP-1990   Harrison B. Prosper   
C-      Now a linear chain
C-   Updated  18-SEP-1990   K. Wyatt Merritt  
C-      Moved creation of link area to routine CAPH_LINK 
C-   Updated  20-NOV-1991   Boaz Klima  
C-      Reduce the number of words to 15 ( no spare words for NN ) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LADDR
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:LKCAPH.INC'
      INCLUDE 'D0$LINKS:IZCAPH.LINK'
      INCLUDE 'D0$LINKS:IZHSTR.LINK'
      INCLUDE 'D0$PARAMS:CAPH.DEF'
C----------------------------------------------------------------------
C
C ****  Bank parameters
C
      INTEGER ND
      PARAMETER( ND = 15 )              ! Number of data words
      INTEGER NL
      PARAMETER( NL = 5  )              ! Number of links
      INTEGER NS
      PARAMETER( NS = 3  )              ! Number of structural links
      INTEGER BANK_VERSION
      PARAMETER( BANK_VERSION = 4  )    ! Bank version number
C
      INTEGER LSUP,I,J,K,IOFF,IVAL
      INTEGER IOCAPH
      INTEGER GZHSTR,GZPROC,LZLAST,JBANK
      REAL VAL
C----------------------------------------------------------------------
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      SAVE FIRST,IOCAPH
C----------------------------------------------------------------------
C
C ****  Create permanent link area and format block
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        ICAPH = 0
        CALL CAPH_LINK
        CALL MZFORM ('CAPH','5I 15F',IOCAPH)
      ENDIF
C
      LADDR = 0
C
C ****  FIND LINK TO SUPPORTING PROC BANK
C
      JPROC = GZPROC()
      IF ( JPROC.LE.0 ) THEN
        CALL BKPROC(JPROC)
      END IF
      IF ( JPROC.LE.0 ) THEN
        CALL ERRMSG('CALORIMETER','BKCAPH',
     &      'CANNOT ESTABLISH LPROC','F')
        GOTO 999
      ENDIF
C
      CALL MZBOOK(IXMAIN,JCAPH,JPROC,-IZCAPH,'CAPH',NL,NS,ND,IOCAPH,0)
      IF ( JCAPH .LE. 0 ) GOTO 999
C
      IQ(JCAPH+1)      = BANK_VERSION   ! version number
      LQ(JCAPH+R_HSTR) = GZHSTR()       ! pointer to history bank
      LCAPHR = LQ(JCAPH+R_HSTR)         ! pointer to history bank
      LADDR            = JCAPH          ! Return bank address
 999  RETURN
C
      ENTRY CAPHFL_INT (IOFF,IVAL)
C----------------------------------------------------------------------
C-   Purpose and Methods : Set value in currently selected CAPH bank.
C----------------------------------------------------------------------
      IF ( JCAPH .GT. 0 ) THEN
        IQ(JCAPH+IOFF) = IVAL
      ENDIF
 1999 RETURN
C        
      ENTRY CAPHFL_REAL (IOFF,VAL)
C----------------------------------------------------------------------
C-   Purpose and Methods : Set value in currently selected CAPH bank.
C----------------------------------------------------------------------
      IF ( JCAPH .GT. 0 ) THEN
        Q(JCAPH+IOFF) = VAL
      ENDIF
 2999 RETURN
      END
