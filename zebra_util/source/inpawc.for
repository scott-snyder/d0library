      SUBROUTINE INPAWC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-       Initialize PAWC common for HBOOK4
C-
C-   Created  27-MAR-1989   Serban D. Protopopescu
C-   Updated  17-APR-1991   Rajendran Raja  Updated to have Global sections
C-   Updated   3-MAR-1992   K. Wyatt Merritt  Put global sections in machine
C-                           block for unix compatibility 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PAWC.INC'
      INTEGER NPAGES,HCREATEG
      INTEGER TRNLNM,STATUS,LENG
      INTEGER TRULEN
C----------------------------------------------------------------------
C
      STATUS = TRNLNM('GLOBAL$PAWC',GNAME,LENG)
      IF ( GNAME(1:LENG).EQ.'GLOBAL$PAWC' ) THEN          ! NO GLOBAL SECTION
        CALL HLIMIT(-PAGELEN*MAXPAGES)
      ELSE
C&IF VAXVMS
        NPAGES = HCREATEG(GNAME(1:LENG),HMEMOR,PAGELEN*MAXPAGES)
        IF ( NPAGES.GT.0 ) THEN
          CALL ERRMSG(' INPAWC ','INPAWC',
     &    ' GLOBAL SECTION: '//GNAME(1:LENG)//' CREATED ','S')
          CALL HLIMIT(-PAGELEN*MAXPAGES)
        ELSE
          CALL ERRMSG(' INPAWC ','INPAWC ',
     &    ' GLOBAL SECTION ERROR ','S')
        ENDIF
C&ELSE
C&        CALL HLIMIT(-PAGELEN*MAXPAGES)
C&        CALL ERRMSG(' INPAWC ','INPAWC ',
C&     &    'Global section not created on non-VMS system','S')
C&ENDIF
      ENDIF
C
  999 RETURN
      END
