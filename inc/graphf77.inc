C CDF Graphics DI-3000 parameters - D. Quarrie 15-JUN-1986
C+++++++
C       D0$INC:GraphF77.Inc - Graphics Parameter Definitions
C
C
C       DI3000 Basic Parameters
C       =======================
C                               Maximum Layer ID
      INTEGER    MXLAID
      PARAMETER (MXLAID =   255)
C                               Maximum Class ID
      INTEGER    MXCLID
      PARAMETER (MXCLID = 32000)
C                               Maximum Segment ID
      INTEGER    MXSGID
      PARAMETER (MXSGID = 32000)
C                               Maximum Segment Priority
      INTEGER    MXSGPR
      PARAMETER (MXSGPR = 32767)
C                               Maximum Pick Priority
      INTEGER    MXSGPK
      PARAMETER (MXSGPK = 32767)
C                               Maximum Pick ID
      INTEGER    MXPKID
      PARAMETER (MXPKID = 32767)
C                               Maximum Device ID
      INTEGER    MXDEVC
      PARAMETER (MXDEVC = 12)
C
C       Batch-Of-Updates Levels
C       =======================
C
C                               Software only
      INTEGER    BASOFT
      PARAMETER (BASOFT = 0)
C                               Fill
      INTEGER    BAFILL
      PARAMETER (BAFILL = 1)
C                               Hidden Surface
      INTEGER    BAHIDE
      PARAMETER (BAHIDE = 2)
C                               Fast
      INTEGER    BAFAST
      PARAMETER (BAFAST = 3)
C
C       Segment Attributes
C       ==================
C
      INTEGER    VISIBL
      PARAMETER (VISIBL = 1)
      INTEGER    INVISI
      PARAMETER (INVISI = 0)
      INTEGER    INACTV
      PARAMETER (INACTV = -1)
      INTEGER    HIGHLI
      PARAMETER (HIGHLI = 1)
      INTEGER    NOHIGH
      PARAMETER (NOHIGH = 0)
      INTEGER    DETECT
      PARAMETER (DETECT = 1)
      INTEGER    NODETC
      PARAMETER (NODETC = 0)
C
C       String Justification
C       ====================
C
      INTEGER    JUSLEF
      PARAMETER (JUSLEF = 1)
      INTEGER    JUSRIG
      PARAMETER (JUSRIG = 3)
      INTEGER    JUSBOT
      PARAMETER (JUSBOT = 1)
      INTEGER    JUSTOP
      PARAMETER (JUSTOP = 3)
      INTEGER    JUSCEN
      PARAMETER (JUSCEN = 2)
C
C       Character Path
C       ==============
C
      INTEGER    PATHRI
      PARAMETER (PATHRI = 1)
      INTEGER    PATHDO
      PARAMETER (PATHDO = 2)
      INTEGER    PATHLE
      PARAMETER (PATHLE = 3)
      INTEGER    PATHUP
      PARAMETER (PATHUP = 4)
C
C       Logical Input Devices
C       =====================
C
      INTEGER    BUTTON
      PARAMETER (BUTTON = 1)
      INTEGER    LOCATR
      PARAMETER (LOCATR = 2)
      INTEGER    VALUAT
      PARAMETER (VALUAT = 3)
      INTEGER    KEYBRD
      PARAMETER (KEYBRD = 4)
      INTEGER    PICK
      PARAMETER (PICK   = 5)
      INTEGER    STROKE
      PARAMETER (STROKE = 6)
C
C       Locator Echo Levels
C       ===================
C
C                               No Echo
      INTEGER    LONONE
      PARAMETER (LONONE = 0)
C                               Track Position
      INTEGER    LOTRAK
      PARAMETER (LOTRAK = 1)
C                               Rubberband
      INTEGER    LOBAND
      PARAMETER (LOBAND = 2)
C                               Horizontal Line
      INTEGER    LOHLIN
      PARAMETER (LOHLIN = 3)
C                               Vertical Line
      INTEGER    LOVLIN
      PARAMETER (LOVLIN = 4)
C                               Horizontal/Vertical Line
      INTEGER    LOHVLI
      PARAMETER (LOHVLI = 5)
C                               Rectangle
      INTEGER    LORECT
      PARAMETER (LORECT = 6)
C                               Drag Segment(s)
      INTEGER    LODRAG
      PARAMETER (LODRAG = 7)
C
C       Pick Echo Levels
C       ================
C
C                               No Echo
      INTEGER    PKNONE
      PARAMETER (PKNONE = 0)
C                               Primitive
      INTEGER    PKPRIM
      PARAMETER (PKPRIM = 1)
C                               Segment
      INTEGER    PKSEGM
      PARAMETER (PKSEGM = 2)
C                               Class
      INTEGER    PKCLAS
      PARAMETER (PKCLAS = 3)
C
C       Colors
C       ======
C
      INTEGER    COLNRM
      PARAMETER (COLNRM = 0)
      INTEGER    COLRED
      PARAMETER (COLRED = 1)
      INTEGER    COLGRE
      PARAMETER (COLGRE = 2)
      INTEGER    COLYEL
      PARAMETER (COLYEL = 3)
      INTEGER    COLBLU
      PARAMETER (COLBLU = 4)
      INTEGER    COLMAG
      PARAMETER (COLMAG = 5)
      INTEGER    COLCYA
      PARAMETER (COLCYA = 6)
      INTEGER    COLWHI
      PARAMETER (COLWHI = 7)
      INTEGER    COLBLA
      PARAMETER (COLBLA = 8)
      INTEGER    COLCMP
      PARAMETER (COLCMP = 9)
C
C       Polygon Interior Styles
C       =======================
C
      INTEGER    EMPTY
      PARAMETER (EMPTY  = 0)
      INTEGER    SOLID
      PARAMETER (SOLID  = 1)
C
C       Line Styles
C       ===========
C
      INTEGER    LSOLID
      PARAMETER (LSOLID = 0)
C
C       Markers
C       =======
C
C                               "."
      INTEGER    MKPOIN
      PARAMETER (MKPOIN = 1)
C                               "+"
      INTEGER    MKPLUS
      PARAMETER (MKPLUS = 2)
C                               "*"
      INTEGER    MKSTAR
      PARAMETER (MKSTAR = 3)
C                               "O"
      INTEGER    MKCIRC
      PARAMETER (MKCIRC = 4)
C                               "X"
      INTEGER    MKXCHA
      PARAMETER (MKXCHA = 5)
C
C       Text Fonts
C       ==========
C
C                               Hardware Generated Font
      INTEGER    HARDWR
      PARAMETER (HARDWR = 0 )
C                               Block Font Modifier
      INTEGER    BLOCK
      PARAMETER (BLOCK  = 0 )
C                               Italics Font Modifier
      INTEGER    ITALIC
      PARAMETER (ITALIC = 1 )
C                               Simplex
      INTEGER    SIMPLX
      PARAMETER (SIMPLX = 1 )
C                               Duplex
      INTEGER    DUPLEX
      PARAMETER (DUPLEX = 3 )
C                               Complex
      INTEGER    COMPLX
      PARAMETER (COMPLX = 5 )
C                               Triplex
      INTEGER    TRIPLX
      PARAMETER (TRIPLX = 7 )
C                               Greek
      INTEGER    GREEK
      PARAMETER (GREEK  = 9 )
C                               Script
      INTEGER    SCRIPT
      PARAMETER (SCRIPT = 11)
C
C       2*PI
C       ====
C
      REAL       GTWOPI
      PARAMETER (GTWOPI = 2.0*3.14159)
C
C       End GraphF77.Inc
C-------
