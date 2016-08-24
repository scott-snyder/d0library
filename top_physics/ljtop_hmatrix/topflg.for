      LOGICAL FUNCTION TOPFLG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : TOPFLG = .TRUE. if this event is a 2-jet
C-                                     event with t tbar
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  27-DEC-1990   P. GRANNIS
C-   Modified 30-JUL-1991   p. grannis
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INCLUDE 'D0$LINKS:IZISAE.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER IE,IR,NTOPS,IETOP(271),IRTOP(271)
      INTEGER NGLUE,IEGLUE(193),IRGLUE(193),I
C
      DATA IRTOP/
     * 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3,
     * 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
     * 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6,
     * 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
     * 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9,
     * 9, 9, 9, 9, 9, 9, 9, 9, 9,10,10,10,10,10,10,10,10,10,10,10,11,
     *11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,13,13,13,13,13,
     *13,13,13,13,13,14,14,14,14,14,14,14,14,14,14,14,14,15,15,15,15,
     *15,15,15,16,16,16,16,16,16,16,16,16,17,17,17,17,17,17,17,17,17,
     *18,18,18,18,18,18,18,18,18,19,19,19,19,19,19,19,20,20,20,20,20,
     *20,20,20,21,21,21,21,21,21,21,21,21,21,21,21,22,22,22,22,22,22,
     *22,22,22,22,22,22,22,23,23,23,23,23,23,23,23,23,23,23,24,24,24,
     *24,24,24,24,24,24,24,24,24,24,25,25,25,25,25,25,25,25,25/
C
      DATA IETOP/
     * 4244044, 6210825, 7744449,10468706,14853604, 3961920, 6036299,
     * 6356792, 7224158, 7784211, 7791723, 8930266, 9647425,12206959,
     *19914382, 2613786, 3104591, 4362808, 6613832, 6894450, 9251261,
     *10853230,11342241,13107460,13122001,13179256,16587170,  180762,
     *  268660, 2134351, 2855111, 3382118, 7117572, 7450834, 7966098,
     *10709378,11996866,14193351,14761480,15321652,15731622,18272250,
     *18553504,19677477,20044905,20054901, 1523296, 5607677, 5681023,
     * 7020963, 7675828, 7773545, 7926914,12085824,14701409,15060714,
     *16561046,17704540,19133905,  694841, 3860483, 5633198, 6051410,
     * 6978895, 7487092, 8939776,11298600,12372728,13456367,15866431,
     *17177459, 1094707, 1263831, 1405473, 3584482, 3925731, 5675720,
     * 5731049, 7884875, 9239239,11280952,14016802,14689533,14881765,
     *15843944,15987355,18589368,  141569, 2232469, 4340143, 5483225,
     * 8222622, 8783032, 8820584, 9631247,10319133,10342462,10908142,
     *13951666,14462077,15517612,17166282,17747521, 1193122, 3111507,
     * 7940591, 8275538,12348862,13661248,14136007,15414191,18839181,
     *19558852,19767914, 1251408, 2258111, 2419649, 2887790, 3967083,
     *11447547,11903119,16527644,17867892,18004387,19437941, 2006176,
     * 5216019, 7936624,15052202,17449157,18306203,18828486,19056773,
     *19982066,  133396, 1021434, 1422970, 4947308, 6221260, 7395212,
     * 8387665,20292845, 5295179, 6030821, 8720445,10853689,11741312,
     *13208390,13710390,14496776,17882602,19875045, 3972443, 5549696,
     * 5803942, 5905418, 5981527, 7751134,10403798,12140274,12820183,
     *17193844,18312453,19261483,  890593, 1322411, 1697412, 6144775,
     *12743104,13068487,15524355, 7036601,10309673,11014613,12754348,
     *14153148,14491285,14797256,15571239,16441762, 3170349, 3632503,
     * 7447946, 8777576, 9678541, 9947570,10848651,16032653,16743720,
     *  654794, 1695001, 2338482, 4490417, 7543783,11362919,14249567,
     *14536112,17470163, 6872273, 8318100,11396022,12612214,17608100,
     *19305702,20361040, 9712235,14021821,14784872,15130638,16193138,
     *18028153,18922613,19717301,  543888, 1153646, 7355621, 8103041,
     * 8988846, 9890251,10617179,13695893,15206221,15255479,16673501,
     *19611760,  279279,  708168, 2786077, 2859567, 3725587, 4245005,
     *10190362,13216280,14104794,14460441,16335875,16372508,17907570,
     *  611445, 2088765, 5174405, 6002271, 7703202, 7907908, 8626434,
     *12426879,17907196,18733718,19830840, 1419698, 2113093, 2171970,
     * 2215032, 7139601, 7351427, 9105169, 9395034,11598982,12563953,
     *13230671,14917669,17574840,  506818,  597393, 1771048, 3340252,
     * 9092300,12838376,15796595,18307942,19531830/
C
      DATA IRGLUE/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 
     * 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5,
     * 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 8, 8, 8, 8,
     * 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 9,10,10,10,10,10,
     *11,11,11,11,11,12,12,12,12,12,13,13,13,13,13,13,14,14,14,14,14,
     *14,14,14,14,15,15,15,15,15,15,16,16,16,16,16,16,16,16,17,17,17,
     *17,18,18,18,18,18,18,18,19,19,19,19,19,19,19,19,19,19,19,19,19,
     *20,20,20,20,20,20,20,20,20,20,21,21,21,21,21,21,21,21,21,21,22,
     *22,22,22,22,22,22,23,23,23,23,23,24,24,24,24,24,24,24,24,24,24,
     *24,25,25,25,25,25,25,25/
C
      DATA IEGLUE /2568266, 5114693, 5809832, 7885504, 8576358,
     * 9356022,14135530,15515895,15709305,17535230, 2275013, 2823992,
     * 3585088, 8093162, 9649255,16942907,17423554,17903037,19881627,
     * 2212630, 2411220, 3551445,10680928,13872030,15251755,16649769,
     *17559368, 2401251, 6252303, 8282248,11069597,15587177, 1598157,
     * 2585219, 5482047, 5866179, 7521309, 7765010, 8309792,16430928,
     *17651040,17927251,20267151, 3226233, 4687592, 5604432, 5985046,
     * 7557079,10095993,10149280,16365721,16480440, 4269162,14965700,
     *20050486,  223555, 2793395, 3242519, 5338540, 5715040, 5989758,
     *13026388,15865667,17345016,19889438,19949387,  807176, 1164503,
     * 3721413, 6578694, 8684088,10182017,11420163,15517376,16477654,
     * 2167588, 2605740,11945535,12170523,13217288, 4722450, 5038359,
     * 5360932, 7128466,15094802, 3741067, 8351869, 9328653, 9385282,
     *19781754, 3016828, 4561919, 6636821, 8890585,18926992,19144121,
     *  220566, 1416714, 1937406, 3540586, 3978155, 7411129,13011837,
     *15526094,18738410, 3020983, 6253326, 7330587,11737612,15007067,
     *18782136, 7617358,10985963,11362115,12255134,18908907,19077497,
     *20122729,20435740,   85322, 4876862, 9917390,16480981, 6189092,
     * 7385295, 7858604, 9698843,11686419,12734549,18581024,  138943,
     *  789432,  807556, 3426582, 4461614, 4804304, 6984284, 8583260,
     *13313166,14833462,15081442,16159090,17640869, 1431167, 2572174,
     * 5397014, 7672101,12100979,12836384,14546216,18181145,18635730,
     *19390138, 3191959, 4681530, 6280659, 7821235,13377380,15034709,
     *15913511,16974235,17577060,18537789,   28791, 5584326, 6680337,
     * 7376664,12738496,13006264,14135184, 3430929, 9595502,10015575,
     *11365874,19477518,  753325, 3377455, 3958990, 4281872, 5073507,
     * 8062827,10088907,12823746,17699382,17733750,18218394, 4256270,
     * 6357547,11672661,13705020,13736161,16132944,18874422 /
C
      PARAMETER (NTOPS=271)
      PARAMETER (NGLUE=193)
C
      TOPFLG=.FALSE.
C
      IE=IQ(LHEAD+7)                    ! event number
      IR=IQ(LHEAD+8)                    ! run number
C
      DO 100 I=1,NTOPS
        IF(IR.EQ.IRTOP(I)) GOTO 50
        GOTO 100
   50   IF(IE.NE.IETOP(I)) GOTO 100
        TOPFLG=.TRUE.        
        GOTO 999
  100   CONTINUE
C
      DO 200 I=1,NGLUE
        IF(IR.EQ.IRGLUE(I)) GOTO 150
        GOTO 200
  150   IF(IE.NE.IEGLUE(I)) GOTO 200
        TOPFLG=.TRUE.        
        GOTO 999
  200   CONTINUE
C
  999 RETURN
      END