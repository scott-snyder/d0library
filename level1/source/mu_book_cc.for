      SUBROUTINE MU_BOOK_CC(MULTBK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  31-JUL-1992   Kamel A. Bazizi
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MULTBK,IER

C----------------------------------------------------------------------
C
C- WAMUS Coarse Centroid distributions per module
      IF(MULTBK.EQ.2) THEN
        CALL HBOOK1(4010,'CC - MOD 10 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4011,'CC - MOD 11 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4012,'CC - MOD 12 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4013,'CC - MOD 13 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4015,'CC - MOD 15 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4016,'CC - MOD 16 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4020,'CC - MOD 20 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4021,'CC - MOD 21 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4022,'CC - MOD 22 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4023,'CC - MOD 23 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4025,'CC - MOD 25 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4026,'CC - MOD 26 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4030,'CC - MOD 30 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4031,'CC - MOD 31 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4032,'CC - MOD 32 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4033,'CC - MOD 33 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4035,'CC - MOD 35 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4036,'CC - MOD 36 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4061,'CC - MOD 61 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4062,'CC - MOD 62 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4064,'CC - MOD 64 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4065,'CC - MOD 65 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4067,'CC - MOD 67 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4091,'CC - MOD 91 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4092,'CC - MOD 92 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4094,'CC - MOD 94 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4095,'CC - MOD 95 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4097,'CC - MOD 97 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4100,'CC - MOD 100 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4101,'CC - MOD 101 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4102,'CC - MOD 102 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4103,'CC - MOD 103 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4104,'CC - MOD 104 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4105,'CC - MOD 105 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4106,'CC - MOD 106 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4107,'CC - MOD 107 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4110,'CC - MOD 110 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4111,'CC - MOD 111 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4112,'CC - MOD 112 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4113,'CC - MOD 113 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4114,'CC - MOD 114 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4115,'CC - MOD 115 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4116,'CC - MOD 116 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4117,'CC - MOD 117 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4120,'CC - MOD 120 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4121,'CC - MOD 121 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4122,'CC - MOD 122 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4123,'CC - MOD 123 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4124,'CC - MOD 124 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4127,'CC - MOD 127 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4130,'CC - MOD 130 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4131,'CC - MOD 131 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4132,'CC - MOD 132 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4133,'CC - MOD 133 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4134,'CC - MOD 134 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4135,'CC - MOD 135 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4136,'CC - MOD 136 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4137,'CC - MOD 137 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4140,'CC - MOD 140 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4141,'CC - MOD 141 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4142,'CC - MOD 142 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4143,'CC - MOD 143 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4144,'CC - MOD 144 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4145,'CC - MOD 145 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4146,'CC - MOD 146 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4147,'CC - MOD 147 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4150,'CC - MOD 150 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4153,'CC - MOD 153 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4160,'CC - MOD 160 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4161,'CC - MOD 161 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4162,'CC - MOD 162 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4163,'CC - MOD 163 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4164,'CC - MOD 164 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4165,'CC - MOD 165 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4166,'CC - MOD 166 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4167,'CC - MOD 167 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4180,'CC - MOD 180 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4183,'CC - MOD 183 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4190,'CC - MOD 190 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4191,'CC - MOD 191 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4192,'CC - MOD 192 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4193,'CC - MOD 193 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4194,'CC - MOD 194 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4195,'CC - MOD 195 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4196,'CC - MOD 196 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4197,'CC - MOD 197 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4200,'CC - MOD 200 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4201,'CC - MOD 201 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4202,'CC - MOD 202 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4203,'CC - MOD 203 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4204,'CC - MOD 204 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4205,'CC - MOD 205 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4206,'CC - MOD 206 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4207,'CC - MOD 207 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4210,'CC - MOD 210 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4211,'CC - MOD 211 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4212,'CC - MOD 212 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4213,'CC - MOD 213 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4214,'CC - MOD 214 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4215,'CC - MOD 215 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4216,'CC - MOD 216 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4217,'CC - MOD 217 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4220,'CC - MOD 220 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4221,'CC - MOD 221 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4222,'CC - MOD 222 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4223,'CC - MOD 223 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4224,'CC - MOD 224 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4227,'CC - MOD 227 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4230,'CC - MOD 230 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4231,'CC - MOD 231 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4232,'CC - MOD 232 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4233,'CC - MOD 233 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4234,'CC - MOD 234 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4235,'CC - MOD 235 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4236,'CC - MOD 236 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4237,'CC - MOD 237 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4240,'CC - MOD 240 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4241,'CC - MOD 241 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4242,'CC - MOD 242 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4243,'CC - MOD 243 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4244,'CC - MOD 244 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4245,'CC - MOD 245 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4246,'CC - MOD 246 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4247,'CC - MOD 247 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4250,'CC - MOD 250 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4251,'CC - MOD 251 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4253,'CC - MOD 253 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4255,'CC - MOD 255 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4260,'CC - MOD 260 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4261,'CC - MOD 261 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4262,'CC - MOD 262 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4263,'CC - MOD 263 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4264,'CC - MOD 264 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4265,'CC - MOD 265 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4266,'CC - MOD 266 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4267,'CC - MOD 267 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4270,'CC - MOD 270 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4271,'CC - MOD 271 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4272,'CC - MOD 272 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4273,'CC - MOD 273 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4274,'CC - MOD 274 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4275,'CC - MOD 275 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4276,'CC - MOD 276 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4277,'CC - MOD 277 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4280,'CC - MOD 280 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4281,'CC - MOD 281 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4283,'CC - MOD 283 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4285,'CC - MOD 285 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4290,'CC - MOD 290 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4291,'CC - MOD 291 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4292,'CC - MOD 292 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4293,'CC - MOD 293 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4294,'CC - MOD 294 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4295,'CC - MOD 295 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4296,'CC - MOD 296 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4297,'CC - MOD 297 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4300,'CC - MOD 300 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4301,'CC - MOD 301 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4302,'CC - MOD 302 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4303,'CC - MOD 303 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4304,'CC - MOD 304 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4305,'CC - MOD 305 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4306,'CC - MOD 306 / profile$',
     &    16,1.,17.,0.)
        CALL HBOOK1(4307,'CC - MOD 307 / profile$',
     &    16,1.,17.,0.)
      ENDIF

C----------------------------------------------------------------------
C- SAMUS Coarse Centroid distributions per module
      CALL HBOOK1(4404,'CC- SNAX (404,400) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4406,'CC- SNAY (406,402) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4405,'CC- SNAU (405,401) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4414,'CC- SNBX (414,410) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4416,'CC- SNBY (416,412) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4417,'CC- SNBU (417,413) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4424,'CC- SNCX (424,420) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4426,'CC- SNCY (426,422) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4425,'CC- SNCU (425,421) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4434,'CC- SSAX (434,430) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4436,'CC- SSAY (436,432) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4437,'CC- SSAU (437,433) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4444,'CC- SSBX (444,440) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4446,'CC- SSBY (446,442) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4445,'CC- SSBU (445,441) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4454,'CC- SSCX (454,450) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4456,'CC- SSCY (456,452) / profile$',
     &  32,0.,32.,0.)
      CALL HBOOK1(4457,'CC- SSCU (457,453) / profile$',
     &  32,0.,32.,0.)

C
C- WAMUS multiplicity distributions per module
      IF(MULTBK.EQ.2) THEN
        CALL HBOOK1(5010,'MULT, MOD 10$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5011,'MULT, MOD 11$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5012,'MULT, MOD 12$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5013,'MULT, MOD 13$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5015,'MULT, MOD 15$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5016,'MULT, MOD 16$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5020,'MULT, MOD 20$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5021,'MULT, MOD 21$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5022,'MULT, MOD 22$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5023,'MULT, MOD 23$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5025,'MULT, MOD 25$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5026,'MULT, MOD 26$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5030,'MULT, MOD 30$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5031,'MULT, MOD 31$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5032,'MULT, MOD 32$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5033,'MULT, MOD 33$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5035,'MULT, MOD 35$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5036,'MULT, MOD 36$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5061,'MULT, MOD 61$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5062,'MULT, MOD 62$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5064,'MULT, MOD 64$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5065,'MULT, MOD 65$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5067,'MULT, MOD 67$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5091,'MULT, MOD 91$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5092,'MULT, MOD 92$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5094,'MULT, MOD 94$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5095,'MULT, MOD 95$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5097,'MULT, MOD 97$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5100,'MULT, MOD 100$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5101,'MULT, MOD 101$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5102,'MULT, MOD 102$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5103,'MULT, MOD 103$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5104,'MULT, MOD 104$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5105,'MULT, MOD 105$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5106,'MULT, MOD 106$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5107,'MULT, MOD 107$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5110,'MULT, MOD 110$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5111,'MULT, MOD 111$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5112,'MULT, MOD 112$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5113,'MULT, MOD 113$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5114,'MULT, MOD 114$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5115,'MULT, MOD 115$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5116,'MULT, MOD 116$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5117,'MULT, MOD 117$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5120,'MULT, MOD 120$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5121,'MULT, MOD 121$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5122,'MULT, MOD 122$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5123,'MULT, MOD 123$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5124,'MULT, MOD 124$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5127,'MULT, MOD 127$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5130,'MULT, MOD 130$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5131,'MULT, MOD 131$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5132,'MULT, MOD 132$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5133,'MULT, MOD 133$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5134,'MULT, MOD 134$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5135,'MULT, MOD 135$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5136,'MULT, MOD 136$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5137,'MULT, MOD 137$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5140,'MULT, MOD 140$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5141,'MULT, MOD 141$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5142,'MULT, MOD 142$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5143,'MULT, MOD 143$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5144,'MULT, MOD 144$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5145,'MULT, MOD 145$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5146,'MULT, MOD 146$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5147,'MULT, MOD 147$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5150,'MULT, MOD 150$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5153,'MULT, MOD 153$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5160,'MULT, MOD 160$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5161,'MULT, MOD 161$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5162,'MULT, MOD 162$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5163,'MULT, MOD 163$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5164,'MULT, MOD 164$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5165,'MULT, MOD 165$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5166,'MULT, MOD 166$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5167,'MULT, MOD 167$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5180,'MULT, MOD 180$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5183,'MULT, MOD 183$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5190,'MULT, MOD 190$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5191,'MULT, MOD 191$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5192,'MULT, MOD 192$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5193,'MULT, MOD 193$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5194,'MULT, MOD 194$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5195,'MULT, MOD 195$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5196,'MULT, MOD 196$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5197,'MULT, MOD 197$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5200,'MULT, MOD 200$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5201,'MULT, MOD 201$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5202,'MULT, MOD 202$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5203,'MULT, MOD 203$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5204,'MULT, MOD 204$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5205,'MULT, MOD 205$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5206,'MULT, MOD 206$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5207,'MULT, MOD 207$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5210,'MULT, MOD 210$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5211,'MULT, MOD 211$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5212,'MULT, MOD 212$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5213,'MULT, MOD 213$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5214,'MULT, MOD 214$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5215,'MULT, MOD 215$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5216,'MULT, MOD 216$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5217,'MULT, MOD 217$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5220,'MULT, MOD 220$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5221,'MULT, MOD 221$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5222,'MULT, MOD 222$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5223,'MULT, MOD 223$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5224,'MULT, MOD 224$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5227,'MULT, MOD 227$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5230,'MULT, MOD 230$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5231,'MULT, MOD 231$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5232,'MULT, MOD 232$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5233,'MULT, MOD 233$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5234,'MULT, MOD 234$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5235,'MULT, MOD 235$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5236,'MULT, MOD 236$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5237,'MULT, MOD 237$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5240,'MULT, MOD 240$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5241,'MULT, MOD 241$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5242,'MULT, MOD 242$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5243,'MULT, MOD 243$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5244,'MULT, MOD 244$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5245,'MULT, MOD 245$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5246,'MULT, MOD 246$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5247,'MULT, MOD 247$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5250,'MULT, MOD 250$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5251,'MULT, MOD 251$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5253,'MULT, MOD 253$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5255,'MULT, MOD 255$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5260,'MULT, MOD 260$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5261,'MULT, MOD 261$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5262,'MULT, MOD 262$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5263,'MULT, MOD 263$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5264,'MULT, MOD 264$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5265,'MULT, MOD 265$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5266,'MULT, MOD 266$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5267,'MULT, MOD 267$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5270,'MULT, MOD 270$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5271,'MULT, MOD 271$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5272,'MULT, MOD 272$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5273,'MULT, MOD 273$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5274,'MULT, MOD 274$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5275,'MULT, MOD 275$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5276,'MULT, MOD 276$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5277,'MULT, MOD 277$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5280,'MULT, MOD 280$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5281,'MULT, MOD 281$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5283,'MULT, MOD 283$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5285,'MULT, MOD 285$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5290,'MULT, MOD 290$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5291,'MULT, MOD 291$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5292,'MULT, MOD 292$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5293,'MULT, MOD 293$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5294,'MULT, MOD 294$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5295,'MULT, MOD 295$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5296,'MULT, MOD 296$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5297,'MULT, MOD 297$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5300,'MULT, MOD 300$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5301,'MULT, MOD 301$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5302,'MULT, MOD 302$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5303,'MULT, MOD 303$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5304,'MULT, MOD 304$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5305,'MULT, MOD 305$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5306,'MULT, MOD 306$',
     &    17,0.,17.,0.)
        CALL HBOOK1(5307,'MULT, MOD 307$',
     &    17,0.,17.,0.)
      ENDIF
C
C- SAMUS multiplicity distributions per module
      CALL HBOOK1(5404,'CC- SNAX (404,400)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5406,'CC- SNAY (406,402)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5405,'CC- SNAU (405,401)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5414,'CC- SNBX (414,410)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5416,'CC- SNBY (416,412)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5417,'CC- SNBU (417,413)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5424,'CC- SNCX (424,420)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5426,'CC- SNCY (426,422)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5425,'CC- SNCU (425,421)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5434,'CC- SSAX (434,430)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5436,'CC- SSAY (436,432)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5437,'CC- SSAU (437,433)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5444,'CC- SSBX (444,440)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5446,'CC- SSBY (446,442)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5445,'CC- SSBU (445,441)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5454,'CC- SSCX (454,450)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5456,'CC- SSCY (456,452)$',
     &  32,0.,32.,0.)
      CALL HBOOK1(5457,'CC- SSCU (457,453)$',
     &  32,0.,32.,0.)

C.. Histograms for veto distributions
      CALL HBOOK1(1071,'SNA MULT - SWN TRIGGERS$',33,0.,33.,0.)
      CALL HBOOK1(1072,'SNB MULT - SWN TRIGGERS$',33,0.,33.,0.)
      CALL HBOOK1(1073,'SNB MULT - S+S+W SWN TRIGGERS$',
     &      33,0.,33.,0.)
      CALL HBOOK1(1074,'SSA MULT - SWS TRIGGERS$',33,0.,33.,0.)
      CALL HBOOK1(1075,'SSB MULT - SWS TRIGGERS$',33,0.,33.,0.)
      CALL HBOOK1(1076,'SSB MULT - S+S+W SWS TRIGGERS$',
     &      33,0.,33.,0.)

      CALL HBOOK1(1081,'SNA MULT - SN TRIGGERS$',33,0.,33.,0.)
      CALL HBOOK1(1082,'SNB MULT - SN TRIGGERS$',33,0.,33.,0.)
      CALL HBOOK1(1083,'SNC MULT - SN TRIGGERS$',33,0.,33.,0.)
      CALL HBOOK1(1084,'SSA MULT - SS TRIGGERS$',33,0.,33.,0.)
      CALL HBOOK1(1085,'SSB MULT - SS TRIGGERS$',33,0.,33.,0.)
      CALL HBOOK1(1086,'SSC MULT - SS TRIGGERS$',33,0.,33.,0.)

C----------------------------------------------------------------------
  999 RETURN
      END
