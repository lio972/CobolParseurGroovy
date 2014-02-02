         TITLE 'COORDONNEES CONDUCTEUR          '                       00001
         PRINT ON,NOGEN                                                 00002
FB02M0   DFHMSD TYPE=MAP,LANG=COBOL,MODE=INOUT,STORAGE=AUTO,SUFFIX=     00003
         TITLE 'COORDONNEES CONDUCTEUR          '                       00004
FB02M00  DFHMDI SIZE=(24,80),MAPATTS=(COLOR),COLUMN=1,LINE=1,          *00005
               DATA=FIELD,TIOAPFX=YES,OBFMT=NO                          00006
* XTRMTRAC                        ECR-XTRMTRAC                          00007
DFH0001  DFHMDF POS=(1,1),LENGTH=9,ATTRB=(PROT,NORM),COLOR=BLUE         00008
         DFHMDF POS=(1,11),LENGTH=1,ATTRB=(PROT,NORM)                   00009
* XAPPLIL                         ECR-XAPPLIL                           00010
DFH0002  DFHMDF POS=(1,26),LENGTH=30,ATTRB=(PROT,NORM),COLOR=BLUE       00011
         DFHMDF POS=(1,57),LENGTH=1,ATTRB=(PROT,NORM)                   00012
         DFHMDF POS=(1,65),LENGTH=6,INITIAL='date :',ATTRB=(PROT,NORM)  00013
* XJOURD                          ECR-XJOURD                            00014
DFH0003  DFHMDF POS=(1,72),LENGTH=8,INITIAL='........',ATTRB=(PROT,NORM*00015
               ),COLOR=BLUE                                             00016
* XRACFL                          ECR-XRACFL                            00017
DFH0004  DFHMDF POS=(2,1),LENGTH=15,INITIAL='...............',         *00018
               ATTRB=(PROT,NORM),COLOR=BLUE                             00019
         DFHMDF POS=(2,17),LENGTH=1,ATTRB=(PROT,NORM)                   00020
         DFHMDF POS=(2,30),LENGTH=22,INITIAL='COORDONNEES CONDUCTEUR', *00021
               ATTRB=(PROT,NORM)                                        00022
         DFHMDF POS=(2,64),LENGTH=7,INITIAL='heure :',ATTRB=(PROT,NORM) 00023
* XHEURED                         ECR-XHEURED                           00024
DFH0005  DFHMDF POS=(2,72),LENGTH=8,INITIAL='........',ATTRB=(PROT,NORM*00025
               ),COLOR=BLUE                                             00026
         DFHMDF POS=(3,1),LENGTH=79,                                   *00027
               INITIAL='-----------------------------------------------*00028
               --------------------------------',ATTRB=(PROT,NORM)      00029
         DFHMDF POS=(4,1),LENGTH=8,INITIAL='CLIENT :',ATTRB=(PROT,NORM) 00030
* GESCLI                          ECR-GESCLI                            00031
DFH0006  DFHMDF POS=(4,10),LENGTH=11,ATTRB=(PROT,NORM),COLOR=BLUE       00032
* RAIC                            ECR-RAIC                              00033
DFH0007  DFHMDF POS=(4,22),LENGTH=3,ATTRB=(PROT,NORM),COLOR=BLUE        00034
* NOMC                            ECR-NOMC                              00035
DFH0008  DFHMDF POS=(4,26),LENGTH=30,ATTRB=(PROT,NORM),COLOR=BLUE       00036
         DFHMDF POS=(4,57),LENGTH=1,ATTRB=(PROT,NORM)                   00037
         DFHMDF POS=(5,1),LENGTH=79,                                   *00038
               INITIAL='-----------------------------------------------*00039
               --------------------------------',ATTRB=(PROT,NORM)      00040
         DFHMDF POS=(6,1),LENGTH=10,INITIAL='IDCLIENT :',ATTRB=(PROT,NO*00041
               RM)                                                      00042
* PERNUMX                         ECR-PERNUMX                           00043
DFH0009  DFHMDF POS=(6,12),LENGTH=6,ATTRB=(UNPROT,NORM,IC),            *00044
               COLOR=NEUTRAL                                            00045
         DFHMDF POS=(6,19),LENGTH=9,INITIAL=' STATUT :',ATTRB=(ASKIP,NO*00046
               RM)                                                      00047
* PERSTAC                         ECR-PERSTAC                           00048
DFH0010  DFHMDF POS=(6,29),LENGTH=2,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00049
         DFHMDF POS=(6,32),LENGTH=33,                                  *00050
               INITIAL='   SI STATUT PM, NB DE SALARIES :',            *00051
               ATTRB=(ASKIP,NORM)                                       00052
* PERSALN                         ECR-PERSALN                           00053
DFH0011  DFHMDF POS=(6,66),LENGTH=3,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00054
         DFHMDF POS=(6,70),LENGTH=1,ATTRB=(ASKIP,NORM)                  00055
         DFHMDF POS=(7,1),LENGTH=7,INITIAL='TITRE :',ATTRB=(ASKIP,NORM) 00056
* PERTITL                         ECR-PERTITL                           00057
DFH0012  DFHMDF POS=(7,9),LENGTH=3,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL    00058
         DFHMDF POS=(7,13),LENGTH=8,INITIAL='   NOM :',ATTRB=(ASKIP,NOR*00059
               M)                                                       00060
* PERNOML                         ECR-PERNOML                           00061
DFH0013  DFHMDF POS=(7,22),LENGTH=20,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00062
         DFHMDF POS=(7,43),LENGTH=11,INITIAL='   PRENOM :',            *00063
               ATTRB=(ASKIP,NORM)                                       00064
* PERPREL                         ECR-PERPREL                           00065
DFH0014  DFHMDF POS=(7,55),LENGTH=20,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00066
         DFHMDF POS=(7,76),LENGTH=1,ATTRB=(ASKIP,NORM)                  00067
         DFHMDF POS=(8,1),LENGTH=27,                                   *00068
               INITIAL='DATE NAISSANCE (JJMMAAAA) :',ATTRB=(ASKIP,NORM) 00069
* PERNAID                         ECR-PERNAID                           00070
DFH0015  DFHMDF POS=(8,29),LENGTH=8,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00071
         DFHMDF POS=(8,38),LENGTH=1,ATTRB=(ASKIP,NORM)                  00072
         DFHMDF POS=(8,46),LENGTH=12,INITIAL='SEXE (M/F) :',           *00073
               ATTRB=(ASKIP,NORM)                                       00074
* PERSEXC                         ECR-PERSEXC                           00075
DFH0016  DFHMDF POS=(8,59),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00076
         DFHMDF POS=(8,61),LENGTH=1,ATTRB=(ASKIP,NORM)                  00077
         DFHMDF POS=(9,1),LENGTH=21,INITIAL='NB ENFANTS A CHARGE :',   *00078
               ATTRB=(ASKIP,NORM)                                       00079
* PERENCN                         ECR-PERENCN                           00080
DFH0017  DFHMDF POS=(9,23),LENGTH=2,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00081
         DFHMDF POS=(9,26),LENGTH=1,ATTRB=(ASKIP,NORM)                  00082
         DFHMDF POS=(9,46),LENGTH=30,                                  *00083
               INITIAL='SITUATION MATRIMONIALE (M/C) :',ATTRB=(ASKIP,NO*00084
               RM)                                                      00085
* PERMATC                         ECR-PERMATC                           00086
DFH0018  DFHMDF POS=(9,77),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00087
         DFHMDF POS=(9,79),LENGTH=1,ATTRB=(ASKIP,NORM)                  00088
         DFHMDF POS=(10,1),LENGTH=12,INITIAL='PROFESSION :',           *00089
               ATTRB=(ASKIP,NORM)                                       00090
* PERPROC                         ECR-PERPROC                           00091
DFH0019  DFHMDF POS=(10,14),LENGTH=2,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00092
         DFHMDF POS=(10,17),LENGTH=0,ATTRB=(ASKIP,NORM)                 00093
* PERPROL                         ECR-PERPROL                           00094
DFH0020  DFHMDF POS=(10,18),LENGTH=30,ATTRB=(UNPROT,NORM),             *00095
               COLOR=NEUTRAL                                            00096
         DFHMDF POS=(10,49),LENGTH=1,ATTRB=(ASKIP,NORM)                 00097
         DFHMDF POS=(12,1),LENGTH=39,                                  *00098
               INITIAL='PERMIS/BREVETS DATE OBTENTION(JJMMAAAA)',      *00099
               ATTRB=(ASKIP,NORM)                                       00100
* PRMTYPC1                        ECR-PRMTYPC1                          00101
DFH0021  DFHMDF POS=(13,6),LENGTH=3,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00102
         DFHMDF POS=(13,10),LENGTH=1,ATTRB=(ASKIP,NORM)                 00103
* PRMOBTD1                        ECR-PRMOBTD1                          00104
DFH0022  DFHMDF POS=(13,20),LENGTH=8,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00105
         DFHMDF POS=(13,29),LENGTH=1,ATTRB=(ASKIP,NORM)                 00106
* PRMTYPC2                        ECR-PRMTYPC2                          00107
DFH0023  DFHMDF POS=(14,6),LENGTH=3,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00108
         DFHMDF POS=(14,10),LENGTH=1,ATTRB=(ASKIP,NORM)                 00109
* PRMOBTD2                        ECR-PRMOBTD2                          00110
DFH0024  DFHMDF POS=(14,20),LENGTH=8,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00111
         DFHMDF POS=(14,29),LENGTH=34,                                 *00001120
               INITIAL='      CONDUITE ACCOMPAGNEE (O/N) :',           *00001130
               ATTRB=(ASKIP,NORM)                                       00001140
* PERCOAC                         ECR-PERCOAC                           00116
DFH0025  DFHMDF POS=(14,64),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001160
         DFHMDF POS=(14,66),LENGTH=1,ATTRB=(ASKIP,NORM)                 00001170
         DFHMDF POS=(16,1),LENGTH=12,INITIAL='ANTECEDENTS:',           *00001180
               ATTRB=(ASKIP,NORM)                                       00121
         DFHMDF POS=(17,1),LENGTH=44,                                  *00001200
               INITIAL='RELEVE D''INFORMATIONS SUR 36 DERNIERS MOIS :',*00001210
               ATTRB=(ASKIP,NORM)                                       00001220
* ANPANCN                         ECR-ANPANCN                           00125
DFH0026  DFHMDF POS=(17,46),LENGTH=2,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001240
         DFHMDF POS=(17,49),LENGTH=4,INITIAL='MOIS',ATTRB=(ASKIP,NORM)  00001250
         DFHMDF POS=(18,1),LENGTH=79,                                  *00001260
               INITIAL='SI ANNUL/SUSP PERMIS ALCOOL/STUPEFIANTS/DEL. FU*00001270
               ITE SUR 60 DERNIERS MOIS : REFUS',ATTRB=(ASKIP,NORM)     00001280
         DFHMDF POS=(19,1),LENGTH=64,                                  *00001290
               INITIAL='SINON, SI AUTRE MOTIF AVEC DUREE >30 JOURS SUR *00001300
               60 DERNIERS MOIS:',ATTRB=(ASKIP,NORM)                    00001310
* ANPINDC                         ECR-ANPINDC                           00132
DFH0027  DFHMDF POS=(19,66),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001330
         DFHMDF POS=(19,68),LENGTH=7,INITIAL='(A/S/N)',ATTRB=(ASKIP,NOR*00001340
               M)                                                       00001350
         DFHMDF POS=(20,1),LENGTH=15,INITIAL='LIBELLE MOTIF :',        *00001360
               ATTRB=(ASKIP,NORM)                                       00135
* ANPMOTL                         ECR-ANPMOTL                           00141
DFH0028  DFHMDF POS=(20,17),LENGTH=20,ATTRB=(UNPROT,NORM),             *00001390
               COLOR=NEUTRAL                                            00143
         DFHMDF POS=(20,38),LENGTH=1,ATTRB=(ASKIP,NORM)                 00001410
         DFHMDF POS=(20,55),LENGTH=10,INITIAL='NB JOURS :',            *00001420
               ATTRB=(ASKIP,NORM)                                       00001430
* ANPNBJN                         ECR-ANPNBJN                           00001440
DFH0029  DFHMDF POS=(20,66),LENGTH=3,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001450
         DFHMDF POS=(20,70),LENGTH=1,ATTRB=(ASKIP,NORM)                 00001460
         DFHMDF POS=(21,1),LENGTH=79,                                  *00145
               INITIAL='-----------------------------------------------*00146
               --------------------------------',ATTRB=(ASKIP,NORM)     00147
         DFHMDF POS=(22,1),LENGTH=7,INITIAL='Cmde =>',ATTRB=(ASKIP,BRT) 00148
* XCDEC                           ECR-XCDEC                             00149
DFH0030  DFHMDF POS=(22,9),LENGTH=9,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00150
         DFHMDF POS=(22,19),LENGTH=1,ATTRB=(ASKIP,NORM)                 00151
* XMSGIL                          ECR-XMSGIL                            00152
DFH0031  DFHMDF POS=(22,21),LENGTH=59,ATTRB=(ASKIP,BRT),COLOR=BLUE      00153
* XMSGAL                          ECR-XMSGAL                            00154
DFH0032  DFHMDF POS=(23,1),LENGTH=79,ATTRB=(PROT,BRT),COLOR=BLUE        00155
         DFHMDF POS=(24,1),LENGTH=1,ATTRB=(PROT,NORM)                   00156
         DFHMSD TYPE=FINAL                                              00157
         END                                                            00158
