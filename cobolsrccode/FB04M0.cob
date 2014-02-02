         TITLE 'CARACTERISTIQUES / ANTECEDENTS  '                       00001
         PRINT ON,NOGEN                                                 00002
FB04M0   DFHMSD TYPE=MAP,LANG=COBOL,MODE=INOUT,STORAGE=AUTO,SUFFIX=     00003
         TITLE 'CARACTERISTIQUES / ANTECEDENTS  '                       00004
FB04M00  DFHMDI SIZE=(24,80),MAPATTS=(COLOR),COLUMN=1,LINE=1,          *00005
               DATA=FIELD,TIOAPFX=YES,OBFMT=NO                          00006
* XTRMTRAC                        ECR-XTRMTRAC                          00007
DFH0001  DFHMDF POS=(1,1),LENGTH=9,ATTRB=(PROT,NORM),COLOR=BLUE         00008
         DFHMDF POS=(1,11),LENGTH=1,ATTRB=(PROT,NORM)                   00009
* XAPPLIL                         ECR-XAPPLIL                           00010
DFH0002  DFHMDF POS=(1,27),LENGTH=30,ATTRB=(PROT,NORM),COLOR=BLUE       00011
         DFHMDF POS=(1,58),LENGTH=1,ATTRB=(PROT,NORM)                   00012
         DFHMDF POS=(1,65),LENGTH=6,INITIAL='date :',ATTRB=(PROT,NORM),*00013
               COLOR=BLUE                                               00014
* XJOURD                          ECR-XJOURD                            00015
DFH0003  DFHMDF POS=(1,72),LENGTH=8,INITIAL='........',ATTRB=(PROT,NORM*00016
               ),COLOR=BLUE                                             00017
* XRACFL                          ECR-XRACFL                            00018
DFH0004  DFHMDF POS=(2,1),LENGTH=15,INITIAL='...............',         *00019
               ATTRB=(PROT,NORM),COLOR=BLUE                             00020
         DFHMDF POS=(2,17),LENGTH=1,ATTRB=(PROT,NORM)                   00021
         DFHMDF POS=(2,27),LENGTH=30,                                  *00022
               INITIAL='CARACTERISTIQUES / ANTECEDENTS',ATTRB=(PROT,NOR*00023
               M)                                                       00024
         DFHMDF POS=(2,64),LENGTH=7,INITIAL='heure :',ATTRB=(PROT,NORM)*00025
               ,COLOR=BLUE                                              00026
* XHEURED                         ECR-XHEURED                           00027
DFH0005  DFHMDF POS=(2,72),LENGTH=8,INITIAL='........',ATTRB=(PROT,NORM*00028
               ),COLOR=BLUE                                             00029
         DFHMDF POS=(3,1),LENGTH=79,                                   *00030
               INITIAL='-----------------------------------------------*00031
               --------------------------------',ATTRB=(PROT,NORM)      00032
         DFHMDF POS=(4,1),LENGTH=8,INITIAL='CLIENT :',ATTRB=(PROT,NORM) 00033
* GESCLI                          ECR-GESCLI                            00034
DFH0006  DFHMDF POS=(4,10),LENGTH=11,ATTRB=(PROT,NORM),COLOR=BLUE       00035
* RAIC                            ECR-RAIC                              00036
DFH0007  DFHMDF POS=(4,22),LENGTH=3,ATTRB=(PROT,NORM),COLOR=BLUE        00037
* NOMC                            ECR-NOMC                              00038
DFH0008  DFHMDF POS=(4,26),LENGTH=31,ATTRB=(PROT,NORM),COLOR=BLUE       00039
         DFHMDF POS=(4,58),LENGTH=1,ATTRB=(PROT,NORM)                   00040
         DFHMDF POS=(5,1),LENGTH=15,INITIAL='C.GRISE : NOM :',         *00000410
               ATTRB=(PROT,NORM)                                        00000420
* VEHCGNC                         ECR-VEHCGNC                           00000430
DFH0009  DFHMDF POS=(5,17),LENGTH=20,ATTRB=(UNPROT,NORM,IC),           *00000440
               COLOR=NEUTRAL                                            00000450
         DFHMDF POS=(5,38),LENGTH=8,INITIAL='PRENOM :',ATTRB=(ASKIP,NOR*00000460
               M)                                                       00000470
* VEHCGPC                         ECR-VEHCGPC                           00000480
DFH0010  DFHMDF POS=(5,47),LENGTH=20,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00000480
         DFHMDF POS=(5,68),LENGTH=8,INITIAL='STATUT :',ATTRB=(ASKIP,NOR*00000510
               M)                                                       00000520
* VEHCGSC                         ECR-VEHCGSC                           00000530
DFH0011  DFHMDF POS=(5,77),LENGTH=2,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00000520
         DFHMDF POS=(5,80),LENGTH=0,ATTRB=(ASKIP,NORM)                  00000560
         DFHMDF POS=(6,1),LENGTH=17,INITIAL='CODE VEHICULE   :',       *00044
               ATTRB=(ASKIP,NORM)                                       00000580
* VEHCODC                         ECR-VEHCODC                           00046
DFH0012  DFHMDF POS=(6,19),LENGTH=7,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00000580
         DFHMDF POS=(6,27),LENGTH=33,                                  *00049
               INITIAL='(9999999 si non classé)    TYPE :',            *00050
               ATTRB=(ASKIP,NORM)                                       00051
* VEHTYPC                         ECR-VEHTYPC                           00052
DFH0013  DFHMDF POS=(6,61),LENGTH=3,ATTRB=(ASKIP,BRT),COLOR=BLUE        00000660
         DFHMDF POS=(6,65),LENGTH=9,INITIAL='  GENRE :',ATTRB=(PROT,NOR*00054
               M)                                                       00055
* VEHGENC                         ECR-VEHGENC                           00056
DFH0014  DFHMDF POS=(6,75),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00000700
         DFHMDF POS=(6,77),LENGTH=1,ATTRB=(ASKIP,NORM)                  00058
         DFHMDF POS=(7,1),LENGTH=6,INITIAL='MARQUE',ATTRB=(ASKIP,NORM)  00059
         DFHMDF POS=(7,17),LENGTH=1,INITIAL=':',ATTRB=(ASKIP,NORM)      00060
* VEHMARL                         ECR-VEHMARL                           00061
DFH0015  DFHMDF POS=(7,19),LENGTH=15,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00000750
         DFHMDF POS=(7,35),LENGTH=11,INITIAL='  MODELE  :',            *00063
               ATTRB=(ASKIP,NORM)                                       00064
* VEHMODL                         ECR-VEHMODL                           00065
DFH0016  DFHMDF POS=(7,47),LENGTH=30,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00000790
         DFHMDF POS=(7,78),LENGTH=1,ATTRB=(ASKIP,NORM)                  00067
         DFHMDF POS=(8,1),LENGTH=17,INITIAL='PUISSANCE/CYL   :',       *00068
               ATTRB=(ASKIP,NORM)                                       00069
* VEHCYLN                         ECR-VEHCYLN                           00070
DFH0017  DFHMDF POS=(8,19),LENGTH=5,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00000840
         DFHMDF POS=(8,25),LENGTH=21,INITIAL='(en cv/cm3) USAGE   :',  *00072
               ATTRB=(ASKIP,NORM)                                       00073
* VEHUSAC                         ECR-VEHUSAC                           00074
DFH0018  DFHMDF POS=(8,47),LENGTH=3,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00000880
         DFHMDF POS=(8,51),LENGTH=1,ATTRB=(ASKIP,NORM)                  00076
         DFHMDF POS=(9,1),LENGTH=17,INITIAL='IMMATRICULATION :',       *00077
               ATTRB=(ASKIP,NORM)                                       00078
* VEHIMMX                         ECR-VEHIMMX                           00079
DFH0019  DFHMDF POS=(9,19),LENGTH=10,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00000930
         DFHMDF POS=(9,30),LENGTH=16,INITIAL=' DATE 1ère MEC :',       *00000810
               ATTRB=(ASKIP,NORM)                                       00084
* VEHCIRD                         ECR-VEHCIRD                           00085
DFH0020  DFHMDF POS=(9,47),LENGTH=6,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00000970
         DFHMDF POS=(9,54),LENGTH=19,INITIAL=' DATE ACQUISITION :',    *00000850
               ATTRB=(ASKIP,NORM)                                       00000860
* VEHACQD                         ECR-VEHACQD                           00000870
DFH0021  DFHMDF POS=(9,74),LENGTH=6,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00001010
         DFHMDF POS=(10,1),LENGTH=17,INITIAL='VALEUR A NEUF   :',      *00088
               ATTRB=(ASKIP,NORM)                                       00089
* VEHVALM                         ECR-VEHVALM                           00090
DFH0022  DFHMDF POS=(10,19),LENGTH=7,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001050
         DFHMDF POS=(10,27),LENGTH=39,                                 *00000920
               INITIAL='(si non classé ou si 4R classé X) GPE :',      *00000930
               ATTRB=(ASKIP,NORM)                                       00094
* VEHGROC                         ECR-VEHGROC                           00000950
DFH0023  DFHMDF POS=(10,67),LENGTH=2,ATTRB=(ASKIP,BRT),COLOR=BLUE       00001100
         DFHMDF POS=(10,70),LENGTH=7,INITIAL='  CLA :',ATTRB=(PROT,NORM*00000970
               )                                                        00000980
* VEHCLAC                         ECR-VEHCLAC                           00000990
DFH0024  DFHMDF POS=(10,78),LENGTH=1,ATTRB=(PROT,BRT),COLOR=BLUE        00001140
         DFHMDF POS=(10,80),LENGTH=0,ATTRB=(PROT,NORM)                  00001010
         DFHMDF POS=(11,1),LENGTH=17,INITIAL='PROTECTION VOL  :',      *00095
               ATTRB=(PROT,NORM)                                        00001030
* VEHPRTC                         ECR-VEHPRTC                           00097
DFH0025  DFHMDF POS=(11,19),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001190
         DFHMDF POS=(11,21),LENGTH=1,ATTRB=(ASKIP,NORM)                 00099
         DFHMDF POS=(11,37),LENGTH=35,                                 *00100
               INITIAL='BOX ou GARAGE INDIVIDUEL CLOS     :',          *00101
               ATTRB=(ASKIP,NORM)                                       00102
* GARCODC                         ECR-GARCODC                           00103
DFH0026  DFHMDF POS=(11,73),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001250
         DFHMDF POS=(11,75),LENGTH=5,INITIAL='(O/N)',ATTRB=(ASKIP,NORM) 00105
         DFHMDF POS=(12,1),LENGTH=18,INITIAL='ACQUIS EN LOA/LLD:',     *00106
               ATTRB=(ASKIP,NORM)                                       00107
* VEHPOSC                         ECR-VEHPOSC                           00108
DFH0027  DFHMDF POS=(12,20),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001300
         DFHMDF POS=(12,22),LENGTH=50,                                 *00110
               INITIAL='(O/N)PAR CONTRAT INCLUANT GIE PERTES FINANCIERE*00111
               S :',ATTRB=(ASKIP,NORM)                                  00112
* VEHPEFC                         ECR-VEHPEFC                           00113
DFH0028  DFHMDF POS=(12,73),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001350
         DFHMDF POS=(12,75),LENGTH=5,INITIAL='(O/N)',ATTRB=(ASKIP,NORM) 00115
         DFHMDF POS=(13,1),LENGTH=17,INITIAL='GARAGE HABITUEL :',      *00116
               ATTRB=(ASKIP,NORM)                                       00117
* GARCOPC                         ECR-GARCOPC                           00118
DFH0029  DFHMDF POS=(13,19),LENGTH=5,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001400
         DFHMDF POS=(13,25),LENGTH=21,INITIAL='(C. postal) COMMUNE :', *00120
               ATTRB=(ASKIP,NORM)                                       00121
* GARVILL                         ECR-GARVILL                           00122
DFH0030  DFHMDF POS=(13,47),LENGTH=30,ATTRB=(UNPROT,NORM),             *00001440
               COLOR=NEUTRAL                                            00124
         DFHMDF POS=(13,78),LENGTH=1,ATTRB=(ASKIP,NORM)                 00125
         DFHMDF POS=(14,1),LENGTH=52,                                  *00126
               INITIAL='VEHICULE SUPPLEMENTAIRE ACQUIS PAR LE FOYER (O/*00127
               N)  :',ATTRB=(ASKIP,NORM)                                00128
* ANVREPC                         ECR-ANVREPC                           00129
DFH0031  DFHMDF POS=(14,54),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001510
         DFHMDF POS=(14,56),LENGTH=15,INITIAL='      FORMULE :',       *00001380
               ATTRB=(ASKIP,NORM)                                       00001320
* VEHFORC                         ECR-VEHFORC                           00134
DFH0032  DFHMDF POS=(14,72),LENGTH=2,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001550
         DFHMDF POS=(14,75),LENGTH=1,ATTRB=(ASKIP,NORM)                 00001420
         DFHMDF POS=(15,1),LENGTH=45,                                  *00001430
               INITIAL='PRECEDENT CONTRAT D''ASSURANCES    N°CONTRAT :'*00001440
               ,ATTRB=(ASKIP,NORM)                                      00001450
* ANVNUMX                         ECR-ANVNUMX                           00142
DFH0033  DFHMDF POS=(15,47),LENGTH=15,ATTRB=(UNPROT,NORM),             *00001610
               COLOR=NEUTRAL                                            00144
         DFHMDF POS=(15,63),LENGTH=8,INITIAL='SOUSCR :',ATTRB=(ASKIP,NO*00001490
               RM)                                                      00001530
* ANVSOUS                         ECR-ANVSOUS                           00001540
DFH0034  DFHMDF POS=(15,72),LENGTH=4,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001660
* ANVSOUN                         ECR-ANVSOUN                           00001530
DFH0035  DFHMDF POS=(15,77),LENGTH=3,ATTRB=(ASKIP,NORM),COLOR=RED       00001680
         DFHMDF POS=(16,1),LENGTH=14,INITIAL='CODE COMPAGNIE',         *00001450
               ATTRB=(ASKIP,NORM)                                       00147
         DFHMDF POS=(16,21),LENGTH=1,INITIAL=':',ATTRB=(ASKIP,NORM)     00001470
* ANVCIEX                         ECR-ANVCIEX                           00149
DFH0036  DFHMDF POS=(16,23),LENGTH=3,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001730
         DFHMDF POS=(16,27),LENGTH=1,ATTRB=(ASKIP,NORM)                 00001500
         DFHMDF POS=(16,35),LENGTH=5,INITIAL='NOM :',ATTRB=(ASKIP,NORM) 00001610
* ANVCIEL                         ECR-ANVCIEL                           00153
DFH0037  DFHMDF POS=(16,41),LENGTH=35,ATTRB=(UNPROT,NORM),             *00001770
               COLOR=NEUTRAL                                            00155
         DFHMDF POS=(16,77),LENGTH=1,ATTRB=(ASKIP,NORM)                 00001650
         DFHMDF POS=(17,1),LENGTH=21,INITIAL='RELEVE D''INFOS SUR  :', *00001560
               ATTRB=(ASKIP,NORM)                                       00158
* ANVANCN                         ECR-ANVANCN                           00159
DFH0038  DFHMDF POS=(17,23),LENGTH=2,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001830
         DFHMDF POS=(17,26),LENGTH=44,                                 *00001700
               INITIAL='MOIS     INTERRUPTION SUR 12 DERNIERS MOIS :', *00001710
               ATTRB=(ASKIP,NORM)                                       00164
* ANVINTN                         ECR-ANVINTN                           00165
DFH0039  DFHMDF POS=(17,71),LENGTH=2,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001880
         DFHMDF POS=(17,74),LENGTH=4,INITIAL='MOIS',ATTRB=(ASKIP,NORM)  00001750
         DFHMDF POS=(18,1),LENGTH=21,INITIAL='DATE RESIL (MMAAAA) :',  *00001670
               ATTRB=(ASKIP,NORM)                                       00169
* ANVRESD                         ECR-ANVRESD                           00170
DFH0040  DFHMDF POS=(18,23),LENGTH=6,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001930
         DFHMDF POS=(18,30),LENGTH=40,                                 *00001800
               INITIAL='     SINISTRES SUR 36 DERNIERS MOIS    :',     *00001810
               ATTRB=(ASKIP,NORM)                                       00175
* SIVINDC                         ECR-SIVINDC                           00176
DFH0041  DFHMDF POS=(18,71),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00001980
         DFHMDF POS=(18,73),LENGTH=5,INITIAL='(O/N)',ATTRB=(ASKIP,NORM) 00001850
         DFHMDF POS=(19,1),LENGTH=69,                                  *00001860
               INITIAL='Si RESILIATION ASSUREUR, MOTIF DE RESIL(0 : Non*00001870
                Paiement/1 : Autre) :',ATTRB=(ASKIP,NORM)               00001880
* ANVMTRC                         ECR-ANVMTRC                           00001810
DFH0042  DFHMDF POS=(19,71),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00002040
         DFHMDF POS=(19,73),LENGTH=1,ATTRB=(ASKIP,NORM)                 00001910
         DFHMDF POS=(20,1),LENGTH=13,INITIAL='DERNIER CRM :',          *00179
               ATTRB=(ASKIP,NORM)                                       00180
* ANVBONT                         ECR-ANVBONT                           00181
DFH0043  DFHMDF POS=(20,15),LENGTH=3,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00002090
         DFHMDF POS=(20,19),LENGTH=11,INITIAL='ACQUIS EN :',           *00001960
               ATTRB=(ASKIP,NORM)                                       00184
* ANVBOND                         ECR-ANVBOND                           00185
DFH0044  DFHMDF POS=(20,31),LENGTH=6,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00002130
         DFHMDF POS=(20,38),LENGTH=32,                                 *00002000
               INITIAL='SI CRM 50, ANNEE D''ACQUISITION :',            *00002010
               ATTRB=(ASKIP,NORM)                                       00189
* ANVABOD                         ECR-ANVABOD                           00190
DFH0045  DFHMDF POS=(20,71),LENGTH=4,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00002180
         DFHMDF POS=(20,76),LENGTH=1,ATTRB=(ASKIP,NORM)                 00002050
         DFHMDF POS=(21,1),LENGTH=79,                                  *00193
               INITIAL='-----------------------------------------------*00194
               --------------------------------',ATTRB=(ASKIP,NORM)     00195
         DFHMDF POS=(22,1),LENGTH=7,INITIAL='Cmde =>',ATTRB=(ASKIP,NORM*00196
               )                                                        00197
* XCDEC                           ECR-XCDEC                             00198
DFH0046  DFHMDF POS=(22,9),LENGTH=9,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00002260
         DFHMDF POS=(22,19),LENGTH=1,ATTRB=(ASKIP,NORM)                 00200
* XMSGIL                          ECR-XMSGIL                            00201
DFH0047  DFHMDF POS=(22,21),LENGTH=59,ATTRB=(ASKIP,BRT),COLOR=BLUE      00002290
* XMSGAL                          ECR-XMSGAL                            00203
DFH0048  DFHMDF POS=(23,1),LENGTH=79,ATTRB=(PROT,BRT),COLOR=BLUE        00002310
         DFHMDF POS=(24,1),LENGTH=1,ATTRB=(PROT,NORM)                   00205
         DFHMSD TYPE=FINAL                                              00206
         END                                                            00207
