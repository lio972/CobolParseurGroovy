         TITLE 'COMPOSITION CONTRAT XXXXXXXXXXX '                       00001
         PRINT ON,NOGEN                                                 00002
FB01M0   DFHMSD TYPE=MAP,LANG=COBOL,MODE=INOUT,STORAGE=AUTO,SUFFIX=     00003
         TITLE 'COMPOSITION CONTRAT 4 ROUES GFA '                       00004
FB01M00  DFHMDI SIZE=(24,80),MAPATTS=(COLOR),COLUMN=1,LINE=1,          *00005
               DATA=FIELD,TIOAPFX=YES,OBFMT=NO                          00006
* XTRMTRAC                        ECR-XTRMTRAC                          00007
DFH0001  DFHMDF POS=(1,1),LENGTH=9,INITIAL='.........',ATTRB=(PROT,NORM*00008
               )                                                        00009
         DFHMDF POS=(1,11),LENGTH=1,ATTRB=(PROT,NORM)                   00010
* XAPPLIL                         ECR-XAPPLIL                           00011
DFH0002  DFHMDF POS=(1,26),LENGTH=30,ATTRB=(PROT,NORM)                  00012
         DFHMDF POS=(1,57),LENGTH=1,ATTRB=(PROT,NORM)                   00013
         DFHMDF POS=(1,64),LENGTH=6,INITIAL='date :',ATTRB=(PROT,NORM)  00014
* XJOURD                          ECR-XJOURD                            00015
DFH0003  DFHMDF POS=(1,71),LENGTH=8,INITIAL='........',ATTRB=(PROT,NORM*00016
               )                                                        00017
         DFHMDF POS=(1,80),LENGTH=0,ATTRB=(PROT,NORM)                   00018
* XRACFL                          ECR-XRACFL                            00019
DFH0004  DFHMDF POS=(2,1),LENGTH=15,INITIAL='...............',         *00020
               ATTRB=(PROT,NORM)                                        00021
         DFHMDF POS=(2,17),LENGTH=1,ATTRB=(PROT,NORM)                   00022
         DFHMDF POS=(2,30),LENGTH=11,INITIAL='COMPOSITION',ATTRB=(PROT,*00023
               NORM)                                                    00024
         DFHMDF POS=(2,42),LENGTH=10,INITIAL='DU CONTRAT',ATTRB=(PROT,N*00025
               ORM)                                                     00026
         DFHMDF POS=(2,63),LENGTH=7,INITIAL='heure :',ATTRB=(PROT,NORM) 00027
* XHEURED                         ECR-XHEURED                           00028
DFH0005  DFHMDF POS=(2,71),LENGTH=8,INITIAL='........',ATTRB=(PROT,NORM*00029
               )                                                        00030
         DFHMDF POS=(2,80),LENGTH=0,ATTRB=(PROT,NORM)                   00031
         DFHMDF POS=(3,1),LENGTH=78,                                   *00032
               INITIAL='-----------------------------------------------*00033
               -------------------------------',ATTRB=(PROT,BRT),      *00034
               COLOR=BLUE                                               00035
         DFHMDF POS=(4,1),LENGTH=6,INITIAL='CLIENT',ATTRB=(PROT,NORM)   00036
         DFHMDF POS=(4,8),LENGTH=1,INITIAL=':',ATTRB=(PROT,NORM)        00037
* GESCLI                          ECR-GESCLI                            00038
DFH0006  DFHMDF POS=(4,10),LENGTH=11,ATTRB=(PROT,NORM)                  00039
* RAIC                            ECR-RAIC                              00040
DFH0007  DFHMDF POS=(4,22),LENGTH=3,ATTRB=(PROT,NORM)                   00041
* NOMC                            ECR-NOMC                              00042
DFH0008  DFHMDF POS=(4,26),LENGTH=30,ATTRB=(PROT,NORM)                  00043
         DFHMDF POS=(4,57),LENGTH=1,ATTRB=(PROT,NORM)                   00044
         DFHMDF POS=(5,1),LENGTH=78,                                   *00045
               INITIAL='-----------------------------------------------*00046
               -------------------------------',ATTRB=(PROT,BRT),      *00047
               COLOR=BLUE                                               00048
         DFHMDF POS=(5,80),LENGTH=3,INITIAL='SEL',ATTRB=(PROT,NORM)     00049
         DFHMDF POS=(6,13),LENGTH=11,INITIAL='IDENTIFIANT',ATTRB=(PROT,*00050
               NORM)                                                    00051
         DFHMDF POS=(6,36),LENGTH=41,                                  *00052
               INITIAL='ENTREE    SORTIE  MOTIF  STATUT    POINTS',    *00053
               ATTRB=(PROT,NORM)                                        00054
         DFHMDF POS=(7,61),LENGTH=16,INITIAL='ou TYPE   ou CRM',       *00055
               ATTRB=(PROT,NORM)                                        00056
* CSECODC                         ECR-CSECODC                           00057
DFH0009  DFHMDF POS=(8,1),LENGTH=1,ATTRB=(UNPROT,NORM,IC),             *00058
               COLOR=NEUTRAL                                            00059
* IDENTIF                         ECR-IDENTIF                           00060
DFH0010  DFHMDF POS=(8,3),LENGTH=30,ATTRB=(ASKIP,NORM)                  00061
         DFHMDF POS=(8,34),LENGTH=0,ATTRB=(PROT,NORM)                   00062
* DATENTD                         ECR-DATENTD                           00063
DFH0011  DFHMDF POS=(8,35),LENGTH=8,ATTRB=(PROT,NORM)                   00064
         DFHMDF POS=(8,44),LENGTH=0,ATTRB=(PROT,NORM)                   00065
* DATSORD                         ECR-DATSORD                           00066
DFH0012  DFHMDF POS=(8,45),LENGTH=8,ATTRB=(PROT,NORM)                   00067
         DFHMDF POS=(8,54),LENGTH=1,ATTRB=(PROT,NORM)                   00068
* MOTIFSC                         ECR-MOTIFSC                           00069
DFH0013  DFHMDF POS=(8,56),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00070
         DFHMDF POS=(8,58),LENGTH=1,ATTRB=(ASKIP,NORM)                  00071
* STATYPC                         ECR-STATYPC                           00072
DFH0014  DFHMDF POS=(8,63),LENGTH=4,ATTRB=(ASKIP,NORM)                  00073
         DFHMDF POS=(8,68),LENGTH=1,ATTRB=(PROT,NORM)                   00074
* PTSCRMX                         ECR-PTSCRMX                           00075
DFH0015  DFHMDF POS=(8,73),LENGTH=3,ATTRB=(PROT,NORM)                   00076
         DFHMDF POS=(8,77),LENGTH=1,ATTRB=(PROT,NORM)                   00077
* CSECODC                         ECR-CSECODC                           00078
DFH0016  DFHMDF POS=(9,1),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL    00079
* IDENTIF                         ECR-IDENTIF                           00080
DFH0017  DFHMDF POS=(9,3),LENGTH=30,ATTRB=(ASKIP,NORM)                  00081
         DFHMDF POS=(9,34),LENGTH=0,ATTRB=(PROT,NORM)                   00082
* DATENTD                         ECR-DATENTD                           00083
DFH0018  DFHMDF POS=(9,35),LENGTH=8,ATTRB=(PROT,NORM)                   00084
         DFHMDF POS=(9,44),LENGTH=0,ATTRB=(PROT,NORM)                   00085
* DATSORD                         ECR-DATSORD                           00086
DFH0019  DFHMDF POS=(9,45),LENGTH=8,ATTRB=(PROT,NORM)                   00087
         DFHMDF POS=(9,54),LENGTH=1,ATTRB=(PROT,NORM)                   00088
* MOTIFSC                         ECR-MOTIFSC                           00089
DFH0020  DFHMDF POS=(9,56),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00090
         DFHMDF POS=(9,58),LENGTH=1,ATTRB=(ASKIP,NORM)                  00091
* STATYPC                         ECR-STATYPC                           00092
DFH0021  DFHMDF POS=(9,63),LENGTH=4,ATTRB=(ASKIP,NORM)                  00093
         DFHMDF POS=(9,68),LENGTH=1,ATTRB=(PROT,NORM)                   00094
* PTSCRMX                         ECR-PTSCRMX                           00095
DFH0022  DFHMDF POS=(9,73),LENGTH=3,ATTRB=(PROT,NORM)                   00096
         DFHMDF POS=(9,77),LENGTH=1,ATTRB=(PROT,NORM)                   00097
* CSECODC                         ECR-CSECODC                           00098
DFH0023  DFHMDF POS=(10,1),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00099
* IDENTIF                         ECR-IDENTIF                           00100
DFH0024  DFHMDF POS=(10,3),LENGTH=30,ATTRB=(ASKIP,NORM)                 00101
         DFHMDF POS=(10,34),LENGTH=0,ATTRB=(PROT,NORM)                  00102
* DATENTD                         ECR-DATENTD                           00103
DFH0025  DFHMDF POS=(10,35),LENGTH=8,ATTRB=(PROT,NORM)                  00104
         DFHMDF POS=(10,44),LENGTH=0,ATTRB=(PROT,NORM)                  00105
* DATSORD                         ECR-DATSORD                           00106
DFH0026  DFHMDF POS=(10,45),LENGTH=8,ATTRB=(PROT,NORM)                  00107
         DFHMDF POS=(10,54),LENGTH=1,ATTRB=(PROT,NORM)                  00108
* MOTIFSC                         ECR-MOTIFSC                           00109
DFH0027  DFHMDF POS=(10,56),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00110
         DFHMDF POS=(10,58),LENGTH=1,ATTRB=(ASKIP,NORM)                 00111
* STATYPC                         ECR-STATYPC                           00112
DFH0028  DFHMDF POS=(10,63),LENGTH=4,ATTRB=(ASKIP,NORM)                 00113
         DFHMDF POS=(10,68),LENGTH=1,ATTRB=(PROT,NORM)                  00114
* PTSCRMX                         ECR-PTSCRMX                           00115
DFH0029  DFHMDF POS=(10,73),LENGTH=3,ATTRB=(PROT,NORM)                  00116
         DFHMDF POS=(10,77),LENGTH=1,ATTRB=(PROT,NORM)                  00117
* CSECODC                         ECR-CSECODC                           00118
DFH0030  DFHMDF POS=(11,1),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00119
* IDENTIF                         ECR-IDENTIF                           00120
DFH0031  DFHMDF POS=(11,3),LENGTH=30,ATTRB=(ASKIP,NORM)                 00121
         DFHMDF POS=(11,34),LENGTH=0,ATTRB=(PROT,NORM)                  00122
* DATENTD                         ECR-DATENTD                           00123
DFH0032  DFHMDF POS=(11,35),LENGTH=8,ATTRB=(PROT,NORM)                  00124
         DFHMDF POS=(11,44),LENGTH=0,ATTRB=(PROT,NORM)                  00125
* DATSORD                         ECR-DATSORD                           00126
DFH0033  DFHMDF POS=(11,45),LENGTH=8,ATTRB=(PROT,NORM)                  00127
         DFHMDF POS=(11,54),LENGTH=1,ATTRB=(PROT,NORM)                  00128
* MOTIFSC                         ECR-MOTIFSC                           00129
DFH0034  DFHMDF POS=(11,56),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00130
         DFHMDF POS=(11,58),LENGTH=1,ATTRB=(ASKIP,NORM)                 00131
* STATYPC                         ECR-STATYPC                           00132
DFH0035  DFHMDF POS=(11,63),LENGTH=4,ATTRB=(ASKIP,NORM)                 00133
         DFHMDF POS=(11,68),LENGTH=1,ATTRB=(PROT,NORM)                  00134
* PTSCRMX                         ECR-PTSCRMX                           00135
DFH0036  DFHMDF POS=(11,73),LENGTH=3,ATTRB=(PROT,NORM)                  00136
         DFHMDF POS=(11,77),LENGTH=1,ATTRB=(PROT,NORM)                  00137
* CSECODC                         ECR-CSECODC                           00138
DFH0037  DFHMDF POS=(12,1),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00139
* IDENTIF                         ECR-IDENTIF                           00140
DFH0038  DFHMDF POS=(12,3),LENGTH=30,ATTRB=(ASKIP,NORM)                 00141
         DFHMDF POS=(12,34),LENGTH=0,ATTRB=(PROT,NORM)                  00142
* DATENTD                         ECR-DATENTD                           00143
DFH0039  DFHMDF POS=(12,35),LENGTH=8,ATTRB=(PROT,NORM)                  00144
         DFHMDF POS=(12,44),LENGTH=0,ATTRB=(PROT,NORM)                  00145
* DATSORD                         ECR-DATSORD                           00146
DFH0040  DFHMDF POS=(12,45),LENGTH=8,ATTRB=(PROT,NORM)                  00147
         DFHMDF POS=(12,54),LENGTH=1,ATTRB=(PROT,NORM)                  00148
* MOTIFSC                         ECR-MOTIFSC                           00149
DFH0041  DFHMDF POS=(12,56),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00150
         DFHMDF POS=(12,58),LENGTH=1,ATTRB=(ASKIP,NORM)                 00151
* STATYPC                         ECR-STATYPC                           00152
DFH0042  DFHMDF POS=(12,63),LENGTH=4,ATTRB=(ASKIP,NORM)                 00153
         DFHMDF POS=(12,68),LENGTH=1,ATTRB=(PROT,NORM)                  00154
* PTSCRMX                         ECR-PTSCRMX                           00155
DFH0043  DFHMDF POS=(12,73),LENGTH=3,ATTRB=(PROT,NORM)                  00156
         DFHMDF POS=(12,77),LENGTH=1,ATTRB=(PROT,NORM)                  00157
* CSECODC                         ECR-CSECODC                           00158
DFH0044  DFHMDF POS=(13,1),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00159
* IDENTIF                         ECR-IDENTIF                           00160
DFH0045  DFHMDF POS=(13,3),LENGTH=30,ATTRB=(ASKIP,NORM)                 00161
         DFHMDF POS=(13,34),LENGTH=0,ATTRB=(PROT,NORM)                  00162
* DATENTD                         ECR-DATENTD                           00163
DFH0046  DFHMDF POS=(13,35),LENGTH=8,ATTRB=(PROT,NORM)                  00164
         DFHMDF POS=(13,44),LENGTH=0,ATTRB=(PROT,NORM)                  00165
* DATSORD                         ECR-DATSORD                           00166
DFH0047  DFHMDF POS=(13,45),LENGTH=8,ATTRB=(PROT,NORM)                  00167
         DFHMDF POS=(13,54),LENGTH=1,ATTRB=(PROT,NORM)                  00168
* MOTIFSC                         ECR-MOTIFSC                           00169
DFH0048  DFHMDF POS=(13,56),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00170
         DFHMDF POS=(13,58),LENGTH=1,ATTRB=(ASKIP,NORM)                 00171
* STATYPC                         ECR-STATYPC                           00172
DFH0049  DFHMDF POS=(13,63),LENGTH=4,ATTRB=(ASKIP,NORM)                 00173
         DFHMDF POS=(13,68),LENGTH=1,ATTRB=(PROT,NORM)                  00174
* PTSCRMX                         ECR-PTSCRMX                           00175
DFH0050  DFHMDF POS=(13,73),LENGTH=3,ATTRB=(PROT,NORM)                  00176
         DFHMDF POS=(13,77),LENGTH=1,ATTRB=(PROT,NORM)                  00177
* CSECODC                         ECR-CSECODC                           00178
DFH0051  DFHMDF POS=(14,1),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00179
* IDENTIF                         ECR-IDENTIF                           00180
DFH0052  DFHMDF POS=(14,3),LENGTH=30,ATTRB=(ASKIP,NORM)                 00181
         DFHMDF POS=(14,34),LENGTH=0,ATTRB=(PROT,NORM)                  00182
* DATENTD                         ECR-DATENTD                           00183
DFH0053  DFHMDF POS=(14,35),LENGTH=8,ATTRB=(PROT,NORM)                  00184
         DFHMDF POS=(14,44),LENGTH=0,ATTRB=(PROT,NORM)                  00185
* DATSORD                         ECR-DATSORD                           00186
DFH0054  DFHMDF POS=(14,45),LENGTH=8,ATTRB=(PROT,NORM)                  00187
         DFHMDF POS=(14,54),LENGTH=1,ATTRB=(PROT,NORM)                  00188
* MOTIFSC                         ECR-MOTIFSC                           00189
DFH0055  DFHMDF POS=(14,56),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00190
         DFHMDF POS=(14,58),LENGTH=1,ATTRB=(ASKIP,NORM)                 00191
* STATYPC                         ECR-STATYPC                           00192
DFH0056  DFHMDF POS=(14,63),LENGTH=4,ATTRB=(ASKIP,NORM)                 00193
         DFHMDF POS=(14,68),LENGTH=1,ATTRB=(PROT,NORM)                  00194
* PTSCRMX                         ECR-PTSCRMX                           00195
DFH0057  DFHMDF POS=(14,73),LENGTH=3,ATTRB=(PROT,NORM)                  00196
         DFHMDF POS=(14,77),LENGTH=1,ATTRB=(PROT,NORM)                  00197
* CSECODC                         ECR-CSECODC                           00198
DFH0058  DFHMDF POS=(15,1),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00199
* IDENTIF                         ECR-IDENTIF                           00200
DFH0059  DFHMDF POS=(15,3),LENGTH=30,ATTRB=(ASKIP,NORM)                 00201
         DFHMDF POS=(15,34),LENGTH=0,ATTRB=(PROT,NORM)                  00202
* DATENTD                         ECR-DATENTD                           00203
DFH0060  DFHMDF POS=(15,35),LENGTH=8,ATTRB=(PROT,NORM)                  00204
         DFHMDF POS=(15,44),LENGTH=0,ATTRB=(PROT,NORM)                  00205
* DATSORD                         ECR-DATSORD                           00206
DFH0061  DFHMDF POS=(15,45),LENGTH=8,ATTRB=(PROT,NORM)                  00207
         DFHMDF POS=(15,54),LENGTH=1,ATTRB=(PROT,NORM)                  00208
* MOTIFSC                         ECR-MOTIFSC                           00209
DFH0062  DFHMDF POS=(15,56),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00210
         DFHMDF POS=(15,58),LENGTH=1,ATTRB=(ASKIP,NORM)                 00211
* STATYPC                         ECR-STATYPC                           00212
DFH0063  DFHMDF POS=(15,63),LENGTH=4,ATTRB=(ASKIP,NORM)                 00213
         DFHMDF POS=(15,68),LENGTH=1,ATTRB=(PROT,NORM)                  00214
* PTSCRMX                         ECR-PTSCRMX                           00215
DFH0064  DFHMDF POS=(15,73),LENGTH=3,ATTRB=(PROT,NORM)                  00216
         DFHMDF POS=(15,77),LENGTH=1,ATTRB=(PROT,NORM)                  00217
* CSECODC                         ECR-CSECODC                           00218
DFH0065  DFHMDF POS=(16,1),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00219
* IDENTIF                         ECR-IDENTIF                           00220
DFH0066  DFHMDF POS=(16,3),LENGTH=30,ATTRB=(ASKIP,NORM)                 00221
         DFHMDF POS=(16,34),LENGTH=0,ATTRB=(PROT,NORM)                  00222
* DATENTD                         ECR-DATENTD                           00223
DFH0067  DFHMDF POS=(16,35),LENGTH=8,ATTRB=(PROT,NORM)                  00224
         DFHMDF POS=(16,44),LENGTH=0,ATTRB=(PROT,NORM)                  00225
* DATSORD                         ECR-DATSORD                           00226
DFH0068  DFHMDF POS=(16,45),LENGTH=8,ATTRB=(PROT,NORM)                  00227
         DFHMDF POS=(16,54),LENGTH=1,ATTRB=(PROT,NORM)                  00228
* MOTIFSC                         ECR-MOTIFSC                           00229
DFH0069  DFHMDF POS=(16,56),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00230
         DFHMDF POS=(16,58),LENGTH=1,ATTRB=(ASKIP,NORM)                 00231
* STATYPC                         ECR-STATYPC                           00232
DFH0070  DFHMDF POS=(16,63),LENGTH=4,ATTRB=(ASKIP,NORM)                 00233
         DFHMDF POS=(16,68),LENGTH=1,ATTRB=(PROT,NORM)                  00234
* PTSCRMX                         ECR-PTSCRMX                           00235
DFH0071  DFHMDF POS=(16,73),LENGTH=3,ATTRB=(PROT,NORM)                  00236
         DFHMDF POS=(16,77),LENGTH=1,ATTRB=(PROT,NORM)                  00237
* CSECODC                         ECR-CSECODC                           00238
DFH0072  DFHMDF POS=(17,1),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00239
* IDENTIF                         ECR-IDENTIF                           00240
DFH0073  DFHMDF POS=(17,3),LENGTH=30,ATTRB=(ASKIP,NORM)                 00241
         DFHMDF POS=(17,34),LENGTH=0,ATTRB=(PROT,NORM)                  00242
* DATENTD                         ECR-DATENTD                           00243
DFH0074  DFHMDF POS=(17,35),LENGTH=8,ATTRB=(PROT,NORM)                  00244
         DFHMDF POS=(17,44),LENGTH=0,ATTRB=(PROT,NORM)                  00245
* DATSORD                         ECR-DATSORD                           00246
DFH0075  DFHMDF POS=(17,45),LENGTH=8,ATTRB=(PROT,NORM)                  00247
         DFHMDF POS=(17,54),LENGTH=1,ATTRB=(PROT,NORM)                  00248
* MOTIFSC                         ECR-MOTIFSC                           00249
DFH0076  DFHMDF POS=(17,56),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00250
         DFHMDF POS=(17,58),LENGTH=1,ATTRB=(ASKIP,NORM)                 00251
* STATYPC                         ECR-STATYPC                           00252
DFH0077  DFHMDF POS=(17,63),LENGTH=4,ATTRB=(ASKIP,NORM)                 00253
         DFHMDF POS=(17,68),LENGTH=1,ATTRB=(PROT,NORM)                  00254
* PTSCRMX                         ECR-PTSCRMX                           00255
DFH0078  DFHMDF POS=(17,73),LENGTH=3,ATTRB=(PROT,NORM)                  00256
         DFHMDF POS=(17,77),LENGTH=1,ATTRB=(PROT,NORM)                  00257
         DFHMDF POS=(18,80),LENGTH=25,                                 *00258
               INITIAL='AJOUTER UN CONDUCTEUR ? :',ATTRB=(PROT,NORM)    00259
* AJOUTCON                        ECR-AJOUTCON                          00260
DFH0079  DFHMDF POS=(19,26),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00261
         DFHMDF POS=(19,28),LENGTH=5,INITIAL='(O/N)',ATTRB=(ASKIP,NORM) 00262
         DFHMDF POS=(19,80),LENGTH=25,                                 *00263
               INITIAL='AJOUTER UN VEHICULE ?   :',ATTRB=(ASKIP,NORM)   00264
* AJOUTVEH                        ECR-AJOUTVEH                          00265
DFH0080  DFHMDF POS=(20,26),LENGTH=1,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL  00266
         DFHMDF POS=(20,28),LENGTH=31,                                 *00267
               INITIAL='(Pr�ciser le type ou taper ''?'')',            *00268
               ATTRB=(ASKIP,NORM)                                       00269
         DFHMDF POS=(20,80),LENGTH=79,                                 *00270
               INITIAL='-----------------------------------------------*00271
               --------------------------------',ATTRB=(ASKIP,BRT),    *00272
               COLOR=BLUE                                               00273
         DFHMDF POS=(21,80),LENGTH=4,INITIAL='Cmde',ATTRB=(ASKIP,BRT), *00274
               COLOR=BLUE                                               00275
         DFHMDF POS=(22,5),LENGTH=2,INITIAL='=>',ATTRB=(ASKIP,BRT),    *00276
               COLOR=BLUE                                               00277
* XCDEC                           ECR-XCDEC                             00278
DFH0081  DFHMDF POS=(22,9),LENGTH=9,ATTRB=(UNPROT,NORM),COLOR=NEUTRAL   00279
         DFHMDF POS=(22,19),LENGTH=1,ATTRB=(ASKIP,NORM)                 00280
* XMSGIL                          ECR-XMSGIL                            00281
DFH0082  DFHMDF POS=(22,21),LENGTH=59,ATTRB=(ASKIP,BRT),COLOR=BLUE      00282
         DFHMDF POS=(23,1),LENGTH=0,ATTRB=(PROT,NORM)                   00283
* XMSGAL                          ECR-XMSGAL                            00284
DFH0083  DFHMDF POS=(23,2),LENGTH=64,ATTRB=(PROT,BRT),COLOR=BLUE        00285
         DFHMDF POS=(23,67),LENGTH=1,ATTRB=(PROT,NORM)                  00286
         DFHMSD TYPE=FINAL                                              00287
         END                                                            00288
