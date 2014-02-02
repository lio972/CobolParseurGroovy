      ***************************************************************** 00000010
      * COMPOSITION CONTRAT 4 ROUES GFA                                 00000020
      ***************************************************************** 00000030
       02   FB01M0I.                                                    00000040
               05 FILLER     PIC X(12).                                 00000050
               05 ECR-XTRMTRACL   COMP PIC S9(4).                       00000060
               05 ECR-XTRMTRACF   PIC X.                                00000070
               05 ECR-XTRMTRACI   PIC X(9).                             00000080
      * LIBELLE APPLICATION                                             00000090
               05 ECR-XAPPLILL    COMP PIC S9(4).                       00000100
               05 ECR-XAPPLILF    PIC X.                                00000110
               05 ECR-XAPPLILI    PIC X(30).                            00000120
      * DATE DU JOUR JJ/MM/AA                                           00000130
               05 ECR-XJOURDL     COMP PIC S9(4).                       00000140
               05 ECR-XJOURDF     PIC X.                                00000150
               05 ECR-XJOURDI     PIC X(8).                             00000160
      * NOM RACF DE L'OPERATEUR                                         00000170
               05 ECR-XRACFLL     COMP PIC S9(4).                       00000180
               05 ECR-XRACFLF     PIC X.                                00000190
               05 ECR-XRACFLI     PIC X(15).                            00000200
      * HEURE HH/MM/SS                                                  00000210
               05 ECR-XHEUREDL    COMP PIC S9(4).                       00000220
               05 ECR-XHEUREDF    PIC X.                                00000230
               05 ECR-XHEUREDI    PIC X(8).                             00000240
      * CODE INTERMEDIAIRE                                              00000250
               05 ECR-GESCLIL     COMP PIC S9(4).                       00000260
               05 ECR-GESCLIF     PIC X.                                00000270
               05 ECR-GESCLII     PIC X(11).                            00000280
      * QUALITE DU CLIENT                                               00000290
               05 ECR-RAICL  COMP PIC S9(4).                            00000300
               05 ECR-RAICF  PIC X.                                     00000310
               05 ECR-RAICI  PIC X(3).                                  00000320
      * NOM DU CLIENT                                                   00000330
               05 ECR-NOMCL  COMP PIC S9(4).                            00000340
               05 ECR-NOMCF  PIC X.                                     00000350
               05 ECR-NOMCI  PIC X(30).                                 00000360
               05 ECR-LIGNEI OCCURS   10 TIMES .                        00000370
      * code sélection                                                  00000380
                 06 ECR-CSECODCL  COMP PIC S9(4).                       00000390
                 06 ECR-CSECODCF  PIC X.                                00000400
                 06 ECR-CSECODCI  PIC X.                                00000410
      * identifiant                                                     00000420
                 06 ECR-IDENTIFL  COMP PIC S9(4).                       00000430
                 06 ECR-IDENTIFF  PIC X.                                00000440
                 06 ECR-IDENTIFI  PIC X(30).                            00000450
      * date d'entrée                                                   00000460
                 06 ECR-DATENTDL  COMP PIC S9(4).                       00000470
                 06 ECR-DATENTDF  PIC X.                                00000480
                 06 ECR-DATENTDI  PIC X(8).                             00000490
      * date de sortie                                                  00000500
                 06 ECR-DATSORDL  COMP PIC S9(4).                       00000510
                 06 ECR-DATSORDF  PIC X.                                00000520
                 06 ECR-DATSORDI  PIC X(8).                             00000530
      * motif de sortie                                                 00000540
                 06 ECR-MOTIFSCL  COMP PIC S9(4).                       00000550
                 06 ECR-MOTIFSCF  PIC X.                                00000560
                 06 ECR-MOTIFSCI  PIC X.                                00000570
      * statut/type                                                     00000540
                 06 ECR-STATYPCL  COMP PIC S9(4).                       00000550
                 06 ECR-STATYPCF  PIC X.                                00000560
                 06 ECR-STATYPCI  PIC X(4).                             00000570
      * points/CRM                                                      00000580
                 06 ECR-PTSCRMXL  COMP PIC S9(4).                       00000590
                 06 ECR-PTSCRMXF  PIC X.                                00000600
                 06 ECR-PTSCRMXI  PIC X(3).                             00000610
      * code ajout conducteur                                           00000620
               05 ECR-AJOUTCONL   COMP PIC S9(4).                       00000630
               05 ECR-AJOUTCONF   PIC X.                                00000640
               05 ECR-AJOUTCONI   PIC X.                                00000650
      * code ajout véhiule                                              00000660
               05 ECR-AJOUTVEHL   COMP PIC S9(4).                       00000670
               05 ECR-AJOUTVEHF   PIC X.                                00000680
               05 ECR-AJOUTVEHI   PIC X.                                00000690
      * code commande                                                   00000700
               05 ECR-XCDECL      COMP PIC S9(4).                       00000710
               05 ECR-XCDECF      PIC X.                                00000720
               05 ECR-XCDECI      PIC X(9).                             00000730
      * message informatif                                              00000740
               05 ECR-XMSGILL     COMP PIC S9(4).                       00000750
               05 ECR-XMSGILF     PIC X.                                00000760
               05 ECR-XMSGILI     PIC X(59).                            00000770
      * message d'anomalie                                              00000780
               05 ECR-XMSGALL     COMP PIC S9(4).                       00000790
               05 ECR-XMSGALF     PIC X.                                00000800
               05 ECR-XMSGALI     PIC X(64).                            00000810
      ***************************************************************** 00000820
      * COMPOSITION CONTRAT 4 ROUES GFA                                 00000830
      ***************************************************************** 00000840
       02   FB01M0O REDEFINES FB01M0I.                                  00000850
               05 FILLER     PIC X(12).                                 00000860
               05 FILLER     PIC X(2).                                  00000870
               05 ECR-XTRMTRACA   PIC X.                                00000880
               05 ECR-XTRMTRACO   PIC X(9).                             00000890
      * LIBELLE APPLICATION                                             00000900
               05 FILLER     PIC X(2).                                  00000910
               05 ECR-XAPPLILA    PIC X.                                00000920
               05 ECR-XAPPLILO    PIC X(30).                            00000930
      * DATE DU JOUR JJ/MM/AA                                           00000940
               05 FILLER     PIC X(2).                                  00000950
               05 ECR-XJOURDA     PIC X.                                00000960
               05 ECR-XJOURDO     PIC X(8).                             00000970
      * NOM RACF DE L'OPERATEUR                                         00000980
               05 FILLER     PIC X(2).                                  00000990
               05 ECR-XRACFLA     PIC X.                                00001000
               05 ECR-XRACFLO     PIC X(15).                            00001010
      * HEURE HH/MM/SS                                                  00001020
               05 FILLER     PIC X(2).                                  00001030
               05 ECR-XHEUREDA    PIC X.                                00001040
               05 ECR-XHEUREDO    PIC X(8).                             00001050
      * CODE INTERMEDIAIRE                                              00001060
               05 FILLER     PIC X(2).                                  00001070
               05 ECR-GESCLIA     PIC X.                                00001080
               05 ECR-GESCLIO     PIC X(11).                            00001090
      * QUALITE DU CLIENT                                               00001100
               05 FILLER     PIC X(2).                                  00001110
               05 ECR-RAICA  PIC X.                                     00001120
               05 ECR-RAICO  PIC X(3).                                  00001130
      * NOM DU CLIENT                                                   00001140
               05 FILLER     PIC X(2).                                  00001150
               05 ECR-NOMCA  PIC X.                                     00001160
               05 ECR-NOMCO  PIC X(30).                                 00001170
               05 ECR-LIGNEO OCCURS   10 TIMES .                        00001220
      * code sélection                                                  00001190
                 06 FILLER   PIC X(2).                                  00001200
                 06 ECR-CSECODCA  PIC X.                                00001210
                 06 ECR-CSECODCO  PIC X.                                00001220
      * identifiant                                                     00001230
                 06 FILLER   PIC X(2).                                  00001240
                 06 ECR-IDENTIFA  PIC X.                                00001250
                 06 ECR-IDENTIFO  PIC X(30).                            00001260
      * date d'entrée                                                   00001270
                 06 FILLER   PIC X(2).                                  00001280
                 06 ECR-DATENTDA  PIC X.                                00001290
                 06 ECR-DATENTDO  PIC X(8).                             00001300
      * date de sortie                                                  00001310
                 06 FILLER   PIC X(2).                                  00001320
                 06 ECR-DATSORDA  PIC X.                                00001330
                 06 ECR-DATSORDO  PIC X(8).                             00001340
      * motif de sortie                                                 00001390
                 06 FILLER   PIC X(2).                                  00001400
                 06 ECR-MOTIFSCA  PIC X.                                00001410
                 06 ECR-MOTIFSCO  PIC X.                                00001420
      * statut/type                                                     00001350
                 06 FILLER   PIC X(2).                                  00001360
                 06 ECR-STATYPCA  PIC X.                                00001370
                 06 ECR-STATYPCO  PIC X(4).                             00001380
      * points/CRM                                                      00001390
                 06 FILLER   PIC X(2).                                  00001400
                 06 ECR-PTSCRMXA  PIC X.                                00001410
                 06 ECR-PTSCRMXO  PIC X(3).                             00001420
      * code ajout conducteur                                           00001430
               05 FILLER     PIC X(2).                                  00001440
               05 ECR-AJOUTCONA   PIC X.                                00001450
               05 ECR-AJOUTCONO   PIC X.                                00001460
      * code ajout véhiule                                              00001470
               05 FILLER     PIC X(2).                                  00001480
               05 ECR-AJOUTVEHA   PIC X.                                00001490
               05 ECR-AJOUTVEHO   PIC X.                                00001500
      * code commande                                                   00001510
               05 FILLER     PIC X(2).                                  00001520
               05 ECR-XCDECA      PIC X.                                00001530
               05 ECR-XCDECO      PIC X(9).                             00001540
      * message informatif                                              00001550
               05 FILLER     PIC X(2).                                  00001560
               05 ECR-XMSGILA     PIC X.                                00001570
               05 ECR-XMSGILO     PIC X(59).                            00001580
      * message d'anomalie                                              00001590
               05 FILLER     PIC X(2).                                  00001600
               05 ECR-XMSGALA     PIC X.                                00001610
               05 ECR-XMSGALO     PIC X(64).                            00001620
