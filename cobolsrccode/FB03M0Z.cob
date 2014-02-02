      ***************************************************************** 00000010
      * ANTECEDENTS CONDUCTEUR                                          00000020
      ***************************************************************** 00000030
       05   FB03M0I.                                                    00000040
           10 FILLER    PIC X(12).                                      00000050
      * CODE TERMINAL,CODE TRANSACTION                                  00000060
           10 ECR-XTRMTRACL  COMP PIC S9(4).                            00000070
           10 ECR-XTRMTRACF  PIC X.                                     00000080
           10 ECR-XTRMTRACI  PIC X(9).                                  00000090
      * LIBELLE APPLICATION                                             00000100
           10 ECR-XAPPLILL   COMP PIC S9(4).                            00000110
           10 ECR-XAPPLILF   PIC X.                                     00000120
           10 ECR-XAPPLILI   PIC X(30).                                 00000130
      * DATE DU JOUR JJ/MM/AA                                           00000140
           10 ECR-XJOURDL    COMP PIC S9(4).                            00000150
           10 ECR-XJOURDF    PIC X.                                     00000160
           10 ECR-XJOURDI    PIC X(8).                                  00000170
      * NOM RACF DE L'OPERATEUR                                         00000180
           10 ECR-XRACFLL    COMP PIC S9(4).                            00000190
           10 ECR-XRACFLF    PIC X.                                     00000200
           10 ECR-XRACFLI    PIC X(15).                                 00000210
      * HEURE HH/MM/SS                                                  00000220
           10 ECR-XHEUREDL   COMP PIC S9(4).                            00000230
           10 ECR-XHEUREDF   PIC X.                                     00000240
           10 ECR-XHEUREDI   PIC X(8).                                  00000250
      * CODE INTERMEDIAIRE                                              00000260
           10 ECR-GESCLIL    COMP PIC S9(4).                            00000270
           10 ECR-GESCLIF    PIC X.                                     00000280
           10 ECR-GESCLII    PIC X(11).                                 00000290
      * QUALITE DU CLIENT                                               00000300
           10 ECR-RAICL      COMP PIC S9(4).                            00000310
           10 ECR-RAICF      PIC X.                                     00000320
           10 ECR-RAICI      PIC X(3).                                  00000330
      * NOM DU CLIENT                                                   00000340
           10 ECR-NOMCL      COMP PIC S9(4).                            00000350
           10 ECR-NOMCF      PIC X.                                     00000360
           10 ECR-NOMCI      PIC X(31).                                 00000370
      * titre                                                           00000380
           10 ECR-PERTITLL   COMP PIC S9(4).                            00000390
           10 ECR-PERTITLF   PIC X.                                     00000400
           10 ECR-PERTITLI   PIC X(3).                                  00000410
      * nom                                                             00000420
           10 ECR-PERNOMLL   COMP PIC S9(4).                            00000430
           10 ECR-PERNOMLF   PIC X.                                     00000440
           10 ECR-PERNOMLI   PIC X(20).                                 00000450
      * prénom                                                          00000460
           10 ECR-PERPRELL   COMP PIC S9(4).                            00000470
           10 ECR-PERPRELF   PIC X.                                     00000480
           10 ECR-PERPRELI   PIC X(20).                                 00000490
      * idclient                                                        00000500
           10 ECR-PERNUMXL   COMP PIC S9(4).                            00000510
           10 ECR-PERNUMXF   PIC X.                                     00000520
           10 ECR-PERNUMXI   PIC X(6).                                  00000530
      * nb sinistres ds 36 dern mois                                    00000540
           10 ECR-ANPSINNL   COMP PIC S9(4).                            00000550
           10 ECR-ANPSINNF   PIC X.                                     00000560
           10 ECR-ANPSINNI   PIC X(2).                                  00000570
      * nature de sinistre                                              00000580
           10 ECR-SIPNATC1L  COMP PIC S9(4).                            00000590
           10 ECR-SIPNATC1F  PIC X.                                     00000600
           10 ECR-SIPNATC1I  PIC X.                                     00000610
      * date de sinistre                                                00000620
           10 ECR-SIPDATD1L  COMP PIC S9(4).                            00000630
           10 ECR-SIPDATD1F  PIC X.                                     00000640
           10 ECR-SIPDATD1I  PIC X(6).                                  00000650
      * nature de sinistre                                              00000660
           10 ECR-SIPNATC2L  COMP PIC S9(4).                            00000670
           10 ECR-SIPNATC2F  PIC X.                                     00000680
           10 ECR-SIPNATC2I  PIC X.                                     00000690
      * date de sinistre                                                00000700
           10 ECR-SIPDATD2L  COMP PIC S9(4).                            00000710
           10 ECR-SIPDATD2F  PIC X.                                     00000720
           10 ECR-SIPDATD2I  PIC X(6).                                  00000730
      * nature de sinistre                                              00000740
           10 ECR-SIPNATC3L  COMP PIC S9(4).                            00000750
           10 ECR-SIPNATC3F  PIC X.                                     00000760
           10 ECR-SIPNATC3I  PIC X.                                     00000770
      * date de sinistre                                                00000780
           10 ECR-SIPDATD3L  COMP PIC S9(4).                            00000790
           10 ECR-SIPDATD3F  PIC X.                                     00000800
           10 ECR-SIPDATD3I  PIC X(6).                                  00000810
      * nature de sinistre                                              00000820
           10 ECR-SIPNATC4L  COMP PIC S9(4).                            00000830
           10 ECR-SIPNATC4F  PIC X.                                     00000840
           10 ECR-SIPNATC4I  PIC X.                                     00000850
      * date de sinistre                                                00000860
           10 ECR-SIPDATD4L  COMP PIC S9(4).                            00000870
           10 ECR-SIPDATD4F  PIC X.                                     00000880
           10 ECR-SIPDATD4I  PIC X(6).                                  00000890
      * nature de sinistre                                              00000900
           10 ECR-SIPNATC5L  COMP PIC S9(4).                            00000910
           10 ECR-SIPNATC5F  PIC X.                                     00000920
           10 ECR-SIPNATC5I  PIC X.                                     00000930
      * date de sinistre                                                00000940
           10 ECR-SIPDATD5L  COMP PIC S9(4).                            00000950
           10 ECR-SIPDATD5F  PIC X.                                     00000960
           10 ECR-SIPDATD5I  PIC X(6).                                  00000970
      * nature de sinistre                                              00000980
           10 ECR-SIPNATC6L  COMP PIC S9(4).                            00000990
           10 ECR-SIPNATC6F  PIC X.                                     00001000
           10 ECR-SIPNATC6I  PIC X.                                     00001010
      * date de sinistre                                                00001020
           10 ECR-SIPDATD6L  COMP PIC S9(4).                            00001030
           10 ECR-SIPDATD6F  PIC X.                                     00001040
           10 ECR-SIPDATD6I  PIC X(6).                                  00001050
      * code commande                                                   00001060
           10 ECR-XCDECL     COMP PIC S9(4).                            00001500
           10 ECR-XCDECF     PIC X.                                     00001510
           10 ECR-XCDECI     PIC X(9).                                  00001520
      * message informatif                                              00001100
           10 ECR-XMSGILL    COMP PIC S9(4).                            00001540
           10 ECR-XMSGILF    PIC X.                                     00001550
           10 ECR-XMSGILI    PIC X(59).                                 00001560
      * message d'anomalie                                              00001140
           10 ECR-XMSGALL    COMP PIC S9(4).                            00001580
           10 ECR-XMSGALF    PIC X.                                     00001590
           10 ECR-XMSGALI    PIC X(79).                                 00001600
      ***************************************************************** 00001610
      * ANTECEDENTS CONDUCTEUR                                          00001190
      ***************************************************************** 00001630
       05   FB03M0O REDEFINES FB03M0I.                                  00001210
           10 FILLER    PIC X(12).                                      00001650
      * CODE TERMINAL,CODE TRANSACTION                                  00001660
           10 FILLER    PIC X(2).                                       00001670
           10 ECR-XTRMTRACA  PIC X.                                     00001680
           10 ECR-XTRMTRACO  PIC X(9).                                  00001690
      * LIBELLE APPLICATION                                             00001700
           10 FILLER    PIC X(2).                                       00001710
           10 ECR-XAPPLILA   PIC X.                                     00001720
           10 ECR-XAPPLILO   PIC X(30).                                 00001730
      * DATE DU JOUR JJ/MM/AA                                           00001740
           10 FILLER    PIC X(2).                                       00001750
           10 ECR-XJOURDA    PIC X.                                     00001760
           10 ECR-XJOURDO    PIC X(8).                                  00001770
      * NOM RACF DE L'OPERATEUR                                         00001780
           10 FILLER    PIC X(2).                                       00001790
           10 ECR-XRACFLA    PIC X.                                     00001800
           10 ECR-XRACFLO    PIC X(15).                                 00001810
      * HEURE HH/MM/SS                                                  00001820
           10 FILLER    PIC X(2).                                       00001830
           10 ECR-XHEUREDA   PIC X.                                     00001840
           10 ECR-XHEUREDO   PIC X(8).                                  00001850
      * CODE INTERMEDIAIRE                                              00001430
           10 FILLER    PIC X(2).                                       00001870
           10 ECR-GESCLIA    PIC X.                                     00001450
           10 ECR-GESCLIO    PIC X(11).                                 00001460
      * QUALITE DU CLIENT                                               00001470
           10 FILLER    PIC X(2).                                       00001910
           10 ECR-RAICA      PIC X.                                     00001920
           10 ECR-RAICO      PIC X(3).                                  00001930
      * NOM DU CLIENT                                                   00001510
           10 FILLER    PIC X(2).                                       00001950
           10 ECR-NOMCA      PIC X.                                     00001960
           10 ECR-NOMCO      PIC X(31).                                 00001970
      * titre                                                           00001550
           10 FILLER    PIC X(2).                                       00001980
           10 ECR-PERTITLA   PIC X.                                     00001570
           10 ECR-PERTITLO   PIC X(3).                                  00001580
      * nom                                                             00001590
           10 FILLER    PIC X(2).                                       00002020
           10 ECR-PERNOMLA   PIC X.                                     00001610
           10 ECR-PERNOMLO   PIC X(20).                                 00001620
      * prénom                                                          00001630
           10 FILLER    PIC X(2).                                       00002060
           10 ECR-PERPRELA   PIC X.                                     00001650
           10 ECR-PERPRELO   PIC X(20).                                 00001660
      * idclient                                                        00001670
           10 FILLER    PIC X(2).                                       00002100
           10 ECR-PERNUMXA   PIC X.                                     00001690
           10 ECR-PERNUMXO   PIC X(6).                                  00001700
      * nb sinistres ds 36 dern mois                                    00001710
           10 FILLER    PIC X(2).                                       00002140
           10 ECR-ANPSINNA   PIC X.                                     00001730
           10 ECR-ANPSINNO   PIC X(2).                                  00001740
      * nature de sinistre                                              00001750
           10 FILLER    PIC X(2).                                       00002180
           10 ECR-SIPNATC1A  PIC X.                                     00001770
           10 ECR-SIPNATC1O  PIC X.                                     00001780
      * date de sinistre                                                00001790
           10 FILLER    PIC X(2).                                       00002220
           10 ECR-SIPDATD1A  PIC X.                                     00001810
           10 ECR-SIPDATD1O  PIC X(6).                                  00001820
      * nature de sinistre                                              00001830
           10 FILLER    PIC X(2).                                       00002260
           10 ECR-SIPNATC2A  PIC X.                                     00001850
           10 ECR-SIPNATC2O  PIC X.                                     00001860
      * date de sinistre                                                00001870
           10 FILLER    PIC X(2).                                       00002300
           10 ECR-SIPDATD2A  PIC X.                                     00001890
           10 ECR-SIPDATD2O  PIC X(6).                                  00001900
      * nature de sinistre                                              00001910
           10 FILLER    PIC X(2).                                       00002340
           10 ECR-SIPNATC3A  PIC X.                                     00001930
           10 ECR-SIPNATC3O  PIC X.                                     00001940
      * date de sinistre                                                00001950
           10 FILLER    PIC X(2).                                       00002380
           10 ECR-SIPDATD3A  PIC X.                                     00001970
           10 ECR-SIPDATD3O  PIC X(6).                                  00001980
      * nature de sinistre                                              00001990
           10 FILLER    PIC X(2).                                       00002420
           10 ECR-SIPNATC4A  PIC X.                                     00002010
           10 ECR-SIPNATC4O  PIC X.                                     00002020
      * date de sinistre                                                00002030
           10 FILLER    PIC X(2).                                       00002460
           10 ECR-SIPDATD4A  PIC X.                                     00002050
           10 ECR-SIPDATD4O  PIC X(6).                                  00002060
      * nature de sinistre                                              00002070
           10 FILLER    PIC X(2).                                       00002500
           10 ECR-SIPNATC5A  PIC X.                                     00002090
           10 ECR-SIPNATC5O  PIC X.                                     00002100
      * date de sinistre                                                00002110
           10 FILLER    PIC X(2).                                       00002540
           10 ECR-SIPDATD5A  PIC X.                                     00002130
           10 ECR-SIPDATD5O  PIC X(6).                                  00002140
      * nature de sinistre                                              00002150
           10 FILLER    PIC X(2).                                       00002580
           10 ECR-SIPNATC6A  PIC X.                                     00002170
           10 ECR-SIPNATC6O  PIC X.                                     00002180
      * date de sinistre                                                00002190
           10 FILLER    PIC X(2).                                       00002620
           10 ECR-SIPDATD6A  PIC X.                                     00002210
           10 ECR-SIPDATD6O  PIC X(6).                                  00002220
      * code commande                                                   00002230
           10 FILLER    PIC X(2).                                       00002660
           10 ECR-XCDECA     PIC X.                                     00003110
           10 ECR-XCDECO     PIC X(9).                                  00003120
      * message informatif                                              00002270
           10 FILLER    PIC X(2).                                       00003140
           10 ECR-XMSGILA    PIC X.                                     00003150
           10 ECR-XMSGILO    PIC X(59).                                 00003160
      * message d'anomalie                                              00002310
           10 FILLER    PIC X(2).                                       00003180
           10 ECR-XMSGALA    PIC X.                                     00003190
           10 ECR-XMSGALO    PIC X(79).                                 00003200
