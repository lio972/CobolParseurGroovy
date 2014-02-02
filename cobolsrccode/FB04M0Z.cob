      ***************************************************************** 00000010
      * CARACTERISTIQUES / ANTECEDENTS                                  00000020
      ***************************************************************** 00000030
       02   FB04M0I.                                                    00000040
           10 FILLER    PIC X(12).                                      00000050
           10 ECR-XTRMTRACL  COMP PIC S9(4).                            00000060
           10 ECR-XTRMTRACF  PIC X.                                     00000070
           10 ECR-XTRMTRACI  PIC X(9).                                  00000080
      * LIBELLE APPLICATION                                             00000090
           10 ECR-XAPPLILL   COMP PIC S9(4).                            00000100
           10 ECR-XAPPLILF   PIC X.                                     00000110
           10 ECR-XAPPLILI   PIC X(30).                                 00000120
      * DATE DU JOUR JJ/MM/AA                                           00000130
           10 ECR-XJOURDL    COMP PIC S9(4).                            00000140
           10 ECR-XJOURDF    PIC X.                                     00000150
           10 ECR-XJOURDI    PIC X(8).                                  00000160
      * NOM RACF DE L'OPERATEUR                                         00000170
           10 ECR-XRACFLL    COMP PIC S9(4).                            00000180
           10 ECR-XRACFLF    PIC X.                                     00000190
           10 ECR-XRACFLI    PIC X(15).                                 00000200
      * HEURE HH/MM/SS                                                  00000210
           10 ECR-XHEUREDL   COMP PIC S9(4).                            00000220
           10 ECR-XHEUREDF   PIC X.                                     00000230
           10 ECR-XHEUREDI   PIC X(8).                                  00000240
      * CODE INTERMEDIAIRE                                              00000250
           10 ECR-GESCLIL    COMP PIC S9(4).                            00000260
           10 ECR-GESCLIF    PIC X.                                     00000270
           10 ECR-GESCLII    PIC X(11).                                 00000280
      * QUALITE DU CLIENT                                               00000290
           10 ECR-RAICL      COMP PIC S9(4).                            00000300
           10 ECR-RAICF      PIC X.                                     00000310
           10 ECR-RAICI      PIC X(3).                                  00000320
      * NOM DU CLIENT                                                   00000330
           10 ECR-NOMCL      COMP PIC S9(4).                            00000340
           10 ECR-NOMCF      PIC X.                                     00000350
           10 ECR-NOMCI      PIC X(31).                                 00000360
      * CARTE GRISE NOM                                                 00000370
           10 ECR-VEHCGNCL   COMP PIC S9(4).                            00000380
           10 ECR-VEHCGNCF   PIC X.                                     00000390
           10 ECR-VEHCGNCI   PIC X(20).                                 00000400
      * CARTE GRISE PRENOM                                              00000410
           10 ECR-VEHCGPCL   COMP PIC S9(4).                            00000420
           10 ECR-VEHCGPCF   PIC X.                                     00000430
           10 ECR-VEHCGPCI   PIC X(20).                                 00000440
      * CARTE GRISE STATUT                                              00000450
           10 ECR-VEHCGSCL   COMP PIC S9(4).                            00000460
           10 ECR-VEHCGSCF   PIC X.                                     00000470
           10 ECR-VEHCGSCI   PIC X(2).                                  00000480
      * CODE DU VEHICULE                                                00000370
           10 ECR-VEHCODCL   COMP PIC S9(4).                            00000460
           10 ECR-VEHCODCF   PIC X.                                     00000470
           10 ECR-VEHCODCI   PIC X(7).                                  00000480
      * TYPE DU VEHICULE                                                00000410
           10 ECR-VEHTYPCL   COMP PIC S9(4).                            00000500
           10 ECR-VEHTYPCF   PIC X.                                     00000510
           10 ECR-VEHTYPCI   PIC X(3).                                  00000520
      * GENRE DU VEHICULE                                               00000450
           10 ECR-VEHGENCL   COMP PIC S9(4).                            00000460
           10 ECR-VEHGENCF   PIC X.                                     00000470
           10 ECR-VEHGENCI   PIC X.                                     00000480
      * MARQUE DU VEHICULE                                              00000490
           10 ECR-VEHMARLL   COMP PIC S9(4).                            00000540
           10 ECR-VEHMARLF   PIC X.                                     00000550
           10 ECR-VEHMARLI   PIC X(15).                                 00000560
      * MODELE DU VEHICULE                                              00000530
           10 ECR-VEHMODLL   COMP PIC S9(4).                            00000580
           10 ECR-VEHMODLF   PIC X.                                     00000590
           10 ECR-VEHMODLI   PIC X(30).                                 00000600
      * PUISSANCE/CYLINDREE                                             00000570
           10 ECR-VEHCYLNL   COMP PIC S9(4).                            00000620
           10 ECR-VEHCYLNF   PIC X.                                     00000630
           10 ECR-VEHCYLNI   PIC X(5).                                  00000640
      * CODE USAGE DU VEHICULE                                          00000610
           10 ECR-VEHUSACL   COMP PIC S9(4).                            00000620
           10 ECR-VEHUSACF   PIC X.                                     00000630
           10 ECR-VEHUSACI   PIC X(3).                                  00000640
      * IMMATRICULATION                                                 00000650
           10 ECR-VEHIMMXL   COMP PIC S9(4).                            00000660
           10 ECR-VEHIMMXF   PIC X.                                     00000670
           10 ECR-VEHIMMXI   PIC X(10).                                 00000680
      * 1ère MISE EN CIRCULATION                                        00000690
           10 ECR-VEHCIRDL   COMP PIC S9(4).                            00000700
           10 ECR-VEHCIRDF   PIC X.                                     00000710
           10 ECR-VEHCIRDI   PIC X(6).                                  00000720
      * DATE D'ACQUISITION DU VEHICULE                                  00000730
           10 ECR-VEHACQDL   COMP PIC S9(4).                            00000740
           10 ECR-VEHACQDF   PIC X.                                     00000750
           10 ECR-VEHACQDI   PIC X(6).                                  00000760
      * VALEUR A NEUF                                                   00000730
           10 ECR-VEHVALML   COMP PIC S9(4).                            00000820
           10 ECR-VEHVALMF   PIC X.                                     00000830
           10 ECR-VEHVALMI   PIC X(7).                                  00000840
      * GROUPE DU VEHICULE                                              00000770
           10 ECR-VEHGROCL   COMP PIC S9(4).                            00000780
           10 ECR-VEHGROCF   PIC X.                                     00000790
           10 ECR-VEHGROCI   PIC X(2).                                  00000800
      * CLASSE DU VEHICULE                                              00000810
           10 ECR-VEHCLACL   COMP PIC S9(4).                            00000820
           10 ECR-VEHCLACF   PIC X.                                     00000830
           10 ECR-VEHCLACI   PIC X.                                     00000840
      * NIVEAU DE PROTECTION VOL                                        00000770
           10 ECR-VEHPRTCL   COMP PIC S9(4).                            00000780
           10 ECR-VEHPRTCF   PIC X.                                     00000790
           10 ECR-VEHPRTCI   PIC X.                                     00000800
      * TYPE DE GARAGE                                                  00000810
           10 ECR-GARCODCL   COMP PIC S9(4).                            00000820
           10 ECR-GARCODCF   PIC X.                                     00000830
           10 ECR-GARCODCI   PIC X.                                     00000840
      * TYPE DE POSSESSION                                              00000850
           10 ECR-VEHPOSCL   COMP PIC S9(4).                            00000860
           10 ECR-VEHPOSCF   PIC X.                                     00000870
           10 ECR-VEHPOSCI   PIC X.                                     00000880
      * INDICATEUR PERTES FINANCIERES                                   00000890
           10 ECR-VEHPEFCL   COMP PIC S9(4).                            00000900
           10 ECR-VEHPEFCF   PIC X.                                     00000910
           10 ECR-VEHPEFCI   PIC X.                                     00000920
      * CODE POSTAL GARAGE HABITUEL                                     00000930
           10 ECR-GARCOPCL   COMP PIC S9(4).                            00000980
           10 ECR-GARCOPCF   PIC X.                                     00000990
           10 ECR-GARCOPCI   PIC X(5).                                  00001000
      * COMMUNE GARAGE HABITUEL                                         00000970
           10 ECR-GARVILLL   COMP PIC S9(4).                            00001020
           10 ECR-GARVILLF   PIC X.                                     00001030
           10 ECR-GARVILLI   PIC X(30).                                 00001040
      * INDICATEUR VEHI SUPPLEMENTAIRE                                  00001010
           10 ECR-ANVREPCL   COMP PIC S9(4).                            00001060
           10 ECR-ANVREPCF   PIC X.                                     00001070
           10 ECR-ANVREPCI   PIC X.                                     00001080
      * FORMULE DE GARANTIES                                            00001050
           10 ECR-VEHFORCL   COMP PIC S9(4).                            00001100
           10 ECR-VEHFORCF   PIC X.                                     00001110
           10 ECR-VEHFORCI   PIC X(2).                                  00001120
      * NUMERO PRECEDENT CONTRAT                                        00001090
           10 ECR-ANVNUMXL   COMP PIC S9(4).                            00001100
           10 ECR-ANVNUMXF   PIC X.                                     00001110
           10 ECR-ANVNUMXI   PIC X(15).                                 00001120
      * ANCIEN CONDUCTEUR                                               00001210
           10 ECR-ANVSOUSL   COMP PIC S9(4).                            00001220
           10 ECR-ANVSOUSF   PIC X.                                     00001230
           10 ECR-ANVSOUSI   PIC X(4).                                  00001240
           10 ECR-ANVSOUNL   COMP PIC S9(4).                            00001250
           10 ECR-ANVSOUNF   PIC X.                                     00001260
           10 ECR-ANVSOUNI   PIC X(3).                                  00001270
      * CODE CIE PRECEDENT CONTRAT                                      00001130
           10 ECR-ANVCIEXL   COMP PIC S9(4).                            00001140
           10 ECR-ANVCIEXF   PIC X.                                     00001150
           10 ECR-ANVCIEXI   PIC X(3).                                  00001160
      * LIBELLE CIE PRECEDENT CONTRAT                                   00001170
           10 ECR-ANVCIELL   COMP PIC S9(4).                            00001180
           10 ECR-ANVCIELF   PIC X.                                     00001190
           10 ECR-ANVCIELI   PIC X(35).                                 00001200
      * ANCIENNETEE PRECEDENT CONTRAT                                   00001210
           10 ECR-ANVANCNL   COMP PIC S9(4).                            00001220
           10 ECR-ANVANCNF   PIC X.                                     00001230
           10 ECR-ANVANCNI   PIC X(2).                                  00001240
      * NB MOIS D'INTERRUPTION CONTRAT                                  00001250
           10 ECR-ANVINTNL   COMP PIC S9(4).                            00001260
           10 ECR-ANVINTNF   PIC X.                                     00001270
           10 ECR-ANVINTNI   PIC X(2).                                  00001280
      * DATE RESIL PRECEDENT CONTRAT                                    00001290
           10 ECR-ANVRESDL   COMP PIC S9(4).                            00001300
           10 ECR-ANVRESDF   PIC X.                                     00001310
           10 ECR-ANVRESDI   PIC X(6).                                  00001320
      * INDICATEUR DE SINISTRES                                         00001250
           10 ECR-SIVINDCL   COMP PIC S9(4).                            00001260
           10 ECR-SIVINDCF   PIC X.                                     00001270
           10 ECR-SIVINDCI   PIC X.                                     00001280
      * MOTIF DE RESIL ANCIEN CONTRAT                                   00001370
           10 ECR-ANVMTRCL   COMP PIC S9(4).                            00001380
           10 ECR-ANVMTRCF   PIC X.                                     00001390
           10 ECR-ANVMTRCI   PIC X.                                     00001400
      * DERNIER CRM                                                     00001330
           10 ECR-ANVBONTL   COMP PIC S9(4).                            00001340
           10 ECR-ANVBONTF   PIC X.                                     00001350
           10 ECR-ANVBONTI   PIC X(3).                                  00001360
      * DATE ACQUISITION CRM                                            00001370
           10 ECR-ANVBONDL   COMP PIC S9(4).                            00001380
           10 ECR-ANVBONDF   PIC X.                                     00001390
           10 ECR-ANVBONDI   PIC X(6).                                  00001400
      * DATE ACQUISITION CRM 50                                         00001410
           10 ECR-ANVABODL   COMP PIC S9(4).                            00001420
           10 ECR-ANVABODF   PIC X.                                     00001430
           10 ECR-ANVABODI   PIC X(4).                                  00001440
      * CODE COMMANDE                                                   00001450
           10 ECR-XCDECL     COMP PIC S9(4).                            00001100
           10 ECR-XCDECF     PIC X.                                     00001110
           10 ECR-XCDECI     PIC X(9).                                  00001120
      * MESSAGE INFORMATIF                                              00001490
           10 ECR-XMSGILL    COMP PIC S9(4).                            00001140
           10 ECR-XMSGILF    PIC X.                                     00001150
           10 ECR-XMSGILI    PIC X(59).                                 00001160
      * MESSAGE D'ANOMALIE                                              00001530
           10 ECR-XMSGALL    COMP PIC S9(4).                            00001180
           10 ECR-XMSGALF    PIC X.                                     00001190
           10 ECR-XMSGALI    PIC X(79).                                 00001200
      ***************************************************************** 00001210
      * CARACTERISTIQUES / ANTECEDENTS                                  00001580
      ***************************************************************** 00001230
       02   FB04M0O REDEFINES FB04M0I.                                  00001280
           10 FILLER    PIC X(12).                                      00001250
           10 FILLER    PIC X(2).                                       00001260
           10 ECR-XTRMTRACA  PIC X.                                     00001270
           10 ECR-XTRMTRACO  PIC X(9).                                  00001280
      * LIBELLE APPLICATION                                             00001290
           10 FILLER    PIC X(2).                                       00001300
           10 ECR-XAPPLILA   PIC X.                                     00001310
           10 ECR-XAPPLILO   PIC X(30).                                 00001320
      * DATE DU JOUR JJ/MM/AA                                           00001330
           10 FILLER    PIC X(2).                                       00001340
           10 ECR-XJOURDA    PIC X.                                     00001350
           10 ECR-XJOURDO    PIC X(8).                                  00001360
      * NOM RACF DE L'OPERATEUR                                         00001370
           10 FILLER    PIC X(2).                                       00001380
           10 ECR-XRACFLA    PIC X.                                     00001390
           10 ECR-XRACFLO    PIC X(15).                                 00001400
      * HEURE HH/MM/SS                                                  00001410
           10 FILLER    PIC X(2).                                       00001420
           10 ECR-XHEUREDA   PIC X.                                     00001430
           10 ECR-XHEUREDO   PIC X(8).                                  00001440
      * CODE INTERMEDIAIRE                                              00001450
           10 FILLER    PIC X(2).                                       00001460
           10 ECR-GESCLIA    PIC X.                                     00001470
           10 ECR-GESCLIO    PIC X(11).                                 00001480
      * QUALITE DU CLIENT                                               00001490
           10 FILLER    PIC X(2).                                       00001500
           10 ECR-RAICA      PIC X.                                     00001510
           10 ECR-RAICO      PIC X(3).                                  00001520
      * NOM DU CLIENT                                                   00001530
           10 FILLER    PIC X(2).                                       00001540
           10 ECR-NOMCA      PIC X.                                     00001550
           10 ECR-NOMCO      PIC X(31).                                 00001560
      * CARTE GRISE NOM                                                 00002320
           10 FILLER    PIC X(2).                                       00002330
           10 ECR-VEHCGNCA   PIC X.                                     00002340
           10 ECR-VEHCGNCO   PIC X(20).                                 00002350
      * CARTE GRISE PRENOM                                              00002360
           10 FILLER    PIC X(2).                                       00002370
           10 ECR-VEHCGPCA   PIC X.                                     00002380
           10 ECR-VEHCGPCO   PIC X(20).                                 00002390
      * CARTE GRISE STATUT                                              00002400
           10 FILLER    PIC X(2).                                       00002410
           10 ECR-VEHCGSCA   PIC X.                                     00002420
           10 ECR-VEHCGSCO   PIC X(2).                                  00002430
      * CODE DU VEHICULE                                                00001930
           10 FILLER    PIC X(2).                                       00001580
           10 ECR-VEHCODCA   PIC X.                                     00001710
           10 ECR-VEHCODCO   PIC X(7).                                  00001720
      * TYPE DU VEHICULE                                                00001970
           10 FILLER    PIC X(2).                                       00001700
           10 ECR-VEHTYPCA   PIC X.                                     00001750
           10 ECR-VEHTYPCO   PIC X(3).                                  00001760
      * GENRE DU VEHICULE                                               00002010
           10 FILLER    PIC X(2).                                       00001740
           10 ECR-VEHGENCA   PIC X.                                     00002030
           10 ECR-VEHGENCO   PIC X.                                     00002040
      * MARQUE DU VEHICULE                                              00002050
           10 FILLER    PIC X(2).                                       00002060
           10 ECR-VEHMARLA   PIC X.                                     00001790
           10 ECR-VEHMARLO   PIC X(15).                                 00001800
      * MODELE DU VEHICULE                                              00002090
           10 FILLER    PIC X(2).                                       00001780
           10 ECR-VEHMODLA   PIC X.                                     00001830
           10 ECR-VEHMODLO   PIC X(30).                                 00001880
      * PUISSANCE/CYLINDREE                                             00002130
           10 FILLER    PIC X(2).                                       00001820
           10 ECR-VEHCYLNA   PIC X.                                     00001870
           10 ECR-VEHCYLNO   PIC X(5).                                  00001880
      * CODE USAGE DU VEHICULE                                          00002170
           10 FILLER    PIC X(2).                                       00001860
           10 ECR-VEHUSACA   PIC X.                                     00002190
           10 ECR-VEHUSACO   PIC X(3).                                  00002200
      * IMMATRICULATION                                                 00002210
           10 FILLER    PIC X(2).                                       00002220
           10 ECR-VEHIMMXA   PIC X.                                     00002230
           10 ECR-VEHIMMXO   PIC X(10).                                 00002240
      * 1ère MISE EN CIRCULATION                                        00002250
           10 FILLER    PIC X(2).                                       00001900
           10 ECR-VEHCIRDA   PIC X.                                     00001950
           10 ECR-VEHCIRDO   PIC X(6).                                  00001960
      * DATE D'ACQUISITION DU VEHICULE                                  00002560
           10 FILLER    PIC X(2).                                       00002570
           10 ECR-VEHACQDA   PIC X.                                     00002580
           10 ECR-VEHACQDO   PIC X(6).                                  00002590
      * VALEUR A NEUF                                                   00002290
           10 FILLER    PIC X(2).                                       00001940
           10 ECR-VEHVALMA   PIC X.                                     00002070
           10 ECR-VEHVALMO   PIC X(7).                                  00002080
      * GROUPE DU VEHICULE                                              00002490
           10 FILLER    PIC X(2).                                       00002500
           10 ECR-VEHGROCA   PIC X.                                     00002510
           10 ECR-VEHGROCO   PIC X(2).                                  00002520
      * CLASSE DU VEHICULE                                              00002530
           10 FILLER    PIC X(2).                                       00002540
           10 ECR-VEHCLACA   PIC X.                                     00002550
           10 ECR-VEHCLACO   PIC X.                                     00002560
      * NIVEAU DE PROTECTION VOL                                        00002330
           10 FILLER    PIC X(2).                                       00002060
           10 ECR-VEHPRTCA   PIC X.                                     00002350
           10 ECR-VEHPRTCO   PIC X.                                     00002360
      * TYPE DE GARAGE                                                  00002370
           10 FILLER    PIC X(2).                                       00002380
           10 ECR-GARCODCA   PIC X.                                     00002390
           10 ECR-GARCODCO   PIC X.                                     00002400
      * TYPE DE POSSESSION                                              00002410
           10 FILLER    PIC X(2).                                       00002420
           10 ECR-VEHPOSCA   PIC X.                                     00002110
           10 ECR-VEHPOSCO   PIC X.                                     00002120
      * INDICATEUR PERTES FINANCIERES                                   00002450
           10 FILLER    PIC X(2).                                       00002180
           10 ECR-VEHPEFCA   PIC X.                                     00002190
           10 ECR-VEHPEFCO   PIC X.                                     00002200
      * CODE POSTAL GARAGE HABITUEL                                     00002490
           10 FILLER    PIC X(2).                                       00002100
           10 ECR-GARCOPCA   PIC X.                                     00002230
           10 ECR-GARCOPCO   PIC X(5).                                  00002240
      * COMMUNE GARAGE HABITUEL                                         00002530
           10 FILLER    PIC X(2).                                       00002220
           10 ECR-GARVILLA   PIC X.                                     00002270
           10 ECR-GARVILLO   PIC X(30).                                 00002280
      * INDICATEUR VEHI SUPPLEMENTAIRE                                  00002570
           10 FILLER    PIC X(2).                                       00002260
           10 ECR-ANVREPCA   PIC X.                                     00002310
           10 ECR-ANVREPCO   PIC X.                                     00002320
      * FORMULE DE GARANTIES                                            00002610
           10 FILLER    PIC X(2).                                       00002300
           10 ECR-VEHFORCA   PIC X.                                     00002350
           10 ECR-VEHFORCO   PIC X(2).                                  00002360
      * NUMERO PRECEDENT CONTRAT                                        00002690
           10 FILLER    PIC X(2).                                       00002380
           10 ECR-ANVNUMXA   PIC X.                                     00002710
           10 ECR-ANVNUMXO   PIC X(15).                                 00002720
      * ANCIEN CONDUCTEUR                                               00002970
           10 FILLER    PIC X(2).                                       00002960
           10 ECR-ANVSOUSA   PIC X.                                     00002990
           10 ECR-ANVSOUSO   PIC X(4).                                  00003030
           10 FILLER    PIC X(2).                                       00003040
           10 ECR-ANVSOUNA   PIC X.                                     00003050
           10 ECR-ANVSOUNO   PIC X(3).                                  00003060
      * CODE CIE PRECEDENT CONTRAT                                      00002690
           10 FILLER    PIC X(2).                                       00002700
           10 ECR-ANVCIEXA   PIC X.                                     00002710
           10 ECR-ANVCIEXO   PIC X(3).                                  00002720
      * LIBELLE CIE PRECEDENT CONTRAT                                   00002730
           10 FILLER    PIC X(2).                                       00002740
           10 ECR-ANVCIELA   PIC X.                                     00002750
           10 ECR-ANVCIELO   PIC X(35).                                 00002760
      * ANCIENNETEE PRECEDENT CONTRAT                                   00002810
           10 FILLER    PIC X(2).                                       00002780
           10 ECR-ANVANCNA   PIC X.                                     00002830
           10 ECR-ANVANCNO   PIC X(2).                                  00002840
      * NB MOIS D'INTERRUPTION CONTRAT                                  00002850
           10 FILLER    PIC X(2).                                       00002860
           10 ECR-ANVINTNA   PIC X.                                     00002870
           10 ECR-ANVINTNO   PIC X(2).                                  00002880
      * DATE RESIL PRECEDENT CONTRAT                                    00002890
           10 FILLER    PIC X(2).                                       00002900
           10 ECR-ANVRESDA   PIC X.                                     00002910
           10 ECR-ANVRESDO   PIC X(6).                                  00002920
      * INDICATEUR DE SINISTRES                                         00002850
           10 FILLER    PIC X(2).                                       00002860
           10 ECR-SIVINDCA   PIC X.                                     00002870
           10 ECR-SIVINDCO   PIC X.                                     00002880
      * MOTIF DE RESIL ANCIEN CONTRAT                                   00003010
           10 FILLER    PIC X(2).                                       00003020
           10 ECR-ANVMTRCA   PIC X.                                     00003030
           10 ECR-ANVMTRCO   PIC X.                                     00003040
      * DERNIER CRM                                                     00002890
           10 FILLER    PIC X(2).                                       00002900
           10 ECR-ANVBONTA   PIC X.                                     00002910
           10 ECR-ANVBONTO   PIC X(3).                                  00002920
      * DATE ACQUISITION CRM                                            00002930
           10 FILLER    PIC X(2).                                       00002940
           10 ECR-ANVBONDA   PIC X.                                     00002950
           10 ECR-ANVBONDO   PIC X(6).                                  00002960
      * DATE ACQUISITION CRM 50                                         00002970
           10 FILLER    PIC X(2).                                       00002980
           10 ECR-ANVABODA   PIC X.                                     00002990
           10 ECR-ANVABODO   PIC X(4).                                  00003000
      * CODE COMMANDE                                                   00003010
           10 FILLER    PIC X(2).                                       00003020
           10 ECR-XCDECA     PIC X.                                     00002310
           10 ECR-XCDECO     PIC X(9).                                  00002320
      * MESSAGE INFORMATIF                                              00003050
           10 FILLER    PIC X(2).                                       00002340
           10 ECR-XMSGILA    PIC X.                                     00002350
           10 ECR-XMSGILO    PIC X(59).                                 00002360
      * MESSAGE D'ANOMALIE                                              00003090
           10 FILLER    PIC X(2).                                       00002380
           10 ECR-XMSGALA    PIC X.                                     00002390
           10 ECR-XMSGALO    PIC X(79).                                 00002400
