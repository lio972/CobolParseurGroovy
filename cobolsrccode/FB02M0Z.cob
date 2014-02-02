      ***************************************************************** 00000010
      * COORDONNEES CONDUCTEUR                                          00000020
      ***************************************************************** 00000030
       05   FB02M0I.                                                    00000040
           10 FILLER    PIC X(12).                                      00000050
      * CODE TRANSACTION                                                00000060
           10 ECR-XTRMTRACL  COMP PIC S9(4).                            00000070
           10 ECR-XTRMTRACF  PIC X.                                     00000080
           10 ECR-XTRMTRACI  PIC X(9).                                  00000090
      * LIBELLE DE L'APPLICATION                                        00000100
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
      * CODE AGENT + NUMERO DE CLIENT                                   00000260
           10 ECR-GESCLIL    COMP PIC S9(4).                            00000270
           10 ECR-GESCLIF    PIC X.                                     00000280
           10 ECR-GESCLII    PIC X(11).                                 00000290
      * TITRE CLIENT                                                    00000300
           10 ECR-RAICL      COMP PIC S9(4).                            00000310
           10 ECR-RAICF      PIC X.                                     00000320
           10 ECR-RAICI      PIC X(3).                                  00000330
      * NOM CLIENT                                                      00000340
           10 ECR-NOMCL      COMP PIC S9(4).                            00000350
           10 ECR-NOMCF      PIC X.                                     00000360
           10 ECR-NOMCI      PIC X(30).                                 00000370
      * IDENTIFIANT CONDUCTEUR                                          00000380
           10 ECR-PERNUMXL   COMP PIC S9(4).                            00000390
           10 ECR-PERNUMXF   PIC X.                                     00000400
           10 ECR-PERNUMXI   PIC X(6).                                  00000410
      * STATUT CONDUCTEUR                                               00000420
           10 ECR-PERSTACL   COMP PIC S9(4).                            00000430
           10 ECR-PERSTACF   PIC X.                                     00000440
           10 ECR-PERSTACI   PIC X(2).                                  00000450
      * NB DE SALARIES                                                  00000460
           10 ECR-PERSALNL   COMP PIC S9(4).                            00000470
           10 ECR-PERSALNF   PIC X.                                     00000480
           10 ECR-PERSALNI   PIC X(3).                                  00000490
      * TITRE CONDUCTEUR                                                00000500
           10 ECR-PERTITLL   COMP PIC S9(4).                            00000510
           10 ECR-PERTITLF   PIC X.                                     00000520
           10 ECR-PERTITLI   PIC X(3).                                  00000530
      * NOM CONDUCTEUR                                                  00000540
           10 ECR-PERNOMLL   COMP PIC S9(4).                            00000550
           10 ECR-PERNOMLF   PIC X.                                     00000560
           10 ECR-PERNOMLI   PIC X(20).                                 00000570
      * PERNOM CONDUCTEUR                                               00000580
           10 ECR-PERPRELL   COMP PIC S9(4).                            00000590
           10 ECR-PERPRELF   PIC X.                                     00000600
           10 ECR-PERPRELI   PIC X(20).                                 00000610
      * DATE DE NAISSANCE CONDUCTEUR                                    00000620
           10 ECR-PERNAIDL   COMP PIC S9(4).                            00000630
           10 ECR-PERNAIDF   PIC X.                                     00000640
           10 ECR-PERNAIDI   PIC X(8).                                  00000650
      * SEXE CONDUCTEUR                                                 00000660
           10 ECR-PERSEXCL   COMP PIC S9(4).                            00000630
           10 ECR-PERSEXCF   PIC X.                                     00000640
           10 ECR-PERSEXCI   PIC X.                                     00000650
      * NB D'ENFANTS A CHARGE                                           00000700
           10 ECR-PERENCNL   COMP PIC S9(4).                            00000710
           10 ECR-PERENCNF   PIC X.                                     00000720
           10 ECR-PERENCNI   PIC X(2).                                  00000730
      * SITUATION MATRIMONIALE                                          00000740
           10 ECR-PERMATCL   COMP PIC S9(4).                            00000750
           10 ECR-PERMATCF   PIC X.                                     00000760
           10 ECR-PERMATCI   PIC X.                                     00000770
      * CODE PROFESSION CONDUCTEUR                                      00000780
           10 ECR-PERPROCL   COMP PIC S9(4).                            00000710
           10 ECR-PERPROCF   PIC X.                                     00000720
           10 ECR-PERPROCI   PIC X(2).                                  00000730
      * LIBELLE PROFESSION CONDUCTEUR                                   00000820
           10 ECR-PERPROLL   COMP PIC S9(4).                            00000830
           10 ECR-PERPROLF   PIC X.                                     00000840
           10 ECR-PERPROLI   PIC X(30).                                 00000850
      * CATEGORIE DU PERMIS 1                                           00000860
           10 ECR-PRMTYPC1L  COMP PIC S9(4).                            00000830
           10 ECR-PRMTYPC1F  PIC X.                                     00000840
           10 ECR-PRMTYPC1I  PIC X(3).                                  00000850
      * DATE DU PERMIS 1                                                00000900
           10 ECR-PRMOBTD1L  COMP PIC S9(4).                            00000870
           10 ECR-PRMOBTD1F  PIC X.                                     00000880
           10 ECR-PRMOBTD1I  PIC X(8).                                  00000890
      * CATEGORIE DU PERMIS 2                                           00000940
           10 ECR-PRMTYPC2L  COMP PIC S9(4).                            00000910
           10 ECR-PRMTYPC2F  PIC X.                                     00000920
           10 ECR-PRMTYPC2I  PIC X(3).                                  00000930
      * DATE DU PERMIS 2                                                00000980
           10 ECR-PRMOBTD2L  COMP PIC S9(4).                            00000950
           10 ECR-PRMOBTD2F  PIC X.                                     00000960
           10 ECR-PRMOBTD2I  PIC X(8).                                  00000970
      * INDICATEUR CONDUITE ACCOMPAGNE                                  00001020
           10 ECR-PERCOACL   COMP PIC S9(4).                            00001150
           10 ECR-PERCOACF   PIC X.                                     00001160
           10 ECR-PERCOACI   PIC X.                                     00001170
      * NB MOIS RELEVE D'INFO                                           00001060
           10 ECR-ANPANCNL   COMP PIC S9(4).                            00001310
           10 ECR-ANPANCNF   PIC X.                                     00001320
           10 ECR-ANPANCNI   PIC X(2).                                  00001330
      * INDICATEUR RETRAIT PERMIS                                       00001140
           10 ECR-ANPINDCL   COMP PIC S9(4).                            00001380
           10 ECR-ANPINDCF   PIC X.                                     00001390
           10 ECR-ANPINDCI   PIC X.                                     00001400
      * MOTIF RETRAIT PERMIS                                            00001140
           10 ECR-ANPMOTLL   COMP PIC S9(4).                            00001150
           10 ECR-ANPMOTLF   PIC X.                                     00001160
           10 ECR-ANPMOTLI   PIC X(20).                                 00001170
      * NB JOURS RETRAIT PERMIS                                         00001180
           10 ECR-ANPNBJNL   COMP PIC S9(4).                            00001420
           10 ECR-ANPNBJNF   PIC X.                                     00001430
           10 ECR-ANPNBJNI   PIC X(3).                                  00001440
      * CODE COMMANDE                                                   00001260
           10 ECR-XCDECL     COMP PIC S9(4).                            00001580
           10 ECR-XCDECF     PIC X.                                     00001590
           10 ECR-XCDECI     PIC X(9).                                  00001600
      * MESSAGE INFORMATIF                                              00001300
           10 ECR-XMSGILL    COMP PIC S9(4).                            00000880
           10 ECR-XMSGILF    PIC X.                                     00000890
           10 ECR-XMSGILI    PIC X(59).                                 00000900
      * MESSAGE D'ANOMALIE                                              00001340
           10 ECR-XMSGALL    COMP PIC S9(4).                            00000920
           10 ECR-XMSGALF    PIC X.                                     00000930
           10 ECR-XMSGALI    PIC X(79).                                 00000940
      ***************************************************************** 00000950
      * COORDONNEES CONDUCTEUR                                          00001390
      ***************************************************************** 00000970
       05   FB02M0O REDEFINES FB02M0I.                                  00001720
           10 FILLER    PIC X(12).                                      00000990
      * CODE TRANSACTION                                                00001740
           10 FILLER    PIC X(2).                                       00001010
           10 ECR-XTRMTRACA  PIC X.                                     00001020
           10 ECR-XTRMTRACO  PIC X(9).                                  00001030
      * LIBELLE DE L'APPLICATION                                        00001780
           10 FILLER    PIC X(2).                                       00001050
           10 ECR-XAPPLILA   PIC X.                                     00001060
           10 ECR-XAPPLILO   PIC X(30).                                 00001070
      * DATE DU JOUR JJ/MM/AA                                           00001080
           10 FILLER    PIC X(2).                                       00001090
           10 ECR-XJOURDA    PIC X.                                     00001100
           10 ECR-XJOURDO    PIC X(8).                                  00001110
      * NOM RACF DE L'OPERATEUR                                         00001120
           10 FILLER    PIC X(2).                                       00001130
           10 ECR-XRACFLA    PIC X.                                     00001140
           10 ECR-XRACFLO    PIC X(15).                                 00001150
      * HEURE HH/MM/SS                                                  00001160
           10 FILLER    PIC X(2).                                       00001170
           10 ECR-XHEUREDA   PIC X.                                     00001180
           10 ECR-XHEUREDO   PIC X(8).                                  00001190
      * CODE AGENT + NUMERO DE CLIENT                                   00001630
           10 FILLER    PIC X(2).                                       00001210
           10 ECR-GESCLIA    PIC X.                                     00001960
           10 ECR-GESCLIO    PIC X(11).                                 00001970
      * TITRE CLIENT                                                    00001670
           10 FILLER    PIC X(2).                                       00001250
           10 ECR-RAICA      PIC X.                                     00001260
           10 ECR-RAICO      PIC X(3).                                  00001270
      * NOM CLIENT                                                      00001710
           10 FILLER    PIC X(2).                                       00001290
           10 ECR-NOMCA      PIC X.                                     00001300
           10 ECR-NOMCO      PIC X(30).                                 00002050
      * IDENTIFIANT CONDUCTEUR                                          00001750
           10 FILLER    PIC X(2).                                       00001330
           10 ECR-PERNUMXA   PIC X.                                     00002080
           10 ECR-PERNUMXO   PIC X(6).                                  00002090
      * STATUT CONDUCTEUR                                               00001790
           10 FILLER    PIC X(2).                                       00001370
           10 ECR-PERSTACA   PIC X.                                     00002120
           10 ECR-PERSTACO   PIC X(2).                                  00002130
      * NB DE SALARIES                                                  00001830
           10 FILLER    PIC X(2).                                       00001470
           10 ECR-PERSALNA   PIC X.                                     00002160
           10 ECR-PERSALNO   PIC X(3).                                  00002170
      * TITRE CONDUCTEUR                                                00001870
           10 FILLER    PIC X(2).                                       00001500
           10 ECR-PERTITLA   PIC X.                                     00002200
           10 ECR-PERTITLO   PIC X(3).                                  00002210
      * NOM CONDUCTEUR                                                  00001910
           10 FILLER    PIC X(2).                                       00001530
           10 ECR-PERNOMLA   PIC X.                                     00002240
           10 ECR-PERNOMLO   PIC X(20).                                 00002250
      * PERNOM CONDUCTEUR                                               00001950
           10 FILLER    PIC X(2).                                       00001560
           10 ECR-PERPRELA   PIC X.                                     00002280
           10 ECR-PERPRELO   PIC X(20).                                 00002290
      * DATE DE NAISSANCE CONDUCTEUR                                    00001990
           10 FILLER    PIC X(2).                                       00002310
           10 ECR-PERNAIDA   PIC X.                                     00002010
           10 ECR-PERNAIDO   PIC X(8).                                  00002020
      * SEXE CONDUCTEUR                                                 00002030
           10 FILLER    PIC X(2).                                       00002040
           10 ECR-PERSEXCA   PIC X.                                     00002320
           10 ECR-PERSEXCO   PIC X.                                     00002330
      * NB D'ENFANTS A CHARGE                                           00002070
           10 FILLER    PIC X(2).                                       00002350
           10 ECR-PERENCNA   PIC X.                                     00002090
           10 ECR-PERENCNO   PIC X(2).                                  00002100
      * SITUATION MATRIMONIALE                                          00002110
           10 FILLER    PIC X(2).                                       00002120
           10 ECR-PERMATCA   PIC X.                                     00002130
           10 ECR-PERMATCO   PIC X.                                     00002140
      * CODE PROFESSION CONDUCTEUR                                      00002150
           10 FILLER    PIC X(2).                                       00002390
           10 ECR-PERPROCA   PIC X.                                     00002400
           10 ECR-PERPROCO   PIC X(2).                                  00002410
      * LIBELLE PROFESSION CONDUCTEUR                                   00002190
           10 FILLER    PIC X(2).                                       00002430
           10 ECR-PERPROLA   PIC X.                                     00002210
           10 ECR-PERPROLO   PIC X(30).                                 00002220
      * CATEGORIE DU PERMIS 1                                           00002230
           10 FILLER    PIC X(2).                                       00001590
           10 ECR-PRMTYPC1A  PIC X.                                     00002520
           10 ECR-PRMTYPC1O  PIC X(3).                                  00002530
      * DATE DU PERMIS 1                                                00002270
           10 FILLER    PIC X(2).                                       00002550
           10 ECR-PRMOBTD1A  PIC X.                                     00002560
           10 ECR-PRMOBTD1O  PIC X(8).                                  00002570
      * CATEGORIE DU PERMIS 2                                           00002310
           10 FILLER    PIC X(2).                                       00002590
           10 ECR-PRMTYPC2A  PIC X.                                     00002600
           10 ECR-PRMTYPC2O  PIC X(3).                                  00002610
      * DATE DU PERMIS 2                                                00002350
           10 FILLER    PIC X(2).                                       00002630
           10 ECR-PRMOBTD2A  PIC X.                                     00002640
           10 ECR-PRMOBTD2O  PIC X(8).                                  00002650
      * INDICATEUR CONDUITE ACCOMPAGNE                                  00002390
           10 FILLER    PIC X(2).                                       00002830
           10 ECR-PERCOACA   PIC X.                                     00002840
           10 ECR-PERCOACO   PIC X.                                     00002850
      * NB MOIS RELEVE D'INFO                                           00002430
           10 FILLER    PIC X(2).                                       00002870
           10 ECR-ANPANCNA   PIC X.                                     00003000
           10 ECR-ANPANCNO   PIC X(2).                                  00003010
      * INDICATEUR RETRAIT PERMIS                                       00002510
           10 FILLER    PIC X(2).                                       00003020
           10 ECR-ANPINDCA   PIC X.                                     00003070
           10 ECR-ANPINDCO   PIC X.                                     00003080
      * MOTIF RETRAIT PERMIS                                            00002470
           10 FILLER    PIC X(2).                                       00002480
           10 ECR-ANPMOTLA   PIC X.                                     00002490
           10 ECR-ANPMOTLO   PIC X(20).                                 00002500
      * NB JOURS RETRAIT PERMIS                                         00002550
           10 FILLER    PIC X(2).                                       00003100
           10 ECR-ANPNBJNA   PIC X.                                     00003110
           10 ECR-ANPNBJNO   PIC X(3).                                  00003120
      * CODE COMMANDE                                                   00002630
           10 FILLER    PIC X(2).                                       00001720
           10 ECR-XCDECA     PIC X.                                     00003270
           10 ECR-XCDECO     PIC X(9).                                  00003280
      * MESSAGE INFORMATIF                                              00002670
           10 FILLER    PIC X(2).                                       00001820
           10 ECR-XMSGILA    PIC X.                                     00001830
           10 ECR-XMSGILO    PIC X(59).                                 00001840
      * MESSAGE D'ANOMALIE                                              00002710
           10 FILLER    PIC X(2).                                       00001860
           10 ECR-XMSGALA    PIC X.                                     00001870
           10 ECR-XMSGALO    PIC X(79).                                 00001880
