       ID DIVISION.                                                     00000100
       PROGRAM-ID. PPGEMB17.                                            00000200
      **************************************************************** X00000300
      *                                                                 00000700
      *  OBJET  : PROGRAMME MAITRE BATCH ENVOYE LES AF/IF VERS CIC      00000500
      *           EN VUE DE LEUR EDITION                                00000600
      *  DCT    : DCT_P_GE_S.                                           00000700
      *                                                                 00001100
      ******************************************************************00001200
      *          H I S T O R I Q U E   D U   C O M P O S A N T          00001300
      ******************************************************************00001400
      * DATE       STE/PERS         VERSION   NOTES                     00001500
      *                                                                 00001600
      * 15.09.2009 INFOTEL/LDE      M000      CREATION.                 00001400
      * 23.11.2009 INFOTEL/LDE      M000      RECOMPILATION EN MODE     00001400
      *                                       NORMAL                    00001800
      * 08.12.2009 INFOTEL/LDE      M001      DI121112 RECUPERATION     00001400
      *                      LIGNE FISCALE POUR ENVOI PONCTUEL TYPE AF
      * 09.12.2009 INFOTEL/LDE      M002      CORRECTION SUITE TU M001  00001400
      * 05.01.2010 SYSTALI/NNI      M003      RECOMPILATION BREF V021   00001400
      * 13.01.2010 INFOTEL/LDE      M004      DI122874 DSNTIAUL         00001400
      * 26.02.2010 INFOTEL/LDE      M005      DI127303                  00001400
      *                       (RECUPERATION V03 EN F AU LIEU V04)       00002000
      *                       + APPORT CORRECTIF DI 127303              00002000
      * 03.03.2010 STERIA/FPUY....  M006      AJOUT CODE QUARTIER SV12
      * 05.03.2010 SysMig/B.Delprat M007      DI127846
      *                                                                 00002000
      * 05.03.2010 SysMig/B.Delprat M008      DI124579 & 124580         00001400
      *                                       AJOUT BLOC UADRESSE
      * 05.03.2010 SysMig/B.Delprat M007      DI127846
      * 11.03.2010 INFOTEL/LDE      M009      correction DI124579 etc..
      *                                       correction UADRESSE
      * 14.03.2010 INFOTEL/LDE      M010      Modif R04                 00001400
      * 31.03.2010 INFOTEL/LDE      M011      MAC contexte P[IF]        00001400
      * 06.04.2010 INFOTEL/LDE      M012      RECOMPIL EN D             00001400
      * 06.04.2010 INFOTEL/LDE      M014      DI 120142 boucle suite ano00001400
      * 07.04.2010 INFOTEL/LDE      M015-16   DI 120142 boucle suite ano00001400
      * 09.04.2010 INFOTEL/LDE      M017      DI 120142 boucle suite ano00001400
      * 09.04.2010 INFOTEL/LDE      M018      gestion relat mandataire  00001400
      * 19.05.2010 INFOTEL/LDE      M019      DI 131845                 00001400
      * JJ.MM.SSAA ....../........  M009
      *                                                                 00002000
      ******************************************************************00002100
       ENVIRONMENT DIVISION.                                            00010500
      ******************************************************************00010600
       CONFIGURATION SECTION.                                           00010800
       SOURCE-COMPUTER. IMB-370.                                        00010900
       OBJECT-COMPUTER. IBM-370.                                        00011000
       SPECIAL-NAMES.                                                   00011100
           DECIMAL-POINT IS COMMA.                                      00011100
                                                                        00011100
                                                                        00002500
        INPUT-OUTPUT SECTION.                                           00002600
        FILE-CONTROL.                                                   00002700
      *--- FICHIER EN ENTREE                                            00002800
      *--- LISTE DES INSTITUTIONS A EXCLURE                             00002900
              SELECT  FCGEEDF1   ASSIGN  FCGEEDF1                       00003000
                                 FILE STATUS IS  FS-FCGEEDF1-STATUS.    00003100
                                                                        00003200
              SELECT  FC99FIS1   ASSIGN  FC99FIS1                       00003210
                                 FILE STATUS IS  FS-FC99FIS1-STATUS.    00003220
                                                                        00003230
      ******************************************************************00011200
       DATA DIVISION.                                                   00011300
      ******************************************************************00011400
       FILE SECTION.                                                    00003800
       FD  FCGEEDF1                                                     00003900
           RECORDING   F                                                00004000
           LABEL  RECORD  STANDARD.                                     00004100
MAC01  01  ENRG-FCGEEDF1          PIC X(221).                           00004200
                                                                        00004300
       FD  FC99FIS1                                                     00004310
           RECORDING   F                                                00004320
           LABEL  RECORD  STANDARD.                                     00004330
M004   01  ENRG-FC99FIS1          PIC X(587).                           00004340
                                                                        00004350
                                                                        00004400
       WORKING-STORAGE SECTION.                                         00011500
      *------------------------                                         00011600
      * VARIABLE DE DEBUT DE WORKING                                    00011700
       01 FILLER          PIC X(24) VALUE '** DEBUT W-S PPGEMB17 **'.   00011800
      *================================================================*00004900
      *   GESTION DES FICHIERS                                         *00005000
      *================================================================*00005100
       01 WS-ID-TECH-INDV-F02      PIC X(009).                          00005200
      *--- GESTION DES-STATUS-DES FICHIERS                              00005300
       01 FS-FCGEEDF1-STATUS   PIC X(2).                                00005400
               88     OK-FCGEEDF1      VALUE '00'.                      00005500
               88     EOF-FCGEEDF1     VALUE '10'.                      00005600
                                                                        00005700
       01 FS-FC99FIS1-STATUS   PIC X(2).                                00005710
               88     OK-FC99FIS1      VALUE '00'.                      00005720
               88     EOF-FC99FIS1     VALUE '10'.                      00005730
                                                                        00005740
                                                                        00005800
      ******************************************************************00011900
      *        DESCRIPTION DES ZONES DE TRAVAIL DU SERVICE              00012000
      ******************************************************************00012100
                                                                        00012200
      * VARIABLE DE TRAVAIL POUR LES APPELS AUX SERVICES                00012300
       01 WS-ID-VERS-APP            PIC 99V99 VALUE 1.                  00012400
       01 WS-DT-FONC                PIC X(10).                          00012500
       01 WS-TI-TRT                 PIC X(08).                          00012600
       01 WS-ID-UTIL.                                                   00012700
          05 WS-ID-UTIL-PRE         PIC X(08).                          00012800
          05 FILLER                 PIC X(01) VALUE SPACE.              00012900
          05 WS-ID-UTIL-SUF         PIC X(08).                          00013000
          05 FILLER                 PIC X(03) VALUE SPACES.             00013100
                                                                        00013200
      * VARIABLES DE FORMATAGE DE DATES                                 00013300
       01 WS-GR-DT-AFF.                                                 00013400
           05  WS-DT-AA             PIC  X(02).                         00013500
           05  WS-DT-MM             PIC  X(02).                         00013600
           05  WS-DT-JJ             PIC  X(02).                         00013700
           05  FILLER               PIC  X(02).                         00013800
                                                                        00013900
       01 WS-GR-DT-AFF-ANG.                                             00014000
           05  WS-DT-SS             PIC  X(02) VALUE '20'.              00014100
           05  WS-DT-AA             PIC  X(02).                         00014200
           05  WS-LB-SEP            PIC  X(01) VALUE '-'.               00014300
           05  WS-DT-MM             PIC  X(02).                         00014400
           05  WS-LB-SEP            PIC  X(01) VALUE '-'.               00014500
           05  WS-DT-JJ             PIC  X(02).                         00014600
                                                                        00014700
      * VARIABLES DE FORMATAGE DE L'HEURE                               00014800
       01 WS-GR-TI.                                                     00014900
           05  WS-TI-HH             PIC  X(02).                         00015000
           05  WS-TI-MIN            PIC  X(02).                         00015100
           05  WS-TI-SEC            PIC  X(02).                         00015200
           05  WS-TI-CEN            PIC  X(02).                         00015300
                                                                        00015400
       01 WS-GR-TI-AFF.                                                 00015500
           05  WS-TI-HH             PIC  X(02).                         00015600
           05  WS-LB-SEP            PIC  X(01) VALUE '.'.               00015700
           05  WS-TI-MIN            PIC  X(02).                         00015800
           05  WS-LB-SEP            PIC  X(01) VALUE '.'.               00015900
           05  WS-TI-SEC            PIC  X(02).                         00016000
           05  WS-LB-SEP            PIC  X(01) VALUE '.'.               00016100
           05  WS-TI-CEN            PIC  X(02).                         00016200
           05  WS-TI-MIL            PIC  X(04) VALUE '0000'.            00016300
                                                                        00016400
      * VARIABLE DE FORMATAGE DU CHAMP TS-CREA-ANO                      00016500
       01 WS-GR-TS-CREA.                                                00016600
           05  WS-DT-CREA           PIC  X(10).                         00016700
           05  WS-LB-SEP            PIC  X(01) VALUE '-'.               00016800
           05  WS-TI-CREA           PIC  X(15).                         00016900
       01 WS-GR-TS-MAJ.                                                 00017000
           05  WS-DT-MAJ            PIC  X(10) VALUE '0001-01-01'.      00017100
           05  WS-LB-SEP            PIC  X(01) VALUE '-'.               00017200
           05  WS-TI-MAJ            PIC  X(15) VALUE '01.00.00.000000'. 00017300
                                                                        00017400
      * VARIABLE DE SAUVEGARDE POUR LA GESTION DES REPRISES             00017500
      * ET LES COMPTEURS                                                00017600
       01 WS-CO-STUT-STEP           PIC X(001).                         00017700
       01 WS-CO-STUT-SAUV           PIC X(001).                         00017800
       01 WS-ID-CLE-RPRI            PIC X(255) VALUE SPACE.             00011900
       01 WS-ID-CLE-RPRI-SAUV       PIC X(255) VALUE SPACE.             00012000
       01 WS-ID-CLE-ANO             PIC X(255).                         00018100
       01 WS-ID-CLE-ANO-SAUV        PIC X(255).                         00018200
       01 WS-NB-ANO                 PIC 9(009).                         00018300
       01 WS-NB-ANO-SAUV            PIC 9(009).                         00018400
M004   01 WS-NB-DPR                 PIC 9(004) VALUE ZERO.              00018400
M004   01 WS-NB-DIVISEUR            PIC 9(004) VALUE 11.                00018400
M004   01 WS-NB-RESULTAT            PIC 9(004) VALUE ZERO.              00018400
M004   01 WS-NB-RESTE               PIC 9(004) VALUE ZERO.              00018400
      * COMPTEUR DU NB D'ANO DU PASSAGE DU PROG                         00018500
       01 WS-NB-ANO-PASS            PIC 9(009).                         00018600
       01 WS-CO-RET                 PIC 9(004).                         00018700
          88 CO-RET-OK              VALUE 0.                            00018900
          88 CO-RET-KO              VALUE 8.                            00019000
       01 WS-CO-RET-SAUV            PIC 9(004).                         00019100
      * COMPTEUR DU NOMBRE D'OCCURRENCES TOTAL TRAITEES PAR LE PROGRAMME00019200
       01 WS-NB-OCC                 PIC 9(009).                         00019300
       01 WS-NB-OCC-SAUV            PIC 9(009).                         00019400
       01 WS-NB-ROLL                PIC 9(009).                         00019500
       01 WS-NB-ROLL-SAUV           PIC 9(009).                         00019600
      * COMPTEUR DU NOMBRE D'OCCURRENCE ENTRE DEUX INTERVALLES DE COMMIT00019700
       01 WS-NB-OCC-COM             PIC 9(009).                         00019800
                                                                        00019900
      * INDICATEUR DU TYPE DE MISE A JOUR DE LA TABLE DES RESTARTS      00020000
       01 WS-IN-MAJ-TAB             PIC X(01).                          00020100
           88 CODE-RETOUR           VALUE '1'.                          00020200
           88 CODE-STATUT           VALUE '2'.                          00020300
           88 IDENTIFIANTS          VALUE '3'.                          00020400
           88 IDENTIFIANT-REP       VALUE '4'.                          00020500
           88 IDENTIFIANT-ANO       VALUE '5'.                          00020600
           88 DERNIERE-OCC          VALUE '6'.                          00020700
                                                                        00020800
      * ZONE DE SAUVEGARDE DE LA BORNE INFERIEURE DES OCCURRENCES       00020900
      * A TRAITER                                                       00021000
       01 WS-ID-BOR-INF             PIC X(255).                         00021100
                                                                        00021200
      * INDICATEUR DE PRESENCE D'AU MOINS UNE OCCURRENCE A TRAITER      00021300
       01 WS-IN-OCC                 PIC  X(01).                         00021400
           88 OCC-A-TRAITER         VALUE 'O'.                          00021500
           88 PAS-OCC-A-TRAITER     VALUE 'N'.                          00021600
                                                                        00021700
      * IDENTIFIANT DE L'UNITE FONCTIONNELLE EN COURS DE TRAITEMENT     00021800
       01 WS-ID-UNIT-FONC-LUE.                                          00022100
          05 WS-CO-REG-LU             PIC X     VALUE SPACE.
          05 WS-NO-INST-LU            PIC X(03) VALUE SPACE.
          05 WS-ID-TECH-INDV-LU       PIC 9(09) VALUE 0.
      *   05 WS-ID-TECH-ENV           PIC 9(09) VALUE 0.

M014+ * IDENTIFIANT DE L'UNITE FONCTIONNELLE PRECEDENT                  00021800
M014+  01 WS-ID-CLE-PREC.                                               00022100
M014+     05 WS-CO-REG-CLE-PREC       PIC X     VALUE SPACE.
M014+     05 WS-NO-INST-CLE-PREC      PIC X(03) VALUE SPACE.
M014+     05 WS-ID-INDV-CLE-PREC      PIC 9(09) VALUE 0.
M014+

M014+ * IDENTIFIANT DE L'UNITE FONCTIONNELLE PRECEDENT                  00021800
M014+  01 WS-ID-UNIT-FONC-PREC.                                         00022100
M014+     05 WS-CO-REG-LU-PREC        PIC X     VALUE 'X'.
M014+     05 WS-NO-INST-PREC          PIC X(03) VALUE '999'.
M014+     05 WS-ID-TECH-INDV-PREC     PIC 9(09) VALUE 999999999.
M014+

       01 WS-ID-TECH-ENV-LUE9         PIC 9(9) VALUE 0.
                                                                        00022400
      * ZONE DE TRAVAIL POUR LES CALL DYNAMIQUES                        00022600
       01 DSNTIAR                   PIC  X(08)  VALUE 'DSNTIAR'.        00022700
       01 PP00SV02                  PIC  X(08)  VALUE 'PP00SV02'.       00022800
       01 PP00SV04                  PIC  X(08)  VALUE 'PP00SV04'.       00022900
       01 PP00SV05                  PIC  X(08)  VALUE 'PP00SV05'.       00023000
       01 PP00SV09                  PIC  X(08)  VALUE 'PP00SV09'.       00023100
       01 PP00SV10                  PIC  X(08)  VALUE 'PP00SV10'.       00023200
       01 PP00SV11                  PIC  X(08)  VALUE 'PP00SV11'.       00023300
       01 PP00SV12                  PIC  X(08)  VALUE 'PP00SV12'.       00023400
       01 PP00SV16                  PIC  X(08)  VALUE 'PP00SV16'.       00023500
       01 PP00SV17                  PIC  X(08)  VALUE 'PP00SV17'.       00023600
       01 PP00SV21                  PIC  X(08)  VALUE 'PP00SV21'.       00023700
       01 PP00SV25                  PIC  X(08)  VALUE 'PP00SV25'.       00023800
       01 PP00SVP2                  PIC  X(08)  VALUE 'PP00SVP2'.       00023900
       01 PP00SVCA                  PIC  X(08)  VALUE 'PP00SVCA'.       00024000
       01 PP00SV31                  PIC  X(08)  VALUE 'PP00SV31'.       00024100
       01 PP00SV30                  PIC  X(08)  VALUE 'PP00SV30'.       00024100
       01 PP00SV32                  PIC  X(08)  VALUE 'PP00SV32'.       00024200
     **ACCESSEUR POUR LE TPSPPAR
       01 PPSPSV07                  PIC  X(08)  VALUE 'PPSPSV07'.       00018200
     **APPEL DE L'ACCESSEUR PPSPAM01 POUR LA TABLE TPSPTRC
       01 PPSPAM01                  PIC  X(08)  VALUE 'PPSPAM01'.
       01 PPGEAM20                  PIC  X(08)  VALUE 'PPGEAM20'.
       01 PPGAAL18                  PIC  X(08)  VALUE 'PPGAAL18'.
       01 PPSPSV12                  PIC  X(08)  VALUE 'PPSPSV12'.
       01 PPSPSV14                  PIC  X(08)  VALUE 'PPSPSV14'.
       01 PPGEAL20                  PIC  X(08)  VALUE 'PPGEAL20'.
      *01 PP00SV58                  PIC  X(08)  VALUE 'PP00SV58'.
      *01 PP00SV56                  PIC  X(08)  VALUE 'PP00SV56'.
       01 PPGEAL21                  PIC  X(08)  VALUE 'PPGEAL21'.
      *--RESTITUER INDIVIDU
       01 PP00SV37                  PIC  X(08)  VALUE 'PP00SV37'.
M019  *--RESTITUER DPR
M019   01 PPGAAL01                  PIC  X(08)  VALUE 'PPGAAL01'.

                                                                        00025600
      * ZONE DE COMMUNICATION POUR DISPLAY                              00025700
       01 LK-GR-PPGEMB17.                                               00019300
           COPY CP00LK01 REPLACING ==:P:== BY PPGEMB17.                 00025900
                                                                        00026000
      * ZONE DE COMMUNICATION DU SERVICE DE GESTION DES TRANSACTIONS    00026100
       01 LK-CP00SV02.                                                  00026200
           COPY CP00LK01 REPLACING ==:P:== BY CP00SV02.                 00026300
           COPY CP00SV02 REPLACING ==:P:== BY CP00SV02.                 00026400
                                                                        00026500
      * ZONE DE COMMUNICATION DU SERVICE DES COMPTES-RENDUS TECHNIQUES  00026600
       01 LK-CP00SV04.                                                  00026700
           COPY CP00LK01 REPLACING ==:P:== BY CP00SV04.                 00026800
           COPY CP00SV04 REPLACING ==:P:== BY CP00SV04.                 00026900
                                                                        00027000
      * ZONE DE COMMUNICATION DU SERVICE DE GESTION DES REPRISES        00027100
       01 LK-CP00SV05.                                                  00027200
           COPY CP00LK01 REPLACING ==:P:== BY CP00SV05.                 00027300
           COPY CP00SV05 REPLACING ==:P:== BY CP00SV05.                 00027400
                                                                        00027500
      * ZONE DE COMMUNICATION DU SERVICE DE RECHERCHE DES PARAMETRES MVS00027600
       01 LK-CP00SV09.                                                  00027700
           COPY CP00LK01 REPLACING ==:P:== BY CP00SV09.                 00027800
           COPY CP00SV09 REPLACING ==:P:== BY CP00SV09.                 00027900
                                                                        00028000
      * ZONE DE COMMUNICATION DU SERVICE DE RECHERCHE DES PARAMETRES    00028100
      *  DE COMMIT                                                      00028200
       01 LK-CP00SV10.                                                  00028300
           COPY CP00LK01 REPLACING ==:P:== BY CP00SV10.                 00028400
           COPY CP00SV10 REPLACING ==:P:== BY CP00SV10.                 00028500
                                                                        00028600
      * ZONE DE COMMUNICATION DU SERVICE DE RECHERCHE DES DONNEES DE LA 00028700
      *  TABLE DE SURVEILLANCE D'EXECUTION                              00028800
       01 LK-CP00SV11.                                                  00028900
           COPY CP00LK01 REPLACING ==:P:== BY CP00SV11.                 00029000
           COPY CP00SV11 REPLACING ==:P:== BY CP00SV11.                 00029100
                                                                        00029200
      * ZONE DE COMMUNICATION DU SERVICE DE RECHERCHE DES PLAGES        00029300
      *  D'IDENTIFIANTS                                                 00029400
       01 LK-CP00SV12.                                                  00029500
           COPY CP00LK01 REPLACING ==:P:== BY CP00SV12.                 00029600
           COPY CP00SV12 REPLACING ==:P:== BY CP00SV12.                 00029700
                                                                        00029800
      * ZONE DE COMMUNICATION DU SERVICE D'INSERTION DANS LA TABLE DES  00029900
      *  ANOMALIES TECHNIQUES                                           00030000
       01 LK-CP00SV16.                                                  00030100
           COPY CP00LK01 REPLACING ==:P:== BY CP00SV16.                 00030200
           COPY CP00SV16 REPLACING ==:P:== BY CP00SV16.                 00030300
                                                                        00030400
      * ZONE DE COMMUNICATION DU SERVICE DE MEMORISATION ET RESTITUTION 00030500
      *  DES CODES APPLICATION ET FONCTION                              00030600
       01 LK-CP00SVCA.                                                  00030700
           COPY CP00LK01 REPLACING ==:P:== BY CP00SVCA.                 00030800
           COPY CP00SVCA REPLACING ==:P:== BY CP00SVCA.                 00030900
                                                                        00031000
      * ZONE DE COMMUNICATION DU SERVICE D'ACCES A LA TABLE DES MESSAGES00031100
      *  D'ERREUR                                                       00031200
       01 LK-CP00SV17.                                                  00031300
           COPY CP00LK01 REPLACING ==:P:== BY CP00SV17.                 00031400
           COPY CP00SV17 REPLACING ==:P:== BY CP00SV17.                 00031500
                                                                        00031600
      * ZONE DE COMMUNICATION DU SERVICE DE TRACAGE GENERAL             00031700
       01 LK-CP00SV21.                                                  00031800
           COPY CP00LK01 REPLACING ==:P:== BY CP00SV21.                 00031900
           COPY CP00SV21 REPLACING ==:P:== BY CP00SV21.                 00032000
                                                                        00032100
      * ZONE DE COMMUNICATION DU SERVICE DE GESTION DU CONTEXTE         00032200
       01 LK-CP00SV25.                                                  00032300
           COPY CP00LK01 REPLACING ==:P:== BY CP00SV25.                 00032400
           COPY CP00SV25 REPLACING ==:P:== BY CP00SV25.                 00032500
                                                                        00032600
      * ZONE DE COMMUNICATION DU SERVICE PP00SV30 GESTION DES CHRONOS
       01 LK-CP00SV30.
          COPY CP00LK01 REPLACING ==:P:== BY CP00SV30.
          COPY CP00SV30 REPLACING ==:P:== BY CP00SV30.

      * ZONE DE COMMUNICATION DU SERVICE PRESTATIONS DE GESTION DES     00032700
      *  ERREURS                                                        00032800
       01 LK-CP00SV31.                                                  00032900
           COPY CP00LK01 REPLACING ==:P:== BY CP00SV31.                 00033000
           COPY CP00SV31 REPLACING ==:P:== BY CP00SV31-FONC.            00033100
                                                                        00033200
      * ZONE DE COMMUNICATION DU SERVICE PRESTATIONS DE GESTION DES     00033300
      *  ANOMALIES FONCTIONNELLES                                       00033400
       01 LK-CP00SV32.                                                  00033500
           COPY CP00LK01 REPLACING ==:P:== BY CP00SV32.                 00033600
           COPY CP00SV32 REPLACING ==:P:== BY CP00SV32.                 00033700
                                                                        00033800
      *-- APPEL ENCAPSULATEUR PRINSV03 RESTITUER INDIVIDU
       01 LK-CP00SV37.
          COPY CP00LK01 REPLACING ==:P:== BY CP00SV37.
          COPY CP00SV37.

      * ZONE DE COMMUNICATION DU SERVICE DE GESTION DES DATES PP00SVP2  00033900
       01 LK-CP00SVP2.                                                  00034000
           COPY CP00LK01 REPLACING ==:P:== BY CP00SVP2.                 00034100
           COPY CP00SVP2 REPLACING ==:P:== BY CP00SVP2.                 00034200
                                                                        00034300
      * ZONE DE COMMUNICATION DU SERVICE DE RECHERCHE DES OCCURRENCES   00034400
       01 LK-CPGEAL20.                                                  00028000
           COPY CP00LK01 REPLACING ==:P:== BY CPGEAL20.                 00028100
           COPY CPGEAL20 REPLACING ==:P:== BY CPGEAL20.                 00028200

      * ZONE DE COMMUNICATION DU SERVICE DE RECHERCHE DES ULF (TPGEEDF)
       01 LK-CPGEAM20.
           COPY CP00LK01 REPLACING ==:P:== BY CPGEAM20.
           COPY CPGEAM20 REPLACING ==:P:== BY CPGEAM20.

      * ZONE DE COMMUNICATION DE RECHERCHE DE LIF (TPGELIF)
       01 LK-CPGEAL21.
           COPY CP00LK01 REPLACING ==:P:== BY CPGEAL21.
           COPY CPGEAL21 REPLACING ==:P:== BY CPGEAL21.

      * ZONE DE COMMUNICATION DU SERVICE DE RECHERCHE DES OCCURRENCES
       01 LK-CPGAAL18.
           COPY CP00LK01 REPLACING ==:P:== BY CPGAAL18.
           COPY CPGAAL18 REPLACING ==:P:== BY CPGAAL18.

M019+ * ZONE DE COMMUNICATION DE L'ACCESSEUR DE LA TABLE TPGADPR
M019+  01 LK-CPGAAL01.
M019+     COPY CP00LK01 REPLACING ==:P:== BY CPGAAL01.
M019+     COPY CPGAAL01.


      * ZONE DE COMMUNICATION DU SERVICE RESTITIER INSTITUTIONS
      *01 LK-CP00SV58.
      *    COPY CP00LK01 REPLACING ==:P:== BY CP00SV58.
      *    COPY CP00SV58 REPLACING ==:P:== BY CP00SV58.

      * ZONE DE COMMUNICATION DU SERVICE RESTITIER ADRESSE INSTITUTIONS
      *01 LK-CP00SV56.
      *    COPY CP00LK01 REPLACING ==:P:== BY CP00SV56.
      *    COPY CP00SV56 REPLACING ==:P:== BY CP00SV56.

                                                                        00034800
      * ZONE DE COMMUNICATION DU SERVICE DE RECHERCHE DE PARAM TRES     00028400
       01 LK-CPSP07I1.                                                  00028500
           COPY CP00LK01 REPLACING ==:P:== BY CPSP07I1.                 00028600
           COPY CPSP07I1.                                               00028700
                                                                        00035300
      * ZONE DE COMMUNICATION POUR ALIMENTATION DES SERVICES D'ERREUR   00029900
       01 WS-GR-ERR.                                                    00030000
           COPY CP00LK01 REPLACING ==:P:== BY ERR.                      00030100

      * ZONE DE COMMUNICATION DE L'ACCESSEUR DE LA TABLE TPSPTRC
       01 LK-CPSPAM01.
          COPY CP00LK01 REPLACING ==:P:== BY CPSPAM01.
          COPY CPSPAM01.

      * ZONE DE COMMUNICATION POUR APPEL AU SERVICE SV12  METH-C1
       01 LK-CPSP12C1.
          COPY CP00LK01 REPLACING ==:P:== BY CPSP12C1.
          COPY CPSP12C1.

      * ZONE DE COMMUNICATION POUR APPEL AU SERVICE SV14 METH-M1
       01 LK-CPSP14M1.
          COPY CP00LK01 REPLACING ==:P:== BY CPSP14M1.
          05 LK-VA-FONC            PIC X(7000).
      *----PALLOCATAIRE
          05 LK-CPSP1408           REDEFINES LK-VA-FONC.
          COPY CPSP1408.
      *----PDOCFISCAL
            05 LK-CPSP1411         REDEFINES LK-VA-FONC.
          COPY CPSP1411.
      *----PREFCOURRIER
          05 LK-CPSP1420           REDEFINES LK-VA-FONC.
          COPY CPSP1420.

M002  * ZONE DE SAUVEGARDE POUR BLOC PDOCFISCAL MULTIPLE
M002   01 WS-VA-DATA-SP1411-SAUV   PIC X(7000) VALUE SPACES.


      * DONNEE BLOC UINSTITUTION UDESTINATAIRE UINDIVIDU
       01 WS-GR-DATA-BLOC1         PIC X(579) VALUE SPACES.
      * BLOC UINSTITUTION
       01 WS-GR-UIS             REDEFINES WS-GR-DATA-BLOC1.
          COPY CPSPEUIS  REPLACING ==:P:== BY LI-UIS.
      * BLOC UDESTINATAIRE
       01 WS-GR-UDE             REDEFINES WS-GR-DATA-BLOC1.
          COPY CPSPEUDE  REPLACING ==:P:== BY LI-UDE.
      * BLOC UINSTITUTION
       01 WS-GR-UIN             REDEFINES WS-GR-DATA-BLOC1.
          COPY CPSPEUIN  REPLACING ==:P:== BY LI-UIN.
      * BLOC UADRESSE
       01 WS-GR-UAD             REDEFINES WS-GR-DATA-BLOC1.
          COPY CPSPEUAD  REPLACING ==:P:== BY LI-UAD.
                                                                        00029800
      * ZONE DE COMMUNICATION LE FICHIER FCGEEDF1 ISSUE DU DECHARGEMENT 00025700
       01 WS-GR-EDF.                                                    00019300
           COPY CPGEEDF1.                                               00025900
                                                                        00025600
      * ZONE DE COMMUNICATION LE FICHIER FC99FIS1 ISSUE DU DECHARGEMENT 00025700
       01 WS-GR-FIS.                                                    00019300
           COPY CP99FIS1.                                               00025900
                                                                        00035700
      ******************************************************************00035800
      *        DESCRIPTION DES ZONES DE TRAVAIL SQLCA                   00035900
      ******************************************************************00036000
       01 SQL-ERROR-FIELDS.                                             00036100
           05 SQL-ERR-CTR           PIC  9(001).                        00036200
           05 SQL-ERROR-TEXT-LEN    PIC S9(009) COMP VALUE +120.        00036300
           05 SQL-ERROR-MESSAGE.                                        00036400
              10 SQL-ERROR-LEN      PIC S9(004) COMP VALUE +960.        00036500
              10 SQL-ERROR-TEXT     PIC  X(120) OCCURS 8 TIMES.         00036600
                                                                        00036700
       01 WS-LB-SQLCA               PIC X(136).                         00036800
                                                                        00036900
      ******************************************************************00037000
      *        DESCRIPTION DES ZONES DE TRAVAIL GESTION DE DATE         00037100
      ******************************************************************00037200
                                                                        00037300
       01 WS-TS-DATE.                                                   00032000
      *-- PARTIE DATE DU TIMESTAMP                                      00037500
          05 WS-DT-TS      PIC  X(10).                                  00037600
          05 FILLER        PIC  X(01).                                  00037700
      *-- PARTIE HEURE DU TIMESTAMP                                     00037800
          05 WS-TI-TS      PIC  X(15).                                  00037900
                                                                        00038000
       01 WS-TI-HHMMSS.                                                 00038100
           05   WS-TI-HH          PIC 9(02).                            00038200
           05   WS-TI-S1          PIC X(01).                            00038300
           05   WS-TI-MM          PIC 9(02).                            00038400
           05   WS-TI-S2          PIC X(01).                            00038500
           05   WS-TI-SS          PIC 9(02).                            00038600
                                                                        00038700
       01 WS-CO-RET-EDIT          PIC -ZZZ9.                            00038800
                                                                        00038900
      ******************************************************************00039000
      *        DESCRIPTION DES ZONES DE TRAVAIL GESTION DU              00039100
      *        COMPTE-RENDU TECHNIQUE                                   00039200
      ******************************************************************00039300
      *-- INDICE DE LA LIGNE DE COMPTE-RENDU TECHNIQUE                  00040400
       01 WS-IX-CRU                     PIC 9(002) VALUE ZERO.          00040500
                                                                        00040600
       01 TB-GR-CRU.                                                    00040700
      *-- NOMBRE DE POSTES                                              00040800
          05 TB-NB-RUB-CRU           PIC 9(004).                        00040900
      *-- LISTE POSTES                                                  00041000
          05 TB-GR-DET-CRU              OCCURS 50.                      00041100
             10 TB-CO-RUB               PIC 9(004).                     00041200
             10 TB-LB-RUB               PIC X(032).                     00041300
             10 TB-NB-RUB-X.                                            00041400
                15 TB-NB-RUB            PIC 9(015).                     00041500
                                                                        00041600
      *-- TYPE DE LIGNE DE COMPTE-RENDU                                 00041700
       01 WS-CO-TYPE-LIG-CRU            PIC 9(003).                     00041800
                                                                        00041900
      ******************************************************************00042000
      *     DESCRIPTION DES ZONES DE COMPTE-RENDU                       00042100
      *         -> NOMBRE DOSSIERS TRAITES (TOTAL)                      00042200
      *         -> NOMBRE ANOMALIE                                      00042300
      *            !!! LES CODES RUBRIQUES DOIVENT COMMENCER            00042400
      *                A 0000 ET SE TERMINER A 0020 MAXIMUM.            00042500
      ******************************************************************00042600
        01 WS-GR-CRU.                                                   00042700
      *      NOMBRE DE POSTES                                           00042800
             05 WS-NB-CRU             PIC 9(004) VALUE 2.               00042900
             05 WS-GR-CRU-D.                                            00043000
      *         SUIVI TECHNIQUE COMPTEUR 10                             00043100
                10 WS-CO-RUB-10       PIC 9(004) VALUE 0010.            00043200
                10 WS-LB-NM-RUB-10    PIC X(032)                        00043300
                   VALUE 'NOMBRE DOSSIERS TRAITES '.                    00043400
                10 WS-NB-RUB-10-X.                                      00043500
                   15 WS-NB-DOS       PIC 9(015) VALUE ZERO.            00043600
      *         SUIVI TECHNIQUE COMPTEUR 15                             00043700
                10 WS-CO-RUB-15       PIC 9(004) VALUE 0009.            00043800
                10 WS-LB-NM-RUB-15    PIC X(032)                        00043900
                   VALUE 'NOMBRE ANOMALIES '.                           00044000
                10 WS-NB-RUB-15-X.                                      00044100
                   15 WS-CT-ANO       PIC 9(015) VALUE ZERO.            00044200
                                                                        00044300
      ******************************************************************00044400
      *     DESCRIPTION DES ZONES DE COMPTE-RENDU METIER                00044500
      ******************************************************************00044600
        01 WS-GR-CRU-MET.                                               00045200
      *--  NOMBRE DE POSTES                                             00045300
           05 WS-NB-RUB-CRU        PIC 9(004) VALUE 12.
           05 WS-GR-CRU-D.                                              00045500
      *--  SUIVI COMPTEUR 1                                             00045600
              10 WS-CO-RUB-1          PIC 9(004) VALUE 0021.            00045700
              10 WS-LB-NM-RUB-1.                                        00038900
                15 WS-LB-NB-RUB-1-TXT PIC X(28) VALUE                   00039000
              '--COMPTE RENDU FONCTIONNEL--'.                           00039100
                15 WS-LB-NB-RUB-1-REG PIC X(01) VALUE SPACE.            00039200
                15 WS-LB-NB-RUB-1-INS PIC X(03).                        00039300
              10 WS-NB-RUB-1-X.                                         00045900
                 15 WS-NB-RUB-1       PIC 9(015) VALUE ZERO.            00046000
      *--  SUIVI COMPTEUR 0                                             00039600
              10 WS-CO-RUB-0          PIC 9(004) VALUE 0022.            00039700
              10 WS-LB-NM-RUB-0.                                        00039800
                15 WS-LB-NB-RUB-0-TXT PIC X(28) VALUE                   00039900
              '[- TRAITEMENT DE MASSE: M -]'.                           00040000
                15 WS-LB-NB-RUB-0-REG PIC X(01) VALUE SPACE.            00040100
                15 WS-LB-NB-RUB-0-INS PIC X(03) VALUE SPACE.            00040200
              10 WS-NB-RUB-0-X.                                         00040300
                 15 WS-NB-RUB-0       PIC 9(015) VALUE ZERO.            00040400
      *--  SUIVI COMPTEUR 2                                             00046100
              10 WS-CO-RUB-2          PIC 9(004) VALUE 0023.            00040600
              10 WS-LB-NM-RUB-2.                                        00040700
                15 WS-LB-NB-RUB-2-TXT PIC X(28) VALUE                   00040800
              '===  NB ALLOC./TYPE INST ==='.                           00040900
                15 WS-LB-NB-RUB-2-REG PIC X(01) VALUE SPACE.            00041000
                15 WS-LB-NB-RUB-2-INS PIC X(03).                        00041100
              10 WS-NB-RUB-2-X.                                         00046400
                 15 WS-NB-RUB-2       PIC 9(015) VALUE ZERO.            00046500
      *--  SUIVI COMPTEUR 3                                             00041400
              10 WS-CO-RUB-3          PIC 9(004) VALUE 0024.            00041500
              10 WS-LB-NM-RUB-3.                                        00041600
                15 WS-LB-NB-RUB-3-TXT PIC X(28) VALUE                   00041700
              'NB ALLOCATAIRES TRAITES'.
                15 WS-LB-NB-RUB-3-REG PIC X(01) VALUE SPACE.            00041900
                15 WS-LB-NB-RUB-3-INS PIC X(03).                        00042000
              10 WS-NB-RUB-3-X.                                         00042100
                 15 WS-NB-RUB-3       PIC 9(015) VALUE ZERO.            00042200
      *--  SUIVI COMPTEUR 4                                             00042300
              10 WS-CO-RUB-4          PIC 9(004) VALUE 0025.            00042400
              10 WS-LB-NM-RUB-4.                                        00042500
                15 WS-LB-NB-RUB-4-TXT PIC X(28) VALUE                   00042600
              'NB ALLOCATAIRE EN ANO BLQ AF'.
                15 WS-LB-NB-RUB-4-REG PIC X(01) VALUE SPACE.            00042800
                15 WS-LB-NB-RUB-4-INS PIC X(03).                        00042900
              10 WS-NB-RUB-4-X.                                         00043000
                 15 WS-NB-RUB-4       PIC 9(015) VALUE ZERO.            00043100
      *--  SUIVI COMPTEUR 5                                             00043200
              10 WS-CO-RUB-5          PIC 9(004) VALUE 0026.            00043300
              10 WS-LB-NM-RUB-5.                                        00043400
                15 WS-LB-NB-RUB-5-TXT PIC X(28) VALUE                   00043500
              'NB AL EXO. EN ANO BLQ AF '.
                15 WS-LB-NB-RUB-5-REG PIC X(01) VALUE SPACE.            00043700
                15 WS-LB-NB-RUB-5-INS PIC X(03).                        00043800
              10 WS-NB-RUB-5-X.                                         00043900
                 15 WS-NB-RUB-5       PIC 9(015) VALUE ZERO.            00044000
      *--  SUIVI COMPTEUR 6                                             00044100
              10 WS-CO-RUB-6          PIC 9(004) VALUE 0027.            00044200
              10 WS-LB-NM-RUB-6.                                        00044300
                15 WS-LB-NB-RUB-6-TXT PIC X(28) VALUE                   00044400
              'NB AL NEXO.ANO BLQ VIV FR'.
                15 WS-LB-NB-RUB-6-REG PIC X(01) VALUE SPACE.            00044600
                15 WS-LB-NB-RUB-6-INS PIC X(03).                        00044700
              10 WS-NB-RUB-6-X.                                         00044800
                 15 WS-NB-RUB-6       PIC 9(015) VALUE ZERO.            00044900
      *--  SUIVI COMPTEUR 7                                             00045000
              10 WS-CO-RUB-7          PIC 9(004) VALUE 0028.            00045100
              10 WS-LB-NM-RUB-7.                                        00045200
                15 WS-LB-NB-RUB-7-TXT PIC X(28) VALUE                   00045300
              'NB AL NEXO.ANO BLQ VIV ETRG '.
                15 WS-LB-NB-RUB-7-REG PIC X(01) VALUE SPACE.            00045500
                15 WS-LB-NB-RUB-7-INS PIC X(03).                        00045600
              10 WS-NB-RUB-7-X.                                         00045700
                 15 WS-NB-RUB-7       PIC 9(015) VALUE ZERO.            00045800
      *--  SUIVI COMPTEUR 8                                             00045900
              10 WS-CO-RUB-8          PIC 9(004) VALUE 0029.            00046000
              10 WS-LB-NM-RUB-8.                                        00046100
                15 WS-LB-NB-RUB-8-TXT PIC X(28) VALUE                   00046200
              'NB AL AYANT AF ENVOYEE'.
                15 WS-LB-NB-RUB-8-REG PIC X(01) VALUE SPACE.            00046400
                15 WS-LB-NB-RUB-8-INS PIC X(03).                        00046500
              10 WS-NB-RUB-1-X.                                         00046600
                 15 WS-NB-RUB-8       PIC 9(015) VALUE ZERO.            00046700
      *--  SUIVI COMPTEUR 9                                             00046800
              10 WS-CO-RUB-9          PIC 9(004) VALUE 0030.            00046900
              10 WS-LB-NM-RUB-9.                                        00047000
                15 WS-LB-NB-RUB-9-TXT PIC X(28) VALUE                   00047100
              'NB AL EXO. DONT AF ENVOYEE'.
                15 WS-LB-NB-RUB-9-REG PIC X(01) VALUE SPACE.            00047300
                15 WS-LB-NB-RUB-9-INS PIC X(03).                        00047400
              10 WS-NB-RUB-9-X.                                         00047500
                 15 WS-NB-RUB-9       PIC 9(015) VALUE ZERO.            00047600
      *--  SUIVI COMPTEUR 10
              10 WS-CO-RUB-10          PIC 9(004) VALUE 0031.
              10 WS-LB-NM-RUB-10.
                15 WS-LB-NB-RUB-10-TXT PIC X(28) VALUE
              'NB AL NEXO. ,AF ENV VIV FR'.
                15 WS-LB-NB-RUB-10-REG PIC X(01) VALUE SPACE.
                15 WS-LB-NB-RUB-10-INS PIC X(03).
              10 WS-NB-RUB-10-X.
                 15 WS-NB-RUB-10       PIC 9(015) VALUE ZERO.
      *--  SUIVI COMPTEUR 11
              10 WS-CO-RUB-11          PIC 9(004) VALUE 0032.
              10 WS-LB-NM-RUB-11.
                15 WS-LB-NB-RUB-11-TXT PIC X(28) VALUE
              'NB AL NEXO. ,AF ENV VIV ETRG'.
                15 WS-LB-NB-RUB-11-REG PIC X(01) VALUE SPACE.
                15 WS-LB-NB-RUB-11-INS PIC X(03).
              10 WS-NB-RUB-11-X.
                 15 WS-NB-RUB-11       PIC 9(015) VALUE ZERO.

      *-DETAIL POUR LES COMPTEUR                                        00047800
        01 WS-GR-CRU-MET-MT.                                            00047900
      *--  NOMBRE DE POSTES                                             00048000
           05 WS-NB-RUB-CRU-MT     PIC 9(004) VALUE 11.                 00048100
           05 WS-GR-CRU-D-MT.                                           00048200
      *--  SUIVI COMPTEUR 1                                             00048300
              10 WS-CO-RUB-1-MT          PIC 9(004) VALUE 0021.         00048400
              10 WS-LB-NM-RUB-1-MT.                                     00048500
                15 WS-LB-NB-RUB-1-TXT-MT PIC X(28) VALUE                00048600
              '===== DETAILS MONTANTS ====='.                           00048700
                15 WS-LB-NB-RUB-1-REG-MT PIC X(01) VALUE SPACE.         00048800
                15 WS-LB-NB-RUB-1-INS-MT PIC X(03).                     00048900
              10 WS-NB-RUB-1-X-MT.                                      00049000
                 15 WS-NB-RUB-1-MT       PIC 9(015) VALUE ZERO.         00049100
      *--  SUIVI COMPTEUR 2                                             00049200
              10 WS-CO-RUB-2-MT          PIC 9(004) VALUE 0022.         00049300
              10 WS-LB-NM-RUB-2-MT.                                     00049400
                15 WS-LB-NB-RUB-2-TXT-MT PIC X(28) VALUE                00049500
              '** NB ALLOC TOTAL     '.
                15 WS-LB-NB-RUB-2-REG-MT PIC X(01) VALUE SPACE.         00049700
                15 WS-LB-NB-RUB-2-INS-MT PIC X(03).                     00049800
              10 WS-NB-RUB-2-X-MT.                                      00049900
                 15 WS-NB-RUB-2-MT       PIC 9(015) VALUE ZERO.         00050000
      *--  SUIVI COMPTEUR 3                                             00050100
              10 WS-CO-RUB-3-MT          PIC 9(004) VALUE 0023.         00050200
              10 WS-LB-NM-RUB-3-MT.                                     00050300
                15 WS-LB-NB-RUB-3-TXT-MT PIC X(28) VALUE                00050400
              'MONTANT BRUT TOTAL         '.                            00050500
                15 WS-LB-NB-RUB-3-REG-MT PIC X(01) VALUE SPACE.         00050600
                15 WS-LB-NB-RUB-3-INS-MT PIC X(03).                     00050700
              10 WS-NB-RUB-3-X-MT.                                      00050800
                 15 WS-NB-RUB-3-MT       PIC 9(015) VALUE ZERO.         00050900
      *--  SUIVI COMPTEUR 4                                             00051000
              10 WS-CO-RUB-4-MT          PIC 9(004) VALUE 0024.         00051100
              10 WS-LB-NM-RUB-4-MT.                                     00051200
                15 WS-LB-NB-RUB-4-TXT-MT PIC X(28) VALUE                00051300
              'MONTANT NET FISCAL TOTAL   '.                            00051400
                15 WS-LB-NB-RUB-4-REG-MT PIC X(01) VALUE SPACE.         00051500
                15 WS-LB-NB-RUB-4-INS-MT PIC X(03).                     00051600
              10 WS-NB-RUB-4-X-MT.                                      00051700
                 15 WS-NB-RUB-4-MT       PIC 9(015) VALUE ZERO.         00051800
      *--  SUIVI COMPTEUR 5                                             00051900
              10 WS-CO-RUB-5-MT       PIC 9(004) VALUE 0025.            00052000
              10 WS-LB-NM-RUB-5-MT.                                     00052100
                15 WS-LB-NB-RUB-5-TXT-MT PIC X(28) VALUE                00052200
              'TOTAL RETENUES A LA SOURCE  '.                           00052300
                15 WS-LB-NB-RUB-5-REG-MT PIC X(01) VALUE SPACE.         00052400
                15 WS-LB-NB-RUB-5-INS-MT PIC X(03).                     00052500
              10 WS-NB-RUB-5-X-MT.                                      00052600
                 15 WS-NB-RUB-5-MT       PIC 9(015) VALUE ZERO.         00052700
      *--  SUIVI COMPTEUR 6                                             00052800
              10 WS-CO-RUB-6-MT          PIC 9(004) VALUE 0026.         00052900
              10 WS-LB-NM-RUB-6-MT.                                     00053000
                15 WS-LB-NB-RUB-6-TXT-MT PIC X(28) VALUE                00053100
              'MONTANT MAJO ENFANT A CHARGE'.                           00053200
                15 WS-LB-NB-RUB-6-REG-MT PIC X(01) VALUE SPACE.         00053300
                15 WS-LB-NB-RUB-6-INS-MT PIC X(03).                     00053400
              10 WS-NB-RUB-6-X-MT.                                      00053500
                 15 WS-NB-RUB-6-MT       PIC 9(015) VALUE ZERO.         00053600
      *--  SUIVI COMPTEUR 7                                             00053700
              10 WS-CO-RUB-7-MT          PIC 9(004) VALUE 0027.         00053800
              10 WS-LB-NM-RUB-7-MT.                                     00053900
                15 WS-LB-NB-RUB-7-TXT-MT PIC X(28) VALUE                00054000
              'MONTANT MAJO ENFANT ELEVE   '.                           00054100
                15 WS-LB-NB-RUB-7-REG-MT PIC X(01) VALUE SPACE.         00054200
                15 WS-LB-NB-RUB-7-INS-MT PIC X(03).                     00054300
              10 WS-NB-RUB-7-X-MT.                                      00054400
                 15 WS-NB-RUB-7-MT       PIC 9(015) VALUE ZERO.         00054500
      *--  SUIVI COMPTEUR 8                                             00054600
              10 WS-CO-RUB-8-MT          PIC 9(004) VALUE 0028.         00054700
              10 WS-LB-NM-RUB-8-MT.                                     00054800
                15 WS-LB-NB-RUB-8-TXT-MT PIC X(28) VALUE                00054900
              'TOT COT SS (COTAL/COTAM)    '.                           00055000
                15 WS-LB-NB-RUB-8-REG-MT PIC X(01) VALUE SPACE.         00055100
                15 WS-LB-NB-RUB-8-INS-MT PIC X(03).                     00055200
              10 WS-NB-RUB-8-X-MT.                                      00055300
                 15 WS-NB-RUB-8-MT       PIC 9(015) VALUE ZERO.         00055400
      *--  SUIVI COMPTEUR 9                                             00055500
              10 WS-CO-RUB-9-MT          PIC 9(004) VALUE 0029.         00055600
              10 WS-LB-NM-RUB-9-MT.                                     00055700
                15 WS-LB-NB-RUB-9-TXT-MT PIC X(28) VALUE                00055800
              'TOT CSG NON DEDUC.ENF ELEVE '.                           00055900
                15 WS-LB-NB-RUB-9-REG-MT PIC X(01) VALUE SPACE.         00056000
                15 WS-LB-NB-RUB-9-INS-MT PIC X(03).                     00056100
              10 WS-NB-RUB-9-X-MT.                                      00056200
                 15 WS-NB-RUB-9-MT    PIC 9(015) VALUE ZERO.            00056300
      *--  SUIVI COMPTEUR 10                                            00056400
              10 WS-CO-RUB-10-MT          PIC 9(004) VALUE 0030.        00056500
              10 WS-LB-NM-RUB-10-MT.                                    00056600
                15 WS-LB-NB-RUB-10-TXT-MT PIC X(28) VALUE               00056700
              'TOT CSG DEDUC.HORS ENF ELEVE'.                           00056800
                15 WS-LB-NB-RUB-10-REG-MT PIC X(01) VALUE SPACE.        00056900
                15 WS-LB-NB-RUB-10-INS-MT PIC X(03).                    00057000
              10 WS-NB-RUB-10-X-MT.                                     00057100
                 15 WS-NB-RUB-10-MT    PIC 9(015) VALUE ZERO.           00057200
      *--  SUIVI COMPTEUR 11                                            00057300
              10 WS-CO-RUB-11-MT          PIC 9(004) VALUE 0031.        00057400
              10 WS-LB-NM-RUB-11-MT.                                    00057500
                15 WS-LB-NB-RUB-11-TXT-MT PIC X(28) VALUE               00057600
              'TOT CSG NON DEDUCTIBLE      '.                           00057700
                15 WS-LB-NB-RUB-11-REG-MT PIC X(01) VALUE SPACE.        00057800
                15 WS-LB-NB-RUB-11-INS-MT PIC X(03).                    00057900
              10 WS-NB-RUB-11-X-MT.                                     00058000
                 15 WS-NB-RUB-11-MT    PIC 9(015) VALUE ZERO.           00058100
      *--  SUIVI COMPTEUR 12                                            00058200
              10 WS-CO-RUB-12-MT          PIC 9(004) VALUE 0032.        00058300
              10 WS-LB-NM-RUB-12-MT.                                    00058400
                15 WS-LB-NB-RUB-12-TXT-MT PIC X(28) VALUE               00058500
              'TOT RDS                     '.                           00058600
                15 WS-LB-NB-RUB-12-REG-MT PIC X(01) VALUE SPACE.        00058700
                15 WS-LB-NB-RUB-12-INS-MT PIC X(03).                    00058800
              10 WS-NB-RUB-12-X-MT.                                     00058900
                 15 WS-NB-RUB-12-MT     PIC 9(015) VALUE ZERO.          00059000
      *--  SUIVI COMPTEUR 13                                            00059100
              10 WS-CO-RUB-13-MT          PIC 9(004) VALUE 0033.        00059200
              10 WS-LB-NM-RUB-13-MT.                                    00059300
                15 WS-LB-NB-RUB-13-TXT-MT PIC X(28) VALUE               00059400
              'MONTANT TOTAL RAPPELS ANTER.'.                           00059500
                15 WS-LB-NB-RUB-13-REG-MT PIC X(01) VALUE SPACE.        00059600
                15 WS-LB-NB-RUB-13-INS-MT PIC X(03).                    00059700
              10 WS-NB-RUB-13-X-MT.                                     00059800
                 15 WS-NB-RUB-13-MT       PIC 9(015) VALUE ZERO.        00059900
                                                                        00060000
        01 WS-GR-CRU-INST.                                              00060100
      *--  NOMBRE DE POSTES                                             00060200
           05 WS-NB-RUB-CRU-I           PIC 9(004) VALUE 3.             00060300
           05 WS-GR-CRU-D-I.                                            00060400
      *--  SUIVI COMPTEUR 1                                             00060500
              10 WS-CO-RUB-1-I          PIC 9(004) VALUE 0021.          00060600
              10 WS-LB-NM-RUB-1-I.                                      00060700
                15 WS-LB-NB-RUB-1-TXT-I PIC X(28) VALUE                 00060800
              '____________________________'.                           00060900
                15 FILLER               PIC X(04) VALUE SPACE.          00061000
              10 WS-NB-RUB-1-X-I.                                       00061100
                 15 WS-NB-RUB-1-I       PIC 9(015) VALUE ZERO.          00061200
      *--  SUIVI COMPTEUR 2                                             00061300
              10 WS-CO-RUB-2-I          PIC 9(004) VALUE 0022.          00061400
              10 WS-LB-NM-RUB-2-I.                                      00061500
                15 WS-LB-NB-RUB-2-TXT-I PIC X(28) VALUE                 00061600
              '*** NB TOTAL INSTITUTION ***'.                           00061700
                15 FILLER               PIC X(04) VALUE SPACE.          00061800
              10 WS-NB-RUB-2-X-I.                                       00061900
                 15 WS-NB-RUB-2-I       PIC 9(015) VALUE ZERO.          00062000
      *--  SUIVI COMPTEUR 3                                             00062100
              10 WS-CO-RUB-3-I          PIC 9(004) VALUE 0023.          00062200
              10 WS-LB-NM-RUB-3-I.                                      00062300
                15 WS-LB-NB-RUB-3-TXT-I PIC X(28) VALUE                 00062400
              '____________________________'.                           00062500
                15 FILLER               PIC X(04) VALUE SPACE.          00062600
              10 WS-NB-RUB-3-X-I.                                       00062700
                 15 WS-NB-RUB-3-I       PIC 9(015) VALUE ZERO.          00062800

      ******************************************************************
      *     DESCRIPTION DES ZONES DE COMPTE-RENDU METIER  PONCTUEL
      ******************************************************************
        01 WS-GR-CRU-MET-P.
      *--  NOMBRE DE POSTES
           05 WS-NB-RUB-CRU-P        PIC 9(004) VALUE 6.
           05 WS-GR-CRU-D-P.
      *--  SUIVI COMPTEUR 1
              10 WS-CO-RUB-1-P          PIC 9(004) VALUE 0021.
              10 WS-LB-NM-RUB-1-P.
                15 WS-LB-NB-RUB-1-TXT-P PIC X(28) VALUE
              '--COMPTE RENDU FONCTIONNEL--'.
                15 WS-LB-NB-RUB-1-REG-P PIC X(01) VALUE SPACE.
                15 WS-LB-NB-RUB-1-INS-P PIC X(03).
              10 WS-NB-RUB-1-X-P.
                 15 WS-NB-RUB-1-P       PIC 9(015) VALUE ZERO.
      *--  SUIVI COMPTEUR 0
              10 WS-CO-RUB-0-P          PIC 9(004) VALUE 0022.
              10 WS-LB-NM-RUB-0-P.
                15 WS-LB-NB-RUB-0-TXT-P PIC X(32) VALUE
              '* EMETTRE ATTESTATIONS FISCALE *'.
              10 WS-NB-RUB-0-X-P.
                 15 WS-NB-RUB-0-P       PIC 9(015) VALUE ZERO.
      *--  SUIVI COMPTEUR 2
              10 WS-CO-RUB-2-P          PIC 9(004) VALUE 0023.
              10 WS-LB-NM-RUB-2-P.
                15 WS-LB-NB-RUB-2-TXT-P PIC X(32) VALUE
              '= PARAMETRE EXECUTION : P       '.
              10 WS-NB-RUB-2-X-P.
                 15 WS-NB-RUB-2-P       PIC 9(015) VALUE ZERO.
      *--  SUIVI COMPTEUR 3
              10 WS-CO-RUB-3-P          PIC 9(004) VALUE 0024.
              10 WS-LB-NM-RUB-3-P.
                15 WS-LB-NB-RUB-3-TXT-P PIC X(32) VALUE
              'NB ENVOIS TRAITES'.
              10 WS-NB-RUB-3-X-P.
                 15 WS-NB-RUB-3-P       PIC 9(015) VALUE ZERO.
      *--  SUIVI COMPTEUR 4
              10 WS-CO-RUB-4-P          PIC 9(004) VALUE 0025.
              10 WS-LB-NM-RUB-4-P.
                15 WS-LB-NB-RUB-4-TXT-P PIC X(32) VALUE
              'NB ENVOIS TRAITES EN ANO BLOQ AF'.
              10 WS-NB-RUB-4-X-P.
                 15 WS-NB-RUB-4-P       PIC 9(015) VALUE ZERO.
      *--  SUIVI COMPTEUR 5
              10 WS-CO-RUB-5-P          PIC 9(004) VALUE 0026.
              10 WS-LB-NM-RUB-5-P.
                15 WS-LB-NB-RUB-5-TXT-P PIC X(32) VALUE
              'NB ENVOIS TRAITES ET ENVOYE '.
              10 WS-NB-RUB-5-X-P.
                 15 WS-NB-RUB-5-P       PIC 9(015) VALUE ZERO.


      ******************************************************************
      *  COMPTEUR FONCTIONNEL TEMPORAIRE
      ******************************************************************
        01 WS-CT-FONC-TEMP.
      **    TYPE DE COMPTEUR (9 AU TOTAL)
           05 WS-NB-TP-TOT-1                     PIC 9(15) VALUE 0.
           05 WS-NB-TP-ANO-BLQ-AF-2              PIC 9(15) VALUE 0.
           05 WS-NB-TP-EXO-ANO-BLQ-AF-3          PIC 9(15) VALUE 0.
           05 WS-NB-TP-NEXO-ANO-BLQ-AF-FR-4      PIC 9(15) VALUE 0.
           05 WS-NB-TP-NEXO-ANO-BLQ-AF-ETG-5     PIC 9(15) VALUE 0.
      *      NOMBRE ALLOCATAIRE AF DONT AF EST ENVOYE
           05 WS-NB-TP-AF-ENV-6                  PIC 9(15) VALUE 0.
           05 WS-NB-TP-EXO-AF-ENV-7              PIC 9(15) VALUE 0.
           05 WS-NB-TP-NEXO-AF-ENV-FR-8          PIC 9(15) VALUE 0.
           05 WS-NB-TP-NEXO-AF-ENV-ETRG-9        PIC 9(15) VALUE 0.
      *==   DETAILS DES MONTANTS
           05 WS-MT-BRT-TOT-TMP                  PIC 9(15) VALUE 0.
           05 WS-MT-NET-FIS-TOT-TMP              PIC 9(15) VALUE 0.
           05 WS-MT-NET-RET-TOT-TMP              PIC 9(15) VALUE 0.
           05 WS-MT-TOT-RET-SRC-TMP              PIC 9(15) VALUE 0.
           05 WS-MT-TOT-MAJ-EC-TMP               PIC 9(15) VALUE 0.
           05 WS-MT-TOT-MAJ-EE-TMP               PIC 9(15) VALUE 0.
           05 WS-MT-TOT-COT-SS-TMP               PIC 9(15) VALUE 0.
           05 WS-MT-TOT-CSG-NDDUC-EE-TMP         PIC 9(15) VALUE 0.
           05 WS-MT-TOT-CSG-DDUC-HEE-TMP         PIC 9(15) VALUE 0.
           05 WS-MT-TOT-CSG-NDDUC-TMP            PIC 9(15) VALUE 0.
           05 WS-MT-TOT-RDS-TMP                  PIC 9(15) VALUE 0.
           05 WS-MT-TOT-RPL-ANT-TMP              PIC 9(15) VALUE 0.
                                                                        00062900
      ********************************************************
      *   COMPTEUR FONCTIONNEL
      ********************************************************
        01 WS-CT-FONC.                                                  00063100
      **   TYPE DE COMPTEUR (7 AU TOTAL)
           05 WS-NB-AL-TOT-1                     PIC 9(15) VALUE 0.
           05 WS-NB-AL-ANO-BLQ-AF-2              PIC 9(15) VALUE 0.
           05 WS-NB-AL-EXO-ANO-BLQ-AF-3          PIC 9(15) VALUE 0.
           05 WS-NB-AL-NEXO-ANO-BLQ-AF-FR-4      PIC 9(15) VALUE 0.
           05 WS-NB-AL-NEXO-ANO-BLQ-AF-ETG-5     PIC 9(15) VALUE 0.
      *      NOMBRE ALLOCATAIRE AF DONT AF EST ENVOYE
           05 WS-NB-AL-AF-ENV-6                  PIC 9(15) VALUE 0.
           05 WS-NB-AL-EXO-AF-ENV-7              PIC 9(15) VALUE 0.
           05 WS-NB-AL-NEXO-AF-ENV-FR-8          PIC 9(15) VALUE 0.
           05 WS-NB-AL-NEXO-AF-ENV-ETRG-9        PIC 9(15) VALUE 0.

      **    DETAIL DES MONTANT PAR TYPE DE COMPTEURS
      **    -1 => PR ALLOCATAIRE TOTAL
      **    -2 => PR ALLOCATAIRE EXONERE
      **    -3 => ETC...
           05 WS-MT-BRT-TOT-1              PIC 9(15) VALUE 0.           00064500
           05 WS-MT-BRT-TOT-2              PIC 9(15) VALUE 0.           00064600
           05 WS-MT-BRT-TOT-3              PIC 9(15) VALUE 0.           00064700
           05 WS-MT-BRT-TOT-4              PIC 9(15) VALUE 0.           00064800
           05 WS-MT-BRT-TOT-5              PIC 9(15) VALUE 0.           00064900
           05 WS-MT-BRT-TOT-6              PIC 9(15) VALUE 0.           00065000
           05 WS-MT-BRT-TOT-7              PIC 9(15) VALUE 0.           00065100
           05 WS-MT-BRT-TOT-8              PIC 9(15) VALUE 0.           00065100
           05 WS-MT-BRT-TOT-9              PIC 9(15) VALUE 0.           00065100
      *-- MONTANT NET FISCAL TOTAL
           05 WS-MT-NET-FIS-TOT-1          PIC 9(15) VALUE 0.           00065200
           05 WS-MT-NET-FIS-TOT-2          PIC 9(15) VALUE 0.           00065300
           05 WS-MT-NET-FIS-TOT-3          PIC 9(15) VALUE 0.           00065400
           05 WS-MT-NET-FIS-TOT-4          PIC 9(15) VALUE 0.           00065500
           05 WS-MT-NET-FIS-TOT-5          PIC 9(15) VALUE 0.           00065600
           05 WS-MT-NET-FIS-TOT-6          PIC 9(15) VALUE 0.           00065700
           05 WS-MT-NET-FIS-TOT-7          PIC 9(15) VALUE 0.           00065800
           05 WS-MT-NET-FIS-TOT-8          PIC 9(15) VALUE 0.           00065800
           05 WS-MT-NET-FIS-TOT-9          PIC 9(15) VALUE 0.           00065800
      *-- MONTANT NET RETRAITE TOTAL ??
           05 WS-MT-NET-RET-TOT-1          PIC 9(15) VALUE 0.
           05 WS-MT-NET-RET-TOT-2          PIC 9(15) VALUE 0.
           05 WS-MT-NET-RET-TOT-3          PIC 9(15) VALUE 0.
           05 WS-MT-NET-RET-TOT-4          PIC 9(15) VALUE 0.
           05 WS-MT-NET-RET-TOT-5          PIC 9(15) VALUE 0.
           05 WS-MT-NET-RET-TOT-6          PIC 9(15) VALUE 0.
           05 WS-MT-NET-RET-TOT-7          PIC 9(15) VALUE 0.
           05 WS-MT-NET-RET-TOT-8          PIC 9(15) VALUE 0.
           05 WS-MT-NET-RET-TOT-9          PIC 9(15) VALUE 0.
      *-- MONTANT TOTAL RETENUES SOURCE
           05 WS-MT-TOT-RET-SRC-1          PIC 9(15) VALUE 0.           00065900
           05 WS-MT-TOT-RET-SRC-2          PIC 9(15) VALUE 0.           00066000
           05 WS-MT-TOT-RET-SRC-3          PIC 9(15) VALUE 0.           00066100
           05 WS-MT-TOT-RET-SRC-4          PIC 9(15) VALUE 0.           00066200
           05 WS-MT-TOT-RET-SRC-5          PIC 9(15) VALUE 0.           00066300
           05 WS-MT-TOT-RET-SRC-6          PIC 9(15) VALUE 0.           00066400
           05 WS-MT-TOT-RET-SRC-7          PIC 9(15) VALUE 0.           00066500
           05 WS-MT-TOT-RET-SRC-8          PIC 9(15) VALUE 0.
           05 WS-MT-TOT-RET-SRC-9          PIC 9(15) VALUE 0.
      *-- MONTANT TOTAL MAJO ENFANT CHARGE
           05 WS-MT-TOT-MAJ-EC-1           PIC 9(15) VALUE 0.           00066600
           05 WS-MT-TOT-MAJ-EC-2           PIC 9(15) VALUE 0.           00066700
           05 WS-MT-TOT-MAJ-EC-3           PIC 9(15) VALUE 0.           00066800
           05 WS-MT-TOT-MAJ-EC-4           PIC 9(15) VALUE 0.           00066900
           05 WS-MT-TOT-MAJ-EC-5           PIC 9(15) VALUE 0.           00067000
           05 WS-MT-TOT-MAJ-EC-6           PIC 9(15) VALUE 0.           00067100
           05 WS-MT-TOT-MAJ-EC-7           PIC 9(15) VALUE 0.           00067200
           05 WS-MT-TOT-MAJ-EC-8           PIC 9(15) VALUE 0.
           05 WS-MT-TOT-MAJ-EC-9           PIC 9(15) VALUE 0.
      *-- MONTANT TOTAL MAJO ENFANT ELEVE
           05 WS-MT-TOT-MAJ-EE-1           PIC 9(15) VALUE 0.           00067300
           05 WS-MT-TOT-MAJ-EE-2           PIC 9(15) VALUE 0.           00067400
           05 WS-MT-TOT-MAJ-EE-3           PIC 9(15) VALUE 0.           00067500
           05 WS-MT-TOT-MAJ-EE-4           PIC 9(15) VALUE 0.           00067600
           05 WS-MT-TOT-MAJ-EE-5           PIC 9(15) VALUE 0.           00067700
           05 WS-MT-TOT-MAJ-EE-6           PIC 9(15) VALUE 0.           00067800
           05 WS-MT-TOT-MAJ-EE-7           PIC 9(15) VALUE 0.           00067900
           05 WS-MT-TOT-MAJ-EE-8           PIC 9(15) VALUE 0.
           05 WS-MT-TOT-MAJ-EE-9           PIC 9(15) VALUE 0.
      *-- MONTANT TOTAL COT SOCIAL(COTAL + COTAM )
           05 WS-MT-TOT-COT-SS-1           PIC 9(15) VALUE 0.           00068000
           05 WS-MT-TOT-COT-SS-2           PIC 9(15) VALUE 0.           00068100
           05 WS-MT-TOT-COT-SS-3           PIC 9(15) VALUE 0.           00068200
           05 WS-MT-TOT-COT-SS-4           PIC 9(15) VALUE 0.           00068300
           05 WS-MT-TOT-COT-SS-5           PIC 9(15) VALUE 0.           00068400
           05 WS-MT-TOT-COT-SS-6           PIC 9(15) VALUE 0.           00068500
           05 WS-MT-TOT-COT-SS-7           PIC 9(15) VALUE 0.           00068600
           05 WS-MT-TOT-COT-SS-8           PIC 9(15) VALUE 0.
           05 WS-MT-TOT-COT-SS-9           PIC 9(15) VALUE 0.
      *-- MONTANT TOTAL CSG NON DEDUCTIBLE ENFANT ELEVE
           05 WS-MT-TOT-CSG-NDDUC-EE-1     PIC 9(15) VALUE 0.           00068700
           05 WS-MT-TOT-CSG-NDDUC-EE-2     PIC 9(15) VALUE 0.           00068800
           05 WS-MT-TOT-CSG-NDDUC-EE-3     PIC 9(15) VALUE 0.           00068900
           05 WS-MT-TOT-CSG-NDDUC-EE-4     PIC 9(15) VALUE 0.           00069000
           05 WS-MT-TOT-CSG-NDDUC-EE-5     PIC 9(15) VALUE 0.           00069100
           05 WS-MT-TOT-CSG-NDDUC-EE-6     PIC 9(15) VALUE 0.           00069200
           05 WS-MT-TOT-CSG-NDDUC-EE-7     PIC 9(15) VALUE 0.           00069300
           05 WS-MT-TOT-CSG-NDDUC-EE-8     PIC 9(15) VALUE 0.
           05 WS-MT-TOT-CSG-NDDUC-EE-9     PIC 9(15) VALUE 0.
      *-- MONTANT TOTAL CSG NON DEDUCTIBLE HORS ENFANT ELEVE
           05 WS-MT-TOT-CSG-DDUC-HEE-1     PIC 9(15) VALUE 0.           00069400
           05 WS-MT-TOT-CSG-DDUC-HEE-2     PIC 9(15) VALUE 0.           00069500
           05 WS-MT-TOT-CSG-DDUC-HEE-3     PIC 9(15) VALUE 0.           00069600
           05 WS-MT-TOT-CSG-DDUC-HEE-4     PIC 9(15) VALUE 0.           00069700
           05 WS-MT-TOT-CSG-DDUC-HEE-5     PIC 9(15) VALUE 0.           00069800
           05 WS-MT-TOT-CSG-DDUC-HEE-6     PIC 9(15) VALUE 0.           00069900
           05 WS-MT-TOT-CSG-DDUC-HEE-7     PIC 9(15) VALUE 0.           00070000
           05 WS-MT-TOT-CSG-DDUC-HEE-8     PIC 9(15) VALUE 0.
           05 WS-MT-TOT-CSG-DDUC-HEE-9     PIC 9(15) VALUE 0.
      *-- MONTANT TOTAL CSG NON DEDUCTIBLE
           05 WS-MT-TOT-CSG-NDDUC-1        PIC 9(15) VALUE 0.           00070100
           05 WS-MT-TOT-CSG-NDDUC-2        PIC 9(15) VALUE 0.           00070200
           05 WS-MT-TOT-CSG-NDDUC-3        PIC 9(15) VALUE 0.           00070300
           05 WS-MT-TOT-CSG-NDDUC-4        PIC 9(15) VALUE 0.           00070400
           05 WS-MT-TOT-CSG-NDDUC-5        PIC 9(15) VALUE 0.           00070500
           05 WS-MT-TOT-CSG-NDDUC-6        PIC 9(15) VALUE 0.           00070600
           05 WS-MT-TOT-CSG-NDDUC-7        PIC 9(15) VALUE 0.           00070700
           05 WS-MT-TOT-CSG-NDDUC-8        PIC 9(15) VALUE 0.
           05 WS-MT-TOT-CSG-NDDUC-9        PIC 9(15) VALUE 0.
      *-- MONTANT TOTAL RDS
           05 WS-MT-TOT-RDS-1              PIC 9(15) VALUE 0.           00070800
           05 WS-MT-TOT-RDS-2              PIC 9(15) VALUE 0.           00070900
           05 WS-MT-TOT-RDS-3              PIC 9(15) VALUE 0.           00071000
           05 WS-MT-TOT-RDS-4              PIC 9(15) VALUE 0.           00071100
           05 WS-MT-TOT-RDS-5              PIC 9(15) VALUE 0.           00071200
           05 WS-MT-TOT-RDS-6              PIC 9(15) VALUE 0.           00071300
           05 WS-MT-TOT-RDS-7              PIC 9(15) VALUE 0.           00071400
           05 WS-MT-TOT-RDS-8              PIC 9(15) VALUE 0.
           05 WS-MT-TOT-RDS-9              PIC 9(15) VALUE 0.
      *-- MONTANT TOTAL RAPPELS ANTERIEURS
           05 WS-MT-TOT-RPL-ANT-1          PIC 9(15) VALUE 0.           00071500
           05 WS-MT-TOT-RPL-ANT-2          PIC 9(15) VALUE 0.           00071600
           05 WS-MT-TOT-RPL-ANT-3          PIC 9(15) VALUE 0.           00071700
           05 WS-MT-TOT-RPL-ANT-4          PIC 9(15) VALUE 0.           00071800
           05 WS-MT-TOT-RPL-ANT-5          PIC 9(15) VALUE 0.           00071900
           05 WS-MT-TOT-RPL-ANT-6          PIC 9(15) VALUE 0.           00072000
           05 WS-MT-TOT-RPL-ANT-7          PIC 9(15) VALUE 0.           00072100
           05 WS-MT-TOT-RPL-ANT-8          PIC 9(15) VALUE 0.
           05 WS-MT-TOT-RPL-ANT-9          PIC 9(15) VALUE 0.

     ** NOMBRE TOTAL INSTITUTION DISTINCTE TRAITE
        01 WS-NB-TOT-INSTIT             PIC 9(15) VALUE 0.
        01 WS-AN-FISCALE                PIC X(4) VALUE SPACE.
M004    01 WS-MT-VTIL-LIF               PIC S9(16)V99 VALUE 0.          00072400
M002+   01 WS-MT-VTIL-LIF-P             PIC 9(16)V99 VALUE 0.           00072400
M002+   01 WS-MT-VTIL-LIF-XP            PIC 9(16),99 VALUE SPACE.       00072400
TEST+   01 WS-MT-TAMPON             PIC S9(16)V99.                      00072400
TEST+   01 WS-MT-TAMPON-SIGN        PIC +9(10)V99 VALUE ZERO.           00072400
TEST+   01 WS-MT-1411               PIC S9(16)V99 COMP-3.               00072400
                                                                        00072500
                                                                        00046600
      ******************************************************************00046700
      *     DESCRIPTION DES                                             00046800
      *                   - ZONES DE COMPTE-RENDU PROGRAMME MAITRE      00046900
      *                   - ZONES DE TRAVAIL PROGRAMME MAITRE           00047000
      *                   - ZONES DE COMPTE-RENDU METIER                00047100
      *                   - ZONES DE TRAVAIL METIER                     00047200
      ******************************************************************00047300
      * ZONE GROUPE DE SAUVEGARDE DANS LA TABLE DES RESTARTS            00049900
      * (DONNEES EN COURS)                                              00050000
        01 WS-GR-SAV.                                                   00050100
      *   DONNEES DE COMPTES-RENDUS SM A SAUVEGARDER                    00050200
           05 WS-GR-CRU-SAV.                                            00050300
      *    --------------------                                         00050400
M002+         10 WS-CT-SAV           PIC X(1755) VALUE ALL '0'.         00074000
M002- *       10 WS-CT-SAV           PIC X(1275) VALUE ALL '0'.         00074000
                                                                        00050900
      *    ZONE DECRIVANT LES VARIABLES AUTRES A SAUVEGARDER            00051000
      *    IDENTIFIANT ANOMALIE (SIMPLE CHRONO)                         00051100
           05 WS-GR-VAR-SAV.                                            00051200
      *    -----------------                                            00051300
              10 WS-ID-ANO           PIC 9(016)  VALUE ZERO.            00051400
              10 WS-ID-ANO-FONC      PIC 9(016)  VALUE ZERO.            00051500
                                                                        00051700
      * ZONE GROUPE DE DERNIERE SAUVEGARDE DANS LA TABLE DES RESTARTS   00051800
      * (DONNEES EFFECTIVEMENT CONSERVEES EN TABLE)                     00051900
        01 WS-GR-SAV-SAUV.                                              00052000
      *    DONNEES DE COMPTES-RENDUS SM SAUVEGARDEES                    00052100
           05 WS-GR-CRU-SAV-SAUV.                                       00052200
M002+         10 WS-CT-SAV-SAUV      PIC X(1755) VALUE ALL '0'.         00075400
M002- *       10 WS-CT-SAV-SAUV      PIC X(1275) VALUE ALL '0'.         00075400
                                                                        00052700
      *    ZONE DECRIVANT LES VARIABLES AUTRES SAUVEGARDEES             00052800
      *    IDENTIFIANT ANOMALIE (SIMPLE CHRONO)                         00052900
           05 WS-GR-VAR-SAV-SAUV.                                       00053000
              10 WS-ID-ANO-SAUV          PIC 9(016)  VALUE ZERO.        00053100
              10 WS-ID-ANO-FONC-SAUV     PIC 9(016)  VALUE ZERO.        00053200
                                                                        00053400
      ******************************************************************00053500
      *                   DONNEES PROPRES AU METIER                     00053600
      ******************************************************************00053700
      * CODE RETOUR PRIMAIRE DU SERVICE D'ORCHESTRATION                 00055200
       01 WS-CO-RET-1ER                        PIC S9(004).             00076600
                                                                        00055400
      * NUMERO DE LA REQUETE                                            00055500
       01 WS-NO-REQ-ACC                        PIC 9(002).              00055600
                                                                        00055700
       01 WS-CO-CONTXT-TRT-MB17                PIC X(01).               00077100
                                                                        00077200
       01 WS-GR-DT-ANG.                                                 00077300
           05  WS-DT-SS             PIC  X(02).                         00077400
           05  WS-DT-AA             PIC  X(02).                         00077500
           05  WS-DT-MM             PIC  X(02).                         00077600
           05  WS-DT-JJ             PIC  X(02).                         00077700
                                                                        00077800
       01 WS-GR-DT-AFC-ANG.                                             00077900
           05  WS-DT-SS             PIC  X(02).                         00078000
           05  WS-DT-AA             PIC  X(02).                         00078100
           05  WS-LB-SEP            PIC  X(01) VALUE '-'.               00078200
           05  WS-DT-MM             PIC  X(02).                         00078300
           05  WS-LB-SEP            PIC  X(01) VALUE '-'.               00078400
           05  WS-DT-JJ             PIC  X(02).                         00078500
                                                                        00078600
       01 WS-GR-ALL-DT-FISC.                                            00078700
           05 WS-GR-DT-FISC .                                           00078800
              10 WS-AN-FISC                PIC 9(4).                    00078900
              10 FILLER                    PIC X(6) VALUE '-01-01'.     00079000
           05 WS-GR-DEB-ANN-FISC.                                       00079100
              10 WS-DT-DEB-AN              PIC 9(4).                    00079200
              10 FILLER                    PIC X(01) VALUE '-'.         00079300
              10 WS-DT-DEB-MOIS            PIC X(02).                   00079400
              10 FILLER                    PIC X(01) VALUE '-'.         00079500
              10 WS-DT-DEB-JOUR            PIC X(02).                   00079600
           05 WS-GR-FIN-ANN-FISC.                                       00079700
              10 WS-DT-FIN-AN              PIC 9(04).                   00079800
              10 FILLER                    PIC X(01) VALUE '-'.         00079900
              10 WS-DT-FIN-MOIS            PIC X(02).                   00080000
              10 FILLER                    PIC X(01) VALUE '-'.         00080100
              10 WS-DT-FIN-JOUR            PIC X(02).                   00080200
                                                                        00080300
       01 WS-ID-TECH-ENV-RPRI              PIC S9(09) COMP.             00080700
       01 WS-ID-TECH-OBJ-DEST              PIC 9(09) VALUE ZERO.        00056000
                                                                        00056000
       01 WS-GR-ZONE-RPRI.
          05 WS-CO-REG-RPRI            PIC X     VALUE SPACE.
          05 WS-NO-INST-RPRI           PIC X(03) VALUE SPACE.
          05 WS-ID-TECH-INDV-RPRI      PIC 9(09) VALUE 0.


      *---------------------------------------------------------------*
      *DECLARATION POUR LES FLUX EDITIQUE
      *---------------------------------------------------------------*
      * IDENTIFIANT TECHNIQUE DU FLUX EDITIQUE
       01 WS-ID-TECH-FLX-EDIT      PIC X(13).
          88 FLX-EDIT-PGEINFOFISCAL           VALUE 'PGEINFOFISCAL'.
          88 FLX-EDIT-PGEDECFISCPCT           VALUE 'PGEDECFISCPCT'.
          88 FLX-EDIT-PGEDECFISCMAS           VALUE 'PGEDECFISCMAS'.

      * CODE BLOC EDITIQUE
       01 WS-CO-BLOC               PIC X(13).
          88 CO-BLOC-UINSTITUTION             VALUE 'UINSTITUTION '.
          88 CO-BLOC-UDESTINATAIRE            VALUE 'UDESTINATAIRE'.
          88 CO-BLOC-PREFCOURRIER             VALUE 'PREFCOURRIER '.
          88 CO-BLOC-PALLOCATAIRE             VALUE 'PALLOCATAIRE '.
          88 CO-BLOC-UINDIVIDU                VALUE 'UINDIVIDU    '.
M008+     88 CO-BLOC-UADRESSE                 VALUE 'UADRESSE     '.
          88 CO-BLOC-PDOCFISCAL               VALUE 'PDOCFISCAL   '.

M009+ *cet var contient la position de depart du premier bloc non unique
M009+ *ce nombre sera incremente pour generer les blocs multiple
M009+ *(ex : pdocfiscal)
M009+  01 WS-NB-BLOC-UNIQ            PIC 9(2) VALUE 7.

       01 WS-TB-BLOC-EDIT.
          05 WS-NB-OCC-BLOC-EDIT     PIC 9(003).
          05 WS-GR-BLOC-EDIT         OCCURS 100.
             10 WS-CO-BLOC-EDIT      PIC X(013).
             10 WS-NB-CAR-BLOC-EDIT  PIC 9(003).
             10 WS-NO-VERS-BLOC-EDIT PIC X(003).
             10 WS-VA-LIG-EDIT       PIC X(579).

      *---------------------------------------------------------------*
      *VARIABLE DE TRAVAIL
      *---------------------------------------------------------------*
       01 WS-CO-MSG-ANO-FONC             PIC X(010) VALUE SPACE.
       01 WS-LB-LNG-ANO-FONC             PIC X(255) VALUE SPACE.
                                                                        00056000
       01 WS-IN-CNTL-EXPLT-DATA          PIC X(01).
           88 CNTL-EXPLT-DATA-KO           VALUE '1'.
           88 CNTL-EXPLT-DATA-OK           VALUE '0'.

       01 WS-IN-CMPT-ALLOCATAIRE         PIC X(01).
           88 COMPTER-ALLOCATAIRE-KO       VALUE '1'.
           88 COMPTER-ALLOCATAIRE-OK       VALUE '0'.
                                                                        00056000
       01 WS-IN-EMISSION-ENVOI           PIC X(02).
           88 ENVOI-NON-EMIS               VALUE 'NE'.
           88 ENVOI-EMIS                   VALUE 'EM'.
                                                                        00056000
      * INDICATEUR DE LECTURE DE ULF SUIVANTE
       01 WS-IN-READ-ULF-NEXT         PIC X(1).
           88 READ-ULF-NEXT-OK        VALUE '1'.
           88 READ-ULF-NEXT-KO        VALUE '0'.

      *-- ENVOI OBJET D''UNE ANOMALIE METIER
       01 WS-GR-ENV-PEX.
          05 WS-CO-REG-PEX             PIC X(001).
          05 WS-NO-INST-DECL-PEX       PIC X(003).
          05 WS-ID-TECH-INDV-PEX       PIC 9(009).

      *-- CLE POUR FCGEEDF1
       01 WS-GR-CLE-EDF-LIF.
          05 WS-GR-CLE-EDF.
             10 WS-CO-REG-EDF             PIC X(001).
             10 WS-NO-INST-DECL-EDF       PIC X(003).
             10 WS-ID-TECH-INDV-EDF       PIC 9(009).
          05 WS-GR-CLE-LIF.
             10 WS-ID-TECH-DPR-LIF        PIC 9(009).

      * DERNIERE INSTITUTION TRAITEE AVANT LECTURE SUIVANTE
       01 WS-GR-INSTIT-PREC-CT.
           05 WS-CO-REGIME-PREC-CT        PIC X VALUE SPACE.
           05 WS-NO-INSTIT-PREC-CT        PIC X(3) VALUE SPACE.

      * DERNIER ALLOCATAIRE TRAITE AVANT LECTURE SUIVANTE
       01 WS-ID-TECH-INDV-PREC-CT        PIC 9(09) VALUE 0.

      *-- IDENTIFIANT DU DPR POUR GESTION RUPTURE
       01 WS-GR-CLE-EDF-LIF-PREC.
          05 WS-GR-CLE-EDF-PREC.
             10 WS-CO-REG-EDF-PREC        PIC X(001).
             10 WS-NO-INST-DECL-EDF-PREC  PIC X(003).
             10 WS-ID-TECH-INDV-EDF-PREC  PIC 9(009).
          05 WS-GR-CLE-LIF-PREC.
             10 WS-ID-TECH-DPR-LIF-PREC   PIC 9(009).

       01 WS-IN-PEC-DGI-EDF-PREC          PIC X(01).
       01 WS-IN-RESI-ETRG-EDF-PREC        PIC X(01).

      *-- CLE POUR FC99FIS1
       01 WS-GR-CLE-FIS.
          05 WS-CO-REG-FIS             PIC X(001).
          05 WS-NO-INST-DECL-FIS       PIC X(003).
          05 WS-ID-TECH-INDV-FIS       PIC 9(009).
      *-- CODE ENQUETE VAUT '00' OU '01'
       01 WS-CO-ENQT                   PIC X(02).
      *-- CONSERVATION CL POUR RESTITUTION INFORMATION INSTITUTION
       01 WS-CO-REG-SAVE               PIC X(001) VALUE SPACE.
       01 WS-NO-INST-SAVE              PIC X(003) VALUE SPACE.

       01 WS-NO-PRDE-LIF-9             PIC 9(4) VALUE 0.
       01 WS-NO-PRDE                   PIC 9(4) VALUE 0.

       01 WS-MT-VTIL-LIF-COMP          PIC S9(15) COMP.
       01 WS-IX-1                      PIC 9(03)  VALUE 0.
       01 WS-IX-0                      PIC 9(03)  VALUE 0.

       01 WS-ID-TECH-ENV-SAUV          PIC 9(11) VALUE 0.

       01 WS-CO-ACTE-GESTION           PIC X(10).
           88 ACTE-GESTION-EMIS-IF-PONC    VALUE 'EMISIFPONC'.
           88 ACTE-GESTION-EMIS-AF-AUTO    VALUE 'EMISSATFIA'.
           88 ACTE-GESTION-EMIS-AF-PONC    VALUE 'EMISSATFIP'.

       01 WS-CO-STATUT-CURSEUR-PPGEAL20   PIC X(01) VALUE '0'.
           88 PPGEAL20-IS-OPENED    VALUE '1'.
           88 PPGEAL20-IS-CLOSED    VALUE '0'.

       01 WS-CO-STATUT-CURSEUR-PPGEAL21   PIC X(01) VALUE '0'.
           88 PPGEAL21-IS-OPENED    VALUE '1'.
           88 PPGEAL21-IS-CLOSED    VALUE '0'.

                                                                        00056000
       01 WS-CO-GEST-PERIODE-MULTIP  PIC X(01) VALUE '0'.
           88 OK-RUPT-PERIOD-FISC   VALUE '1'.
           88 KO-RUPT-PERIOD-FISC   VALUE '0'.
                                                                        00056000
M002  *TEST TEST TEST
M002   01 WS-MT-BRUT-TEST           PIC S9(16)V99 COMP-3.
M002   01 WS-MT-BRUT-TEST-C         PIC +9(14),99.

M014+  01 WS-IN-REPRISE-INTERNE     PIC X(01) VALUE 'N'.
M017+      88 MB17-MODE-REPRISE-ANO-TECH VALUE 'T'.
M014+      88 MB17-MODE-REPRISE-ANO-FONC VALUE 'O'.
M014+      88 MB17-MODE-CREATION         VALUE 'N'.

M015+ * indicateur de lecture de toutes les lignes fiscale pour ULF
M015+  01 WS-IN-LECT-LIGNE-FISC-ULF      PIC X(01) VALUE '0'.
M015+ *       ttes LF ont ete lu
M015+      88 OUI-LECT-TTE-LF-ULF-OK     VALUE '1'.
M015+ *       ttes LF non pas ete lu
M015+      88 NON-LECT-TTE-LF-ULF        VALUE '0'.



M015+ * indicateur de probleme lors creation flux editique
M015+  01 WS-IN-ETAT-LCREAT-LUX          PIC X(01) VALUE '0'.
M015+      88 ARRET-CREATION-FLUX        VALUE '1'.
M015+      88 CONTINUE-CREAT-FLUX        VALUE '0'.




      ******************************************************************00056100
      *                     T R A I T E M E N T                         00056200
      ******************************************************************00056300
       PROCEDURE DIVISION.                                              00056400
      *-------------------                                              00056500
           PERFORM 10000-INITIALISATION                                 00056700
           PERFORM 20000-TRAITEMENT
           PERFORM 30000-FIN-PROGRAMME                                  00057100
           .                                                            00057200
      ******************************************************************00057300
       10000-INITIALISATION.
      *---------------------*
      *--  GESTION DE TRACE                                             00057600
           PERFORM 80000-DECL-TRACE                                     00057700
           PERFORM 80000-INIT-TRACE                                     00057800
                                                                        00057900
           IF CP00SV21-TRACE-ACTIVE                                     00058000
              MOVE '**PPGEMB17-10000-INITIALISATION'  TO LI-ID-DON-TRACE00058100
              SET       CP00SV21-NIV-PGM    TO TRUE                     00058200
              PERFORM   80000-ALIM-TRACE                                00058300
           END-IF                                                       00058400
                                                                        00058500
      *--  INITIALISATION DES VARIABLES DE SAUVEGARDE ET DES COMPTEURS  00058600
           INITIALIZE    WS-CT-FONC WS-GR-CLE-EDF  WS-GR-CLE-FIS
                         WS-CT-FONC-TEMP WS-GR-ZONE-RPRI
           SET READ-ULF-NEXT-KO TO TRUE
           MOVE SPACE TO WS-ID-CLE-RPRI                                 00058700
                         WS-ID-CLE-RPRI-SAUV                            00058800
                         WS-ID-CLE-ANO                                  00058900
                         WS-ID-CLE-ANO-SAUV                             00059000
                         WS-ID-BOR-INF                                  00059100
           MOVE ZERO  TO WS-NB-ANO                                      00059200
                         WS-NB-ANO-SAUV                                 00059300
                         WS-NB-ANO-PASS                                 00059400
                         WS-NB-OCC                                      00059500
                         WS-NB-OCC-SAUV                                 00059600
                         WS-NB-ROLL                                     00059700
                         WS-NB-ROLL-SAUV                                00059800
                         WS-ID-TECH-ENV-RPRI                            00085800
                         WS-CO-RET                                      00060000
                         WS-CO-RET-SAUV                                 00060100

           SET CNTL-EXPLT-DATA-OK TO TRUE                               00060200
                                                                        00060200
      *--  SERVICE DE GESTION DES DATES TECHNIQUE ET                    00060300
      *--  DES DATES D'EXPLOITATION                                     00060400
           PERFORM 40000-INITIALISER-PP00SVP2                           00060500
           PERFORM 80000-APPEL-PP00SVP2                                 00060600
                                                                        00060700
           MOVE LO-TS           TO    WS-TS-DATE                        00086700
           MOVE WS-DT-TS        TO    WS-DT-FONC                        00061000
           MOVE WS-TI-TS        TO    WS-GR-TI-AFF                      00061200
           MOVE WS-TI-TS        TO    WS-TI-TRT                         00061400
                                                                        00061500
      *--  RECHERCHE DES PARAMETRES MVS (ID-JOB, ID-STEP...)            00061600
           PERFORM 11000-RECH-MVS                                       00061700
                                                                        00061800
      *--  RECUPERATION DES PARAMETRES TECHNIQUES (PAS DE COMMIT, PAS DE00061900
      *--   DISPLAY...)                                                 00062000
           PERFORM 12000-GESTION-PARAM                                  00062100
                                                                        00062200
      *--  MEMORISATION DU CONTEXTE APPLICATIF ETENDU                   00062300
           PERFORM 40000-INITIALISER-PP00SVCA                           00062400
           PERFORM 80000-APPEL-PP00SVCA                                 00062500
                                                                        00062600
      *--  APPEL AU SERVICE DE RECUPERATION DE LA PLAGE D'IDENTIFIANTS  00062700
           PERFORM 13000-PLAGE-IDENTIFIANT                              00063200
                                                                        00063300
      *--  GESTION DU MODE DE REPRISE                                   00063400
           PERFORM 15000-INIT-RESTART                                   00064100
                                                                        00064200
      *--  CAS OU LE TRAITEMENT ETAIT EN COURS OU NON DEMARRE           00064300
           IF LO-CO-STATUT-STEP NOT = 'T'                               00064400
      *--     AJOUT D'UN INIT METIER TOUT MODE DE DEMARAGE              00064500
              PERFORM 60050-INIT-METIER-REP-OU-NON                      00064600
                                                                        00064700
      *--     CAS OU LE TRAITEMENT ETAIT EN COURS                       00064800
              IF CP00SV05-MODE-REPRISE                                  00064900
      *--        LE NB DE REPRISE A ETE RECUPERE EN NB_ROLL DANS LA     00065000
      *--        TABLE TZ00RST                                          00065100
      *--        INCREMENTATION DU COMPTEUR DU NBRE DE REPRISE (NB_ROLL)00065200
                 ADD 1 TO WS-NB-ROLL                                    00065300
      *--        SI LE NOMBRE DE REPRISE MAXIMUM N'A PAS ETE DEPASSE    00065400
                 IF WS-NB-ROLL       <= LO-NB-MAXI-RPRI                 00065500
                    MOVE 0           TO WS-NB-ANO-PASS                  00065600
      *--           GESTION DES COMPTES-RENDU                           00065700
                    PERFORM 40000-CPTRENDU-REPRISE                      00065800
      *--           INITIALISATION DES DONNEES METIER                   00065900
                    PERFORM 60100-INIT-METIER-REPRISE                   00066000
      *--           ON RECONDUIT LES CHAMPS DE LA TABLE RESTART         00066100
      *--           POUR CONSERVER CES DONNEES LORS DU PREMIER COMMIT   00066200
                    SET DERNIERE-OCC    TO TRUE                         00066300
                    PERFORM 40000-MAJ-RESTART                           00066400
                 ELSE                                                   00066500
      *--           GESTION DES COMPTES-RENDUS                          00066600
                    PERFORM 16000-CPTRENDU-MAXREP                       00066700
                    SET CO-RET-KO TO TRUE                               00066900
                    PERFORM 30000-FIN-PROGRAMME                         00067000
                 END-IF                                                 00067100
                                                                        00067200
      *--     CAS OU LE TRAITEMENT N'AVAIT PAS ENCORE DEMARRE           00067300
              ELSE                                                      00067400
      *--        INITIALISATION DES DONNEES METIER                      00067500
                 PERFORM 60000-INIT-METIER-DEMARRAGE                    00067600
      *--        MISE A JOUR DU CODE STATUT A 'EN COURS'                00067700
      *--        DANS LA TABLE DE RESTART                               00067800
                 SET CODE-STATUT TO TRUE                                00067900
                 MOVE 'E'   TO WS-CO-STUT-STEP                          00068000
                 PERFORM 40000-MAJ-RESTART                              00068100
      *          NETTOYAGE DE LA TABLE DES ANOS                         00068200
      *          ...                                                    00068300
      *           (EN ATTENTE DE LA REPONSE DU GSA SUR UNE EVOLUTION DU 00068400
      *           SERVICE BT DE GESTION DES ANOMALIES)                  00068500
                                                                        00068600
      *--        GESTION DES COMPTES-RENDU                              00068700
                 PERFORM 17000-CPTRENDU-DEMARRE                         00068800
              END-IF                                                    00068900
      *--     ON ENTERINE TOUTES LES ACTIONS PRECEDENTES PAR UN COMMIT  00069000
              PERFORM 40000-GESTION-COMMIT                              00069100
           END-IF                                                       00069200
                                                                        00069300
      *--  SI LE PROGRAMME A DEJA ETE EXECUTE ET EST TERMINE            00069400
      *--  ALORS SORTIE DU PROGRAMME AVEC LE MEME CODE RETOUR QUE       00069500
      *--  LA FOIS PRECEDENTE                                           00069600
           IF LO-CO-STATUT-STEP = 'T'                                   00069700
                                                                        00069800
      *--     APPEL AU SERVICE DE GESTION DES COMPTES-RENDUS DE STEP    00069900
      *--     EN MODE TERMINE                                           00070000
              PERFORM 40000-CPTRENDU-TERMINE                            00070100
              MOVE LO-CO-RET OF LK-CP00SV05 TO WS-CO-RET                00070300
              PERFORM 30000-FIN-PROGRAMME                               00070500
           END-IF                                                       00070600
      *==== RECUPERATION DU CONTEXTE D'EXECUTION DU PROGRAMME
      *-- INITIALISER LA COMMAREA D'APPEL AU MODULE SP                  00095000
           PERFORM 40000-INITIALISER-PPSP07I1                           00095100
M005+ *    VALORISE AVEC NOM DU JCL(EX:JPGEDA3)
M005+      MOVE LO-ID-JOB  OF LK-CP00SV09
M005+                              TO LI-CO-APPL OF LK-CPSP07I1
      *--  RENSEIGNER LE CODE CRITERE                                   00095400
           MOVE 'CONTXT-TRT'       TO LI-CO-CRIT OF LK-CPSP07I1
      *--  RENSEIGNER LA DATE                                           00095700
           MOVE WS-DT-FONC         TO LI-DT-DEFF OF LK-CPSP07I1         00095800
      *--  APPEL AU MODULE SP                                           00096000
           PERFORM 80000-APPELER-PPSP07I1                               00096100
           IF CPSP07I1-CO-RET-1ER-OK AND CPSP07I1-CO-RET-2ND-OK
              MOVE LO-LB-VA-CRIT(1)(1:1) TO WS-CO-CONTXT-TRT-MB17
           ELSE
              MOVE SPACE                 TO WS-CO-CONTXT-TRT-MB17
           END-IF

      *==== RECUPERATION DE ANNEE FISCALE SI CONTEXTE TRAITEMENT = 'M'
           IF WS-CO-CONTXT-TRT-MB17 = 'M'
              PERFORM 40000-INITIALISER-PPSP07I1
M005+ *       VALORISE AVEC NON JCL
M005+         MOVE LO-ID-JOB  OF LK-CP00SV09
                                        TO LI-CO-APPL OF LK-CPSP07I1
M004+         MOVE 'AN-FISC'            TO LI-CO-CRIT OF LK-CPSP07I1
              MOVE WS-DT-FONC           TO LI-DT-DEFF OF LK-CPSP07I1
              PERFORM 80000-APPELER-PPSP07I1
              IF CPSP07I1-CO-RET-1ER-OK AND CPSP07I1-CO-RET-2ND-OK
                 MOVE LO-LB-VA-CRIT(1)(1:4) TO WS-AN-FISCALE
              ELSE
                 MOVE SPACE                 TO WS-AN-FISCALE
              END-IF
           END-IF
           .                                                            00070700

      *--------------*                                                  00096300
       11000-RECH-MVS.                                                  00070900
      *--------------*                                                  00096500
      *--  GESTION DES TRACES                                           00071100
           IF CP00SV21-TRACE-ACTIVE                                     00071200
              MOVE    '==>MB17 11000-RECH-MVS'  TO LI-ID-DON-TRACE      00071300
              SET       CP00SV21-NIV-PAR    TO TRUE                     00071400
              PERFORM   80000-ALIM-TRACE                                00071500
           END-IF                                                       00071600
                                                                        00071700
      *--  APPEL AU SERVICE DE GESTION DE RECHERCHE DES VARIABLES MVS   00071800
           PERFORM 40000-INITIALISER-PP00SV09                           00071900
           PERFORM 80000-APPEL-PP00SV09                                 00072000
                                                                        00072100
           MOVE LO-ID-JOB OF LK-CP00SV09 TO WS-ID-UTIL-PRE              00072200
           MOVE 'PPGEMB17'          TO WS-ID-UTIL-SUF                   00072300
                                                                        00072400
      *--  ALIMENTER ID-JOB ET ID-STEP DANS LA ZONE TECHNIQUE GENERIQUE 00072500
      *--  DU PROGRAMME MAITRE                                          00072600
           MOVE LO-ID-JOB  OF LK-CP00SV09                               00098200
           TO LK-ID-JOB  OF LK-GR-PPGEMB17                              00098300
           MOVE LO-ID-STEP OF LK-CP00SV09                               00098400
           TO LK-ID-STEP OF LK-GR-PPGEMB17                              00098500
           .                                                            00072900
      *-------------------*                                             00098700
       12000-GESTION-PARAM.                                             00073100
      *-------------------*                                             00098900
      *--  GESTION DES TRACES                                           00073300
           IF CP00SV21-TRACE-ACTIVE                                     00073400
              MOVE '==>MB17 12000-GESTION-PARAM' TO LI-ID-DON-TRACE     00073500
              SET       CP00SV21-NIV-PAR    TO TRUE                     00073600
              PERFORM   80000-ALIM-TRACE                                00073700
           END-IF                                                       00073800
                                                                        00073900
      *--  RECUPERATION DU PAS DE COMMIT                                00074000
           PERFORM 12100-RECUP-PAS-COMMIT                               00074100
                                                                        00074200
      *--  RECUPERATION DE L'INTERVALLE DE DISPLAY, DU NOMBRE MAX D'ANO 00074300
      *--  DU NOMBRE MAX DE REPRISE ET DE L'INDICATEUR DE SYSOUT        00074400
           PERFORM 12200-RECUP-PARAM-TRT                                00074500
           .                                                            00074600
      *----------------------*                                          00100400
       12100-RECUP-PAS-COMMIT.                                          00074800
      *----------------------*                                          00100600
      *--  GESTION DES TRACES                                           00075000
           IF CP00SV21-TRACE-ACTIVE                                     00075100
              MOVE '==>MB17 12100-RECUP-PAS-COMMIT'  TO LI-ID-DON-TRACE 00075200
              SET       CP00SV21-NIV-PAR    TO TRUE                     00075300
              PERFORM   80000-ALIM-TRACE                                00075400
           END-IF                                                       00075500
                                                                        00075600
      *--  APPEL AU SERVICE DE RECUPERATION DES DONNEES DE LA TABLE     00075700
      *--  DES COMMIT                                                   00075800
           PERFORM 40000-INITIALISER-PP00SV10                           00075900
           PERFORM 80000-APPEL-PP00SV10                                 00076000
                                                                        00076100
      *--  POSITIONNEMENT D'UNE DEMANDE DE COMMIT                       00076200
           PERFORM 40000-INITIALISER-PP00SV25                           00076300
           SET CP00SV25-MODIF-PAS-COMMIT   TO TRUE                      00076400
           MOVE LO-NB-COMMIT OF LK-CP00SV10 TO LI-NB-COMMIT             00076500
                                                     OF LK-CP00SV25     00076600
           PERFORM 80000-APPEL-PP00SV25                                 00076700
           .                                                            00076800
      *---------------------*                                           00102600
       12200-RECUP-PARAM-TRT.                                           00077000
      *---------------------*                                           00102800
      *--  GESTION DES TRACES                                           00077200
           IF CP00SV21-TRACE-ACTIVE                                     00077300
              MOVE '==>MB17 12200-RECUP-PARAM-TRT' TO LI-ID-DON-TRACE   00077400
              SET       CP00SV21-NIV-PAR    TO TRUE                     00077500
              PERFORM   80000-ALIM-TRACE                                00077600
           END-IF                                                       00077700
                                                                        00077800
      *--  APPEL AU SERVICE DE RECUPERATION DES DONNEES DE LA TABLE     00077900
      *--  DE SURVEILLANCE D'EXECUTION                                  00078000
           PERFORM 40000-INITIALISER-PP00SV11                           00078100
           PERFORM 80000-APPEL-PP00SV11                                 00078200
           .                                                            00078300
      ******************************************************************00078400
      * INITIALISATION DES PARAMETRES D'APPEL DU SERVICE DE RECHERCHE   00078500
      * DE LA PLAGE D'IDENTIFIANT                                       00078600
      ******************************************************************00078700
      *-----------------------*                                         00104500
       13000-PLAGE-IDENTIFIANT.
      *-----------------------*                                         00104700
      *--  GESTION DES TRACES                                           00079000
           IF CP00SV21-TRACE-ACTIVE                                     00079100
              MOVE '==>MB17 13000-PLAGE-IDENTIFIANT' TO LI-ID-DON-TRACE 00079200
              SET       CP00SV21-NIV-PAR     TO TRUE                    00079300
              PERFORM   80000-ALIM-TRACE                                00079400
           END-IF                                                       00079500
                                                                        00079600
      *--  APPEL AU SERVICE DE GESTION DE RECHERCHE DE LA PLAGE         00079700
      *--  D'IDENTIFIANT                                                00079800
           PERFORM 40000-INITIALISER-PP00SV12                           00079900
           PERFORM 80000-APPEL-PP00SV12                                 00080000
           .                                                            00080100
      ******************************************************************00080200
       15000-INIT-RESTART.
      *-------------------                                              00080400
      *--  GESTION DES TRACES                                           00080500
           IF CP00SV21-TRACE-ACTIVE                                     00080600
              MOVE '==>MB17 15000-INIT-RESTART'  TO LI-ID-DON-TRACE     00080700
              SET       CP00SV21-NIV-PAR    TO TRUE                     00080800
              PERFORM   80000-ALIM-TRACE                                00080900
           END-IF                                                       00081000
                                                                        00081100
      *--  APPEL AU SERVICE DE GESTION DES REPRISES                     00081200
           PERFORM 40000-INITIALISER-PP00SV05                           00081300
           SET CP00SV05-FONC-INIT TO TRUE                               00081400
           MOVE WS-DT-FONC        TO LI-DT-RST    OF LK-CP00SV05        00081500
           PERFORM 80000-APPEL-PP00SV05                                 00081600
                                                                        00081700
           MOVE LO-CO-STATUT-STEP      TO WS-CO-STUT-STEP               00081800
                                          WS-CO-STUT-SAUV               00081900
                                                                        00082000
      *--  RECUPERATION DE LA CLE DE REPRISE                            00082100
           IF CP00SV05-MODE-REPRISE                                     00082200
              MOVE LO-ID-CLE-RPRI      TO WS-ID-CLE-RPRI                00082300
                                          WS-ID-CLE-RPRI-SAUV           00082400
                                          WS-ID-BOR-INF                 00082500

              PERFORM 28000-ALIM-VAR-AFTER-REPRISE
           END-IF                                                       00082600
                                                                        00082700
           MOVE LO-ID-CLE-ANO          TO WS-ID-CLE-ANO                 00082800
                                          WS-ID-CLE-ANO-SAUV            00082900
           MOVE LO-NB-ANO              TO WS-NB-ANO                     00083000
                                          WS-NB-ANO-SAUV                00083100
           MOVE LO-NB-OCC              TO WS-NB-OCC                     00083200
                                          WS-NB-OCC-SAUV                00083300
           MOVE LO-NB-ROLL             TO WS-NB-ROLL                    00083400
                                          WS-NB-ROLL-SAUV               00083500
      *--  RECONDUCTION DE LA DATE ISSUE DE LA TABLE RESTART            00083600
           MOVE LO-DT-RST              TO WS-DT-FONC                    00083700
      *--  ON INITIALISE LES DONNEES DE TRAVAIL METIER AVEC CELLES      00083800
      *--  SAUVEGARDEES PRECEDEMMENT                                    00083900
           MOVE LO-LB-SAUV             TO WS-GR-SAV                     00084000
                                          WS-GR-SAV-SAUV                00084100
           IF LO-LB-SAUV (1:1) NOT NUMERIC                              00110600
              MOVE SPACE TO WS-GR-SAV WS-GR-SAV-SAUV                    00110700
              INSPECT WS-GR-SAV       REPLACING ALL SPACE BY '0'        00110800
              INSPECT WS-GR-SAV-SAUV  REPLACING ALL SPACE BY '0'        00110900
           ELSE                                                         00111000
              MOVE LO-LB-SAUV          TO WS-GR-SAV                     00111100
           END-IF                                                       00111200
                                                                        00111300
           MOVE WS-CT-SAV              TO WS-CT-FONC                    00111400
                                                                        00111500
      *--  LORS D'UNE REPRISE LES ID-ANO ET ID-ANO-FONC RECOMMENCENT    00084200
      *--  A ZERO PUISQUE C'EST UN NOUVEAU NUMERO DE JOB                00084300
           INITIALIZE WS-ID-ANO                                         00084400
                      WS-ID-ANO-SAUV                                    00084500
                      WS-ID-ANO-FONC                                    00084600
                      WS-ID-ANO-FONC-SAUV                               00084700

M017+      SET MB17-MODE-REPRISE-ANO-TECH TO TRUE
           .                                                            00084800
                                                                        00112300
      ******************************************************************00084900
      * INITIALISATION DES PARAMETRES D'APPEL AU SERVICE DE GESTION     00085000
      * DES COMPTES RENDUS EN MODE DEPASSEMENT SUR LE NOMBRE DE REPRISE 00085100
      ******************************************************************00085200
       16000-CPTRENDU-MAXREP.                                           00085300
      *----------------------                                           00085400
      *--  GESTION DES TRACES                                           00085500
           IF CP00SV21-TRACE-ACTIVE                                     00085600
              MOVE '==>MB17 16000-CPTRENDU-MAXREP' TO LI-ID-DON-TRACE   00085700
              SET       CP00SV21-NIV-PAR    TO TRUE                     00085800
              PERFORM   80000-ALIM-TRACE                                00085900
           END-IF                                                       00086000
                                                                        00086100
      *--  APPEL AU SERVICE DE GESTION DES COMPTES RENDUS               00086200
           MOVE    1 TO WS-CO-TYPE-LIG-CRU                              00086300
           PERFORM 40000-INITIALISER-PP00SV04                           00086400
           PERFORM 80000-APPEL-PP00SV04                                 00086500
           .                                                            00086600
      ******************************************************************00086700
      * INITIALISATION DES PARAMETRES D'APPEL AU SERVICE DE GESTION     00086800
      * DES COMPTES RENDUS EN MODE DEMARRE                              00086900
      ******************************************************************00087000
       17000-CPTRENDU-DEMARRE.                                          00087100
      *------------------------                                         00087200
      *--  GESTION DES TRACES                                           00087300
           IF CP00SV21-TRACE-ACTIVE                                     00087400
              MOVE '==>MB17 17000-CPTRENDU-DEMARRE' TO LI-ID-DON-TRACE  00087500
              PERFORM 88888-NIV-TRACE-SHOW                              00087700
           END-IF                                                       00087800
                                                                        00087900
           MOVE    2 TO WS-CO-TYPE-LIG-CRU                              00088000
           PERFORM 40000-INITIALISER-PP00SV04                           00088100
           PERFORM 80000-APPEL-PP00SV04                                 00088200
                                                                        00088300
           MOVE    3 TO WS-CO-TYPE-LIG-CRU                              00088400
           PERFORM 40000-INITIALISER-PP00SV04                           00088500
           PERFORM 80000-APPEL-PP00SV04                                 00088600
                                                                        00088700
           MOVE    4 TO WS-CO-TYPE-LIG-CRU                              00088800
           PERFORM 40000-INITIALISER-PP00SV04                           00088900
           PERFORM 80000-APPEL-PP00SV04                                 00089000
                                                                        00089100
           MOVE    5 TO WS-CO-TYPE-LIG-CRU                              00089200
           PERFORM 40000-INITIALISER-PP00SV04                           00089300
           PERFORM 80000-APPEL-PP00SV04                                 00089400
           .                                                            00089500
                                                                        00089600
      *----------------*                                                00117200
       20000-TRAITEMENT.
      *----------------*                                                00117400
      *--  GESTION DES TRACES                                           00090000
           IF CP00SV21-TRACE-ACTIVE                                     00090100
              MOVE '*--PPGEMB17 20000-TRAITEMENT--*' TO LI-ID-DON-TRACE 00090200
              PERFORM 88888-NIV-TRACE-SHOW                              00090400
           END-IF                                                       00090500
                                                                        00090600
           EVALUATE WS-CO-CONTXT-TRT-MB17
               WHEN 'M'
                  PERFORM 21000-TRT-MASSE
               WHEN 'P'
                  PERFORM 22000-TRT-PONCTUEL
               WHEN OTHER
                  display 'MB17 Code contxt-trt invalide'
                  PERFORM 31000-ABANDON
            END-EVALUATE
            .
      *---------------*
       21000-TRT-MASSE.
      *---------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '**MB17 21000-TRT-MASSE'
                TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
                                                                        00121500
      *--  RECUPERATION DU PARAMETRE DU BATCH                           00083500
           PERFORM 40000-OPEN-FCGEEDF1                                  00083600
           PERFORM 40000-OPEN-FC99FIS1                                  00083600

      *    IF WS-CO-CONTXT-TRT-MB17 NOT = 'M' AND 'P'                   00083900
      *       PERFORM 31000-ABANDON                                     00084000
      *    END-IF                                                       00084100
                                                                        00084200
           PERFORM 21100-TRT-METIER-MASSE UNTIL EOF-FCGEEDF1

      *==  COMPTE-RENDU METIER DE FIN POUR DERNIER INSTITUTION TRAITEE
           PERFORM 26000-ALIM-CRF-MET
           PERFORM 26000-ALIM-CRF-MET-MT

      *==  COMPTE-RENDU METIER TOTAL INSTITUTION
           PERFORM 26200-ALIM-CRF-INST

      *==  CREER 1 TRACE DE GESTION GLOBALE (PRG1103)
           SET ACTE-GESTION-EMIS-AF-AUTO TO TRUE
           PERFORM 27000-EMETTRE-TRACE-GESTION

           PERFORM 24000-DERNIERE-OCC
           .

      *-----------------------*
       21100-TRT-METIER-MASSE.
      *-----------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              STRING '**MB17 21100-TRT-METIER-MASSE'
              '  WS-ID-CLE-ANO:'WS-ID-CLE-ANO ';'
              DELIMITED BY SIZE INTO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
                                                                        00121500
      *-- CREATION COMPTE RENDU FONCTIONNEL POUR INSTITUTION
?          IF (WS-GR-INSTIT-PREC-CT NOT = WS-GR-CLE-EDF(1:4))
?          AND (WS-GR-INSTIT-PREC-CT NOT = SPACE)
m015+      AND NOT MB17-MODE-REPRISE-ANO-FONC
      *--     gestion des traces
M015+         IF CP00SV21-TRACE-ACTIVE
M015+            STRING ' MB17 GENERATE-CRF*  '
M015+            'WS-GR-INSTIT-PREC-CT='WS-GR-INSTIT-PREC-CT '<'
M015+            ' WS-GR-CLE-EDF='WS-GR-CLE-EDF(1:13) '<'
M015+            ' WS-ID-CLE-PREC:'WS-ID-CLE-PREC '<'
M015+            DELIMITED BY SIZE INTO LI-ID-DON-TRACE
M015+            PERFORM 88888-NIV-TRACE-SHOW
M015+         END-IF
                                                                        00121500
?             IF WS-GR-INSTIT-PREC-CT NOT = SPACE AND LOW-VALUE
?                PERFORM 26000-ALIM-CRF-MET
?                PERFORM 26000-ALIM-CRF-MET-MT
?             END-IF
?
?             ADD 1 TO WS-NB-TOT-INSTIT
?
?             MOVE WS-GR-CLE-EDF(1:4) TO WS-GR-INSTIT-PREC-CT
?             INITIALIZE WS-CT-FONC
?          END-IF

M015+ *--  existant erreur lors creation flux editique
M015+      IF ARRET-CREATION-FLUX
M015+         IF CP00SV21-TRACE-ACTIVE
M015+            string '!! KO creation flux pr:'
M015+            WS-ID-UNIT-FONC-PREC ' Ou ' WS-ID-UNIT-FONC-LUE
M015+            'WS-ID-CLE-ANO:'WS-ID-CLE-ANO ';'
M015+            delimited by size inTO LI-ID-DON-TRACE
M015+            PERFORM 88888-NIV-TRACE-SHOW
M015+         END-IF

M015+         SET MB17-MODE-REPRISE-ANO-FONC TO TRUE
M017- *       MOVE WS-ID-CLE-PREC TO WS-ID-CLE-ANO(1:13)

M015+         IF CP00SV21-TRACE-ACTIVE
M015+            STRING 'MB17  FIN-KO CREATION FLUX PR:'
M015+            'WS-ID-CLE-ANO:'WS-ID-CLE-ANO(1:13) ';'
M015+            DELIMITED BY SIZE INTO LI-ID-DON-TRACE
M015+            PERFORM 88888-NIV-TRACE-SHOW
M015+         END-IF
M015+      END-IF


                                                                        00121500
      *--  MODE REPRISE?
           PERFORM 21110-MODE-REPRISE

      *    LECTURE DE ULF                                               00118400
           IF CNTL-EXPLT-DATA-KO
      *-- CHANGER LA CL (CO-REG, NO-INST-DCLA, ID-TECH-INDV,ID-TECH-ENV
            PERFORM  25000-COMPTEUR-ALL-ANO
              INITIALIZE WS-CT-FONC-TEMP
              SET CNTL-EXPLT-DATA-OK TO TRUE
           ELSE
              IF READ-ULF-NEXT-KO
                 PERFORM 41000-LECTURE-FCGEEDF1
              END-IF
           END-IF

M014+      PERFORM  99999-TRT-M-SHOW01                                  00119500
                                                                        00119500
           IF WS-ID-CLE-ANO(1:13) = WS-GR-CLE-EDF
              PERFORM 23100-TRAITEMENT-ANO
           ELSE
              IF OK-FCGEEDF1
                SET ENVOI-NON-EMIS TO TRUE
                PERFORM 20100-CNTL-DONNEES
                IF CNTL-EXPLT-DATA-OK
                   PERFORM 41000-LECTURE-FC99FIS1 UNTIL
                   WS-GR-CLE-EDF = WS-GR-CLE-FIS OR EOF-FC99FIS1
                   IF OK-FC99FIS1
      *--            RESTRICTION: NOTION DE RETOUR NPAI NON GEREE
                     PERFORM 20200-ATTRIB-CODE-ENQUETE
M015+                SET CONTINUE-CREAT-FLUX TO TRUE
                     PERFORM 20300-CREATION-FLUX-ED

                     IF CPSP12C1-CO-RET-1ER-OK
                     AND CPSP12C1-CO-RET-2ND-OK
M015+                AND CONTINUE-CREAT-FLUX
                        SET ENVOI-EMIS TO  TRUE

      *--               GESTION DES TRACES
                        IF CP00SV21-TRACE-ACTIVE
                           STRING '< ========================'
                             '======> MB17 FLX EMIS <'
                             WS-ID-UNIT-FONC-PREC
                           DELIMITED BY SIZE
                           INTO LI-ID-DON-TRACE
                           PERFORM 88888-NIV-TRACE-SHOW
                        END-IF

      *--               MISE A JOUR EDF
                        PERFORM 25000-UPDATE-AF-EDF
                        PERFORM 28000-CNTL-COMMIT
                        ADD 1      TO WS-NB-OCC
M015+
M015+                   SET NON-LECT-TTE-LF-ULF TO TRUE
M015+ *                 signal de lecture ulf suivante
M015+                   SET CNTL-EXPLT-DATA-OK TO TRUE
M015+                   SET READ-ULF-NEXT-KO TO TRUE
                     END-IF
                   END-IF
                END-IF
              END-IF
           END-IF
           .

      *-----------------------*
       21110-MODE-REPRISE.
      *-----------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 21110-MODE-REPRISE'
                TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
                                                                        00121500
      *--  REPOSITIONNEMENT SUR L'ULF (NORMAL, REPRISE)
      *CONTEXTE DE MASSE ON SE POSITIONNE SUR L'ULF COMMIT2
           IF CP00SV05-MODE-REPRISE                                     			
M014+      OR MB17-MODE-REPRISE-ANO-FONC
M017+      OR MB17-MODE-REPRISE-ANO-TECH
TRC           display 'CNTXT:' WS-CO-CONTXT-TRT-MB17 '->'               			
TRC                   '*MODE REPRISE*'
TRC                   'CLE-RPRIS:'WS-ID-CLE-RPRI(1:13) '<'
M015+                 'WS-GR-CLE-EDF:'WS-GR-CLE-EDF '<'
M015+         display 'WS-GR-ZONE-RPRI:'WS-GR-ZONE-RPRI (1:13) '<'
M015+                 'WS-ID-CLE-ANO: 'WS-ID-CLE-ANO (1:13)';'
M015+                 'WS-ID-UNIT-FONC-LUE:'WS-ID-UNIT-FONC-LUE '<'
      *       MOVE SPACE  TO WS-ID-INST-PREC WS-CO-REG-PREC
                                                                        			
              IF (WS-ID-CLE-RPRI(1:1) = SPACE OR LOW-VALUE) OR
                 (WS-ID-CLE-RPRI(2:3) = SPACE OR LOW-VALUE) OR
                 (WS-ID-CLE-RPRI(5:9) NOT NUMERIC)

                 SET READ-ULF-NEXT-KO TO TRUE
              ELSE
                 IF WS-CO-CONTXT-TRT-MB17 = 'P'
                   PERFORM 50100-RESTIT-OCC                               			
                   UNTIL WS-GR-ZONE-RPRI = WS-ID-UNIT-FONC-LUE
                   SET READ-ULF-NEXT-KO TO TRUE
                 END-IF

                 IF WS-CO-CONTXT-TRT-MB17 = 'M'
                    PERFORM 41000-LECTURE-FCGEEDF1 UNTIL
                    (WS-GR-CLE-EDF > WS-GR-ZONE-RPRI)
                    OR EOF-FCGEEDF1

                    SET READ-ULF-NEXT-OK TO TRUE
                 END-IF
              END-IF

              SET CNTL-EXPLT-DATA-OK     TO TRUE
              SET CP00SV05-MODE-CREATION  TO TRUE                       			
M014+         SET MB17-MODE-CREATION      TO TRUE                       			
           END-IF                                                       00118400
           .                                                            00093500
M019+ *-----------------------
M019+  21100-TRAIT-PPGAAL01.
M019+ *-----------------------
M019+ *--  GESTION DES TRACES
M019+      IF CP00SV21-TRACE-ACTIVE
M019+         STRING 'MB17-21100-TRAIT-PPGAAL01 :'
M019+         DELIMITED BY SIZE             INTO LI-ID-DON-TRACE
M019+         SET CP00SV21-NIV-PAR            TO TRUE
M019+         PERFORM 80000-ALIM-TRACE
M019+      END-IF
M019+ *--
M019+      MOVE 01 TO WS-NO-REQ-ACC
M019+      PERFORM 40000-INITIALISER-PPGAAL01
M019+      SET CPGAAL01-SELECT TO TRUE
M019+ *--
M019+ *--  Identifiant technique DPR (issue de ppgeal21 ou fichier edf
M019+      MOVE WS-ID-TECH-DPR-LIF
M019+        TO LI-DPR-ID-TECH-DPR  OF LK-CPGAAL01
M019+ *--
M019+      PERFORM 80000-APPELER-PPGAAL01
M019+ *--
M019+ *--  GESTION DES TRACES
M019+      IF CP00SV21-TRACE-ACTIVE
M019+         STRING 'MB17-F-21100-TRAIT-PPGAAL01 '
M019+         ' ID-DPR->' WS-ID-TECH-DPR-LIF
M019+         DELIMITED BY SIZE             INTO LI-ID-DON-TRACE
M019+         SET CP00SV21-NIV-PAR            TO TRUE
M019+         PERFORM 80000-ALIM-TRACE
M019+      END-IF
M019+      .

                                                                        00121500
      *-----------------*
       22000-TRT-PONCTUEL.
      *-----------------*
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 22000-TRT-PONCTUEL'
                TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF

           MOVE SPACE  TO WS-CO-REG-EDF-PREC WS-NO-INST-DECL-EDF-PREC

      *--  SERVICE DE RECHERCHE DES OCCURRENCES A TRAITER
           PERFORM 22001-OPEN-CURSEUR-ULF
           SET OCC-A-TRAITER TO TRUE
           SET CNTL-EXPLT-DATA-OK TO TRUE
           SET READ-ULF-NEXT-KO TO TRUE

      *--  TRAITEMENT METIER
           PERFORM 22100-TRT-METIER-PONCTUEL UNTIL
           CPGEAL20-FIN-CURSEUR

      *==  COMPTE-RENDU METIER DE FIN POUR DERNIER INSTITUTION TRAITEE
           PERFORM 26000-ALIM-CRF-MET-PONCTUEL

           PERFORM 24000-DERNIERE-OCC
           .

      *---------------------*
       22001-OPEN-CURSEUR-ULF.
      *---------------------*
      *--  OPEN CURSEUR SUR TPGEEDF POUR 'EC', 'P'
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 22001-OPEN-CURSEUR-ULF'
                TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF

           PERFORM 40000-INITIALISER-PPGEAL20

           MOVE WS-CO-CONTXT-TRT-MB17
             TO LI-EDF-CO-MOD-CREA-ENV           OF LK-CPGEAL20
           MOVE 'EC'    TO LI-EDF-CO-STUT-ENV-AF OF LK-CPGEAL20

           SET CPGEAL20-OPEN TO TRUE
           PERFORM 80000-APPEL-PPGEAL20
           .

      *--------------------------*
       22100-TRT-METIER-PONCTUEL.
      *-------------------------*
      *--  CREATION OMPTE RENDU
      *-- CREATION COMPTE RENDU FONCTIONNEL POUR INSTITUTION
?     *    IF (WS-GR-INSTIT-PREC-CT NOT = WS-GR-CLE-EDF(1:4))
?     *    AND (WS-GR-INSTIT-PREC-CT NOT = SPACE)
?     *       IF WS-GR-INSTIT-PREC-CT NOT = SPACE AND LOW-VALUE
?     *          PERFORM 26000-ALIM-CRF-MET
?     *          PERFORM 26000-ALIM-CRF-MET-MT
?     *       END-IF
?     *
?     *       ADD 1 TO WS-NB-TOT-INSTIT
?     *
?     *       MOVE WS-GR-CLE-EDF(1:4) TO WS-GR-INSTIT-PREC-CT
?     *       INITIALIZE WS-CT-FONC
?     *    END-IF

           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 22100-TRT-METIER-PONCTUEL'
                TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE
              PERFORM   80000-ALIM-TRACE
           END-IF


      *--  MODE REPRISE-PONCTUEL
           PERFORM 21110-MODE-REPRISE

      *    LECTURE DE ULF
      *    IF CNTL-EXPLT-DATA-KO
      *       PERFORM  25000-COMPTEUR-ALL-ANO
      *       INITIALIZE WS-CT-FONC-TEMP
              SET CNTL-EXPLT-DATA-OK TO TRUE
      *    ELSE
              IF READ-ULF-NEXT-KO
      *--        LECTURE ULF A TRAITER
                 PERFORM 50100-RESTIT-OCC
              END-IF
      *    END-IF

      *-- INITIALISATION DE LA ZONE SOURCE ERREUR PEX
           INITIALIZE WS-GR-ENV-PEX

           IF WS-ID-CLE-ANO(1:13) = WS-GR-CLE-EDF
              PERFORM 23100-TRAITEMENT-ANO
           ELSE
              IF CPGEAL20-CO-RET-1ER-OK AND CPGEAL20-CO-RET-2ND-OK
                SET ENVOI-NON-EMIS TO TRUE
                PERFORM 20100-CNTL-DONNEES

      *--       NB ENVOI LU
                ADD 1 TO WS-NB-AL-TOT-1

                IF CNTL-EXPLT-DATA-OK
      *--          RESTRICTION: NOTION DE RETOUR NPAI NON GEREE
                   PERFORM 20200-ATTRIB-CODE-ENQUETE
                   PERFORM 20300-CREATION-FLUX-ED

                   IF CPSP12C1-CO-RET-1ER-OK
                   AND CPSP12C1-CO-RET-2ND-OK
                      SET ENVOI-EMIS TO TRUE

      *--               GESTION DES TRACES
                        IF CP00SV21-TRACE-ACTIVE
                           STRING '< PONCTUEL ====================='
                             '+++===> MB17 FLX EMIS' DELIMITED BY SIZE
                           INTO LI-ID-DON-TRACE
                           PERFORM 88888-NIV-TRACE-SHOW
                        END-IF

                      PERFORM 25000-UPDATE-AF-EDF
      *==             CREER 1 TRACE DE GESTION GLOBALE (PRG1103)
                      PERFORM 27000-EMETTRE-TRACE-GESTION

      *--             NB ENVOI TRAITE
                      ADD 1 TO WS-NB-AL-AF-ENV-6
      *--             CONTROLE DU PAS DE COMMIT
                      PERFORM 28000-CNTL-COMMIT

      *--             COMPTEUR POUR COMPTE-RENDU TZ00CRU
                      ADD 1 TO WS-NB-OCC

      *--             SIGNAL DE LECTURE ULF SUIVANTE
                      SET CNTL-EXPLT-DATA-OK TO TRUE
                      SET READ-ULF-NEXT-KO TO TRUE
                   END-IF
                END-IF
              END-IF
           END-IF
           .

      *-------------------------*
       20100-CNTL-DONNEES.
      *-------------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
M015+         STRING '**MB17 20100-CNTL-DONNEES'
              '       WS-GR-CLE-EDF:'WS-GR-CLE-EDF'<'
              DELIMITED BY SIZE
              INTO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE
              PERFORM   80000-ALIM-TRACE
           END-IF
                                                                        00121500
      *== CONTROLE INDICATEUR ENVOI AF
      *-- SI ERREUR TECNIQUE --  ROLLBACK ARRET TRTMT
           SET CNTL-EXPLT-DATA-OK TO TRUE
           INITIALIZE WS-GR-ENV-PEX

           IF WS-CO-CONTXT-TRT-MB17 = 'M'
              PERFORM 20100-CNTL-DONNEES-M
              IF CNTL-EXPLT-DATA-KO
                 MOVE LI-EDF-ID-TECH-ENV OF WS-GR-EDF
                 TO WS-ID-TECH-ENV-SAUV
              END-IF
           END-IF
           IF WS-CO-CONTXT-TRT-MB17 = 'P'
              PERFORM 20100-CNTL-DONNEES-P
              IF CNTL-EXPLT-DATA-KO
                 MOVE LO-EDF-ID-TECH-ENV OF LK-CPGEAL20
                   TO WS-ID-TECH-ENV-SAUV

                 ADD 1 TO WS-NB-AL-ANO-BLQ-AF-2
              END-IF
           END-IF

           IF CNTL-EXPLT-DATA-KO
      *--     CREATION ANOMALIE FONCTIONNEL (TP00ANO)
              MOVE WS-GR-CLE-EDF TO WS-GR-ENV-PEX

              PERFORM 20110-INIT-ERR-MET
              PERFORM 23340-GESTION-ANO-FONC

      *--     MISE A JOUR EDF
              PERFORM 25000-UPDATE-AF-EDF

              INITIALIZE LK-GR-ANO OF WS-GR-ERR
           END-IF
           .


      *-------------------------*                                       00121600
       20100-CNTL-DONNEES-M.
      *-------------------------*
           IF LI-EDF-ID-TECH-OBJ-DEST OF WS-GR-EDF = 0 OR LOW-VALUE
              SET CNTL-EXPLT-DATA-KO TO TRUE
              MOVE 'PGE0000052' TO WS-CO-MSG-ANO-FONC
           ELSE
              IF LI-EDF-IN-ENV-AF OF WS-GR-EDF = 'N'
                 SET CNTL-EXPLT-DATA-KO TO TRUE
                 MOVE 'PGE0000052' TO WS-CO-MSG-ANO-FONC
              ELSE
                 IF LI-EDF-IN-ANO-NOM OF WS-GR-EDF = 'O'
                    SET CNTL-EXPLT-DATA-KO TO TRUE
                    MOVE 'PGE0000053' TO WS-CO-MSG-ANO-FONC
                 END-IF
              END-IF
           END-IF
           .

      *-------------------------*
       20100-CNTL-DONNEES-P.
      *-------------------------*
           MOVE LO-EDF-ID-TECH-OBJ-DEST OF LK-CPGEAL20
             TO WS-ID-TECH-OBJ-DEST

           IF WS-ID-TECH-OBJ-DEST   = 0 OR
             (WS-ID-TECH-OBJ-DEST   NOT NUMERIC)
              SET CNTL-EXPLT-DATA-KO    TO TRUE
              MOVE 'PGE0000052'         TO WS-CO-MSG-ANO-FONC
           ELSE
              IF LO-EDF-IN-ENV-AF       OF LK-CPGEAL20 = 'N'
                 SET CNTL-EXPLT-DATA-KO TO TRUE
                 MOVE 'PGE0000052'      TO WS-CO-MSG-ANO-FONC
              ELSE
                 IF LO-EDF-IN-ANO-NOM   OF LK-CPGEAL20 = 'O'
                    SET CNTL-EXPLT-DATA-KO TO TRUE
                    MOVE 'PGE0000053'   TO WS-CO-MSG-ANO-FONC
                 END-IF
              END-IF
           END-IF
           .

      *-------------------------*
       20110-INIT-ERR-MET.
      *-------------------------*
           INITIALIZE LK-GR-ANO            OF WS-GR-ERR
           MOVE  4    TO LK-CO-RET-1ER-INI OF WS-GR-ERR
           SET ERR-ERREUR-FONC             TO TRUE
           MOVE  +4   TO LK-CO-RET-2ND-INI OF WS-GR-ERR
           SET ERR-MESSAGE-INFORMATION     TO TRUE

           MOVE 'PPGEMB17' TO LK-ID-PGM-APEL-ERR      OF WS-GR-ERR
           MOVE 'PPGEMB17' TO LK-ID-PGM-ERR           OF WS-GR-ERR


           MOVE WS-CO-MSG-ANO-FONC         TO LK-CO-MSG-MOD OF WS-GR-ERR
           STRING LI-GR-EDF OF WS-GR-EDF (1:57)
           DELIMITED BY SIZE INTO LK-LB-CRIT-REQ OF WS-GR-ERR
           .

      *-------------------------*
       20200-ATTRIB-CODE-ENQUETE.
      *-------------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '**MB17 20200-ATTRIB-CODE-ENQUETE'
                TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
                                                                        00121500
      *// DETERMINATION DU CODE ENQUETE
           IF WS-CO-CONTXT-TRT-MB17 = 'M'
              IF LI-EDF-IN-DC OF WS-GR-EDF NOT = 'O'
                 MOVE '01' TO WS-CO-ENQT
              ELSE
                 PERFORM 40000-INITIALISER-PPGAAL18
                 MOVE 5 TO LK-NO-REQ-ACC OF LK-CPGAAL18
                 SET CPGAAL18-SELECT TO TRUE

                 MOVE LI-EDF-ID-TECH-INDV OF WS-GR-EDF
                   TO LI-REM-ID-TECH-INDV OF LK-CPGAAL18

                 MOVE 'O'
                    TO LI-REM-IN-UTIL-AD-MAND OF LK-CPGAAL18
                 PERFORM 80000-APPEL-PPGAAL18

                 IF CPGAAL18-CO-RET-1ER-OK
                   MOVE '01' TO WS-CO-ENQT
                 ELSE
                   MOVE '00' TO WS-CO-ENQT
                 END-IF
              END-IF
           END-IF

           IF WS-CO-CONTXT-TRT-MB17 = 'P'
              IF LO-EDF-IN-DC                 OF LK-CPGEAL20 NOT = 'O'
                 MOVE '01' TO WS-CO-ENQT
              ELSE
                 PERFORM 40000-INITIALISER-PPGAAL18
                 MOVE 5 TO LK-NO-REQ-ACC      OF LK-CPGAAL18
                 SET CPGAAL18-SELECT          TO TRUE

                 MOVE LO-EDF-ID-TECH-INDV     OF LK-CPGEAL20
                   TO LI-REM-ID-TECH-INDV     OF LK-CPGAAL18

                 MOVE 'O'
                    TO LI-REM-IN-UTIL-AD-MAND OF LK-CPGAAL18
                 PERFORM 80000-APPEL-PPGAAL18

                 IF CPGAAL18-CO-RET-1ER-OK
                   MOVE '01' TO WS-CO-ENQT
                 ELSE
                   MOVE '00' TO WS-CO-ENQT
                 END-IF
              END-IF
           END-IF
           .

      *-----------------------*
       20300-CREATION-FLUX-ED.
      *-------------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '**MB17 20300-CREATION-FLUX-ED' TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
                                                                        00121500
           EVALUATE WS-CO-CONTXT-TRT-MB17
              WHEN 'M'
                IF LI-EDF-CO-TYPE-ENV OF WS-GR-EDF = 'AF'
                 SET FLX-EDIT-PGEDECFISCMAS TO TRUE
                END-IF
      *--  == CONSERVATION DE L'IDENTIFIANT DE L'ENVOI
             MOVE LI-EDF-ID-TECH-ENV OF WS-GR-EDF
               TO WS-ID-TECH-ENV-SAUV
              WHEN 'P'
                IF LO-EDF-CO-TYPE-ENV          OF LK-CPGEAL20 = 'AF'
                 SET FLX-EDIT-PGEDECFISCPCT    TO TRUE
                 SET ACTE-GESTION-EMIS-AF-PONC TO TRUE
                END-IF
                IF LO-EDF-CO-TYPE-ENV          OF LK-CPGEAL20 = 'IF'
                 SET FLX-EDIT-PGEINFOFISCAL    TO TRUE
      *--        ALIMENTATION CODE POUR TRACE DE GESTION
                 SET ACTE-GESTION-EMIS-IF-PONC TO TRUE
                END-IF
      *--  == CONSERVATION DE L'IDENTIFIANT DE L'ENVOI
             MOVE LO-EDF-ID-TECH-ENV           OF LK-CPGEAL20
               TO WS-ID-TECH-ENV-SAUV
              WHEN OTHER
                  display '20300-crea. MB17 Code contxt-trt invalide'
               PERFORM 31000-ABANDON
           END-EVALUATE

M016+ *--  initialisation id lec ttes LF pour ULF
M016+      SET NON-LECT-TTE-LF-ULF TO TRUE

           EVALUATE TRUE
              WHEN FLX-EDIT-PGEDECFISCMAS
                 PERFORM 20310-CREAT-PGEDECFISCMAS
              WHEN FLX-EDIT-PGEDECFISCPCT
                 PERFORM 20310-CREAT-PGEDECFISCPCT
              WHEN FLX-EDIT-PGEINFOFISCAL
                 PERFORM 20310-CREAT-PGEINFOFISCAL
              WHEN OTHER
                  display '20300-crea. INFO FLUX EDITIQUE invalide'
                 PERFORM 31000-ABANDON
           END-EVALUATE


M015+      IF CONTINUE-CREAT-FLUX
              PERFORM 22000-EMISSION-FLX
M015+      END-IF
           .

      *-------------------------*
       20310-CREAT-PGEDECFISCMAS.
      *-------------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '**MB17 20310-CREAT-PGEDECFISCMAS'
                TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
                                                                        00121500
M008       MOVE 6 TO WS-NB-OCC-BLOC-EDIT
           PERFORM 21000-BLOC-UINSTITUTION
           PERFORM 21000-BLOC-UDESTINATAIRE
           PERFORM 21000-BLOC-PREFCOURRIER
M015+      IF CONTINUE-CREAT-FLUX
              PERFORM 21000-BLOC-PALLOCATAIRE
M015+         IF CONTINUE-CREAT-FLUX
                 PERFORM 21000-BLOC-UINDIVIDU
M008+            PERFORM 21000-BLOC-UADRESSE

      *ITERATION DE CE BLOC: POUR CHAQUE DPR DISTINCT
      *+ INCREMENETATION DU COMPTEUR DU NBR DE BLOC
                 PERFORM 21000-BLOC-PDOCFISCAL
M015+         END-IF
M015+      END-IF
           .

      *-------------------------*
       20310-CREAT-PGEDECFISCPCT.
      *-------------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '**MB17 20310-CREAT-PGEDECFISCPCT'
                TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
DISP  *       DISPLAY '==>  LO-ID-TECH-ENV-ORIG:'
DISP  *                                          LO-EDF-ID-TECH-ENV-ORIG
DISP  *       OF LK-CPGEAL20
           END-IF
                                                                        00121500
M008       MOVE 6 TO WS-NB-OCC-BLOC-EDIT
           PERFORM 21000-BLOC-UINSTITUTION
           PERFORM 21000-BLOC-UDESTINATAIRE
           PERFORM 21000-BLOC-PREFCOURRIER
M015+      IF CONTINUE-CREAT-FLUX
              PERFORM 21000-BLOC-PALLOCATAIRE
M015+         IF CONTINUE-CREAT-FLUX
                 PERFORM 21000-BLOC-UINDIVIDU
M008+            PERFORM 21000-BLOC-UADRESSE

      *        ITERATION DE CE BLOC: POUR CHAQUE DPR DISTINCT
      *+       INCREMENETATION DU COMPTEUR DU NBR DE BLOC
      *--        RECUPERATION DES LIGNES FISCALE LIEES
                 PERFORM 40000-INITIALISER-PPGEAL21

M001- *          MOVE LO-EDF-ID-TECH-ENV OF LK-CPGEAL20
M001             MOVE LO-EDF-ID-TECH-ENV-ORIG OF LK-CPGEAL20
                   TO LI-LIF-ID-TECH-ENV OF LK-CPGEAL21


                 SET CPGEAL21-OPEN TO TRUE
                 PERFORM 80000-APPEL-PPGEAL21

      *--        ALIMENTATION DES BLOCS FISCAUX
                 PERFORM 21000-BLOC-PDOCFISCAL-P

                 PERFORM 40000-INITIALISER-PPGEAL21
                 SET CPGEAL21-CLOSE TO TRUE
                 PERFORM 80000-APPEL-PPGEAL21
M015+         END-IF
M015+      END-IF
           .

      *-------------------------*
       20310-CREAT-PGEINFOFISCAL.
      *-------------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '**MB17 20310-CREAT-PGEINFOFISCAL'
                TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
                                                                        00121500
M008       MOVE 6 TO WS-NB-OCC-BLOC-EDIT
           PERFORM 21000-BLOC-UINSTITUTION
           PERFORM 21000-BLOC-UDESTINATAIRE
           PERFORM 21000-BLOC-PREFCOURRIER
M015+      IF CONTINUE-CREAT-FLUX
              PERFORM 21000-BLOC-PALLOCATAIRE
M015+         IF CONTINUE-CREAT-FLUX
                 PERFORM 21000-BLOC-UINDIVIDU
M008+            PERFORM 21000-BLOC-UADRESSE

      *ITERATION       DE CE BLOC: POUR CHAQUE DPR DISTINCT
      *+       INCREMENETATION DU COMPTEUR DU NBR DE BLOC
      *--        RECUPERATION DES LIGNES FISCALE LIEES
                 PERFORM 40000-INITIALISER-PPGEAL21

                 MOVE LO-EDF-ID-TECH-ENV OF LK-CPGEAL20
                   TO LI-LIF-ID-TECH-ENV OF LK-CPGEAL21
                 SET CPGEAL21-OPEN TO TRUE
                 PERFORM 80000-APPEL-PPGEAL21

      *--        ALIMENTATION DES BLOCS FISCAUX
                 PERFORM 21000-BLOC-PDOCFISCAL-P

                 PERFORM 40000-INITIALISER-PPGEAL21
                 SET CPGEAL21-CLOSE TO TRUE
                 PERFORM 80000-APPEL-PPGEAL21
M015+         END-IF
M015+      END-IF
           .

      *-------------------------*
       21000-BLOC-UINSTITUTION.
      *-------------------------*
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 21000-BLOC-UINSTITUTION' TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF

      *     IF (WS-CO-REG-SAVE NOT = WS-CO-REG-EDF)
      *     AND (WS-NO-INST-SAVE NOT = WS-NO-INST-DECL-EDF)
      *         PERFORM 40000-INITIALISER-PP00SV58
      *         MOVE LI-EDF-CO-REG-DECL OF WS-GR-EDF
      *           TO LI-CO-REG OF LK-CP00SV58
      *         MOVE LI-EDF-NO-INST-DECL OF WS-GR-EDF
      *           TO LI-NO-INST OF LK-CP00SV58
      *         PERFORM 80000-APPEL-PP00SV58
      *
      *         PERFORM 40000-INITIALISER-PP00SV56
      *         PERFORM 80000-APPEL-PP00SV56
      *
      *         MOVE WS-CO-REG-EDF TO WS-CO-REG-SAVE
      *         MOVE WS-NO-INST-DECL-EDF TO WS-NO-INST-SAVE
      *
      *         TO LI-CO-REG-RET OF LK-CP00SV56
      *         TO LI-NO-INST  OF LK-CP00SV56
      *         TO LI-DT-EFF  OF  LK-CP00SV56
      *     END-IF
      *
      *    PERFORM 40000-INITIALISER-PPSPSV14
M015+      INITIALIZE LI-UIS-VA-LIG-EDIT
           SET CO-BLOC-UINSTITUTION TO TRUE

      *    MOVE 'M1'   TO LI-CO-METH     OF LK-ZONE-TECH-UIS
      *    MOVE WS-CO-BLOC TO LI-CO-BLOC OF LK-ZONE-TECH-UIS
      *    MOVE 'V01'  TO LI-NO-VERS-BLOC OF LK-ZONE-TECH-UIS

           IF WS-CO-CONTXT-TRT-MB17 = 'M'
              MOVE LI-EDF-CO-REG-DECL OF WS-GR-EDF
               TO LI-UIS-CO-REG-INST

              MOVE LI-EDF-NO-INST-DECL OF WS-GR-EDF
              TO LI-UIS-NO-INST
           END-IF

           IF WS-CO-CONTXT-TRT-MB17 = 'P'
              MOVE LO-EDF-CO-REG-DECL  OF LK-CPGEAL20
              TO LI-UIS-CO-REG-INST

              MOVE LO-EDF-NO-INST-DECL OF LK-CPGEAL20
              TO LI-UIS-NO-INST
           END-IF

      *    MOVE LO-LB-CRT-INST OF LK-CP00SV58
      *    TO LI-UIS-LB-CRT-INST
      *    MOVE LO-LB-LNG-INST OF LK-CP00SV58
      *    TO LI-UIS-LB-LNG-INST
      *    MOVE LO-LB-MENT-LEG-INST OF LK-CP00SV58
      *    TO LI-UIS-MENT-LGAL-INST
      *    MOVE LO-LB-GRP-INST OF LK-CP00SV58
      *    TO LI-UIS-LB-GRP-INST
      *    MOVE LO-NO-TEL-INST OF LK-CP00SV58
      *    TO LI-UIS-TEL-INST
      *    MOVE LO-NO-FAX-INST OF LK-CP00SV58
      *    TO LI-UIS-FAX-INST
      *    MOVE LO-AD-MAIL-INST OF LK-CP00SV58
      *    TO LI-UIS-AD-EMAIL-INST
      *
      *    MOVE LO-LB-LIGN-AD-2 OF LK-CP00SV56
      *    TO LI-UIS-AD-LIGNE-2-INST
      *    MOVE LO-LB-LIGN-AD-3 OF LK-CP00SV56
      *    TO LI-UIS-AD-LIGNE-3-INST
      *    MOVE LO-LB-LIGN-AD-4 OF LK-CP00SV56
      *    TO LI-UIS-AD-LIGNE-4-INST
      *    MOVE LO-LB-LIGN-AD-5 OF LK-CP00SV56
      *    TO LI-UIS-AD-LIGNE-5-INST
      *    MOVE LO-LB-LIGN-AD-6 OF LK-CP00SV56
      *    TO LI-UIS-AD-LIGNE-6-INST
      *
      *    PERFORM 80000-APPELER-PPSPSV14
      *    IF CPSP14M1-CO-RET-1ER-OK AND CPSP14M1-CO-RET-2ND-OK
           MOVE 'V01'  TO WS-NO-VERS-BLOC-EDIT (1)
           MOVE 585 TO WS-NB-CAR-BLOC-EDIT (1)
           MOVE WS-CO-BLOC        TO WS-CO-BLOC-EDIT(1)
           MOVE LI-UIS-VA-LIG-EDIT
             TO WS-VA-LIG-EDIT (1)

      *    END-IF
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              STRING '--1-->BLOC:' WS-GR-BLOC-EDIT (1)
              DELIMITED BY SIZE INTO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
           .

      *-------------------------*
       21000-BLOC-UDESTINATAIRE.
      *-------------------------*
      *--  CE BLOC EST ALIMENTE A PARTIR DES INFORMATION
      *--  CONTENU DANS LA PARTIE FIXE DU FLUX (CPSP12C1)
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 21000-BLOC-UDESTINATAIRE' TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF

           SET   CO-BLOC-UDESTINATAIRE TO TRUE
           MOVE WS-CO-BLOC        TO WS-CO-BLOC-EDIT(2)
           MOVE 'V02'             TO WS-NO-VERS-BLOC-EDIT (2)
           MOVE 341
           TO WS-NB-CAR-BLOC-EDIT (2)
           MOVE SPACE
           TO WS-VA-LIG-EDIT (2)

      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              STRING '--2-->BLOC:' WS-GR-BLOC-EDIT (2)
              DELIMITED BY SIZE INTO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
           .

      *-------------------------*
       21000-BLOC-PREFCOURRIER.
      *-------------------------*
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 21000-BLOC-PREFCOURRIER' TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF

           SET   CO-BLOC-PREFCOURRIER TO TRUE
           MOVE WS-CO-BLOC        TO WS-CO-BLOC-EDIT(3)

      *   MOVE 'V01'             TO WS-NO-VERS-BLOC-EDIT (3)
      *   MOVE LO-LONG-BLOC      OF LK-CPSPEUIS
      *     TO WS-NB-CAR-BLOC-EDIT (3)
      **  MOVE SPACE      OF LK-CPSPEUIS
      **    TO WS-VA-LIG-EDIT (3)

           PERFORM 40000-INITIALISER-PPSPSV14
           MOVE 'M1'          TO LI-CO-METH    OF LK-CPSP1420
           MOVE WS-CO-BLOC
             TO LI-CO-BLOC             OF LK-CPSP1420
           MOVE 'V01'
             TO LI-NO-VERS-BLOC         OF LK-CPSP1420

           MOVE 0
             TO LI-ID-REF-PRC           OF LK-CPSP1420

           IF WS-CO-CONTXT-TRT-MB17 = 'M'
              MOVE LI-LIF-ID-TECH-DPR   OF WS-GR-EDF
                TO LI-ID-TECH-DPR       OF LK-CPSP1420

      *       INDIVIDU BENEFICIARE ?
              MOVE LI-EDF-ID-TECH-OBJ-DEST  OF WS-GR-EDF
                TO LI-ID-TECH-INDV      OF LK-CPSP1420
              MOVE LI-EDF-CO-REG-DECL   OF WS-GR-EDF
                TO LI-CO-REG-LIQ        OF LK-CPSP1420
              MOVE LI-EDF-NO-INST-DECL  OF WS-GR-EDF
                TO LI-NO-INST-LIQ       OF LK-CPSP1420
           END-IF

           IF WS-CO-CONTXT-TRT-MB17 = 'P'
              MOVE LO-LIF-ID-TECH-DPR      OF LK-CPGEAL21
                TO LI-ID-TECH-DPR          OF LK-CPSP1420
      *       INDIVIDU BENEFICIARE ?
              MOVE LO-EDF-ID-TECH-OBJ-DEST OF LK-CPGEAL20
                TO LI-ID-TECH-INDV         OF LK-CPSP1420
              MOVE LO-EDF-CO-REG-DECL      OF LK-CPGEAL20
                TO LI-CO-REG-LIQ           OF LK-CPSP1420
              MOVE LO-EDF-NO-INST-DECL     OF LK-CPGEAL20
                TO LI-NO-INST-LIQ          OF LK-CPSP1420
           END-IF

      *  *ALLER CHERCHER DANS TPGADPR ET RAMENER AUSSI LE NO-ORD-DPR
           MOVE 1
             TO LI-NO-ORD-DPR        OF LK-CPSP1420
      *  *ALLER CHERCHER DANS TPGADPR ET RAMENER AUSSI LE ID-REF-PRC
           MOVE SPACE
             TO LI-ID-REF-CAIS       OF LK-CPSP1420
           MOVE SPACE
             TO LI-DT-LIM-REP        OF LK-CPSP1420
           PERFORM 80000-APPELER-PPSPSV14
           IF CPSP14M1-CO-RET-1ER-OK AND CPSP14M1-CO-RET-2ND-OK
              MOVE 'V01'             TO WS-NO-VERS-BLOC-EDIT (3)
              MOVE LO-LONG-BLOC      OF LK-CPSP1420
                TO WS-NB-CAR-BLOC-EDIT (3)
              MOVE LO-DATA-BLOC      OF LK-CPSP1420
                TO WS-VA-LIG-EDIT (3)

      *--     GESTION DES TRACES
              IF CP00SV21-TRACE-ACTIVE
                 STRING '--3-->BLOC:' WS-GR-BLOC-EDIT (3)
                 DELIMITED BY SIZE INTO LI-ID-DON-TRACE
                 PERFORM 88888-NIV-TRACE-SHOW
              END-IF
           END-IF
           .


      ******************************************************************
      * TRAITEMENT BLOC PALLOCATAIRE
      ******************************************************************
       21000-BLOC-PALLOCATAIRE.
      *-----------------------------
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 21000-BLOC-PALLOCATAIRE' TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF

           SET  CO-BLOC-PALLOCATAIRE TO TRUE
           MOVE WS-CO-BLOC TO WS-CO-BLOC-EDIT(4)


           PERFORM 40000-INITIALISER-PPSPSV14
           MOVE 'M1'       TO LI-CO-METH         OF LK-CPSP1408
           MOVE WS-CO-BLOC
             TO LI-CO-BLOC        OF LK-CPSP1408
           MOVE 'V01'
             TO LI-NO-VERS-BLOC   OF LK-CPSP1408

           IF WS-CO-CONTXT-TRT-MB17 = 'M'
              MOVE LI-EDF-ID-TECH-INDV OF WS-GR-EDF
                TO LI-ID-TECH-INDV   OF LK-CPSP1408

              MOVE LI-FIS-DT-DC      OF WS-GR-FIS
                TO LI-DT-DC          OF LK-CPSP1408
              MOVE '9999-12-31'
                TO LI-DT-MARI        OF LK-CPSP1408
              MOVE LI-FIS-CO-CIVI-INDV OF WS-GR-FIS
                TO LI-CO-CIVI        OF LK-CPSP1408
           END-IF

           IF WS-CO-CONTXT-TRT-MB17 = 'P'
      *--     APPEL BREF RESTITUER INDIVIDU
              PERFORM 21100-RESTIT-IND

              MOVE LO-EDF-ID-TECH-INDV OF LK-CPGEAL20
                TO LI-ID-TECH-INDV     OF LK-CPSP1408

              IF LO-DT-DC OF LK-CP00SV37 NOT = SPACE AND LOW-VALUE
                 MOVE LO-DT-DC  OF LK-CP00SV37
                   TO LI-DT-DC  OF LK-CPSP1408
              ELSE
                 MOVE '9999-12-31'   TO LI-DT-DC  OF LK-CPSP1408
              END-IF

              MOVE '9999-12-31'
                TO LI-DT-MARI        OF LK-CPSP1408

              MOVE LO-CO-CIVI-INDV   OF LO-GRP-INDV OF LK-CP00SV37
                TO LI-CO-CIVI        OF LK-CPSP1408
           END-IF

           PERFORM 80000-APPELER-PPSPSV14

           IF CPSP14M1-CO-RET-1ER-OK AND CPSP14M1-CO-RET-2ND-OK
              MOVE 'V01'          TO WS-NO-VERS-BLOC-EDIT (4)
              MOVE LO-LONG-BLOC   OF LK-CPSP1408
                TO WS-NB-CAR-BLOC-EDIT (4)
              MOVE LO-DATA-BLOC   OF LK-CPSP1408
                TO WS-VA-LIG-EDIT (4)

      *--     GESTION DES TRACES
              IF CP00SV21-TRACE-ACTIVE
                 STRING '--4-->BLOC:' WS-GR-BLOC-EDIT (4)
                 DELIMITED BY SIZE INTO LI-ID-DON-TRACE
                 PERFORM 88888-NIV-TRACE-SHOW
              END-IF
           END-IF
           .

      ******************************************************************
      * TRAITEMENT BLOC UINDIVIDU
      ******************************************************************
       21000-BLOC-UINDIVIDU.
      *-----------------------------
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 21000-BLOC-UINDIVIDU' TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF

           SET  CO-BLOC-UINDIVIDU TO TRUE
           MOVE WS-CO-BLOC TO WS-CO-BLOC-EDIT(5)

           MOVE SPACE   TO  WS-GR-UIN

           IF WS-CO-CONTXT-TRT-MB17 = 'M'
              MOVE LI-EDF-ID-TECH-INDV OF WS-GR-EDF
                TO LI-UIN-ID-TECH-INDV
           END-IF

           IF WS-CO-CONTXT-TRT-MB17 = 'P'
              MOVE LO-EDF-ID-TECH-INDV OF LK-CPGEAL20
                TO LI-UIN-ID-TECH-INDV
           END-IF
           INSPECT LI-UIN-ID-TECH-INDV REPLACING ALL ' ' BY '0'

      *-------
      *!!!!!  MODIFICATION DE LA TNPM0FE
           MOVE 'V01'          TO WS-NO-VERS-BLOC-EDIT (5)
M001       MOVE 587
             TO WS-NB-CAR-BLOC-EDIT (5)
           MOVE WS-GR-UIN
             TO WS-VA-LIG-EDIT (5)

      *--     GESTION DES TRACES
              IF CP00SV21-TRACE-ACTIVE
                 STRING '--5-->BLOC:' WS-GR-BLOC-EDIT (5)
                 DELIMITED BY SIZE INTO LI-ID-DON-TRACE
                 PERFORM 88888-NIV-TRACE-SHOW
              END-IF
           .
      *-----------------------------
       21000-BLOC-UADRESSE.
      *-----------------------------
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 21000-BLOC-UADRESSE' TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF

           SET  CO-BLOC-UADRESSE TO TRUE
           MOVE WS-CO-BLOC TO WS-CO-BLOC-EDIT(6)

           MOVE SPACE   TO  WS-GR-UAD

           IF WS-CO-CONTXT-TRT-MB17 = 'M'
              MOVE LI-EDF-ID-TECH-INDV OF WS-GR-EDF
                TO LI-UAD-ID-TECH-OBJ-GEST
              MOVE 'IND'
                TO LI-UAD-CO-TYPE-OBJ-GEST
           END-IF

           IF WS-CO-CONTXT-TRT-MB17 = 'P'
M009+         MOVE LO-EDF-ID-TECH-INDV OF LK-CPGEAL20
                TO LI-UAD-ID-TECH-OBJ-GEST
M009+         MOVE 'IND'
M009+           TO LI-UAD-CO-TYPE-OBJ-GEST
           END-IF

      *-------
      *!!!!!  MODIFICATION DE LA TNPM0FE
           MOVE 'V01'          TO WS-NO-VERS-BLOC-EDIT (6)
M009       MOVE 287
             TO WS-NB-CAR-BLOC-EDIT (6)
           MOVE WS-GR-UAD
             TO WS-VA-LIG-EDIT (6)
*DEBUG*    DISPLAY 'BDE WS-VA-LIG-EDIT >' WS-VA-LIG-EDIT (6) '<'

      *--     GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              STRING '--6-->BLOC:' WS-GR-BLOC-EDIT (6)
              DELIMITED BY SIZE INTO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
           .
      *****************************************************************
      * TRAITEMENT BLOC PDOCFISCAL
      *****************************************************************
       21000-BLOC-PDOCFISCAL.
      *-----------------------------*
      * POUR CHAQUE DPR DU TRIPLET REG/INST/IND
      *
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 21000-BLOC-PDOCFISCAL' TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF

           MOVE WS-GR-CLE-EDF-LIF TO WS-GR-CLE-EDF-LIF-PREC

M009+      MOVE  WS-NB-BLOC-UNIQ TO WS-IX-1
           SET COMPTER-ALLOCATAIRE-KO TO TRUE

      * ALIMENTATION DES COMPTEURS TEMPORAIRE
           IF LI-EDF-IN-PEC-DGI OF WS-GR-EDF = 'N'
             IF COMPTER-ALLOCATAIRE-KO
                ADD 1 TO WS-NB-TP-EXO-AF-ENV-7
                SET COMPTER-ALLOCATAIRE-OK TO TRUE
              END-IF
           ELSE
              IF COMPTER-ALLOCATAIRE-KO
                 IF LI-EDF-IN-RESI-ETRG OF WS-GR-EDF = 'N'
                    ADD 1 TO WS-NB-TP-NEXO-AF-ENV-FR-8
                 ELSE
                    ADD 1 TO WS-NB-TP-NEXO-AF-ENV-ETRG-9
                 END-IF
                 SET COMPTER-ALLOCATAIRE-OK TO TRUE
              END-IF
           END-IF

      *--  CONSERVATION D'INFORMATION SUR L'ENVOI TRAITE
           MOVE LI-EDF-IN-RESI-ETRG OF WS-GR-EDF
           TO WS-IN-RESI-ETRG-EDF-PREC
           MOVE LI-EDF-IN-PEC-DGI   OF WS-GR-EDF
           TO WS-IN-PEC-DGI-EDF-PREC

      *--  PREPARATION ALIM ZONE COM POUR PPSPSV12
           PERFORM 41000-ALIM-FIXE-PPSPSV12

           PERFORM UNTIL (WS-GR-CLE-EDF NOT = WS-GR-CLE-EDF-PREC)
           OR EOF-FCGEEDF1
      *--     AJOUT DE LA LIGNE FISCAL AU BLOC POUR LE DPR TRAIT
      *--     RUPTURE SUR LE DPR POUR 1TRIPLET REG/INST/INDV
              PERFORM 21100-ADD-LIF-BLOC-PDOCFISCAL

              IF WS-GR-CLE-EDF = WS-GR-CLE-EDF-PREC
              AND OK-FCGEEDF1
      *--        INCREMENTATION COMPTEUR NOMBRE DE BLOC PDOCFISCAL
                 ADD 1 TO WS-IX-1
                 MOVE WS-GR-CLE-EDF-LIF   TO WS-GR-CLE-EDF-LIF-PREC
              END-IF
           END-PERFORM

      *     POSITIONNEMENT INDICATEUR LECTURE ULF SUIVANTE A TRUE
           SET READ-ULF-NEXT-OK TO TRUE

      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              STRING  'TRIPLET TRAITE:' WS-GR-CLE-EDF-LIF-PREC
              'MAX BLOC:'  WS-IX-1
              DELIMITED BY SIZE INTO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF

           .

      *-----------------------------
       21100-ADD-LIF-BLOC-PDOCFISCAL.
      *-----------------------------
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 21100-ADD-LIF-BLOC-PDOCFISCAL'
              TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF

           SET  CO-BLOC-PDOCFISCAL TO TRUE
           MOVE WS-CO-BLOC TO WS-CO-BLOC-EDIT(WS-IX-1)
D     *    DISPLAY '--  WS-IX-1:' WS-IX-1

           PERFORM 21105-ALIM-DATA-PDOCFISCAL-M
      *--  INITIALISATION BOOLEEN GESTION RUPTURE SUR PERIODE FISCALE
           SET KO-RUPT-PERIOD-FISC TO TRUE

      *    LECTURE UNTIL RUPTURE SUR ID-TECH-DPR
M004       MOVE ZERO TO WS-NB-DPR
           PERFORM UNTIL (WS-ID-TECH-DPR-LIF
           NOT = WS-ID-TECH-DPR-LIF-PREC)
           OR EOF-FCGEEDF1

M004          ADD 1 TO WS-NB-DPR
              COMPUTE WS-MT-VTIL-LIF =
              FUNCTION NUMVAL-C(LI-LIF-MT-VTIL-LIF OF WS-GR-EDF)

      *--     GESTION DES TRACES
              IF CP00SV21-TRACE-ACTIVE
                 STRING 'BLOC EDITIQUE N°:' WS-IX-1
                 ' CO-VTIL-MT-LIF:' LI-LIF-CO-VTIL-MT-LIF OF WS-GR-EDF
      *          '<MT-VTIL-LIF:' WS-MT-VTIL-LIF  '<->'
                 DELIMITED BY SIZE INTO LI-ID-DON-TRACE
                 PERFORM 88888-NIV-TRACE-SHOW
DISP  *          MOVE LI-LIF-CO-VTIL-MT-LIF OF WS-GR-EDF
DISP  *            TO WS-MT-TAMPON-SIGN
      *          DISPLAY
DISP  *          'WS-MT-TAMPON-SIGN:'WS-MT-TAMPON-SIGN '<--|'
DISP  *          DISPLAY
DISP  *          'LI-MT-VTIL-LIF:'LI-LIF-CO-VTIL-MT-LIF OF WS-GR-EDF
DISP  *          '<--|'
              END-IF

DISP  *          DISPLAY
DISP  *          'LI-MT-VTIL-LIF:'LI-LIF-CO-VTIL-MT-LIF OF WS-GR-EDF
DISP  *          '<--|'
DISP  *          'LI-MT-VTIL-LIF:'WS-MT-VTIL-LIF
DISP  *          '<--|'
      *

              EVALUATE LI-LIF-CO-VTIL-MT-LIF OF WS-GR-EDF
                 WHEN 'MB    '
                   MOVE WS-MT-VTIL-LIF
                     TO LI-MT-BRUT           OF LK-CPSP1411
                 WHEN 'TMAJEE'
                   MOVE WS-MT-VTIL-LIF
                     TO LI-MT-MAJO-EE        OF LK-CPSP1411
                 WHEN 'TMAJEC'
                   MOVE WS-MT-VTIL-LIF
                     TO LI-MT-MAJO-EACH      OF LK-CPSP1411
                 WHEN 'TCOTSS'
                   MOVE WS-MT-VTIL-LIF
                     TO LI-MT-TOT-COT-SECU   OF LK-CPSP1411
                 WHEN 'TCSGND'
                   MOVE WS-MT-VTIL-LIF
                     TO LI-MT-CSGND          OF LK-CPSP1411
                 WHEN 'TCSGD '
                   MOVE WS-MT-VTIL-LIF
                     TO LI-MT-CSGD           OF LK-CPSP1411
                 WHEN 'TCNDEE'
                   MOVE WS-MT-VTIL-LIF
                     TO LI-MT-CSGND-EE       OF LK-CPSP1411
                 WHEN 'TCRSD '
                    MOVE WS-MT-VTIL-LIF
                      TO LI-MT-RDS           OF LK-CPSP1411
                 WHEN 'MNF   '
                   MOVE WS-MT-VTIL-LIF
                     TO LI-MT-NET-FISC       OF LK-CPSP1411
                 WHEN 'MRA   '
                   MOVE WS-MT-VTIL-LIF
                     TO LI-MT-RAPL-ANT       OF LK-CPSP1411
                 WHEN 'RAL   '
                   MOVE WS-MT-VTIL-LIF
                     TO LI-MT-RTN-SRC        OF LK-CPSP1411
                 WHEN OTHER
                    CONTINUE
              END-EVALUATE

              MOVE LI-LIF-NO-PRDE-LIF OF WS-GR-EDF
                TO WS-NO-PRDE-LIF-9
      *       GESTION RUPTURE SUR PERIODE FISCALE
M004          DIVIDE    WS-NB-DPR BY WS-NB-DIVISEUR
M004             GIVING WS-NB-RESULTAT REMAINDER WS-NB-RESTE
M004          IF WS-NB-RESTE = ZERO
M004             MOVE WS-NB-RESULTAT TO WS-NO-PRDE
M004  *       IF FUNCTION MOD (WS-NO-PRDE-LIF-9, 11) = 0
M004  *          COMPUTE WS-NO-PRDE = WS-NO-PRDE-LIF-9 / 11

                 MOVE WS-NO-PRDE TO LI-NO-PRDE      OF LK-CPSP1411
M002             SET  CO-BLOC-PDOCFISCAL TO TRUE
M002             MOVE WS-CO-BLOC TO WS-CO-BLOC-EDIT(WS-IX-1)

                 PERFORM 21110-GET-MNT-LIF-PDOCFISCAL
                 SET OK-RUPT-PERIOD-FISC TO TRUE
              END-IF

      *       LECTURE DE ULF SUIVANTE
              PERFORM 41000-LECTURE-FCGEEDF1

      *       SI EXISTE PERIODE SUIVANTE PR MEME DPR TRAITE AVANT
              IF(WS-ID-TECH-DPR-LIF = WS-ID-TECH-DPR-LIF-PREC)
              AND OK-FCGEEDF1 AND OK-RUPT-PERIOD-FISC
                 ADD 1 TO WS-IX-1
                 SET KO-RUPT-PERIOD-FISC TO TRUE
                 PERFORM 21105-ALIM-DATA-PDOCFISCAL-M

      *--        GESTION DES TRACES
                 IF CP00SV21-TRACE-ACTIVE
                    STRING '     == INFO:'
                    'IL EXISTE 2 PERIODES FISCALES SUR LE DPR'
                    'ADD 1 TO WS-IX-1 -->WS-IX-1:' WS-IX-1
                    DELIMITED BY SIZE INTO LI-ID-DON-TRACE
                    PERFORM 88888-NIV-TRACE-SHOW
                 END-IF
              END-IF
            END-PERFORM
            .

      *------------------------*
       000000-XXXX-YYYY.
      *------------------------*
           MOVE 0 TO LI-MT-BRUT          OF LK-CPSP1411
           MOVE 0 TO LI-MT-MAJO-EE       OF LK-CPSP1411
           MOVE 0 TO LI-MT-MAJO-EACH     OF LK-CPSP1411
           MOVE 0 TO LI-MT-COT-AMF       OF LK-CPSP1411
           MOVE 0 TO LI-MT-COT-AML       OF LK-CPSP1411
           MOVE 0 TO LI-MT-CSGND         OF LK-CPSP1411
           MOVE 0 TO LI-MT-CSGND-EE      OF LK-CPSP1411
           MOVE 0 TO LI-MT-CSGD          OF LK-CPSP1411
           MOVE 0 TO LI-MT-RDS           OF LK-CPSP1411
           MOVE 0 TO LI-MT-NET-FISC      OF LK-CPSP1411
           MOVE 0 TO LI-MT-RAPL-ANT      OF LK-CPSP1411
           MOVE 0 TO LI-MT-RTN-SRC       OF LK-CPSP1411
           MOVE 0 TO LI-MT-TOT-COT-SECU  OF LK-CPSP1411
           .

      *-----------------------------
       21105-ALIM-DATA-PDOCFISCAL-M.
      *-----------------------------
           PERFORM 40000-INITIALISER-PPSPSV14
           MOVE 'M1'       TO LI-CO-METH         OF LK-CPSP1411
           MOVE WS-CO-BLOC
             TO LI-CO-BLOC                       OF LK-CPSP1411
           MOVE 'V01'
             TO LI-NO-VERS-BLOC                  OF LK-CPSP1411

           MOVE LI-EDF-AN-FISC-TRT               OF WS-GR-EDF
             TO LI-AN-FISC-TRT                   OF LK-CPSP1411
           MOVE LI-LIF-IN-PRDE-UNIQ              OF WS-GR-EDF
             TO LI-IN-PRDE-UNIQ                  OF LK-CPSP1411

           MOVE LI-LIF-DT-DEB-PRDE-VRST          OF WS-GR-EDF
             TO LI-DT-DEB-PRDE-FISC              OF LK-CPSP1411
           MOVE LI-LIF-DT-FIN-PRDE-VRST          OF WS-GR-EDF
             TO LI-DT-FIN-PRDE-FISC              OF LK-CPSP1411
           MOVE LI-EDF-DT-SCIS-AF                OF WS-GR-EDF
             TO LI-DT-SCIS                       OF LK-CPSP1411

           MOVE LI-EDF-CO-TYPE-ENV               OF WS-GR-EDF
             TO LI-CO-TYPE-DOC-FISC              OF LK-CPSP1411

           MOVE LI-EDF-CO-MOD-CREA-ENV           OF WS-GR-EDF
             TO LI-CO-MOD-CREA-ENV               OF LK-CPSP1411

M019+      PERFORM 21100-TRAIT-PPGAAL01
M019+      IF CPGAAL01-CO-RET-1ER-OK AND CPGAAL01-CO-RET-2ND-OK
M019+         MOVE LO-DPR-CO-TYPE-DROI           OF LK-CPGAAL01
M019+           TO LI-CO-TYPE-DROI               OF LK-CPSP1411
M019+      ELSE
              MOVE SPACE
                TO LI-CO-TYPE-DROI               OF LK-CPSP1411
M019+      END-IF

           MOVE LI-EDF-IN-RESI-ETRG     OF WS-GR-EDF
             TO LI-IN-RESI-ETRG                  OF LK-CPSP1411
           MOVE LI-EDF-IN-DC            OF WS-GR-EDF
             TO LI-IN-DC                         OF LK-CPSP1411

           MOVE LI-EDF-IN-PEC-DGI       OF WS-GR-EDF
             TO LI-IN-PEC-DGI                    OF LK-CPSP1411
           MOVE LI-EDF-CO-REG-DECL      OF WS-GR-EDF
             TO LI-CO-REG-DCLA                   OF LK-CPSP1411
           MOVE LI-EDF-NO-INST-DECL     OF WS-GR-EDF
             TO LI-NO-INST-DCLA                  OF LK-CPSP1411
           MOVE LI-EDF-CO-REG-AG-PAYR   OF WS-GR-EDF
             TO LI-CO-REG-AG-PAYR                OF LK-CPSP1411
           MOVE LI-EDF-NO-INST-AG-PAYR  OF WS-GR-EDF
             TO LI-NO-INST-AG-PAYR               OF LK-CPSP1411
           MOVE LI-EDF-CO-TYPE-OBJ-DEST OF WS-GR-EDF
             TO LI-CO-TYPE-OBJ-DEST              OF LK-CPSP1411
           MOVE LI-EDF-ID-TECH-OBJ-DEST OF WS-GR-EDF
             TO LI-ID-TECH-OBJ-DEST              OF LK-CPSP1411

M004+         PERFORM 000000-XXXX-YYYY
           .
      *-----------------------------
       21110-GET-MNT-LIF-PDOCFISCAL.
      *-----------------------------
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '21110-GET-MNT-LIF-PDOCFISCAL' TO LI-ID-DON-TRACE
              SET CP00SV21-NIV-PAR TO TRUE
              PERFORM 80000-ALIM-TRACE
           END-IF

            MOVE 0
            TO LI-MT-COT-AMF           OF LK-CPSP1411
               LI-MT-COT-AML           OF LK-CPSP1411

            MOVE 0
            TO LI-IN-NUL-MT-BRUT       OF LK-CPSP1411
            LI-IN-NUL-MT-MAJO-EE       OF LK-CPSP1411
            LI-IN-NUL-MT-MAJO-EACH     OF LK-CPSP1411
            LI-IN-NUL-MT-CSGND         OF LK-CPSP1411
            LI-IN-NUL-MT-CSGND-EE      OF LK-CPSP1411
            LI-IN-NUL-MT-CSGD          OF LK-CPSP1411
            LI-IN-NUL-MT-RDS           OF LK-CPSP1411
            LI-IN-NUL-MT-NET-FISC      OF LK-CPSP1411
            LI-IN-NUL-MT-RAPL-ANT      OF LK-CPSP1411
            LI-IN-NUL-MT-RTN-SRC       OF LK-CPSP1411
            LI-IN-NUL-MT-TOT-COT-SECU  OF LK-CPSP1411
            MOVE -1
            TO LI-IN-NUL-MT-COT-AMF    OF LK-CPSP1411
            LI-IN-NUL-MT-COT-AML       OF LK-CPSP1411

      *-- INCREMENTATION DES MONTANTS POUR LES LF DE L'ALLOCATAIRE
      *-- SUR TOUTES SES DPR
            IF WS-CO-CONTXT-TRT-MB17 = 'M'
               ADD LI-MT-BRUT                 OF LK-CPSP1411
                  TO WS-MT-BRT-TOT-TMP
               ADD LI-MT-MAJO-EE              OF LK-CPSP1411
                  TO WS-MT-TOT-MAJ-EE-TMP
               ADD LI-MT-MAJO-EACH            OF LK-CPSP1411
                  TO WS-MT-TOT-MAJ-EC-TMP
               ADD LI-MT-CSGND                OF LK-CPSP1411
                  TO WS-MT-TOT-CSG-NDDUC-TMP
               ADD LI-MT-CSGND-EE             OF LK-CPSP1411
                  TO WS-MT-TOT-CSG-NDDUC-EE-TMP
               ADD LI-MT-CSGD                 OF LK-CPSP1411
                  TO WS-MT-TOT-CSG-DDUC-HEE-TMP
               ADD LI-MT-RDS                  OF LK-CPSP1411
                  TO WS-MT-TOT-RDS-TMP
               ADD LI-MT-NET-FISC             OF LK-CPSP1411
                  TO WS-MT-NET-FIS-TOT-TMP
               ADD LI-MT-RAPL-ANT             OF LK-CPSP1411
                  TO WS-MT-TOT-RPL-ANT-TMP
               ADD LI-MT-RTN-SRC              OF LK-CPSP1411
                  TO WS-MT-TOT-RET-SRC-TMP
               ADD LI-MT-TOT-COT-SECU         OF LK-CPSP1411
                  TO WS-MT-TOT-COT-SS-TMP
           END-IF

           PERFORM 80000-APPELER-PPSPSV14


           IF CPSP14M1-CO-RET-1ER-OK AND CPSP14M1-CO-RET-2ND-OK
              MOVE 'V01'          TO WS-NO-VERS-BLOC-EDIT (WS-IX-1)
              MOVE LO-LONG-BLOC   OF LK-CPSP1411
                TO WS-NB-CAR-BLOC-EDIT (WS-IX-1)
              MOVE LO-DATA-BLOC   OF LK-CPSP1411
                TO WS-VA-LIG-EDIT (WS-IX-1)


      *--     GESTION DES TRACES
              IF CP00SV21-TRACE-ACTIVE
                 STRING ' BLOC N°:' WS-IX-1 ' WS-GR-BLOC-EDIT:'
                 WS-GR-BLOC-EDIT(WS-IX-1) '<--'
                 DELIMITED BY SIZE INTO LI-ID-DON-TRACE
                 PERFORM 88888-NIV-TRACE-SHOW
              END-IF
           END-IF
           .

      *****************************************************************
       21000-BLOC-PDOCFISCAL-P.
      *-----------------------------*
      * POUR CHAQUE DPR DU TRIPLET REG/INST/IND
      * CAS DU TRAITEMENT PONCTUEL
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 21000-BLOC-PDOCFISCAL-P' TO LI-ID-DON-TRACE
              SET CP00SV21-NIV-PAR TO TRUE
              PERFORM 80000-ALIM-TRACE
DISP  *       DISPLAY 'LO-LIF-ID-TECH-DPR;' LO-LIF-ID-TECH-DPR
DISP  *       OF LK-CPGEAL21
DISP  *       DISPLAY ' LUE:'WS-GR-CLE-EDF-LIF '<>PREC:'
DISP  *       WS-GR-CLE-EDF-LIF-PREC '<'
           END-IF

      *
      *--  LECTURE DE LA 1ERE LIGNE FISCALE
           PERFORM 40000-INITIALISER-PPGEAL21
           SET CPGEAL21-FETCH TO TRUE
           PERFORM 80000-APPEL-PPGEAL21


           MOVE LO-LIF-ID-TECH-DPR OF LK-CPGEAL21
             TO WS-ID-TECH-DPR-LIF OF WS-GR-CLE-LIF
             OF WS-GR-CLE-EDF-LIF
           MOVE WS-GR-CLE-EDF-LIF  TO WS-GR-CLE-EDF-LIF-PREC


m009+      MOVE WS-NB-BLOC-UNIQ TO WS-IX-1
           SET COMPTER-ALLOCATAIRE-KO TO TRUE

      * ALIMENTATION DES COMPTEURS TEMPORAIRE
           IF LO-EDF-IN-PEC-DGI   OF LK-CPGEAL20 = 'N'
              IF COMPTER-ALLOCATAIRE-KO
                 ADD 1 TO WS-NB-TP-EXO-AF-ENV-7
                 SET COMPTER-ALLOCATAIRE-OK TO TRUE
              END-IF
           ELSE
              IF COMPTER-ALLOCATAIRE-KO
                 IF LO-EDF-IN-RESI-ETRG OF LK-CPGEAL20 = 'N'
                     ADD 1 TO WS-NB-TP-NEXO-AF-ENV-FR-8
                  ELSE
                     ADD 1 TO WS-NB-TP-NEXO-AF-ENV-ETRG-9
                  END-IF
                  SET COMPTER-ALLOCATAIRE-OK TO TRUE
               END-IF
            END-IF

      *--  CONSERVATION D'INFORMATION SUR L'ENVOI TRAITE
           MOVE LO-EDF-IN-RESI-ETRG OF LK-CPGEAL20
           TO WS-IN-RESI-ETRG-EDF-PREC
           MOVE LO-EDF-IN-PEC-DGI   OF LK-CPGEAL20
           TO WS-IN-PEC-DGI-EDF-PREC

      *--  PREPARATION ALIM ZONE COM POUR PPSPSV12
           PERFORM 41000-ALIM-FIXE-PPSPSV12

           PERFORM UNTIL CPGEAL21-FIN-CURSEUR
      *--     AJOUT DE LA LIGNE FISCAL AU BLOC POUR LE DPR TRAIT
      *--     RUPTURE SUR LE DPR POUR 1TRIPLET REG/INST/INDV
              PERFORM 21100-ADD-LIF-PDOCFISCAL-P

              IF WS-GR-CLE-EDF = WS-GR-CLE-EDF-PREC
              AND NOT CPGEAL21-FIN-CURSEUR
      *--        INCREMENTATION COMPTEUR NOMBRE DE BLOC PDOCFISCAL
                 ADD 1 TO WS-IX-1

      *--        GESTION DES TRACES
                 IF CP00SV21-TRACE-ACTIVE
                    DISPLAY '  =========== INFO: RUPTURE SUR DPR'
                    DISPLAY '>WS-GR-CLE-EDF-PREC:' WS-GR-CLE-EDF-PREC
                    DISPLAY '>WS-GR-CLE-EDF:' WS-GR-CLE-EDF
                    DISPLAY '  ===========>ADD 1 TO =BLOC N°:' WS-IX-1
                 END-IF
      *
                 MOVE WS-GR-CLE-EDF-LIF   TO WS-GR-CLE-EDF-LIF-PREC
              END-IF
           END-PERFORM

      *     POSITIONNEMENT INDICATEUR LECTURE ULF SUIVANTE A TRUE
           SET READ-ULF-NEXT-OK TO TRUE

      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              STRING  'TRIPLET TRAITE:' WS-GR-CLE-EDF-LIF-PREC
              'MAX BLOC:' WS-IX-1
              DELIMITED BY SIZE INTO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
           .

      *-----------------------------
       21100-ADD-LIF-PDOCFISCAL-P.
      *-----------------------------
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 21100-ADD-LIF-PDOCFISCAL-P'
                TO LI-ID-DON-TRACE
              SET CP00SV21-NIV-PAR TO TRUE
              PERFORM 88888-NIV-TRACE-SHOW
              STRING '   |-> BLOC N°:' WS-IX-1
              DELIMITED BY SIZE INTO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW

              DISPLAY '     WS-ID-TECH-DPR-LIF:' WS-ID-TECH-DPR-LIF
              OF WS-GR-CLE-LIF '<---'
              '  WS-ID-TECH-DPR-LIF-PREC:'
              WS-ID-TECH-DPR-LIF-PREC OF WS-GR-CLE-LIF-PREC '<---'
           END-IF
      *
           SET  CO-BLOC-PDOCFISCAL TO TRUE
           MOVE WS-CO-BLOC TO WS-CO-BLOC-EDIT(WS-IX-1)

           PERFORM 21110-ALIM-DATA-PDOCFISCAL-P
      *--  INITIALISATION BOOLEEN GESTION RUPTURE SUR PERIODE FISCALE
           SET KO-RUPT-PERIOD-FISC TO TRUE

      *    LECTURE UNTIL RUPTURE SUR ID-TECH-DPR
M004       MOVE ZERO TO WS-NB-DPR
           PERFORM UNTIL (WS-ID-TECH-DPR-LIF OF WS-GR-CLE-LIF
           NOT = WS-ID-TECH-DPR-LIF-PREC OF WS-GR-CLE-LIF-PREC)
           OR CPGEAL21-FIN-CURSEUR
M002          IF NOT CPGEAL21-FIN-CURSEUR
M002             ADD 1 TO WS-NB-DPR
M002          END-IF

      *--         GESTION DES TRACES
                  IF CP00SV21-TRACE-ACTIVE
NNI                  DISPLAY '======CODE VENTILATION : '
NNI                           LO-LIF-CO-VTIL-MT-LIF OF LK-CPGEAL21
NNI                          '      MONTANT VENTILATION : '
NNI                           LO-LIF-MT-VTIL-LIF OF LK-CPGEAL21 '<---'
                     STRING '   |-> BLOC N°:' WS-IX-1
                     DELIMITED BY SIZE INTO LI-ID-DON-TRACE
                     PERFORM 88888-NIV-TRACE-SHOW
                  END-IF

                  EVALUATE LO-LIF-CO-VTIL-MT-LIF OF LK-CPGEAL21
                     WHEN 'MB    '
                       MOVE LO-LIF-MT-VTIL-LIF   OF LK-CPGEAL21
                         TO LI-MT-BRUT           OF LK-CPSP1411
                     WHEN 'TMAJEE'
                       MOVE LO-LIF-MT-VTIL-LIF   OF LK-CPGEAL21
                         TO LI-MT-MAJO-EE        OF LK-CPSP1411
                     WHEN 'TMAJEC'
                       MOVE LO-LIF-MT-VTIL-LIF   OF LK-CPGEAL21
                         TO LI-MT-MAJO-EACH      OF LK-CPSP1411
                     WHEN 'TCOTSS'
                       MOVE LO-LIF-MT-VTIL-LIF   OF LK-CPGEAL21
                         TO LI-MT-TOT-COT-SECU   OF LK-CPSP1411
                     WHEN 'TCSGND'
                       MOVE LO-LIF-MT-VTIL-LIF   OF LK-CPGEAL21
                         TO LI-MT-CSGND          OF LK-CPSP1411
                     WHEN 'TCSGD '
                       MOVE LO-LIF-MT-VTIL-LIF   OF LK-CPGEAL21
                         TO LI-MT-CSGD           OF LK-CPSP1411
                     WHEN 'TCNDEE'
                       MOVE LO-LIF-MT-VTIL-LIF   OF LK-CPGEAL21
                         TO LI-MT-CSGND-EE       OF LK-CPSP1411
                     WHEN 'TCRSD '
                       MOVE LO-LIF-MT-VTIL-LIF  OF LK-CPGEAL21
                          TO LI-MT-RDS           OF LK-CPSP1411
                     WHEN 'MNF   '
                       MOVE LO-LIF-MT-VTIL-LIF   OF LK-CPGEAL21
                         TO LI-MT-NET-FISC       OF LK-CPSP1411
                     WHEN 'MRA   '
                       MOVE LO-LIF-MT-VTIL-LIF   OF LK-CPGEAL21
                         TO LI-MT-RAPL-ANT       OF LK-CPSP1411
                     WHEN 'RAL   '
                       MOVE LO-LIF-MT-VTIL-LIF   OF LK-CPGEAL21
                         TO LI-MT-RTN-SRC        OF LK-CPSP1411
                     WHEN OTHER
                        CONTINUE
                  END-EVALUATE

                  MOVE LO-LIF-NO-PRDE-LIF  OF LK-CPGEAL21
                    TO WS-NO-PRDE-LIF-9

      *           GESTION RUPTURE SUR PERIODE FISCALE
M004              DIVIDE    WS-NB-DPR BY WS-NB-DIVISEUR
M004                 GIVING WS-NB-RESULTAT REMAINDER WS-NB-RESTE
M004              IF WS-NB-RESTE = ZERO
M004                 MOVE WS-NB-RESULTAT TO WS-NO-PRDE
M004  *           IF FUNCTION MOD (WS-NO-PRDE-LIF-9, 11) = 0
M004  *              COMPUTE WS-NO-PRDE = WS-NO-PRDE-LIF-9 / 11
                     MOVE WS-NO-PRDE TO LI-NO-PRDE  OF LK-CPSP1411

                     PERFORM 21110-GET-MNT-LIF-PDOCFISCAL
                     SET OK-RUPT-PERIOD-FISC TO TRUE
                  END-IF

      *           LECTURE DE ULF SUIVANTE
                  PERFORM 40000-INITIALISER-PPGEAL21
                  SET CPGEAL21-FETCH TO TRUE
                  PERFORM 80000-APPEL-PPGEAL21

      *--         GESTION DES TRACES
                  IF CP00SV21-TRACE-ACTIVE
                    STRING '     WS-ID-TECH-DPR-LIF:' WS-ID-TECH-DPR-LIF
                     OF WS-GR-CLE-LIF
                     DELIMITED BY SIZE INTO LI-ID-DON-TRACE
                     PERFORM 88888-NIV-TRACE-SHOW
                  END-IF

      *           SI EXISTE PERIODE SUIVANTE PR MEME DPR TRAITE AVANT
                  IF (WS-ID-TECH-DPR-LIF = WS-ID-TECH-DPR-LIF-PREC)
                  AND NOT CPGEAL21-FIN-CURSEUR
                  AND OK-RUPT-PERIOD-FISC
                     ADD 1 TO WS-IX-1
M002                 SET KO-RUPT-PERIOD-FISC TO TRUE
M002  *--            ALIMENTATION POUR BLOC PDOCFISCAL SUIVANT
M002                 PERFORM 21110-ALIM-DATA-PDOCFISCAL-P
M002                 SET  CO-BLOC-PDOCFISCAL TO TRUE
M002                 MOVE WS-CO-BLOC TO WS-CO-BLOC-EDIT(WS-IX-1)

      *--            GESTION DES TRACES
                     IF CP00SV21-TRACE-ACTIVE
                        STRING ' !!! INFO(PONCTUEL)'
                        'IL EXISTE 2 PERIODES SUR LE DPR'
                        ',ADD 1, WS-IX-1=' WS-IX-1
                        DELIMITED BY SIZE INTO LI-ID-DON-TRACE
                        PERFORM 88888-NIV-TRACE-SHOW
                     END-IF
                  END-IF
           END-PERFORM
           .

      *-----------------------------
       21110-ALIM-DATA-PDOCFISCAL-P.
      *-----------------------------
           PERFORM 40000-INITIALISER-PPSPSV14
           MOVE 'M1'       TO LI-CO-METH        OF LK-CPSP1411
           MOVE WS-CO-BLOC
             TO LI-CO-BLOC                      OF LK-CPSP1411
           MOVE 'V01'
             TO LI-NO-VERS-BLOC                 OF LK-CPSP1411

           MOVE LO-EDF-AN-FISC-TRT              OF LK-CPGEAL20
             TO LI-AN-FISC-TRT                  OF LK-CPSP1411
           MOVE LO-LIF-IN-PRDE-UNIQ             OF LK-CPGEAL21
             TO LI-IN-PRDE-UNIQ                 OF LK-CPSP1411

           MOVE LO-LIF-DT-DEB-PRDE-VRST         OF LK-CPGEAL21
             TO LI-DT-DEB-PRDE-FISC             OF LK-CPSP1411
           MOVE LO-LIF-DT-FIN-PRDE-VRST         OF LK-CPGEAL21
             TO LI-DT-FIN-PRDE-FISC             OF LK-CPSP1411
           MOVE LO-EDF-DT-SCIS-AF               OF LK-CPGEAL20
             TO LI-DT-SCIS                      OF LK-CPSP1411

           MOVE LO-EDF-CO-TYPE-ENV              OF LK-CPGEAL20
             TO LI-CO-TYPE-DOC-FISC             OF LK-CPSP1411

           MOVE LO-EDF-CO-MOD-CREA-ENV          OF LK-CPGEAL20
             TO LI-CO-MOD-CREA-ENV              OF LK-CPSP1411

M019+      PERFORM 21100-TRAIT-PPGAAL01
M019+      IF CPGAAL01-CO-RET-1ER-OK AND CPGAAL01-CO-RET-2ND-OK
M019+         MOVE LO-DPR-CO-TYPE-DROI          OF LK-CPGAAL01
M019+           TO LI-CO-TYPE-DROI              OF LK-CPSP1411
M019+      ELSE
              MOVE SPACE
                TO LI-CO-TYPE-DROI              OF LK-CPSP1411
M019+      END-IF

           MOVE LO-EDF-IN-RESI-ETRG             OF LK-CPGEAL20
             TO LI-IN-RESI-ETRG                 OF LK-CPSP1411
           MOVE LO-EDF-IN-DC                    OF LK-CPGEAL20
             TO LI-IN-DC                        OF LK-CPSP1411

           MOVE LO-EDF-IN-PEC-DGI               OF LK-CPGEAL20
             TO LI-IN-PEC-DGI                   OF LK-CPSP1411
           MOVE LO-EDF-CO-REG-DECL              OF LK-CPGEAL20
             TO LI-CO-REG-DCLA                  OF LK-CPSP1411
           MOVE LO-EDF-NO-INST-DECL             OF LK-CPGEAL20
             TO LI-NO-INST-DCLA                 OF LK-CPSP1411
           MOVE LO-EDF-CO-REG-AG-PAYR           OF LK-CPGEAL20
             TO LI-CO-REG-AG-PAYR               OF LK-CPSP1411
           MOVE LO-EDF-NO-INST-AG-PAYR          OF LK-CPGEAL20
             TO LI-NO-INST-AG-PAYR              OF LK-CPSP1411
           MOVE LO-EDF-CO-TYPE-OBJ-DEST         OF LK-CPGEAL20
             TO LI-CO-TYPE-OBJ-DEST             OF LK-CPSP1411
           MOVE LO-EDF-ID-TECH-OBJ-DEST         OF LK-CPGEAL20
             TO LI-ID-TECH-OBJ-DEST             OF LK-CPSP1411

      *--  SAUVEGARDE DE LK-CPSP1411 POUR GESTION DES BLOCS FISCAUX MUL-
      *--  TIPLES
M002  *    MOVE LK-CPSP1411 TO WS-VA-DATA-SP1411-SAUV
           .

      *-------------------------*
       22000-EMISSION-FLX.
      *-------------------------*
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              DISPLAY '< =============================================='
              MOVE '==>MB17 22000-EMISSION-FLX' TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF

      *    AFFECTATION DU NOMBRE DE BLOCS POUR LE FLUX
           MOVE WS-IX-1  TO WS-NB-OCC-BLOC-EDIT
                LI-NB-OCC-BLOC-EDIT OF LK-CPSP12C1


      *===:>DEB GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              DISPLAY ' == WS-IX-1:' WS-IX-1
              STRING  'WS-NB-OCC-BLOC-EDIT:' WS-NB-OCC-BLOC-EDIT
              DELIMITED BY SIZE  INTO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW

              PERFORM VARYING WS-IX-0 FROM 1 BY 1
              UNTIL WS-IX-0 > WS-IX-1
                 STRING  'WS-GR-BLOC-EDIT(1:19)' WS-GR-BLOC-EDIT
                      (WS-IX-0) (1:19)
                    'LIGNE :' WS-IX-0
                    'WS-VA-LIG-EDIT:' WS-VA-LIG-EDIT(WS-IX-0)
                 DELIMITED BY SIZE  INTO LI-ID-DON-TRACE
                 PERFORM 88888-NIV-TRACE-SHOW
              END-PERFORM
           END-IF
      *===:>FIN GESTION DES TRACES


      *--  ALIMENTATION DES ZONES SPECIFIQUES CF 41000-ALIM-VAR-PPS.
      *    TRANSFERT DES DIVERS BLOCS EDITIQUES
           MOVE WS-TB-BLOC-EDIT
             TO LI-TB-BLOC-EDIT         OF LK-CPSP12C1
           PERFORM 80000-APPELER-PPSPSV12
           .

      *-------------------------*                                       00121600
       25000-UPDATE-AF-EDF.
      *-------------------------*                                       00121600
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '*=== MB17 25000-UPDATE-AF-EDF'  TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE
              PERFORM   80000-ALIM-TRACE
           END-IF
                                                                        00121500
           PERFORM 40000-INITIALISER-PPGEAM20
      *--  ALIMENTATION DES ZONES METIER D'ENTREE SPECIFIQUE PAR METHODE
           MOVE WS-ID-TECH-ENV-SAUV
             TO LI-EDF-ID-TECH-ENV            OF LK-CPGEAM20

           MOVE WS-IN-EMISSION-ENVOI
             TO LI-EDF-CO-STUT-ENV-AF OF LK-CPGEAM20

           MOVE 6     TO LK-NO-REQ-ACC      OF LK-CPGEAM20
           SET CPGEAM20-UPDATE TO TRUE
           PERFORM 80000-APPEL-PPGEAM20
           .                                                            00093500
      ******************************************************************
      * ZONE DE GENEREATION DES COMPTE RENDU METIER
      ******************************************************************
      *------------------*
       26000-ALIM-CRF-MET.
      *------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 26000-ALIM-CRF-MET'  TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE
              PERFORM   80000-ALIM-TRACE
           END-IF
                                                                        00121500
           MOVE   WS-NB-AL-TOT-1                 TO WS-NB-RUB-3
           MOVE   WS-NB-AL-ANO-BLQ-AF-2          TO WS-NB-RUB-4
           MOVE   WS-NB-AL-EXO-ANO-BLQ-AF-3      TO WS-NB-RUB-5
           MOVE   WS-NB-AL-NEXO-ANO-BLQ-AF-FR-4  TO WS-NB-RUB-6
           MOVE   WS-NB-AL-NEXO-ANO-BLQ-AF-ETG-5 TO WS-NB-RUB-7
           MOVE   WS-NB-AL-AF-ENV-6              TO WS-NB-RUB-8
           MOVE   WS-NB-AL-EXO-AF-ENV-7          TO WS-NB-RUB-9
           MOVE   WS-NB-AL-NEXO-AF-ENV-FR-8      TO WS-NB-RUB-10
           MOVE   WS-NB-AL-NEXO-AF-ENV-ETRG-9    TO WS-NB-RUB-11

           MOVE 0 TO  WS-NB-RUB-1 WS-NB-RUB-2 WS-NB-RUB-0
           PERFORM 26100-ALIM-REG-INST-MET

      *    GESTION DES COMPTE-RENDUS METIER
           MOVE 12 TO WS-NB-RUB-CRU-MT
           MOVE WS-GR-CRU-MET TO TB-GR-CRU
           PERFORM 40000-CPTRENDU-GENE
           .

      *---------------------------*
       26000-ALIM-CRF-MET-PONCTUEL.
      *---------------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 26000-ALIM-CRF-MET-PONCTUEL'
                TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE
              PERFORM   80000-ALIM-TRACE
           END-IF

           MOVE   WS-NB-AL-TOT-1                 TO WS-NB-RUB-3-P
           MOVE   WS-NB-AL-ANO-BLQ-AF-2          TO WS-NB-RUB-4-P
           MOVE   WS-NB-AL-AF-ENV-6              TO WS-NB-RUB-5-P

      *    GESTION DES COMPTE-RENDUS METIER
           MOVE 6 TO WS-NB-RUB-CRU-MT
           MOVE WS-GR-CRU-MET TO TB-GR-CRU
           PERFORM 40000-CPTRENDU-GENE
           .

      *----------------------*
       26100-ALIM-REG-INST-MET.
      *----------------------*
           MOVE '===' TO WS-LB-NB-RUB-2-INS
           MOVE '='   TO WS-LB-NB-RUB-2-REG
           MOVE WS-NO-INSTIT-PREC-CT
             TO WS-LB-NB-RUB-1-INS
                WS-LB-NB-RUB-3-INS WS-LB-NB-RUB-4-INS
                WS-LB-NB-RUB-5-INS WS-LB-NB-RUB-6-INS
                WS-LB-NB-RUB-7-INS WS-LB-NB-RUB-8-INS
                WS-LB-NB-RUB-9-INS WS-LB-NB-RUB-10-INS
                WS-LB-NB-RUB-11-INS
                                                                        00121500
           MOVE WS-CO-REGIME-PREC-CT
             TO WS-LB-NB-RUB-1-REG
                WS-LB-NB-RUB-3-REG WS-LB-NB-RUB-4-REG
                WS-LB-NB-RUB-5-REG WS-LB-NB-RUB-6-REG
                WS-LB-NB-RUB-7-REG WS-LB-NB-RUB-8-REG
                WS-LB-NB-RUB-9-REG WS-LB-NB-RUB-10-REG
                WS-LB-NB-RUB-11-REG
           .                                                            00093500

      *----------------------*
       26100-ALIM-REG-INST-MNT.
      *----------------------*
           MOVE 0 TO  WS-NB-RUB-1-MT WS-NB-RUB-2-MT

           MOVE '===' TO WS-LB-NB-RUB-2-INS-MT WS-LB-NB-RUB-1-INS-MT
           MOVE '='   TO WS-LB-NB-RUB-2-REG-MT WS-LB-NB-RUB-1-REG-MT

           MOVE WS-NO-INSTIT-PREC-CT
             TO WS-LB-NB-RUB-3-INS-MT
                WS-LB-NB-RUB-4-INS-MT
                WS-LB-NB-RUB-5-INS-MT WS-LB-NB-RUB-6-INS-MT
                WS-LB-NB-RUB-7-INS-MT WS-LB-NB-RUB-8-INS-MT
                WS-LB-NB-RUB-9-INS-MT WS-LB-NB-RUB-10-INS-MT
                WS-LB-NB-RUB-11-INS-MT WS-LB-NB-RUB-12-INS-MT
                WS-LB-NB-RUB-13-INS-MT

           MOVE WS-CO-REGIME-PREC-CT
             TO WS-LB-NB-RUB-3-REG-MT
                WS-LB-NB-RUB-4-REG-MT
                WS-LB-NB-RUB-5-REG-MT WS-LB-NB-RUB-6-REG-MT
                WS-LB-NB-RUB-7-REG-MT WS-LB-NB-RUB-8-REG-MT
                WS-LB-NB-RUB-9-REG-MT WS-LB-NB-RUB-10-REG-MT
                WS-LB-NB-RUB-11-REG-MT WS-LB-NB-RUB-12-REG-MT
                WS-LB-NB-RUB-13-REG-MT
           .

      *----------------------*
       26200-ALIM-CRF-INST.
      *----------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 25100-ALIM-CRF-INST'  TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE
              PERFORM   80000-ALIM-TRACE
           END-IF

      *    TOTAL D'INSTITUTIONS DISTINCT TRAITE
           MOVE 0                 TO  WS-NB-RUB-1-I WS-NB-RUB-3-I
           MOVE WS-NB-TOT-INSTIT             TO  WS-NB-RUB-2-I

      *    GESTION DES COMPTE-RENDUS METIER
           MOVE 3  TO WS-NB-RUB-CRU-MT
           MOVE WS-GR-CRU-INST   TO TB-GR-CRU
           PERFORM 40000-CPTRENDU-GENE
           .

      *----------------------*
       26000-ALIM-CRF-MET-MT.
      *----------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 25100-ALIM-CRF-MET-MT'  TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE
              PERFORM   80000-ALIM-TRACE
           END-IF

      *    DETAIL  MONTANT NOMBRE ALLOCATAIRE TOTAL ENVOI INSTIT
      *    INITIALIZE WS-GR-CRU-MET

           MOVE WS-MT-BRT-TOT-1              TO  WS-NB-RUB-3-MT
           MOVE WS-MT-NET-FIS-TOT-1          TO  WS-NB-RUB-4-MT
           MOVE WS-MT-TOT-RET-SRC-1          TO  WS-NB-RUB-5-MT
           MOVE WS-MT-TOT-MAJ-EC-1           TO  WS-NB-RUB-6-MT
           MOVE WS-MT-TOT-MAJ-EE-1           TO  WS-NB-RUB-7-MT
           MOVE WS-MT-TOT-COT-SS-1           TO  WS-NB-RUB-8-MT
           MOVE WS-MT-TOT-CSG-NDDUC-EE-1     TO  WS-NB-RUB-9-MT
           MOVE WS-MT-TOT-CSG-DDUC-HEE-1     TO  WS-NB-RUB-10-MT
           MOVE WS-MT-TOT-CSG-NDDUC-1        TO  WS-NB-RUB-11-MT
           MOVE WS-MT-TOT-RDS-1              TO  WS-NB-RUB-12-MT
           MOVE WS-MT-TOT-RPL-ANT-1          TO  WS-NB-RUB-13-MT

           PERFORM 26100-ALIM-REG-INST-MNT
           MOVE 'NB ALLOCATAIRES TRAITES  '
             TO  WS-LB-NB-RUB-2-TXT-MT

      *    GESTION DES COMPTE-RENDUS METIER
           MOVE 13 TO WS-NB-RUB-CRU-MT
           MOVE WS-GR-CRU-MET-MT TO TB-GR-CRU
           PERFORM 40000-CPTRENDU-GENE


      *= =>DETAIL MONTANT NOMBRE ALLOCATAIRE EXONERE EN ANO BLOQUANTE(2)
      *    INITIALIZE WS-GR-CRU-MET-MT
           MOVE WS-MT-BRT-TOT-2              TO  WS-NB-RUB-3-MT
           MOVE WS-MT-NET-FIS-TOT-2          TO  WS-NB-RUB-4-MT
           MOVE WS-MT-TOT-RET-SRC-2          TO  WS-NB-RUB-5-MT
           MOVE WS-MT-TOT-MAJ-EC-2           TO  WS-NB-RUB-6-MT
           MOVE WS-MT-TOT-MAJ-EE-2           TO  WS-NB-RUB-7-MT
           MOVE WS-MT-TOT-COT-SS-2           TO  WS-NB-RUB-8-MT
           MOVE WS-MT-TOT-CSG-NDDUC-EE-2     TO  WS-NB-RUB-9-MT
           MOVE WS-MT-TOT-CSG-DDUC-HEE-2     TO  WS-NB-RUB-10-MT
           MOVE WS-MT-TOT-CSG-NDDUC-2        TO  WS-NB-RUB-11-MT
           MOVE WS-MT-TOT-RDS-2              TO  WS-NB-RUB-12-MT
           MOVE WS-MT-TOT-RPL-ANT-2          TO  WS-NB-RUB-13-MT

           PERFORM 26100-ALIM-REG-INST-MNT
           MOVE 'NB AL EXO. EN ANO BLQ AF'
             TO  WS-LB-NB-RUB-2-TXT-MT

      *    GESTION DES COMPTE-RENDUS METIER
           MOVE 13 TO WS-NB-RUB-CRU-MT
           MOVE WS-GR-CRU-MET-MT TO TB-GR-CRU
           PERFORM 40000-CPTRENDU-GENE

      *= =>DETAIL  MONTANT ALLOCATAIRE EXONERE EN ANO BLOQUANTE
      *    INITIALIZE WS-GR-CRU-MET-MT
           MOVE WS-MT-BRT-TOT-3              TO  WS-NB-RUB-3-MT
           MOVE WS-MT-NET-FIS-TOT-3          TO  WS-NB-RUB-4-MT
           MOVE WS-MT-TOT-RET-SRC-3          TO  WS-NB-RUB-5-MT
           MOVE WS-MT-TOT-MAJ-EC-3           TO  WS-NB-RUB-6-MT
           MOVE WS-MT-TOT-MAJ-EE-3           TO  WS-NB-RUB-7-MT
           MOVE WS-MT-TOT-COT-SS-3           TO  WS-NB-RUB-8-MT
           MOVE WS-MT-TOT-CSG-NDDUC-EE-3     TO  WS-NB-RUB-9-MT
           MOVE WS-MT-TOT-CSG-DDUC-HEE-3     TO  WS-NB-RUB-10-MT
           MOVE WS-MT-TOT-CSG-NDDUC-3        TO  WS-NB-RUB-11-MT
           MOVE WS-MT-TOT-RDS-3              TO  WS-NB-RUB-12-MT
           MOVE WS-MT-TOT-RPL-ANT-3          TO  WS-NB-RUB-13-MT

           PERFORM 26100-ALIM-REG-INST-MNT
           MOVE 'NB AL EXO. EN ANO BLQ AF    '
             TO  WS-LB-NB-RUB-2-TXT-MT

      *    GESTION DES COMPTE-RENDUS METIER
           MOVE 13 TO WS-NB-RUB-CRU-MT
           MOVE WS-GR-CRU-MET-MT TO TB-GR-CRU
           PERFORM 40000-CPTRENDU-GENE


      *    DETAIL  MONTANT ALLOC NON EXONERE RESIDANT EN FRANCE(4)
      *    INITIALIZE WS-GR-CRU-MET-MT
           MOVE WS-MT-BRT-TOT-4              TO  WS-NB-RUB-3-MT
           MOVE WS-MT-NET-FIS-TOT-4          TO  WS-NB-RUB-4-MT
           MOVE WS-MT-TOT-RET-SRC-4          TO  WS-NB-RUB-5-MT
           MOVE WS-MT-TOT-MAJ-EC-4           TO  WS-NB-RUB-6-MT
           MOVE WS-MT-TOT-MAJ-EE-4           TO  WS-NB-RUB-7-MT
           MOVE WS-MT-TOT-COT-SS-4           TO  WS-NB-RUB-8-MT
           MOVE WS-MT-TOT-CSG-NDDUC-EE-4     TO  WS-NB-RUB-9-MT
           MOVE WS-MT-TOT-CSG-DDUC-HEE-4     TO  WS-NB-RUB-10-MT
           MOVE WS-MT-TOT-CSG-NDDUC-4        TO  WS-NB-RUB-11-MT
           MOVE WS-MT-TOT-RDS-4              TO  WS-NB-RUB-12-MT
           MOVE WS-MT-TOT-RPL-ANT-4          TO  WS-NB-RUB-13-MT

           PERFORM 26100-ALIM-REG-INST-MNT
           MOVE 'NB AL NEXO.ANO BLQ VIV FR'
             TO  WS-LB-NB-RUB-2-TXT-MT

      *    GESTION DES COMPTE-RENDUS METIER
           MOVE 13 TO WS-NB-RUB-CRU-MT
           MOVE WS-GR-CRU-MET-MT TO TB-GR-CRU
           PERFORM 40000-CPTRENDU-GENE


      *    DETAIL  MONTANT ALLOC NON EXONERE RESIDANT A L'ETRANGER(4)
      *    INITIALIZE WS-GR-CRU-MET-MT
           MOVE WS-MT-BRT-TOT-5              TO  WS-NB-RUB-3-MT
           MOVE WS-MT-NET-FIS-TOT-5          TO  WS-NB-RUB-4-MT
           MOVE WS-MT-TOT-RET-SRC-5          TO  WS-NB-RUB-5-MT
           MOVE WS-MT-TOT-MAJ-EC-5           TO  WS-NB-RUB-6-MT
           MOVE WS-MT-TOT-MAJ-EE-5           TO  WS-NB-RUB-7-MT
           MOVE WS-MT-TOT-COT-SS-5           TO  WS-NB-RUB-8-MT
           MOVE WS-MT-TOT-CSG-NDDUC-EE-5     TO  WS-NB-RUB-9-MT
           MOVE WS-MT-TOT-CSG-DDUC-HEE-5     TO  WS-NB-RUB-10-MT
           MOVE WS-MT-TOT-CSG-NDDUC-5        TO  WS-NB-RUB-11-MT
           MOVE WS-MT-TOT-RDS-5              TO  WS-NB-RUB-12-MT
           MOVE WS-MT-TOT-RPL-ANT-5          TO  WS-NB-RUB-13-MT

           PERFORM 26100-ALIM-REG-INST-MNT
           MOVE 'NB AL NEXO.ANO BLQ VIV ETRG'
             TO  WS-LB-NB-RUB-2-TXT-MT

      *    GESTION DES COMPTE-RENDUS METIER
           MOVE 13 TO WS-NB-RUB-CRU-MT
           MOVE WS-GR-CRU-MET-MT TO TB-GR-CRU
           PERFORM 40000-CPTRENDU-GENE

      *    DETAIL MONTANT ALLOC AYANT AF ENVOYEE
      *    INITIALIZE WS-GR-CRU-MET-MT
           MOVE WS-MT-BRT-TOT-6              TO  WS-NB-RUB-3-MT
           MOVE WS-MT-NET-FIS-TOT-6          TO  WS-NB-RUB-4-MT
           MOVE WS-MT-TOT-RET-SRC-6          TO  WS-NB-RUB-5-MT
           MOVE WS-MT-TOT-MAJ-EC-6           TO  WS-NB-RUB-6-MT
           MOVE WS-MT-TOT-MAJ-EE-6           TO  WS-NB-RUB-7-MT
           MOVE WS-MT-TOT-COT-SS-6           TO  WS-NB-RUB-8-MT
           MOVE WS-MT-TOT-CSG-NDDUC-EE-6     TO  WS-NB-RUB-9-MT
           MOVE WS-MT-TOT-CSG-DDUC-HEE-6     TO  WS-NB-RUB-10-MT
           MOVE WS-MT-TOT-CSG-NDDUC-6        TO  WS-NB-RUB-11-MT
           MOVE WS-MT-TOT-RDS-6              TO  WS-NB-RUB-12-MT
           MOVE WS-MT-TOT-RPL-ANT-6          TO  WS-NB-RUB-13-MT

           PERFORM 26100-ALIM-REG-INST-MNT
           MOVE 'NB AL AYANT AF ENVOYEE'
             TO  WS-LB-NB-RUB-2-TXT-MT

      *    GESTION DES COMPTE-RENDUS METIER
           MOVE 13 TO WS-NB-RUB-CRU-MT
           MOVE WS-GR-CRU-MET-MT TO TB-GR-CRU
           PERFORM 40000-CPTRENDU-GENE

      *    DETAIL MONTANT ALLOC EN ANOMALIE  BLOQ AF/IF    INSTIT (7)
      *    INITIALIZE WS-GR-CRU-MET-MT
           MOVE WS-MT-BRT-TOT-7              TO  WS-NB-RUB-3-MT
           MOVE WS-MT-NET-FIS-TOT-7          TO  WS-NB-RUB-4-MT
           MOVE WS-MT-TOT-RET-SRC-7          TO  WS-NB-RUB-5-MT
           MOVE WS-MT-TOT-MAJ-EC-7           TO  WS-NB-RUB-6-MT
           MOVE WS-MT-TOT-MAJ-EE-7           TO  WS-NB-RUB-7-MT
           MOVE WS-MT-TOT-COT-SS-7           TO  WS-NB-RUB-8-MT
           MOVE WS-MT-TOT-CSG-NDDUC-EE-7     TO  WS-NB-RUB-9-MT
           MOVE WS-MT-TOT-CSG-DDUC-HEE-7     TO  WS-NB-RUB-10-MT
           MOVE WS-MT-TOT-CSG-NDDUC-7        TO  WS-NB-RUB-11-MT
           MOVE WS-MT-TOT-RDS-7              TO  WS-NB-RUB-12-MT
           MOVE WS-MT-TOT-RPL-ANT-7          TO  WS-NB-RUB-13-MT

           PERFORM 26100-ALIM-REG-INST-MNT
           MOVE 'NB AL EXO. DONT AF ENVOYEE'
             TO  WS-LB-NB-RUB-2-TXT-MT

      *    GESTION DES COMPTE-RENDUS METIER
           MOVE 13 TO WS-NB-RUB-CRU-MT
           MOVE WS-GR-CRU-MET-MT TO TB-GR-CRU
           PERFORM 40000-CPTRENDU-GENE

      *    DETAIL MONTANT ALLOC NON EXONERE DONT AF
      *    EST ENVOYEE ET RESIDANT EN FRANCE (8)
           MOVE WS-MT-BRT-TOT-8              TO  WS-NB-RUB-3-MT
           MOVE WS-MT-NET-FIS-TOT-8          TO  WS-NB-RUB-4-MT
           MOVE WS-MT-TOT-RET-SRC-8          TO  WS-NB-RUB-5-MT
           MOVE WS-MT-TOT-MAJ-EC-8           TO  WS-NB-RUB-6-MT
           MOVE WS-MT-TOT-MAJ-EE-8           TO  WS-NB-RUB-7-MT
           MOVE WS-MT-TOT-COT-SS-8           TO  WS-NB-RUB-8-MT
           MOVE WS-MT-TOT-CSG-NDDUC-EE-8     TO  WS-NB-RUB-9-MT
           MOVE WS-MT-TOT-CSG-DDUC-HEE-8     TO  WS-NB-RUB-10-MT
           MOVE WS-MT-TOT-CSG-NDDUC-8        TO  WS-NB-RUB-11-MT
           MOVE WS-MT-TOT-RDS-8              TO  WS-NB-RUB-12-MT
           MOVE WS-MT-TOT-RPL-ANT-8          TO  WS-NB-RUB-13-MT

           PERFORM 26100-ALIM-REG-INST-MNT
           MOVE 'NB AL NEXO. ,AF ENV VIV FR'
             TO  WS-LB-NB-RUB-2-TXT-MT

      *    GESTION DES COMPTE-RENDUS METIER
           MOVE 13 TO WS-NB-RUB-CRU-MT
           MOVE WS-GR-CRU-MET-MT TO TB-GR-CRU
           PERFORM 40000-CPTRENDU-GENE

      *    DETAIL MONTANT ALLOC NON EXONERE DONT AF
      *    EST ENVOYEE ET RESIDANT A ETRANGER (9)
           MOVE WS-MT-BRT-TOT-9              TO  WS-NB-RUB-3-MT
           MOVE WS-MT-NET-FIS-TOT-9          TO  WS-NB-RUB-4-MT
           MOVE WS-MT-TOT-RET-SRC-9          TO  WS-NB-RUB-5-MT
           MOVE WS-MT-TOT-MAJ-EC-9           TO  WS-NB-RUB-6-MT
           MOVE WS-MT-TOT-MAJ-EE-9           TO  WS-NB-RUB-7-MT
           MOVE WS-MT-TOT-COT-SS-9           TO  WS-NB-RUB-8-MT
           MOVE WS-MT-TOT-CSG-NDDUC-EE-9     TO  WS-NB-RUB-9-MT
           MOVE WS-MT-TOT-CSG-DDUC-HEE-9     TO  WS-NB-RUB-10-MT
           MOVE WS-MT-TOT-CSG-NDDUC-9        TO  WS-NB-RUB-11-MT
           MOVE WS-MT-TOT-RDS-9              TO  WS-NB-RUB-12-MT
           MOVE WS-MT-TOT-RPL-ANT-9          TO  WS-NB-RUB-13-MT

           PERFORM 26100-ALIM-REG-INST-MNT
           MOVE 'NB AL NEXO. ,AF ENV VIV ETRG'
             TO  WS-LB-NB-RUB-2-TXT-MT

      *    GESTION DES COMPTE-RENDUS METIER
           MOVE 13 TO WS-NB-RUB-CRU-MT
           MOVE WS-GR-CRU-MET-MT TO TB-GR-CRU
           PERFORM 40000-CPTRENDU-GENE
           .

      *---------------------------*
       27000-EMETTRE-TRACE-GESTION.
      *---------------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 27000-EMETTRE-TRACE-GESTION'
                TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE
              PERFORM   80000-ALIM-TRACE
           END-IF

      *- RECUPERATION D'UN NUMERO CHRONO
           PERFORM 40000-INITIALISER-PP00SV30
           PERFORM 80000-APPELER-PP00SV30
      *- ALIMENTATION TABLE TRACE
           MOVE 01 TO WS-NO-REQ-ACC
           PERFORM 40000-INITIALISER-PPSPAM01
           SET CPSPAM01-INSERT TO TRUE
           PERFORM 80000-APPELER-PPSPAM01
           .

      *--------------------*                                            00148800
       23100-TRAITEMENT-ANO.
      *--------------------*                                            00149000
      *--  GESTION DES TRACES                                           00101500
           IF CP00SV21-TRACE-ACTIVE                                     00101600
              MOVE '==>MB17 23100-TRAITEMENT-ANO' TO LI-ID-DON-TRACE    00101700
              SET       CP00SV21-NIV-PAR    TO TRUE                     00101800
              PERFORM   80000-ALIM-TRACE                                00101900
           END-IF                                                       00102000
                                                                        00102100
      *--  MISE A JOUR DE LA ZONE TAMPON DU NOMBRE D'OCCURRENCES        00102200
      *--  TRAITEES                                                     00102300
           MOVE WS-NB-OCC TO WS-NB-OCC-SAUV                             00102400
                                                                        00103100
      *--  APPEL AU SERVICE DE GESTION DES REPRISES EN MISE A           00103200
      *--  JOUR POUR SAUVEGARDER L'ULF EN ANO EN CLE DE REPRISE         00103300
           SET IDENTIFIANTS TO TRUE                                     00103400
M014+      IF CP00SV21-TRACE-ACTIVE                                     00101600
M014+         STRING '==>    avant move;WS-ID-CLE-ANO-SAUV:'            00101700
M014+         WS-ID-CLE-ANO-SAUV(1:13) ';'
M014+         'WS-ID-CLE-RPRI:'WS-ID-CLE-RPRI(1:13)
M014+         DELIMITED BY SIZE
M014+         INTO LI-ID-DON-TRACE
M014+         SET       CP00SV21-NIV-PAR    TO TRUE                     00101800
M014+         PERFORM   80000-ALIM-TRACE                                00101900
M014+      END-IF                                                       00102000

           MOVE WS-ID-CLE-ANO-SAUV   TO WS-ID-CLE-RPRI                  00103500
                                                                        00150600
      *--  GESTION DES TRACES                                           00101500
M014+      IF CP00SV21-TRACE-ACTIVE                                     00101600
M014+         STRING '==>  apres move; WS-ID-CLE-RPRI='                 00101700
M014+         WS-ID-CLE-RPRI(1:13)
M014+         DELIMITED BY SIZE
M014+         INTO LI-ID-DON-TRACE
M014+         SET       CP00SV21-NIV-PAR    TO TRUE                     00101800
M014+         PERFORM   80000-ALIM-TRACE                                00101900
M014+      END-IF                                                       00102000
                                                                        00102100
                                                                        00150600
      *    MOVE DE LA CLE DE REPRISE DE TZ00RST
      *    DANS LA VARIABLE INTERNE AU PPGEMB17
           PERFORM 28000-ALIM-VAR-AFTER-REPRISE
                                                                        00151300
           MOVE SPACE                TO WS-ID-CLE-ANO                   00103600
           PERFORM 40000-MAJ-RESTART                                    00103700
                                                                        00103800
      *--  APPEL AU SERVICE GESTION DES TRANSACTIONS BATCH              00103900
      *--  POUR REALISER UN COMMIT                                      00104000
           PERFORM 40000-GESTION-COMMIT                                 00104100

M014+      IF WS-CO-CONTXT-TRT-MB17 ='M'
M014+         PERFORM 41000-LECTURE-FCGEEDF1 UNTIL
M014+         (WS-GR-CLE-EDF > WS-GR-ZONE-RPRI) OR EOF-FCGEEDF1
M014+         SET READ-ULF-NEXT-OK TO TRUE
M014+      END-IF
           .                                                            00104200

                                                                        00152100
      *------------------------*                                        00152200
       23300-TRAITEMENT-ERREUR.
      *------------------------*                                        00152400
      *--  GESTION DES TRACES                                           00104600
           IF CP00SV21-TRACE-ACTIVE                                     00104700
              MOVE '==>MB17 23300-TRAITEMENT-ERREUR' TO LI-ID-DON-TRACE 00104800
              SET       CP00SV21-NIV-PAR    TO TRUE                     00104900
              PERFORM   80000-ALIM-TRACE                                00105000
           END-IF                                                       00105100
                                                                        00105200
      *--  INFO : TRANSPORT INFO ANOMALIE POUR GESTION
M002       MOVE LK-GR-ANO OF LK-GR-PPGEMB17
M002         TO LK-GR-ANO OF WS-GR-ERR

      *--  APPEL AU SERVICE DE GESTION DES TRANSACTIONS BATCH           00105300
      *--  POUR REALISER UN ROLLBACK ET INCREMENTATION DES COMPTEURS    00105400
      *--  DU NOMBRE DE ROLLBACK ET DU NOMBRE D'ANOMALIES FONCTIONNELLES00105500
           ADD 1 TO WS-NB-ANO-PASS                                      00105600
           PERFORM 23310-GESTION-ROLLBACK                               00105700

      *--  TEST SI L'ON DEPASSE OU NON LE NOMBRE MAXIMUM D'ANOMALIE     00105900
           IF WS-NB-ANO-PASS   > LO-NB-MAXI-ANO                         00106000
      *--     ARRET IMMEDIAT DU TRAITEMENT                              00106100
              SET CO-RET-KO TO TRUE                                     00106300
                                                                        00106400
      *--     MISE A JOUR DE LA TABLE DES COMPTES RENDUS                00106500
              PERFORM 23320-CPTRENDU-MAXANO                             00106600
                                                                        00106700
      *--     ARRET DU PROGRAMME                                        00106800
              PERFORM 30000-FIN-PROGRAMME                               00106900
           ELSE                                                         00107100
      *--     APPEL AU SERVICE DE GESTION DES ERREURS => DETERMINE SI   00107300
      *--      L'ANOMALIE NECESSITE L'ARRET IMMEDIAT DU PROGRAMME       00107400
              PERFORM 40000-INITIALISER-PP00SV31                        00107500
              PERFORM 80000-APPEL-PP00SV31                              00107600
                                                                        00107700
              IF LO-IN-ERR-FATA OF LK-CP00SV31 = 'O'                    00107800
      *          MOVE LK-GR-ANO OF WS-GR-ERR                            00155500
                 MOVE LK-GR-ANO OF LK-CP00SV31                          00155500
                   TO LK-GR-ANO OF LK-GR-PPGEMB17                       00155600
                 PERFORM 90000-DISPLAY-ERREUR                           00108200
                 SET CO-RET-KO TO TRUE                                  00108400
                 PERFORM 32000-ABANDON-ERREUR-FAT                       00108500
              ELSE                                                      00108600
                 ADD 1 TO WS-NB-ANO                                     00108700
      *--        INFO : TRANSPORT INFO ANOMALIE POUR GESTION APRES INIT
      *--        PP00SV31
M002             MOVE LK-GR-ANO OF LK-GR-PPGEMB17
M002               TO LK-GR-ANO OF WS-GR-ERR
M017+            PERFORM 23331-INTERCEPT-RC8-BREF
M018+            PERFORM 23332-INTERCEPT-RC8-REM
      *--        APPEL AU SERVICE DE GESTION DES ANOMALIES              00108800
                 IF ERR-ERREUR-TECH                                     00156300
                    PERFORM 23330-GESTION-ANO-TECH                      00109000
                 ELSE                                                   00109100
                    PERFORM 23340-GESTION-ANO-FONC                      00109200
                 END-IF                                                 00109300
                                                                        00109400
      *--        APPEL AU SERVICE DE GESTION DES REPRISES
      *--        EN MISE A JOUR
                 SET IDENTIFIANT-ANO TO TRUE                            00109600
M014+            PERFORM 23350-ALIM-UNIT-FONC-REJ
                 PERFORM 40000-MAJ-RESTART                              00109800
      *--        MISE A JOUR DE LA ZONE TAMPON DE SAUVEGARDE DE         00109900
      *--        L'IDENTIFIANT ANO                                      00110000
                 MOVE WS-ID-CLE-ANO TO WS-ID-CLE-ANO-SAUV               00110100
                                                                        00110200
      *--        APPEL AU SERVICE GESTION DES TRANSACTIONS BATCH        00110300
      *--        POUR REALISER UN COMMIT                                00110400
                 PERFORM 40000-GESTION-COMMIT                           00110500

      *--        ARRET TRAITEMENT EN CAS D'ANO TECH
                 IF ERR-ERREUR-TECH
                    SET CO-RET-KO TO TRUE
                     display '23300-trt   arret suite ano tech'
                    PERFORM 31000-ABANDON
                 END-IF
              END-IF                                                    00110700
           END-IF                                                       00110800
                                                                        00110900
           IF CP00SV21-TRACE-ACTIVE                                     00111000
              MOVE '==>MB17 CLOTURE FICHIER/CURSEUR PR REPRISE'         00111100
                TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE                     00111200
              PERFORM   80000-ALIM-TRACE                                00111300
           END-IF                                                       00111400

      *--  POSITIONNEMENT D'UN REPRISE A EFFECTUER SUITE ERREUR
           SET CP00SV05-MODE-REPRISE TO TRUE
M014+      SET MB17-MODE-REPRISE-ANO-FONC TO TRUE

      *=== GESTION DES FERMETURE/OUVERTURE DES FICHIERS/CURSEUR
      *--  MISE A JOUR DE L'INDICATEUR DE REPRISE
           PERFORM 23400-REINIT-COMPTEURS
           IF WS-CO-CONTXT-TRT-MB17 = 'P'
      *--     FERMETURE DES CURSEURS OUVERTS
              IF WS-CO-CONTXT-TRT-MB17 = 'P'
                  IF PPGEAL20-IS-OPENED
                     IF CP00SV21-TRACE-ACTIVE                           00111000
                        MOVE '      ==>MB17 DEMANDE CLOTURE PPGEAL20-06'00111100
                          TO LI-ID-DON-TRACE
                        SET CP00SV21-NIV-PAR TO TRUE                    00111200
                        PERFORM 80000-ALIM-TRACE                        00111300
                     END-IF                                             00111400

Mac2- *              PERFORM 40000-INITIALISER-PPGEAL20
mac2- *              MOVE 06 TO LK-NO-REQ-ACC OF LK-CPGEAL20
                     SET CPGEAL20-CLOSE TO TRUE
                     PERFORM 80000-APPEL-PPGEAL20
                  END-IF

                  IF PPGEAL21-IS-OPENED
                     IF CP00SV21-TRACE-ACTIVE                           00111000
                        MOVE '      ==>MB17 DEMANDE DE CLOTURE PPGEAL21'00111100
                          TO LI-ID-DON-TRACE
                        SET CP00SV21-NIV-PAR TO TRUE                    00111200
                        PERFORM 80000-ALIM-TRACE                        00111300
                     END-IF                                             00111400

mac2- *              PERFORM 40000-INITIALISER-PPGEAL21
                     SET CPGEAL21-CLOSE TO TRUE
                     PERFORM 80000-APPEL-PPGEAL21
                  END-IF
              END-IF

              PERFORM 50000-OUV-SELEC-OCC
           END-IF


           IF WS-CO-CONTXT-TRT-MB17 = 'M'
              IF CP00SV21-TRACE-ACTIVE                                  00111000
                 MOVE '      ==>MB17 CLOTURE FICHIER '                  00111100
                   TO LI-ID-DON-TRACE
                 SET CP00SV21-NIV-PAR TO TRUE                           00111200
                 PERFORM 80000-ALIM-TRACE                               00111300
              END-IF                                                    00111400

              PERFORM 48000-CLOSE-FC99FIS1
              PERFORM 48000-CLOSE-FCGEEDF1

              PERFORM 40000-OPEN-FCGEEDF1
M015+         INITIALIZE WS-GR-CLE-EDF
              IF OK-FCGEEDF1
                 PERFORM 40000-OPEN-FC99FIS1
              END-IF
           END-IF
           .                                                            00111500

      ******************************************************************00111600
      * INITIALISATION DES PARAMETRES D'APPEL AU SERVICE DES            00111700
      * TRANSACTIONS BATCH POUR REALISER UN ROLLBACK                    00111800
      ******************************************************************00111900
       23310-GESTION-ROLLBACK.                                          00112000
      *-----------------------                                          00112100
      *--  GESTION DES TRACES                                           00112200
           IF CP00SV21-TRACE-ACTIVE                                     00112300
              MOVE '==>MB17 23310-GESTION-ROLLBACK' TO LI-ID-DON-TRACE  00112400
              SET       CP00SV21-NIV-PAR    TO TRUE                     00112500
              PERFORM   80000-ALIM-TRACE                                00112600
           END-IF                                                       00112700
                                                                        00112800
      *--  APPEL AU SERVICE DE GESTION DES TRANSACTIONS BATCH           00112900
           PERFORM 40000-INITIALISER-PP00SV02                           00113000
           SET CP00SV02-ROLLBACK     TO TRUE                            00113100
           PERFORM 80000-APPEL-PP00SV02                                 00113200
                                                                        00113500
      *--  SI L'INDICE DE RELECTURE DU PAS DE COMMIT EST A OUI          00113600
      *--  => RAFRAICHISSEMENT DE LA DATE ET DE L'HEURE                 00113700
      *--  => LECTURE DU PAS DE COMMIT 'ACTUALISE' EN FONCTION          00113800
      *--     DE LA DATE ET DE L'HEURE SYSTEME                          00113900
           IF LO-IN-RLEC = 'O'                                          00114000
              PERFORM 12100-RECUP-PAS-COMMIT                            00114100
           END-IF                                                       00114200
           .                                                            00114300
      ******************************************************************00114400
      * INITIALISATION DES PARAMETRES D'APPEL AU SERVICE DE GESTION     00114500
      * DES COMPTES RENDUS EN MODE DEPASSEMENT SUR LE NOMBRE            00114600
      * D'ANOMALIES                                                     00114700
      ******************************************************************00114800
       23320-CPTRENDU-MAXANO.
      *----------------------                                           00115000
      *--  GESTION DES TRACES                                           00115100
           IF CP00SV21-TRACE-ACTIVE                                     00115200
              MOVE '==>MB17 23320-CPTRENDU-MAXANO' TO LI-ID-DON-TRACE   00115300
              SET       CP00SV21-NIV-PAR    TO TRUE                     00115400
              PERFORM   80000-ALIM-TRACE                                00115500
           END-IF                                                       00115600
                                                                        00115700
           MOVE    6 TO WS-CO-TYPE-LIG-CRU                              00115800
           PERFORM 40000-INITIALISER-PP00SV04                           00115900
           PERFORM 80000-APPEL-PP00SV04                                 00116000
           .                                                            00116100
      ******************************************************************00116200
      * INITIALISATION DES PARAMETRES D'APPEL AU SERVICE DES GESTIONS   00116300
      * D'ANOMALIE                                                      00116400
      ******************************************************************00116500
       23330-GESTION-ANO-TECH.
      *------------------------------*                                  00116700
      *--  GESTION DES TRACES                                           00116800
           IF CP00SV21-TRACE-ACTIVE                                     00116900
              MOVE '==>MB17 23330-GESTION-ANO-TECH' TO LI-ID-DON-TRACE  00117000
              SET       CP00SV21-NIV-PAR    TO TRUE                     00117100
              PERFORM   80000-ALIM-TRACE                                00117200
           END-IF                                                       00117300
                                                                        00117400
      *--  INITIALISATION DES ZONES TECHNIQUES COMMUNES                 00117500
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI    OF LK-CP00SV16  00117600
                                       LK-ID-PGM-APLT   OF LK-CP00SV16  00117700
           MOVE 'PP00SV16'          TO LK-ID-PGM-APLE   OF LK-CP00SV16  00117800
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT  OF LK-CP00SV16  00117900
           SET CP00SV16-APPEL-BATCH TO TRUE                             00118000
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT   OF LK-CP00SV16  00118100
           MOVE WS-TI-TRT           TO LK-TI-TRT        OF LK-CP00SV16  00118200
           MOVE WS-ID-UTIL          TO LK-CO-UTIL       OF LK-CP00SV16  00118300
                                                                        00118400
      *--  ZONE SPECIFIQUE AU SERVICE DE GESTION DES ANOMALIES          00118500
           MOVE LO-ID-JOB  OF LK-CP00SV09 TO LI-ID-JOB  OF LK-CP00SV16  00118600
           MOVE LO-NO-JOB  OF LK-CP00SV09 TO LI-NO-JOB  OF LK-CP00SV16  00118700
           MOVE LO-ID-STEP OF LK-CP00SV09 TO LI-ID-STEP OF LK-CP00SV16  00118800
                                                                        00118900
      *--  ZONES DEDIES A L'ANOMALIE                                    00119000
           ADD 1 TO WS-ID-ANO                                           00119100
           ADD 1 TO WS-ID-ANO-SAUV                                      00119200
           MOVE WS-ID-ANO                                               00119300
                TO LI-ID-ANO          OF LK-CP00SV16                    00119400
           MOVE LK-ID-PGM-APEL-ERR OF WS-GR-ERR                         00166600
                TO LI-ID-PGM-APEL-ERR OF LK-CP00SV16                    00119600
           MOVE LK-ID-PGM-ERR      OF WS-GR-ERR                         00166800
                TO LI-ID-PGM-ERR      OF LK-CP00SV16                    00119800
           MOVE LK-CO-RET-1ER-MOD  OF WS-GR-ERR                         00167000
                TO LI-CO-RET-1ER-MOD  OF LK-CP00SV16                    00120000
           MOVE LK-CO-RET-2ND-MOD  OF WS-GR-ERR                         00167200
                TO LI-CO-RET-2ND-MOD  OF LK-CP00SV16                    00120200
           MOVE LK-CO-MSG-MOD      OF WS-GR-ERR                         00167400
                TO LI-CO-MSG-MOD      OF LK-CP00SV16                    00120400
           MOVE LK-CO-MSG-MOD      OF WS-GR-ERR                         00167600
                TO LI-CO-MSG          OF LK-CP00SV17                    00120600
           MOVE LK-LB-MSG-LNG-MOD       OF WS-GR-ERR                    00167800
                TO LI-LB-MSG-VAR      OF LK-CP00SV17                    00120800
           PERFORM 80000-ACCES-LIBELLE                                  00120900
                                                                        00121000
           MOVE LO-LB-LNG          OF LK-CP00SV17                       00121100
                TO LI-LB-MSG-LNG-MOD OF LK-CP00SV16                     00121200
           MOVE LK-CO-SQL               OF WS-GR-ERR                    00168400
                TO LI-CO-SQL               OF LK-CP00SV16               00121400
           MOVE LK-LB-SQLCA             OF WS-GR-ERR                    00168600
                TO LI-LB-SQLCA             OF LK-CP00SV16               00121600
           MOVE LK-LB-ACT-SQL           OF WS-GR-ERR                    00168800
                TO LI-LB-ACT-SQL           OF LK-CP00SV16               00121800
           MOVE LK-NO-ORD-REQ           OF WS-GR-ERR                    00169000
                TO LI-NO-ORD-REQ           OF LK-CP00SV16               00122000
           MOVE LK-LB-CRIT-REQ          OF WS-GR-ERR                    00169200
                TO LI-LB-CRIT-REQ       OF LK-CP00SV16                  00122200


M014+      EVALUATE WS-CO-CONTXT-TRT-MB17
M014+         WHEN 'P'
                 MOVE WS-ID-UNIT-FONC-LUE                               00122300
                      TO LI-ID-UNIT-FONC-REJ OF LK-CP00SV16             00122400
M014+         WHEN 'M'
M015+            IF NON-LECT-TTE-LF-ULF
M015+               MOVE WS-ID-UNIT-FONC-LUE                            00122300
M014+                 TO LI-ID-UNIT-FONC-REJ OF LK-CP00SV16             00122400
M015+            ELSE
M014+               MOVE WS-ID-UNIT-FONC-PREC                           00122300
M014+                 TO LI-ID-UNIT-FONC-REJ OF LK-CP00SV16             00122400
M015+            END-IF
M014+         WHEN OTHER
M014+            CONTINUE
M014+      END-EVALUATE
                                                                        00122500
           PERFORM 40000-REFRESH-DATE                                   00122600
           MOVE WS-TS-DATE       TO LI-TS-CREA-ANO OF LK-CP00SV16       00169800
                                                                        00122800
           MOVE WS-GR-TS-MAJ     TO LI-TS-MAJ-ANO  OF LK-CP00SV16       00122900
                                                                        00123000
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              DISPLAY ' --------- CHECK PP00SV16---------------'        00130000
              DISPLAY ' LI-ID-JOB   :'    LI-ID-JOB  OF LK-CP00SV16     00130000
              DISPLAY ' LI-ID-STEP  :'    LI-ID-STEP  OF LK-CP00SV16    00130000
              DISPLAY ' LI-NO-JOB   :'    LI-NO-JOB  OF LK-CP00SV16     00130000
              DISPLAY ' LI-ID-ANO   :'    LI-ID-ANO  OF LK-CP00SV16     00130000
              DISPLAY ' LI-ID-NM-PGM-APEL:' LI-ID-PGM-APEL-ERR          00130000
               OF LK-CP00SV16
              DISPLAY ' LI-ID-NM-PGM-ERR :' LI-ID-PGM-ERR               00130000
               OF LK-CP00SV16
              DISPLAY ' LI-CO-RET-1ER:' LI-CO-RET-1ER-MOD OF LK-CP00SV1600130000
              DISPLAY ' LI-CO-RET-2ND:' LI-CO-RET-2ND-MOD OF LK-CP00SV1600130000
              DISPLAY ' LI-CO-MSG    :' LI-CO-MSG-MOD OF LK-CP00SV16    00130000
              DISPLAY ' LI-ID-UNIT-FONC-REJ:' LI-ID-UNIT-FONC-REJ       00130000
               OF LK-CP00SV16                                           00130000
           END-IF

      *--  APPEL AU SERVICE DE GESTION DES ANOMALIES                    00123100
           PERFORM 80000-APPEL-PP00SV16                                 00123200
           .                                                            00123300
      ******************************************************************00123400
       23340-GESTION-ANO-FONC.
      *-----------------------                                          00123600
      *--  GESTION DES TRACES                                           00123700
           IF CP00SV21-TRACE-ACTIVE                                     00123800
              MOVE '==>MB17 23340-GESTION-ANO-FONC' TO LI-ID-DON-TRACE  00123900
              SET       CP00SV21-NIV-PAR    TO TRUE                     00124000
              PERFORM   80000-ALIM-TRACE                                00124100
           END-IF                                                       00124200
                                                                        00124300
      *--  INITIALISATION DES ZONES TECHNIQUES COMMUNES                 00124400
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI    OF LK-CP00SV32  00124500
                                       LK-ID-PGM-APLT   OF LK-CP00SV32  00124600
           MOVE 'PP00SV32'          TO LK-ID-PGM-APLE   OF LK-CP00SV32  00124700
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT  OF LK-CP00SV32  00124800
           SET CP00SV32-APPEL-BATCH TO TRUE                             00124900
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT   OF LK-CP00SV32  00125000
           MOVE WS-TI-TRT           TO LK-TI-TRT        OF LK-CP00SV32  00125100
           MOVE WS-ID-UTIL          TO LK-CO-UTIL       OF LK-CP00SV32  00125200
                                                                        00125300
      *--  ZONE SPECIFIQUE AU SERVICE DE GESTION DES ANOMALIES          00125400
           MOVE 'PPGEMB17'              TO LI-ID-NM-PGM OF LK-CP00SV32  00125500
           MOVE LO-ID-JOB  OF LK-CP00SV09 TO LI-ID-JOB  OF LK-CP00SV32  00125600
           MOVE LO-NO-JOB  OF LK-CP00SV09 TO LI-NO-JOB  OF LK-CP00SV32  00125700
           MOVE LO-ID-STEP OF LK-CP00SV09 TO LI-ID-STEP OF LK-CP00SV32  00125800
                                                                        00125900
           ADD 1 TO WS-ID-ANO-FONC                                      00126000
           ADD 1 TO WS-ID-ANO-FONC-SAUV                                 00126100
           MOVE WS-ID-ANO-FONC                                          00126200
                TO LI-ID-ANO            OF LK-CP00SV32                  00126300
           MOVE LK-ID-PGM-APEL-ERR      OF WS-GR-ERR                    00173500
                TO LI-ID-NM-PGM-APEL    OF LK-CP00SV32                  00126500
           MOVE LK-ID-PGM-ERR           OF WS-GR-ERR                    00173700
                TO LI-ID-NM-PGM-ERR     OF LK-CP00SV32                  00126700
           MOVE LK-CO-RET-1ER-MOD       OF WS-GR-ERR                    00173900
                TO LI-CO-RET-1ER        OF LK-CP00SV32                  00126900
           MOVE LK-CO-RET-2ND-MOD       OF WS-GR-ERR                    00174100
                TO LI-CO-RET-2ND        OF LK-CP00SV32                  00127100
           MOVE LK-CO-MSG-MOD           OF WS-GR-ERR                    00174300
                TO LI-CO-MSG            OF LK-CP00SV32                  00127300
                                                                        00127400
           MOVE LK-CO-MSG-MOD           OF WS-GR-ERR                    00174600
                TO LI-CO-MSG            OF LK-CP00SV17                  00127600
           MOVE LK-LB-MSG-LNG-MOD       OF WS-GR-ERR                    00174800
                TO LI-LB-MSG-VAR        OF LK-CP00SV17                  00127800
           PERFORM 80000-ACCES-LIBELLE                                  00127900
           MOVE LO-LB-LNG               OF LK-CP00SV17                  00128000
                TO LI-LB-MSG            OF LK-CP00SV32                  00128100
                                                                        00128200
           MOVE LK-CO-SQL               OF WS-GR-ERR                    00175400
                TO LI-CO-SQL            OF LK-CP00SV32                  00128400
           MOVE LK-LB-SQLCA             OF WS-GR-ERR                    00175600
                TO LI-LB-SQLCA          OF LK-CP00SV32                  00128600
           MOVE LK-LB-ACT-SQL           OF WS-GR-ERR                    00175800
                TO LI-LB-ACT-SQL        OF LK-CP00SV32                  00128800
           MOVE LK-NO-ORD-REQ           OF WS-GR-ERR                    00176000
                TO LI-NO-ORD-REQ        OF LK-CP00SV32                  00129000
           MOVE LK-LB-CRIT-REQ          OF WS-GR-ERR                    00176200
                TO LI-LB-CRIT-REQ       OF LK-CP00SV32                  00129200
                                                                        00129500
M014+      EVALUATE WS-CO-CONTXT-TRT-MB17
M014+         WHEN 'P'
                 MOVE WS-ID-UNIT-FONC-LUE                               00129300
                      TO LI-ID-UNIT-FONC-REJ OF LK-CP00SV32             00129400
M014+         WHEN 'M'
M015+ *          IF CNTL-EXPLT-DATA-KO
M015+            IF NON-LECT-TTE-LF-ULF
m016+               IF CP00SV21-TRACE-ACTIVE
M016+                  display '-->NON-LECT-TTE-LF-ULF'
M016                END-IF

M015+               MOVE WS-ID-UNIT-FONC-LUE
M015+                    TO LI-ID-UNIT-FONC-REJ OF LK-CP00SV32
                 ELSE
m016+               IF CP00SV21-TRACE-ACTIVE
M016+                  display '-->OUI-LECT-TTE-LF-ULF-OK'
M016                END-IF

M014+               MOVE WS-ID-UNIT-FONC-PREC                           00122300
M014+                 TO LI-ID-UNIT-FONC-REJ OF LK-CP00SV32             00122400
M015+            END-IF
M014+         WHEN OTHER
M014+            CONTINUE
M014+      END-EVALUATE
                                                                        00122500
                                                                        00129500
           PERFORM 40000-REFRESH-DATE                                   00129600
           MOVE WS-TS-DATE       TO LI-TS-CREA-ANO OF LK-CP00SV32       00176800
                                                                        00129800
           MOVE WS-GR-TS-MAJ     TO LI-TS-MAJ-ANO  OF LK-CP00SV32       00129900
                                                                        00130000
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              DISPLAY ' --------- CHECK PP00SV32---------------'        00130000
              DISPLAY ' LI-ID-NM-PGM:'    LI-ID-NM-PGM OF LK-CP00SV32   00130000
              DISPLAY ' LI-ID-JOB   :'    LI-ID-JOB  OF LK-CP00SV32     00130000
              DISPLAY ' LI-ID-STEP  :'    LI-ID-STEP  OF LK-CP00SV32    00130000
              DISPLAY ' LI-NO-JOB   :'    LI-NO-JOB  OF LK-CP00SV32     00130000
              DISPLAY ' LI-ID-ANO   :'    LI-ID-ANO  OF LK-CP00SV32     00130000
              DISPLAY ' LI-ID-NM-PGM-APEL:' LI-ID-NM-PGM-APEL           00130000
               OF LK-CP00SV32
              DISPLAY ' LI-ID-NM-PGM-ERR :' LI-ID-NM-PGM-ERR            00130000
               OF LK-CP00SV32
              DISPLAY ' LI-CO-RET-1ER    :' LI-CO-RET-1ER OF LK-CP00SV3200130000
              DISPLAY ' LI-CO-RET-2ND    :' LI-CO-RET-2ND OF LK-CP00SV3200130000
              DISPLAY ' LI-CO-MSG        :' LI-CO-MSG OF LK-CP00SV32    00130000
              DISPLAY ' LI-ID-UNIT-FONC-REJ:' LI-ID-UNIT-FONC-REJ       00130000
               OF LK-CP00SV32                                           00130000
           END-IF

      *--  APPEL AU SERVICE DE GESTION DES ANOMALIES                    00130100
           PERFORM 80000-APPEL-PP00SV32                                 00130200
           .                                                            00130300
M014+ *--------------------------*
M014+   23350-ALIM-UNIT-FONC-REJ.
M014+ *--------------------------*
M014+      EVALUATE WS-CO-CONTXT-TRT-MB17
M014+         WHEN 'P'
M014+            MOVE WS-ID-UNIT-FONC-LUE TO WS-ID-CLE-ANO              00122300
M014+         WHEN 'M'
M015+            IF NON-LECT-TTE-LF-ULF
M014+               MOVE WS-ID-UNIT-FONC-LUE  TO WS-ID-CLE-ANO          00122300
M015+            ELSE
M014+               MOVE WS-ID-UNIT-FONC-PREC TO WS-ID-CLE-ANO          00122300
M015+            END-IF
M014+         WHEN OTHER
M014+            CONTINUE
M014+      END-EVALUATE
           .

      ******************************************************************00130400
      * REINITIALISATION DES COMPTEURS                                  00130500
      ******************************************************************00130600
       23400-REINIT-COMPTEURS.
      *-----------------------                                          00130800
      *--  GESTION DES TRACES                                           00130900
           IF CP00SV21-TRACE-ACTIVE                                     00131000
              MOVE    '23400-REINIT-COMPTEURS'  TO LI-ID-DON-TRACE      00131100
              SET       CP00SV21-NIV-PAR    TO TRUE                     00131200
              PERFORM   80000-ALIM-TRACE                                00131300
           END-IF                                                       00131400
                                                                        00131500
      *--  REINIT                                                       00131600
           MOVE WS-CO-STUT-SAUV      TO WS-CO-STUT-STEP                 00131700
           display 'WS-ID-CLE-RPRI-SAUV:'WS-ID-CLE-RPRI-SAUV (1:25)
           MOVE WS-ID-CLE-RPRI-SAUV  TO WS-ID-CLE-RPRI                  00131800
                                                                        00179000
           PERFORM 28000-ALIM-VAR-AFTER-REPRISE
                                                                        00179700
           MOVE WS-ID-CLE-ANO-SAUV   TO WS-ID-CLE-ANO                   00131900
           MOVE WS-NB-OCC-SAUV       TO WS-NB-OCC                       00132000
           MOVE WS-NB-ANO-SAUV       TO WS-NB-ANO                       00132100
           MOVE WS-NB-ROLL-SAUV      TO WS-NB-ROLL                      00132200
           MOVE WS-ID-ANO-SAUV       TO WS-ID-ANO                       00132300
           MOVE WS-ID-ANO-FONC-SAUV  TO WS-ID-ANO-FONC                  00132400
           .
      *-------------------------*
       28000-ALIM-VAR-AFTER-REPRISE.
      *-------------------------*
      *--  GESTION DES TRACES                                           00181000
           IF CP00SV21-TRACE-ACTIVE                                     00181100
              STRING'==>MB17 28000-ALIM-VAR-AFTER-REPRISE'
              'WS-ID-CLE-RPRI(1:4):'WS-ID-CLE-RPRI(1:13)
              DELIMITED BY SIZE
              INTO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW                              00181400
           END-IF                                                       00181500
                                                                        00181600
           IF WS-ID-CLE-RPRI(1:4) = SPACE
      **?     MOVE ZERO
      **?         TO WS-ID-TECH-ENV-RPRI OF WS-GR-ZONE-RPRI
              MOVE SPACE
                  TO WS-NO-INST-RPRI WS-CO-REG-RPRI
              MOVE ZERO
                  TO WS-ID-TECH-INDV-RPRI OF WS-GR-ZONE-RPRI
           ELSE
              MOVE WS-ID-CLE-RPRI(01:01)
                TO WS-CO-REG-RPRI  OF WS-GR-ZONE-RPRI
              MOVE WS-ID-CLE-RPRI(02:03)
                TO WS-NO-INST-RPRI OF WS-GR-ZONE-RPRI
              MOVE WS-ID-CLE-RPRI(05:09)
                TO WS-ID-TECH-INDV-RPRI OF WS-GR-ZONE-RPRI
           END-IF

      *--  GESTION DES TRACES                                           00181000
           IF CP00SV21-TRACE-ACTIVE                                     00181100
              STRING '==>MB17 FIN-28000-ALIM-VAR-AFTER-REPRISE'
                     '  ;WS-GR-ZONE-RPRI=' WS-GR-ZONE-RPRI '<'
              DELIMITED BY SIZE INTO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW                              00181400
           END-IF                                                       00181500
                                                                        00181600
           .                                                            00133100

      ******************************************************************00133200
      * TRAITEMENT DES COMPTEUR POUR COMPTE RENDU FONCTIONNEL           00180600
      ******************************************************************00180700
       25000-COMPTEURS-ALL-TRAITE.
      *-----------------------------------*
      *--  GESTION DES TRACES                                           00181000
           IF CP00SV21-TRACE-ACTIVE                                     00181100
              MOVE '==>PPGEMB17 25000-COMPTEURS-ALL-TRAITE'
              TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW                              00181400
           END-IF                                                       00181500
                                                                        00181600
           ADD 1 TO WS-NB-AL-AF-ENV-6
                                                                        00183300
           ADD WS-NB-TP-EXO-AF-ENV-7      TO WS-NB-AL-EXO-AF-ENV-7
           ADD WS-NB-TP-NEXO-AF-ENV-FR-8  TO WS-NB-AL-NEXO-AF-ENV-FR-8
           ADD WS-NB-TP-NEXO-AF-ENV-ETRG-9
            TO WS-NB-AL-NEXO-AF-ENV-ETRG-9

      * ALIMENTATION DES COMPTEURS TEMPORAIRE(MONTANT TOTAL AL ENVOYE)
              ADD WS-MT-BRT-TOT-TMP
                  TO WS-MT-BRT-TOT-6            WS-MT-BRT-TOT-1
              ADD WS-MT-NET-FIS-TOT-TMP
                  TO WS-MT-NET-FIS-TOT-6        WS-MT-NET-FIS-TOT-1
              ADD WS-MT-NET-RET-TOT-TMP
                  TO WS-MT-NET-RET-TOT-6        WS-MT-NET-RET-TOT-1
              ADD WS-MT-TOT-RET-SRC-TMP
                  TO WS-MT-TOT-RET-SRC-6        WS-MT-TOT-RET-SRC-1
              ADD WS-MT-TOT-MAJ-EC-TMP
                  TO WS-MT-TOT-MAJ-EC-6         WS-MT-TOT-MAJ-EC-1
              ADD WS-MT-TOT-MAJ-EE-TMP
                  TO WS-MT-TOT-MAJ-EE-6         WS-MT-TOT-MAJ-EE-1
              ADD WS-MT-TOT-COT-SS-TMP
                  TO WS-MT-TOT-COT-SS-6         WS-MT-TOT-COT-SS-1
              ADD WS-MT-TOT-CSG-NDDUC-EE-TMP
                  TO WS-MT-TOT-CSG-NDDUC-EE-6   WS-MT-TOT-CSG-NDDUC-EE-1
              ADD WS-MT-TOT-CSG-DDUC-HEE-TMP
                  TO WS-MT-TOT-CSG-DDUC-HEE-6   WS-MT-TOT-CSG-DDUC-HEE-1
              ADD WS-MT-TOT-CSG-NDDUC-TMP
                  TO WS-MT-TOT-CSG-NDDUC-6      WS-MT-TOT-CSG-NDDUC-1
              ADD WS-MT-TOT-RDS-TMP
                  TO WS-MT-TOT-RDS-6            WS-MT-TOT-RDS-1
              ADD WS-MT-TOT-RPL-ANT-TMP
                  TO WS-MT-TOT-RPL-ANT-6        WS-MT-TOT-RPL-ANT-1


      * ALIMENTATION DES COMPTEURS TEMPORAIRE
           IF WS-IN-PEC-DGI-EDF-PREC = 'N'
              ADD WS-MT-BRT-TOT-TMP          TO WS-MT-BRT-TOT-7
              ADD WS-MT-NET-FIS-TOT-TMP      TO WS-MT-NET-FIS-TOT-7
              ADD WS-MT-NET-RET-TOT-TMP      TO WS-MT-NET-RET-TOT-7
              ADD WS-MT-TOT-RET-SRC-TMP      TO WS-MT-TOT-RET-SRC-7
              ADD WS-MT-TOT-MAJ-EC-TMP       TO WS-MT-TOT-MAJ-EC-7
              ADD WS-MT-TOT-MAJ-EE-TMP       TO WS-MT-TOT-MAJ-EE-7
              ADD WS-MT-TOT-COT-SS-TMP       TO WS-MT-TOT-COT-SS-7
              ADD WS-MT-TOT-CSG-NDDUC-EE-TMP
               TO WS-MT-TOT-CSG-NDDUC-EE-7

              ADD WS-MT-TOT-CSG-DDUC-HEE-TMP
               TO WS-MT-TOT-CSG-DDUC-HEE-7
              ADD WS-MT-TOT-CSG-NDDUC-TMP    TO WS-MT-TOT-CSG-NDDUC-7
              ADD WS-MT-TOT-RDS-TMP          TO WS-MT-TOT-RDS-7
              ADD WS-MT-TOT-RPL-ANT-TMP      TO WS-MT-TOT-RPL-ANT-7
           ELSE                                                         00185900
              IF WS-IN-RESI-ETRG-EDF-PREC = 'N'
                 ADD WS-MT-BRT-TOT-TMP       TO WS-MT-BRT-TOT-8
                 ADD WS-MT-NET-FIS-TOT-TMP   TO WS-MT-NET-FIS-TOT-8
                 ADD WS-MT-NET-RET-TOT-TMP   TO WS-MT-NET-RET-TOT-8
                 ADD WS-MT-TOT-RET-SRC-TMP   TO WS-MT-TOT-RET-SRC-8
                 ADD WS-MT-TOT-MAJ-EC-TMP    TO WS-MT-TOT-MAJ-EC-8
                 ADD WS-MT-TOT-MAJ-EE-TMP    TO WS-MT-TOT-MAJ-EE-8
                 ADD WS-MT-TOT-COT-SS-TMP    TO WS-MT-TOT-COT-SS-8
                 ADD WS-MT-TOT-CSG-NDDUC-EE-TMP
                  TO WS-MT-TOT-CSG-NDDUC-EE-8
                 ADD WS-MT-TOT-CSG-DDUC-HEE-TMP
                  TO WS-MT-TOT-CSG-DDUC-HEE-8
                 ADD WS-MT-TOT-CSG-NDDUC-TMP TO WS-MT-TOT-CSG-NDDUC-8
                 ADD WS-MT-TOT-RDS-TMP       TO WS-MT-TOT-RDS-8
                 ADD WS-MT-TOT-RPL-ANT-TMP   TO WS-MT-TOT-RPL-ANT-8
              ELSE                                                      00188300
                 ADD WS-MT-BRT-TOT-TMP       TO WS-MT-BRT-TOT-9
                 ADD WS-MT-NET-FIS-TOT-TMP   TO WS-MT-NET-FIS-TOT-9
                 ADD WS-MT-NET-RET-TOT-TMP   TO WS-MT-NET-RET-TOT-9
                 ADD WS-MT-TOT-RET-SRC-TMP   TO WS-MT-TOT-RET-SRC-9
                 ADD WS-MT-TOT-MAJ-EC-TMP    TO WS-MT-TOT-MAJ-EC-9
                 ADD WS-MT-TOT-MAJ-EE-TMP    TO WS-MT-TOT-MAJ-EE-9
                 ADD WS-MT-TOT-COT-SS-TMP    TO WS-MT-TOT-COT-SS-9
                 ADD WS-MT-TOT-CSG-NDDUC-EE-TMP
                  TO WS-MT-TOT-CSG-NDDUC-EE-9
                 ADD WS-MT-TOT-CSG-DDUC-HEE-TMP
                  TO WS-MT-TOT-CSG-DDUC-HEE-9
                 ADD WS-MT-TOT-CSG-NDDUC-TMP TO WS-MT-TOT-CSG-NDDUC-9
                 ADD WS-MT-TOT-RDS-TMP       TO WS-MT-TOT-RDS-9
                 ADD WS-MT-TOT-RPL-ANT-TMP   TO WS-MT-TOT-RPL-ANT-9
              END-IF                                                    00190600
           END-IF                                                       00190700
                                                                        00190800
      **** SAUVEGARDE DES DIVERS COMPTEURS                              00196000
           MOVE WS-CT-FONC    TO WS-CT-SAV                              00196100
           .

      ******************************************************************
      * TRAITEMENT DES COMPTEUR POUR COMPTE RENDU FONCTIONNEL
      ******************************************************************
       25000-COMPTEUR-ALL-ANO.
      *----------------------
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>PPGEMB17 25000-COMPTEUR-ALL-ANO'
              TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF

      *    DISPLAY '-- ///////////////////////:'
      *    DISPLAY '-- < NB-AL-ANO-BLQ-AF-2 = :'WS-NB-AL-ANO-BLQ-AF-2
      *    DISPLAY '-- < CLE SRC ANO:'WS-GR-ENV-PEX
           PERFORM UNTIL (WS-GR-CLE-EDF > WS-GR-ENV-PEX)
           OR EOF-FCGEEDF1
               COMPUTE WS-MT-VTIL-LIF-COMP =
M004+ *               LI-LIF-MT-VTIL-LIF OF LI-GR-LIF
               FUNCTION NUMVAL(LI-LIF-MT-VTIL-LIF OF LI-GR-LIF)

               EVALUATE LI-LIF-CO-VTIL-MT-LIF OF WS-GR-EDF
                  WHEN 'MB    '
                    ADD WS-MT-VTIL-LIF-COMP
                      TO WS-MT-BRT-TOT-TMP
                  WHEN 'TMAJEE'
                    ADD WS-MT-VTIL-LIF-COMP
                      TO WS-MT-TOT-MAJ-EE-TMP
                  WHEN 'TMAJEC'
                    ADD WS-MT-VTIL-LIF-COMP
                      TO WS-MT-TOT-MAJ-EC-TMP
                  WHEN 'TCOTSS'
                    ADD WS-MT-VTIL-LIF-COMP
                      TO WS-MT-TOT-COT-SS-TMP
                  WHEN 'TCSGND'
                    ADD WS-MT-VTIL-LIF-COMP
                      TO WS-MT-TOT-CSG-NDDUC-TMP
                  WHEN 'TCSGD '
                    ADD WS-MT-VTIL-LIF-COMP
                      TO WS-MT-TOT-CSG-DDUC-HEE-TMP
                  WHEN 'TCNDEE'
      *             COMPUTE WS-MT-TOT-CSG-NDDUC-EE-TMP =
      *             FUNCTION NUMVAL(LI-LIF-MT-VTIL-LIF OF LI-GR-LIF)
      *             + WS-MT-TOT-CSG-NDDUC-EE-TMP
                    ADD WS-MT-VTIL-LIF-COMP
                      TO WS-MT-TOT-CSG-NDDUC-EE-TMP
                  WHEN 'TCRSD '
                     ADD WS-MT-VTIL-LIF-COMP
                      TO WS-MT-TOT-RDS-TMP
                  WHEN 'MNF   '
                    ADD WS-MT-VTIL-LIF-COMP
                      TO WS-MT-NET-FIS-TOT-TMP
                  WHEN 'MRA   '
                    ADD WS-MT-VTIL-LIF-COMP
                      TO WS-MT-TOT-RPL-ANT-TMP
                  WHEN 'RAL   '
                    ADD WS-MT-VTIL-LIF-COMP
                      TO WS-MT-TOT-RET-SRC-TMP
                  WHEN OTHER
                     CONTINUE
               END-EVALUATE

               PERFORM 41000-LECTURE-FCGEEDF1
           END-PERFORM
      *    DISPLAY '-- < CLE RUP    :' WS-GR-CLE-EDF

           ADD 1 TO WS-NB-AL-ANO-BLQ-AF-2
      *    DISPLAY '-- < ADD 1  =   :' WS-NB-AL-ANO-BLQ-AF-2

      *--  GESTION MONTANT COMPTEUR
           ADD WS-MT-BRT-TOT-TMP
               TO WS-MT-BRT-TOT-2              WS-MT-BRT-TOT-1
           ADD WS-MT-NET-FIS-TOT-TMP
               TO WS-MT-NET-FIS-TOT-2          WS-MT-NET-FIS-TOT-1
           ADD WS-MT-NET-RET-TOT-TMP
               TO WS-MT-NET-RET-TOT-2          WS-MT-NET-RET-TOT-1
           ADD WS-MT-TOT-RET-SRC-TMP
               TO WS-MT-TOT-RET-SRC-2          WS-MT-TOT-RET-SRC-1
           ADD WS-MT-TOT-MAJ-EC-TMP
               TO WS-MT-TOT-MAJ-EC-2           WS-MT-TOT-MAJ-EC-1
           ADD WS-MT-TOT-MAJ-EE-TMP
               TO WS-MT-TOT-MAJ-EE-2           WS-MT-TOT-MAJ-EE-1
           ADD WS-MT-TOT-COT-SS-TMP
               TO WS-MT-TOT-COT-SS-2           WS-MT-TOT-COT-SS-1
           ADD WS-MT-TOT-CSG-NDDUC-EE-TMP
               TO WS-MT-TOT-CSG-NDDUC-EE-2     WS-MT-TOT-CSG-NDDUC-EE-1
           ADD WS-MT-TOT-CSG-DDUC-HEE-TMP
               TO WS-MT-TOT-CSG-DDUC-HEE-2     WS-MT-TOT-CSG-DDUC-HEE-1
           ADD WS-MT-TOT-CSG-NDDUC-TMP
               TO WS-MT-TOT-CSG-NDDUC-2        WS-MT-TOT-CSG-NDDUC-1
           ADD WS-MT-TOT-RDS-TMP
               TO WS-MT-TOT-RDS-2              WS-MT-TOT-RDS-1
           ADD WS-MT-TOT-RPL-ANT-TMP
               TO WS-MT-TOT-RPL-ANT-2          WS-MT-TOT-RPL-ANT-1


           IF WS-IN-PEC-DGI-EDF-PREC = 'O'
              ADD 1 TO WS-NB-AL-EXO-ANO-BLQ-AF-3

              ADD WS-MT-BRT-TOT-TMP          TO WS-MT-BRT-TOT-3
              ADD WS-MT-NET-FIS-TOT-TMP      TO WS-MT-NET-FIS-TOT-3
              ADD WS-MT-NET-RET-TOT-TMP      TO WS-MT-NET-RET-TOT-3
              ADD WS-MT-TOT-RET-SRC-TMP      TO WS-MT-TOT-RET-SRC-3
              ADD WS-MT-TOT-MAJ-EC-TMP       TO WS-MT-TOT-MAJ-EC-3
              ADD WS-MT-TOT-MAJ-EE-TMP       TO WS-MT-TOT-MAJ-EE-3
              ADD WS-MT-TOT-COT-SS-TMP       TO WS-MT-TOT-COT-SS-3
              ADD WS-MT-TOT-CSG-NDDUC-EE-TMP
               TO WS-MT-TOT-CSG-NDDUC-EE-3
              ADD WS-MT-TOT-CSG-DDUC-HEE-TMP
               TO WS-MT-TOT-CSG-DDUC-HEE-3
              ADD WS-MT-TOT-CSG-NDDUC-TMP    TO WS-MT-TOT-CSG-NDDUC-3
              ADD WS-MT-TOT-RDS-TMP          TO WS-MT-TOT-RDS-3
              ADD WS-MT-TOT-RPL-ANT-TMP      TO WS-MT-TOT-RPL-ANT-3
           ELSE
              IF WS-IN-RESI-ETRG-EDF-PREC = 'N'
                 ADD 1 TO WS-NB-AL-NEXO-ANO-BLQ-AF-FR-4

                 ADD WS-MT-BRT-TOT-TMP        TO WS-MT-BRT-TOT-4
                 ADD WS-MT-NET-FIS-TOT-TMP    TO WS-MT-NET-FIS-TOT-4
                 ADD WS-MT-NET-RET-TOT-TMP    TO WS-MT-NET-RET-TOT-4
                 ADD WS-MT-TOT-RET-SRC-TMP    TO WS-MT-TOT-RET-SRC-4
                 ADD WS-MT-TOT-MAJ-EC-TMP     TO WS-MT-TOT-MAJ-EC-4
                 ADD WS-MT-TOT-MAJ-EE-TMP     TO WS-MT-TOT-MAJ-EE-4
                 ADD WS-MT-TOT-COT-SS-TMP     TO WS-MT-TOT-COT-SS-4
                 ADD WS-MT-TOT-CSG-NDDUC-EE-TMP
                  TO WS-MT-TOT-CSG-NDDUC-EE-4
                 ADD WS-MT-TOT-CSG-DDUC-HEE-TMP
                  TO WS-MT-TOT-CSG-DDUC-HEE-4
                 ADD WS-MT-TOT-CSG-NDDUC-TMP  TO WS-MT-TOT-CSG-NDDUC-4
                 ADD WS-MT-TOT-RDS-TMP        TO WS-MT-TOT-RDS-4
                 ADD WS-MT-TOT-RPL-ANT-TMP    TO WS-MT-TOT-RPL-ANT-4
              ELSE
                 ADD 1 TO WS-NB-AL-NEXO-ANO-BLQ-AF-ETG-5

                 ADD WS-MT-BRT-TOT-TMP        TO WS-MT-BRT-TOT-5
                 ADD WS-MT-NET-FIS-TOT-TMP    TO WS-MT-NET-FIS-TOT-5
                 ADD WS-MT-NET-RET-TOT-TMP    TO WS-MT-NET-RET-TOT-5
                 ADD WS-MT-TOT-RET-SRC-TMP    TO WS-MT-TOT-RET-SRC-5
                 ADD WS-MT-TOT-MAJ-EC-TMP     TO WS-MT-TOT-MAJ-EC-5
                 ADD WS-MT-TOT-MAJ-EE-TMP     TO WS-MT-TOT-MAJ-EE-5
                 ADD WS-MT-TOT-COT-SS-TMP     TO WS-MT-TOT-COT-SS-5
                 ADD WS-MT-TOT-CSG-NDDUC-EE-TMP
                  TO WS-MT-TOT-CSG-NDDUC-EE-5
                 ADD WS-MT-TOT-CSG-DDUC-HEE-TMP
                  TO WS-MT-TOT-CSG-DDUC-HEE-5
                 ADD WS-MT-TOT-CSG-NDDUC-TMP  TO WS-MT-TOT-CSG-NDDUC-5
                 ADD WS-MT-TOT-RDS-TMP        TO WS-MT-TOT-RDS-5
                 ADD WS-MT-TOT-RPL-ANT-TMP    TO WS-MT-TOT-RPL-ANT-5
              END-IF
           END-IF

      **** SAUVEGARDE DES DIVERS COMPTEURS
           MOVE WS-CT-FONC    TO WS-CT-SAV
           .                                                            00196300
                                                                        00196400
      *-----------------------*
       28000-CNTL-COMMIT.
      *-----------------------*
      *--  VERIFICATION QUE L'ON A ATTEINT LE PAS DE COMMIT OU NON
           IF WS-NB-OCC-COM = LO-NB-COMMIT OF LK-CP00SV10
              OR CP00SV25-LO-COMMIT-OUI
      *--     APPEL AU SERVICE DE GESTION DES REPRISES EN MISE A
      *--     JOUR
              SET IDENTIFIANT-REP TO TRUE
      *       DISPLAY 'WS-ID-UNIT-FONC-LUE : ' WS-ID-UNIT-FONC-LUE
              MOVE WS-ID-UNIT-FONC-LUE TO WS-ID-CLE-RPRI
                                                                        00197400
      *       DISPLAY 'WS-ID-CLE-RPRI PR COMMIT:' WS-ID-CLE-RPRI
              PERFORM 28000-ALIM-VAR-AFTER-REPRISE
                                                                        00198200
              MOVE WS-CT-FONC        TO WS-CT-SAV
              PERFORM 40000-MAJ-RESTART

      *--     APPEL AU SERVICE GESTION DES TRANSACTIONS BATCH
      *--     POUR REALISER UN COMMIT
              PERFORM 40000-GESTION-COMMIT
              MOVE WS-CT-SAV         TO WS-CT-SAV-SAUV
           END-IF
           .
                                                                        00222800
      ******************************************************************00222900
      * TRAITEMENT DE LA DERNIERE OCCURRENCE                            00133300
      ******************************************************************00133400
       24000-DERNIERE-OCC.
      *-------------------                                              00133600
      *--  GESTION DES TRACES                                           00133700
           IF CP00SV21-TRACE-ACTIVE                                     00133800
              MOVE    '24000-DERNIERE-OCC'  TO LI-ID-DON-TRACE          00133900
              SET       CP00SV21-NIV-PAR    TO TRUE                     00134000
              PERFORM   80000-ALIM-TRACE                                00134100
           END-IF                                                       00134200
                                                                        00134300
      *--  ZONE SPECIFIQUE AU SERVICE DE GESTION DES OCCURRENCES        00134400
                                                                        00134800
      *--  APPEL AU SERVICE DE GESTION DES OCCURRENCES A TRAITER        00134900
      *--  POUR FERMETURE EXPLICITE DU CURSEUR                          00135000
           IF WS-CO-CONTXT-TRT-MB17 = 'P'
              PERFORM 50200-CLOSE-OCC
              SET DERNIERE-OCC TO TRUE
           END-IF
                                                                        00135300
      *--  APPEL AU SERVICE DE GESTION DES REPRISES                     00135400
      *--  AVEC EFFACEMENT DE LA CLEF DE REPRISE, L'IDENTIFIANT         00135500
      *--  D'ANOMALIE ET MISE A JOUR DU STATUT A TERMINE                00135600
           MOVE SPACE TO WS-ID-CLE-RPRI                                 00135700
                         WS-ID-CLE-ANO                                  00135800
           MOVE 'T'   TO WS-CO-STUT-STEP                                00135900
                                                                        00136000
      *--  ALIMENTATION DANS LA TABLE DES RESTART                       00136100
           PERFORM 40000-MAJ-RESTART                                    00136200
                                                                        00136300
      *--  APPEL AU SERVICE DE GESTION DES COMPTES RENDU DE STEP        00136400
      *--  EN MODE TERMINE                                              00136500
           PERFORM 60500-FIN-METIER                                     00136600
           PERFORM 40000-CPTRENDU-TERMINE                               00136700
           .                                                            00136800
      ******************************************************************00136900
      * PARAGRAPHES DE FIN DE PROGRAMME                                 00137000
      ******************************************************************00137100
       30000-FIN-PROGRAMME.
      *--------------------                                             00137300
      *--  GESTION DES TRACES                                           00137400
           IF CP00SV21-TRACE-ACTIVE                                     00137500
              MOVE    '==>PPGEMB17  30000-FIN-PROGRAMME'                00137600
              TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW                              00137800
           END-IF                                                       00137900
                                                                        00138000
      *--  GESTION DES TRACES                                           00138100
           PERFORM 80000-FIN-TRACE                                      00138200
                                                                        00138300
      *--  SI IL N'Y AVAIT PAS D'OCCURRENCES A TRAITER CODE RETOUR = 2  00138400
           IF PAS-OCC-A-TRAITER                                         00138500
              SET CO-RET-OK TO TRUE                                     00138700
           END-IF                                                       00138800
                                                                        00138900
           MOVE WS-CO-RET TO RETURN-CODE                                00139000
           STOP RUN                                                     00139200
           .                                                            00139300
      ******************************************************************00139400
       31000-ABANDON.
      *--------------                                                   00139600
      *--  GESTION DES TRACES                                           00139700
           IF CP00SV21-TRACE-ACTIVE                                     00139800
              MOVE '==>PPGEMB17 31000-ABANDON'  TO LI-ID-DON-TRACE      00139900
              SET       CP00SV21-NIV-PGM    TO TRUE                     00140000
              PERFORM   80000-ALIM-TRACE                                00140100
           END-IF                                                       00140200
                                                                        00140300
           DISPLAY 'ABANDON SUITE A UNE ERREUR TECHNIQUE !!!'           00140400
                                                                        00140500
      *--  GESTION DES TRACES                                           00140600
           PERFORM 80000-FIN-TRACE                                      00140700
                                                                        00140800
      *--  POSITIONNEMENT CODE RETOUR A 12                              00140900
           SET CO-RET-KO TO TRUE                                        00141100
                                                                        00141200
      *--  APPEL AU SERVICE DE GESTION DES TRANSACTIONS BATCH           00141300
           PERFORM 40000-INITIALISER-PP00SV02                           00141400
           SET CP00SV02-ROLLBACK     TO TRUE                            00141500
           PERFORM 80000-APPEL-PP00SV02-ABANDON                         00141600
                                                                        00141700
           MOVE WS-CO-RET TO RETURN-CODE                                00141800
           STOP RUN                                                     00141900
           .                                                            00142000
      ******************************************************************00142100
       32000-ABANDON-ERREUR-FAT.
      *-------------------------                                        00142300
      *--  GESTION DES TRACES                                           00142400
           IF CP00SV21-TRACE-ACTIVE                                     00142500
              MOVE      '32000-ABANDON-ERREUR-FAT' TO LI-ID-DON-TRACE   00142600
              SET       CP00SV21-NIV-PGM    TO TRUE                     00142700
              PERFORM   80000-ALIM-TRACE                                00142800
           END-IF                                                       00142900
                                                                        00143000
           DISPLAY 'ABANDON SUITE A UNE ERREUR FATALE'                  00143100
                                                                        00143200
      *--  GESTION DES TRACES                                           00143300
           PERFORM 80000-FIN-TRACE                                      00143400
                                                                        00144100
           MOVE WS-CO-RET TO RETURN-CODE                                00144200
           STOP RUN                                                     00144300
           .                                                            00144400
      *------------------------*
       21100-RESTIT-IND.
      *------------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>PPGEMB17 21100-PRG625-RESTIT-IND'
               TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PGM    TO TRUE
              PERFORM   80000-ALIM-TRACE
           END-IF

      *--  INITIALIZE PP00SV37
           INITIALIZE LI-GR-FONC-ENT OF LK-CP00SV37

      *--  BREF RESTITUER INDIVIDU WS-ID-TECH-INDV-C13(1) -- [PP00SV37]
      *    DISPLAY 'RECHERCHE INDIVIDU'
           MOVE LO-EDF-ID-TECH-INDV OF LK-CPGEAL20
                              TO LI-ID-TECH-INDV       OF LK-CP00SV37
           MOVE 'O'
                              TO LI-CO-OPT-ADR         OF LK-CP00SV37
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CP00SV37
                                       LK-ID-PGM-APLT  OF LK-CP00SV37
           MOVE 'PP00SV37'          TO LK-ID-PGM-APLE  OF LK-CP00SV37
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CP00SV37
           SET CP00SV37-APPEL-BATCH TO TRUE

           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CP00SV37
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CP00SV37
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CP00SV37

           PERFORM 80000-APPELER-PP00SV37
           .

      ******************************************************************00144500
       40000-INITIALISER-PP00SV02.                                      00144600
      *---------------------------                                      00144700
      *--  INITIALISATION DE LA ZONE DE COMM. FONC. ENTRANTE DE L'APPELE00144800
           INITIALIZE LI-GR-FONC-ENT OF LK-CP00SV02                     00144900
                                                                        00145000
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00145100
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CP00SV02   00145200
                                       LK-ID-PGM-APLT  OF LK-CP00SV02   00145300
           MOVE 'PP00SV02'          TO LK-ID-PGM-APLE  OF LK-CP00SV02   00145400
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CP00SV02   00145500
           SET CP00SV02-APPEL-BATCH TO TRUE                             00145600
                                                                        00145700
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CP00SV02   00145800
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CP00SV02   00145900
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CP00SV02   00146000
                                                                        00146100
      *--  ALIMENTATION DES ZONES SP?CIFIQUES                           00146200
           MOVE LO-ID-JOB  OF LK-CP00SV09 TO LI-ID-JOB  OF LK-CP00SV02  00146300
           MOVE LO-ID-STEP OF LK-CP00SV09 TO LI-ID-STEP OF LK-CP00SV02  00146400
           .                                                            00146500
      ******************************************************************00146600
       40000-INITIALISER-PP00SV04.                                      00146700
      *---------------------------                                      00146800
      *--  INITIALISATION DE LA ZONE DE COMM. FONC. ENTRANTE DE L'APPELE00146900
           INITIALIZE LI-GR-FONC-ENT OF LK-CP00SV04                     00147000
                                                                        00147100
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00147200
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CP00SV04   00147300
                                       LK-ID-PGM-APLT  OF LK-CP00SV04   00147400
           MOVE 'PP00SV04'          TO LK-ID-PGM-APLE  OF LK-CP00SV04   00147500
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CP00SV04   00147600
           SET CP00SV04-APPEL-BATCH TO TRUE                             00147700
                                                                        00147800
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CP00SV04   00147900
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CP00SV04   00148000
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CP00SV04   00148100
                                                                        00148200
      *--  ALIMENTATION DES ZONES SP?CIFIQUES                           00148300
           MOVE LO-ID-JOB  OF LK-CP00SV09 TO LI-ID-JOB  OF LK-CP00SV04  00148400
           MOVE LO-ID-STEP OF LK-CP00SV09 TO LI-ID-STEP OF LK-CP00SV04  00148500
                                                                        00148600
           PERFORM 40000-REFRESH-DATE                                   00148700
                                                                        00148800
           MOVE WS-DT-TS                    TO LI-DT-TRT                00148900
           MOVE LO-IN-SYSOUT OF LK-CP00SV11 TO LI-IN-SYSOUT             00149000
                                                        OF LK-CP00SV04  00149100
           EVALUATE WS-CO-TYPE-LIG-CRU                                  00149200
              WHEN 1                                                    00149300
                 MOVE '0013'                             TO LI-CO-RUB   00149400
                 MOVE 'NOMBRE DE REPRISES MAX. DEPASSE!' TO LI-LB-RUB   00149500
                 MOVE WS-NB-ROLL                         TO LI-NB-RUB   00149600
                 SET CP00SV04-ENCOURS                    TO TRUE        00149700
              WHEN 2                                                    00149800
                 MOVE '0001'                             TO LI-CO-RUB   00149900
                 MOVE 'DEBUT DE TRAITEMENT'              TO LI-LB-RUB   00150000
                 MOVE SPACES                             TO LI-NB-RUB   00150100
                 SET CP00SV04-DEMARRE                    TO TRUE        00150200
              WHEN 3                                                    00150300
                 MOVE '0002'                             TO LI-CO-RUB   00150400
                 MOVE '  DATE             '              TO LI-LB-RUB   00150500
                 MOVE WS-DT-TS                           TO LI-NB-RUB   00150600
                 SET CP00SV04-DEMARRE                    TO TRUE        00150700
              WHEN 4                                                    00150800
                 MOVE '0003'                             TO LI-CO-RUB   00150900
                 MOVE '  HEURE          '                TO LI-LB-RUB   00151000
                 MOVE WS-TI-TS                           TO LI-NB-RUB   00151100
                 SET CP00SV04-DEMARRE                    TO TRUE        00151200
              WHEN 5                                                    00151300
                 MOVE '0004'                             TO LI-CO-RUB   00151400
                 MOVE '  EN DATE EXPLOITATION DU'        TO LI-LB-RUB   00151500
                 MOVE WS-DT-FONC                         TO LI-NB-RUB   00151600
                 SET CP00SV04-DEMARRE                    TO TRUE        00151700
              WHEN 6                                                    00151800
                 MOVE '0014'                             TO LI-CO-RUB   00151900
                 MOVE 'NOMBRE D''ANOMALIES DEPASSE!'     TO LI-LB-RUB   00152000
                 MOVE WS-NB-ANO-PASS                     TO LI-NB-RUB   00152100
                 SET CP00SV04-ENCOURS                    TO TRUE        00152200
              WHEN 7                                                    00152300
                 MOVE '0007'                             TO LI-CO-RUB   00152400
                 MOVE 'FIN DE TRAITEMENT'                TO LI-LB-RUB   00152500
                 MOVE SPACES                             TO LI-NB-RUB   00152600
                 SET CP00SV04-TERMINE                    TO TRUE        00152700
              WHEN 8                                                    00152800
                 MOVE '0008'                             TO LI-CO-RUB   00152900
                 MOVE '  DATE             '              TO LI-LB-RUB   00153000
                 MOVE WS-DT-TS                           TO LI-NB-RUB   00153100
                 SET CP00SV04-TERMINE                    TO TRUE        00153200
              WHEN 9                                                    00153300
                 MOVE '0009'                             TO LI-CO-RUB   00153400
                 MOVE '  HEURE            '              TO LI-LB-RUB   00153500
                 MOVE WS-TI-HHMMSS                       TO LI-NB-RUB   00153600
                 SET CP00SV04-TERMINE                    TO TRUE        00153700
              WHEN 10                                                   00153800
                 MOVE '0006'                             TO LI-CO-RUB   00153900
                 MOVE 'REPRISE EN COURS'                 TO LI-LB-RUB   00154000
                 MOVE SPACES                             TO LI-NB-RUB   00154100
                 SET CP00SV04-REPRISE                    TO TRUE        00154200
              WHEN 11                                                   00154300
                 MOVE TB-CO-RUB (WS-IX-CRU) TO LI-CO-RUB                00154400
                 MOVE TB-LB-RUB (WS-IX-CRU) TO LI-LB-RUB                00154500
                 MOVE TB-NB-RUB (WS-IX-CRU) TO LI-NB-RUB                00154600
                 SET CP00SV04-TERMINE TO TRUE                           00154700
              WHEN OTHER                                                00154800
                 MOVE '0000'                             TO LI-CO-RUB   00154900
                 MOVE 'CO-TYPE-LIG-CRU NON PREVU'        TO LI-LB-RUB   00155000
                 MOVE SPACES                             TO LI-NB-RUB   00155100
                 SET CP00SV04-TERMINE                    TO TRUE        00155200
           END-EVALUATE                                                 00155300
           .                                                            00155400

      ******************************************************************00155500
       40000-INITIALISER-PP00SV05.                                      00155600
      *---------------------------                                      00155700
      *--  INITIALISATION DE LA ZONE DE COMM. FONC. ENTRANTE DE L'APPELE00155800
           INITIALIZE LI-GR-FONC-ENT OF LK-CP00SV05                     00155900
                                                                        00156000
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00156100
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI    OF LK-CP00SV05  00156200
                                       LK-ID-PGM-APLT   OF LK-CP00SV05  00156300
           MOVE 'PP00SV05'          TO LK-ID-PGM-APLE   OF LK-CP00SV05  00156400
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT  OF LK-CP00SV05  00156500
           SET CP00SV05-APPEL-BATCH TO TRUE                             00156600
                                                                        00156700
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT   OF LK-CP00SV05  00156800
           MOVE WS-TI-TRT           TO LK-TI-TRT        OF LK-CP00SV05  00156900
           MOVE WS-ID-UTIL          TO LK-CO-UTIL       OF LK-CP00SV05  00157000
                                                                        00157100
      *--  ALIMENTATION DES ZONES SPECIFIQUES
           MOVE LO-ID-JOB  OF LK-CP00SV09 TO LI-ID-JOB  OF LK-CP00SV05  00157300
           MOVE LO-ID-STEP OF LK-CP00SV09 TO LI-ID-STEP OF LK-CP00SV05  00157400
           .                                                            00157500
      ******************************************************************00157600
       40000-ALIM-DONNEES-RESTART.
      *---------------------------                                      00157800
      *--  GESTION DES TRACES                                           00157900
           IF CP00SV21-TRACE-ACTIVE                                     00158000
              MOVE 'MB17 40000-ALIM-DONNEES-RESTART'                    00158100
              TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW                              00158300
              PERFORM 99999-ALIM-DON-SHOW                               00158300
           END-IF                                                       00158400
                                                                        00158500
      *--  ALIMENTATION DU CODE STATUT                                  00158600
           IF CODE-STATUT OR DERNIERE-OCC                               00158700
              MOVE WS-CO-STUT-STEP TO LI-CO-STATUT-STEP                 00158800
           ELSE                                                         00158900
              MOVE WS-CO-STUT-SAUV TO LI-CO-STATUT-STEP                 00159000
           END-IF                                                       00159100
      *--  ALIMENTATION DE L'IDENTIFIANT DE REPRISE                     00159200
           IF NOT IDENTIFIANTS AND NOT IDENTIFIANT-REP AND              00159300
              NOT DERNIERE-OCC                                          00159400
              MOVE WS-ID-CLE-RPRI-SAUV TO LI-ID-CLE-RPRI                00159500
                                        WS-ID-BOR-INF                   00159600
           ELSE                                                         00159700
              MOVE WS-ID-CLE-RPRI    TO LI-ID-CLE-RPRI                  00159800
                                        WS-ID-BOR-INF                   00159900
           END-IF                                                       00160000
      *--  ALIMENTATION DE L'IDENTIFIANT D'ANOMALIE                     00160100
           IF NOT IDENTIFIANTS AND NOT IDENTIFIANT-ANO AND              00160200
              NOT DERNIERE-OCC                                          00160300
              MOVE WS-ID-CLE-ANO-SAUV TO LI-ID-CLE-ANO                  00160400
           ELSE                                                         00160500
              MOVE WS-ID-CLE-ANO     TO LI-ID-CLE-ANO                   00160600
           END-IF                                                       00160700
      *--  ALIMENTATION DU CODE RETOUR                                  00160800
           IF CODE-RETOUR OR DERNIERE-OCC                               00160900
              MOVE WS-CO-RET         TO LI-CO-RET                       00161000
           ELSE                                                         00161100
              MOVE WS-CO-RET-SAUV    TO LI-CO-RET                       00161200
           END-IF                                                       00161300
      *--  ALIMENTATION DU NOMBRE D'ANOMALIE                            00161400
           IF IDENTIFIANT-ANO OR DERNIERE-OCC                           00161500
              MOVE WS-NB-ANO         TO LI-NB-ANO                       00161600
           ELSE                                                         00161700
              MOVE WS-NB-ANO-SAUV    TO LI-NB-ANO                       00161800
           END-IF                                                       00161900
      *--  ALIMENTATION DU NOMBRE D'OCCURRENCE                          00162000
           IF IDENTIFIANT-REP OR DERNIERE-OCC                           00162100
              MOVE WS-NB-OCC         TO LI-NB-OCC                       00162200
           ELSE                                                         00162300
              MOVE WS-NB-OCC-SAUV    TO LI-NB-OCC                       00162400
           END-IF                                                       00162500
      *--  ALIMENTATION DU NOMBRE DE ROLLBACK                           00162600
           IF IDENTIFIANT-ANO OR DERNIERE-OCC                           00162700
              MOVE WS-NB-ROLL        TO LI-NB-ROLL                      00162800
           ELSE                                                         00162900
              MOVE WS-NB-ROLL-SAUV   TO LI-NB-ROLL                      00163000
           END-IF                                                       00163100
      *--  RECONDUCTION DES ZONES DE TRAVAIL METIER                     00163200
      *     - COMPTE-RENDU                                              00163300
      *     - VARIABLES INTERNES                                        00163400
      *--  ALIMENTATION DU NOMBRE DE DOSSIERS TRAITES                   00163500
           IF IDENTIFIANT-REP OR DERNIERE-OCC                           00163600
              MOVE WS-GR-SAV            TO LI-LB-SAUV                   00163700
           ELSE                                                         00163800
              MOVE WS-GR-SAV-SAUV            TO LI-LB-SAUV              00163900
           END-IF                                                       00164000
           .                                                            00164100
      ******************************************************************00164200
       40000-INITIALISER-PP00SV09.                                      00164300
      *---------------------------                                      00164400
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00164500
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CP00SV09   00164600
                                       LK-ID-PGM-APLT  OF LK-CP00SV09   00164700
           MOVE 'PP00SV09'          TO LK-ID-PGM-APLE  OF LK-CP00SV09   00164800
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CP00SV09   00164900
           SET CP00SV09-APPEL-BATCH TO TRUE                             00165000
                                                                        00165100
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CP00SV09   00165200
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CP00SV09   00165300
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CP00SV09   00165400
           .                                                            00165500
      ******************************************************************00165600
       40000-INITIALISER-PP00SV10.                                      00165700
      *---------------------------                                      00165800
      *--  INITIALISATION DE LA ZONE DE COMM. FONC. ENTRANTE DE L'APPELE00165900
           INITIALIZE LI-GR-FONC-ENT OF LK-CP00SV10                     00166000
                                                                        00166100
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00166200
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CP00SV10   00166300
                                       LK-ID-PGM-APLT  OF LK-CP00SV10   00166400
           MOVE 'PP00SV10'          TO LK-ID-PGM-APLE  OF LK-CP00SV10   00166500
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CP00SV10   00166600
           SET CP00SV10-APPEL-BATCH TO TRUE                             00166700
                                                                        00166800
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CP00SV10   00166900
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CP00SV10   00167000
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CP00SV10   00167100
                                                                        00167200
      *--  ALIMENTATION DES ZONES SP?CIFIQUES                           00167300
           MOVE LO-ID-JOB  OF LK-CP00SV09 TO LI-ID-JOB  OF LK-CP00SV10  00167400
           MOVE LO-ID-STEP OF LK-CP00SV09 TO LI-ID-STEP OF LK-CP00SV10  00167500
           PERFORM 40000-REFRESH-DATE                                   00167600
           MOVE WS-TI-HHMMSS              TO LI-TS-DEB  OF LK-CP00SV10  00167700
           .                                                            00167800
      ******************************************************************00167900
       40000-INITIALISER-PP00SV11.                                      00168000
      *---------------------------                                      00168100
      *--  INITIALISATION DE LA ZONE DE COMM. FONC. ENTRANTE DE L'APPELE00168200
           INITIALIZE LI-GR-FONC-ENT OF LK-CP00SV11                     00168300
                                                                        00168400
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00168500
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CP00SV11   00168600
                                       LK-ID-PGM-APLT  OF LK-CP00SV11   00168700
           MOVE 'PP00SV11'          TO LK-ID-PGM-APLE  OF LK-CP00SV11   00168800
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CP00SV11   00168900
           SET CP00SV11-APPEL-BATCH TO TRUE                             00169000
                                                                        00169100
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CP00SV11   00169200
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CP00SV11   00169300
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CP00SV11   00169400
                                                                        00169500
      *--  ALIMENTATION DES ZONES SP?CIFIQUES                           00169600
           MOVE LO-ID-JOB  OF LK-CP00SV09 TO LI-ID-JOB  OF LK-CP00SV11  00169700
           MOVE LO-ID-STEP OF LK-CP00SV09 TO LI-ID-STEP OF LK-CP00SV11  00169800
           .                                                            00169900
      ******************************************************************00170000
       40000-INITIALISER-PP00SV12.
      *---------------------------                                      00170200
      *--  INITIALISATION DE LA ZONE DE COMM. FONC. ENTRANTE DE L'APPELE00170300
           INITIALIZE LI-GR-FONC-ENT OF LK-CP00SV12                     00170400
                                                                        00170500
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00170600
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CP00SV12   00170700
                                       LK-ID-PGM-APLT  OF LK-CP00SV12   00170800
           MOVE 'PP00SV12'          TO LK-ID-PGM-APLE  OF LK-CP00SV12   00170900
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CP00SV12   00171000
           SET CP00SV12-APPEL-BATCH TO TRUE                             00171100
                                                                        00171200
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CP00SV12   00171300
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CP00SV12   00171400
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CP00SV12   00171500
                                                                        00171600
      *--  ALIMENTATION DES ZONES SP?CIFIQUES                           00171700
           MOVE LO-ID-JOB  OF LK-CP00SV09 TO LI-ID-JOB  OF LK-CP00SV12  00171800
           MOVE LO-ID-STEP OF LK-CP00SV09 TO LI-ID-STEP OF LK-CP00SV12  00171900
           .                                                            00172000
      ******************************************************************00172100
       40000-INITIALISER-PP00SV25.                                      00172200
      *---------------------------                                      00172300
      *--  INITIALISATION DE LA ZONE DE COMM. FONC. ENTRANTE DE L'APPELE00172400
           INITIALIZE LI-GR-FONC-ENT OF LK-CP00SV25                     00172500
                                                                        00172600
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00172700
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CP00SV25   00172800
                                       LK-ID-PGM-APLT  OF LK-CP00SV25   00172900
           MOVE 'PP00SV25'          TO LK-ID-PGM-APLE  OF LK-CP00SV25   00173000
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CP00SV25   00173100
           SET CP00SV25-APPEL-BATCH TO TRUE                             00173200
                                                                        00173300
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CP00SV25   00173400
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CP00SV25   00173500
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CP00SV25   00173600
           .                                                            00173700
      ******************************************************************00173800
       40000-INITIALISER-PP00SVCA.                                      00173900
      *---------------------------                                      00174000
      *--  INITIALISATION DE LA ZONE DE COMM. FONC. ENTRANTE DE L'APPELE00174100
           INITIALIZE LI-GR-FONC-ENT OF LK-CP00SVCA                     00174200
                                                                        00174300
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00174400
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CP00SVCA   00174500
                                       LK-ID-PGM-APLT  OF LK-CP00SVCA   00174600
           MOVE 'PP00SVCA'          TO LK-ID-PGM-APLE  OF LK-CP00SVCA   00174700
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CP00SVCA   00174800
           SET CP00SVCA-APPEL-BATCH TO TRUE                             00174900
                                                                        00175000
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CP00SVCA   00175100
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CP00SVCA   00175200
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CP00SVCA   00175300
                                                                        00175400
      *--  ALIMENTATION DES ZONES SP?CIFIQUES                           00175500
           SET CP00SVCA-FONC-DEM-ENREG TO TRUE                          00175600
           .                                                            00176100
                                                                        00264400
      *--------------------------*                                      00264500
       40000-INITIALISER-PP00SV31.                                      00176300
      *--------------------------*                                      00264700
      *--  INITIALISATION DE LA ZONE DE COMM. FONC. ENTRANTE DE L'APPELE00176500
           INITIALIZE LI-GR-FONC-ENT OF LK-CP00SV31                     00176600
                                                                        00176700
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00176800
           MOVE LO-ID-JOB  OF LK-CP00SV09 TO LK-ID-JOB  OF LK-CP00SV31  00118600
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CP00SV31   00176900
                                       LK-ID-PGM-APLT  OF LK-CP00SV31   00177000
           MOVE 'PP00SV31'          TO LK-ID-PGM-APLE  OF LK-CP00SV31   00177100
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CP00SV31   00177200
           SET CP00SV31-APPEL-BATCH TO TRUE                             00177300
                                                                        00177400
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CP00SV31   00177500
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CP00SV31   00177600
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CP00SV31   00177700
                                                                        00177800
      **-  ALIMENTATION DES ZONES SP?CIFIQUES                           00177900
           MOVE LK-ID-PGM-MAI       OF LK-CP00SV31                      00178000
             TO LI-ID-PGM-MAI       OF LK-CP00SV31                      00178100
           MOVE LK-ID-PGM-APEL-ERR  OF WS-GR-ERR                        00266500
             TO LI-ID-PGM-APEL-ERR  OF LK-CP00SV31                      00178300
           MOVE LK-ID-PGM-ERR       OF WS-GR-ERR                        00266700
             TO LI-ID-PGM-ERR       OF LK-CP00SV31                      00178500
           MOVE LK-CO-RET-1ER-INI   OF WS-GR-ERR                        00266900
             TO LI-CO-RET-1ER-INI   OF LK-CP00SV31                      00178700
           MOVE LK-CO-RET-1ER-MOD   OF WS-GR-ERR                        00267100
             TO LI-CO-RET-1ER-MOD   OF LK-CP00SV31                      00178900
           MOVE LK-CO-RET-2ND-INI   OF WS-GR-ERR                        00267300
             TO LI-CO-RET-2ND-INI   OF LK-CP00SV31                      00179100
           MOVE LK-CO-RET-2ND-MOD   OF WS-GR-ERR                        00267500
             TO LI-CO-RET-2ND-MOD   OF LK-CP00SV31                      00179300
           MOVE LK-CO-MSG-INI       OF WS-GR-ERR                        00267700
             TO LI-CO-MSG-INI       OF LK-CP00SV31                      00179500
           MOVE LK-CO-MSG-MOD       OF WS-GR-ERR                        00267900
             TO LI-CO-MSG-MOD       OF LK-CP00SV31                      00179700
           MOVE LK-LB-MSG-LNG-INI   OF WS-GR-ERR                        00268100
             TO LI-LB-MSG-LNG-INI   OF LK-CP00SV31                      00179900
           MOVE LK-LB-MSG-LNG-MOD   OF WS-GR-ERR                        00268300
             TO LI-LB-MSG-LNG-MOD   OF LK-CP00SV31                      00180100
           MOVE LK-LB-MSG-CRT-INI   OF WS-GR-ERR                        00268500
             TO LI-LB-MSG-CRT-INI   OF LK-CP00SV31                      00180300
           MOVE LK-LB-MSG-CRT-MOD   OF WS-GR-ERR                        00268700
             TO LI-LB-MSG-CRT-MOD   OF LK-CP00SV31                      00180500
           MOVE LK-CO-CRTQ-INI      OF WS-GR-ERR                        00268900
             TO LI-CO-CRTQ-INI      OF LK-CP00SV31                      00180700
           MOVE LK-CO-CRTQ-MOD      OF WS-GR-ERR                        00269100
             TO LI-CO-CRTQ-MOD      OF LK-CP00SV31                      00180900
           MOVE LK-ID-ERR           OF WS-GR-ERR                        00269300
             TO LI-ID-ERR           OF LK-CP00SV31                      00181100
           MOVE LK-LB-SQLCA         OF WS-GR-ERR                        00269500
             TO LI-LB-SQLCA         OF LK-CP00SV31                      00181300
           MOVE LK-CO-SQL           OF WS-GR-ERR                        00269700
             TO LI-CO-SQL           OF LK-CP00SV31                      00181500
           MOVE LK-LB-ACT-SQL       OF WS-GR-ERR                        00269900
             TO LI-LB-ACT-SQL       OF LK-CP00SV31                      00181700
           MOVE LK-LB-OBJ-DB2       OF WS-GR-ERR                        00270100
             TO LI-LB-OBJ-DB2       OF LK-CP00SV31                      00181900
           MOVE LK-NO-ORD-REQ       OF WS-GR-ERR                        00270300
             TO LI-NO-ORD-REQ       OF LK-CP00SV31                      00182100
           MOVE LK-LB-CRIT-REQ      OF WS-GR-ERR                        00270500
             TO LI-LB-CRIT-REQ      OF LK-CP00SV31                      00182300

      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              DISPLAY '=======INIT PP00SV31 =CHECK'
              DISPLAY 'LI-ID-PGM-MAI:' LI-ID-PGM-MAI OF LK-CP00SV31
              DISPLAY 'LI-ID-PGM-APEL-ERR:' LI-ID-PGM-APEL-ERR
              OF LK-CP00SV31
              DISPLAY 'LI-ID-PGM-ERR    :' LI-ID-PGM-ERR
              OF LK-CP00SV31
              DISPLAY 'LI-CO-RET-1ER-MOD:'  LI-CO-RET-1ER-MOD
              OF LK-CP00SV31
              DISPLAY 'LI-CO-RET-2ND-MOD:'  LI-CO-RET-2ND-MOD
              OF LK-CP00SV31
           END-IF
           .                                                            00182400
      *--------------------------*                                      00270800
       40000-INITIALISER-PP00SV30.
      *--------------------------*
      *--  INITIALISATION DE LA ZONE DE COMM. DE L'APPELE
           INITIALIZE LK-CP00SV30
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI    OF LK-CP00SV30
                                       LK-ID-PGM-APLT   OF LK-CP00SV30
           MOVE 'PP00SV30'          TO LK-ID-PGM-APLE   OF LK-CP00SV30
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT  OF LK-CP00SV30
           SET CP00SV30-APPEL-BATCH TO TRUE

           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT   OF LK-CP00SV30
           MOVE WS-TI-TRT           TO LK-TI-TRT        OF LK-CP00SV30
           MOVE WS-ID-UTIL          TO LK-CO-UTIL       OF LK-CP00SV30

      *-- ALIMENTATION DU CODE METHODE
           MOVE  2            TO LI-CO-FONC      OF LK-CP00SV30
      *
      *--  ALIMENTATION DES ZONES SPECIFIQUES
           MOVE  'TPSPTRC'           TO LI-LB-NM-ENTI OF LK-CP00SV30
           MOVE  'ID_TECH_TRAC'      TO LI-LB-NM-COL  OF LK-CP00SV30
           .

      ******************************************************************
       40000-INITIALISER-PPSPAM01.
      *---------------------------
      *-  INITIALISATION DE LA ZONE DE COMM. DE L'APPELE
           INITIALIZE LK-CPSPAM01

           MOVE LO-VA-CHRO   OF LK-CP00SV30
             TO LI-TRC-ID-TECH-TRAC  OF LK-CPSPAM01

      *-  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI    OF LK-CPSPAM01
                                       LK-ID-PGM-APLT   OF LK-CPSPAM01
           MOVE 'PPSPAM01'          TO LK-ID-PGM-APLE   OF LK-CPSPAM01
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT  OF LK-CPSPAM01
           SET CPSPAM01-APPEL-BATCH TO TRUE

           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT   OF LK-CPSPAM01
           MOVE WS-TI-TRT           TO LK-TI-TRT        OF LK-CPSPAM01
           MOVE WS-ID-UTIL          TO LK-CO-UTIL       OF LK-CPSPAM01

      *-  ALIMENTATION DU NUMERO D'ORDRE
           MOVE WS-NO-REQ-ACC TO LK-NO-REQ-ACC      OF LK-CPSPAM01

      *-  ALIMENTATION DES ZONES SPECIFIQUES
      *--TODOROKI
           MOVE WS-CO-ACTE-GESTION
             TO LI-TRC-CO-TYPE-ACT-GEST

           MOVE SPACE TO LI-TRC-CO-CTXT-ACT-GEST OF LK-CPSPAM01
           MOVE -1    TO LI-TRC-CO-CTXT-ACT-GEST-NULL OF LK-CPSPAM01

           MOVE 'PRE' TO LI-TRC-CO-ORIG-ACT-GEST OF LK-CPSPAM01

?          MOVE SPACE      TO LI-TRC-LB-VA-REF-TRAC OF LK-CPSPAM01


           MOVE WS-DT-FONC
             TO LI-TRC-DT-DEFF-ACT OF LK-CPSPAM01

           MOVE 'PPGEMB17' TO LI-TRC-ID-APPL OF LK-CPSPAM01
           MOVE 'PPGEMB17' TO LI-TRC-ID-FONC OF LK-CPSPAM01
           MOVE 0          TO LI-TRC-CO-ANO-TRAC-NULL OF LK-CPSPAM01
           MOVE 0          TO LI-TRC-CO-ANO-TRAC OF LK-CPSPAM01

           IF WS-CO-CONTXT-TRT-MB17 = 'M'
             MOVE SPACE  TO LI-TRC-LB-INFO-CPLM OF LK-CPSPAM01
             MOVE -1
               TO LI-TRC-LB-INFO-CPLM-NULL OF LK-CPSPAM01

             MOVE  0  TO LI-TRC-ID-EG OF LK-CPSPAM01
             MOVE -1  TO LI-TRC-ID-EG-NULL OF LK-CPSPAM01
           END-IF

           IF WS-CO-CONTXT-TRT-MB17 = 'P'
             MOVE WS-ID-TECH-INDV-EDF-PREC
               TO  LI-TRC-ID-EG OF LK-CPSPAM01
             MOVE 0  TO LI-TRC-ID-EG-NULL OF LK-CPSPAM01

             STRING WS-CO-REG-EDF-PREC ' ' WS-NO-INST-DECL-EDF-PREC
             DELIMITED BY SIZE
             INTO LI-TRC-LB-INFO-CPLM OF LK-CPSPAM01

             MOVE 0    TO LI-TRC-LB-INFO-CPLM-NULL OF LK-CPSPAM01
           END-IF
      *    TYPE OBJET DE GESTION  = IND , VAR ???

      *    MOVE LI-CO-NIV-TRAC TO LI-TRC-IN-NIV-TRAC
      *    MOVE LI-IN-ACT-MAJ TO LI-TRC-IN-ACT-MAJ

           MOVE  0  TO LI-TRC-ID-TECH-UTIL OF LK-CPSPAM01
           MOVE 'N' TO LI-TRC-IN-TRAC-TP OF LK-CPSPAM01

      *    PERFORM 40000-INITIALISER-PPSPAM01-2
           .

      *--------------------------*
       40000-INITIALISER-PP00SVP2.                                      00182600
      *---------------------------                                      00182700
      *--  INITIALISATION DE LA ZONE DE COMM. FONC. ENTRANTE DE L'APPELE00182800
           INITIALIZE LI-GR-FONC-ENT OF LK-CP00SVP2                     00182900
                                                                        00183000
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00183100
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CP00SVP2   00183200
                                       LK-ID-PGM-APLT  OF LK-CP00SVP2   00183300
           MOVE 'PP00SVP2'          TO LK-ID-PGM-APLE  OF LK-CP00SVP2   00183400
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CP00SVP2   00183500
           SET CP00SVP2-APPEL-BATCH TO TRUE                             00183600
                                                                        00183700
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CP00SVP2   00183800
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CP00SVP2   00183900
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CP00SVP2   00184000
                                                                        00184100
      *--  ALIMENTATION DES ZONES SP?CIFIQUES                           00184200
           SET CP00SVP2-DT-JOUR   TO  TRUE                              00184300
           SET CP00SVP2-TS-OUI    TO TRUE                               00184400
           .                                                            00184500
      *--------------------------*                                      00272900
       40000-INITIALISER-PPGEAL20.
      *---------------------------                                      00184800
      *--  INITIALISATION DE LA ZONE DE COMM. FONC. ENTRANTE DE L'APPELE00184900
      *    INITIALIZE LI-GR-FONC-ENT OF LK-CPGEAL20                     00273300
M002       INITIALIZE LK-CPGEAL20                                       00273300
                                                                        00185100
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00185200
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CPGEAL20   00273600
                                       LK-ID-PGM-APLT  OF LK-CPGEAL20   00273700
           MOVE 'PPGEAL20'          TO LK-ID-PGM-APLE  OF LK-CPGEAL20   00273800
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CPGEAL20   00273900
           SET CPGEAL20-APPEL-BATCH TO TRUE                             00274000
                                                                        00185800
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CPGEAL20   00274200
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CPGEAL20   00274300
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CPGEAL20   00274400
      *--  ALIMENTATION DU NUMERO D'ORDRE                               00186200
           .                                                            00274600
                                                                        00186400
M019+ *---------------------------
M019+  40000-INITIALISER-PPGAAL01.
M019+ *---------------------------
M019+ *--  GESTION DES TRACES
M019+      IF CP00SV21-TRACE-ACTIVE
M019+         MOVE 'MB17-40000-INITIALISER-PPGAAL01'
M019+                                             TO LI-ID-DON-TRACE
M019+         SET CP00SV21-NIV-PAR                TO TRUE
M019+         PERFORM 80000-ALIM-TRACE
M019+      END-IF
M019+ *--
M019+ *--  INITIALISATION DE LA ZONE DE COMM. DE L'APPELE
M019+      INITIALIZE LK-CPGAAL01
M019+ *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00185200
M019+      MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CPGAAL01   00273600
M019+                                  LK-ID-PGM-APLT  OF LK-CPGAAL01   00273700
M019+      MOVE 'PPGAAL01'          TO LK-ID-PGM-APLE  OF LK-CPGAAL01   00273800
M019+      MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CPGAAL01   00273900
M019+      SET CPGAAL01-APPEL-BATCH TO TRUE                             00274000
M019+                                                                   00185800
M019+      MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CPGAAL01   00274200
M019+      MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CPGAAL01   00274300
M019+      MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CPGAAL01   00274400
M019+ *--
M019+ *--  ALIMENTATION DU NUMERO D'ORDRE
M019+      MOVE WS-NO-REQ-ACC TO LK-NO-REQ-ACC   OF LK-CPGAAL01
M019+      .
      *--------------------------*
       40000-INITIALISER-PPGAAL18.
      *---------------------------
      *--  INITIALISATION DE LA ZONE DE COMM. FONC. ENTRANTE DE L'APPELE
           INITIALIZE LI-GR-FONC-ENT OF LK-CPGAAL18

      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CPGAAL18
                                       LK-ID-PGM-APLT  OF LK-CPGAAL18
           MOVE 'PPGAAL18'          TO LK-ID-PGM-APLE  OF LK-CPGAAL18
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CPGAAL18
           SET CPGAAL18-APPEL-BATCH TO TRUE

           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CPGAAL18
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CPGAAL18
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CPGAAL18
      *--  ALIMENTATION DU NUMERO D'ORDRE
           .

      ******************************************************************
       40000-INITIALISER-PPSPSV14.
      *---------------------------
      *--  INITIALISATION DE LA ZONE DE COMM. DE L'APPELE
           INITIALIZE LK-CPSP14M1

      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CPSP14M1
                                       LK-ID-PGM-APLT  OF LK-CPSP14M1
           MOVE 'PPSPSV14'          TO LK-ID-PGM-APLE  OF LK-CPSP14M1
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CPSP14M1
           SET CPSP14M1-APPEL-BATCH TO TRUE
                                                                        00186400
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CPSP14M1
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CPSP14M1
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CPSP14M1
                                                                        00274800
      *    MOVE LK-GR-TECH-GEN-ENT OF LK-CPGEMB17
      *                       TO LK-GR-TECH-GEN-ENT OF LK-CPSP14M1
      *--  ALIMENTATION DES PROGRAMMES APPELANT ET APPELE
           MOVE 'PPGEMB17'    TO LK-ID-PGM-APLT     OF LK-CPSP14M1
           MOVE 'PPSPSV14'    TO LK-ID-PGM-APLE     OF LK-CPSP14M1
      *
      *--  ALIMENTATION DES ZONES SPECIFIQUES                           00171700
      *
           MOVE LO-ID-JOB  OF LK-CP00SV09 TO LK-ID-JOB  OF LK-CPSP14M1  00171800
           MOVE LO-ID-STEP OF LK-CP00SV09 TO LK-ID-STEP OF LK-CPSP14M1  00171900
           SET CPSP14M1-SELECT TO TRUE
      *
           .                                                            00278600
                                                                        00278700
                                                                        00279700
      ******************************************************************00189500
      *--  ALIMENTATION DES ZONES METIER D'ENTREE SPECIFIQUE PAR METHODE00289300
      ******************************************************************00289400
       40000-INITIALISER-PPSP07I1.                                      00289500
      *---------------------------                                      00289600
      *--  GESTION DES TRACES                                           00289700
           IF CP00SV21-TRACE-ACTIVE                                     00289800
              MOVE '40000-INITIALISER-PPSP07I1' TO LI-ID-DON-TRACE      00289900
              SET CP00SV21-NIV-PAR TO TRUE                              00290000
              PERFORM 80000-ALIM-TRACE                                  00290100
           END-IF                                                       00290200
                                                                        00290300
      *--  INITIALISATION DE LA ZONE DE COMM. DE L'APPELE               00290400
           INITIALIZE LK-CPSP07I1                                       00290500
                                                                        00290600
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00290700
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CPSP07I1   00290800
                                       LK-ID-PGM-APLT  OF LK-CPSP07I1   00290900
           MOVE 'PPSPSV07'          TO LK-ID-PGM-APLE  OF LK-CPSP07I1   00291000
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CPSP07I1   00291100
           SET CPSP07I1-APPEL-BATCH TO TRUE                             00291200
                                                                        00291300
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CPSP07I1   00291400
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CPSP07I1   00291500
                                                                        00291600
           MOVE WS-ID-UTIL          TO LK-CO-UTIL       OF LK-CPSP07I1  00291700
           MOVE LO-ID-JOB  OF LK-CP00SV09 TO LK-ID-JOB  OF LK-CPSP07I1  00291800
           MOVE LO-ID-STEP OF LK-CP00SV09 TO LK-ID-STEP OF LK-CPSP07I1  00291900
      *--  ALIMENTATION DU CODE METHODE                                 00292000
           MOVE 'I1'                TO LI-CO-METH       OF LK-CPSP07I1  00292100
           MOVE 1                   TO LI-NB-OCC-DMD    OF LK-CPSP07I1  00292200
           .                                                            00292300

      *--------------------------*
       40000-INITIALISER-PPGEAL21.
      *---------------------------
      *--  INITIALISATION DE LA ZONE DE COMM. FONC. ENTRANTE DE L'APPELE
      *    INITIALIZE LI-GR-FONC-ENT OF LK-CPGEAL21
M002       INITIALIZE LK-CPGEAL21

      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CPGEAL21
                                       LK-ID-PGM-APLT  OF LK-CPGEAL21
           MOVE 'PPGEAL21'          TO LK-ID-PGM-APLE  OF LK-CPGEAL21
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CPGEAL21
           SET CPGEAL21-APPEL-BATCH TO TRUE

           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CPGEAL21
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CPGEAL21
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CPGEAL21
      *--  ALIMENTATION DU NUMERO D'ORDRE
           MOVE 2 TO LK-NO-REQ-ACC OF LK-CPGEAL21
           .


      *--------------------------*
       40000-INITIALISER-PPGEAM20.
      *--------------------------*
           IF CP00SV21-TRACE-ACTIVE
              MOVE '40000-INITIALISER-PPGEAM20' TO LI-ID-DON-TRACE
              SET CP00SV21-NIV-PAR TO TRUE
              PERFORM 80000-ALIM-TRACE
           END-IF

           INITIALIZE LK-CPGEAM20

      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CPGEAM20   00273600
                                       LK-ID-PGM-APLT  OF LK-CPGEAM20   00273700
           MOVE 'PPGEAM20'          TO LK-ID-PGM-APLE  OF LK-CPGEAM20   00273800
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CPGEAM20   00273900
           SET CPGEAM20-APPEL-BATCH TO TRUE                             00274000
                                                                        00185800
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CPGEAM20   00274200
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CPGEAM20   00274300
           MOVE WS-ID-UTIL              TO LK-CO-UTIL  OF LK-CPGEAM20
           .

      *---------------------------
       40000-INITIALISER-PPSPSV12.
      *---------------------------
      *--  INITIALISATION DE LA ZONE DE COMM. DE L'APPELE
           INITIALIZE LK-CPSP12C1

      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CPSP12C1
                                       LK-ID-PGM-APLT  OF LK-CPSP12C1
           MOVE 'PPSPSV12'          TO LK-ID-PGM-APLE  OF LK-CPSP12C1
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CPSP12C1
           SET CPSP12C1-APPEL-BATCH TO TRUE

           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CPSP12C1
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CPSP12C1
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CPSP12C1

      *--  ALIMENTATION DU CODE METHODE
           MOVE 'C1'                TO LI-CO-METH      OF LK-CPSP12C1

      *--  ALIMENTATION DES ZONES SPECIFIQUES CF 41000-ALIM-VAR-PPS.
      *    TRANSFERT DES DIVERS BLOCS EDITIQUES
      *    MOVE WS-TB-BLOC-EDIT
      *      TO LI-TB-BLOC-EDIT         OF LK-CPSP12C1
           .

      *-----------------------*
       41000-ALIM-FIXE-PPSPSV12.
      *-----------------------*
           IF CP00SV21-TRACE-ACTIVE                                     00190100
              MOVE '*MB17 41000-ALIM-FIXE-PPSPSV12'  TO LI-ID-DON-TRACE 00190200
              SET       CP00SV21-NIV-PAR    TO TRUE                     00190300
              PERFORM   80000-ALIM-TRACE                                00190400
           END-IF                                                       00190500
                                                                        00190600
           PERFORM 40000-INITIALISER-PPSPSV12

      *--TODOROKI:  CF PAGE 144-144 SFD
      *--  OBJET COURIER = ALLOCATAIRE

      *--  NOM DU FLUX EDITIQUE SUR 10
           MOVE WS-ID-TECH-FLX-EDIT (4:10)
             TO LI-ID-MODL          OF LK-CPSP12C1

           EVALUATE WS-CO-CONTXT-TRT-MB17
              WHEN 'P'
                PERFORM 41000-ALIM-FIXE-PPSPSV12-P
              WHEN 'M'
                PERFORM 41000-ALIM-FIXE-PPSPSV12-M
              WHEN OTHER
                     display '41000-alim-fixe-sv12 co-contxt KO'
                PERFORM 31000-ABANDON
           END-EVALUATE

           EVALUATE TRUE
               WHEN FLX-EDIT-PGEINFOFISCAL
                  MOVE 'EMISIFPONC'
                   TO LI-CO-TYPE-ACT-GEST
                   OF LI-GR-FONC-SPE-ENT OF LK-CPSP12C1
               WHEN FLX-EDIT-PGEDECFISCPCT
                  MOVE 'EMISSATFIP'
                   TO LI-CO-TYPE-ACT-GEST
                   OF LI-GR-FONC-SPE-ENT OF LK-CPSP12C1
               WHEN FLX-EDIT-PGEDECFISCMAS
                  MOVE 'EMISSATFIA'
                   TO LI-CO-TYPE-ACT-GEST
                   OF LI-GR-FONC-SPE-ENT OF LK-CPSP12C1
              WHEN OTHER
                     display '41000-alim-fixe-sv12 info flux KO'
                PERFORM 31000-ABANDON
           END-EVALUATE

           MOVE 'PPGEMB17'
             TO LI-ID-APPL-APEL         OF LK-CPSP12C1
                LI-ID-FONC-APEL         OF LK-CPSP12C1
           MOVE 'U'
             TO LI-CO-MODE-APP          OF LK-CPSP12C1
M005       MOVE 'PGE'
M005         TO LI-CO-QART-HABI         OF LK-CPSP12C1
           .

      *--------------------------*
       41000-ALIM-FIXE-PPSPSV12-M.
      *--------------------------*
           IF CP00SV21-TRACE-ACTIVE                                     00190100
              MOVE '==>MB17 41000-ALIM-FIXE-PPSPSV12-M'                 00190200
                TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE                     00190300
              PERFORM   80000-ALIM-TRACE                                00190400
           END-IF                                                       00190500
                                                                        00190600
           MOVE LI-EDF-ID-TECH-INDV OF WS-GR-EDF
             TO LI-ID-TECH-IND      OF LK-CPSP12C1
?          INSPECT LI-ID-TECH-IND OF LK-CPSP12C1
?          REPLACING ALL ' ' BY '0'

      *--  EVOL ? TEMP:ATTRIBUTION DU 1 NUMERO DE DPR
           MOVE LI-LIF-ID-TECH-DPR  OF WS-GR-EDF
             TO LI-ID-TECH-DPR      OF LK-CPSP12C1
           INSPECT LI-ID-TECH-DPR      OF LK-CPSP12C1
           REPLACING ALL ' ' BY '0'

      *--  TYPE DESTINATAIRE 'IN' TPGAXDE
           MOVE LI-EDF-CO-TYPE-OBJ-DEST  OF WS-GR-EDF
             TO LI-CO-TYPE-OBJ-GEST OF LK-CPSP12C1
      *         LI-CO-TYPE-DEST     OF LK-CPSP12C1

      *--  TYPE OBJET DE GESTION = 'ALL' TPGAXOD
           EVALUATE LI-EDF-CO-TYPE-OBJ-DEST OF WS-GR-EDF
M001- *       WHEN 'IN '
M001          WHEN 'IND'
                 IF LI-EDF-IN-DC OF WS-GR-EDF = 'N' OR SPACE
                   MOVE 'ALL'
                     TO LI-CO-TYPE-DEST     OF LK-CPSP12C1
      *              TO LI-CO-TYPE-OBJ-GEST OF LK-CPSP12C1
                 ELSE
                   MOVE 'TDD'
                     TO LI-CO-TYPE-DEST     OF LK-CPSP12C1
                 END-IF
                 MOVE 'IND'
                   TO LI-CO-TYPE-OBJ-GEST OF LK-CPSP12C1
              WHEN 'TIE'
                 MOVE 'TMA'
                   TO LI-CO-TYPE-DEST     OF LK-CPSP12C1
              WHEN OTHER
                 MOVE 'XXX'
                   TO LI-CO-TYPE-DEST     OF LK-CPSP12C1
           END-EVALUATE

           MOVE LI-EDF-ID-TECH-OBJ-DEST OF WS-GR-EDF
             TO LI-ID-TECH-OBJ-GEST OF LK-CPSP12C1
?          INSPECT LI-ID-TECH-OBJ-GEST OF LK-CPSP12C1
?          REPLACING ALL ' ' BY '0'

           MOVE LI-EDF-CO-REG-DECL OF WS-GR-EDF
             TO LI-CO-REG           OF LK-CPSP12C1
           MOVE LI-EDF-NO-INST-DECL OF WS-GR-EDF
             TO LI-NO-INST          OF LK-CPSP12C1

           MOVE LI-EDF-CO-TYPE-OBJ-DEST OF WS-GR-EDF
              TO LI-CO-TYPE-OBJ-GEST-ADR OF LK-CPSP12C1
M001- *    IF LI-CO-TYPE-OBJ-GEST-ADR OF LK-CPSP12C1 = 'IN '
M001- *       MOVE 'IND' TO LI-CO-TYPE-OBJ-GEST-ADR OF LK-CPSP12C1
M001- *    END-IF

           MOVE LI-EDF-ID-TECH-AD-DEST OF WS-GR-EDF
              TO LI-ID-TECH-OBJ-GEST-ADR OF LK-CPSP12C1
?          INSPECT LI-ID-TECH-OBJ-GEST-ADR OF LK-CPSP12C1
?          REPLACING ALL ' ' BY '0'
           .

      *--------------------------*
       41000-ALIM-FIXE-PPSPSV12-P.
      *--------------------------*
           IF CP00SV21-TRACE-ACTIVE                                     00190100
              MOVE '==>MB17 41000-ALIM-FIXE-PPSPSV12-P'                 00190200
                TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE                     00190300
              PERFORM   80000-ALIM-TRACE                                00190400
           END-IF                                                       00190500
                                                                        00190600
           MOVE LO-EDF-ID-TECH-INDV  OF LK-CPGEAL20
             TO LI-ID-TECH-IND       OF LK-CPSP12C1

           INSPECT LI-ID-TECH-IND OF LK-CPSP12C1
           REPLACING ALL ' ' BY '0'

      *--  EVOL ? TEMP:ATTRIBUTION DU 1 NUMERO DE DPR
           MOVE LO-LIF-ID-TECH-DPR   OF LK-CPGEAL21
             TO LI-ID-TECH-DPR       OF LK-CPSP12C1
           INSPECT LI-ID-TECH-DPR      OF LK-CPSP12C1
           REPLACING ALL ' ' BY '0'

      *--  TYPE DESTINATAIRE
           MOVE LO-EDF-CO-TYPE-OBJ-DEST     OF LK-CPGEAL20
             TO LI-CO-TYPE-OBJ-GEST OF LK-CPSP12C1
M002  *      TO LI-CO-TYPE-DEST      OF LK-CPSP12C1

      *--  TYPE OBJET DE GESTION = 'ALL'
           EVALUATE LO-EDF-CO-TYPE-OBJ-DEST OF LK-CPGEAL20
M001- *       WHEN 'IN '
M001          WHEN 'IND'
                 IF LO-EDF-IN-DC OF LK-CPGEAL20 = 'N' OR SPACE
                     MOVE 'ALL'
M002                 TO LI-CO-TYPE-DEST     OF LK-CPSP12C1
                 ELSE
                     MOVE 'TDD'
M002                 TO LI-CO-TYPE-DEST     OF LK-CPSP12C1
                 END-IF
                 MOVE 'IND'
                   TO LI-CO-TYPE-OBJ-GEST OF LK-CPSP12C1
              WHEN 'TIE'
                 MOVE 'TMA'
M002                 TO LI-CO-TYPE-DEST     OF LK-CPSP12C1
              WHEN OTHER
                 MOVE 'YYY'
M002                 TO LI-CO-TYPE-DEST     OF LK-CPSP12C1
           END-EVALUATE

           MOVE LO-EDF-ID-TECH-OBJ-DEST     OF LK-CPGEAL20
             TO LI-ID-TECH-OBJ-GEST OF LK-CPSP12C1
           INSPECT LI-ID-TECH-OBJ-GEST OF LK-CPSP12C1
           REPLACING ALL ' ' BY '0'

           MOVE LO-EDF-CO-REG-DECL  OF LK-CPGEAL20
             TO LI-CO-REG           OF LK-CPSP12C1
           MOVE LO-EDF-NO-INST-DECL OF LK-CPGEAL20
             TO LI-NO-INST          OF LK-CPSP12C1

           MOVE LO-EDF-CO-TYPE-OBJ-DEST     OF LK-CPGEAL20
              TO LI-CO-TYPE-OBJ-GEST-ADR OF LK-CPSP12C1
M001- *    IF LO-EDF-CO-TYPE-OBJ-DEST     OF LK-CPGEAL20 = 'IN '
M001- *       MOVE 'IND'  TO LO-EDF-CO-TYPE-OBJ-DEST  OF LK-CPGEAL20
M001- *    END-IF

           MOVE LO-EDF-ID-TECH-AD-DEST      OF LK-CPGEAL20
              TO LI-ID-TECH-OBJ-GEST-ADR OF LK-CPSP12C1
?          INSPECT LI-ID-TECH-OBJ-GEST-ADR OF LK-CPSP12C1
?          REPLACING ALL ' ' BY '0'
           .

                                                                        00292400
      ******************************************************************00189800
       40000-REFRESH-DATE.                                              00189900
      *-------------------                                              00190000
           IF CP00SV21-TRACE-ACTIVE                                     00190100
              MOVE    '40000-REFRESH-DATE ' TO LI-ID-DON-TRACE          00190200
              SET       CP00SV21-NIV-PAR    TO TRUE                     00190300
              PERFORM   80000-ALIM-TRACE                                00190400
           END-IF                                                       00190500
                                                                        00190600
           PERFORM 40000-INITIALISER-PP00SVP2                           00190700
           PERFORM 80000-APPEL-PP00SVP2                                 00190800
                                                                        00190900
           MOVE LO-TS           TO    WS-TS-DATE                        00293700
                                                                        00191100
           MOVE WS-TI-TS        TO    WS-TI-HHMMSS                      00191200
           .                                                            00191300
      ******************************************************************00191400
      * INITIALISATION DES PARAMETRES D'APPEL AU SERVICE DE GESTION     00191500
      * DES COMPTES RENDUS EN MODE TERMINE                              00191600
      ******************************************************************00191700
       40000-CPTRENDU-TERMINE.                                          00191800
      *-----------------------                                          00191900
      *--  GESTION DES TRACES                                           00192000
           IF CP00SV21-TRACE-ACTIVE                                     00192100
              MOVE '40000-CPTRENDU-TERMINE' TO LI-ID-DON-TRACE          00192200
              SET       CP00SV21-NIV-PAR    TO TRUE                     00192300
              PERFORM   80000-ALIM-TRACE                                00192400
           END-IF                                                       00192500
                                                                        00192600
           MOVE    7 TO WS-CO-TYPE-LIG-CRU                              00192700
           PERFORM 40000-INITIALISER-PP00SV04                           00192800
           PERFORM 80000-APPEL-PP00SV04                                 00192900
                                                                        00193000
           MOVE    8 TO WS-CO-TYPE-LIG-CRU                              00193100
           PERFORM 40000-INITIALISER-PP00SV04                           00193200
           PERFORM 80000-APPEL-PP00SV04                                 00193300
                                                                        00193400
           MOVE    9 TO WS-CO-TYPE-LIG-CRU                              00193500
           PERFORM 40000-INITIALISER-PP00SV04                           00193600
           PERFORM 80000-APPEL-PP00SV04                                 00193700
                                                                        00193800
      *--  GESTION DES COMPTES-RENDUS FINAUX                            00193900
           MOVE WS-NB-OCC     TO WS-NB-DOS                              00194000
           MOVE WS-NB-ANO        TO WS-CT-ANO                           00194100
                                                                        00194200
           MOVE WS-GR-CRU TO TB-GR-CRU                                  00194300
           PERFORM 40000-CPTRENDU-GENE                                  00194400
                                                                        00194500
      *--  POSITIONNEMENT CODE RETOUR A ZERO CAR TRAITEMENT TERMINE     00194600
           SET CO-RET-OK TO TRUE                                        00194800
           .                                                            00194900
      ******************************************************************00195000
      * ALIMENTATION DU COMPTE-RENDU PAR LES INFORMATIONS METIER        00195100
      *    UNE STRUCTURE A PARTIR DE LAQUELLE ON ALIMENTE :             00195200
      *         . GR-CRU                                                00195300
      *           EN TENANT COMPTE DE CRU-NB-RUB                        00195400
      ******************************************************************00195500
       40000-CPTRENDU-GENE.
      *--------------------                                             00195700
      *--  GESTION DES TRACES                                           00195800
           IF CP00SV21-TRACE-ACTIVE                                     00195900
              MOVE '40000-CPTRENDU-GENE' TO LI-ID-DON-TRACE             00196000
              SET       CP00SV21-NIV-PAR    TO TRUE                     00196100
              PERFORM   80000-ALIM-TRACE                                00196200
           END-IF                                                       00196300
                                                                        00196400
           MOVE 1 TO WS-IX-CRU                                          00196500
D     *    DISPLAY '         --  TB-NB-RUB-CRU :'TB-NB-RUB-CRU          00299200
           PERFORM UNTIL WS-IX-CRU > TB-NB-RUB-CRU                      00196600
                                                                        00196700
              MOVE   11 TO WS-CO-TYPE-LIG-CRU                           00196800
              PERFORM 40000-INITIALISER-PP00SV04                        00196900
              PERFORM 80000-APPEL-PP00SV04                              00197000
                                                                        00197100
              ADD 1 TO WS-IX-CRU                                        00197200
                                                                        00197300
           END-PERFORM                                                  00197400
           .                                                            00197500
      ******************************************************************00197600
      * INITIALISATION DES PARAMETRES D'APPEL AU SERVICE DE GESTION     00197700
      * DES REPRISES EN MISE A JOUR                                     00197800
      ******************************************************************00197900
       40000-MAJ-RESTART.                                               00198000
      *------------------                                               00198100
      *--  GESTION DES TRACES                                           00198200
           IF CP00SV21-TRACE-ACTIVE                                     00198300
              MOVE    '40000-MAJ-RESTART'   TO LI-ID-DON-TRACE          00198400
              SET       CP00SV21-NIV-PAR    TO TRUE                     00198500
              PERFORM   80000-ALIM-TRACE                                00198600
           END-IF                                                       00198700
                                                                        00198800
      *--  APPEL AU SERVICE DE GESTION DES REPRISES                     00198900
           PERFORM 40000-INITIALISER-PP00SV05                           00199000
           SET CP00SV05-FONC-MAJ      TO TRUE                           00199100
           PERFORM 40000-ALIM-DONNEES-RESTART                           00199200
           PERFORM 80000-APPEL-PP00SV05                                 00199300
           .                                                            00199400
      ******************************************************************00199500
      * INITIALISATION DES PARAMETRES D'APPEL AU SERVICE DES            00199600
      * TRANSACTIONS BATCH POUR REALISER UN COMMIT                      00199700
      ******************************************************************00199800
       40000-GESTION-COMMIT.
      *---------------------                                            00200000
      *--  GESTION DES TRACES                                           00200100
           IF CP00SV21-TRACE-ACTIVE                                     00200200
              MOVE   '40000-GESTION-COMMIT' TO LI-ID-DON-TRACE          00200300
              SET       CP00SV21-NIV-PAR    TO TRUE                     00200400
              PERFORM   80000-ALIM-TRACE                                00200500
           END-IF                                                       00200600
                                                                        00200700
      *--  APPEL AU SERVICE DE GESTION DES TRANSACTIONS BATCH           00200800
           PERFORM 40000-INITIALISER-PP00SV02                           00200900
           SET CP00SV02-COMMIT       TO TRUE                            00201000
           PERFORM 80000-APPEL-PP00SV02                                 00201100
                                                                        00201200
                                                                        00201300
           MOVE ZERO TO WS-NB-OCC-COM                                   00201400
      *--  MISE A JOUR DES ZONES TAMPONS PAR LES CHAMPS DE              00201500
      *--  LA MISE A JOUR DE LA TABLE DES RESTARTS                      00201600
           MOVE LI-CO-STATUT-STEP TO WS-CO-STUT-SAUV                    00201700
           MOVE LI-ID-CLE-RPRI TO WS-ID-CLE-RPRI-SAUV                   00201800
           MOVE LI-ID-CLE-ANO TO WS-ID-CLE-ANO-SAUV                     00201900
           MOVE LI-NB-ANO TO WS-NB-ANO-SAUV                             00202000
           MOVE LI-CO-RET TO WS-CO-RET-SAUV                             00202100
           MOVE LI-NB-OCC TO WS-NB-OCC-SAUV                             00202200
           MOVE LI-NB-ROLL TO WS-NB-ROLL-SAUV                           00202300
           MOVE LI-LB-SAUV TO WS-GR-SAV-SAUV                            00202400
                                                                        00202500
      *--  SI L'INDICE DE RELECTURE DU PAS DE COMMIT EST A OUI          00202600
      *--   RAFRAICHISSEMENT DE LA DATE ET DE L'HEURE                   00202700
      *--   LECTURE DU PAS DE COMMIT 'ACTUALISE' EN FONCTION            00202800
      *--   DE LA DATE ET DE L'HEURE SYSTEME                            00202900
           IF LO-IN-RLEC = 'O'                                          00203000
              PERFORM 12100-RECUP-PAS-COMMIT                            00203100
           END-IF                                                       00203200
                                                                        00203300
      *--  REINITIALISATION DE L'INDICATEUR DE COMMIT A 'NON'           00203400
           PERFORM 40000-INITIALISER-PP00SV25                           00203500
           SET CP00SV25-MODIF-IND-COMMIT   TO TRUE                      00203600
           SET CP00SV25-LI-COMMIT-NON      TO TRUE                      00203700
           PERFORM 80000-APPEL-PP00SV25                                 00203800
           .                                                            00203900
      ******************************************************************00204000
      * INITIALISATION DES PARAMETRES D'APPEL AU SERVICE DE GESTION     00204100
      * DES COMPTES RENDUS EN MODE REPRISE                              00204200
      ******************************************************************00204300
       40000-CPTRENDU-REPRISE.                                          00204400
      *-----------------------                                          00204500
      *--  GESTION DES TRACES                                           00204600
           IF CP00SV21-TRACE-ACTIVE                                     00204700
              MOVE '40000-CPTRENDU-REPRISE' TO LI-ID-DON-TRACE          00204800
              SET       CP00SV21-NIV-PAR    TO TRUE                     00204900
              PERFORM   80000-ALIM-TRACE                                00205000
           END-IF                                                       00205100
                                                                        00205200
           MOVE   10 TO WS-CO-TYPE-LIG-CRU                              00205300
           PERFORM 40000-INITIALISER-PP00SV04                           00205400
           PERFORM 80000-APPEL-PP00SV04                                 00205500
           .                                                            00205600
      ******************************************************************00205700
      * INITIALISATION DES PARAMETRES D'APPEL DU SERVICE DE GESTION     00205800
      * DES OCCURRENCES A TRAITER EN MODE SELECTION                     00205900
      ******************************************************************00206000
       50000-OUV-SELEC-OCC.                                             00206100
      *--------------------                                             00206200
      *--  GESTION DES TRACES                                           00206300
           IF CP00SV21-TRACE-ACTIVE                                     00206400
              MOVE '50000-OUV-SELEC-OCC'    TO LI-ID-DON-TRACE          00206500
              SET       CP00SV21-NIV-PAR    TO TRUE                     00206600
              PERFORM   80000-ALIM-TRACE                                00206700
           END-IF                                                       00206800
                                                                        00206900
      *--  INITIALISATION DE L'UNITE FONCTIONNELLE EN COURS DE LECTURE  00207000
           INITIALIZE WS-ID-UNIT-FONC-LUE                               00309800
           INITIALIZE WS-ID-TECH-ENV-LUE9                               00309900
                                                                        00207200
      *--  APPEL AU MODULE DE GESTION DES OCCURRENCES A TRAITER EN INIT 00207300
           IF PPGEAL20-IS-CLOSED
              PERFORM 40000-INITIALISER-PPGEAL20                        00310200
              MOVE WS-CO-CONTXT-TRT-MB17
                TO LI-EDF-CO-MOD-CREA-ENV   OF LK-CPGEAL20
              MOVE 'EC' TO LI-EDF-CO-STUT-ENV-AF OF LK-CPGEAL20
                                                                        00311700
              MOVE '[- TRAITEMENT PONCTUEL: P -]'
                TO WS-LB-NB-RUB-0-TXT
                                                                        00312400
              SET CPGEAL20-OPEN TO TRUE                                 00312600
              PERFORM 80000-APPEL-PPGEAL20                              00312700
           END-IF
           .                                                            00208000
                                                                        00312900
                                                                        00313000
      *-----------------------*                                         00313100
       40000-OPEN-FCGEEDF1.
      *-----------------------*                                         00313300
           OPEN INPUT  FCGEEDF1                                         00313400
                                                                        00313500
           IF FS-FCGEEDF1-STATUS NOT = '00'                             00313600
              SET PPGEMB17-ERREUR-TECH    TO TRUE                       00313900
              SET PPGEMB17-ERREUR-PGM     TO TRUE                       00314000
                                                                        00314100
              STRING 'ERREUR 40000-OPEN-FCGEEDF1 STATUS:'               00314200
              FS-FCGEEDF1-STATUS DELIMITED BY SIZE                      00314300
                  INTO LK-LB-MSG-LNG-MOD OF LK-GR-PPGEMB17              00314400
              PERFORM 90000-DISPLAY-ERREUR                              00314500
              SET CO-RET-KO TO TRUE                                     00314600
                     display '40000-open fcgeedf1 KO KO'
              PERFORM 31000-ABANDON                                     00314700
           ELSE
M014+         INITIALIZE WS-ID-UNIT-FONC-LUE     WS-ID-CLE-PREC
           END-IF                                                       00314800
           .                                                            00314900
                                                                        00315000
      *-----------------------*                                         00315100
       40000-OPEN-FC99FIS1.
      *-----------------------*                                         00315300
           OPEN INPUT  FC99FIS1                                         00313400
                                                                        00313500
           IF FS-FC99FIS1-STATUS NOT = '00'                             00313600
              SET PPGEMB17-ERREUR-TECH    TO TRUE                       00313900
              SET PPGEMB17-ERREUR-PGM     TO TRUE                       00314000
                                                                        00314100
              STRING 'ERREUR 40000-OPEN-FC99FIS1 STATUS:'               00314200
              FS-FC99FIS1-STATUS DELIMITED BY SIZE                      00314300
                  INTO LK-LB-MSG-LNG-MOD OF LK-GR-PPGEMB17              00314400
              PERFORM 90000-DISPLAY-ERREUR                              00314500
              SET CO-RET-KO TO TRUE                                     00314600
                     display '40000-open FC99FIS1 KO KO'
              PERFORM 31000-ABANDON                                     00314700
           END-IF                                                       00314800
           .                                                            00314900
                                                                        00315000
      *-----------------------*                                         00315100
       41000-LECTURE-FCGEEDF1.
      *-----------------------*                                         00315300
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              STRING '==>MB17 41000-LECTURE-FCGEEDF1'
              ' AVANT LECT; WS-GR-CLE-EDF =' WS-GR-CLE-EDF
              DELIMITED BY SIZE INTO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
      *

           READ FCGEEDF1 INTO WS-GR-EDF                                 00315400

           IF NOT EOF-FCGEEDF1 AND NOT OK-FCGEEDF1                      00315500
              SET PPGEMB17-ERREUR-TECH     TO TRUE                      00315800
              SET PPGEMB17-ERREUR-PGM     TO TRUE                       00315900
                                                                        00316000
              STRING 'ERREUR 40000-OPEN-FCGEEDF1 STATUS:'               00316100
              FS-FCGEEDF1-STATUS DELIMITED BY SIZE                      00316200
                  INTO LK-LB-MSG-LNG-MOD OF LK-GR-PPGEMB17              00316300
              PERFORM 90000-DISPLAY-ERREUR                              00316400
              SET CO-RET-KO TO TRUE                                     00316500
                     display '41000-LECTURE-FCGEEDF1 KO'
              PERFORM 31000-ABANDON                                     00316600
           ELSE                                                         00316700
              IF NOT EOF-FCGEEDF1                                       00316800

                 MOVE LI-EDF-CO-REG-DECL   OF WS-GR-EDF
                   TO WS-CO-REG-EDF WS-CO-REG-LU
                 MOVE LI-EDF-NO-INST-DECL  OF WS-GR-EDF
                   TO WS-NO-INST-DECL-EDF WS-NO-INST-LU
                 MOVE LI-EDF-ID-TECH-INDV  OF WS-GR-EDF
                   TO WS-ID-TECH-INDV-EDF WS-ID-TECH-INDV-LU
                 MOVE LI-LIF-ID-TECH-DPR   OF WS-GR-EDF
                   TO WS-ID-TECH-DPR-LIF

M014+ *          Initialize
M014+ *          WS-ID-UNIT-FONC-PREC alimentera  cle-ano en cntxt M
M014+            IF WS-ID-CLE-PREC(1:4) = SPACE
M014+               MOVE WS-ID-UNIT-FONC-LUE
M014+                 TO WS-ID-CLE-PREC  WS-ID-UNIT-FONC-PREC
M014+            END-IF
M014+
M014+            IF WS-ID-UNIT-FONC-LUE NOT = WS-ID-CLE-PREC
M014+               MOVE WS-ID-CLE-PREC
M014+                 TO WS-ID-UNIT-FONC-PREC
M014+
M015+ *--           toutes les Lignes fiscales de l'ULF sont  lues
M015+               SET OUI-LECT-TTE-LF-ULF-OK TO TRUE
M014+
M014+               MOVE WS-ID-UNIT-FONC-LUE
M014+                 TO WS-ID-CLE-PREC
M014+            END-IF


INFO  *   BLOC DEPLACE DANS BLOC METIER AVANT CONTROLE REPRISE
INFO  *        SUITE BUG COMPTEUR QUAND RUPTURE SUR INSTITUTION
INFO  *---TODO : A FAIRE IDEM DANS  TRT-PONCTUEL
INFO  *_________________________________________________
      *           IF WS-GR-INSTIT-PREC-CT NOT = WS-GR-CLE-EDF(1:4)
      *-- CREATION COMPTE RENDU FONCTIONNEL POUR INSTITUTION
      *              IF WS-GR-INSTIT-PREC-CT NOT = SPACE AND LOW-VALUE
      *                 PERFORM 26000-ALIM-CRF-MET
      *                 PERFORM 26000-ALIM-CRF-MET-MT
      *              END-IF
      *
      *              ADD 1 TO WS-NB-TOT-INSTIT
      *
      *              MOVE WS-GR-CLE-EDF(1:4) TO WS-GR-INSTIT-PREC-CT
      *              INITIALIZE WS-CT-FONC
      *           END-IF

                  IF WS-ID-TECH-INDV-EDF NOT = WS-ID-TECH-INDV-PREC-CT
                     MOVE WS-ID-TECH-INDV-EDF
                       TO WS-ID-TECH-INDV-PREC-CT
                     ADD 1 TO WS-NB-AL-TOT-1
                  END-IF

M004+             IF WS-GR-INSTIT-PREC-CT = SPACE
    |                MOVE WS-GR-CLE-EDF(1:4) TO WS-GR-INSTIT-PREC-CT
M004+             END-IF                                                00317000
      *--         GESTION DES TRACES
                  IF CP00SV21-TRACE-ACTIVE
                     STRING '==>MB17 FIN-41000-LECTURE-FCGEEDF1'
                     ' APRES LECT; WS-GR-CLE-EDF =' WS-GR-CLE-EDF '<'
                     DELIMITED BY SIZE INTO LI-ID-DON-TRACE
                     PERFORM 88888-NIV-TRACE-SHOW

                     STRING 'WS-ID-CLE-PREC=' WS-ID-CLE-PREC
                     '; WS-ID-UNIT-FONC-PREC=' WS-ID-UNIT-FONC-PREC '<'
                     DELIMITED BY SIZE INTO LI-ID-DON-TRACE
                     PERFORM 88888-NIV-TRACE-SHOW
                  END-IF
              ELSE
M014+           IF EOF-FCGEEDF1
M014+             MOVE WS-ID-UNIT-FONC-LUE     TO WS-ID-UNIT-FONC-PREC
M014+           END-IF
              END-IF                                                    00317000
           END-IF                                                       00317100
           .                                                            00317200
                                                                        00317300
      *-----------------------*                                         00315100
       41000-LECTURE-FC99FIS1.
      *-----------------------*                                         00315300
      *-- GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 41000-LECTURE-FC99FIS1' TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
      *
           READ FC99FIS1 INTO WS-GR-FIS                                 00315400
           IF NOT EOF-FC99FIS1 AND NOT OK-FC99FIS1                      00315500
              SET PPGEMB17-ERREUR-TECH     TO TRUE                      00315800
              SET PPGEMB17-ERREUR-PGM     TO TRUE                       00315900
                                                                        00316000
              STRING 'ERREUR 41000-LECTURE-FC99FIS1 STATUS:'            00316100
              FS-FC99FIS1-STATUS DELIMITED BY SIZE                      00316200
                  INTO LK-LB-MSG-LNG-MOD OF LK-GR-PPGEMB17              00316300
              PERFORM 90000-DISPLAY-ERREUR                              00316400
              SET CO-RET-KO TO TRUE                                     00316500
                     display '41000-LECTURE-FC99FIS1 KO'
              PERFORM 31000-ABANDON                                     00316600
           ELSE                                                         00316700
              IF NOT EOF-FC99FIS1                                       00316800
                  MOVE LI-FIS-CO-REG OF WS-GR-FIS
                    TO WS-CO-REG-FIS
                  MOVE LI-FIS-NO-INST-DCLA  OF WS-GR-FIS
                    TO WS-NO-INST-DECL-FIS
                  MOVE LI-FIS-ID-TECH-INDV OF WS-GR-FIS
                    TO WS-ID-TECH-INDV-FIS
              END-IF                                                    00317000
           END-IF                                                       00317100
           .                                                            00317200
                                                                        00317300
      *-----------------------*                                         00317400
       48000-CLOSE-FCGEEDF1.
      *-----------------------*                                         00317600
           CLOSE  FCGEEDF1                                              00317700
                                                                        00317800
           IF FS-FCGEEDF1-STATUS NOT = '00'                             00317900
              SET PPGEMB17-ERREUR-TECH     TO TRUE                      00318200
              SET PPGEMB17-ERREUR-PGM     TO TRUE                       00318300
                                                                        00318400
              STRING 'ERREUR 48000-CLOSE-FCGEEDF1 STATUS:'              00318500
              FS-FCGEEDF1-STATUS DELIMITED BY SIZE                      00318600
                  INTO LK-LB-MSG-LNG-MOD OF LK-GR-PPGEMB17              00318700
              PERFORM 90000-DISPLAY-ERREUR                              00318800
              SET CO-RET-KO TO TRUE                                     00318900
                     display '48000-CLOSE-FCGEEDFI   KO'
              PERFORM 31000-ABANDON                                     00319000
           END-IF                                                       00319100
           .                                                            00319200
                                                                        00319300
      *-----------------------*                                         00317400
       48000-CLOSE-FC99FIS1.
      *-----------------------*                                         00317600
           CLOSE  FC99FIS1                                              00317700
                                                                        00317800
           IF FS-FC99FIS1-STATUS NOT = '00'                             00317900
              SET PPGEMB17-ERREUR-TECH     TO TRUE                      00318200
              SET PPGEMB17-ERREUR-PGM     TO TRUE                       00318300
                                                                        00318400
              STRING 'ERREUR 48000-CLOSE-FC99FIS1 STATUS:'              00318500
              FS-FC99FIS1-STATUS DELIMITED BY SIZE                      00318600
                  INTO LK-LB-MSG-LNG-MOD OF LK-GR-PPGEMB17              00318700
              PERFORM 90000-DISPLAY-ERREUR                              00318800
              SET CO-RET-KO TO TRUE                                     00318900
                     display '48000-CLOSE-FC99FIS1   KO'
              PERFORM 31000-ABANDON                                     00319000
           END-IF                                                       00319100
           .                                                            00319200
                                                                        00319300
                                                                        00319400
      ******************************************************************00208100
      * SELECTION UNITAIRE DES OCCURRENCES A TRAITER                    00208200
      ******************************************************************00208300
      *-----------------                                                00319800
       50100-RESTIT-OCC.
      *-----------------                                                00208500
      *--  APPEL AU MODULE DE GESTION DES OCCURRENCES A TRAITER EN RECH.00209300
           IF PPGEAL20-IS-OPENED
              PERFORM 40000-INITIALISER-PPGEAL20                        00320900
              SET CPGEAL20-FETCH TO TRUE                                00321000
              MOVE 06 TO LK-NO-REQ-ACC OF LK-CPGEAL20
              PERFORM 80000-APPEL-PPGEAL20                              00322200
           END-IF
           .                                                            00209900
                                                                        00322400
      ******************************************************************00210000
      * FERMETURE DE LA SELECTION OCCURRENCES                           00210100
      ******************************************************************00210200
       50200-CLOSE-OCC.                                                 00210300
      *----------------                                                 00210400
      *--  GESTION DES TRACES                                           00210500
           IF CP00SV21-TRACE-ACTIVE                                     00210600
              MOVE '==>MB17 50200-FIN-SELEC-OCC'    TO LI-ID-DON-TRACE  00210700
              SET       CP00SV21-NIV-PAR    TO TRUE                     00210800
              PERFORM   80000-ALIM-TRACE                                00210900
           END-IF                                                       00211000
                                                                        00211100
      *--  APPEL AU MODULE DE GESTION DES OCCURRENCES A TRAITER MODE FIN00211200
           IF PPGEAL20-IS-OPENED
              PERFORM 40000-INITIALISER-PPGEAL20
              SET CPGEAL20-CLOSE TO TRUE
              PERFORM 80000-APPEL-PPGEAL20
           END-IF
                                                                        00324000
           IF PPGEAL21-IS-OPENED
              PERFORM 40000-INITIALISER-PPGEAL21
              SET CPGEAL21-CLOSE TO TRUE
              PERFORM 80000-APPEL-PPGEAL21
           END-IF
           .                                                            00211800
                                                                        00324300
      ******************************************************************00211900
      * INITIALISATION DES PARAMETRES METIER                            00212000
      ******************************************************************00212100
       60000-INIT-METIER-DEMARRAGE.                                     00212200
      *----------------------------                                     00212300
      *--  GESTION DES TRACES                                           00214200
           IF CP00SV21-TRACE-ACTIVE                                     00214300
              MOVE '60000-INIT-METIER-DEMARRAGE' TO LI-ID-DON-TRACE     00214400
              SET       CP00SV21-NIV-PAR    TO TRUE                     00214500
              PERFORM   80000-ALIM-TRACE                                00214600
           END-IF                                                       00214700
                                                                        00214800
      *--  ACTIONS SPECIFIQUES                                          00214900
           .                                                            00215200
      ******************************************************************00215300
       60050-INIT-METIER-REP-OU-NON.                                    00215400
      *-----------------------------                                    00215500
      *--  GESTION DES TRACES                                           00215600
           IF CP00SV21-TRACE-ACTIVE                                     00215700
              MOVE '60050-INIT-METIER-REP-OU-NON' TO LI-ID-DON-TRACE    00215800
              SET       CP00SV21-NIV-PAR    TO TRUE                     00215900
              PERFORM   80000-ALIM-TRACE                                00216000
           END-IF                                                       00216100
                                                                        00216200
      *--  ACTIONS SPECIFIQUES                                          00216300
           .                                                            00216800
      ******************************************************************00216900
      * INITIALISATION DES PARAMETRES METIER                            00217000
      ******************************************************************00217100
       60100-INIT-METIER-REPRISE.                                       00217200
      *--------------------------                                       00217300
      *--  GESTION DES TRACES                                           00218300
           IF CP00SV21-TRACE-ACTIVE                                     00218400
              MOVE '60100-INIT-METIER-REPRISE' TO LI-ID-DON-TRACE       00218500
              SET       CP00SV21-NIV-PAR    TO TRUE                     00218600
              PERFORM   80000-ALIM-TRACE                                00218700
           END-IF                                                       00218800
                                                                        00218900
      *--  ACTIONS SPECIFIQUES                                          00219000
           .                                                            00219500
      ******************************************************************00219600
      *60200-TRT-METIER.                                                00328500
      *-----------------                                                00219800
      *--  GESTION DES TRACES                                           00222700
      *    IF CP00SV21-TRACE-ACTIVE                                     00328800
      *       MOVE   '60200-TRT-METIER' TO LI-ID-DON-TRACE              00328900
      *       SET       CP00SV21-NIV-PAR    TO TRUE                     00329000
      *       PERFORM   80000-ALIM-TRACE                                00329100
      *    END-IF                                                       00329200
      *                                                                 00329300
      *--  APPEL AU SERVICE D'ORCHESTRATION BATCH                       00223400
      *    PERFORM 40000-INITIALISER-PPGESVB3                           00329500
      *    PERFORM 80000-APPEL-PPGESVB3                                 00329600
      *    .                                                            00329700
      ******************************************************************00223800
      * FINALISATION DES ACTIONS METIER                                 00223900
      *    . EN FIN DE TRAITEMENT                                       00224000
      ******************************************************************00224100
       60500-FIN-METIER.                                                00224200
      *-----------------                                                00224300
      *--  GESTION DES TRACES                                           00225100
           IF CP00SV21-TRACE-ACTIVE                                     00225200
              MOVE '60500-FIN-METIER' TO LI-ID-DON-TRACE                00225300
              SET       CP00SV21-NIV-PAR    TO TRUE                     00225400
              PERFORM   80000-ALIM-TRACE                                00225500
           END-IF                                                       00225600
                                                                        00225700
      *--  ACTIONS SPECIFIQUES                                          00225800
                                                                        00226600
      *--  GESTION DES COMPTE-RENDUS METIER                             00226700
      *    MOVE WS-GR-CRU-MET TO TB-GR-CRU                              00331400
      *    PERFORM 40000-CPTRENDU-GENE                                  00331500
           .                                                            00227000
                                                                        00331700
      ******************************************************************00227100
      *    APPEL AU GESTIONNAIRE DE TRACE (DECLARE, INIT, APPEL, FIN)   00227200
      ******************************************************************00227300
       80000-DECL-TRACE.                                                00227400
      *-----------------                                                00227500
           INITIALIZE LK-CP00SV21                                       00227600
                                                                        00227700
           MOVE 'PPGEMB17' TO LI-ID-PGM-APLT OF LK-CP00SV21             00227800
           SET CP00SV21-APPEL-BATCH     TO TRUE                         00227900
           SET CP00SV21-APPEL-PGM TO TRUE                               00228000
                                                                        00228100
           CALL PP00SV21 USING LK-CP00SV21                              00228200
           .                                                            00228300
      ******************************************************************00228400
       80000-INIT-TRACE.                                                00228500
      *-----------------                                                00228600
      *--  ALIMENTATION DES ZONES TECHNIQUES GENERIQUES                 00228700
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CP00SV21   00228800
                                       LK-ID-PGM-APLT  OF LK-CP00SV21   00228900
                                       LI-ID-PGM-APLT  OF LK-CP00SV21   00229000
           MOVE 'PP00SV02'          TO LK-ID-PGM-APLE  OF LK-CP00SV21   00229100
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CP00SV21   00229200
           SET CP00SV02-APPEL-BATCH TO TRUE                             00229300
                                                                        00229400
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CP00SV21   00229500
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CP00SV21   00229600
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CP00SV21   00229700
                                                                        00229800
      *--  ALIMENTATION DES ZONES SP?CIFIQUES                           00229900
           SET CP00SV21-DEM-NIV-TRACE TO TRUE                           00230000
                                                                        00230100
           CALL      PP00SV21                 USING LK-CP00SV21         00230200
           .                                                            00230300
      ******************************************************************00230400
       80000-ALIM-TRACE.                                                00230500
      *-----------------                                                00230600
           MOVE 'PPGEMB17'          TO LK-ID-PGM-APLT OF LK-CP00SV21    00230700
           MOVE WS-ID-UNIT-FONC-LUE TO LI-ID-COURS    OF LK-CP00SV21    00230800
                                                                        00335600
           SET CP00SV21-STANDARD    TO TRUE                             00230900
           CALL PP00SV21           USING LK-CP00SV21                    00231000
           .                                                            00231100
      ******************************************************************00231200
       80000-FIN-TRACE.                                                 00231300
      *----------------                                                 00231400
           SET CP00SV21-FIN-PGM       TO TRUE                           00231500
           CALL     PP00SV21                USING LK-CP00SV21           00231600
           .                                                            00231700
      ******************************************************************00231800
      * APPEL AU SERVICE DE GESTION DES TRANSACTIONS BATCH              00231900
      ******************************************************************00232000
       80000-APPEL-PP00SV02.                                            00232100
      *---------------------                                            00232200
      *--  GESTION DES TRACES                                           00232300
           IF CP00SV21-TRACE-ACTIVE                                     00232400
              MOVE '80000-APPEL-PP00SV02' TO LI-ID-DON-TRACE            00232500
              SET       CP00SV21-NIV-PAR    TO TRUE                     00232600
              PERFORM   80000-ALIM-TRACE                                00232700
           END-IF                                                       00232800
                                                                        00232900
           CALL PP00SV02 USING LK-CP00SV02                              00233000
                                                                        00233100
           IF NOT CP00SV02-CO-RET-1ER-OK                                00233200
      *--     ERREUR PROGRAMME                                          00233300
              MOVE LK-LB-SQLCA OF LK-CP00SV02 TO WS-LB-SQLCA            00233400
              PERFORM 80000-ALIM-TRACE-SQLCA                            00233500
      *--     ABANDON DU TRAITEMENT                                     00233600
                     display '80000-APPEL-PP00SV02   KO'
              PERFORM 31000-ABANDON                                     00233700
           END-IF                                                       00233800
           .                                                            00233900
      ******************************************************************00234000
      * APPEL AU SERVICE DE GESTION DES TRANSACTIONS BATCH              00234100
      ******************************************************************00234200
       80000-APPEL-PP00SV02-ABANDON.                                    00234300
      *-----------------------------                                    00234400
      *--  GESTION DES TRACES                                           00234500
           IF CP00SV21-TRACE-ACTIVE                                     00234600
              MOVE '80000-APPEL-PP00SV02-ABANDON' TO LI-ID-DON-TRACE    00234700
              SET       CP00SV21-NIV-PAR    TO TRUE                     00234800
              PERFORM   80000-ALIM-TRACE                                00234900
           END-IF                                                       00235000
                                                                        00235100
           CALL PP00SV02 USING LK-CP00SV02                              00235200
           .                                                            00235300
      ******************************************************************00235400
      * APPEL AU SERVICE DES COMPTES RENDUS                             00235500
      ******************************************************************00235600
       80000-APPEL-PP00SV04.                                            00235700
      *---------------------                                            00235800
      *--  GESTION DES TRACES                                           00235900
           IF CP00SV21-TRACE-ACTIVE                                     00236000
              MOVE '80000-APPEL-PP00SV04' TO LI-ID-DON-TRACE            00236100
              SET       CP00SV21-NIV-PAR    TO TRUE                     00236200
              PERFORM   80000-ALIM-TRACE                                00236300
           END-IF                                                       00236400
                                                                        00236500
           CALL PP00SV04 USING LK-CP00SV04                              00236600
                                                                        00236700
           IF NOT CP00SV04-CO-RET-1ER-OK                                00236800
      *--     ERREUR PROGRAMME                                          00236900
              MOVE LK-LB-SQLCA OF LK-CP00SV04 TO WS-LB-SQLCA            00237000
              PERFORM 80000-ALIM-TRACE-SQLCA                            00237100
      *--     ABANDON DU TRAITEMENT                                     00237200
                     display '80000-APPEL-PP00SV04  CPT RENDu KO'
              PERFORM 31000-ABANDON                                     00237300
           END-IF                                                       00237400
           .                                                            00237500
      ******************************************************************00237600
      * APPEL AU SERVICE DE GESTION DES REPRISES                        00237700
      ******************************************************************00237800
       80000-APPEL-PP00SV05.                                            00237900
      *---------------------                                            00238000
      *--  GESTION DES TRACES                                           00238100
           IF CP00SV21-TRACE-ACTIVE                                     00238200
              MOVE '80000-APPEL-PP00SV05' TO LI-ID-DON-TRACE            00238300
              SET       CP00SV21-NIV-PAR    TO TRUE                     00238400
              PERFORM   80000-ALIM-TRACE                                00238500
           END-IF                                                       00238600
                                                                        00238700
           INITIALIZE LK-GR-ANO OF LK-CP00SV05                          00238800
                                                                        00238900
           CALL PP00SV05 USING LK-CP00SV05                              00239000
                                                                        00239100
           IF NOT CP00SV05-CO-RET-1ER-OK                                00239200
      *--     ERREUR PROGRAMME                                          00239300
              MOVE LK-LB-SQLCA OF LK-CP00SV05 TO WS-LB-SQLCA            00239400
              PERFORM 80000-ALIM-TRACE-SQLCA                            00239500
      *--     ABANDON DU TRAITEMENT                                     00239600
                     display '80000-APPEL-PP00SV05  gest reprise KO'
              PERFORM 31000-ABANDON                                     00239700
           END-IF                                                       00239800
           .                                                            00239900
      ******************************************************************00240000
      * APPEL AU SERVICE DE RECHERCHE DES VARIABLES MVS                 00240100
      ******************************************************************00240200
       80000-APPEL-PP00SV09.                                            00240300
      *---------------------                                            00240400
      *--  GESTION DES TRACES                                           00240500
           IF CP00SV21-TRACE-ACTIVE                                     00240600
              MOVE      '80000-APPEL-PP00SV09' TO LI-ID-DON-TRACE       00240700
              SET       CP00SV21-NIV-PAR    TO TRUE                     00240800
              PERFORM   80000-ALIM-TRACE                                00240900
           END-IF                                                       00241000
                                                                        00241100
           CALL PP00SV09 USING LK-CP00SV09                              00241200
           .                                                            00241500
      ******************************************************************00241600
      * APPEL AU SERVICE DE RECUPERATION DES DONNEES DE LA TABLE        00241700
      * DES COMMIT                                                      00241800
      ******************************************************************00241900
       80000-APPEL-PP00SV10.                                            00242000
      *---------------------                                            00242100
      *--  GESTION DES TRACES                                           00242200
           IF CP00SV21-TRACE-ACTIVE                                     00242300
              MOVE '80000-APPEL-PP00SV10' TO LI-ID-DON-TRACE            00242400
              SET       CP00SV21-NIV-PAR    TO TRUE                     00242500
              PERFORM   80000-ALIM-TRACE                                00242600
           END-IF                                                       00242700
                                                                        00242800
           CALL PP00SV10 USING LK-CP00SV10                              00242900
                                                                        00243000
           IF NOT CP00SV10-CO-RET-1ER-OK                                00243100
      *--     ERREUR PROGRAMME                                          00243200
              IF CP00SV10-NON-TROUVE                                    00243300
                 DISPLAY                                                00243400
                   ' AUCUNE DONNEE DE PILOTAGE DANS LA TABLE TZ00CMT'   00243500
                 DISPLAY                                                00243600
                   ' POUR GERER LE PAS DE COMMIT DU PRESENT APPLICATIF,'00243700
                 DISPLAY                                                00243800
                   ' IL FAUDRAIT CREER UNE OCCURRENCE AVEC LES CLES : ' 00243900
                 DISPLAY                                                00244000
                   ' ID-JOB   = ' LO-ID-JOB OF LK-CP00SV09              00244100
                 DISPLAY                                                00244200
                   ' ID-STEP  = ' LO-ID-STEP OF LK-CP00SV09             00244300
                 DISPLAY                                                00244400
                   ' ID-NM-PGM   = ' 'PPGEMB17'                         00244500
                 DISPLAY                                                00244600
                   ' EN DEHORS DE TOUTE SPECIFICITE, IL EST NECESSAIRE,'00244700
                 DISPLAY                                                00244800
                   ' DE DISPOSER D''UNE OCCURRENCE PAR DEFAUT AVEC '    00244900
                   ' LES CLES : '                                       00245000
                 DISPLAY ' ID-NM-JOB   = ' LO-ID-JOB  OF LK-CP00SV09    00245100
                 DISPLAY ' ID-NM-STEP  = ' LO-ID-STEP OF LK-CP00SV09    00245200
                 DISPLAY ' ID-NM-PGM   = ' 'PPGEMB17'                   00245300
                 DISPLAY                                                00245400
                   ' ATTENTION A BIEN POSITIONNER LA PLAGE DE VALIDITE '00245500
                 DISPLAY                                                00245600
                   ' SUR LAQUELLE SONT APPLIQUES LES PARAMETRES...'     00245700
              END-IF                                                    00245800
                                                                        00245900
              MOVE LK-LB-SQLCA OF LK-CP00SV10 TO WS-LB-SQLCA            00246000
              PERFORM 80000-ALIM-TRACE-SQLCA                            00246100
      *--     ABANDON DU TRAITEMENT                                     00246200
                     display '80000-APPEL-PP00SV10   KO'
              PERFORM 31000-ABANDON                                     00246300
           END-IF                                                       00246400
           .                                                            00246500
      ******************************************************************00246600
      * APPEL AU SERVICE DE RECUPERATION DES DONNEES DE LA TABLE        00246700
      * DE SURVEILLANCE D'EXECUTION                                     00246800
      ******************************************************************00246900
       80000-APPEL-PP00SV11.                                            00247000
      *---------------------                                            00247100
      *--  GESTION DES TRACES                                           00247200
           IF CP00SV21-TRACE-ACTIVE                                     00247300
              MOVE '80000-APPEL-PP00SV11' TO LI-ID-DON-TRACE            00247400
              SET       CP00SV21-NIV-PAR    TO TRUE                     00247500
              PERFORM   80000-ALIM-TRACE                                00247600
           END-IF                                                       00247700
                                                                        00247800
           CALL PP00SV11 USING LK-CP00SV11                              00247900
                                                                        00248000
           IF NOT CP00SV11-CO-RET-1ER-OK                                00248100
      *--     ERREUR PROGRAMME                                          00248200
              IF CP00SV11-NON-TROUVE                                    00248300
                 DISPLAY 'PPGEMB17'                                     00248400
                 DISPLAY 'PPGEMB17'                                     00248500
                   ' AUCUNE DONNEE DE PILOTAGE DANS LA TABLE '          00248600
                 DISPLAY 'PPGEMB17'                                     00248700
                   ' TZ00EXE !!!'                                       00248800
                 DISPLAY 'PPGEMB17'                                     00248900
                 DISPLAY 'PPGEMB17'                                     00249000
                   ' POUR GERER LE CONTEXTE DU PRESENT APPLICATIF,'     00249100
                 DISPLAY 'PPGEMB17'                                     00249200
                   ' IL FAUDRAIT CREER UNE OCCURRENCE AVEC LES CLES : ' 00249300
                 DISPLAY 'PPGEMB17'                                     00249400
                   ' ID-JOB   = ' LO-ID-JOB OF LK-CP00SV09              00249500
                 DISPLAY 'PPGEMB17'                                     00249600
                   ' ID-STEP  = ' LO-ID-STEP OF LK-CP00SV09             00249700
                 DISPLAY 'PPGEMB17'                                     00249800
                   ' ID-NM-PGM   = ' 'PPGEMB17'                         00249900
                 DISPLAY 'PPGEMB17'                                     00250000
                 DISPLAY 'PPGEMB17'                                     00250100
                   ' EN DEHORS DE TOUTE SPECIFICITE, IL EST NECESSAIRE,'00250200
                 DISPLAY 'PPGEMB17'                                     00250300
                   ' DE DISPOSER D''UNE OCCURRENCE PAR DEFAUT AVEC '    00250400
                   ' LES CLES : '                                       00250500
                 DISPLAY 'PPGEMB17'                                     00250600
                   ' ID-NM-JOB   = ' LO-ID-JOB  OF LK-CP00SV09          00250700
                 DISPLAY 'PPGEMB17'                                     00250800
                   ' ID-NM-STEP  = ' LO-ID-STEP OF LK-CP00SV09          00250900
                 DISPLAY 'PPGEMB17'                                     00251000
                   ' ID-NM-PGM   = ' 'PPGEMB17'                         00251100
                 DISPLAY 'PPGEMB17'                                     00251200
              END-IF                                                    00251300
              MOVE LK-LB-SQLCA OF LK-CP00SV11 TO WS-LB-SQLCA            00251400
              PERFORM 80000-ALIM-TRACE-SQLCA                            00251500
      *--     ABANDON DU TRAITEMENT                                     00251600
                     display '80000-APPEL-PP00SV11   KO'
              PERFORM 31000-ABANDON                                     00251700
           END-IF                                                       00251800
           .                                                            00251900
      ******************************************************************00252000
      * APPEL AU SERVICE DE RECUPERATION DES DONNEES DE LA TABLE        00252100
      * PLAGE D'IDENTIFIANT                                             00252200
      ******************************************************************00252300
       80000-APPEL-PP00SV12.                                            00252400
      *---------------------                                            00252500
      *--  GESTION DES TRACES                                           00252600
           IF CP00SV21-TRACE-ACTIVE                                     00252700
              MOVE   '80000-APPEL-PP00SV12'   TO LI-ID-DON-TRACE        00252800
              SET       CP00SV21-NIV-PAR    TO TRUE                     00252900
              PERFORM   80000-ALIM-TRACE                                00253000
           END-IF                                                       00253100
                                                                        00253200
           CALL PP00SV12 USING LK-CP00SV12                              00253300
                                                                        00253400
           IF NOT CP00SV12-CO-RET-1ER-OK                                00253500
      *--  ERREUR PROGRAMME                                             00253600
              IF CP00SV12-NON-TROUVE                                    00253700
                 DISPLAY 'PPGEMB17'                                     00253800
                 DISPLAY 'PPGEMB17'                                     00253900
                   ' AUCUNE PLAGE D''IDENTIFIANTS DANS LA TABLE '       00254000
                 DISPLAY 'PPGEMB17'                                     00254100
                   ' TZ00IDE !!!'                                       00254200
                 DISPLAY 'PPGEMB17'                                     00254300
                 DISPLAY 'PPGEMB17'                                     00254400
                   ' IL FAUDRAIT CREER UNE OCCURRENCE AVEC LES CLES : ' 00254500
                 DISPLAY 'PPGEMB17'                                     00254600
                   ' ID-JOB   = ' LO-ID-JOB  OF LK-CP00SV09             00254700
                 DISPLAY 'PPGEMB17'                                     00254800
                   ' ID-STEP  = ' LO-ID-STEP OF LK-CP00SV09             00254900
                 DISPLAY 'PPGEMB17'                                     00255000
                   ' ID-NM-PGM   = ' 'PPGEMB17'                         00255100
                 DISPLAY 'PPGEMB17'                                     00255200
              END-IF                                                    00255300
              MOVE LK-LB-SQLCA OF LK-CP00SV12 TO WS-LB-SQLCA            00255400
              PERFORM 80000-ALIM-TRACE-SQLCA                            00255500
      *--     ABANDON DU TRAITEMENT                                     00255600
                     display '80000-APPEL-PP00SV12   KO'
              PERFORM 31000-ABANDON                                     00255700
           END-IF                                                       00255800
           .                                                            00255900
      ******************************************************************00256000
      * APPEL AU SERVICE D'INSERTION DE DONNEES DANS LA TABLE           00256100
      * ANOMALIE                                                        00256200
      ******************************************************************00256300
       80000-APPEL-PP00SV16.                                            00256400
      *---------------------                                            00256500
      *--  GESTION DES TRACES                                           00256600
           IF CP00SV21-TRACE-ACTIVE                                     00256700
              MOVE      '80000-APPEL-PP00SV16'   TO LI-ID-DON-TRACE     00256800
              SET       CP00SV21-NIV-PAR    TO TRUE                     00256900
              PERFORM   80000-ALIM-TRACE                                00257000
           END-IF                                                       00257100
                                                                        00257200
           CALL PP00SV16 USING LK-CP00SV16                              00257300
                                                                        00257400
           IF NOT CP00SV16-CO-RET-1ER-OK                                00257500
      *--     ERREUR PROGRAMME                                          00257600
              MOVE LK-LB-SQLCA OF LK-CP00SV16 TO WS-LB-SQLCA            00257700
              PERFORM 80000-ALIM-TRACE-SQLCA                            00257800
      *--     ABANDON DU TRAITEMENT                                     00257900
                     display '80000-APPEL-PP00SV16  ANO tech KO'
              DISPLAY ' --------- CHECK PP00SV16---------------'        00130000
              DISPLAY ' LI-ID-JOB   :'    LI-ID-JOB  OF LK-CP00SV16     00130000
              DISPLAY ' LI-ID-STEP  :'    LI-ID-STEP  OF LK-CP00SV16    00130000
              DISPLAY ' LI-NO-JOB   :'    LI-NO-JOB  OF LK-CP00SV16     00130000
              DISPLAY ' LI-ID-ANO   :'    LI-ID-ANO  OF LK-CP00SV16     00130000
              DISPLAY ' LI-ID-NM-PGM-APEL:' LI-ID-PGM-APEL-ERR          00130000
               OF LK-CP00SV16
              DISPLAY ' LI-ID-NM-PGM-ERR :' LI-ID-PGM-ERR               00130000
               OF LK-CP00SV16
              DISPLAY ' LI-CO-RET-1ER:' LI-CO-RET-1ER-MOD OF LK-CP00SV1600130000
              DISPLAY ' LI-CO-RET-2ND:' LI-CO-RET-2ND-MOD OF LK-CP00SV1600130000
              DISPLAY ' LI-CO-MSG    :' LI-CO-MSG-MOD OF LK-CP00SV16    00130000
              DISPLAY ' LI-ID-UNIT-FONC-REJ:' LI-ID-UNIT-FONC-REJ       00130000
               OF LK-CP00SV16                                           00130000
              PERFORM 31000-ABANDON                                     00258000
           END-IF                                                       00258100
           .                                                            00258200
      ******************************************************************00258300
      * APPEL AU SERVICE DE CONTEXTE PP00SV25                           00258400
      ******************************************************************00258500
       80000-APPEL-PP00SV25.                                            00258600
      *---------------------                                            00258700
      *--  GESTION DES TRACES                                           00258800
           IF CP00SV21-TRACE-ACTIVE                                     00258900
              MOVE   '80000-APPEL-PP00SV25'   TO LI-ID-DON-TRACE        00259000
              SET       CP00SV21-NIV-PAR    TO TRUE                     00259100
              PERFORM   80000-ALIM-TRACE                                00259200
           END-IF                                                       00259300
                                                                        00259400
           CALL PP00SV25 USING LK-CP00SV25                              00259500
                                                                        00259600
           IF NOT CP00SV25-CO-RET-1ER-OK                                00259700
              MOVE LK-GR-ANO OF LK-CP00SV25                             00364400
              TO LK-GR-ANO OF LK-GR-PPGEMB17                            00364500
              PERFORM 90000-DISPLAY-ERREUR                              00259900
      *--     ABANDON DU TRAITEMENT                                     00260000
                     display '80000-APPEL-PP00SV25  service contexte KO'
              PERFORM 31000-ABANDON                                     00260100
           END-IF                                                       00260200
           .                                                            00260300
      ******************************************************************00260400
      * APPEL SERVICE DE MEMORISATION DU CONTEXTE APPLICATIF            00260500
      ******************************************************************00260600
       80000-APPEL-PP00SVCA.                                            00260700
      *---------------------                                            00260800
      *--  GESTION DES TRACES                                           00260900
           IF CP00SV21-TRACE-ACTIVE                                     00261000
              MOVE   '80000-APPEL-PP00SVCA'   TO LI-ID-DON-TRACE        00261100
              SET       CP00SV21-NIV-PAR    TO TRUE                     00261200
              PERFORM   80000-ALIM-TRACE                                00261300
           END-IF                                                       00261400
                                                                        00261500
           CALL PP00SVCA USING LK-CP00SVCA                              00261700
                                                                        00261800
           IF NOT CP00SVCA-CO-RET-1ER-OK                                00261900
              MOVE LK-GR-ANO OF LK-CP00SVCA                             00366600
              TO LK-GR-ANO OF LK-GR-PPGEMB17                            00366700
              PERFORM 90000-DISPLAY-ERREUR                              00262100
      *--     ABANDON DU TRAITEMENT                                     00262200
                     display '80000-APPEL-PP00SVca contexte aplli KO'
              PERFORM 31000-ABANDON                                     00262300
           END-IF                                                       00262400
           .                                                            00262500
      ******************************************************************00262600
      * APPEL SERVICE DE GESTION DES ERREURS                            00262700
      ******************************************************************00262800
       80000-APPEL-PP00SV31.                                            00262900
      *---------------------                                            00263000
      *--  GESTION DES TRACES                                           00263100
           IF CP00SV21-TRACE-ACTIVE                                     00263200
              MOVE   '80000-APPEL-PP00SV31'   TO LI-ID-DON-TRACE        00263300
              SET       CP00SV21-NIV-PAR    TO TRUE                     00263400
              PERFORM   80000-ALIM-TRACE                                00263500
           END-IF                                                       00263600
                                                                        00263700
           CALL PP00SV31 USING LK-CP00SV31                              00263800
                                                                        00263900
           INITIALIZE LK-GR-ANO            OF WS-GR-ERR

           IF NOT CP00SV31-CO-RET-1ER-OK                                00264000
              MOVE LK-GR-ANO OF LK-CP00SV31                             00368800
              TO LK-GR-ANO OF LK-GR-PPGEMB17                            00368900
              PERFORM 90000-DISPLAY-ERREUR                              00264200
      *--     ABANDON DU TRAITEMENT                                     00264300
                     display '80000-APPEL-PP00SV31 serv gest erreurs KO'
              PERFORM 31000-ABANDON                                     00264400
           END-IF                                                       00264500
           .                                                            00264600
      ******************************************************************00264700
      * APPEL SERVICE D'INSERTTIONS DES ANOS FONC                       00264800
      ******************************************************************00264900
       80000-APPEL-PP00SV32.                                            00265000
      *---------------------                                            00265100
      *--  GESTION DES TRACES                                           00265200
           IF CP00SV21-TRACE-ACTIVE                                     00265300
              MOVE   '80000-APPEL-PP00SV32'   TO LI-ID-DON-TRACE        00265400
              SET       CP00SV21-NIV-PAR    TO TRUE                     00265500
              PERFORM   80000-ALIM-TRACE                                00265600
           END-IF                                                       00265700
                                                                        00265800
           CALL PP00SV32 USING LK-CP00SV32                              00265900
                                                                        00266000
           INITIALIZE LK-GR-ANO OF WS-GR-ERR
                                                                        00266000
           IF NOT CP00SV32-CO-RET-1ER-OK                                00266100
              MOVE LK-GR-ANO OF LK-CP00SV32                             00371000
              TO LK-GR-ANO OF LK-GR-PPGEMB17                            00371100
              PERFORM 90000-DISPLAY-ERREUR                              00266300
      *--     ABANDON DU TRAITEMENT                                     00266400
                     display '80000-APPEL-PP00SV32 serv ano fonc KO'
              DISPLAY ' --------- CHECK PP00SV32---------------'        00130000
              DISPLAY ' LI-ID-NM-PGM:'    LI-ID-NM-PGM OF LK-CP00SV32   00130000
              DISPLAY ' LI-ID-JOB   :'    LI-ID-JOB  OF LK-CP00SV32     00130000
              DISPLAY ' LI-ID-STEP  :'    LI-ID-STEP  OF LK-CP00SV32    00130000
              DISPLAY ' LI-NO-JOB   :'    LI-NO-JOB  OF LK-CP00SV32     00130000
              DISPLAY ' LI-ID-ANO   :'    LI-ID-ANO  OF LK-CP00SV32     00130000
              DISPLAY ' LI-ID-NM-PGM-APEL:' LI-ID-NM-PGM-APEL           00130000
               OF LK-CP00SV32
              DISPLAY ' LI-ID-NM-PGM-ERR :' LI-ID-NM-PGM-ERR            00130000
               OF LK-CP00SV32
              DISPLAY ' LI-CO-RET-1ER    :' LI-CO-RET-1ER OF LK-CP00SV3200130000
              DISPLAY ' LI-CO-RET-2ND    :' LI-CO-RET-2ND OF LK-CP00SV3200130000
              DISPLAY ' LI-CO-MSG        :' LI-CO-MSG OF LK-CP00SV32    00130000
              DISPLAY ' LI-ID-UNIT-FONC-REJ:' LI-ID-UNIT-FONC-REJ       00130000
               OF LK-CP00SV32                                           00130000

              PERFORM 31000-ABANDON                                     00266500
           END-IF                                                       00266600
           .                                                            00266700
      *----------------------*
       80000-APPELER-PP00SV30.
      *----------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '80000-APPELER-PP00SV30' TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE
              PERFORM   80000-ALIM-TRACE
           END-IF

           CALL PP00SV30 USING LK-CP00SV30

           IF CP00SV30-MAJ-DB2-OUI
              SET PPGEMB17-MAJ-DB2-OUI TO TRUE
           END-IF

      *--  GESTION DES RETOURS
           EVALUATE TRUE
      *--     GESTION DES RETOURS OK
              WHEN CP00SV30-CO-RET-1ER-OK AND CP00SV30-CO-RET-2ND-OK
                 CONTINUE
      *--     GESTION DES ERREURS TECHNIQUE, FONCTIONNELLE OU AUTRES
              WHEN OTHER
      *--      RECUPERATION DES DONNEES DE LA ZONE DE GESTION D'ANOMALIE
      *--      POUR REMONTEE AU PROGRAMME APPELANT
                 MOVE LK-GR-ANO OF LK-CPSPAM01
                   TO LK-GR-ANO OF LK-GR-PPGEMB17

      *          GESTION DES ANOMALIES FONC ET TECHNIQUE
                 PERFORM 23300-TRAITEMENT-ERREUR
           END-EVALUATE
           .
      *----------------------*
       80000-APPELER-PP00SV37.
      *----------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '80000-APPELER-PP00SV37  ' TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE
              PERFORM   80000-ALIM-TRACE
           END-IF

           CALL PP00SV37 USING LK-CP00SV37

           IF CP00SV37-MAJ-DB2-OUI
              SET PPGEMB17-MAJ-DB2-OUI TO TRUE
           END-IF

           EVALUATE TRUE
      *--     GESTION DES RETOURS OK
              WHEN CP00SV37-CO-RET-1ER-OK AND CP00SV37-CO-RET-2ND-OK
                 CONTINUE
      *--     GESTION DES NON TROUVE, ARRET TRAITEMENT
              WHEN CP00SV37-NON-TROUVE
                  CONTINUE
      *--     GESTION DES ERREURS TECHNIQUE, FONCTIONNELLE OU AUTRES
              WHEN OTHER
                 MOVE LK-GR-ANO OF LK-CP00SV37
                   TO LK-GR-ANO OF LK-GR-PPGEMB17
      *          GESTION DES ANOMALIES FONC ET TECHNIQUE
                 PERFORM 23300-TRAITEMENT-ERREUR
           END-EVALUATE
           .

      ******************************************************************
       80000-APPELER-PPSPAM01.
      *-----------------------
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '80000-APPELER-PPSPAM01' TO LI-ID-DON-TRACE
              SET       CP00SV21-NIV-PAR    TO TRUE
              PERFORM   80000-ALIM-TRACE
           END-IF

           CALL PPSPAM01 USING LK-CPSPAM01

           IF CPSPAM01-MAJ-DB2-OUI
              SET PPGEMB17-MAJ-DB2-OUI TO TRUE
           END-IF

      *--  GESTION DES RETOURS
           EVALUATE TRUE
      *--     GESTION DES RETOURS OK
              WHEN CPSPAM01-CO-RET-1ER-OK AND CPSPAM01-CO-RET-2ND-OK
                   CONTINUE
      *--     GESTION DES ERREURS TECHNIQUE, FONCTIONNELLE OU AUTRES
              WHEN OTHER
      *--     GESTION DES TRACES
                 IF CP00SV21-TRACE-ACTIVE
                    MOVE '90000-GESTION-ERREUR-PPSPAM01'
                    TO LI-ID-DON-TRACE
                    SET CP00SV21-NIV-PAR    TO TRUE
                    PERFORM 80000-ALIM-TRACE
                 END-IF

                 MOVE LK-GR-ANO OF LK-CPSPAM01
                   TO LK-GR-ANO OF LK-GR-PPGEMB17

      *          GESTION DES ANOMALIES FONC ET TECHNIQUE
                 PERFORM 23300-TRAITEMENT-ERREUR
           END-EVALUATE
           .
      *-----------------------
      *80000-APPELER-PP00SV58.
      *-----------------------
      *--  GESTION DES TRACES
      *    IF CP00SV21-TRACE-ACTIVE
      *       MOVE 'SV12-80000-APPELER-PP00SV58' TO LI-ID-DON-TRACE
      *       SET       CP00SV21-NIV-PAR    TO TRUE
      *       PERFORM   80000-ALIM-TRACE
      *    END-IF
      *
      *    CALL PP00SV58 USING LK-CP00SV58
      *
      *    IF CP00SV58-MAJ-DB2-OUI
      *       SET PPGEMB17-MAJ-DB2-OUI TO TRUE
      *    END-IF
      *
      *--  GESTION DES RETOURS
      *    EVALUATE TRUE
      *--     GESTION DES RETOURS OK
      *       WHEN CP00SV58-CO-RET-1ER-OK AND CP00SV58-CO-RET-2ND-OK
      *          CONTINUE
      *--     GESTION DES ERREURS TECHNIQUE, FONCTIONNELLE OU AUTRES
      *       WHEN OTHER
      *--        GESTION DES TRACES
      *          IF CP00SV21-TRACE-ACTIVE
      *             MOVE '     GESTION-ERREUR-CP00SV58'
      *             TO LI-ID-DON-TRACE
      *             SET CP00SV21-NIV-PAR    TO TRUE
      *             PERFORM 80000-ALIM-TRACE
      *          END-IF
      *
      *          MOVE LK-GR-ANO OF LK-CP00SV58
      *            TO LK-GR-ANO OF LK-GR-PPGEMB17
      *
      *       GESTION DES ANOMALIES FONC ET TECHNIQUE
      *          PERFORM 23300-TRAITEMENT-ERREUR
      *    END-EVALUATE
      *    .

      *-----------------------
       80000-APPELER-PPSPSV12.
      *-----------------------
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              MOVE '==>MB17 80000-APPELER-PPSPSV12' TO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
            DISPLAY 'CPSP12C1:' LI-GR-FONC-SPE-ENT OF LK-CPSP12C1(1:190)
            DISPLAY ' BLOC:' LI-GR-FONC-SPE-ENT OF LK-CPSP12C1(191:3474)
            DISPLAY 'BLOC PDOC:' LI-GR-BLOC-EDIT(07)
           END-IF
      *
           CALL PPSPSV12 USING LK-CPSP12C1

           IF CPSP12C1-MAJ-DB2-OUI
              SET PPGEMB17-MAJ-DB2-OUI TO TRUE
           END-IF

      *--  GESTION DES RETOURS
           EVALUATE TRUE
      *--     GESTION DES RETOURS OK
              WHEN CPSP12C1-CO-RET-1ER-OK AND CPSP12C1-CO-RET-2ND-OK
                 PERFORM 25000-COMPTEURS-ALL-TRAITE
                 INITIALIZE WS-CT-FONC-TEMP
      *--     GESTION DES ERREURS TECHNIQUE, FONCTIONNELLE OU AUTRES
              WHEN OTHER
      *--        GESTION DES TRACES
                 IF CP00SV21-TRACE-ACTIVE
                    MOVE '       ==>MB17 GESTION-ERREUR-CPSP12C1'
                    TO LI-ID-DON-TRACE
                    PERFORM 88888-NIV-TRACE-SHOW
                    STRING '-->:' LK-GR-ANO OF LK-CPSP12C1
                    DELIMITED BY SIZE INTO LI-ID-DON-TRACE
                    PERFORM 88888-NIV-TRACE-SHOW
                 END-IF

                 MOVE LK-GR-ANO OF LK-CPSP12C1
                   TO LK-GR-ANO OF LK-GR-PPGEMB17

      *--        GESTION DES TRACES
                  IF CP00SV21-TRACE-ACTIVE
                     DISPLAY '=======RETOUR PPSPSV12 ====='
                     DISPLAY 'LK-ID-PGM-MAI:'      LK-ID-PGM-MAI
                     OF LK-CPSP12C1
                     DISPLAY 'LK-ID-PGM-APLT:'     LK-ID-PGM-APLT
                     OF LK-CPSP12C1
                     DISPLAY 'LK-ID-PGM-APLE:'     LK-ID-PGM-APLE
                     OF LK-CPSP12C1
                     DISPLAY 'LK-ID-PGM-APEL-ERR:' LK-ID-PGM-APEL-ERR
                     OF LK-CPSP12C1
                     DISPLAY 'LK-ID-PGM-ERR     :' LK-ID-PGM-ERR
                     OF LK-CPSP12C1
                     DISPLAY 'LK-CO-RET-1ER-MOD:'  LK-CO-RET-1ER-MOD
                     OF LK-CPSP12C1
                     DISPLAY 'LK-CO-RET-2ND-MOD:'  LK-CO-RET-2ND-MOD
                     OF LK-CPSP12C1
                  END-IF

      *           GESTION DES ANOMALIES FONC ET TECHNIQUE
                  PERFORM 23300-TRAITEMENT-ERREUR
           END-EVALUATE
           .

      *-----------------------
       80000-APPELER-PPSPSV14.
      *-----------------------
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              STRING '==>MB17 80000-APPELER-PPSPSV14  BLOC:'
              LI-CO-BLOC OF LK-CPSP1411 OF LK-CPSP14M1

              DELIMITED BY SIZE INTO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
              IF LI-CO-BLOC OF LK-CPSP1411 OF LK-CPSP14M1
              = 'PDOCFISCAL'
                 DISPLAY '        LI-MT-BRUT         :' LI-MT-BRUT
                 OF LK-CPSP14M1 '<===='
                 DISPLAY '        LI-MT-MAJO-EE      :' LI-MT-MAJO-EE
                 OF LK-CPSP14M1 '<===='
                 DISPLAY '        LI-MT-MAJO-EACH    :' LI-MT-MAJO-EACH
                 OF LK-CPSP14M1 '<===='
                 DISPLAY '        LI-MT-COT-AMF      :' LI-MT-COT-AMF
                 OF LK-CPSP14M1 '<===='
                 DISPLAY '        LI-MT-COT-AML      :' LI-MT-COT-AML
                 OF LK-CPSP14M1 '<===='
                 DISPLAY '        LI-MT-CSGND        :' LI-MT-CSGND
                 OF LK-CPSP14M1 '<===='
                 DISPLAY '        LI-MT-CSGND-EE     :' LI-MT-CSGND-EE
                 OF LK-CPSP14M1 '<===='
                 DISPLAY '        LI-MT-CSGD         :' LI-MT-CSGD
                 OF LK-CPSP14M1 '<===='
                 DISPLAY '        LI-MT-RDS          :' LI-MT-RDS
                 OF LK-CPSP14M1 '<===='
                 DISPLAY '        LI-MT-NET-FISC     :' LI-MT-NET-FISC
                 OF LK-CPSP14M1 '<===='
                 DISPLAY '        LI-MT-RAPL-ANT     :' LI-MT-RAPL-ANT
                 OF LK-CPSP14M1 '<===='
                 DISPLAY '        LI-MT-RTN-SRC      :' LI-MT-RTN-SRC
                 OF LK-CPSP14M1 '<===='
                 DISPLAY '      LI-MT-TOT-COT-SECU :' LI-MT-TOT-COT-SECU
                 OF LK-CPSP14M1 '<===='
              END-IF
           END-IF

           CALL PPSPSV14 USING LK-CPSP14M1

           IF CPSP14M1-MAJ-DB2-OUI
              SET PPGEMB17-MAJ-DB2-OUI TO TRUE
           END-IF

      *--  GESTION DES RETOURS
           EVALUATE TRUE
      *--     GESTION DES RETOURS OK
              WHEN CPSP14M1-CO-RET-1ER-OK AND CPSP14M1-CO-RET-2ND-OK
                 CONTINUE
      *--     GESTION DES ERREURS TECHNIQUE, FONCTIONNELLE OU AUTRES
              WHEN OTHER
      *--        GESTION DES TRACES
                 IF CP00SV21-TRACE-ACTIVE
                    MOVE '     GESTION-ERREUR-CPSP14M1'
                    TO LI-ID-DON-TRACE
                    SET CP00SV21-NIV-PAR    TO TRUE
                    PERFORM 80000-ALIM-TRACE
                 END-IF

                 MOVE LK-GR-ANO OF LK-CPSP14M1
                   TO LK-GR-ANO OF LK-GR-PPGEMB17

      *       GESTION DES ANOMALIES FONC ET TECHNIQUE
M016+            SET ARRET-CREATION-FLUX  TO TRUE
M016+            SET ARRET-CREATION-FLUX  TO TRUE
                 PERFORM 23300-TRAITEMENT-ERREUR
M015+-*asticot   SET ARRET-CREATION-FLUX  TO TRUE
           END-EVALUATE
           .


      ******************************************************************00266800
      * APPEL SERVICE GESTION DE DATE                                   00266900
      ******************************************************************00267000
       80000-APPEL-PP00SVP2.                                            00267100
      *---------------------                                            00267200
      *--  GESTION DES TRACES                                           00267300
           IF CP00SV21-TRACE-ACTIVE                                     00267400
              MOVE      '80000-APPEL-PP00SVP2'   TO LI-ID-DON-TRACE     00267500
              SET       CP00SV21-NIV-PAR    TO TRUE                     00267600
              PERFORM   80000-ALIM-TRACE                                00267700
           END-IF                                                       00267800
                                                                        00267900
           CALL PP00SVP2 USING LK-CP00SVP2                              00268000
                                                                        00268100
      *--  GESTION DES CODES RETOUR                                     00268200
           IF NOT CP00SVP2-CO-RET-1ER-OK                                00268300
              MOVE LK-GR-ANO OF LK-CP00SVP2                             00373300
              TO LK-GR-ANO OF LK-GR-PPGEMB17                            00373400
              PERFORM 90000-DISPLAY-ERREUR                              00268500
      *--     ABANDON DU TRAITEMENT                                     00268600
                     display '80000-APPEL-PP00SV31 serv gest date KO'
              PERFORM 31000-ABANDON                                     00268700
           END-IF                                                       00268800
           .                                                            00268900
      ******************************************************************00269000
      * APPEL AU SERVICE DE GESTION DES OCCURRENCES A TRAITER           00269100
      ******************************************************************00269200
       80000-APPEL-PPGEAL20.
      *---------------------                                            00269400
      *--  GESTION DES TRACES                                           00269500
           IF CP00SV21-TRACE-ACTIVE                                     00269600
              MOVE '==>MB17 80000-APPEL-PPGEAL20' TO LI-ID-DON-TRACE    00374700
              SET       CP00SV21-NIV-PAR    TO TRUE                     00269800
              PERFORM   80000-ALIM-TRACE                                00269900
              DISPLAY '-MB17 -GEAL20 INFO:STATUT CURSEUR:'
              WS-CO-STATUT-CURSEUR-PPGEAL20

              IF PPGEAL20-IS-OPENED
                 DISPLAY '    PPGEAL20 IS OPENED'
              END-IF
              IF PPGEAL20-IS-CLOSED
                 DISPLAY '    PPGEAL20 IS CLOSED'
              END-IF
           END-IF                                                       00270000
      *
           MOVE 6 TO LK-NO-REQ-ACC OF LK-CPGEAL20
           CALL PPGEAL20 USING LK-CPGEAL20                              00375200

MAC2 +     IF CPGEAL20-CLOSE AND LK-CO-SQL OF LK-CPGEAL20 = -501
MAC2 +        SET CPGEAL20-CO-RET-2ND-OK  TO TRUE
MAC2 +        SET CPGEAL20-CO-RET-1ER-OK TO TRUE
MAC2 +     END-IF


           EVALUATE TRUE
      *--     GESTION DES RETOURS OK
              WHEN CPGEAL20-CO-RET-1ER-OK AND CPGEAL20-CO-RET-2ND-OK
      *--        ALIMENTATION DES ZONES DE TRAVAIL AVEC LES DONNEES DE  00270600
      *--         L'ULF RECUPEREE                                       00270700
                 IF CPGEAL20-OPEN
                    SET PPGEAL20-IS-OPENED TO TRUE
                 END-IF
                 IF CPGEAL20-CLOSE
                    SET PPGEAL20-IS-CLOSED TO TRUE
                 END-IF

                 IF CPGEAL20-FETCH                                      00375800
                    ADD 1 TO WS-NB-AL-TOT-1

                    MOVE LO-EDF-CO-REG-DECL OF LK-CPGEAL20
                      TO WS-CO-REG-LU  WS-CO-REG-EDF
                    MOVE LO-EDF-NO-INST-DECL OF LK-CPGEAL20
                      TO WS-NO-INST-LU WS-NO-INST-DECL-EDF
                    MOVE LO-EDF-ID-TECH-INDV OF LK-CPGEAL20
                      TO WS-ID-TECH-INDV-LU WS-ID-TECH-INDV-EDF
                 END-IF                                                 00376100
      *--     GESTION DES FINS CURSEURS
              WHEN CPGEAL20-CO-RET-1ER-OK AND CPGEAL20-FIN-CURSEUR
                 CONTINUE
              WHEN OTHER
      *--        GESTION DES TRACES
                 IF CP00SV21-TRACE-ACTIVE
                    MOVE '     GESTION-ERREUR-CPGEAL20'
                      TO LI-ID-DON-TRACE
                    SET CP00SV21-NIV-PAR    TO TRUE
                    PERFORM 80000-ALIM-TRACE
                 END-IF

                 MOVE LK-GR-ANO OF LK-CPGEAL20
                   TO LK-GR-ANO OF LK-GR-PPGEMB17

      *       GESTION DES ANOMALIES FONC ET TECHNIQUE
                 PERFORM 23300-TRAITEMENT-ERREUR
           END-EVALUATE
           .                                                            00271800
M019+ *-----------------------
M019+  80000-APPELER-PPGAAL01.
M019+ *-----------------------
M019+ *--  GESTION DES TRACES
M019+      IF CP00SV21-TRACE-ACTIVE
M019+          STRING 'MB17-80000-APPELER-PPGAAL01'
M019+          ' DPR->' WS-ID-TECH-DPR-LIF
M019+          DELIMITED BY SIZE           INTO LI-ID-DON-TRACE
M019+          SET       CP00SV21-NIV-PAR    TO TRUE
M019+          PERFORM   80000-ALIM-TRACE
M019+      END-IF
M019+ *--
M019+      CALL PPGAAL01 USING LK-CPGAAL01
M019+ *--
M019+ *-- GESTION DES RETOURS
M019+      EVALUATE TRUE
M019+ *--     GESTION DES RETOURS OK
M019+         WHEN CPGAAL01-CO-RET-1ER-OK AND CPGAAL01-CO-RET-2ND-OK
M019+             IF CP00SV21-TRACE-ACTIVE
M019+                MOVE '... Ok Appel à PPGAAL01 (MB17)'
M019+                  TO      LI-ID-DON-TRACE
M019+                SET       CP00SV21-NIV-PAR         TO TRUE
M019+                PERFORM   80000-ALIM-TRACE
M019+             END-IF
M019+ *--     GESTION DES NON-TROUVES
M019+         WHEN CPGAAL01-NON-TROUVE
M019+             IF CP00SV21-TRACE-ACTIVE
M019+                STRING ' KO Appel à PPGAAL01 (MB17)'
M019+                       '  Non trouvé '
M019+                DELIMITED BY SIZE         INTO LI-ID-DON-TRACE
M019+                SET       CP00SV21-NIV-PAR         TO TRUE
M019+                PERFORM   80000-ALIM-TRACE
M019+             END-IF
M019+ *--     GESTION DES ERREURS TECHNIQUE, FONCTIONNELLE OU AUTRES
M019+         WHEN OTHER
M019+             IF CP00SV21-TRACE-ACTIVE
M019+                STRING ' KO Appel à PPGAAL01 (MB17)'
M019+                       '  autres '
M019+                DELIMITED BY SIZE         INTO LI-ID-DON-TRACE
M019+                SET       CP00SV21-NIV-PAR         TO TRUE
M019+                PERFORM   80000-ALIM-TRACE
M019+             END-IF
M019+            MOVE LK-GR-ANO OF LK-CPGAAL01
M019+              TO LK-GR-ANO OF LK-GR-PPGEMB17
M019+
M019+ *       GESTION DES ANOMALIES FONC ET TECHNIQUE
M019+            PERFORM 23300-TRAITEMENT-ERREUR
M019+      END-EVALUATE
M019+      .                                                            00121500
      *---------------------
       80000-APPEL-PPGEAL21.
      *---------------------
      *--  GESTION DES TRACES                                           00269500
           IF CP00SV21-TRACE-ACTIVE                                     00269600
              MOVE '==>MB17 80000-APPEL-PPGEAL21' TO LI-ID-DON-TRACE    00374700
              SET       CP00SV21-NIV-PAR    TO TRUE                     00269800
              PERFORM   80000-ALIM-TRACE                                00269900
              DISPLAY '-MB17 -GEAL21 INFO:STATUT CURSEUR:'
              WS-CO-STATUT-CURSEUR-PPGEAL21
           END-IF                                                       00270000
M002- *    IF CPGEAL21-OPEN
M002- *       SET PPGEAL21-IS-CLOSED TO TRUE
M002- *    END-IF


           MOVE 2 TO LK-NO-REQ-ACC OF LK-CPGEAL21
           CALL PPGEAL21 USING LK-CPGEAL21

           EVALUATE TRUE
      *--     GESTION DES RETOURS OK
              WHEN CPGEAL21-CO-RET-1ER-OK AND CPGEAL21-CO-RET-2ND-OK
                 IF CPGEAL21-OPEN
                    SET PPGEAL21-IS-OPENED TO TRUE
                 END-IF
                 IF CPGEAL21-CLOSE
                    SET PPGEAL21-IS-CLOSED TO TRUE
                 END-IF

                 IF CPGEAL21-FETCH
                    MOVE LO-LIF-ID-TECH-DPR OF LK-CPGEAL21
                      TO WS-ID-TECH-DPR-LIF OF WS-GR-CLE-LIF
                      OF WS-GR-CLE-EDF-LIF
                 END-IF
      *--     GESTION DES FINS CURSEURS
              WHEN CPGEAL21-CO-RET-1ER-OK AND CPGEAL21-FIN-CURSEUR
                 CONTINUE
              WHEN OTHER
      *--        GESTION DES TRACES
                 IF CP00SV21-TRACE-ACTIVE
                    MOVE '     GESTION-ERREUR-CPGEAL21'
                    TO LI-ID-DON-TRACE
                    SET CP00SV21-NIV-PAR    TO TRUE
                    PERFORM 80000-ALIM-TRACE
                 END-IF

                 MOVE LK-GR-ANO OF LK-CPGEAL21
                   TO LK-GR-ANO OF LK-GR-PPGEMB17

      *       GESTION DES ANOMALIES FONC ET TECHNIQUE
                 PERFORM 23300-TRAITEMENT-ERREUR
           END-EVALUATE
           .                                                            00271800

      *---------------------                                            00269400
       80000-APPEL-PPGEAM20.
      *---------------------                                            00269400
      *--  GESTION DES TRACES                                           00269500
           IF CP00SV21-TRACE-ACTIVE                                     00269600
              MOVE '==>MB17 80000-APPEL-PPGEAM20' TO LI-ID-DON-TRACE    00374700
              SET       CP00SV21-NIV-PAR    TO TRUE                     00269800
              PERFORM   80000-ALIM-TRACE                                00269900
           END-IF                                                       00270000
                                                                        00270100
           CALL PPGEAM20 USING LK-CPGEAM20                              00375200
      *--  GESTION DES RETOURS
           EVALUATE TRUE
      *--     GESTION DES RETOURS OK
              WHEN CPGEAM20-CO-RET-1ER-OK AND CPGEAM20-CO-RET-2ND-OK
                 CONTINUE
              WHEN OTHER
                 IF CP00SV21-TRACE-ACTIVE
                    MOVE '     GESTION-ERREUR-CPGEAM20'
                      TO LI-ID-DON-TRACE
                    SET CP00SV21-NIV-PAR    TO TRUE
                    PERFORM 80000-ALIM-TRACE
                 END-IF
                                                                        00270300
                 MOVE LK-GR-ANO OF LK-CPGEAM20
                   TO LK-GR-ANO OF LK-GR-PPGEMB17

      *--*       GESTION DES ANOMALIES FONC ET TECHNIQUE
                 PERFORM 23300-TRAITEMENT-ERREUR
           END-EVALUATE
           .                                                            00271800

      *---------------------                                            00269400
       80000-APPEL-PPGAAL18.
      *---------------------                                            00269400
      *--  GESTION DES TRACES                                           00269500
           IF CP00SV21-TRACE-ACTIVE                                     00269600
              MOVE '==>MB17 80000-APPEL-PPGAAL18' TO LI-ID-DON-TRACE    00374700
              SET       CP00SV21-NIV-PAR    TO TRUE                     00269800
              PERFORM   80000-ALIM-TRACE                                00269900
           END-IF                                                       00270000
                                                                        00270100
           CALL PPGAAL18 USING LK-CPGAAL18                              00375200
           EVALUATE TRUE
      *--     GESTION DES RETOURS OK
              WHEN CPGAAL18-CO-RET-1ER-OK AND CPGAAL18-CO-RET-2ND-OK
                  CONTINUE
      *--     GESTION DES FINS CURSEURS
              WHEN CPGAAL18-CO-RET-1ER-OK AND CPGAAL18-NON-TROUVE
                 CONTINUE
              WHEN OTHER
      *--        GESTION DES TRACES
                 IF CP00SV21-TRACE-ACTIVE
                    MOVE '     GESTION-ERREUR-CPGAAL18'
                      TO LI-ID-DON-TRACE
                    SET CP00SV21-NIV-PAR    TO TRUE
                    PERFORM 80000-ALIM-TRACE
                 END-IF
                                                                        00270300
                 MOVE LK-GR-ANO OF LK-CPGAAL18
                   TO LK-GR-ANO OF LK-GR-PPGEMB17

      *       GESTION DES ANOMALIES FONC ET TECHNIQUE
                 PERFORM 23300-TRAITEMENT-ERREUR
           END-EVALUATE
           .                                                            00271800

      *-----------------------                                          00379700
       80000-APPELER-PPSP07I1.                                          00379800
      *-----------------------                                          00379900
      *--  GESTION DES TRACES                                           00380000
           IF CP00SV21-TRACE-ACTIVE                                     00380100
              MOVE '==>MB17 80000-APPELER-PPSP07I1' TO LI-ID-DON-TRACE  00380200
              PERFORM 88888-NIV-TRACE-SHOW                              00380400

              STRING 'PARAM MB17[PPSP07-I1]> CO_APPL='
              LI-CO-APPL OF LK-CPSP07I1 ';CO_CRIT='
              LI-CO-CRIT OF LK-CPSP07I1 ';DT_DEFF='
              LI-DT-DEFF OF LK-CPSP07I1 '<'
              DELIMITED BY SIZE INTO LI-ID-DON-TRACE

              PERFORM 88888-NIV-TRACE-SHOW                              00380400
           END-IF                                                       00380500
      *                                                                 00380600
           CALL PPSPSV07 USING LK-CPSP07I1                              00380700
      *                                                                 00380800
           IF CP00SV21-TRACE-ACTIVE                                     00380900
              STRING '-----  RC 1AIRE : '                               00381000
                      LK-CO-RET-1ER-MOD OF LK-CPSP07I1                  00381100
              DELIMITED BY SIZE   INTO LI-ID-DON-TRACE                  00381200
              SET CP00SV21-NIV-SPEC TO TRUE                             00381300
              PERFORM 80000-ALIM-TRACE                                  00381400
      *---                                                              00381500
              STRING '-----  RC 2AIRE : '                               00381600
                      LK-CO-RET-2ND-MOD OF LK-CPSP07I1                  00381700
              DELIMITED BY SIZE   INTO LI-ID-DON-TRACE                  00381800
              SET CP00SV21-NIV-SPEC TO TRUE                             00381900
              PERFORM 80000-ALIM-TRACE                                  00382000
           END-IF                                                       00382100
      *-- GESTION DES RETOURS                                           00382200
           EVALUATE TRUE                                                00382300
      *--  GESTION DES RETOURS OK                                       00382400
           WHEN CPSP07I1-CO-RET-1ER-OK AND CPSP07I1-CO-RET-2ND-OK       00382500
                CONTINUE
      *--  GESTION DES ERREURS TECHNIQUE, FONCTIONNELLE OU AUTRES       00382700
           WHEN OTHER                                                   00382800
      *--     GESTION DES TRACES
              IF CP00SV21-TRACE-ACTIVE
                 MOVE '     GESTION-ERREUR-PPSPAM01'
                   TO LI-ID-DON-TRACE
                 SET CP00SV21-NIV-PAR    TO TRUE
                 PERFORM 80000-ALIM-TRACE
              END-IF

              MOVE LK-GR-ANO OF LK-CPSP07I1
                TO LK-GR-ANO OF LK-GR-PPGEMB17

      *       GESTION DES ANOMALIES FONC ET TECHNIQUE
              PERFORM 23300-TRAITEMENT-ERREUR
           END-EVALUATE                                                 00383100
           .                                                            00383200
      *                                                                 00383300
      ******************************************************************00273500
       80000-ALIM-TRACE-SQLCA.                                          00273600
      *-----------------------                                          00273700
           IF CP00SV21-TRACE-ACTIVE                                     00273800
              SET       CP00SV21-NIV-SQL    TO TRUE                     00273900
              CALL      DSNTIAR             USING WS-LB-SQLCA           00274000
                                                  SQL-ERROR-MESSAGE     00274100
                                                  SQL-ERROR-TEXT-LEN    00274200
              IF RETURN-CODE = ZERO                                     00274300
                 MOVE         1                   TO SQL-ERR-CTR        00274400
                 PERFORM WITH TEST BEFORE                               00274500
                  UNTIL SQL-ERR-CTR > 8                                 00274600
                    MOVE         SQL-ERROR-TEXT (SQL-ERR-CTR)           00274700
                                              TO LI-ID-DON-TRACE        00274800
                    IF NOT (LI-ID-DON-TRACE = SPACES)                   00274900
                       PERFORM      80000-ALIM-TRACE                    00275000
                    END-IF                                              00275100
                    ADD          1                   TO SQL-ERR-CTR     00275200
                  END-PERFORM                                           00275300
              ELSE                                                      00275400
                 MOVE      WS-LB-SQLCA      TO LI-ID-DON-TRACE          00275500
                 PERFORM      80000-ALIM-TRACE                          00275600
              END-IF                                                    00275700
           END-IF                                                       00275800
           .                                                            00275900

      ******************************************************************00276000
      * APPEL AU SERVICE POUR RECUPERER LE LIBELLE DES MESSAGES         00276100
      * D'ERREURS                                                       00276200
      ******************************************************************00276300
       80000-ACCES-LIBELLE.                                             00276400
      *--------------------                                             00276500
      *--  INITIALISATION DE LA ZONE COMMUNE TECHNIQUE ENTRANTE         00276600
           MOVE 'PPGEMB17'          TO LK-ID-PGM-MAI   OF LK-CP00SV17   00276700
                                       LK-ID-PGM-APLT  OF LK-CP00SV17   00276800
           MOVE 'PP00SV17'          TO LK-ID-PGM-APLE  OF LK-CP00SV17   00276900
           MOVE WS-ID-VERS-APP      TO LK-ID-VERS-APLT OF LK-CP00SV17   00277000
           SET CP00SV17-APPEL-BATCH TO TRUE                             00277100
           MOVE WS-DT-FONC          TO LK-DT-FONC-TRT  OF LK-CP00SV17   00277200
           MOVE WS-TI-TRT           TO LK-TI-TRT       OF LK-CP00SV17   00277300
           MOVE WS-ID-UTIL          TO LK-CO-UTIL      OF LK-CP00SV17   00277400
                                                                        00277500
           CALL PP00SV17 USING LK-CP00SV17                              00277600
           .                                                            00277700

      *--------------------*
       88888-NIV-TRACE-SHOW.
      *--------------------*
            SET CP00SV21-NIV-PAR TO TRUE
            PERFORM 80000-ALIM-TRACE
            .

      ******************************************************************00277800
       90000-DISPLAY-ERREUR.                                            00277900
      *---------------------                                            00278000
           DISPLAY 'ZONE ERREUR GR-ANO : ' LK-GR-ANO                    00278100
                                  OF LK-GR-PPGEMB17                     00388100
           DISPLAY 'ID-PGM-ERR          : ' LK-ID-PGM-ERR               00278300
                                  OF LK-GR-PPGEMB17                     00388300
           DISPLAY 'ID-ERR              : ' LK-ID-ERR OF LK-GR-PPGEMB17 00388400
           MOVE LK-CO-RET-1ER-INI OF LK-GR-PPGEMB17                     00388500
                                      TO WS-CO-RET-EDIT                 00278700
           DISPLAY 'RET-1ER-INI         : ' WS-CO-RET-EDIT              00278800
           MOVE LK-CO-RET-1ER-MOD OF LK-GR-PPGEMB17                     00388800
                                      TO WS-CO-RET-EDIT                 00279000
           DISPLAY 'RET-1ER-MOD         : ' WS-CO-RET-EDIT              00279100
           MOVE LK-CO-RET-2ND-INI OF LK-GR-PPGEMB17                     00389100
                                      TO WS-CO-RET-EDIT                 00279300
           DISPLAY 'RET-2ND-INI         : ' WS-CO-RET-EDIT              00279400
           MOVE LK-CO-RET-2ND-MOD OF LK-GR-PPGEMB17                     00389400
                                      TO WS-CO-RET-EDIT                 00279600
           DISPLAY 'RET-2ND-MOD         : ' WS-CO-RET-EDIT              00279700
           MOVE LK-CO-SQL         OF LK-GR-PPGEMB17                     00389700
                                      TO WS-CO-RET-EDIT                 00279900
           DISPLAY 'CO-SQL              : ' WS-CO-RET-EDIT              00280000
           .                                                            00280100

      *-----------------------------*
       99999-TRT-M-SHOW01.                                              00119500
      *-----------------------------*
      *--  GESTION DES TRACES
           IF CP00SV21-TRACE-ACTIVE
              STRING 'WS-ID-CLE-ANO(1:13):' WS-ID-CLE-ANO(1:13)
              'WS-GR-CLE-EDF:' WS-GR-CLE-EDF
              DELIMITED BY SIZE
              INTO LI-ID-DON-TRACE
              PERFORM 88888-NIV-TRACE-SHOW
           END-IF
           .
M017+ *------------------------*
M017+  23331-INTERCEPT-RC8-BREF.
M017+ *------------------------*
M017+      IF LK-CO-MSG-MOD OF WS-GR-ERR = 'RCO0000641'
M017+         MOVE +4 TO LK-CO-RET-1ER-MOD  OF WS-GR-ERR
M017+      END-IF
M017+      .

M018+ *------------------------*
M018+  23332-INTERCEPT-RC8-REM.
M018+ *------------------------*
M018+ *    gestion probleme sur relation mandataire
M018+      IF LK-ID-PGM-APEL-ERR OF WS-GR-ERR = 'PPSPSV12'
M018+      AND LK-ID-PGM-ERR OF WS-GR-ERR = 'PPGAAL18'
M018+      AND LK-CO-SQL OF WS-GR-ERR = -811
M018+         MOVE +4 TO LK-CO-RET-1ER-MOD  OF WS-GR-ERR
M018+      END-IF
M018+      .


M015+ *------------------------*
M015+  99999-ALIM-DON-SHOW.
M015+ *------------------------*
M015+      IF CP00SV21-TRACE-ACTIVE                                     00158000
M015+         STRING 'MB17 :WS-CO-STUT-STEP='WS-CO-STUT-STEP ';'        00158100
M015+         'WS-ID-CLE-RPRI-SAUV:'WS-ID-CLE-RPRI-SAUV(1:13) ';'
M015+         'WS-ID-CLE-RPRI:'WS-ID-CLE-RPRI(1:13) ';'
M015+         DELIMITED BY SIZE INTO LI-ID-DON-TRACE
M015+         PERFORM 88888-NIV-TRACE-SHOW                              00158300
              string 'MB17:'
M015+         'WS-ID-CLE-ANO-SAUV:'WS-ID-CLE-ANO-SAUV(1:13) ';'
M015+         'WS-ID-CLE-ANO:'WS-ID-CLE-ANO(1:13) ';'
M015+         DELIMITED BY SIZE INTO LI-ID-DON-TRACE
M015+         PERFORM 88888-NIV-TRACE-SHOW                              00158300
M015+      END-IF                                                       00158400
M015+      .
