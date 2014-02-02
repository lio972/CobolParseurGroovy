00001  IDENTIFICATION DIVISION.                                         13/02/02
00002  PROGRAM-ID.                     FB01T00.                         EQW9ZWFZ
00003 * MODULE EN COURS DE MAINTENANCE PAR   : ...................... !    LV001
00004 * PREVU POUR ETRE MIS EN PRODUCTION LE : ../../..               ! EQW9ZWFZ
00005 * N.B..: ...................................................... ! EQW9ZWFZ
00006 *---------------------------------------------------------------+ EQW9ZWFZ
00007 * NOM DU LOAD MODULE : FB01T00                CREE LE 01/02/02    EQW9ZWFZ
00008 * NOM DE TRANSACTION : FB01                        A 13:58        EQW9ZWFZ
00009 * NOM DE MAP         : FB01M0Z                                    EQW9ZWFZ
00010 * NOM DU PGM BATCH   : ........ (SI MODULE COMMUN TP/BATCH)       EQW9ZWFZ
00011 * AUTEUR             : B.BAURËS                                   EQW9ZWFZ
00012 * LOGON TSO          : ETDA4                                      EQW9ZWFZ
00013 *                                                                 EQW9ZWFZ
00014 * TITRE              : COMPOSITION DU CONTRAT                     EQW9ZWFZ
00015 *---------------------------------------------------------------+ EQW9ZWFZ
00016 * CNC$PROG MAINT     : TK    090990 AJOUT TRAITEMENT TS PLAN      EQW9ZWFZ
00017 *                    :              POUR  EXIT DE SELECTION DYNA  EQW9ZWFZ
00018 *   09/09/90         :              MIQUE DE PLAN                 EQW9ZWFZ
00019 *                    : POUR TOUTE   CONVERSATION (SAUF MENU)      EQW9ZWFZ
00020 *---------------------------------------------------------------+ EQW9ZWFZ
00021 * CNC$PROG MAINT     : TK    AJOUT ++INCLUDE SQKCPLDE             EQW9ZWFZ
00022 *                    :       AJOUT APPEL DELETE-TS-PLAN DANS      EQW9ZWFZ
00023 *  09-10-90          :       SORTIE-LEVEL-(SUP/MAX/SIGNATURE)     EQW9ZWFZ
00024 *                    :       ET ABANDON-TACHE (SQKCMERO)          EQW9ZWFZ
00025 *                    : POUR TOUTE   CONVERSATION (SAUF MENU)      EQW9ZWFZ
00026 *---------------------------------------------------------------+ EQW9ZWFZ
00027 *                          >> BUT <<                              EQW9ZWFZ
00028 * .............................................................   EQW9ZWFZ
00029 *                                                               ! EQW9ZWFZ
00030 *                       >> SYNOPTIQUE <<                        ! EQW9ZWFZ
00031 *----------+-------+--------------------------------------------+ EQW9ZWFZ
00032 * RESSOURCE! M/O/A !               DESCRIPTION                  ! EQW9ZWFZ
00033 *----------+-------+--------------------------------------------+ EQW9ZWFZ
00034 *          !       !                                            ! EQW9ZWFZ
00035 *----------+-------+----------+---------------------------------+ EQW9ZWFZ
00036 * PROGR.   ! MODE  ! COMMAREA !        DESCRIPTION              ! EQW9ZWFZ
00037 * APPELANT ! APPEL ! TRANSMISE!                                 ! EQW9ZWFZ
00038 *----------+-------+----------+---------------------------------+ EQW9ZWFZ
00039 *          !       !          !                                 ! EQW9ZWFZ
00040 *----------+-------+----------+---------------------------------+ EQW9ZWFZ
00041 * PROGR.   ! MODE  ! COMMAREA !        DESCRIPTION              ! EQW9ZWFZ
00042 * APPELE   ! APPEL ! TRANSMISE!                                 ! EQW9ZWFZ
00043 *----------+-------+----------+---------------------------------+ EQW9ZWFZ
00044 *          !       !          !                                 ! EQW9ZWFZ
00045 *----------+-------+----------+---------------------------------+ EQW9ZWFZ
00046 * ERREUR   !             MESSAGE OU TRAITEMENT                  ! EQW9ZWFZ
00047 *----------+----------------------------------------------------+ EQW9ZWFZ
00048 *          !                                                    ! EQW9ZWFZ
00049 *----------+----------------------------------------------------+ EQW9ZWFZ
00050 *             >> STRUCTURE GENERALE DU PROGRAMME <<             ! EQW9ZWFZ
00051 * ............................................................. ! EQW9ZWFZ
00052 * ............................................................. ! EQW9ZWFZ
00053 *                                                               ! EQW9ZWFZ
00054 *                       >> MAINTENANCE <<                       ! EQW9ZWFZ
00055 *-------------+-------------------------------------------------+ EQW9ZWFZ
00056 * DATE/AUTEUR !         DESCRIPTION DE LA MODIFICATION          ! EQW9ZWFZ
00057 *-------------+-------------------------------------------------+ EQW9ZWFZ
00058 * 99/99/99    !                                                 ! EQW9ZWFZ
00059 * AUTEUR      !                                                 ! EQW9ZWFZ
00060 * LEVEL       !                                                 ! EQW9ZWFZ
00061 *-------------+-------------------------------------------------+ EQW9ZWFZ
00058 * 03/05/2002  ! FD F1936: LE CONTROLE DU NIVEAU D'HABILITATION  ! EQW9ZWFZ
00059 * C.LANDRODIE !           POUR ACCES A CRAN FB11 EST SUPPRIME.  ! EQW9ZWFZ
00060 *             !           IL SE FAIT DANS FB11T00               ! EQW9ZWFZ
00061 *-------------+-------------------------------------------------+ EQW9ZWFZ
00058 * 02/09/2002  ! U3137   : REMISE EN COURS DES GTIES LORS DE LA  ! EQW9ZWFZ
00059 * B.BAURES    !           RESTAURATION D'UN VÈHICULE.           ! EQW9ZWFZ
      *-------------+-------------------------------------------------+
00066 * 30/07/2002  ! FD 23216 : OPTION CHANGEMENT DE VÈHICULE        ! FB01T00B
00067 * B.BAURES    !                                                 ! FB01T00B
      *-------------+-------------------------------------------------+
00066 * 15/11/2002  ! FD 22110 : PB ALIMENTATION CIGAL                ! FB01T00B
00067 * A.BERNARD   !                                                 ! FB01T00B
      *-------------+-------------------------------------------------+
00066 * 14/02/2003  ! FD 27833 : VISUALISATION DES DONNÈES PERSONNES  ! FB01T00B
00067 * C.LANDRODIE ! ET VÈHICULE POUR LES SUSPENS REPRIS POUR AC     ! FB01T00B
      *-------------+-------------------------------------------------+
U3319 * 22/04/2003  ! FD U3319 : EXT.AACONV + MODIF ALIM. BANDEAU     ! EFU4RQMP
U3319 * O.LASEIGNE  !            CLIENT                               ! EFU4RQMP
      *-------------+-------------------------------------------------+
U3796 * 30/04/2003  ! FD U3796 : ANOMALIE RECTIFICATIF                ! EFU4RQMP
U3796 * R. SANCHEZ  !                                                   EFU4RQMP
U3796 *-------------+-------------------------------------------------+
U3402 * 26/09/2003  ! FD U3402 : ANOMALIE SUR CTRL USAGE PROF INCOMPA-! EFU4RQMP
U3402 * A. BERNARD  !            TIBLES, MISE A BLANC PRMTYPC         ! EFU4RQMP
U3796 *-------------+-------------------------------------------------+
F2980 * 03/12/2003  ! FD 32980 : EVOLUTION HORS SERIE PRO             ! EFU4RQMP
F2980 * B. MELLON   !            RECHERCHE VDR = E                    ! EFU4RQMP
F2980 *-------------+-------------------------------------------------+
F3576 * 14/11/2003  ! FD 33576 : EVOLUTION MESURES DE SOUSCRIPTION    !
F3576 * A. LEMAITRE !            RECOMPILATION POUR PRISE EN COMPTE   !
F3576 *             !            DE LA NOUVELLE VERSION DE L'INCLUDE  !
F3576 *             !            FBIVEHI                              !
F3576 *-------------+-------------------------------------------------+
F37105* 17/03/2004  ! FD 37105 : EVOLUTION MESURES AUTOMOBILES        ! EFU4RQMP
F37105* B. MELLON   !            REGLES DES BUTOIRS                   ! EFU4RQMP
F37105*-------------+-------------------------------------------------+
F37105* 25/03/2004  ! FD 37105 : - ANO ALIMENTATION ENSP/ENAP         ! EFU4RQMP
F37105* B. CARDON   !            - ANO COPTYPC = 1                    ! EFU4RQMP
F37105*-------------+-------------------------------------------------+
36835 * 25/06/2004  ! FD 36835 : MISE A NIVEAU CONVERGENES            !
36835 * D CARDON    !            CARTE GRISE --> RECOMPIL             !
36835 *-------------+-------------------------------------------------+
DELTS * B PORTEFAIX ! 30/12/2004 AJOUT INCLUDE DELETE TS              !
      *-------------+-------------------------------------------------+
U9949 * D CARDON    ! 03/07/2006 VARIABLE ENERGIE NON REDETERMINEE    !
U9949 *             !  EN CHANGEMENT DE VEHICULE                      !
U9949 *-------------+-------------------------------------------------+
00062 *                       *************                           ! EQW9ZWFZ
00063 *                       ** LEGENDE **                           ! EQW9ZWFZ
00064 *                       *************                           ! EQW9ZWFZ
00065 *                                                               ! EQW9ZWFZ
00066 * RESSOURCE       : FICHIER , BASE , TS                         ! EQW9ZWFZ
00067 *                   - INDIQUER NOM COBOL ET DDNAME SI DIFFERENT ! EQW9ZWFZ
00068 *                   - SI POSSIBLE CODE DU DICTIONAIRE           ! EQW9ZWFZ
00069 *                   - DONNER PSB SI BASE                        ! EQW9ZWFZ
00070 *                                                               ! EQW9ZWFZ
00071 * M/O/A           : MODE / ORGANISATION / ACCES                 ! EQW9ZWFZ
00072 *                    !         !           !                    ! EQW9ZWFZ
00073 *                    !         !           +---> D = DIRECT     ! EQW9ZWFZ
00074 *                    !         !                 S = SEQUENTIEL ! EQW9ZWFZ
00075 *                    !         +---> S = SEQUENTIELLE           ! EQW9ZWFZ
00076 *                    !               V = VSAM                   ! EQW9ZWFZ
00077 *                    !               X = AUTRE                  ! EQW9ZWFZ
00078 *                    +---> L = LECTURE                          ! EQW9ZWFZ
00079 *                          E = ECRITURE                         ! EQW9ZWFZ
00080 *                          M = MISE A JOUR                      ! EQW9ZWFZ
00081 *                                                               ! EQW9ZWFZ
00082 * MODE D'APPEL    : CALL , XCTL , LINK                          ! EQW9ZWFZ
00083 *                                                               ! EQW9ZWFZ
00084 * COMMAREA TRANS. : - NOM DU NIVEAU "01"                        ! EQW9ZWFZ
00085 *                   - LONGUEUR DE LA COMMAREA                   ! EQW9ZWFZ
00086 *                                                               ! EQW9ZWFZ
00087 * MAINTENANCE     : LE LEVEL D'UNE MAINTENANCE CORRESPOND AU 1ER! EQW9ZWFZ
00088 *                   N∞ DE LEVEL D'INSTRUCTION CORRESPONDANT A   ! EQW9ZWFZ
00089 *                   CETTE MAINTENANCE.                          ! EQW9ZWFZ
00090 *===============================================================+ EQW9ZWFZ
00090 * 21062002     *  F22110 : HS PHASE 2  NE PLUS DÈCLENCHER LE    + EQW9ZWFZ
00090 * A SOULOUMIAC *  CONTROLE SUR MOTIF  SI VEHI OU PERS SORTIS    + EQW9ZWFZ
00090 *              *  LORS D'UN MOUVEMENT PRÈCEDENT                 + EQW9ZWFZ
00090 *===============================================================+ EQW9ZWFZ
00091  ENVIRONMENT DIVISION.                                            EQW9ZWFZ
00092  CONFIGURATION SECTION.                                           EQW9ZWFZ
00093  SPECIAL-NAMES.                                                   EQW9ZWFZ
00094      DECIMAL-POINT IS COMMA.                                      EQW9ZWFZ
00095  DATA DIVISION.                                                   EQW9ZWFZ
00096  WORKING-STORAGE SECTION.                                         EQW9ZWFZ
00097 *                                                                 EQW9ZWFZ
00098 ***************************************************************** EQW9ZWFZ
00099 *   ZONES DE PILOTAGE DU SQUELETTE                              * EQW9ZWFZ
00100 *   ATTENTION: CE DOIT ETRE LE PREMIER INCLUDE                  * EQW9ZWFZ
00101 ***************************************************************** EQW9ZWFZ
00102  ++INCLUDE SQKWDV0B                                               EQW9ZWFZ
U3319  01  IA-SAUVE               PIC S9(3) COMP-3   VALUE +0.
00103 /                                                                 EQW9ZWFZ
00104 ***************************************************************** EQW9ZWFZ
00105 * COMMAREA POUR APPEL D'INTERFACE DE CONFIDENTIALITE (AUAAL00)  * EQW9ZWFZ
00106 ***************************************************************** EQW9ZWFZ
00107  01  AUAAC.                                                       EQW9ZWFZ
00108  ++INCLUDE AUAAC                                                  EQW9ZWFZ
00109 /                                                                 EQW9ZWFZ
00110 ***************************************************************** EQW9ZWFZ
00111 *   IDENTIFICATION DES TABLES SPI POUR ACCES AUX TABLES         * EQW9ZWFZ
00112 *   DE TYPE MENU OU CONVERSATION                                * EQW9ZWFZ
00113 ***************************************************************** EQW9ZWFZ
00114  01  IDENT-TABLE.                                                 EQW9ZWFZ
00115      05  TABLE-PREF         PIC X(06).                            EQW9ZWFZ
00116      05  TABLE-SUFF         PIC X(02).                            EQW9ZWFZ
00117 ***************************************************************** EQW9ZWFZ
00118 *   IDENTIFICATION DE LA TS DE CONFIDENTIALITE                  * EQW9ZWFZ
00119 ***************************************************************** EQW9ZWFZ
00120  01  IDENT-TS-CONF.                                               EQW9ZWFZ
00121      05  CONF-TS-PREF       PIC X(04).                            EQW9ZWFZ
00122      05  CONF-TS-SUFF.                                            EQW9ZWFZ
00123          10  CONF-TS-CONV   PIC X(03).                            EQW9ZWFZ
00124          10  FILLER         PIC X(01) VALUE '1'.                  EQW9ZWFZ
00125 ***************************************************************** EQW9ZWFZ
00126 *   IDENTIFICATION DE LA TS APPLICATIVE                         * EQW9ZWFZ
00127 ***************************************************************** EQW9ZWFZ
00128  01  IDENT-TS-APP.                                                EQW9ZWFZ
00129      05  APP-TS-PREF        PIC X(04).                            EQW9ZWFZ
00130      05  APP-TS-SUFF.                                             EQW9ZWFZ
00131          10  APP-TS-CONV    PIC X(03).                            EQW9ZWFZ
00132          10  FILLER         PIC X(01) VALUE '1'.                  EQW9ZWFZ
00133 ****************************************************************  EQW9ZWFZ
00134 *   IDENTIFICATION DE LA TS CONTRAT GENERALE                      EQW9ZWFZ
00135 ****************************************************************  EQW9ZWFZ
00136  01  IDENT-TS-CNTPROD.                                            EQW9ZWFZ
00137      05  TS-CNTPROD-EIBTRMID PIC X(04).                           EQW9ZWFZ
00138      05  FILLER              PIC X(03) VALUE 'POL'.               EQW9ZWFZ
00139      05  TS-CNTPROD-NUM      PIC X(01).                           EQW9ZWFZ
00140 ***************************************************************** EQW9ZWFZ
00141 *   IDENTIFICATION DE LA TS DE PAGINATION                       * EQW9ZWFZ
00142 ***************************************************************** EQW9ZWFZ
00143  01  IDENT-TS-PAGE.                                               EQW9ZWFZ
00144      05  PAGE-TS-PREF       PIC X(04).                            EQW9ZWFZ
00145      05  PAGE-TS-SUFF.                                            EQW9ZWFZ
00146          10  PAGE-TS-CONV   PIC X(03).                            EQW9ZWFZ
00147          10  FILLER         PIC X(01) VALUE '1'.                  EQW9ZWFZ
00148 ***************************************************************** EQW9ZWFZ
00149 *   IDENTIFICATION DE LA TS TECHNIQUE                           * EQW9ZWFZ
00150 ***************************************************************** EQW9ZWFZ
00151  01  IDENT-TS-TECHNIQUE.                                          EQW9ZWFZ
00152      05  TS-TECHNIQUE-EIBTRMID PIC X(04).                         EQW9ZWFZ
00153      05  FILLER                PIC X(03) VALUE 'TEC'.             EQW9ZWFZ
00154      05  TS-TECHNIQUE-NUM      PIC X(01) VALUE '1'.               EQW9ZWFZ
00155 /                                                                 EQW9ZWFZ
00156 ********************************************************          EQW9ZWFZ
00157 *       DESCRIPTION DE LA TS DE PAGINATION                        EQW9ZWFZ
00158 ********************************************************          EQW9ZWFZ
00159 *                                                                 EQW9ZWFZ
00160  ++INCLUDE SQKWTSPY                                               EQW9ZWFZ
00161 /                                                                 EQW9ZWFZ
00162 ******************* POUR CONVERSATION *************************** EQW9ZWFZ
00163 *TK0909 POUR EXIT-SELECTION-DE-PLAN : DESCRIPTION DE LA TS        EQW9ZWFZ
00164 ***************************************************************** EQW9ZWFZ
00165  ++INCLUDE SQKWPLTS                                               EQW9ZWFZ
00166 /                                                                 EQW9ZWFZ
00167 ***************************************************************** EQW9ZWFZ
00168 *   DESCRIPTION DE LA TS DE CONFIDENTIALITE  CONVERSATION       * EQW9ZWFZ
00169 ***************************************************************** EQW9ZWFZ
00170  01  AUAAIW.                                                      EQW9ZWFZ
00171  ++INCLUDE AUAAIW                                                 EQW9ZWFZ
DELTS *
DELTS *  TABLE DES TS A DELETER
DELTS  ++INCLUDE CCMADLTS
00172 /                                                                 EQW9ZWFZ
00173 ***************************************************************** EQW9ZWFZ
00174 *   DESCRIPTION DE LA TABLE DES CONVERSATIONS (SPITAB)          * EQW9ZWFZ
00175 ***************************************************************** EQW9ZWFZ
00176  ++INCLUDE CCAACONV                                               EQW9ZWFZ
U3319  ++INCLUDE CCAACON2                                               EFUTSUGF
00177 /                                                                 EQW9ZWFZ
00178 ***************************************************************** EQW9ZWFZ
00179 *   DECRIRE   ICI   LES   ZONES   SPECIFIQUES   AU   PROGRAMME  * EQW9ZWFZ
00180 ***************************************************************** EQW9ZWFZ
00181 *** DESCRIPTION DE LA TS SUSPENS***                               EQW9ZWFZ
00182 * LONGUEUR DE LA TS SUSPENS                                       EQW9ZWFZ
00183  ++INCLUDE MAILONG                                                EQW9ZWFZ
00184 *                                                                 EQW9ZWFZ
00185  01 TS-SUSPENS1.                                                  EQW9ZWFZ
00186  ++INCLUDE MASP                                                   EQW9ZWFZ
00187                                                                   EQW9ZWFZ
00188 *                                                                 EQW9ZWFZ
00189  01 TS-SUSPENS2.                                                  EQW9ZWFZ
00190  ++INCLUDE MASP                                                   EQW9ZWFZ
00191                                                                   EQW9ZWFZ
00192 *** DESCRIPTION DE L'ORG REFONTE AUTOMOBILE GFA ***               EQW9ZWFZ
00193 *-----------                                                      EQW9ZWFZ
00194 * TS CONTRAT                                                      EQW9ZWFZ
00195  01 TS-CNTPROD.                                                   EQW9ZWFZ
00196  ++INCLUDE FBICONT                                                EQW9ZWFZ
00197 *------------                                                     EQW9ZWFZ
00198 * TS PERSONNE                                                     EQW9ZWFZ
00199  01 TS-PERSONNE.                                                  EQW9ZWFZ
00200  ++INCLUDE FBIPERS                                                EQW9ZWFZ
00201 *------------                                                     EQW9ZWFZ
00202 * TS VEHICULE                                                     EQW9ZWFZ
00203  01 TS-VEHICULE.                                                  EQW9ZWFZ
00204  ++INCLUDE FBIVEHI                                                EQW9ZWFZ
00205                                                                   EQW9ZWFZ
00202 * TS VEHICULE CHANGER                                             EQW9ZWFZ
00203  01 TS-VEHICULE-CHANGER.                                          EQW9ZWFZ
00204  ++INCLUDE FBIVEHI                                                EQW9ZWFZ
00205                                                                   EQW9ZWFZ
00206 *** DESCRIPTION DE LA TS TECHNIQUE ***                            EQW9ZWFZ
00207 * TS TECHNIQUE                                                    EQW9ZWFZ
00208  01 TS-TECHNIQUE.                                                 EQW9ZWFZ
00209  ++INCLUDE FBITECH                                                EQW9ZWFZ
00210                                                                   EQW9ZWFZ
00211 *** DESCRIPTION DU SEGMENT TRANSIT ***                            EQW9ZWFZ
00212 * TS TRANSIT                                                      EQW9ZWFZ
00213  01 FBMISPTR-IT1.                                                 EQW9ZWFZ
00214  ++INCLUDE FBMISPTR                                               EQW9ZWFZ
00215                                                                   EQW9ZWFZ
00216 * TS TRANSIT                                                      EQW9ZWFZ
00217  01 FBMISPTR-IT2.                                                 EQW9ZWFZ
00218  ++INCLUDE FBMISPTR                                               EQW9ZWFZ
00219                                                                   EQW9ZWFZ
00220 *** ZONES NECESSAIRES AU MODULE DES CONTROLES TECHNIQUES          EQW9ZWFZ
00221  01 MAI90C00.                                                     EQW9ZWFZ
00222  ++INCLUDE MAI90C00                                               EQW9ZWFZ
00223                                                                   EQW9ZWFZ
00224 *** ZONES NECESSAIRES AU MODULE DES CONTROLES HABILITATION        EQW9ZWFZ
00225  01 MAI90C20.                                                     EQW9ZWFZ
00226  ++INCLUDE MAI90C20                                               EQW9ZWFZ
00227                                                                   EQW9ZWFZ
00228 * ZONES TRAVAIL POUR CONTROLE D'ACCES                             EQW9ZWFZ
00229  01  Z-CONTROLE-ACCES.                                            EQW9ZWFZ
00230      05  Z-CODE-REGIME-AS     PIC X(05).                          EQW9ZWFZ
00231      05  Z-CODE-REGIME-CA     PIC X(05).                          EQW9ZWFZ
00232      05  Z-COMPTEUR-TS        PIC S9(2) COMP.                     EQW9ZWFZ
00233      05  Z-AFFICHAGE-SELECTIF PIC X(03).                          EQW9ZWFZ
00234      05  Z-AFFICHER-OPTION    PIC X(03).                          EQW9ZWFZ
00235 ***************************************************************** EQW9ZWFZ
00236 * ZONE DE COMMUNICATION DU MODULE DE DETERMINATION DES DONNEES  * EQW9ZWFZ
00237 ***************************************************************** EQW9ZWFZ
00238  01 FBI90C11.                                                     EQW9ZWFZ
00239  ++INCLUDE FB90C11                                                EQW9ZWFZ
00240 *                                                                 EQW9ZWFZ
00241 ***************************************************************** EQW9ZWFZ
00242 *   ZONES GENERALES OBLIGATOIRES                                * EQW9ZWFZ
00243 ***************************************************************** EQW9ZWFZ
00244 *   ZONES DE TEST DU CODE-RETOUR CICS  :  EIBRCODE                EQW9ZWFZ
00245  ++INCLUDE SQKWEIB0                                               EQW9ZWFZ
00246 ***************************************************************** EQW9ZWFZ
00247 * ZONES DATE/HEURE ET NOM DE TERMINAL/CODE TRANSACTION            EQW9ZWFZ
00248 ***************************************************************** EQW9ZWFZ
00249  ++INCLUDE SQKWDATH                                               EQW9ZWFZ
00250 ***************************************************************** EQW9ZWFZ
00251 * ZONES BMS     (TOUCHES FONCTION ET ATTRIBUTS)                 * EQW9ZWFZ
00252 ***************************************************************** EQW9ZWFZ
00253 *                                                               * EQW9ZWFZ
00254 *                 DEFINITION DES ATTRIBUTS                      * EQW9ZWFZ
00255 *         MDT ON                             MDT OFF            * EQW9ZWFZ
00256 *                                                               * EQW9ZWFZ
00257 *        BRT-ALP-FSET                        BRT-ALP            * EQW9ZWFZ
00258 *        BRT-NUM-FSET                        BRT-NUM            * EQW9ZWFZ
00259 *        BRT-PRO-FSET                        BRT-PRO            * EQW9ZWFZ
00260 *        DRK-ALP-FSET                        DRK-ALP            * EQW9ZWFZ
00261 *        DRK-PRO-FSET                        DRK-PRO            * EQW9ZWFZ
00262 *        DRK-PRO-RSET                        DRK-RSET           * EQW9ZWFZ
00263 *        NOR-ALP-FSET                        NOR-ALP            * EQW9ZWFZ
00264 *        NOR-NUM-FSET                        NOR-NUM            * EQW9ZWFZ
00265 *        NOR-PRO-FSET                        NOR-PRO            * EQW9ZWFZ
00266 ***************************************************************** EQW9ZWFZ
00267 *300890    ++INCLUDE SQKWECRA  A: SANS PF9,  B: AVEC  PF9         EQW9ZWFZ
00268  ++INCLUDE SQKWECRA                                               EQW9ZWFZ
00269 ***************************************************************** EQW9ZWFZ
00270 *   ZONES DE CONTROLE ET DE TRAITEMENT SPECIFIQUES              * EQW9ZWFZ
00271 ***************************************************************** EQW9ZWFZ
00272  01 W-GESCLI.                                                     EQW9ZWFZ
00273     05 W-GES       PIC X(06).                                     EQW9ZWFZ
00274     05 W-CLI       PIC X(05).                                     EQW9ZWFZ
00275                                                                   EQW9ZWFZ
00276  01 WSS-TABLEAU-AFF.                                              EQW9ZWFZ
00277     05 WSS-TAB-AFF OCCURS 83.                                     EQW9ZWFZ
00278        10 WSS-TYPTS     PIC X(1).                                 EQW9ZWFZ
00279        10 WSS-RANTS     PIC S9(4) COMP.                           EQW9ZWFZ
00291        10 WSS-RESTAU    PIC X(1).                                 FB01T00B
00292        10 WSS-CHANGE    PIC X(1).                                 FB01T00B
00294        10 WSS-CODE-ACT  PIC X(1).                                 FB01T00B
00280        10 WSS-LIGNE-TABLEAU-AFF.                                  EQW9ZWFZ
00281           15 WSS-SELCOD PIC X(1).                                 EQW9ZWFZ
00282           15 WSS-IDENT  PIC X(30).                                EQW9ZWFZ
00283           15 WSS-DATENT PIC X(8).                                 EQW9ZWFZ
00284           15 WSS-DATSOR PIC X(8).                                 EQW9ZWFZ
00285           15 WSS-MOTIFC PIC X(1).                                 EQW9ZWFZ
00286           15 WSS-STATYP PIC X(4).                                 EQW9ZWFZ
00287           15 WSS-PTSCRM PIC X(3).                                 EQW9ZWFZ
00288                                                                   EQW9ZWFZ
00289  01 WSS-LIGNE-TIRET.                                              EQW9ZWFZ
00290     05 FILLER        PIC X(1).                                    EQW9ZWFZ
00291     05 FILLER        PIC X(54) VALUE ALL '-'.                     EQW9ZWFZ
00292                                                                   EQW9ZWFZ
00293  01  WSS-IND-TS     PIC 999 VALUE 0.                              EQW9ZWFZ
00293  01  IND-GTI        PIC 999 VALUE 0.                              EQW9ZWFZ
00294                                                                   EQW9ZWFZ
00295  01 I                PIC S9(3) COMP-3.                            EQW9ZWFZ
00296  01 J                PIC S9(3) COMP-3.                            EQW9ZWFZ
00297  01 IB               PIC S9(3) COMP-3.                            EQW9ZWFZ
00298  01 IC               PIC S9(3) COMP-3.                            EQW9ZWFZ
00299  01 IE               PIC S9(3) COMP-3.                            EQW9ZWFZ
00300  01 IG               PIC S9(3) COMP-3.                            EQW9ZWFZ
00316  01 IR               PIC S9(3) COMP-3.                            FB01T00B
00301  01 I-TSTECH         PIC S9(4) COMP.                              EQW9ZWFZ
00301  01 WSS-RANG-TS-CHANGER PIC S9(4) COMP.                           EQW9ZWFZ
00301  01 I-PERS           PIC S9(4) COMP.                              EQW9ZWFZ
00302  01 I-VEHI           PIC S9(4) COMP.                              EQW9ZWFZ
00303  01 I-VEHI-CIGAL     PIC S9(4) COMP.                              EQW9ZWFZ
00320  01 I-COV            PIC S9(4) COMP.                              FB01T00B
F2980  01 I-GTI-CODE       PIC S9(4) COMP.
00304  01 WSS-FIN-PERS     PIC X.                                       EQW9ZWFZ
00305  01 WSS-FIN-VEHI     PIC X.                                       EQW9ZWFZ
00305  01 WSS-FIN-VEHI-CHG PIC X.                                       EQW9ZWFZ
00306  01 WSS-IDENTIFIANT  PIC X(30).                                   EQW9ZWFZ
00307  01 WSS-NOM          PIC X(20).                                   EQW9ZWFZ
00308  01 WSS-IMMAT        PIC X(10).                                   EQW9ZWFZ
00309  01 WSS-APPEL-AIDE-AJTVEHI  PIC X.                                EQW9ZWFZ
00310  01 WSS-APPEL-AIDE-MOTIF    PIC X.                                EQW9ZWFZ
F37105 01 WSS-TOP-PERMIS   PIC X.                                       EQW9ZWFZ
00311  01 WSS-COEF-ENTIER  PIC 9(03).                                   EQW9ZWFZ
00312                                                                   EQW9ZWFZ
00313  01 WSS-SSAAMMJJ.                                                 EQW9ZWFZ
00314     05 WSS-SSAA      PIC X(4).                                    EQW9ZWFZ
00315     05 WSS-MM        PIC X(2).                                    EQW9ZWFZ
00316     05 WSS-JJ        PIC X(2).                                    EQW9ZWFZ
00317  01 WSS-JJMMSSAA.                                                 EQW9ZWFZ
00318     05 WSS-JJ        PIC X(2).                                    EQW9ZWFZ
00319     05 WSS-MM        PIC X(2).                                    EQW9ZWFZ
00320     05 WSS-SSAA      PIC X(4).                                    EQW9ZWFZ
00321                                                                   EQW9ZWFZ
00322  01 SELECTION-CODE PIC X.                                         EQW9ZWFZ
00323     88 NO-SELECTION  VALUE '0'.                                   EQW9ZWFZ
00324     88 SELECTION-1   VALUE '1'.                                   EQW9ZWFZ
00325     88 SELECTION-2   VALUE '2'.                                   EQW9ZWFZ
00326                                                                   EQW9ZWFZ
00327  01 RANG-TS-PERS   PIC S9(4) COMP VALUE +0.                       EQW9ZWFZ
00328  01 RANG-TS-VEHI   PIC S9(4) COMP VALUE +0.                       EQW9ZWFZ
00329  01 RANG-TS-TECH   PIC S9(4) COMP VALUE +0.                       EQW9ZWFZ
00330 ***************************************************************** EQW9ZWFZ
00331 *   LONGUEUR DE LA COMMAREA                                     * EQW9ZWFZ
00332 ***************************************************************** EQW9ZWFZ
00333  01  COM-GENE-LONG-COMMAREA           PIC S9(4) COMP VALUE +4096. EQW9ZWFZ
00334 *                                                                 EQW9ZWFZ
00335 ***************************************************************** EQW9ZWFZ
00336 *   ZONES DE COMMAREA POUR APPEL A SPITAB                       * EQW9ZWFZ
00337 ***************************************************************** EQW9ZWFZ
00338  01  XSPIPARM.                                                    EQW9ZWFZ
00339  ++INCLUDE SPIPARTP                                               EQW9ZWFZ
00340 /                                                                 EQW9ZWFZ
00341 *                                                                 EQW9ZWFZ
00342 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* EQW9ZWFZ
00343 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* EQW9ZWFZ
00344 *   COMMAREA GENERALE DES APPLICATIONS CONCORDE ( SQKWCOMM )    * EQW9ZWFZ
00345 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* EQW9ZWFZ
00346 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* EQW9ZWFZ
00347  ++INCLUDE SQKWCOMM                                               EQW9ZWFZ
00348  ++INCLUDE MAICOMM                                                EQW9ZWFZ
00349  ++INCLUDE FBICOMM                                                EQW9ZWFZ
00350 *                                                                 EQW9ZWFZ
00351 ***************************************************************** EQW9ZWFZ
00352 *    ZONES DE LA MAP  FB01M0                                      EQW9ZWFZ
00353 ***************************************************************** EQW9ZWFZ
00354  01  FILLER  PIC X(16) VALUE '*** MAP FB01 ***'.                  EQW9ZWFZ
00355 *                                                                 EQW9ZWFZ
00356  01  Z-MAP.                                                       EQW9ZWFZ
00357 *                                                                 EQW9ZWFZ
00358  ++INCLUDE FB01M0Z                                                EQW9ZWFZ
00359 *                                                                 EQW9ZWFZ
00360 ***************************************************************** EQW9ZWFZ
00361 *     DESCRIPTION DE LA TS-ECRAN   MDT OFF                        EQW9ZWFZ
00362 ***************************************************************** EQW9ZWFZ
00363  ++INCLUDE SQKWTSMA                                               EQW9ZWFZ
00364      02  TS-FB01M00O REDEFINES ZONE-TS-ECRAN.                     EQW9ZWFZ
00365          10 FILLER PIC X(12).                                     EQW9ZWFZ
00366          05 TS-ECR-XTRMTRACL    COMP PIC S9(4).                   EQW9ZWFZ
00367          05 TS-ECR-XTRMTRACA    PIC X.                            EQW9ZWFZ
00368          05 TS-ECR-XTRMTRACO    PIC X(9).                         EQW9ZWFZ
00369          05 TS-ECR-XAPPLILL     COMP PIC S9(4).                   EQW9ZWFZ
00370          05 TS-ECR-XAPPLILA     PIC X.                            EQW9ZWFZ
00371          05 TS-ECR-XAPPLILO     PIC X(30).                        EQW9ZWFZ
00372          05 TS-ECR-XJOURDL      COMP PIC S9(4).                   EQW9ZWFZ
00373          05 TS-ECR-XJOURDA      PIC X.                            EQW9ZWFZ
00374          05 TS-ECR-XJOURDO      PIC X(8).                         EQW9ZWFZ
00375          05 TS-ECR-XRACFLL      COMP PIC S9(4).                   EQW9ZWFZ
00376          05 TS-ECR-XRACFLA      PIC X.                            EQW9ZWFZ
00377          05 TS-ECR-XRACFLO      PIC X(15).                        EQW9ZWFZ
00378          05 TS-ECR-XHEUREDL     COMP PIC S9(4).                   EQW9ZWFZ
00379          05 TS-ECR-XHEUREDA     PIC X.                            EQW9ZWFZ
00380          05 TS-ECR-XHEUREDO     PIC X(8).                         EQW9ZWFZ
00381          05 TS-ECR-GESCLIL      COMP PIC S9(4).                   EQW9ZWFZ
00382          05 TS-ECR-GESCLIA      PIC X.                            EQW9ZWFZ
00383          05 TS-ECR-GESCLIO      PIC X(11).                        EQW9ZWFZ
00384          05 TS-ECR-RAICL   COMP PIC S9(4).                        EQW9ZWFZ
00385          05 TS-ECR-RAICA   PIC X.                                 EQW9ZWFZ
00386          05 TS-ECR-RAICO   PIC X(3).                              EQW9ZWFZ
00387          05 TS-ECR-NOMCL   COMP PIC S9(4).                        EQW9ZWFZ
00388          05 TS-ECR-NOMCA   PIC X.                                 EQW9ZWFZ
00389          05 TS-ECR-NOMCO   PIC X(30).                             EQW9ZWFZ
00390          05 TS-ECR-LIGNEO  OCCURS 10 TIMES.                       EQW9ZWFZ
00391             06 TS-ECR-CSECODCL   COMP PIC S9(4).                  EQW9ZWFZ
00392             06 TS-ECR-CSECODCA   PIC X.                           EQW9ZWFZ
00393             06 TS-ECR-CSECODCO   PIC X.                           EQW9ZWFZ
00394             06 TS-ECR-IDENTIFL   COMP PIC S9(4).                  EQW9ZWFZ
00395             06 TS-ECR-IDENTIFA   PIC X.                           EQW9ZWFZ
00396             06 TS-ECR-IDENTIFO   PIC X(30).                       EQW9ZWFZ
00397             06 TS-ECR-DATENTDL   COMP PIC S9(4).                  EQW9ZWFZ
00398             06 TS-ECR-DATENTDA   PIC X.                           EQW9ZWFZ
00399             06 TS-ECR-DATENTDO   PIC X(8).                        EQW9ZWFZ
00400             06 TS-ECR-DATSORDL   COMP PIC S9(4).                  EQW9ZWFZ
00401             06 TS-ECR-DATSORDA   PIC X.                           EQW9ZWFZ
00402             06 TS-ECR-DATSORDO   PIC X(8).                        EQW9ZWFZ
00403             06 TS-ECR-MOTIFSCL   COMP PIC S9(4).                  EQW9ZWFZ
00404             06 TS-ECR-MOTIFSCA   PIC X.                           EQW9ZWFZ
00405             06 TS-ECR-MOTIFSCO   PIC X.                           EQW9ZWFZ
00406             06 TS-ECR-STATYPCL   COMP PIC S9(4).                  EQW9ZWFZ
00407             06 TS-ECR-STATYPCA   PIC X.                           EQW9ZWFZ
00408             06 TS-ECR-STATYPCO   PIC X(4).                        EQW9ZWFZ
00409             06 TS-ECR-PTSCRMXL   COMP PIC S9(4).                  EQW9ZWFZ
00410             06 TS-ECR-PTSCRMXA   PIC X.                           EQW9ZWFZ
00411             06 TS-ECR-PTSCRMXO   PIC X(3).                        EQW9ZWFZ
00412          05 TS-ECR-AJOUTCONL    COMP PIC S9(4).                   EQW9ZWFZ
00413          05 TS-ECR-AJOUTCONA    PIC X.                            EQW9ZWFZ
00414          05 TS-ECR-AJOUTCONO    PIC X.                            EQW9ZWFZ
00415          05 TS-ECR-AJOUTVEHL    COMP PIC S9(4).                   EQW9ZWFZ
00416          05 TS-ECR-AJOUTVEHA    PIC X.                            EQW9ZWFZ
00417          05 TS-ECR-AJOUTVEHO    PIC X.                            EQW9ZWFZ
00418          05 TS-ECR-XCDECL       COMP PIC S9(4).                   EQW9ZWFZ
00419          05 TS-ECR-XCDECA       PIC X.                            EQW9ZWFZ
00420          05 TS-ECR-XCDECO       PIC X(9).                         EQW9ZWFZ
00421          05 TS-ECR-XMSGILL      COMP PIC S9(4).                   EQW9ZWFZ
00422          05 TS-ECR-XMSGILA      PIC X.                            EQW9ZWFZ
00423          05 TS-ECR-XMSGILO      PIC X(59).                        EQW9ZWFZ
00424          05 TS-ECR-XMSGALL      COMP PIC S9(4).                   EQW9ZWFZ
00425          05 TS-ECR-XMSGALA      PIC X.                            EQW9ZWFZ
00426          05 TS-ECR-XMSGALO      PIC X(64).                        EQW9ZWFZ
00427 *                                                                 EQW9ZWFZ
00428 ***************************************************************** EQW9ZWFZ
00429 *     ZONE BUFFER D'ENTREE/SORTIE                                 EQW9ZWFZ
00430 ***************************************************************** EQW9ZWFZ
00431 *                                                                 EQW9ZWFZ
00432  ++INCLUDE SQKWZINO                                               EQW9ZWFZ
00433 *                                                                 EQW9ZWFZ
00434 ***************************************************************** EQW9ZWFZ
00435 *     DESCRIPTION DE LA TS PAGINATION                             EQW9ZWFZ
00436 ***************************************************************** EQW9ZWFZ
00437  01 ZONE-TS-PAGE.                                                 EQW9ZWFZ
00438          10 FILLER PIC X(12).                                     EQW9ZWFZ
00439          05 TS-PAG-XTRMTRACL    COMP PIC S9(4).                   EQW9ZWFZ
00440          05 TS-PAG-XTRMTRACA    PIC X.                            EQW9ZWFZ
00441          05 TS-PAG-XTRMTRACO    PIC X(9).                         EQW9ZWFZ
00442          05 TS-PAG-XAPPLILL     COMP PIC S9(4).                   EQW9ZWFZ
00443          05 TS-PAG-XAPPLILA     PIC X.                            EQW9ZWFZ
00444          05 TS-PAG-XAPPLILO     PIC X(30).                        EQW9ZWFZ
00445          05 TS-PAG-XJOURDL      COMP PIC S9(4).                   EQW9ZWFZ
00446          05 TS-PAG-XJOURDA      PIC X.                            EQW9ZWFZ
00447          05 TS-PAG-XJOURDO      PIC X(8).                         EQW9ZWFZ
00448          05 TS-PAG-XRACFLL      COMP PIC S9(4).                   EQW9ZWFZ
00449          05 TS-PAG-XRACFLA      PIC X.                            EQW9ZWFZ
00450          05 TS-PAG-XRACFLO      PIC X(15).                        EQW9ZWFZ
00451          05 TS-PAG-XHEUREDL     COMP PIC S9(4).                   EQW9ZWFZ
00452          05 TS-PAG-XHEUREDA     PIC X.                            EQW9ZWFZ
00453          05 TS-PAG-XHEUREDO     PIC X(8).                         EQW9ZWFZ
00454          05 TS-PAG-GESCLIL      COMP PIC S9(4).                   EQW9ZWFZ
00455          05 TS-PAG-GESCLIA      PIC X.                            EQW9ZWFZ
00456          05 TS-PAG-GESCLIO      PIC X(11).                        EQW9ZWFZ
00457          05 TS-PAG-RAICL   COMP PIC S9(4).                        EQW9ZWFZ
00458          05 TS-PAG-RAICA   PIC X.                                 EQW9ZWFZ
00459          05 TS-PAG-RAICO   PIC X(3).                              EQW9ZWFZ
00460          05 TS-PAG-NOMCL   COMP PIC S9(4).                        EQW9ZWFZ
00461          05 TS-PAG-NOMCA   PIC X.                                 EQW9ZWFZ
00462          05 TS-PAG-NOMCO   PIC X(30).                             EQW9ZWFZ
00463          05 TS-PAG-TYPTS   PIC S9(4) COMP.                        EQW9ZWFZ
00464          05 TS-PAG-RANTS   PIC X(1).                              EQW9ZWFZ
00465          05 TS-PAG-LIGNEO  OCCURS 10 TIMES.                       EQW9ZWFZ
00466             06 TS-PAG-CSECODCL   COMP PIC S9(4).                  EQW9ZWFZ
00467             06 TS-PAG-CSECODCA   PIC X.                           EQW9ZWFZ
00468             06 TS-PAG-CSECODCO   PIC X.                           EQW9ZWFZ
00469             06 TS-PAG-IDENTIFL   COMP PIC S9(4).                  EQW9ZWFZ
00470             06 TS-PAG-IDENTIFA   PIC X.                           EQW9ZWFZ
00471             06 TS-PAG-IDENTIFO   PIC X(30).                       EQW9ZWFZ
00472             06 TS-PAG-DATENTDL   COMP PIC S9(4).                  EQW9ZWFZ
00473             06 TS-PAG-DATENTDA   PIC X.                           EQW9ZWFZ
00474             06 TS-PAG-DATENTDO   PIC X(8).                        EQW9ZWFZ
00475             06 TS-PAG-DATSORDL   COMP PIC S9(4).                  EQW9ZWFZ
00476             06 TS-PAG-DATSORDA   PIC X.                           EQW9ZWFZ
00477             06 TS-PAG-DATSORDO   PIC X(8).                        EQW9ZWFZ
00478             06 TS-PAG-MOTIFSCL   COMP PIC S9(4).                  EQW9ZWFZ
00479             06 TS-PAG-MOTIFSCA   PIC X.                           EQW9ZWFZ
00480             06 TS-PAG-MOTIFSCO   PIC X.                           EQW9ZWFZ
00481             06 TS-PAG-STATYPCL   COMP PIC S9(4).                  EQW9ZWFZ
00482             06 TS-PAG-STATYPCA   PIC X.                           EQW9ZWFZ
00483             06 TS-PAG-STATYPCO   PIC X(4).                        EQW9ZWFZ
00484             06 TS-PAG-PTSCRMXL   COMP PIC S9(4).                  EQW9ZWFZ
00485             06 TS-PAG-PTSCRMXA   PIC X.                           EQW9ZWFZ
00486             06 TS-PAG-PTSCRMXO   PIC X(3).                        EQW9ZWFZ
00487          05 TS-PAG-AJOUTCONL    COMP PIC S9(4).                   EQW9ZWFZ
00488          05 TS-PAG-AJOUTCONA    PIC X.                            EQW9ZWFZ
00489          05 TS-PAG-AJOUTCONO    PIC X.                            EQW9ZWFZ
00490          05 TS-PAG-AJOUTVEHL    COMP PIC S9(4).                   EQW9ZWFZ
00491          05 TS-PAG-AJOUTVEHA    PIC X.                            EQW9ZWFZ
00492          05 TS-PAG-AJOUTVEHO    PIC X.                            EQW9ZWFZ
00493          05 TS-PAG-XCDECL       COMP PIC S9(4).                   EQW9ZWFZ
00494          05 TS-PAG-XCDECA       PIC X.                            EQW9ZWFZ
00495          05 TS-PAG-XCDECO       PIC X(9).                         EQW9ZWFZ
00496          05 TS-PAG-XMSGILL      COMP PIC S9(4).                   EQW9ZWFZ
00497          05 TS-PAG-XMSGILA      PIC X.                            EQW9ZWFZ
00498          05 TS-PAG-XMSGILO      PIC X(59).                        EQW9ZWFZ
00499          05 TS-PAG-XMSGALL      COMP PIC S9(4).                   EQW9ZWFZ
00500          05 TS-PAG-XMSGALA      PIC X.                            EQW9ZWFZ
00501          05 TS-PAG-XMSGALO      PIC X(64).                        EQW9ZWFZ
00502 *                                                                 EQW9ZWFZ
00503 ***************************************************************** EQW9ZWFZ
00504 *     ZONE BUFFER D'ENTREE/SORTIE                                 EQW9ZWFZ
00505 ***************************************************************** EQW9ZWFZ
00506 *                                                                 EQW9ZWFZ
00507 *                                                                 EQW9ZWFZ
00508 ***************************************************************** EQW9ZWFZ
00509 * ZONE D'INTERFACE POUR LA GESTION DES ERREURS NON RECOUVRABLES   EQW9ZWFZ
00510 ***************************************************************** EQW9ZWFZ
00511  ++INCLUDE SQKWERRO                                               EQW9ZWFZ
00512 ***************************************************************** EQW9ZWFZ
00513 *  ZONES DE GESTION DES FICHIERS                                  EQW9ZWFZ
00514 ***************************************************************** EQW9ZWFZ
00515 *                                                                 EQW9ZWFZ
00516 /                                                                 EQW9ZWFZ
00517 ***************************************************************** EQW9ZWFZ
00518 ***************************************************************** EQW9ZWFZ
00519 **********************  LINKAGE SECTION ************************* EQW9ZWFZ
00520 ***************************************************************** EQW9ZWFZ
00521 ***************************************************************** EQW9ZWFZ
00522 *                                                                 EQW9ZWFZ
00523  LINKAGE SECTION.                                                 EQW9ZWFZ
00524 *---------------*    DFHEIBLK ; DFHCOMMAREA.                      EQW9ZWFZ
00525  01  DFHCOMMAREA.                                                 EQW9ZWFZ
00526      02  FILLER             PIC X(4096).                          EQW9ZWFZ
00527 *                                                                 EQW9ZWFZ
00528 ***************************************************************   EQW9ZWFZ
00529 *        ZONES ADRESSABLES EXTERNES A LA TACHE                    EQW9ZWFZ
00530 ***************************************************************   EQW9ZWFZ
00531 *    USER                                                         EQW9ZWFZ
00532  01  LINK-USER              PIC X(4000).                          EQW9ZWFZ
00533 /                                                                 EQW9ZWFZ
00534  PROCEDURE DIVISION.                                              EQW9ZWFZ
00535 *------------------*                                              EQW9ZWFZ
00536 *                                                                 EQW9ZWFZ
00537 ***************************************************************** EQW9ZWFZ
00538 *              T R A M E   DU   P R O G R A M M E               * EQW9ZWFZ
00539 ***************************************************************** EQW9ZWFZ
00540 *                                                                 EQW9ZWFZ
00541 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
00542 *                                                               * EQW9ZWFZ
00543 *           -----------------------------------------           * EQW9ZWFZ
00544 *           I MODULE DE BASE DE LA TRANSACTION FB01 I       *     EQW9ZWFZ
00545 *           -----------------------------------------           * EQW9ZWFZ
00546 *                               I                               * EQW9ZWFZ
00547 *           -----------------------------------------           * EQW9ZWFZ
00548 *           I                   I                   I           * EQW9ZWFZ
00549 *   --------V--------   --------V--------   --------V--------   * EQW9ZWFZ
00550 *   I    ENTREE     I   I  TRAITEMENT   I   I     SORTIE    I   * EQW9ZWFZ
00551 *   -----------------   -----------------   -----------------   * EQW9ZWFZ
00552 *                                                               * EQW9ZWFZ
00553 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
00554  MODULE-FB01.                                                     EQW9ZWFZ
00555 *-----------*                                                     EQW9ZWFZ
00556      PERFORM MODULE-ENTREE THRU                                   EQW9ZWFZ
00557              FIN-MODULE-ENTREE.                                   EQW9ZWFZ
00558 *                                                                 EQW9ZWFZ
00559      IF TRAITEMENT                                                EQW9ZWFZ
00560         PERFORM MODULE-TRAITEMENT THRU                            EQW9ZWFZ
00561                 FIN-MODULE-TRAITEMENT                             EQW9ZWFZ
00562      END-IF.                                                      EQW9ZWFZ
00563 *                                                                 EQW9ZWFZ
00564      PERFORM MODULE-SORTIE THRU                                   EQW9ZWFZ
00565              FIN-MODULE-SORTIE.                                   EQW9ZWFZ
00566 *                                                                 EQW9ZWFZ
00567  FIN-MODULE-FB01.  EXIT.                                          EQW9ZWFZ
00568 /                                                                 EQW9ZWFZ
00569 ***************************************************************** EQW9ZWFZ
00570 ***************************************************************** EQW9ZWFZ
00571 ***********************  MODULE ENTREE  ************************* EQW9ZWFZ
00572 ***************************************************************** EQW9ZWFZ
00573 ***************************************************************** EQW9ZWFZ
00574 *                                                                 EQW9ZWFZ
00575 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
00576 *                                                               * EQW9ZWFZ
00577 *                    -----------------------                    * EQW9ZWFZ
00578 *                    I   MODULE D'ENTREE   I                    * EQW9ZWFZ
00579 *                    -----------------------                    * EQW9ZWFZ
00580 *                               I                               * EQW9ZWFZ
00581 *       ---------------------------------------------           * EQW9ZWFZ
00582 *       I          I         I                      I           * EQW9ZWFZ
00583 *  ----------  --------  ----------    -----------------------  * EQW9ZWFZ
00584 *  I HANDLE I  I USER I  I ADRESS I    I RECEPTION-MESSAGE   I  * EQW9ZWFZ
00585 *  ----------  --------  ----------    -----------------------  * EQW9ZWFZ
00586 *                                                               * EQW9ZWFZ
00587 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
00588 *                                                                 EQW9ZWFZ
00589  MODULE-ENTREE.                                                   EQW9ZWFZ
00590 *-------------*                                                   EQW9ZWFZ
00591      PERFORM INIT-USER          THRU                              EQW9ZWFZ
00592              FIN-INIT-USER.                                       EQW9ZWFZ
00593 *                                                                 EQW9ZWFZ
00594      PERFORM INIT-ADDRESS       THRU                              EQW9ZWFZ
00595              FIN-INIT-ADDRESS.                                    EQW9ZWFZ
00596 *                                                                 EQW9ZWFZ
00597      PERFORM RECEPTION-MESSAGE  THRU                              EQW9ZWFZ
00598              FIN-RECEPTION-MESSAGE.                               EQW9ZWFZ
00599 *                                                                 EQW9ZWFZ
00600  FIN-MODULE-ENTREE.   EXIT.                                       EQW9ZWFZ
00601 *                                                                 EQW9ZWFZ
00602 ***************************************************************** EQW9ZWFZ
00603 *    INITIALISATION DES ZONES QUI NE PEUVENT PAS ETRE EN VALUE  * EQW9ZWFZ
00604 ***************************************************************** EQW9ZWFZ
00605  INIT-USER.                                                       EQW9ZWFZ
00606 *---------*                                                       EQW9ZWFZ
00607      MOVE LOW-VALUE TO Z-MAP.                                     EQW9ZWFZ
00608      MOVE   'FB01'  TO NOM-TACHE,                                 EQW9ZWFZ
00609                        NOM-TACHE-MAP,                             EQW9ZWFZ
00610                        NOM-TACHE-MAPSET,                          EQW9ZWFZ
00611                        NOM-TACHE-PROG.                            EQW9ZWFZ
00612      MOVE 'AA00'    TO NOM-LEVEL-SIGN.                            EQW9ZWFZ
00613      MOVE EIBTRMID  TO TRMID-TS-ECRAN,                            EQW9ZWFZ
00614                        W-XTERMIC.                                 EQW9ZWFZ
00615      MOVE NOM-TACHE TO W-XTRANSC,                                 EQW9ZWFZ
00616                        TRNID-TS-ECRAN.                            EQW9ZWFZ
00617      PERFORM RETRIEVE-DATA THRU                                   EQW9ZWFZ
00618              FIN-RETRIEVE-DATA.                                   EQW9ZWFZ
00619 *                     TROUVE : CODE-RETOUR = 0                    EQW9ZWFZ
00620      IF TROUVE                                                    EQW9ZWFZ
00621         MOVE 1 TO TYPE-PASSAGE                                    EQW9ZWFZ
00622      END-IF.                                                      EQW9ZWFZ
00623 *                                                                 EQW9ZWFZ
00624 *  SI ON N'EST PAS PASSE PAR LA SIGNATURE                         EQW9ZWFZ
00625 *  ET SI ON N'A PAS ETE APPELE PAR START  ERREUR                  EQW9ZWFZ
00626 *                                                                 EQW9ZWFZ
00627 *           TYPE-PASSAGE = 1  ===> PASSAGE-PAR-START              EQW9ZWFZ
00628      IF NOT PASSAGE-PAR-START                                     EQW9ZWFZ
00629         IF  EIBCALEN  = 0                                         EQW9ZWFZ
00630 *           MESSAGE ERREUR; RETOUR A SIGNATURE                    EQW9ZWFZ
00631             MOVE 'SQ001'         TO COM-GENE-MESANO               EQW9ZWFZ
00632                                     COM-CODERR                    EQW9ZWFZ
00633             MOVE CODE-LEVEL-SIGN TO Z-FONCTION                    EQW9ZWFZ
00634             PERFORM MODULE-SORTIE   THRU                          EQW9ZWFZ
00635                     FIN-MODULE-SORTIE                             EQW9ZWFZ
00636         ELSE                                                      EQW9ZWFZ
00637 *          ICI  ON RECUPERE LA COMMAREA PASSEE PAR XCTL OU RETURN EQW9ZWFZ
00638            MOVE DFHCOMMAREA      TO Z-COMMAREA                    EQW9ZWFZ
00639         END-IF                                                    EQW9ZWFZ
00640      END-IF.                                                      EQW9ZWFZ
00641 *                                                                 EQW9ZWFZ
00642 * RECUPERATION DU CODE COMPAGNIE POUR IDENTIFICATION DES TABLES   EQW9ZWFZ
00643 *    AAMENUXX   ET  AACONVXX                                      EQW9ZWFZ
00644 *                                                                 EQW9ZWFZ
00645      MOVE COM-GENE-CODCIE-PRINCIPAL      TO TABLE-SUFF.           EQW9ZWFZ
00646 *                                                                 EQW9ZWFZ
00647 *                                                                 EQW9ZWFZ
00648 * IDENTIFICATION DE LA TS DE PAGINATION                           EQW9ZWFZ
00649 *                                                                 EQW9ZWFZ
00650      MOVE EIBTRMID   TO PAGE-TS-PREF.                             EQW9ZWFZ
00651      MOVE 'PAG'      TO PAGE-TS-CONV.                             EQW9ZWFZ
00652 *                                                                 EQW9ZWFZ
00653 * IDENTIFICATION DE LA TS DE CONFIDENTIALITE                      EQW9ZWFZ
00654 *                                                                 EQW9ZWFZ
00655      MOVE EIBTRMID   TO CONF-TS-PREF.                             EQW9ZWFZ
00656      MOVE 'CF0'      TO CONF-TS-CONV.                             EQW9ZWFZ
00657 *                                                                 EQW9ZWFZ
00658 * IDENTIFICATION DE  LA TS DE L'APPLICATIVE                       EQW9ZWFZ
00659 *                                                                 EQW9ZWFZ
00660      MOVE EIBTRMID   TO APP-TS-PREF.                              EQW9ZWFZ
00661      MOVE 'APP'      TO APP-TS-CONV.                              EQW9ZWFZ
00662      MOVE   'NON'    TO DEBUGGIN.                                 EQW9ZWFZ
00663 *                                                                 EQW9ZWFZ
00664 * IDENTIFICATION DE  LA TS CONTRAT                                EQW9ZWFZ
00665 *                                                                 EQW9ZWFZ
00666      MOVE EIBTRMID   TO TS-CNTPROD-EIBTRMID.                      EQW9ZWFZ
00667      MOVE '1'        TO TS-CNTPROD-NUM.                           EQW9ZWFZ
00668 ****************** POUR CONVERSATION **************************** EQW9ZWFZ
00669 *TK191190 POUR EXIT-SELECTION-DE-PLAN : IDENTIFICATION DE TS-PLAN EQW9ZWFZ
00670 ***************************************************************** EQW9ZWFZ
00671 *  POUR CONVERSATION SANS  SQL : LE PLAN PROPOSE = 'UTILPLN'      EQW9ZWFZ
00672  ++INCLUDE SQKCPLIU                                               EQW9ZWFZ
00673 */                                                                EQW9ZWFZ
00674  FIN-INIT-USER.  EXIT.                                            EQW9ZWFZ
00675 *                                                                 EQW9ZWFZ
00676 ***************************************************************   EQW9ZWFZ
00677 * INIT-ADDRESS.  ADRESSAGE DES ZONES SYSTEME DE CICS              EQW9ZWFZ
00678 *                CREATION TS PLAN POUR EXIT-SELECTION-PLAN        EQW9ZWFZ
00679 ***************************************************************   EQW9ZWFZ
00680  ++INCLUDE SQKCADDB                                               EQW9ZWFZ
00681 */                                                                EQW9ZWFZ
00682 /                                                                 EQW9ZWFZ
00683 ***************************************************************** EQW9ZWFZ
00684 * DETERMINATION DU TRAITEMENT EN FONCTION DE L'ENVIRONNEMENT    * EQW9ZWFZ
00685 ***************************************************************** EQW9ZWFZ
00686  RECEPTION-MESSAGE.                                               EQW9ZWFZ
00687 *-----------------*                                               EQW9ZWFZ
00688 *                                      PASSAGE PAR START          EQW9ZWFZ
00689      IF  PASSAGE-PAR-START                                        EQW9ZWFZ
00690          PERFORM DELETE-TS-ECRAN THRU                             EQW9ZWFZ
00691                  FIN-DELETE-TS-ECRAN                              EQW9ZWFZ
00692          MOVE    CODE-TRAITEMENT-AUTOMATIQUE TO Z-FONCTION        EQW9ZWFZ
00693          MOVE    1               TO NUMERO-PASSAGE                EQW9ZWFZ
00694          GO TO                   FIN-RECEPTION-MESSAGE            EQW9ZWFZ
00695      END-IF.                                                      EQW9ZWFZ
00696 *                                            AUTRE TACHE          EQW9ZWFZ
00697      IF  EIBTRNID NOT = NOM-TACHE                                 EQW9ZWFZ
00698          PERFORM DELETE-TS-ECRAN THRU                             EQW9ZWFZ
00699                  FIN-DELETE-TS-ECRAN                              EQW9ZWFZ
00700          MOVE   CODE-TRAITEMENT-AUTOMATIQUE TO Z-FONCTION         EQW9ZWFZ
00701          MOVE   1               TO NUMERO-PASSAGE                 EQW9ZWFZ
00702          GO TO                  FIN-RECEPTION-MESSAGE             EQW9ZWFZ
00703      END-IF.                                                      EQW9ZWFZ
00704 *                                            FAST PATH            EQW9ZWFZ
00705      IF EIBTRNID = Z-COMMAREA-TACHE-JUMP                          EQW9ZWFZ
00706         PERFORM DELETE-TS-ECRAN THRU                              EQW9ZWFZ
00707                 FIN-DELETE-TS-ECRAN                               EQW9ZWFZ
00708         MOVE    SPACES          TO  Z-COMMAREA-TACHE-JUMP         EQW9ZWFZ
00709         MOVE    CODE-TRAITEMENT-AUTOMATIQUE TO Z-FONCTION         EQW9ZWFZ
00710         MOVE    1               TO NUMERO-PASSAGE                 EQW9ZWFZ
00711         GO TO                   FIN-RECEPTION-MESSAGE             EQW9ZWFZ
00712      END-IF.                                                      EQW9ZWFZ
00713 *                                                                 EQW9ZWFZ
00714      PERFORM RECEIVE-MAP THRU                                     EQW9ZWFZ
00715              FIN-RECEIVE-MAP.                                     EQW9ZWFZ
00716      MOVE    EIBAID             TO WORKAID.                       EQW9ZWFZ
00717 *                                            LEVEL-MAX            EQW9ZWFZ
00718      IF  TOUCHE-PF4 OR TOUCHE-PF16                                EQW9ZWFZ
00719          MOVE CODE-LEVEL-MAX TO Z-FONCTION                        EQW9ZWFZ
00720      END-IF.                                                      EQW9ZWFZ
00721 *                                            DERNIER-AFFICHAGE    EQW9ZWFZ
00722 *    REAFFICHAGE DE L'ECRAN A PARTIR DE LA                        EQW9ZWFZ
00723 *    TS-ECRAN                                                     EQW9ZWFZ
00724 *                                            DERNIER-AFFICHAGE    EQW9ZWFZ
00725      IF  TOUCHE-PF5 OR TOUCHE-PF17                                EQW9ZWFZ
00726          MOVE CODE-LAST-AFF TO Z-FONCTION                         EQW9ZWFZ
00727          MOVE LOW-VALUE     TO Z-MAP                              EQW9ZWFZ
00728          MOVE NOM-TACHE     TO NOM-TACHE-RETOUR                   EQW9ZWFZ
00729          MOVE SPACES        TO COM-GENE-REAF                      EQW9ZWFZ
00730          PERFORM FUSION-TS-ECRAN THRU                             EQW9ZWFZ
00731                  FIN-FUSION-TS-ECRAN                              EQW9ZWFZ
00732          PERFORM SEND-MAP THRU                                    EQW9ZWFZ
00733                  FIN-SEND-MAP                                     EQW9ZWFZ
00734          PERFORM RETOUR-COMMAREA THRU                             EQW9ZWFZ
00735                  FIN-RETOUR-COMMAREA                              EQW9ZWFZ
00736      END-IF.                                                      EQW9ZWFZ
00737 *                                                                 EQW9ZWFZ
00738 * RECUPERATION DU CONTENU DE LA TS SI LA SORTIE N'EST PAS         EQW9ZWFZ
00739 * DEFINITIVE (LA SORTIE NE PEUT ETRE SURE QUE DANS                EQW9ZWFZ
00740 * MODULE-SORTIE)                                                  EQW9ZWFZ
00741 *                                                                 EQW9ZWFZ
00742          PERFORM FUSION-TS-ECRAN THRU                             EQW9ZWFZ
00743                  FIN-FUSION-TS-ECRAN.                             EQW9ZWFZ
00744 *                                            LEVEL-PREC           EQW9ZWFZ
00745 * POUR LES CONVERSATIONS SEULEMENT           LEVEL-PREC           EQW9ZWFZ
00746 *                                            LEVEL-PREC           EQW9ZWFZ
00747      IF  TOUCHE-PF12 OR TOUCHE-PF24                               EQW9ZWFZ
00748          MOVE CODE-LEVEL-PREC TO Z-FONCTION                       EQW9ZWFZ
00749      END-IF.                                                      EQW9ZWFZ
00750 *                                            AIDE                 EQW9ZWFZ
00751      IF  TOUCHE-PF1 OR TOUCHE-PF13                                EQW9ZWFZ
00752          MOVE CODE-TRAITEMENT-NORMAL TO Z-FONCTION                EQW9ZWFZ
00753      END-IF.                                                      EQW9ZWFZ
00754 *                                            LEVEL-SUP            EQW9ZWFZ
00755      IF  TOUCHE-PF3 OR TOUCHE-PF15                                EQW9ZWFZ
00756          MOVE CODE-LEVEL-SUP TO Z-FONCTION                        EQW9ZWFZ
00757      END-IF.                                                      EQW9ZWFZ
00758 *                                            PAGE SUIVANTE        EQW9ZWFZ
00759      IF  TOUCHE-PF8 OR TOUCHE-PF20                                EQW9ZWFZ
00760          MOVE CODE-SUIVANTE          TO Z-FONCTION                EQW9ZWFZ
00761      END-IF.                                                      EQW9ZWFZ
00762 *                                            PAGE PRECEDENTE      EQW9ZWFZ
00763      IF  TOUCHE-PF7 OR TOUCHE-PF19                                EQW9ZWFZ
00764          MOVE CODE-PRECEDENTE        TO Z-FONCTION                EQW9ZWFZ
00765      END-IF.                                                      EQW9ZWFZ
00766 *                                            SUITE                EQW9ZWFZ
00767      IF  TOUCHE-ENTER                                             EQW9ZWFZ
00768          MOVE CODE-TRAITEMENT-NORMAL TO Z-FONCTION                EQW9ZWFZ
00769      END-IF.                                                      EQW9ZWFZ
00770 *                                            SIGNATURE            EQW9ZWFZ
00771      IF  TOUCHE-CLEAR                                             EQW9ZWFZ
00772          MOVE CODE-LEVEL-SIGN        TO Z-FONCTION                EQW9ZWFZ
00773          GO TO FIN-RECEPTION-MESSAGE                              EQW9ZWFZ
00774      END-IF.                                                      EQW9ZWFZ
00775 *                                                                 EQW9ZWFZ
00776  FIN-RECEPTION-MESSAGE.  EXIT.                                    EQW9ZWFZ
00777 /                                                                 EQW9ZWFZ
00778 ***************************************************************** EQW9ZWFZ
00779 * RECEIVE-MAP. RECEPTION DE LA MAP                                EQW9ZWFZ
00780 ***************************************************************** EQW9ZWFZ
00781 *                                                                 EQW9ZWFZ
00782  ++INCLUDE SQKCRECV                                               EQW9ZWFZ
00783 /                                                                 EQW9ZWFZ
00784 ***************************************************************** EQW9ZWFZ
00785 *        SECTION  DE PROGRAMME POUR LE TRAITEMENT DU MDT/OFF      EQW9ZWFZ
00786 ***************************************************************** EQW9ZWFZ
00787  ++INCLUDE SQKCMDTB                                               EQW9ZWFZ
00788 /                                                                 EQW9ZWFZ
00789 ***************************************************************** EQW9ZWFZ
00790 *   SECTION  DE PROGRAMME POUR L'ECRITURE DE LA TS PAGINATION     EQW9ZWFZ
00791 ***************************************************************** EQW9ZWFZ
00792  ++INCLUDE SQKCWRPG                                               EQW9ZWFZ
00793 /                                                                 EQW9ZWFZ
00794 ***************************************************************** EQW9ZWFZ
00795 *   SECTION  DE PROGRAMME POUR L'ECRITURE DE LA TS PLAN           EQW9ZWFZ
00796 ***************************************************************** EQW9ZWFZ
00797  ++INCLUDE SQKCWRPL                                               EQW9ZWFZ
00798 /                                                                 EQW9ZWFZ
00799 ***************************************************************** EQW9ZWFZ
00800 *        MISE-A-JOUR-TS-ECRAN     GENEREE                         EQW9ZWFZ
00801 ***************************************************************** EQW9ZWFZ
00802 *                                                                 EQW9ZWFZ
00803  MISE-A-JOUR-TS-ECRAN.                                            EQW9ZWFZ
00804 *                                                                 EQW9ZWFZ
00805 *                                                                 EQW9ZWFZ
00806      IF ECR-XTRMTRACL    = ZEROS AND                              EQW9ZWFZ
00807         ECR-XTRMTRACA  NOT = EFFACE-FIN-ZONE                      EQW9ZWFZ
00808         MOVE TS-ECR-XTRMTRACO TO ECR-XTRMTRACO                    EQW9ZWFZ
00809         MOVE TS-ECR-XTRMTRACA TO ECR-XTRMTRACA                    EQW9ZWFZ
00810      ELSE                                                         EQW9ZWFZ
00811         MOVE ECR-XTRMTRACO     TO TS-ECR-XTRMTRACO                EQW9ZWFZ
00812         MOVE LOW-VALUE      TO TS-ECR-XTRMTRACA                   EQW9ZWFZ
00813      END-IF.                                                      EQW9ZWFZ
00814 *                                                                 EQW9ZWFZ
00815      IF ECR-XAPPLILL    = ZEROS AND                               EQW9ZWFZ
00816         ECR-XAPPLILA  NOT = EFFACE-FIN-ZONE                       EQW9ZWFZ
00817         MOVE TS-ECR-XAPPLILO TO ECR-XAPPLILO                      EQW9ZWFZ
00818         MOVE TS-ECR-XAPPLILA TO ECR-XAPPLILA                      EQW9ZWFZ
00819      ELSE                                                         EQW9ZWFZ
00820         MOVE ECR-XAPPLILO     TO TS-ECR-XAPPLILO                  EQW9ZWFZ
00821         MOVE LOW-VALUE      TO TS-ECR-XAPPLILA                    EQW9ZWFZ
00822      END-IF.                                                      EQW9ZWFZ
00823 *                                                                 EQW9ZWFZ
00824      IF ECR-XJOURDL    = ZEROS AND                                EQW9ZWFZ
00825         ECR-XJOURDA  NOT = EFFACE-FIN-ZONE                        EQW9ZWFZ
00826         MOVE TS-ECR-XJOURDO TO ECR-XJOURDO                        EQW9ZWFZ
00827         MOVE TS-ECR-XJOURDA TO ECR-XJOURDA                        EQW9ZWFZ
00828      ELSE                                                         EQW9ZWFZ
00829         MOVE ECR-XJOURDO     TO TS-ECR-XJOURDO                    EQW9ZWFZ
00830         MOVE LOW-VALUE      TO TS-ECR-XJOURDA                     EQW9ZWFZ
00831      END-IF.                                                      EQW9ZWFZ
00832 *                                                                 EQW9ZWFZ
00833      IF ECR-XRACFLL    = ZEROS AND                                EQW9ZWFZ
00834         ECR-XRACFLA  NOT = EFFACE-FIN-ZONE                        EQW9ZWFZ
00835         MOVE TS-ECR-XRACFLO TO ECR-XRACFLO                        EQW9ZWFZ
00836         MOVE TS-ECR-XRACFLA TO ECR-XRACFLA                        EQW9ZWFZ
00837      ELSE                                                         EQW9ZWFZ
00838         MOVE ECR-XRACFLO     TO TS-ECR-XRACFLO                    EQW9ZWFZ
00839         MOVE LOW-VALUE      TO TS-ECR-XRACFLA                     EQW9ZWFZ
00840      END-IF.                                                      EQW9ZWFZ
00841 *                                                                 EQW9ZWFZ
00842      IF ECR-XHEUREDL    = ZEROS AND                               EQW9ZWFZ
00843         ECR-XHEUREDA  NOT = EFFACE-FIN-ZONE                       EQW9ZWFZ
00844         MOVE TS-ECR-XHEUREDO TO ECR-XHEUREDO                      EQW9ZWFZ
00845         MOVE TS-ECR-XHEUREDA TO ECR-XHEUREDA                      EQW9ZWFZ
00846      ELSE                                                         EQW9ZWFZ
00847         MOVE ECR-XHEUREDO     TO TS-ECR-XHEUREDO                  EQW9ZWFZ
00848         MOVE LOW-VALUE      TO TS-ECR-XHEUREDA                    EQW9ZWFZ
00849      END-IF.                                                      EQW9ZWFZ
00850 *                                                                 EQW9ZWFZ
00851      IF ECR-GESCLIL    = ZEROS AND                                EQW9ZWFZ
00852         ECR-GESCLIA  NOT = EFFACE-FIN-ZONE                        EQW9ZWFZ
00853         MOVE TS-ECR-GESCLIO TO ECR-GESCLIO                        EQW9ZWFZ
00854         MOVE TS-ECR-GESCLIA TO ECR-GESCLIA                        EQW9ZWFZ
00855      ELSE                                                         EQW9ZWFZ
00856         MOVE ECR-GESCLIO     TO TS-ECR-GESCLIO                    EQW9ZWFZ
00857         MOVE LOW-VALUE      TO TS-ECR-GESCLIA                     EQW9ZWFZ
00858         MOVE '2'            TO ETAT-ECRAN                         EQW9ZWFZ
00859      END-IF.                                                      EQW9ZWFZ
00860 *                                                                 EQW9ZWFZ
00861      IF ECR-RAICL    = ZEROS AND                                  EQW9ZWFZ
00862         ECR-RAICA  NOT = EFFACE-FIN-ZONE                          EQW9ZWFZ
00863         MOVE TS-ECR-RAICO TO ECR-RAICO                            EQW9ZWFZ
00864         MOVE TS-ECR-RAICA TO ECR-RAICA                            EQW9ZWFZ
00865      ELSE                                                         EQW9ZWFZ
00866         MOVE ECR-RAICO     TO TS-ECR-RAICO                        EQW9ZWFZ
00867         MOVE LOW-VALUE      TO TS-ECR-RAICA                       EQW9ZWFZ
00868         MOVE '2'            TO ETAT-ECRAN                         EQW9ZWFZ
00869      END-IF.                                                      EQW9ZWFZ
00870 *                                                                 EQW9ZWFZ
00871      IF ECR-NOMCL    = ZEROS AND                                  EQW9ZWFZ
00872         ECR-NOMCA  NOT = EFFACE-FIN-ZONE                          EQW9ZWFZ
00873         MOVE TS-ECR-NOMCO TO ECR-NOMCO                            EQW9ZWFZ
00874         MOVE TS-ECR-NOMCA TO ECR-NOMCA                            EQW9ZWFZ
00875      ELSE                                                         EQW9ZWFZ
00876         MOVE ECR-NOMCO     TO TS-ECR-NOMCO                        EQW9ZWFZ
00877         MOVE LOW-VALUE      TO TS-ECR-NOMCA                       EQW9ZWFZ
00878         MOVE '2'            TO ETAT-ECRAN                         EQW9ZWFZ
00879      END-IF.                                                      EQW9ZWFZ
00880 *                                                                 EQW9ZWFZ
00881      MOVE 0 TO IL.                                                EQW9ZWFZ
00882      PERFORM BOUCLE-MATS1  THRU                                   EQW9ZWFZ
00883              FIN-BOUCLE-MATS1                                     EQW9ZWFZ
00884              10 TIMES.                                            EQW9ZWFZ
00885 *                                                                 EQW9ZWFZ
00886 *                                                                 EQW9ZWFZ
00887      IF ECR-AJOUTCONL    = ZEROS AND                              EQW9ZWFZ
00888         ECR-AJOUTCONA  NOT = EFFACE-FIN-ZONE                      EQW9ZWFZ
00889         MOVE TS-ECR-AJOUTCONO TO ECR-AJOUTCONO                    EQW9ZWFZ
00890         MOVE TS-ECR-AJOUTCONA TO ECR-AJOUTCONA                    EQW9ZWFZ
00891      ELSE                                                         EQW9ZWFZ
00892         MOVE ECR-AJOUTCONO     TO TS-ECR-AJOUTCONO                EQW9ZWFZ
00893         MOVE LOW-VALUE      TO TS-ECR-AJOUTCONA                   EQW9ZWFZ
00894         MOVE '2'            TO ETAT-ECRAN                         EQW9ZWFZ
00895      END-IF.                                                      EQW9ZWFZ
00896 *                                                                 EQW9ZWFZ
00897      IF ECR-AJOUTVEHL    = ZEROS AND                              EQW9ZWFZ
00898         ECR-AJOUTVEHA  NOT = EFFACE-FIN-ZONE                      EQW9ZWFZ
00899         MOVE TS-ECR-AJOUTVEHO TO ECR-AJOUTVEHO                    EQW9ZWFZ
00900         MOVE TS-ECR-AJOUTVEHA TO ECR-AJOUTVEHA                    EQW9ZWFZ
00901      ELSE                                                         EQW9ZWFZ
00902         MOVE ECR-AJOUTVEHO     TO TS-ECR-AJOUTVEHO                EQW9ZWFZ
00903         MOVE LOW-VALUE      TO TS-ECR-AJOUTVEHA                   EQW9ZWFZ
00904         MOVE '2'            TO ETAT-ECRAN                         EQW9ZWFZ
00905      END-IF.                                                      EQW9ZWFZ
00906 *                                                                 EQW9ZWFZ
00907      IF ECR-XCDECL    = ZEROS AND                                 EQW9ZWFZ
00908         ECR-XCDECA  NOT = EFFACE-FIN-ZONE                         EQW9ZWFZ
00909         MOVE TS-ECR-XCDECO TO ECR-XCDECO                          EQW9ZWFZ
00910         MOVE TS-ECR-XCDECA TO ECR-XCDECA                          EQW9ZWFZ
00911      ELSE                                                         EQW9ZWFZ
00912         MOVE ECR-XCDECO     TO TS-ECR-XCDECO                      EQW9ZWFZ
00913         MOVE LOW-VALUE      TO TS-ECR-XCDECA                      EQW9ZWFZ
00914      END-IF.                                                      EQW9ZWFZ
00915 *                                                                 EQW9ZWFZ
00916 *    MOVE Z-TIMER-TIMJOU    TO MTIMJOUI TS-MTIMJOUI.              EQW9ZWFZ
00917 *    MISE A BLANC DES MESSAGES.                                   EQW9ZWFZ
00918      MOVE SPACES            TO ECR-XMSGILO  ECR-XMSGALO.          EQW9ZWFZ
00919 *                                                                 EQW9ZWFZ
00920  FIN-MISE-A-JOUR-TS-ECRAN.  EXIT.                                 EQW9ZWFZ
00921               EJECT                                               EQW9ZWFZ
00922 *                                                                 EQW9ZWFZ
00923  BOUCLE-MATS1.                                                    EQW9ZWFZ
00924      ADD 1 TO IL.                                                 EQW9ZWFZ
00925 *                                                                 EQW9ZWFZ
00926      IF ECR-CSECODCL (IL)    = ZEROS AND                          EQW9ZWFZ
00927         ECR-CSECODCA (IL) NOT = EFFACE-FIN-ZONE                   EQW9ZWFZ
00928         MOVE TS-ECR-CSECODCO (IL) TO ECR-CSECODCO (IL)            EQW9ZWFZ
00929         MOVE TS-ECR-CSECODCA (IL) TO ECR-CSECODCA (IL)            EQW9ZWFZ
00930      ELSE                                                         EQW9ZWFZ
00931         MOVE ECR-CSECODCO (IL) TO TS-ECR-CSECODCO (IL)            EQW9ZWFZ
00932         MOVE LOW-VALUE       TO TS-ECR-CSECODCA (IL)              EQW9ZWFZ
00933         MOVE '2'             TO ETAT-ECRAN                        EQW9ZWFZ
00934      END-IF.                                                      EQW9ZWFZ
00935 *                                                                 EQW9ZWFZ
00936      IF ECR-IDENTIFL (IL)    = ZEROS AND                          EQW9ZWFZ
00937         ECR-IDENTIFA (IL) NOT = EFFACE-FIN-ZONE                   EQW9ZWFZ
00938         MOVE TS-ECR-IDENTIFO (IL) TO ECR-IDENTIFO (IL)            EQW9ZWFZ
00939         MOVE TS-ECR-IDENTIFA (IL) TO ECR-IDENTIFA (IL)            EQW9ZWFZ
00940      ELSE                                                         EQW9ZWFZ
00941         MOVE ECR-IDENTIFO (IL) TO TS-ECR-IDENTIFO (IL)            EQW9ZWFZ
00942         MOVE LOW-VALUE       TO TS-ECR-IDENTIFA (IL)              EQW9ZWFZ
00943      END-IF.                                                      EQW9ZWFZ
00944 *                                                                 EQW9ZWFZ
00945      IF ECR-DATENTDL (IL)    = ZEROS AND                          EQW9ZWFZ
00946         ECR-DATENTDA (IL) NOT = EFFACE-FIN-ZONE                   EQW9ZWFZ
00947         MOVE TS-ECR-DATENTDO (IL) TO ECR-DATENTDO (IL)            EQW9ZWFZ
00948         MOVE TS-ECR-DATENTDA (IL) TO ECR-DATENTDA (IL)            EQW9ZWFZ
00949      ELSE                                                         EQW9ZWFZ
00950         MOVE ECR-DATENTDO (IL) TO TS-ECR-DATENTDO (IL)            EQW9ZWFZ
00951         MOVE LOW-VALUE       TO TS-ECR-DATENTDA (IL)              EQW9ZWFZ
00952      END-IF.                                                      EQW9ZWFZ
00953 *                                                                 EQW9ZWFZ
00954      IF ECR-DATSORDL (IL)    = ZEROS AND                          EQW9ZWFZ
00955         ECR-DATSORDA (IL) NOT = EFFACE-FIN-ZONE                   EQW9ZWFZ
00956         MOVE TS-ECR-DATSORDO (IL) TO ECR-DATSORDO (IL)            EQW9ZWFZ
00957         MOVE TS-ECR-DATSORDA (IL) TO ECR-DATSORDA (IL)            EQW9ZWFZ
00958      ELSE                                                         EQW9ZWFZ
00959         MOVE ECR-DATSORDO (IL) TO TS-ECR-DATSORDO (IL)            EQW9ZWFZ
00960         MOVE LOW-VALUE       TO TS-ECR-DATSORDA (IL)              EQW9ZWFZ
00961      END-IF.                                                      EQW9ZWFZ
00962 *                                                                 EQW9ZWFZ
00963      IF ECR-MOTIFSCL (IL)    = ZEROS AND                          EQW9ZWFZ
00964         ECR-MOTIFSCA (IL) NOT = EFFACE-FIN-ZONE                   EQW9ZWFZ
00965         MOVE TS-ECR-MOTIFSCO (IL) TO ECR-MOTIFSCO (IL)            EQW9ZWFZ
00966         MOVE TS-ECR-MOTIFSCA (IL) TO ECR-MOTIFSCA (IL)            EQW9ZWFZ
00967      ELSE                                                         EQW9ZWFZ
00968         MOVE ECR-MOTIFSCO (IL) TO TS-ECR-MOTIFSCO (IL)            EQW9ZWFZ
00969         MOVE LOW-VALUE       TO TS-ECR-MOTIFSCA (IL)              EQW9ZWFZ
00970         MOVE '2'             TO ETAT-ECRAN                        EQW9ZWFZ
00971      END-IF.                                                      EQW9ZWFZ
00972 *                                                                 EQW9ZWFZ
00973      IF ECR-STATYPCL (IL)    = ZEROS AND                          EQW9ZWFZ
00974         ECR-STATYPCA (IL) NOT = EFFACE-FIN-ZONE                   EQW9ZWFZ
00975         MOVE TS-ECR-STATYPCO (IL) TO ECR-STATYPCO (IL)            EQW9ZWFZ
00976         MOVE TS-ECR-STATYPCA (IL) TO ECR-STATYPCA (IL)            EQW9ZWFZ
00977      ELSE                                                         EQW9ZWFZ
00978         MOVE ECR-STATYPCO (IL) TO TS-ECR-STATYPCO (IL)            EQW9ZWFZ
00979         MOVE LOW-VALUE       TO TS-ECR-STATYPCA (IL)              EQW9ZWFZ
00980      END-IF.                                                      EQW9ZWFZ
00981 *                                                                 EQW9ZWFZ
00982      IF ECR-PTSCRMXL (IL)    = ZEROS AND                          EQW9ZWFZ
00983         ECR-PTSCRMXA (IL) NOT = EFFACE-FIN-ZONE                   EQW9ZWFZ
00984         MOVE TS-ECR-PTSCRMXO (IL) TO ECR-PTSCRMXO (IL)            EQW9ZWFZ
00985         MOVE TS-ECR-PTSCRMXA (IL) TO ECR-PTSCRMXA (IL)            EQW9ZWFZ
00986      ELSE                                                         EQW9ZWFZ
00987         MOVE ECR-PTSCRMXO (IL) TO TS-ECR-PTSCRMXO (IL)            EQW9ZWFZ
00988         MOVE LOW-VALUE       TO TS-ECR-PTSCRMXA (IL)              EQW9ZWFZ
00989      END-IF.                                                      EQW9ZWFZ
00990 *                                                                 EQW9ZWFZ
00991  FIN-BOUCLE-MATS1.  EXIT.                                         EQW9ZWFZ
00992 *                                                                 EQW9ZWFZ
00993 ***************************************************************** EQW9ZWFZ
00994 ***************************************************************** EQW9ZWFZ
00995 ***********************  MODULE TRAITEMENT ********************** EQW9ZWFZ
00996 ***************************************************************** EQW9ZWFZ
00997 ***************************************************************** EQW9ZWFZ
00998 *                                                                 EQW9ZWFZ
00999  MODULE-TRAITEMENT.                                               EQW9ZWFZ
01000 *-----------------*                                               EQW9ZWFZ
01001      IF  COM-GENE-CODCNV = SPACES OR LOW-VALUE                    EQW9ZWFZ
01002          PERFORM LECT-TS-SUSPENS THRU FLECT-TS-SUSPENS            EQW9ZWFZ
01003          IF (INF-NATMVT OF TS-SUSPENS1 = 'AN' OR 'RP')            EQW9ZWFZ
01004             AND COM-FB-TACHE-START NOT = SPACES                   EQW9ZWFZ
01005             MOVE 'A'                TO COM-FB-CODE-ACTION         EQW9ZWFZ
01006             MOVE ZERO               TO  COM-FB-RANG-TS-LIRE       EQW9ZWFZ
01007             MOVE 'P'                TO  COM-FB-TYPE-TS-LIRE       EQW9ZWFZ
01008             MOVE COM-FB-TACHE-START TO NOM-TACHE-START            EQW9ZWFZ
01009             PERFORM SORTIE-SUITE    THRU FIN-SORTIE-SUITE         EQW9ZWFZ
01010          ELSE                                                     EQW9ZWFZ
01011             MOVE SPACES              TO NOM-TACHE-START           EQW9ZWFZ
01012          END-IF                                                   EQW9ZWFZ
01013 * CE TRAITEMENT LIS LES TS VEHICULES ET PERSONNES, CONSTRUIT      EQW9ZWFZ
01014 * LA TS TECHNIQUE , CRÈE LE FICHIER ORDONNE D'AFFICHAGE           EQW9ZWFZ
01015          PERFORM LECT-TS-PRODUIT-TECH THRU FLECT-TS-PRODUIT-TECH  EQW9ZWFZ
01016          PERFORM APPEL-MODULE-DET THRU FAPPEL-MODULE-DET          EQW9ZWFZ
01017          IF FB90C11-RETCOD NOT = '00' AND '01'                    EQW9ZWFZ
01018             IF COM-GENE-MESINF = SPACE OR LOW-VALUE               EQW9ZWFZ
01019                MOVE FB90C11-MESINF TO COM-GENE-MESINF             EQW9ZWFZ
01020                                       COM-CODERR                  EQW9ZWFZ
01021             END-IF                                                EQW9ZWFZ
01022          ELSE                                                     EQW9ZWFZ
01023             MOVE FB90C11-AJOUT-VEHICULE TO COM-FB-VEHI-AJOUT      EQW9ZWFZ
01024             MOVE FB90C11-SUPPR-VEHICULE TO COM-FB-VEHI-SUPPR      EQW9ZWFZ
01025             MOVE FB90C11-AJOUT-PERSONNE TO COM-FB-PERS-AJOUT      EQW9ZWFZ
01026             MOVE FB90C11-SUPPR-PERSONNE TO COM-FB-PERS-SUPPR      EQW9ZWFZ
01027             MOVE FB90C11-AJOUT-PERS-CF  TO COM-FB-PERS-CF-AJOUT   EQW9ZWFZ
01028             MOVE FB90C11-SUPPR-PERS-CF  TO COM-FB-PERS-CF-SUPPR   EQW9ZWFZ
01029          END-IF                                                   EQW9ZWFZ
01030          PERFORM LECT-TS-PRODUIT-AFF THRU FLECT-TS-PRODUIT-AFF    EQW9ZWFZ
01031      END-IF.                                                      EQW9ZWFZ
01032 *                                                                 EQW9ZWFZ
01033      IF  TRAITEMENT-NORMAL                                        EQW9ZWFZ
01034          PERFORM M-TRAITEMENT-NORMAL THRU                         EQW9ZWFZ
01035                  FIN-M-TRAITEMENT-NORMAL                          EQW9ZWFZ
01036      END-IF.                                                      EQW9ZWFZ
01037 *                                                                 EQW9ZWFZ
01038      IF  TRAITEMENT-AUTOMATIQUE                                   EQW9ZWFZ
01039 * PERMET DE GERER LA REMISE DANS L'ORDRE DU TABLEAU D'AFFICHAGE   EQW9ZWFZ
01040 * EN CAS DE SORTIE OU RESTAURATION                                EQW9ZWFZ
01041          IF COM-FB-CODE-ACTION = 'D' OR 'R'                       EQW9ZWFZ
01042             PERFORM LECT-TS-PRODUIT-TECH                          EQW9ZWFZ
01043                                      THRU FLECT-TS-PRODUIT-TECH   EQW9ZWFZ
01044             PERFORM APPEL-MODULE-DET THRU FAPPEL-MODULE-DET       EQW9ZWFZ
01045             IF FB90C11-RETCOD NOT = '00' AND '01'                 EQW9ZWFZ
01046                IF COM-GENE-MESINF = SPACE OR LOW-VALUE            EQW9ZWFZ
01047                   MOVE FB90C11-MESINF TO COM-GENE-MESINF          EQW9ZWFZ
01048                                          COM-CODERR               EQW9ZWFZ
01049                END-IF                                             EQW9ZWFZ
01050             ELSE                                                  EQW9ZWFZ
01051                MOVE FB90C11-AJOUT-VEHICULE TO COM-FB-VEHI-AJOUT   EQW9ZWFZ
01052                MOVE FB90C11-SUPPR-VEHICULE TO COM-FB-VEHI-SUPPR   EQW9ZWFZ
01053                MOVE FB90C11-AJOUT-PERSONNE TO COM-FB-PERS-AJOUT   EQW9ZWFZ
01054                MOVE FB90C11-SUPPR-PERSONNE TO COM-FB-PERS-SUPPR   EQW9ZWFZ
01055                MOVE FB90C11-AJOUT-PERS-CF  TO COM-FB-PERS-CF-AJOUTEQW9ZWFZ
01056                MOVE FB90C11-SUPPR-PERS-CF  TO COM-FB-PERS-CF-SUPPREQW9ZWFZ
01057             END-IF                                                EQW9ZWFZ
01058             PERFORM LECT-TS-PRODUIT-AFF THRU FLECT-TS-PRODUIT-AFF EQW9ZWFZ
01059          END-IF                                                   EQW9ZWFZ
01060          PERFORM M-TRAITEMENT-AUTOMATIQUE THRU                    EQW9ZWFZ
01061                  FIN-M-TRAITEMENT-AUTOMATIQUE                     EQW9ZWFZ
01062      END-IF.                                                      EQW9ZWFZ
01063 *                                                                 EQW9ZWFZ
01064  FIN-MODULE-TRAITEMENT.  EXIT.                                    EQW9ZWFZ
01065 *                                                                 EQW9ZWFZ
01066 *                                                                 EQW9ZWFZ
01067 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01068 * MODULE DE BASE   * FB01 * TRAITEMENT AUTOMATIQUE                EQW9ZWFZ
01069 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01070 *                                                               * EQW9ZWFZ
01071 *                -------------------------------                * EQW9ZWFZ
01072 *                I   TRAITEMENT AUTOMATIQUE    I                * EQW9ZWFZ
01073 *                -------------------------------                * EQW9ZWFZ
01074 *                               I                               * EQW9ZWFZ
01075 *                               I                               * EQW9ZWFZ
01076 *                -------------------------------                * EQW9ZWFZ
01077 *                I  REMPLISSAGE FORMAT ECRAN   I                * EQW9ZWFZ
01078 *                -------------------------------                * EQW9ZWFZ
01079 *                                                               * EQW9ZWFZ
01080 *                                                               * EQW9ZWFZ
01081 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01082 *                                                                 EQW9ZWFZ
01083  M-TRAITEMENT-AUTOMATIQUE.                                        EQW9ZWFZ
01084 *------------------------*                                        EQW9ZWFZ
01085 *                                                                 EQW9ZWFZ
01086 * RECHERCHE DES ENCHAINEMENTS DES CONVERSATIONS                   EQW9ZWFZ
01087 * SI PREMIERE TRANSACTION DE LA CONVERSATION                      EQW9ZWFZ
01088 *                                                                 EQW9ZWFZ
01089      IF COM-GENE-CODCNV NOT = SPACES AND                          EQW9ZWFZ
01090                               LOW-VALUE                           EQW9ZWFZ
01091         PERFORM INIT-CONV THRU                                    EQW9ZWFZ
01092                 FIN-INIT-CONV                                     EQW9ZWFZ
01093      ELSE                                                         EQW9ZWFZ
01094         MOVE COM-GENE-LNGCNV TO LONG-TS                           EQW9ZWFZ
01095         PERFORM READ-TS-CONF THRU                                 EQW9ZWFZ
01096                 FIN-READ-TS-CONF                                  EQW9ZWFZ
01097         PERFORM CONTROLE-CONV     THRU                            EQW9ZWFZ
01098                 FIN-CONTROLE-CONV                                 EQW9ZWFZ
01099      END-IF.                                                      EQW9ZWFZ
01100 *                                                                 EQW9ZWFZ
01101      PERFORM INIT-RECH-CONV-LIBRE THRU                            EQW9ZWFZ
01102              FIN-INIT-RECH-CONV-LIBRE.                            EQW9ZWFZ
01103 *                                                                 EQW9ZWFZ
01104      PERFORM POSITIONNEMENT-FICHIER   THRU                        EQW9ZWFZ
01105              FIN-POSITIONNEMENT-FICHIER.                          EQW9ZWFZ
01106 *                                                                 EQW9ZWFZ
01107      PERFORM REMPLISSAGE-FORMAT-ECRAN THRU                        EQW9ZWFZ
01108              FIN-REMPLISSAGE-FORMAT-ECRAN.                        EQW9ZWFZ
01109      PERFORM SORTIE-AFFICHAGE-FORMAT THRU                         EQW9ZWFZ
01110              FIN-SORTIE-AFFICHAGE-FORMAT.                         EQW9ZWFZ
01111 *                                                                 EQW9ZWFZ
01112  FIN-M-TRAITEMENT-AUTOMATIQUE. EXIT.                              EQW9ZWFZ
01113 *                                                                 EQW9ZWFZ
01114 *                                                                 EQW9ZWFZ
01115 ***************************************************************   EQW9ZWFZ
01116 * LECTURE TABLE AACONVXX POSTE = COM-GENE-CODCONV POUR AVOIR      EQW9ZWFZ
01117 * LE LIBELLE  DE LA CONVERSATION                                  EQW9ZWFZ
01118 ***************************************************************   EQW9ZWFZ
01119  INIT-CONV.                                                       EQW9ZWFZ
01120 *---------*                                                       EQW9ZWFZ
01121      MOVE  SPACES                   TO XSPIPARM.                  EQW9ZWFZ
01122      MOVE 'AACONV'                  TO TABLE-PREF.                EQW9ZWFZ
01123      MOVE COM-GENE-CODCIE-PRINCIPAL TO TABLE-SUFF.                EQW9ZWFZ
01124      MOVE COM-GENE-CODCNV           TO REF-POSTE OF XSPIPARM.     EQW9ZWFZ
01125      PERFORM LECT-SPI-CONV THRU                                   EQW9ZWFZ
01126              FIN-LECT-SPI-CONV.                                   EQW9ZWFZ
01127      IF NON-TROUVE                                                EQW9ZWFZ
01128         MOVE  1      TO KONTROL                                   EQW9ZWFZ
01129         MOVE 'SQ006' TO COM-GENE-MESANO                           EQW9ZWFZ
01130                         COM-CODERR                                EQW9ZWFZ
01131         PERFORM MODULE-SORTIE THRU                                EQW9ZWFZ
01132                 FIN-MODULE-SORTIE                                 EQW9ZWFZ
01133      END-IF.                                                      EQW9ZWFZ
01134 * SI TROUVE                                                       EQW9ZWFZ
01135      MOVE LIB-CONV   TO COM-GENE-LIBCNV.                          EQW9ZWFZ
01136 *                                                                 EQW9ZWFZ
01137 * SI ON EST DANS LA PREMIERE TRANSACTION D'UNE CONVERSATION       EQW9ZWFZ
01138 * 1- ON PASSE DANS L'INTERFACE  AUAAL04  POUR  CONTROLE D'ACCES   EQW9ZWFZ
01139 *             SI   OK VERS 2. SI NON  LEVEL-SUPERIEUR             EQW9ZWFZ
01140 * 2- ON ACCEDE A L'INTERFACE POUR CONSTRUCTION DE LA TS           EQW9ZWFZ
01141 *    DE CONFIDENTIALITE                                           EQW9ZWFZ
01142 * 3- ON STOCKE EN COMMAREA TOUS LES ENCHAINEMENTS POSSIBLES POUR  EQW9ZWFZ
01143 *    CETTE CONVERSATION SI ELLE EST AUTORISEE                     EQW9ZWFZ
01144 *                                                                 EQW9ZWFZ
01145      PERFORM INTERFACE-CONTROLE-ACCES  THRU                       EQW9ZWFZ
01146              FIN-INTERFACE-CONTROLE-ACCES.                        EQW9ZWFZ
01147      IF COM-AU-MESSAGE NOT = SPACES AND LOW-VALUE                 EQW9ZWFZ
01148         MOVE    1      TO KONTROL                                 EQW9ZWFZ
01149         MOVE    COM-AU-MESSAGE TO COM-GENE-MESANO                 EQW9ZWFZ
01150                                   COM-CODERR                      EQW9ZWFZ
01151 *                                                                 EQW9ZWFZ
01152         PERFORM SORTIE-LEVEL-SUPERIEUR  THRU                      EQW9ZWFZ
01153             FIN-SORTIE-LEVEL-SUPERIEUR                            EQW9ZWFZ
01154      END-IF.                                                      EQW9ZWFZ
01155 *                                                                 EQW9ZWFZ
01156 *    FABRICATION  DE LA TS CONFIDENTIALITE CONVERSATION           EQW9ZWFZ
01157 *            PAR L'INTERFACE AUAAL00                              EQW9ZWFZ
01158      PERFORM INTERFACE-CONFIDENTIALITE THRU                       EQW9ZWFZ
01159              FIN-INTERFACE-CONFIDENTIALITE.                       EQW9ZWFZ
U3319      MOVE    COM-GENE-CODCNV  TO  COM-GENE-CODCNV-SAUVE.          EFUTSQP3
01160      MOVE    SPACES        TO COM-GENE-CODCNV.                    EQW9ZWFZ
01161      MOVE COM-GENE-LNGCNV TO LONG-TS.                             EQW9ZWFZ
01162      PERFORM READ-TS-CONF THRU                                    EQW9ZWFZ
01163              FIN-READ-TS-CONF.                                    EQW9ZWFZ
01164      MOVE    1 TO IA.                                             EQW9ZWFZ
01165      PERFORM STOCKAGE-ENCH-CONV  THRU                             EQW9ZWFZ
01166              FIN-STOCKAGE-ENCH-CONV 51 TIMES.                     EQW9ZWFZ
01167 *                                                                 EQW9ZWFZ
01168  FIN-INIT-CONV. EXIT.                                             EQW9ZWFZ
01169 *                                                                 EQW9ZWFZ
01170 * **********************************************************      EQW9ZWFZ
01171 * LECTURE DE LA TS  DE CONFIDENTIALITE  CONVERSATION              EQW9ZWFZ
01172 * **********************************************************      EQW9ZWFZ
01173  LECT-TS-CONF-CONV.                                               EQW9ZWFZ
01174 *-----------------*                                               EQW9ZWFZ
01175      MOVE COM-GENE-LNGCNV TO LONG-TS.                             EQW9ZWFZ
01176      MOVE IDENT-TS-CONF   TO IDENT-TS.                            EQW9ZWFZ
01177      MOVE 1               TO RANG-TS.                             EQW9ZWFZ
01178      PERFORM READ-TS THRU                                         EQW9ZWFZ
01179              FIN-READ-TS.                                         EQW9ZWFZ
01180      IF TROUVE                                                    EQW9ZWFZ
01181         MOVE Z-INOUT TO CONF-AUAAI                                EQW9ZWFZ
01182      END-IF.                                                      EQW9ZWFZ
01183 *                                                                 EQW9ZWFZ
01184  FIN-LECT-TS-CONF-CONV. EXIT.                                     EQW9ZWFZ
01185 *                                                                 EQW9ZWFZ
01186 *                                                                 EQW9ZWFZ
01187 * **********************************************************      EQW9ZWFZ
01188  CONTROLE-CONV.                                                   EQW9ZWFZ
01189 *-------------*                                                   EQW9ZWFZ
01190 *     VOIR LA TECHNIQUE  DANS LE MEME PARAGRAPHE CONTROLE-CONV    EQW9ZWFZ
01191 *     DU PROGRAMME  DE  MENU UNIQUE                               EQW9ZWFZ
01192 *                                                                 EQW9ZWFZ
01193 *    IF CONTROLE NON OK                                           EQW9ZWFZ
01194 *           MOVE 'SQ016' TO COM-GENE-MESANO                       EQW9ZWFZ
01195 *                           COM-CODERR                            EQW9ZWFZ
01196 *           PERFORM SORTIE-LEVEL-SUPERIEUR THRU                   EQW9ZWFZ
01197 *                   FIN-SORTIE-LEVEL-SUPERIEUR                    EQW9ZWFZ
01198 *    END-IF.                                                      EQW9ZWFZ
01199 *                                                                 EQW9ZWFZ
01200  FIN-CONTROLE-CONV.  EXIT.                                        EQW9ZWFZ
01201 *                                                                 EQW9ZWFZ
01202 * **********************************************************      EQW9ZWFZ
01203  LECT-SPI-CONV.                                                   EQW9ZWFZ
01204 *-------------*                                                   EQW9ZWFZ
01205      MOVE  'GP'                   TO FONCTION  OF XSPIPARM.       EQW9ZWFZ
01206      MOVE  IDENT-TABLE            TO CODTAB    OF XSPIPARM.       EQW9ZWFZ
01207      MOVE  '= '                   TO OPERATEUR OF XSPIPARM.       EQW9ZWFZ
01208      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9ZWFZ
01209      MOVE SPACES                  TO TAB-AACONV.                  EQW9ZWFZ
01210      IF  RETCOD OF XSPIPARM  = ZERO                               EQW9ZWFZ
01211          MOVE 0                   TO CODE-RETOUR                  EQW9ZWFZ
01212          MOVE IOAREA OF XSPIPARM  TO TAB-AACONV                   EQW9ZWFZ
01213      ELSE                                                         EQW9ZWFZ
01214          MOVE 1 TO CODE-RETOUR                                    EQW9ZWFZ
01215      END-IF.                                                      EQW9ZWFZ
01216  FIN-LECT-SPI-CONV. EXIT.                                         EQW9ZWFZ
01217 * **********************************************************      EQW9ZWFZ
01218  INIT-RECH-CONV-LIBRE.                                            EQW9ZWFZ
01219 * ------------------ *                                            EQW9ZWFZ
01220      MOVE 1  TO IA.                                               EQW9ZWFZ
01221      PERFORM RECHERCHE-CONV-LIBRE THRU                            EQW9ZWFZ
01222              FIN-RECHERCHE-CONV-LIBRE                             EQW9ZWFZ
01223              UNTIL COM-GENE-PILCNV(IA) = SPACES OR                EQW9ZWFZ
01224                    LOW-VALUE                                      EQW9ZWFZ
01225              OR IA > 50                                           EQW9ZWFZ
01226              OR COM-GENE-PILCNV(IA) = NOM-TACHE.                  EQW9ZWFZ
01227      IF IA > 50                                                   EQW9ZWFZ
01228         MOVE 1 TO KONTROL                                         EQW9ZWFZ
01229         MOVE 'SQ005'   TO COM-GENE-MESANO                         EQW9ZWFZ
01230                           COM-CODERR                              EQW9ZWFZ
01231         PERFORM MODULE-SORTIE THRU                                EQW9ZWFZ
01232                 FIN-MODULE-SORTIE                                 EQW9ZWFZ
01233      ELSE                                                         EQW9ZWFZ
01234         MOVE NOM-TACHE TO COM-GENE-PILCNV(IA)                     EQW9ZWFZ
01235         MOVE IA        TO COM-GENE-INDCNV                         EQW9ZWFZ
01236      END-IF.                                                      EQW9ZWFZ
01237  FIN-INIT-RECH-CONV-LIBRE.  EXIT.                                 EQW9ZWFZ
01238 *                                                                 EQW9ZWFZ
01239 ***************************************************************   EQW9ZWFZ
01240 * RECHERCHE DANS LA COMMAREA D'UN POSTE LIBRE POUR STOCKER LE     EQW9ZWFZ
01241 * CODE TRANSACTION DE LA CONVERSATION EN COURS                    EQW9ZWFZ
01242 ***************************************************************   EQW9ZWFZ
01243  RECHERCHE-CONV-LIBRE.                                            EQW9ZWFZ
01244 *--------------------*                                            EQW9ZWFZ
01245      IF COM-GENE-PILCNV(IA) NOT = SPACES AND LOW-VALUE            EQW9ZWFZ
01246         ADD 1 TO IA                                               EQW9ZWFZ
01247      END-IF.                                                      EQW9ZWFZ
01248  FIN-RECHERCHE-CONV-LIBRE. EXIT.                                  EQW9ZWFZ
01249 *                                                                 EQW9ZWFZ
01250  STOCKAGE-ENCH-CONV.                                              EQW9ZWFZ
01251 *------------------*                                              EQW9ZWFZ
01252       MOVE COD-TRN-ECR(IA)  TO COM-GENE-ENCCNV-CODTRN(IA).        EQW9ZWFZ
01253       MOVE COD-MNE-ECR(IA)  TO COM-GENE-ENCCNV-CODMNE(IA).        EQW9ZWFZ
01254       ADD 1 TO IA.                                                EQW9ZWFZ
01255  FIN-STOCKAGE-ENCH-CONV. EXIT.                                    EQW9ZWFZ
01256 *                                                                 EQW9ZWFZ
01257 ***************************************************************   EQW9ZWFZ
01258  POSITIONNEMENT-FICHIER.                                          EQW9ZWFZ
01259 *----------------------*    PAGING                                EQW9ZWFZ
01260 *                                                                 EQW9ZWFZ
01261       MOVE LOW-VALUE  TO Z-MAP.                                   EQW9ZWFZ
01262 *                                                                 EQW9ZWFZ
01263       IF PREMIER-PASSAGE                                          EQW9ZWFZ
01264         MOVE  CODE-PREMIERE TO Z-FONCTION                         EQW9ZWFZ
01265         MOVE  ' '           TO COM-FB-CODE-ACTION                 EQW9ZWFZ
01266         MOVE  ZERO          TO COM-FB-RANG-TS-LIRE                EQW9ZWFZ
01267         MOVE  ' '           TO COM-FB-TYPE-TS-LIRE                EQW9ZWFZ
01268         MOVE  SPACE         TO COM-GENE-REAF                      EQW9ZWFZ
01269       END-IF.                                                     EQW9ZWFZ
01270 *                                                                 EQW9ZWFZ
01271 *     SI PREMIERE PAGE                                            EQW9ZWFZ
01272 *                                                                 EQW9ZWFZ
01273       IF PREMIERE                                                 EQW9ZWFZ
01274 *                                                                 EQW9ZWFZ
01275 *  GESTION DU RETOUR DE L'ECRAN D'AIDE MA84 RETOUR SUR LA BONNE   EQW9ZWFZ
01276 *  PAGE                                                           EQW9ZWFZ
01277 *                                                                 EQW9ZWFZ
01278          IF COM-FB-PAGE-ENCOUR NUMERIC                            EQW9ZWFZ
01279             AND COM-FB-PAGE-ENCOUR > ZERO                         EQW9ZWFZ
01280             MOVE COM-FB-PAGE-ENCOUR  TO COM-ITEM-AFFICH           EQW9ZWFZ
01281          ELSE                                                     EQW9ZWFZ
01282             MOVE 1     TO COM-ITEM-AFFICH                         EQW9ZWFZ
01283          END-IF                                                   EQW9ZWFZ
01284          PERFORM REMPLISSAGE-FORMAT-ECRAN THRU                    EQW9ZWFZ
01285                     FIN-REMPLISSAGE-FORMAT-ECRAN                  EQW9ZWFZ
01286          PERFORM SORTIE-AFFICHAGE-FORMAT THRU                     EQW9ZWFZ
01287                     FIN-SORTIE-AFFICHAGE-FORMAT                   EQW9ZWFZ
01288       END-IF.                                                     EQW9ZWFZ
01289 *                                                                 EQW9ZWFZ
01290 * PAGE SUIVANTE                                                   EQW9ZWFZ
01291 *                                                                 EQW9ZWFZ
01292       IF SUIVANTE                                                 EQW9ZWFZ
01293          ADD   1    TO    COM-ITEM-AFFICH                         EQW9ZWFZ
01294          IF COM-ITEM-AFFICH  >  COM-ITEM-TOTAL                    EQW9ZWFZ
01295             MOVE COM-ITEM-TOTAL TO COM-ITEM-AFFICH                EQW9ZWFZ
01296             MOVE ZONE-TS-ECRAN  TO Z-MAP                          EQW9ZWFZ
01297 *                                                                 EQW9ZWFZ
01298 *  CODE ERREUR CORRESPONDANT AU MESSAGE                           EQW9ZWFZ
01299 *  VOUS ETES SUR LA DERNIERE PAGE                                 EQW9ZWFZ
01300 *                                                                 EQW9ZWFZ
01301             MOVE 'SQ014'        TO COM-CODERR                     EQW9ZWFZ
01302                                    COM-GENE-MESANO                EQW9ZWFZ
01303             MOVE 1              TO KONTROL                        EQW9ZWFZ
01304             MOVE CURSEUR        TO ECR-AJOUTCONL                  EQW9ZWFZ
01305             PERFORM SORTIE-ERREUR THRU                            EQW9ZWFZ
01306                     FIN-SORTIE-ERREUR                             EQW9ZWFZ
01307          ELSE                                                     EQW9ZWFZ
01308             MOVE 'O'            TO COM-GENE-REAF                  EQW9ZWFZ
01309             PERFORM REMPLISSAGE-FORMAT-ECRAN THRU                 EQW9ZWFZ
01310                     FIN-REMPLISSAGE-FORMAT-ECRAN                  EQW9ZWFZ
01311             PERFORM SORTIE-AFFICHAGE-FORMAT THRU                  EQW9ZWFZ
01312                     FIN-SORTIE-AFFICHAGE-FORMAT                   EQW9ZWFZ
01313          END-IF                                                   EQW9ZWFZ
01314       END-IF.                                                     EQW9ZWFZ
01315 *                                                                 EQW9ZWFZ
01316 *  PAGE PRECEDENTE                                                EQW9ZWFZ
01317 *                                                                 EQW9ZWFZ
01318       IF PRECEDENTE                                               EQW9ZWFZ
01319          SUBTRACT  1 FROM COM-ITEM-AFFICH                         EQW9ZWFZ
01320          IF COM-ITEM-AFFICH < 1                                   EQW9ZWFZ
01321             MOVE  1            TO   COM-ITEM-AFFICH               EQW9ZWFZ
01322             MOVE ZONE-TS-ECRAN TO Z-MAP                           EQW9ZWFZ
01323 *                                                                 EQW9ZWFZ
01324 *  CODE ERREUR CORRESPONDANT AU MESSAGE                           EQW9ZWFZ
01325 *  VOUS ETES SUR LA PREMIERE PAGE                                 EQW9ZWFZ
01326 *                                                                 EQW9ZWFZ
01327             MOVE 'SQ015'    TO   COM-CODERR                       EQW9ZWFZ
01328                                  COM-GENE-MESANO                  EQW9ZWFZ
01329             MOVE 1          TO KONTROL                            EQW9ZWFZ
01330             MOVE CURSEUR    TO ECR-AJOUTCONL                      EQW9ZWFZ
01331             PERFORM SORTIE-ERREUR THRU                            EQW9ZWFZ
01332                     FIN-SORTIE-ERREUR                             EQW9ZWFZ
01333          ELSE                                                     EQW9ZWFZ
01334             MOVE 'O'            TO COM-GENE-REAF                  EQW9ZWFZ
01335             PERFORM REMPLISSAGE-FORMAT-ECRAN THRU                 EQW9ZWFZ
01336                     FIN-REMPLISSAGE-FORMAT-ECRAN                  EQW9ZWFZ
01337             PERFORM SORTIE-AFFICHAGE-FORMAT THRU                  EQW9ZWFZ
01338                     FIN-SORTIE-AFFICHAGE-FORMAT                   EQW9ZWFZ
01339          END-IF                                                   EQW9ZWFZ
01340       END-IF.                                                     EQW9ZWFZ
01341 *                                                                 EQW9ZWFZ
01342  FIN-POSITIONNEMENT-FICHIER.  EXIT.                               EQW9ZWFZ
01343 *                                                                 EQW9ZWFZ
01344 ******************************************************************EQW9ZWFZ
01345 * LECTURE SUCCESSIVE DES TS PRODUIT  ET CREATION WSS-AFFICHAGE   *EQW9ZWFZ
01346 ******************************************************************EQW9ZWFZ
01347  LECT-TS-PRODUIT-AFF.                                             EQW9ZWFZ
01348 *                                                                 EQW9ZWFZ
01349       MOVE ZERO                  TO IB.                           EQW9ZWFZ
01350 *  INITIALISATION DU TABLEAU D'AFFICHAGE                          EQW9ZWFZ
01351 *  ET DES COMPTEUR DE COMMAREA TYPE PERSONNE                      EQW9ZWFZ
01352       INITIALIZE WSS-TABLEAU-AFF.                                 EQW9ZWFZ
01353       MOVE ZERO                  TO COM-FB-NBRE-PERS-CF.          EQW9ZWFZ
01354       MOVE ZERO                  TO COM-FB-NBRE-PERS-CJ.          EQW9ZWFZ
01355       MOVE ZERO                  TO COM-FB-NBRE-PERS-EN.          EQW9ZWFZ
01356       MOVE ZERO                  TO COM-FB-NBRE-PERS-PM.          EQW9ZWFZ
01357 *                                                                 EQW9ZWFZ
01358       MOVE ZERO       TO RANG-TS-PERS.                            EQW9ZWFZ
01359       MOVE 'N'        TO WSS-FIN-PERS.                            EQW9ZWFZ
01360       PERFORM READ-TS-PERSONNE-ENC THRU FREAD-TS-PERSONNE-ENC     EQW9ZWFZ
01361               UNTIL WSS-FIN-PERS = 'O'                            EQW9ZWFZ
01362 *                                                                 EQW9ZWFZ
01363       MOVE ZERO       TO RANG-TS-VEHI.                            EQW9ZWFZ
01364       MOVE 'N'        TO WSS-FIN-VEHI.                            EQW9ZWFZ
01365       PERFORM READ-TS-VEHICULE-ENC THRU FREAD-TS-VEHICULE-ENC     EQW9ZWFZ
01366               UNTIL WSS-FIN-VEHI = 'O'                            EQW9ZWFZ
01367 *                                                                 EQW9ZWFZ
01368       MOVE ZERO       TO RANG-TS-PERS.                            EQW9ZWFZ
01369       MOVE 'N'        TO WSS-FIN-PERS.                            EQW9ZWFZ
01370       PERFORM READ-TS-PERSONNE-SOR THRU FREAD-TS-PERSONNE-SOR     EQW9ZWFZ
01371               UNTIL WSS-FIN-PERS = 'O'                            EQW9ZWFZ
01372 *                                                                 EQW9ZWFZ
01373       MOVE ZERO       TO RANG-TS-VEHI.                            EQW9ZWFZ
01374       MOVE 'N'        TO WSS-FIN-VEHI.                            EQW9ZWFZ
01375       PERFORM READ-TS-VEHICULE-SOR THRU FREAD-TS-VEHICULE-SOR     EQW9ZWFZ
01376               UNTIL WSS-FIN-VEHI = 'O'                            EQW9ZWFZ
01377 *                                                                 EQW9ZWFZ
01378 * DETERMINATION DU NOMBRE DE PAGE TOTALE                          EQW9ZWFZ
01379       COMPUTE COM-ITEM-TOTAL = 1 + (IB - 1) / 10.                 EQW9ZWFZ
01380 *                                                                 EQW9ZWFZ
01381 *                                                                 EQW9ZWFZ
01382  FLECT-TS-PRODUIT-AFF.                                            EQW9ZWFZ
01383 ******************************************************************EQW9ZWFZ
01384 * LECTURE SUCCESSIVE DES TS PRODUIT ET CREATION DE LA TS TECHNIQUEEQW9ZWFZ
01385 ******************************************************************EQW9ZWFZ
01386  LECT-TS-PRODUIT-TECH.                                            EQW9ZWFZ
01387 *                                                                 EQW9ZWFZ
01388 *  INITIALISATION DE LA TS TECHNIQUE                              EQW9ZWFZ
01389       PERFORM DEL-TS-TECHNIQUE   THRU FDEL-TS-TECHNIQUE.          EQW9ZWFZ
01390 *                                                                 EQW9ZWFZ
01391 *                                                                 EQW9ZWFZ
01392       MOVE ZERO       TO RANG-TS-PERS.                            EQW9ZWFZ
01393       MOVE ZERO       TO I-PERS.                                  EQW9ZWFZ
01394       MOVE 'N'        TO WSS-FIN-PERS.                            EQW9ZWFZ
01395       PERFORM VARYING RANG-TS-PERS FROM 1 BY 1                    EQW9ZWFZ
01396               UNTIL WSS-FIN-PERS = 'O'                            EQW9ZWFZ
01397          PERFORM READ-TS-PERSONNE   THRU FREAD-TS-PERSONNE        EQW9ZWFZ
01398          IF WSS-FIN-PERS = 'N'                                    EQW9ZWFZ
01399             ADD 1                      TO I-PERS                  EQW9ZWFZ
01400             PERFORM ENREG-TS-TECH-PERS THRU FENREG-TS-TECH-PERS   EQW9ZWFZ
01401          END-IF                                                   EQW9ZWFZ
01402       END-PERFORM.                                                EQW9ZWFZ
01403 *                                                                 EQW9ZWFZ
01404       MOVE ZERO       TO RANG-TS-VEHI.                            EQW9ZWFZ
F2980       MOVE ZERO       TO COM-FB-NBRE-VEHI-USA.                    EQW9ZWFZ
01405       MOVE ZERO       TO I-VEHI.                                  EQW9ZWFZ
01406       MOVE ZERO       TO NB-CARTE-VERTE OF FBMISPTR-IT1.          EQW9ZWFZ
01407       MOVE ZERO       TO I-VEHI-CIGAL.                            EQW9ZWFZ
01408       MOVE 'N'        TO WSS-FIN-VEHI.                            EQW9ZWFZ
01409       PERFORM VARYING RANG-TS-VEHI FROM 1 BY 1                    EQW9ZWFZ
01410               UNTIL WSS-FIN-VEHI = 'O'                            EQW9ZWFZ
01411          PERFORM READ-TS-VEHICULE     THRU FREAD-TS-VEHICULE      EQW9ZWFZ
01412          IF WSS-FIN-VEHI = 'N'                                    EQW9ZWFZ
01413             ADD 1                      TO I-VEHI                  EQW9ZWFZ
01414             PERFORM ALIM-CIGAL         THRU FALIM-CIGAL           EQW9ZWFZ
F2980             PERFORM ALIM-VDR           THRU FALIM-VDR             EQW9ZWFZ
F2980             PERFORM ALIM-VEHI-USA      THRU FALIM-VEHI-USA        EQW9ZWFZ
01415             PERFORM ENREG-TS-TECH-VEHI THRU FENREG-TS-TECH-VEHI   EQW9ZWFZ
01416          END-IF                                                   EQW9ZWFZ
01417       END-PERFORM.                                                EQW9ZWFZ
01418 *                                                                 EQW9ZWFZ
01419       PERFORM ECR-TS-TECHNIQUE THRU FECR-TS-TECHNIQUE.            EQW9ZWFZ
01420 *                                                                 EQW9ZWFZ
01421  FLECT-TS-PRODUIT-TECH.                                           EQW9ZWFZ
01422 ******************************************************************EQW9ZWFZ
01423 * CHARGEMENT TABLEAU AFFICHAGE POUR LES PERSONNES                *EQW9ZWFZ
01424 ******************************************************************EQW9ZWFZ
01425 *                                                                 EQW9ZWFZ
01426  CHARG-TAB-AFF-PERS.                                              EQW9ZWFZ
01427 *------------------*                                              EQW9ZWFZ
01428                                                                   EQW9ZWFZ
01429       ADD 1 TO IB.                                                EQW9ZWFZ
01430       MOVE 'P'                              TO WSS-TYPTS (IB).    EQW9ZWFZ
01431       MOVE RANG-TS-PERS                     TO WSS-RANTS (IB).    EQW9ZWFZ
01432 * ALGORYTHME PERMETTANT D'ACCOLLER LE NOM ET PRENOM               EQW9ZWFZ
01433       IF PERNOML OF DONNEES-PERSONNE (1) NOT = SPACES             EQW9ZWFZ
01434          PERFORM VARYING I FROM 20 BY -1                          EQW9ZWFZ
01435                  UNTIL (PERNOML OF DONNEES-PERSONNE (1) (I:1)     EQW9ZWFZ
01436                  NOT = SPACES)                                    EQW9ZWFZ
01437          END-PERFORM                                              EQW9ZWFZ
01438          STRING PERNOML OF DONNEES-PERSONNE (1) (1:I)             EQW9ZWFZ
01439                 ' ' PERPREL OF DONNEES-PERSONNE (1)               EQW9ZWFZ
01440                 DELIMITED BY SIZE INTO                            EQW9ZWFZ
01441                                                WSS-IDENT (IB)     EQW9ZWFZ
01442       ELSE                                                        EQW9ZWFZ
01443          STRING ' ' PERPREL OF DONNEES-PERSONNE (1)               EQW9ZWFZ
01444                 DELIMITED BY SIZE INTO                            EQW9ZWFZ
01445                                                WSS-IDENT (IB)     EQW9ZWFZ
01446       END-IF.                                                     EQW9ZWFZ
01447       MOVE RPERENTD OF DONNEES-PERSONNE (1) TO WSS-SSAAMMJJ.      EQW9ZWFZ
01448       MOVE CORRESPONDING WSS-SSAAMMJJ   TO WSS-JJMMSSAA .         EQW9ZWFZ
01449       MOVE WSS-JJMMSSAA                 TO WSS-DATENT(IB).        EQW9ZWFZ
01450       IF RPERSORD OF DONNEES-PERSONNE (1) = '99999999'            EQW9ZWFZ
01451          MOVE SPACES                    TO WSS-DATSOR(IB)         EQW9ZWFZ
01452       ELSE                                                        EQW9ZWFZ
01453          MOVE RPERSORD OF DONNEES-PERSONNE (1)                    EQW9ZWFZ
01454                                          TO WSS-SSAAMMJJ          EQW9ZWFZ
01455          MOVE CORRESPONDING WSS-SSAAMMJJ TO WSS-JJMMSSAA          EQW9ZWFZ
01456          MOVE WSS-JJMMSSAA               TO WSS-DATSOR (IB)       EQW9ZWFZ
01457       END-IF.                                                     EQW9ZWFZ
01458       MOVE PERMTFC  OF DONNEES-PERSONNE (1)                       EQW9ZWFZ
01459                                                 TO WSS-MOTIFC(IB).EQW9ZWFZ
01460       IF PERSTAC  OF DONNEES-PERSONNE (1) = 'EN'                  EQW9ZWFZ
01461          IF PRMTYPC OF DONNEES-PERSONNE(1, 1) = SPACES            EQW9ZWFZ
01462             AND PRMTYPC OF DONNEES-PERSONNE(1, 2) = SPACES        EQW9ZWFZ
01463             STRING PERSTAC  OF DONNEES-PERSONNE (1)               EQW9ZWFZ
01464                    'SP' DELIMITED BY SIZE                         EQW9ZWFZ
01465                                               INTO WSS-STATYP(IB) EQW9ZWFZ
01466          ELSE                                                     EQW9ZWFZ
01467             STRING PERSTAC  OF DONNEES-PERSONNE (1)               EQW9ZWFZ
01468                    'AP' DELIMITED BY SIZE                         EQW9ZWFZ
01469                                               INTO WSS-STATYP(IB) EQW9ZWFZ
01470          END-IF                                                   EQW9ZWFZ
01471       ELSE                                                        EQW9ZWFZ
01472          MOVE PERSTAC  OF DONNEES-PERSONNE (1)                    EQW9ZWFZ
01473                                                 TO WSS-STATYP(IB) EQW9ZWFZ
01474       END-IF.                                                     EQW9ZWFZ
01475 *                                                                 EQW9ZWFZ
F37105     PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10                   EQW9ZWFZ
01477              OR COPTYPC  OF COEFFICIENT-PERSONNE (1, I) = '1'     EQW9ZWFZ
01478      END-PERFORM.                                                 EQW9ZWFZ
F37105     IF I > 10                                                    EQW9ZWFZ
01480         MOVE SPACES                                               EQW9ZWFZ
01481                               TO   WSS-PTSCRM (IB)                EQW9ZWFZ
01482      END-IF.                                                      EQW9ZWFZ
01483      IF COPTYPC  OF COEFFICIENT-PER (1, I) = '1'                  EQW9ZWFZ
01484         IF RCOPTAUT OF COEFFICIENT-PER (1, I)                     EQW9ZWFZ
01485                               = SPACES OR LOW-VALUE               EQW9ZWFZ
01486            MOVE SPACES                                            EQW9ZWFZ
01487                               TO   WSS-PTSCRM (IB)                EQW9ZWFZ
01488         ELSE                                                      EQW9ZWFZ
01489            MOVE COPTAUT OF COEFFICIENT-PER (1, I)                 EQW9ZWFZ
01490                               TO   WSS-COEF-ENTIER                EQW9ZWFZ
01491            MOVE WSS-COEF-ENTIER                                   EQW9ZWFZ
01492                               TO   WSS-PTSCRM (IB)                EQW9ZWFZ
01493         END-IF                                                    EQW9ZWFZ
01494      END-IF.                                                      EQW9ZWFZ
01495 *                                                                 EQW9ZWFZ
01496  FIN-CHARG-TAB-AFF-PERS.                                          EQW9ZWFZ
01497 *                                                                 EQW9ZWFZ
01498 ******************************************************************EQW9ZWFZ
01499 * CHARGEMENT TABLEAU AFFICHAGE POUR LES VEHICULES                *EQW9ZWFZ
01500 ******************************************************************EQW9ZWFZ
01501 *                                                                 EQW9ZWFZ
01502  CHARG-TAB-AFF-VEHI.                                              EQW9ZWFZ
01503 *------------------*                                              EQW9ZWFZ
01504       ADD 1 TO IB.                                                EQW9ZWFZ
01505       MOVE 'V'                              TO WSS-TYPTS (IB).    EQW9ZWFZ
01506       MOVE RANG-TS-VEHI                     TO WSS-RANTS (IB).    EQW9ZWFZ
01524       MOVE VEHRSTC OF TS-VEHICULE (1)       TO WSS-RESTAU(IB).    FB01T00B
01525       MOVE VEHCHGC OF TS-VEHICULE (1)       TO WSS-CHANGE(IB).    FB01T00B
01527       MOVE VEHACTC OF TS-VEHICULE (1)       TO WSS-CODE-ACT(IB).  FB01T00B
01507 *  SI 4R 2R CC   IDENT = IMMAT + CODE AUTO                        EQW9ZWFZ
01508       IF VEHTYPC OF TS-VEHICULE (1) = '4R ' OR '2R ' OR      'CC 'EQW9ZWFZ
01509 *  EN RECTIFICATIF LA FUTUR IMMATRICULATION VEHFIMX EST RENSEIGNE EQW9ZWFZ
01510          IF VEHFIMX OF TS-VEHICULE (1) = SPACES OR LOW-VALUE      EQW9ZWFZ
01511             STRING VEHIMMX OF TS-VEHICULE (1)                     EQW9ZWFZ
01512                 ' ' VEHCODC OF TS-VEHICULE (1)                    EQW9ZWFZ
01513                 DELIMITED BY SIZE INTO                            EQW9ZWFZ
01514                                                WSS-IDENT (IB)     EQW9ZWFZ
01515          ELSE                                                     EQW9ZWFZ
01516             STRING VEHFIMX OF TS-VEHICULE (1)                     EQW9ZWFZ
01517                 ' ' VEHCODC OF TS-VEHICULE (1)                    EQW9ZWFZ
01518                 DELIMITED BY SIZE INTO                            EQW9ZWFZ
01519                                                WSS-IDENT (IB)     EQW9ZWFZ
01520          END-IF                                                   EQW9ZWFZ
01521 *  SI REM CAR    IDENT = IMMAT + MARQUE                           EQW9ZWFZ
01522       ELSE                                                        EQW9ZWFZ
01523          IF VEHFIMX OF TS-VEHICULE (1) = SPACES                   EQW9ZWFZ
01524                                                OR LOW-VALUE       EQW9ZWFZ
01525             STRING VEHIMMX OF TS-VEHICULE (1)                     EQW9ZWFZ
01526                 ' ' VEHMARL OF TS-VEHICULE (1)                    EQW9ZWFZ
01527                 DELIMITED BY SIZE INTO                            EQW9ZWFZ
01528                                                WSS-IDENT (IB)     EQW9ZWFZ
01529          ELSE                                                     EQW9ZWFZ
01530             STRING VEHFIMX OF TS-VEHICULE (1)                     EQW9ZWFZ
01531                 ' ' VEHMARL OF TS-VEHICULE (1)                    EQW9ZWFZ
01532                 DELIMITED BY SIZE INTO                            EQW9ZWFZ
01533                                                WSS-IDENT (IB)     EQW9ZWFZ
01534       END-IF.                                                     EQW9ZWFZ
01535       MOVE RVEHENTD OF TS-VEHICULE (1) TO WSS-SSAAMMJJ.           EQW9ZWFZ
01536       MOVE CORRESPONDING WSS-SSAAMMJJ TO WSS-JJMMSSAA.            EQW9ZWFZ
01537       MOVE WSS-JJMMSSAA              TO WSS-DATENT(IB).           EQW9ZWFZ
01538       IF RVEHSORD OF TS-VEHICULE (1) = '99999999'                 EQW9ZWFZ
01539          MOVE SPACES                    TO WSS-DATSOR(IB)         EQW9ZWFZ
01540       ELSE                                                        EQW9ZWFZ
01541          MOVE RVEHSORD OF TS-VEHICULE (1)                         EQW9ZWFZ
01542                                                TO WSS-SSAAMMJJ    EQW9ZWFZ
01543          MOVE CORRESPONDING WSS-SSAAMMJJ TO WSS-JJMMSSAA          EQW9ZWFZ
01544          MOVE WSS-JJMMSSAA              TO WSS-DATSOR (IB)        EQW9ZWFZ
01545       END-IF.                                                     EQW9ZWFZ
01546       MOVE VEHMTFC  OF TS-VEHICULE (1)                            EQW9ZWFZ
01547                                             TO WSS-MOTIFC(IB).    EQW9ZWFZ
01548       MOVE VEHTYPC  OF TS-VEHICULE (1)                            EQW9ZWFZ
01549                                             TO WSS-STATYP(IB).    EQW9ZWFZ
01550 *                                                                 EQW9ZWFZ
01551      PERFORM VARYING I FROM 1 BY 1 UNTIL I = 10                   EQW9ZWFZ
01552              OR COVTYPC  OF TS-VEHICULE (1, I) = '1'              EQW9ZWFZ
01553      END-PERFORM.                                                 EQW9ZWFZ
01554      IF I = 10                                                    EQW9ZWFZ
01555         MOVE SPACES                                               EQW9ZWFZ
01556                               TO   WSS-PTSCRM (IB)                EQW9ZWFZ
01557      END-IF.                                                      EQW9ZWFZ
01558      IF COVTYPC  OF TS-VEHICULE (1, I) = '1'                      EQW9ZWFZ
01559         IF RCOVTAFT OF TS-VEHICULE (1, I)                         EQW9ZWFZ
01560                                   NOT = SPACES AND LOW-VALUE      EQW9ZWFZ
01561            COMPUTE WSS-COEF-ENTIER ROUNDED                        EQW9ZWFZ
01562                  = COVTAFT OF TS-VEHICULE (1, I) * 100            EQW9ZWFZ
01563            MOVE WSS-COEF-ENTIER                                   EQW9ZWFZ
01564                               TO   WSS-PTSCRM (IB)                EQW9ZWFZ
01565         ELSE                                                      EQW9ZWFZ
01566            IF RCOVTAUT OF TS-VEHICULE (1, I) = SPACE OR           EQW9ZWFZ
01567                                                         LOW-VALUE EQW9ZWFZ
01568               MOVE SPACE      TO   WSS-PTSCRM (IB)                EQW9ZWFZ
01569            ELSE                                                   EQW9ZWFZ
01570               COMPUTE WSS-COEF-ENTIER ROUNDED                     EQW9ZWFZ
01571                  = COVTAUT OF TS-VEHICULE (1, I) * 100            EQW9ZWFZ
01572               MOVE WSS-COEF-ENTIER                                EQW9ZWFZ
01573                               TO   WSS-PTSCRM (IB)                EQW9ZWFZ
01574         END-IF                                                    EQW9ZWFZ
01575      END-IF.                                                      EQW9ZWFZ
01576 *                                                                 EQW9ZWFZ
01577  FIN-CHARG-TAB-AFF-VEHI.                                          EQW9ZWFZ
01578 *                                                                 EQW9ZWFZ
01579 ***************************************************************** EQW9ZWFZ
01580 * CREATION DE LA LIGNE D'AFFICHAGE DE SEPARATION                  EQW9ZWFZ
01581 ***************************************************************** EQW9ZWFZ
01582  CREAT-SEPARATION-AFF.                                            EQW9ZWFZ
01583      IF RANG-TS-PERS > 1                                          EQW9ZWFZ
01584         OR RANG-TS-VEHI > 1                                       EQW9ZWFZ
01585         ADD 1 TO IB                                               EQW9ZWFZ
01586         MOVE WSS-LIGNE-TIRET TO WSS-LIGNE-TABLEAU-AFF(IB)         EQW9ZWFZ
01587         MOVE ' '             TO WSS-MOTIFC(IB)                    EQW9ZWFZ
01588         MOVE ' '             TO WSS-SELCOD (IB)                   EQW9ZWFZ
01589      END-IF.                                                      EQW9ZWFZ
01590  FCREAT-SEPARATION-AFF.    EXIT.                                  EQW9ZWFZ
01591 *                                                                 EQW9ZWFZ
01592 *                                                                 EQW9ZWFZ
01593 ***************************************************************** EQW9ZWFZ
01594 * ALIMENATATION DES DONNEES DE L'ECRAN CIGAL DANS LA TRANSIT      EQW9ZWFZ
01595 ***************************************************************** EQW9ZWFZ
01596  ALIM-CIGAL.                                                      EQW9ZWFZ
01597                                                                   EQW9ZWFZ
01598      IF  (RVEHSORD OF TS-VEHICULE (1) = '99999999')               EQW9ZWFZ
01599      AND (VEHACTC  OF TS-VEHICULE (1) = 'U' OR 'I')               EQW9ZWFZ
01600         ADD 1 TO I-VEHI-CIGAL                                     EQW9ZWFZ
01601         IF I-VEHI-CIGAL > 1                                       EQW9ZWFZ
01602            MOVE SPACE   TO CIGAL-IMMAT  OF FBMISPTR-IT1           EQW9ZWFZ
01603                            CIGAL-MARQUE OF FBMISPTR-IT1           EQW9ZWFZ
01604         ELSE                                                      EQW9ZWFZ
01605            MOVE VEHIMMX OF TS-VEHICULE (1)                        EQW9ZWFZ
01606                         TO CIGAL-IMMAT  OF FBMISPTR-IT1           EQW9ZWFZ
03021            IF COM-MA-CODCNV (7:2) = 'RE'                          EQW9ZWFZ
01605               MOVE VEHFIMX OF TS-VEHICULE (1)                     EQW9ZWFZ
01606                         TO CIGAL-IMMAT  OF FBMISPTR-IT1           EQW9ZWFZ
                 END-IF
01607            IF VEHMARL OF TS-VEHICULE (1) = SPACE OR      LOW-VALUEEQW9ZWFZ
01608               MOVE VEHMARL OF TS-VEHICULE (2)                     EQW9ZWFZ
01609                         TO CIGAL-MARQUE OF FBMISPTR-IT1           EQW9ZWFZ
01610            ELSE                                                   EQW9ZWFZ
01611               MOVE VEHMARL OF TS-VEHICULE (1)                     EQW9ZWFZ
01612                         TO CIGAL-MARQUE OF FBMISPTR-IT1           EQW9ZWFZ
01613            END-IF                                                 EQW9ZWFZ
01614         END-IF                                                    EQW9ZWFZ
01615      END-IF.                                                      EQW9ZWFZ
01616                                                                   EQW9ZWFZ
01617  FALIM-CIGAL. EXIT.                                               EQW9ZWFZ
F2980 ***************************************************************** EQW9ZWFZ
F2980 * ALIMENTATION DE LA VARIABLE VEHICULE DE REMPLACEMENT            EQW9ZWFZ
F2980 ***************************************************************** EQW9ZWFZ
F2980  ALIM-VDR.                                                        EQW9ZWFZ
F2980                                                                   EQW9ZWFZ
F2980      MOVE SPACE     TO COM-FB-VDR.
F2980      MOVE 0         TO I-GTI-CODE.
F2980      IF  RVEHSORD OF TS-VEHICULE (1) = '99999999'                 EQW9ZWFZ
F2980          PERFORM VARYING I-GTI-CODE FROM 1 BY 1
F2980                UNTIL I-GTI-CODE > 30
F2980                OR GTICODC OF TS-VEHICULE (1, I-GTI-CODE) = 'ASE'
F2980          END-PERFORM
F2980          IF I-GTI-CODE <= 30
F2980             IF GTISORD OF TS-VEHICULE (1, I-GTI-CODE) = '99999999'EQW9ZWFZ
F2980                 MOVE 'O' TO COM-FB-VDR                            EQW9ZWFZ
F2980             ELSE
F2980                 MOVE 'N' TO COM-FB-VDR                            EQW9ZWFZ
F2980             END-IF
F2980          END-IF
F2980      END-IF.                                                      EQW9ZWFZ
F2980                                                                   EQW9ZWFZ
F2980  FALIM-VDR. EXIT.                                                 EQW9ZWFZ
01618 ************************************************************      EQW9ZWFZ
F2980 ***************************************************************** EQW9ZWFZ
F2980  ALIM-VEHI-USA.                                                   EQW9ZWFZ
F2980                                                                   EQW9ZWFZ
F2980      IF  RVEHSORD OF TS-VEHICULE (1) = '99999999'                 EQW9ZWFZ
F2980        IF  (VEHUSFC OF TS-VEHICULE (1) = '2' OR '3')
F2980        AND (VEHPRFC OF TS-VEHICULE (1) = '09' OR '10' OR '13')    EQW9ZWFZ
F2980            ADD 1 TO COM-FB-NBRE-VEHI-USA
F2980        ELSE
F2980          IF  (VEHUSAC OF TS-VEHICULE (1) = '2' OR '3')            EQW9ZWFZ
F2980          AND (VEHPROC OF TS-VEHICULE (1) = '09' OR '10' OR '13')  EQW9ZWFZ
F2980            ADD 1 TO COM-FB-NBRE-VEHI-USA
F2980          END-IF
F2980        END-IF
F2980      END-IF.                                                      EQW9ZWFZ
F2980                                                                   EQW9ZWFZ
F2980  FALIM-VEHI-USA. EXIT.                                            EQW9ZWFZ
01618 ************************************************************      EQW9ZWFZ
01619 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01620 *                                                               * EQW9ZWFZ
01621 *                -------------------------------                * EQW9ZWFZ
01622 *                I   REMPLISSAGE DE L'ECRAN    I                * EQW9ZWFZ
01623 *                -------------------------------                * EQW9ZWFZ
01624 *                               I                               * EQW9ZWFZ
01625 *           -----------------------------------------           * EQW9ZWFZ
01626 *           I                   I                   I           * EQW9ZWFZ
01627 * ----------------------   -------------   -----------------    * EQW9ZWFZ
01628 * I ZONES OBLIGATOIRES I   I PROTEGEES I   I NON PROTEGEES I    * EQW9ZWFZ
01629 * ----------------------   -------------   -----------------    * EQW9ZWFZ
01630 *                                                               * EQW9ZWFZ
01631 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01632 * REMPLISSAGE ECRAN   * FB01 * TRAITEMENT AUTOMATIQUE             EQW9ZWFZ
01633 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01634 *                                                                 EQW9ZWFZ
01635  REMPLISSAGE-FORMAT-ECRAN.                                        EQW9ZWFZ
01636 * ---------------------- *                                        EQW9ZWFZ
01637       PERFORM REMPLISSAGE-ZONES-OBLIGATOIRES THRU                 EQW9ZWFZ
01638               FIN-REMP-ZONES-OBL.                                 EQW9ZWFZ
01639 *                                                                 EQW9ZWFZ
01640       PERFORM REMPLISSAGE-ZONES-PROTEGEES THRU                    EQW9ZWFZ
01641               FIN-REMP-ZONES-PROT.                                EQW9ZWFZ
01642 *                                                                 EQW9ZWFZ
01643       PERFORM REMPLISSAGE-ZONES-NO-PROTEGEES THRU                 EQW9ZWFZ
01644               FIN-REMP-ZONES-NO-PROT.                             EQW9ZWFZ
01645 *                                                                 EQW9ZWFZ
01646       PERFORM RESTAURATION-ATTRIBUTS THRU                         EQW9ZWFZ
01647               FIN-RESTAURATION-ATTRIBUTS.                         EQW9ZWFZ
01648 *                                                                 EQW9ZWFZ
01649  FIN-REMPLISSAGE-FORMAT-ECRAN.  EXIT.                             EQW9ZWFZ
01650 *                                                                 EQW9ZWFZ
01651 *                                                                 EQW9ZWFZ
01652 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01653 * ZONES OBLIGATOIRES  * FB01 * TRAITEMENT AUTOMATIQUE             EQW9ZWFZ
01654 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01655 *                                                                 EQW9ZWFZ
01656  REMPLISSAGE-ZONES-OBLIGATOIRES.                                  EQW9ZWFZ
01657 * ---------------------------- *                                  EQW9ZWFZ
01658      MOVE Z-TIMER-DATJOU        TO   ECR-XJOURDO.                 EQW9ZWFZ
01659      MOVE Z-TIMER-TIMJOU        TO   ECR-XHEUREDO.                EQW9ZWFZ
01660      MOVE W-XTRMTRAC            TO   ECR-XTRMTRACO.               EQW9ZWFZ
01661      MOVE COM-GENE-LIBUSR       TO   ECR-XRACFLO.                 EQW9ZWFZ
01662      MOVE COM-GENE-LIBCNV       TO   ECR-XAPPLILO.                EQW9ZWFZ
01663 *                                                                 EQW9ZWFZ
01664 *  VERIFICATION DE LA PRESENCE DE CODES MESSAGES EN COMMAREA      EQW9ZWFZ
01665 *  SI OUI AFFICHAGE                                               EQW9ZWFZ
01666 *         REMISE A BLANC EN  COMMAREA                             EQW9ZWFZ
01667      IF COM-GENE-MESINF  NOT = SPACES AND                         EQW9ZWFZ
01668                                LOW-VALUE                          EQW9ZWFZ
01669         PERFORM LECTURE-ERREUR THRU                               EQW9ZWFZ
01670                 FIN-LECTURE-ERREUR                                EQW9ZWFZ
01671         MOVE    SPACES   TO COM-GENE-MESINF                       EQW9ZWFZ
01672         MOVE    W-ERREUR TO ECR-XMSGILO                           EQW9ZWFZ
01673      END-IF.                                                      EQW9ZWFZ
01674      IF COM-GENE-MESANO  NOT = SPACES AND                         EQW9ZWFZ
01675                                LOW-VALUE                          EQW9ZWFZ
01676         PERFORM LECTURE-ERREUR THRU                               EQW9ZWFZ
01677                 FIN-LECTURE-ERREUR                                EQW9ZWFZ
01678         MOVE SPACES      TO COM-GENE-MESANO                       EQW9ZWFZ
01679         MOVE W-ERREUR    TO ECR-XMSGALO                           EQW9ZWFZ
01680      END-IF.                                                      EQW9ZWFZ
01681  FIN-REMP-ZONES-OBL.  EXIT.                                       EQW9ZWFZ
01682 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01683 * ZONES PROTEGEES  * FB01 * TRAITEMENT AUTOMATIQUE                EQW9ZWFZ
01684 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01685  REMPLISSAGE-ZONES-PROTEGEES.                                     EQW9ZWFZ
01686 * ------------------------- *                                     EQW9ZWFZ
01687      MOVE COM-GENE-LIBCNV TO ECR-XAPPLILO.                        EQW9ZWFZ
01688 *---CODE INTERMEDIAIRE ET NUMERO CLIENT                           EQW9ZWFZ
01689      MOVE INF-GES  OF TS-SUSPENS1 TO W-GES.                       EQW9ZWFZ
01690      MOVE NOM-CLI  OF TS-SUSPENS1 TO W-CLI.                       EQW9ZWFZ
U3318  ++INCLUDE MAIPCLI
01691      MOVE W-GESCLI              TO ECR-GESCLIO.                   EQW9ZWFZ
01692                                                                   EQW9ZWFZ
01693 *---QUALITE DU CLIENT                                             EQW9ZWFZ
01694      MOVE NOM-RAIC OF TS-SUSPENS1 TO ECR-RAICO.                   EQW9ZWFZ
01695                                                                   EQW9ZWFZ
01696 *---NOM DU CLIENT                                                 EQW9ZWFZ
01697      MOVE NOM-NOMC OF TS-SUSPENS1 TO ECR-NOMCO.                   EQW9ZWFZ
01698 *--- DONNEES LIGNE D'AFFICHAGE PROTEGEE                           EQW9ZWFZ
01699      PERFORM VARYING IE FROM 1 BY 1 UNTIL IE > 10                 EQW9ZWFZ
01700         COMPUTE IG = (COM-ITEM-AFFICH - 1) * 10 + IE              EQW9ZWFZ
01701         MOVE WSS-IDENT  (IG) TO ECR-IDENTIFO (IE)                 EQW9ZWFZ
01702         MOVE WSS-DATENT (IG) TO ECR-DATENTDO (IE)                 EQW9ZWFZ
01703         MOVE WSS-DATSOR (IG) TO ECR-DATSORDO (IE)                 EQW9ZWFZ
01704         MOVE WSS-STATYP (IG) TO ECR-STATYPCO (IE)                 EQW9ZWFZ
01705         MOVE WSS-PTSCRM (IG) TO ECR-PTSCRMXO (IE)                 EQW9ZWFZ
01706      END-PERFORM.                                                 EQW9ZWFZ
01707  FIN-REMP-ZONES-PROT. EXIT.                                       EQW9ZWFZ
01708 *                                                                 EQW9ZWFZ
01709 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01710 * ZONES NON PROTEGEES * FB01 * TRAITEMENT AUTOMATIQUE             EQW9ZWFZ
01711 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01712  REMPLISSAGE-ZONES-NO-PROTEGEES.                                  EQW9ZWFZ
01713 *------------------------------*                                  EQW9ZWFZ
01714 *                                                                 EQW9ZWFZ
01715      PERFORM VARYING IE FROM 1 BY 1 UNTIL IE > 10                 EQW9ZWFZ
01716         COMPUTE IG = (COM-ITEM-AFFICH - 1) * 10 + IE              EQW9ZWFZ
01717         MOVE WSS-SELCOD (IG) TO ECR-CSECODCO (IE)                 EQW9ZWFZ
01718         MOVE WSS-MOTIFC (IG) TO ECR-MOTIFSCO (IE)                 EQW9ZWFZ
01719      END-PERFORM.                                                 EQW9ZWFZ
01720      EVALUATE COM-FB-CODE-ACTION                                  EQW9ZWFZ
01721        WHEN 'A'                                                   EQW9ZWFZ
01722          MOVE 'O'                TO ECR-AJOUTCONO                 EQW9ZWFZ
01723          MOVE 'N'                TO ECR-AJOUTVEHO                 EQW9ZWFZ
01724        WHEN '1'                                                   EQW9ZWFZ
01725          MOVE '1'                TO ECR-AJOUTVEHO                 EQW9ZWFZ
01726          MOVE 'N'                TO ECR-AJOUTCONO                 EQW9ZWFZ
01727        WHEN '2'                                                   EQW9ZWFZ
01728          MOVE '2'                TO ECR-AJOUTVEHO                 EQW9ZWFZ
01729          MOVE 'N'                TO ECR-AJOUTCONO                 EQW9ZWFZ
01730        WHEN '3'                                                   EQW9ZWFZ
01731          MOVE '3'                TO ECR-AJOUTVEHO                 EQW9ZWFZ
01732          MOVE 'N'                TO ECR-AJOUTCONO                 EQW9ZWFZ
01733        WHEN '4'                                                   EQW9ZWFZ
01734          MOVE '4'                TO ECR-AJOUTVEHO                 EQW9ZWFZ
01735          MOVE 'N'                TO ECR-AJOUTCONO                 EQW9ZWFZ
01736        WHEN '5'                                                   EQW9ZWFZ
01737          MOVE '5'                TO ECR-AJOUTVEHO                 EQW9ZWFZ
01738          MOVE 'N'                TO ECR-AJOUTCONO                 EQW9ZWFZ
01739        WHEN '?'                                                   EQW9ZWFZ
01740          MOVE '?'                TO ECR-AJOUTVEHO                 EQW9ZWFZ
01741          MOVE 'N'                TO ECR-AJOUTCONO                 EQW9ZWFZ
01742        WHEN OTHER                                                 EQW9ZWFZ
01743          MOVE 'N'                TO ECR-AJOUTCONO                 EQW9ZWFZ
01744          MOVE 'N'                TO ECR-AJOUTVEHO                 EQW9ZWFZ
01745      END-EVALUATE.                                                EQW9ZWFZ
01746 *                                                                 EQW9ZWFZ
01747  FIN-REMP-ZONES-NO-PROT. EXIT.                                    EQW9ZWFZ
01748 /                                                                 EQW9ZWFZ
01749 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01750 * MODULE DE BASE    * FB01 * TRAITEMENT NORMAL                    EQW9ZWFZ
01751 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01752 *                                                               * EQW9ZWFZ
01753 *              -----------------------------------              * EQW9ZWFZ
01754 *              I  TRAITEMENT            NORMAL   I              * EQW9ZWFZ
01755 *              -----------------------------------              * EQW9ZWFZ
01756 *                               I                               * EQW9ZWFZ
01757 *           -----------------------------------------           * EQW9ZWFZ
01758 *           I                   I                   I           * EQW9ZWFZ
01759 * -------------------- -------------------- ------------------- * EQW9ZWFZ
01760 * I CONTROLE SYNTAXE I I CONTROLE LOGIQUE I I TRAITEMENT TACHE I* EQW9ZWFZ
01761 * -------------------- -------------------- ------------------- * EQW9ZWFZ
01762 *                                                   I           * EQW9ZWFZ
01763 *                                           ------------------- * EQW9ZWFZ
01764 *                                           I DETERMI-ECR-SUIV I* EQW9ZWFZ
01765 *                                           ------------------- * EQW9ZWFZ
01766 *                                                               * EQW9ZWFZ
01767 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01768 *                                                                 EQW9ZWFZ
01769  M-TRAITEMENT-NORMAL.                                             EQW9ZWFZ
01770 * ----------------- *                                             EQW9ZWFZ
01771       PERFORM RESTAURATION-ATTRIBUTS THRU                         EQW9ZWFZ
01772               FIN-RESTAURATION-ATTRIBUTS.                         EQW9ZWFZ
01773                                                                   EQW9ZWFZ
01774       PERFORM CONTROLE-SYNTAXE THRU                               EQW9ZWFZ
01775               FIN-CONTROLE-SYNTAXE.                               EQW9ZWFZ
01776       IF OK                                                       EQW9ZWFZ
01777          PERFORM CONTROLE-LOGIQUE THRU                            EQW9ZWFZ
01778                  FIN-CONTROLE-LOGIQUE                             EQW9ZWFZ
01779       END-IF.                                                     EQW9ZWFZ
01780       IF OK                                                       EQW9ZWFZ
01781          IF  ECRAN-MODIFIE                                        EQW9ZWFZ
01782          OR (NOT ECRAN-MODIFIE AND COM-GENE-REAF = ' ')           EQW9ZWFZ
01783             PERFORM TRAITEMENT-TACHE THRU                         EQW9ZWFZ
01784                     FIN-TRAITEMENT-TACHE                          EQW9ZWFZ
01785          END-IF                                                   EQW9ZWFZ
01786 *                                                                 EQW9ZWFZ
01787 *    SI DOUBLE AFFICHAGE  AVEC MESSAGE  INFORMATIF  FAIRE :       EQW9ZWFZ
01788 *    IF ........                                                  EQW9ZWFZ
01789 *    AND ECRAN-MODIFIE                                            EQW9ZWFZ
01790 *        MOVE 'SQXXX'             TO COM-GENE-MESINF              EQW9ZWFZ
01791 *                                    COM-CODERR                   EQW9ZWFZ
01792 *        MOVE 'O'                 TO W-REAF                       EQW9ZWFZ
01793 *    END-IF.                                                      EQW9ZWFZ
01794 *                                                                 EQW9ZWFZ
01795 */   LORSQUE LE DOUBLE AFFICHAGE EST LIE A UN CONTROLE            EQW9ZWFZ
01796 */   ON NE LE REMET A BLANC QUE SI L'ECRAN EST NON MODIFIE        EQW9ZWFZ
01797 */   LES PROGRAMMES APPLICATIFS DEVRONT ADAPTER LE DERNIER        EQW9ZWFZ
01798 */   EVALUATE A LEURS BESOINS PROPRES                             EQW9ZWFZ
01799 *                                                                 EQW9ZWFZ
01800         EVALUATE  TRUE                                            EQW9ZWFZ
01801             WHEN  W-REAF        =  ' '                            EQW9ZWFZ
01802              AND  COM-GENE-REAF =  'O'                            EQW9ZWFZ
01803                   MOVE SPACES            TO  COM-GENE-REAF        EQW9ZWFZ
01804                   MOVE SPACES            TO  COM-GENE-MESINF      EQW9ZWFZ
01805             WHEN  W-REAF        =  ' '                            EQW9ZWFZ
01806              AND  COM-GENE-REAF =  ' '                            EQW9ZWFZ
01807                   CONTINUE                                        EQW9ZWFZ
01808             WHEN  W-REAF        =  'O'                            EQW9ZWFZ
01809              AND  COM-GENE-REAF =  ' '                            EQW9ZWFZ
01810                   MOVE 'O'               TO  COM-GENE-REAF        EQW9ZWFZ
01811             WHEN  W-REAF        =  'O'                            EQW9ZWFZ
01812              AND  COM-GENE-REAF =  'O'                            EQW9ZWFZ
01813                   EVALUATE  TRUE                                  EQW9ZWFZ
01814                       WHEN  ECRAN-MODIFIE                         EQW9ZWFZ
01815                             CONTINUE                              EQW9ZWFZ
01816                       WHEN  OTHER                                 EQW9ZWFZ
01817                             MOVE SPACES      TO  COM-GENE-REAF    EQW9ZWFZ
01818                             MOVE SPACES      TO  COM-GENE-MESINF  EQW9ZWFZ
01819                  END-EVALUATE                                     EQW9ZWFZ
01820         END-EVALUATE                                              EQW9ZWFZ
01821         IF  COM-GENE-REAF   =  'O'                                EQW9ZWFZ
01822             MOVE CODE-TRAITEMENT-AUTOMATIQUE  TO  Z-FONCTION      EQW9ZWFZ
01823         ELSE                                                      EQW9ZWFZ
01824             IF  ECR-XCDECO  =  LOW-VALUE  OR  SPACES              EQW9ZWFZ
01825                 PERFORM  DETERMINATION-ECR-SUIV  THRU             EQW9ZWFZ
01826                          FIN-DETERMINATION-ECR-SUIV               EQW9ZWFZ
01827             END-IF                                                EQW9ZWFZ
01828         END-IF                                                    EQW9ZWFZ
01829      END-IF.                                                      EQW9ZWFZ
01830  FIN-M-TRAITEMENT-NORMAL.     EXIT.                               EQW9ZWFZ
01831 /                                                                 EQW9ZWFZ
01832 ***************************************************************** EQW9ZWFZ
01833 *    RESTAURATION DES ATTRIBUTS AVANT CONTROLES                   EQW9ZWFZ
01834 ***************************************************************** EQW9ZWFZ
01835 *                                                                 EQW9ZWFZ
01836  RESTAURATION-ATTRIBUTS.                                          EQW9ZWFZ
01837 *                                                                 EQW9ZWFZ
01838      MOVE NOR-ASK    TO  ECR-XTRMTRACA.                           EQW9ZWFZ
01839      MOVE NOR-ASK    TO  ECR-XAPPLILA.                            EQW9ZWFZ
01840      MOVE NOR-ASK    TO  ECR-XJOURDA.                             EQW9ZWFZ
01841      MOVE NOR-ASK    TO  ECR-XRACFLA.                             EQW9ZWFZ
01842      MOVE NOR-ASK    TO  ECR-XHEUREDA.                            EQW9ZWFZ
01843      MOVE NOR-ASK    TO  ECR-GESCLIA.                             EQW9ZWFZ
01844      MOVE NOR-ASK    TO  ECR-RAICA.                               EQW9ZWFZ
01845      MOVE NOR-ASK    TO  ECR-NOMCA.                               EQW9ZWFZ
01846                                                                   EQW9ZWFZ
01847      MOVE BRT-ALP    TO  ECR-XCDECA.                              EQW9ZWFZ
01848 *                                                                 EQW9ZWFZ
01849      MOVE 0 TO IL.                                                EQW9ZWFZ
01850      PERFORM BOUCLE-REST1  THRU                                   EQW9ZWFZ
01851              FIN-BOUCLE-REST1  10 TIMES.                          EQW9ZWFZ
01852 *                                                                 EQW9ZWFZ
01853      IF COM-MA-IND-BLOCAGE = 'O'                                  EQW9ZWFZ
01854         MOVE NOR-ASK    TO  ECR-AJOUTCONA                         EQW9ZWFZ
01855         MOVE NOR-ASK    TO  ECR-AJOUTVEHA                         EQW9ZWFZ
01856      ELSE                                                         EQW9ZWFZ
01857         MOVE BRT-ALP    TO  ECR-AJOUTCONA                         EQW9ZWFZ
01858         MOVE BRT-ALP    TO  ECR-AJOUTVEHA                         EQW9ZWFZ
01859      END-IF.                                                      EQW9ZWFZ
F7833      IF INF-ETAT OF TS-SUSPENS1 = '3'
F7833         AND (COM-GENE-CODSIT NOT = INF-OPID-SITE OF TS-SUSPENS1)  EQW9ZWFZ
F7833      OR INF-ETAT OF TS-SUSPENS1 = '7'
F7833         PERFORM VARYING IL FROM 1 BY 1 UNTIL IL > 10              EQW9ZWFZ
F7833            MOVE NOR-ASK    TO  ECR-MOTIFSCA (IL)                  EQW9ZWFZ
F7833         END-PERFORM                                               EQW9ZWFZ
01859      END-IF.                                                      EQW9ZWFZ
01860 *                                                                 EQW9ZWFZ
01861  FIN-RESTAURATION-ATTRIBUTS.  EXIT.                               EQW9ZWFZ
01862               EJECT                                               EQW9ZWFZ
01863 *                                                                 EQW9ZWFZ
01864  BOUCLE-REST1.                                                    EQW9ZWFZ
01865      ADD 1 TO IL.                                                 EQW9ZWFZ
01866      IF COM-MA-IND-BLOCAGE NOT = 'O'                              EQW9ZWFZ
           OR INF-ETAT OF TS-SUSPENS1 = '3'
           OR INF-ETAT OF TS-SUSPENS1 = '7'
01867         IF ECR-DATENTDO(IL) NOT = '--------' AND '        '       EQW9ZWFZ
01868 *                                                                 EQW9ZWFZ
01869 * EN RECTIFICATIF PAS DE PRISE EN COMPTE LORS DE LA SAISIE        EQW9ZWFZ
01870 * DU CODE MOTIF                                                   EQW9ZWFZ
01871 *                                                                 EQW9ZWFZ
01872            IF COM-MA-CODCNV = 'FB0001RE'                          EQW9ZWFZ
01873               OR COM-MA-CODCNV = 'FB0002RE'                       EQW9ZWFZ
01874               MOVE NOR-PRO    TO ECR-MOTIFSCA(IL)                 EQW9ZWFZ
01875               MOVE BRT-ALP    TO ECR-CSECODCA(IL)                 EQW9ZWFZ
01876            ELSE                                                   EQW9ZWFZ
01877               MOVE BRT-ALP    TO ECR-CSECODCA(IL)                 EQW9ZWFZ
01878               MOVE BRT-ALP    TO ECR-MOTIFSCA(IL)                 EQW9ZWFZ
01879            END-IF                                                 EQW9ZWFZ
01880         ELSE                                                      EQW9ZWFZ
01881            MOVE NOR-PRO    TO ECR-CSECODCA(IL)                    EQW9ZWFZ
01882            MOVE NOR-PRO    TO ECR-MOTIFSCA(IL)                    EQW9ZWFZ
01883         END-IF                                                    EQW9ZWFZ
01884      ELSE                                                         EQW9ZWFZ
01885         IF ECR-DATENTDO(IL) NOT = '--------' AND '        '       EQW9ZWFZ
01886            MOVE NOR-ASK    TO ECR-CSECODCA(IL)                    EQW9ZWFZ
01887            MOVE NOR-ASK    TO ECR-MOTIFSCA(IL)                    EQW9ZWFZ
01888         ELSE                                                      EQW9ZWFZ
01889            MOVE NOR-PRO    TO ECR-CSECODCA(IL)                    EQW9ZWFZ
01890            MOVE NOR-PRO    TO ECR-MOTIFSCA(IL)                    EQW9ZWFZ
01891         END-IF                                                    EQW9ZWFZ
01892      END-IF.                                                      EQW9ZWFZ
01893      MOVE NOR-PRO    TO ECR-IDENTIFA (IL).                        EQW9ZWFZ
01894      MOVE NOR-PRO    TO ECR-DATENTDA (IL).                        EQW9ZWFZ
01895      MOVE NOR-PRO    TO ECR-DATSORDA (IL).                        EQW9ZWFZ
01896      MOVE NOR-PRO    TO ECR-STATYPCA (IL).                        EQW9ZWFZ
01897      MOVE NOR-PRO    TO ECR-PTSCRMXA (IL).                        EQW9ZWFZ
01898 *                                                                 EQW9ZWFZ
01899  FIN-BOUCLE-REST1.  EXIT.                                         EQW9ZWFZ
01900 *                                                                 EQW9ZWFZ
01901 *                                                                 EQW9ZWFZ
01902 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01903 * CONTROLES SYNTAXIQUES * FB01 * TRAITEMENT NORMAL                EQW9ZWFZ
01904 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
01905 *                                                                 EQW9ZWFZ
01906  CONTROLE-SYNTAXE.                                                EQW9ZWFZ
01907 * -------------- *                                                EQW9ZWFZ
01908 *                                                                 EQW9ZWFZ
01909 * TEST DES CODES MNEMONIQUES POUR L'ENCHAINEMENT DES              EQW9ZWFZ
01910 * TRANSACTIONS DANS UNE CONVERSATION                              EQW9ZWFZ
01911 *                                                                 EQW9ZWFZ
01912      IF ECR-XCDECO NOT = LOW-VALUE AND SPACES                     EQW9ZWFZ
01913         IF ECR-XCDECO = 'AIDE' OR 'ACTI' OR 'MOTI'                EQW9ZWFZ
01914            MOVE 'MA84'                 TO NOM-TACHE-XCTL          EQW9ZWFZ
01915            IF ECR-XCDECO = 'AIDE'                                 EQW9ZWFZ
01916               MOVE 'AIDE'              TO COM-MA-GENRE-TXT        EQW9ZWFZ
01917               MOVE COM-ITEM-AFFICH     TO COM-FB-PAGE-ENCOUR      EQW9ZWFZ
01918            END-IF                                                 EQW9ZWFZ
01919            IF ECR-XCDECO = 'ACTI'                                 EQW9ZWFZ
01920               MOVE 'ACTI'              TO COM-MA-GENRE-TXT        EQW9ZWFZ
01921               MOVE COM-ITEM-AFFICH     TO COM-FB-PAGE-ENCOUR      EQW9ZWFZ
01922            END-IF                                                 EQW9ZWFZ
01923            IF ECR-XCDECO = 'MOTI'                                 EQW9ZWFZ
01924               MOVE 'MOTI'              TO COM-MA-GENRE-TXT        EQW9ZWFZ
01925               MOVE COM-ITEM-AFFICH     TO COM-FB-PAGE-ENCOUR      EQW9ZWFZ
01926            END-IF                                                 EQW9ZWFZ
01927         ELSE                                                      EQW9ZWFZ
01928            MOVE ZERO                   TO COM-FB-PAGE-ENCOUR      EQW9ZWFZ
01929                                                                   EQW9ZWFZ
U3319            PERFORM  CONTROLE-CODE-COMMANDE  THRU                  EFUTSQP3
U3319                     FIN-CONTROLE-CODE-COMMANDE                    EFUTSQP3
01929                                                                   EQW9ZWFZ
01954         END-IF                                                    EQW9ZWFZ
01955      END-IF.                                                      EQW9ZWFZ
01956 *                                                                 EQW9ZWFZ
01978 * CODE SELECTION : VALEURS POSSIBLE M OU D OU R OU F OU C OU SPACEFB01T00B
F7833 * POUR LES RS EN DA, LA SEULE SAISIE POSSIBLE EST 'V' POUR        EQW9ZWFZ
F7833 * VISUALISATION SANS MODIFICATION                                 EQW9ZWFZ
01958 *                                                                 EQW9ZWFZ
01959      SET NO-SELECTION TO TRUE.                                    EQW9ZWFZ
01960      PERFORM VARYING I FROM 1 BY 1                                EQW9ZWFZ
01961              UNTIL   I > 10                                       EQW9ZWFZ
01962         IF ECR-CSECODCO(I) = LOW-VALUE                            EQW9ZWFZ
01963            MOVE SPACE          TO   ECR-CSECODCO(I)               EQW9ZWFZ
01964         END-IF                                                    EQW9ZWFZ
F7833 *
F7833 * - POUR LES SUSPENS EN DEMANDE D'ACCORD: SI LE SITE REPRENANT LE
F7833 * SUSPENS (OM-GENE-CODSIT) N'EST PAS LE SITE QUI A FAIT LA DEMANDE
F7833 * D'ACCORD (INF-OPID-SITE: SITE DE LA DERNIËRE INTERVENTION)
F7833 * - POUR LES SUSPENS EMIS EN ATTENTE D'ÈPURARTION
F7833 * --> SEULE LA VISUALISATION DES DONNÈES EST POSSIBLE, SANS
F7833 *     MODIFICATION (CODE SELECTION 'V')
F7833 *
F7833         IF ((INF-ETAT OF TS-SUSPENS1 = '3')                       EQW9ZWFZ
F7833         AND (COM-GENE-CODSIT NOT = INF-OPID-SITE OF TS-SUSPENS1)) EQW9ZWFZ
F7833            IF ECR-CSECODCO(I) NOT = SPACE AND 'V'                 FB01T00B
F7833               MOVE NOR-ALP        TO   ECR-CSECODCA(I)            EQW9ZWFZ
F7833               IF OK                                               EQW9ZWFZ
F7833                  MOVE 'FB462'     TO   COM-GENE-MESANO            EQW9ZWFZ
F7833                                        COM-CODERR                 EQW9ZWFZ
F7833                  MOVE CURSEUR     TO   ECR-CSECODCL(I)            EQW9ZWFZ
F7833                  MOVE  1          TO   KONTROL                    EQW9ZWFZ
F7833                  GO TO  FIN-CONTROLE-SYNTAXE                      EQW9ZWFZ
01981               END-IF                                              EQW9ZWFZ
01974            END-IF                                                 EQW9ZWFZ
01975         ELSE                                                      EQW9ZWFZ
F7833            IF (INF-ETAT OF TS-SUSPENS1 = '7')                     EQW9ZWFZ
F7833               IF ECR-CSECODCO(I) NOT = SPACE AND 'V'              FB01T00B
F7833                  MOVE NOR-ALP        TO   ECR-CSECODCA(I)         EQW9ZWFZ
F7833                  IF OK                                            EQW9ZWFZ
F7833                     MOVE 'FB462'     TO   COM-GENE-MESANO         EQW9ZWFZ
F7833                                           COM-CODERR              EQW9ZWFZ
F7833                     MOVE CURSEUR     TO   ECR-CSECODCL(I)         EQW9ZWFZ
F7833                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
F7833                     GO TO  FIN-CONTROLE-SYNTAXE                   EQW9ZWFZ
F7833                  END-IF                                           EQW9ZWFZ
F7833               END-IF                                              EQW9ZWFZ
01975            ELSE                                                   EQW9ZWFZ
01986               IF ECR-CSECODCO(I) NOT = 'M' AND 'D' AND 'C'        FB01T00B
01966                          AND 'R' AND 'F' AND '?' AND SPACE        EQW9ZWFZ
F7833                  MOVE NOR-ALP        TO   ECR-CSECODCA(I)         EQW9ZWFZ
F7833                  IF OK                                            EQW9ZWFZ
F7833                     MOVE 'FB066'     TO   COM-GENE-MESANO         EQW9ZWFZ
F7833                                           COM-CODERR              EQW9ZWFZ
F7833                     MOVE CURSEUR     TO   ECR-CSECODCL(I)         EQW9ZWFZ
F7833                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
F7833                     GO TO  FIN-CONTROLE-SYNTAXE                   EQW9ZWFZ
F7833                  END-IF                                           EQW9ZWFZ
01979               ELSE                                                EQW9ZWFZ
01976                  IF ECR-CSECODCO (I) NOT = SPACES                 EQW9ZWFZ
01977                     IF NO-SELECTION                               EQW9ZWFZ
01978                        SET SELECTION-1 TO TRUE                    EQW9ZWFZ
01979                     ELSE                                          EQW9ZWFZ
01980                        SET SELECTION-2 TO TRUE                    EQW9ZWFZ
01981                     END-IF                                        EQW9ZWFZ
01981                  END-IF                                           EQW9ZWFZ
01981               END-IF                                              EQW9ZWFZ
01982            END-IF                                                 EQW9ZWFZ
01983         END-IF                                                    EQW9ZWFZ
01984      END-PERFORM.                                                 EQW9ZWFZ
01985 *                                                                 EQW9ZWFZ
01986 * MOTIF DE SORTIE: VALEURS POSSIBLE 1 OU 2 OU 3 OU 4 OU 6 OU 8    EQW9ZWFZ
01987 *                                OU 9 OU H OU L OU N OU P OU SPACEEQW9ZWFZ
02009 *                                OU R UNIQUEMENT POUR CHANGEMENT  FB01T00B
02010 *                               DE VÈHICULE, MOTIF NON SAISISSABLEFB01T00B
01988 *                                                                 EQW9ZWFZ
01989      MOVE 'N' TO WSS-APPEL-AIDE-MOTIF.                            EQW9ZWFZ
01990 *                                                                 EQW9ZWFZ
01991      PERFORM VARYING I FROM 1 BY 1                                EQW9ZWFZ
01992              UNTIL   I > 10                                       EQW9ZWFZ
01993         IF ECR-MOTIFSCO(I) = LOW-VALUE                            EQW9ZWFZ
01994            MOVE SPACE          TO   ECR-MOTIFSCO(I)               EQW9ZWFZ
01995         END-IF                                                    EQW9ZWFZ
01996         PERFORM DETER-AIDE-AJTMOTI THRU FDETER-AIDE-AJTMOTI       EQW9ZWFZ
01997         IF ECR-STATYPCO(I) = '4R  ' OR '2R  ' OR 'CC  '           EQW9ZWFZ
01998                           OR 'CAR ' OR 'REM '                     EQW9ZWFZ
01999            IF ECR-MOTIFSCO(I) NOT = SPACE AND '6' AND '9' AND 'H' EQW9ZWFZ
02000                                           AND 'N' AND 'L' AND '?' EQW9ZWFZ
                    IF ECR-DATSORDO (I) NOT = SPACES
02237                  MOVE ECR-DATSORDO (I)           TO WSS-JJMMSSAA  EQW9ZWFZ
02238                  MOVE CORRESPONDING WSS-JJMMSSAA TO WSS-SSAAMMJJ  EQW9ZWFZ
02239                  IF COM-FB-DATE-EFFET-1 NOT = SPACES AND LOW-VALUEEQW9ZWFZ
02240                     IF WSS-SSAAMMJJ     = COM-FB-DATE-EFFET-1     EQW9ZWFZ
02029                        IF ECR-MOTIFSCO(I) NOT = 'R'               FB01T00B
02030                           MOVE NOR-ALP        TO   ECR-MOTIFSCA(I)FB01T00B
02031                           IF OK                                   FB01T00B
02032                              MOVE 'FB068'     TO   COM-GENE-MESANOFB01T00B
02033                                                    COM-CODERR     FB01T00B
02034                              MOVE CURSEUR     TO   ECR-MOTIFSCL(I)FB01T00B
02035                              MOVE  1          TO   KONTROL        FB01T00B
02036                              GO TO  FIN-CONTROLE-SYNTAXE          FB01T00B
02037                           END-IF                                  FB01T00B
02008                        END-IF                                     EQW9ZWFZ
02008                     END-IF                                        EQW9ZWFZ
02008                  END-IF                                           EQW9ZWFZ
                    ELSE
02001                  MOVE NOR-ALP        TO   ECR-MOTIFSCA(I)         EQW9ZWFZ
02002                  IF OK                                            EQW9ZWFZ
02003                     MOVE 'FB068'     TO   COM-GENE-MESANO         EQW9ZWFZ
02004                                           COM-CODERR              EQW9ZWFZ
02005                     MOVE CURSEUR     TO   ECR-MOTIFSCL(I)         EQW9ZWFZ
02006                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
02007                     GO TO  FIN-CONTROLE-SYNTAXE                   EQW9ZWFZ
02008                  END-IF                                           EQW9ZWFZ
02008               END-IF                                              EQW9ZWFZ
02009            END-IF                                                 EQW9ZWFZ
02010         END-IF                                                    EQW9ZWFZ
02011         IF ECR-STATYPCO(I) = 'PM  ' OR 'CF  ' OR 'CJ  '           EQW9ZWFZ
02012                           OR 'ENAP' OR 'ENSP'                     EQW9ZWFZ
02013            IF ECR-MOTIFSCO(I) NOT = SPACE AND '1' AND '2' AND '3' EQW9ZWFZ
02014                                           AND '4' AND '6' AND '8' EQW9ZWFZ
02015                                           AND 'L' AND '?'         EQW9ZWFZ
                    IF ECR-DATSORDO (I) NOT = SPACES
02237                  MOVE ECR-DATSORDO (I)           TO WSS-JJMMSSAA  EQW9ZWFZ
02238                  MOVE CORRESPONDING WSS-JJMMSSAA TO WSS-SSAAMMJJ  EQW9ZWFZ
02239                  IF COM-FB-DATE-EFFET-1 NOT = SPACES AND LOW-VALUEEQW9ZWFZ
02240                     IF WSS-SSAAMMJJ     = COM-FB-DATE-EFFET-1     EQW9ZWFZ
02016                        MOVE NOR-ALP        TO   ECR-MOTIFSCA(I)   EQW9ZWFZ
02017                        IF OK                                      EQW9ZWFZ
02018                           MOVE 'FB068'     TO   COM-GENE-MESANO   EQW9ZWFZ
02019                                                 COM-CODERR        EQW9ZWFZ
02020                           MOVE CURSEUR     TO   ECR-MOTIFSCL(I)   EQW9ZWFZ
02021                           MOVE  1          TO   KONTROL           EQW9ZWFZ
02022                           GO TO  FIN-CONTROLE-SYNTAXE             EQW9ZWFZ
02023                        END-IF                                     EQW9ZWFZ
02023                     END-IF                                        EQW9ZWFZ
02023                  END-IF                                           EQW9ZWFZ
                    ELSE
02016                  MOVE NOR-ALP        TO   ECR-MOTIFSCA(I)         EQW9ZWFZ
02017                  IF OK                                            EQW9ZWFZ
02018                     MOVE 'FB068'     TO   COM-GENE-MESANO         EQW9ZWFZ
02019                                           COM-CODERR              EQW9ZWFZ
02020                     MOVE CURSEUR     TO   ECR-MOTIFSCL(I)         EQW9ZWFZ
02021                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
02022                     GO TO  FIN-CONTROLE-SYNTAXE                   EQW9ZWFZ
02023                  END-IF                                           EQW9ZWFZ
02023               END-IF                                              EQW9ZWFZ
02024            END-IF                                                 EQW9ZWFZ
02025         END-IF                                                    EQW9ZWFZ
02026      END-PERFORM.                                                 EQW9ZWFZ
02027 *                                                                 EQW9ZWFZ
02028 * CODE AJOUT CONDUCTEUR : VALEURS POSSIBLE 'O' OU 'N' OU SPACE    EQW9ZWFZ
02029 * ICI                                                             EQW9ZWFZ
02030      IF ECR-AJOUTCONO = LOW-VALUE                                 EQW9ZWFZ
02031         MOVE SPACES TO ECR-AJOUTCONO                              EQW9ZWFZ
02032      END-IF.                                                      EQW9ZWFZ
02033      IF ECR-AJOUTCONO NOT = SPACE AND 'O' AND 'N'                 EQW9ZWFZ
02034         MOVE NOR-ALP        TO   ECR-AJOUTCONA                    EQW9ZWFZ
02035         IF OK                                                     EQW9ZWFZ
02036            MOVE 'FB071'     TO   COM-GENE-MESANO                  EQW9ZWFZ
02037                                  COM-CODERR                       EQW9ZWFZ
02038            MOVE CURSEUR     TO   ECR-AJOUTCONL                    EQW9ZWFZ
02039            MOVE  1          TO   KONTROL                          EQW9ZWFZ
02040            GO TO  FIN-CONTROLE-SYNTAXE                            EQW9ZWFZ
02041         END-IF                                                    EQW9ZWFZ
02042      ELSE                                                         EQW9ZWFZ
02043         IF ECR-AJOUTCONO NOT = SPACES AND 'N'                     EQW9ZWFZ
02044            IF NO-SELECTION                                        EQW9ZWFZ
02045               SET SELECTION-1 TO TRUE                             EQW9ZWFZ
02046            ELSE                                                   EQW9ZWFZ
02047               SET SELECTION-2 TO TRUE                             EQW9ZWFZ
02048            END-IF                                                 EQW9ZWFZ
02049         END-IF                                                    EQW9ZWFZ
02050      END-IF.                                                      EQW9ZWFZ
02051 *                                                                 EQW9ZWFZ
02052 * CODE AJOUT VEHICULE : VALEURS POSSIBLE '1' OU '2' OU '3' OU '4' EQW9ZWFZ
02053 *                                     OU '5' OU SPACE             EQW9ZWFZ
02054 * ICI                                                             EQW9ZWFZ
02055      IF ECR-AJOUTVEHO = LOW-VALUE                                 EQW9ZWFZ
02056         MOVE SPACES TO ECR-AJOUTVEHO                              EQW9ZWFZ
02057      END-IF.                                                      EQW9ZWFZ
02058      PERFORM DETER-AIDE-AJTVEHI THRU FDETER-AIDE-AJTVEHI.         EQW9ZWFZ
02059      IF ECR-AJOUTVEHO NOT = SPACE AND '1' AND '2' AND '3' AND '4' EQW9ZWFZ
02060                         AND '5' AND 'N' AND '?'                   EQW9ZWFZ
02061         MOVE NOR-ALP        TO   ECR-AJOUTVEHA                    EQW9ZWFZ
02062         MOVE 'FB072'        TO   COM-GENE-MESANO                  EQW9ZWFZ
02063                                  COM-CODERR                       EQW9ZWFZ
02064         MOVE CURSEUR        TO   ECR-AJOUTVEHL                    EQW9ZWFZ
02065         MOVE  1             TO   KONTROL                          EQW9ZWFZ
02066         GO TO  FIN-CONTROLE-SYNTAXE                               EQW9ZWFZ
02067      ELSE                                                         EQW9ZWFZ
02068         IF ECR-AJOUTVEHO NOT = SPACES AND 'N'                     EQW9ZWFZ
02069            IF NO-SELECTION                                        EQW9ZWFZ
02070               SET SELECTION-1 TO TRUE                             EQW9ZWFZ
02071            ELSE                                                   EQW9ZWFZ
02072               SET SELECTION-2 TO TRUE                             EQW9ZWFZ
02073            END-IF                                                 EQW9ZWFZ
02074         END-IF                                                    EQW9ZWFZ
02075      END-IF.                                                      EQW9ZWFZ
02076 *                                                                 EQW9ZWFZ
02077 *    SI DOUBLE AFFICHAGE  AVEC MESSAGE  INFORMATIF :              EQW9ZWFZ
02078 *    IF ........                                                  EQW9ZWFZ
02079 *    AND ECRAN-MODIFIE                                            EQW9ZWFZ
02080 *        MOVE 'SQXXX'             TO COM-GENE-MESINF              EQW9ZWFZ
02081 *                                    COM-CODERR                   EQW9ZWFZ
02082 *        MOVE 'O'                 TO W-REAF                       EQW9ZWFZ
02083 *    END-IF.                                                      EQW9ZWFZ
02084 *                                                                 EQW9ZWFZ
02085  FIN-CONTROLE-SYNTAXE.  EXIT.                                     EQW9ZWFZ
02086 *                                                                 EQW9ZWFZ
02087 *                                                                 EQW9ZWFZ
02088 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02089 * CONTROLES LOGIQUES    * FB01 * TRAITEMENT NORMAL                EQW9ZWFZ
02090 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02091  CONTROLE-LOGIQUE.                                                EQW9ZWFZ
02092 *----------------*                                                EQW9ZWFZ
02093 *                                                                 EQW9ZWFZ
02094 * AVANT DE PASSER LA MAIN AU PROGRAMME SUIVANT                    EQW9ZWFZ
02095 * APPORTER ICI LES CONTROLES INHERENTS A LA TS DE CONFIDENTIALITE EQW9ZWFZ
02096 *                                                                 EQW9ZWFZ
02097 *                                                                 EQW9ZWFZ
02098 * LA LIGNE SELECTIONNEE DOIT ETRE REMPLIE                         EQW9ZWFZ
02099 * UNE SEULE SELECTION POSSIBLE ET REPONSE AUX QUESTIONS           EQW9ZWFZ
02100 *                                                                 EQW9ZWFZ
02101      IF SELECTION-2                                               EQW9ZWFZ
02102         PERFORM VARYING I FROM 1 BY 1                             EQW9ZWFZ
02103                 UNTIL  (I > 10)                                   EQW9ZWFZ
02104                 OR  (ECR-CSECODCO(I) NOT = SPACE)                 EQW9ZWFZ
02105         END-PERFORM                                               EQW9ZWFZ
02106         IF I NOT > 10                                             EQW9ZWFZ
02107            MOVE NOR-ALP        TO   ECR-CSECODCA(I)               EQW9ZWFZ
02108            IF KONTROL = 0                                         EQW9ZWFZ
02109               MOVE 'MBV83'     TO   COM-GENE-MESANO               EQW9ZWFZ
02110                                     COM-CODERR                    EQW9ZWFZ
02111               MOVE CURSEUR     TO   ECR-CSECODCL(I)               EQW9ZWFZ
02112               MOVE  1          TO   KONTROL                       EQW9ZWFZ
02113               GO TO  FIN-CONTROLE-LOGIQUE                         EQW9ZWFZ
02114            END-IF                                                 EQW9ZWFZ
02115         ELSE                                                      EQW9ZWFZ
02116            MOVE NOR-ALP        TO   ECR-AJOUTCONA                 EQW9ZWFZ
02117            IF OK                                                  EQW9ZWFZ
02118               MOVE 'MBV83'     TO   COM-GENE-MESANO               EQW9ZWFZ
02119                                     COM-CODERR                    EQW9ZWFZ
02120               MOVE CURSEUR     TO   ECR-AJOUTCONL                 EQW9ZWFZ
02121               MOVE  1          TO   KONTROL                       EQW9ZWFZ
02122               GO TO  FIN-CONTROLE-LOGIQUE                         EQW9ZWFZ
02123            END-IF                                                 EQW9ZWFZ
02124         END-IF                                                    EQW9ZWFZ
02125      END-IF.                                                      EQW9ZWFZ
02126 *  SI AJOUT D'UN ONZIËME CONDUCTEUR BLOQUAGE                      EQW9ZWFZ
02127 *  SI AJOUT UN DEUXIEME CONDUCTEUR ET PREMIER COND 'PM' BLOQUAGE  EQW9ZWFZ
02128      IF ECR-AJOUTCONO = 'O'                                       EQW9ZWFZ
02129         IF COM-FB-NBRE-PERS-ENC = 10                              EQW9ZWFZ
02130            MOVE NOR-ALP        TO   ECR-AJOUTCONA                 EQW9ZWFZ
02131            IF OK                                                  EQW9ZWFZ
02132               MOVE 'FB069'     TO   COM-GENE-MESANO               EQW9ZWFZ
02133                                     COM-CODERR                    EQW9ZWFZ
02134               MOVE CURSEUR     TO   ECR-AJOUTCONL                 EQW9ZWFZ
02135               MOVE  1          TO   KONTROL                       EQW9ZWFZ
02136               GO TO  FIN-CONTROLE-LOGIQUE                         EQW9ZWFZ
02137            END-IF                                                 EQW9ZWFZ
02138         END-IF                                                    EQW9ZWFZ
02139         IF COM-FB-NBRE-PERS-PM  = 1                               EQW9ZWFZ
02140            MOVE NOR-ALP        TO   ECR-AJOUTCONA                 EQW9ZWFZ
02141            IF OK                                                  EQW9ZWFZ
02142               MOVE 'FB186'     TO   COM-GENE-MESANO               EQW9ZWFZ
02143                                     COM-CODERR                    EQW9ZWFZ
02144               MOVE CURSEUR     TO   ECR-AJOUTCONL                 EQW9ZWFZ
02145               MOVE  1          TO   KONTROL                       EQW9ZWFZ
02146               GO TO  FIN-CONTROLE-LOGIQUE                         EQW9ZWFZ
02147            END-IF                                                 EQW9ZWFZ
02148         END-IF                                                    EQW9ZWFZ
02149      END-IF.                                                      EQW9ZWFZ
02150 *  SI AJOUT D'UN ONZIËME VEHICULE BLOQUAGE                        EQW9ZWFZ
02151      IF ECR-AJOUTVEHO = '1' OR '2' OR '3' OR '4' OR '5'           EQW9ZWFZ
02152         IF COM-FB-NBRE-VEHI-ENC = 10                              EQW9ZWFZ
02153            MOVE NOR-ALP        TO   ECR-AJOUTVEHA                 EQW9ZWFZ
02154            IF OK                                                  EQW9ZWFZ
02155               MOVE 'FB070'     TO   COM-GENE-MESANO               EQW9ZWFZ
02156                                     COM-CODERR                    EQW9ZWFZ
02157               MOVE CURSEUR     TO   ECR-AJOUTVEHL                 EQW9ZWFZ
02158               MOVE  1          TO   KONTROL                       EQW9ZWFZ
02159               GO TO  FIN-CONTROLE-LOGIQUE                         EQW9ZWFZ
02160            END-IF                                                 EQW9ZWFZ
02161         END-IF                                                    EQW9ZWFZ
02162         IF (ECR-AJOUTVEHO = '4' OR '5')                           EQW9ZWFZ
02163            AND COM-FB-NBRE-VEHI-TRACTEUR = ZERO                   EQW9ZWFZ
02164            MOVE NOR-ALP        TO   ECR-AJOUTVEHA                 EQW9ZWFZ
02165            IF OK                                                  EQW9ZWFZ
02166               MOVE 'FB264'     TO   COM-GENE-MESANO               EQW9ZWFZ
02167                                     COM-CODERR                    EQW9ZWFZ
02168               MOVE CURSEUR     TO   ECR-AJOUTVEHL                 EQW9ZWFZ
02169               MOVE  1          TO   KONTROL                       EQW9ZWFZ
02170               GO TO  FIN-CONTROLE-LOGIQUE                         EQW9ZWFZ
02171            END-IF                                                 EQW9ZWFZ
02172         END-IF                                                    EQW9ZWFZ
02173      END-IF.                                                      EQW9ZWFZ
02409 * EN RECTIFICATIF CONTROLE SUPLEMENTAIRE                          EQW9ZWFZ
02410 * EN EFFET 1/ ON NE PEUT FAIRE D'ACTION QUE SUR LES VEHICULES     EQW9ZWFZ
02411 *          2/ LA SEULE ACTION POSSIBLE EST UNE MODIFICATION       EQW9ZWFZ
02412 *                                                                 EQW9ZWFZ
02413      IF OK                                                        EQW9ZWFZ
02414         IF COM-MA-CODCNV = 'FB0001RE'                             EQW9ZWFZ
02415            OR COM-MA-CODCNV = 'FB0002RE'                          EQW9ZWFZ
02416            PERFORM VARYING I FROM 1 BY 1 UNTIL                    EQW9ZWFZ
02417               ECR-CSECODCO (I) NOT = ' '                          EQW9ZWFZ
02418               OR I = 10                                           EQW9ZWFZ
02419            END-PERFORM                                            EQW9ZWFZ
U3796            IF (ECR-STATYPCO (I) = 'PM' OR 'CJ'                    EQW9ZWFZ
U3796                                OR 'CF' OR 'ENSP' OR 'ENAP')       EQW9ZWFZ
U3796               IF ECR-CSECODCO (I) NOT = '  '                      EQW9ZWFZ
02432                  MOVE NOR-ALP        TO   ECR-CSECODCA(I)         EQW9ZWFZ
02433                  IF OK                                            EQW9ZWFZ
U3796                     MOVE 'FB242'     TO   COM-GENE-MESANO            EQW9Z
02435                                           COM-CODERR              EQW9ZWFZ
02436                     MOVE CURSEUR     TO   ECR-CSECODCL(I)         EQW9ZWFZ
02437                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
02438                     GO TO  FIN-CONTROLE-LOGIQUE                   EQW9ZWFZ
02439                  END-IF                                           EQW9ZWFZ
02429               END-IF                                              EQW9ZWFZ
02430            ELSE                                                   EQW9ZWFZ
02255               IF ECR-CSECODCO (I) = 'R' OR 'D' OR 'F' OR 'C'      FB01T00B
02432                  MOVE NOR-ALP        TO   ECR-CSECODCA(I)         EQW9ZWFZ
02433                  IF OK                                            EQW9ZWFZ
02434                     MOVE 'FB243'     TO   COM-GENE-MESANO         EQW9ZWFZ
02435                                           COM-CODERR              EQW9ZWFZ
02436                     MOVE CURSEUR     TO   ECR-CSECODCL(I)         EQW9ZWFZ
02437                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
02438                     GO TO  FIN-CONTROLE-LOGIQUE                   EQW9ZWFZ
02439                  END-IF                                           EQW9ZWFZ
02440               END-IF                                              EQW9ZWFZ
02441            END-IF                                                 EQW9ZWFZ
02442            IF    ECR-AJOUTCONO = 'O'                              EQW9ZWFZ
02443               MOVE NOR-ALP        TO   ECR-AJOUTCONA              EQW9ZWFZ
02444               IF OK                                               EQW9ZWFZ
02445                  MOVE 'FB244'     TO   COM-GENE-MESANO            EQW9ZWFZ
02446                                        COM-CODERR                 EQW9ZWFZ
02447                  MOVE CURSEUR     TO   ECR-AJOUTCONL              EQW9ZWFZ
02448                  MOVE  1          TO   KONTROL                    EQW9ZWFZ
02449                  GO TO  FIN-CONTROLE-LOGIQUE                      EQW9ZWFZ
02450               END-IF                                              EQW9ZWFZ
02451            END-IF                                                 EQW9ZWFZ
02452            IF ECR-AJOUTVEHO NOT = 'N' AND ' '                     EQW9ZWFZ
02453               MOVE NOR-ALP        TO   ECR-AJOUTVEHA              EQW9ZWFZ
02454               IF OK                                               EQW9ZWFZ
02455                  MOVE 'FB244'     TO   COM-GENE-MESANO            EQW9ZWFZ
02456                                        COM-CODERR                 EQW9ZWFZ
02457                  MOVE CURSEUR     TO   ECR-AJOUTVEHL              EQW9ZWFZ
02458                  MOVE  1          TO   KONTROL                    EQW9ZWFZ
02459                  GO TO  FIN-CONTROLE-LOGIQUE                      EQW9ZWFZ
02460               END-IF                                              EQW9ZWFZ
02461            END-IF                                                 EQW9ZWFZ
02462         END-IF                                                    EQW9ZWFZ
02463      END-IF.                                                      EQW9ZWFZ
F1936 *                                                                 EQW9ZWFZ
F1936 *  SI CODE ACTION A 'F' => ACCES A ECRAN VALIDATION DES DONNEES   EQW9ZWFZ
F1936 *  (FB11); LE CONTROLE DU NIVEAU D'HABILITATION SE FAIT SUR       EQW9ZWFZ
F1936 *  L'ECRAN FB11                                                   EQW9ZWFZ
F1936 *                                                                 EQW9ZWFZ
02175      IF OK                                                        EQW9ZWFZ
02176         PERFORM VARYING I FROM 1 BY 1 UNTIL                       EQW9ZWFZ
02177            ECR-CSECODCO (I) = 'F'                                 EQW9ZWFZ
02178            OR I = 10                                              EQW9ZWFZ
02179         END-PERFORM                                               EQW9ZWFZ
02180         IF ECR-CSECODCO (I) = 'F'                                 EQW9ZWFZ
02181            IF ECR-DATSORDO (I) = SPACES                           EQW9ZWFZ
02182               IF ECR-STATYPCO (I) NOT = '4R' AND '2R'             EQW9ZWFZ
02183                                             AND 'CC'              EQW9ZWFZ
02184                  MOVE NOR-ALP        TO   ECR-CSECODCA (I)        EQW9ZWFZ
02185                  IF OK                                            EQW9ZWFZ
02186                     MOVE 'FB066'     TO   COM-GENE-MESANO         EQW9ZWFZ
02187                                           COM-CODERR              EQW9ZWFZ
02188                     MOVE CURSEUR     TO   ECR-CSECODCL (I)        EQW9ZWFZ
02189                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
02190                     GO TO  FIN-CONTROLE-LOGIQUE                   EQW9ZWFZ
02191                  END-IF                                           EQW9ZWFZ
02192               END-IF                                              EQW9ZWFZ
02197            ELSE                                                   EQW9ZWFZ
02198               MOVE NOR-ALP        TO   ECR-CSECODCA (I)           EQW9ZWFZ
02199               IF OK                                               EQW9ZWFZ
02200                  MOVE 'FB066'     TO   COM-GENE-MESANO            EQW9ZWFZ
02201                                        COM-CODERR                 EQW9ZWFZ
02202                  MOVE CURSEUR     TO   ECR-CSECODCL (I)           EQW9ZWFZ
02203                  MOVE  1          TO   KONTROL                    EQW9ZWFZ
02204                  GO TO  FIN-CONTROLE-LOGIQUE                      EQW9ZWFZ
02205               END-IF                                              EQW9ZWFZ
02206            END-IF                                                 EQW9ZWFZ
02207         END-IF                                                    EQW9ZWFZ
02208      END-IF.                                                      EQW9ZWFZ
02209 *  SI CODE ACTION A 'M' => L' ELEMENT DE COMPOSITION DU CONTRAT   EQW9ZWFZ
02210 *  DOIT ÍTRE EN COURS                                             EQW9ZWFZ
02211      IF OK                                                        EQW9ZWFZ
02212         PERFORM VARYING I FROM 1 BY 1 UNTIL                       EQW9ZWFZ
02213            ECR-CSECODCO (I) = 'M'                                 EQW9ZWFZ
02214            OR I = 10                                              EQW9ZWFZ
02215         END-PERFORM                                               EQW9ZWFZ
02216         IF ECR-CSECODCO (I) = 'M'                                 EQW9ZWFZ
02217            IF ECR-DATSORDO (I)  NOT = SPACES                      EQW9ZWFZ
02218               MOVE NOR-ALP        TO   ECR-CSECODCA (I)           EQW9ZWFZ
02219               IF OK                                               EQW9ZWFZ
02220                  MOVE 'FB188'     TO   COM-GENE-MESANO            EQW9ZWFZ
02221                                        COM-CODERR                 EQW9ZWFZ
02222                  MOVE CURSEUR     TO   ECR-CSECODCL (I)           EQW9ZWFZ
02223                  MOVE  1          TO   KONTROL                    EQW9ZWFZ
02224                  GO TO  FIN-CONTROLE-LOGIQUE                      EQW9ZWFZ
02225               END-IF                                              EQW9ZWFZ
02226            END-IF                                                 EQW9ZWFZ
02227         END-IF                                                    EQW9ZWFZ
02228      END-IF.                                                      EQW9ZWFZ
02343 *  SI CODE ACTION A 'C' => L' ELEMENT DE COMPOSITION DU CONTRAT   FB01T00B
02344 *  DOIT ÍTRE EN COURS, ÍTRE UN 4R, CC OU 2R UNIQUEMENT            FB01T00B
02345 * LE VEHICULE CHANGÈ NE DOIT PAS ÍTRE UNE ENTRÈE LORS DU MOUVEMENTFB01T00B
02346 * EN COURS                                                        FB01T00B
02229      IF OK                                                        EQW9ZWFZ
02230         PERFORM VARYING I FROM 1 BY 1 UNTIL                       EQW9ZWFZ
02349            ECR-CSECODCO (I) = 'C'                                 FB01T00B
02350            OR I = 10                                              FB01T00B
02351         END-PERFORM                                               FB01T00B
02352         IF ECR-CSECODCO (I) = 'C'                                 FB01T00B
02353            IF ECR-STATYPCO (I) = '4R ' OR 'CC ' OR '2R '          FB01T00B
02354               IF ECR-DATSORDO (I)  NOT = SPACES                   FB01T00B
02355                  MOVE NOR-ALP        TO   ECR-CSECODCA (I)        FB01T00B
02356                  IF OK                                            FB01T00B
02357                     MOVE 'FB188'     TO   COM-GENE-MESANO         FB01T00B
02358                                           COM-CODERR              FB01T00B
02359                     MOVE CURSEUR     TO   ECR-CSECODCL (I)        FB01T00B
02360                     MOVE  1          TO   KONTROL                 FB01T00B
02361                     GO TO  FIN-CONTROLE-LOGIQUE                   FB01T00B
02362                  END-IF                                           FB01T00B
02363               ELSE                                                FB01T00B
02364                 COMPUTE IR = (COM-ITEM-AFFICH - 1) * 10 + I       FB01T00B
02365                 IF WSS-CHANGE(IR) = '1'                           FB01T00B
02366                    MOVE NOR-ALP      TO   ECR-CSECODCA (I)        FB01T00B
02367                    IF OK                                          FB01T00B
02368                       MOVE 'FB448'   TO   COM-GENE-MESANO         FB01T00B
02369                                           COM-CODERR              FB01T00B
02370                       MOVE CURSEUR   TO   ECR-CSECODCL (I)        FB01T00B
02371                       MOVE  1        TO   KONTROL                 FB01T00B
02372                       GO TO  FIN-CONTROLE-LOGIQUE                 FB01T00B
02373                    END-IF                                         FB01T00B
02374                 ELSE                                              FB01T00B
02375                    IF WSS-CODE-ACT(IR) = 'I'                      FB01T00B
02376                       MOVE NOR-ALP      TO   ECR-CSECODCA (I)     FB01T00B
02377                       IF OK                                       FB01T00B
02378                          MOVE 'FB447'   TO   COM-GENE-MESANO      FB01T00B
02379                                              COM-CODERR           FB01T00B
02380                          MOVE CURSEUR   TO   ECR-CSECODCL (I)     FB01T00B
02381                          MOVE  1        TO   KONTROL              FB01T00B
02382                          GO TO  FIN-CONTROLE-LOGIQUE              FB01T00B
02383                       END-IF                                      FB01T00B
02384                    END-IF                                         FB01T00B
02385                 END-IF                                            FB01T00B
02386               END-IF                                              FB01T00B
02387            ELSE                                                   FB01T00B
02388               MOVE NOR-ALP        TO   ECR-CSECODCA (I)           FB01T00B
02389               IF OK                                               FB01T00B
02390                  MOVE 'FB445'     TO   COM-GENE-MESANO            FB01T00B
02391                                        COM-CODERR                 FB01T00B
02392                  MOVE CURSEUR     TO   ECR-CSECODCL (I)           FB01T00B
02393                  MOVE  1          TO   KONTROL                    FB01T00B
02394                  GO TO  FIN-CONTROLE-LOGIQUE                      FB01T00B
02395               END-IF                                              FB01T00B
02396            END-IF                                                 FB01T00B
02397         END-IF                                                    FB01T00B
02398      END-IF.                                                      FB01T00B
02399      IF OK                                                        FB01T00B
02400         PERFORM VARYING I FROM 1 BY 1 UNTIL                       FB01T00B
02231            ECR-CSECODCO (I) = 'R'                                 EQW9ZWFZ
02232            OR I = 10                                              EQW9ZWFZ
02233         END-PERFORM                                               EQW9ZWFZ
02234         IF ECR-CSECODCO (I) = 'R'                                 EQW9ZWFZ
02235 *  SI CODE ACTION A 'R' => L' ELEMENT DE COMPOSITION DU CONTRAT   EQW9ZWFZ
02236 *  DOIT ÍTRE SORTIE ‡ L'EFFET DU JOURS                            EQW9ZWFZ
02237            MOVE ECR-DATSORDO (I)           TO WSS-JJMMSSAA        EQW9ZWFZ
02238            MOVE CORRESPONDING WSS-JJMMSSAA TO WSS-SSAAMMJJ        EQW9ZWFZ
02239            IF COM-FB-DATE-EFFET-1 NOT = SPACES AND LOW-VALUE      EQW9ZWFZ
02240               IF WSS-SSAAMMJJ     NOT  = COM-FB-DATE-EFFET-1      EQW9ZWFZ
02241                  MOVE NOR-ALP        TO   ECR-CSECODCA (I)        EQW9ZWFZ
02242                  IF OK                                            EQW9ZWFZ
02243                     MOVE 'FB188'     TO   COM-GENE-MESANO         EQW9ZWFZ
02244                                           COM-CODERR              EQW9ZWFZ
02245                     MOVE CURSEUR     TO   ECR-CSECODCL (I)        EQW9ZWFZ
02246                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
02247                     GO TO  FIN-CONTROLE-LOGIQUE                   EQW9ZWFZ
02248                  END-IF                                           EQW9ZWFZ
02249               END-IF                                              EQW9ZWFZ
02250            END-IF                                                 EQW9ZWFZ
02251 *  SI CODE ACTION A 'R' => L' ELEMENT DE COMPOSITION DU CONTRAT   EQW9ZWFZ
02252 *  VA ETRE REMIS EN COURS CONTROLE SI STATUS PM                   EQW9ZWFZ
02253            IF OK                                                  EQW9ZWFZ
02254               IF COM-FB-NBRE-PERS-PM  = 1                         EQW9ZWFZ
02255                  AND (ECR-STATYPCO (I) = 'CF' OR 'PM' OR 'CJ' OR  EQW9ZWFZ
02256                              'ENSP' OR 'ENAP')                    EQW9ZWFZ
02257                  MOVE NOR-ALP        TO   ECR-CSECODCA (I)        EQW9ZWFZ
02258                  IF OK                                            EQW9ZWFZ
02259                     MOVE 'FB186'     TO   COM-GENE-MESANO         EQW9ZWFZ
02260                                        COM-CODERR                 EQW9ZWFZ
02261                     MOVE CURSEUR     TO   ECR-CSECODCL (I)        EQW9ZWFZ
02262                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
02263                     GO TO  FIN-CONTROLE-LOGIQUE                   EQW9ZWFZ
02264                  END-IF                                           EQW9ZWFZ
02265               END-IF                                              EQW9ZWFZ
02266            END-IF                                                 EQW9ZWFZ
02267            IF OK                                                  EQW9ZWFZ
02268               IF COM-FB-NBRE-PERS-ENC = 10                        EQW9ZWFZ
02269                  AND (ECR-STATYPCO (I) = 'CF' OR 'PM' OR 'CJ' OR  EQW9ZWFZ
02270                              'ENSP' OR 'ENAP')                    EQW9ZWFZ
02271                  MOVE NOR-ALP        TO   ECR-CSECODCA (I)        EQW9ZWFZ
02272                  IF OK                                            EQW9ZWFZ
02273                     MOVE 'FB069'     TO   COM-GENE-MESANO         EQW9ZWFZ
02274                                        COM-CODERR                 EQW9ZWFZ
02275                     MOVE CURSEUR     TO   ECR-CSECODCL (I)        EQW9ZWFZ
02276                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
02277                     GO TO  FIN-CONTROLE-LOGIQUE                   EQW9ZWFZ
02278                  END-IF                                           EQW9ZWFZ
02279               END-IF                                              EQW9ZWFZ
02280            END-IF                                                 EQW9ZWFZ
02281            IF OK                                                  EQW9ZWFZ
02282               IF COM-FB-NBRE-VEHI-ENC = 10                        EQW9ZWFZ
02283                  AND (ECR-STATYPCO (I) = '4R' OR '2R' OR 'CC' OR  EQW9ZWFZ
02284                              'REM' OR 'CAR')                      EQW9ZWFZ
02285                  MOVE NOR-ALP        TO   ECR-CSECODCA (I)        EQW9ZWFZ
02286                  IF OK                                            EQW9ZWFZ
02287                     MOVE 'FB070'     TO   COM-GENE-MESANO         EQW9ZWFZ
02288                                           COM-CODERR              EQW9ZWFZ
02289                     MOVE CURSEUR     TO   ECR-CSECODCL (I)        EQW9ZWFZ
02290                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
02291                     GO TO  FIN-CONTROLE-LOGIQUE                   EQW9ZWFZ
02292                  END-IF                                           EQW9ZWFZ
02293               END-IF                                              EQW9ZWFZ
02294            END-IF                                                 EQW9ZWFZ
02295         END-IF                                                    EQW9ZWFZ
02296      END-IF.                                                      EQW9ZWFZ
02297 *  CONTROLE EN CAS DE SORTIE                                      EQW9ZWFZ
02298      IF OK                                                        EQW9ZWFZ
02299         PERFORM VARYING I FROM 1 BY 1 UNTIL                       EQW9ZWFZ
02300            ECR-CSECODCO (I) = 'D'                                 EQW9ZWFZ
02301            OR I = 10                                              EQW9ZWFZ
02302         END-PERFORM                                               EQW9ZWFZ
02303 *  SI CODE ACTION A 'D' => L' ELEMENT DE COMPOSITION DU CONTRAT   EQW9ZWFZ
02304 *  DOIT ÍTRE EN COURS                                             EQW9ZWFZ
02305         IF OK                                                     EQW9ZWFZ
02306            IF ECR-CSECODCO (I) = 'D'                              EQW9ZWFZ
02307               IF ECR-DATSORDO (I) NOT = SPACES                    EQW9ZWFZ
02308                  MOVE NOR-ALP        TO   ECR-CSECODCA (I)        EQW9ZWFZ
02309                  IF OK                                            EQW9ZWFZ
02310                     MOVE 'FB188'     TO   COM-GENE-MESANO         EQW9ZWFZ
02311                                           COM-CODERR              EQW9ZWFZ
02312                     MOVE CURSEUR     TO   ECR-CSECODCL (I)        EQW9ZWFZ
02313                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
02314                     GO TO  FIN-CONTROLE-LOGIQUE                   EQW9ZWFZ
02315                  END-IF                                           EQW9ZWFZ
02316               END-IF                                              EQW9ZWFZ
02317            END-IF                                                 EQW9ZWFZ
02318         END-IF                                                    EQW9ZWFZ
02319 *  SI CODE ACTION A 'D' => LE MOTIF DOIT ETRE RENSEIGNE           EQW9ZWFZ
02320         IF OK                                                     EQW9ZWFZ
02321            IF  ECR-CSECODCO (I) = 'D'                             EQW9ZWFZ
02322               IF ECR-MOTIFSCO (I) = SPACES OR LOW-VALUE OR '?'    EQW9ZWFZ
02323                  MOVE NOR-ALP        TO   ECR-CSECODCA (I)        EQW9ZWFZ
02324                  IF OK                                            EQW9ZWFZ
02325                     MOVE 'FB281'     TO   COM-GENE-MESANO         EQW9ZWFZ
02326                                           COM-CODERR              EQW9ZWFZ
02327                     MOVE CURSEUR     TO   ECR-CSECODCL (I)        EQW9ZWFZ
02328                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
02329                     GO TO  FIN-CONTROLE-LOGIQUE                   EQW9ZWFZ
02330                  END-IF                                           EQW9ZWFZ
02331               END-IF                                              EQW9ZWFZ
02332            END-IF                                                 EQW9ZWFZ
02333         END-IF                                                    EQW9ZWFZ
02334 * POUR SORTIR UNE PERSONNE OU UN VÈHICULE IL FAUT QUE LE NOMBRE   EQW9ZWFZ
02335 * DE PERSONNES OU DE VÈHICULES SOIENT > 1                         EQW9ZWFZ
02336         IF OK                                                     EQW9ZWFZ
02337            IF ECR-CSECODCO(I) = 'D'                               EQW9ZWFZ
02338               IF ECR-STATYPCO (I) = 'CF' OR 'PM' OR 'CJ' OR 'ENSP'EQW9ZWFZ
02339                                     OR 'ENAP'                     EQW9ZWFZ
02340                  IF COM-FB-NBRE-PERS-ENC < 2                      EQW9ZWFZ
02341                     MOVE NOR-ALP        TO   ECR-CSECODCA(I)      EQW9ZWFZ
02342                     IF OK                                         EQW9ZWFZ
02343                        MOVE 'FB190'     TO   COM-GENE-MESANO      EQW9ZWFZ
02344                                           COM-CODERR              EQW9ZWFZ
02345                        MOVE CURSEUR     TO   ECR-CSECODCL(I)      EQW9ZWFZ
02346                        MOVE  1          TO   KONTROL              EQW9ZWFZ
02347                        GO TO  FIN-CONTROLE-LOGIQUE                EQW9ZWFZ
02348                     END-IF                                        EQW9ZWFZ
02349                  END-IF                                           EQW9ZWFZ
02350               ELSE                                                EQW9ZWFZ
02351                  IF COM-FB-NBRE-VEHI-ENC < 2                      EQW9ZWFZ
02352                     MOVE NOR-ALP        TO   ECR-CSECODCA(I)      EQW9ZWFZ
02353                     IF OK                                         EQW9ZWFZ
02354                        MOVE 'FB189'     TO   COM-GENE-MESANO      EQW9ZWFZ
02355                                           COM-CODERR              EQW9ZWFZ
02356                        MOVE CURSEUR     TO   ECR-CSECODCL(I)      EQW9ZWFZ
02357                        MOVE  1          TO   KONTROL              EQW9ZWFZ
02358                        GO TO  FIN-CONTROLE-LOGIQUE                EQW9ZWFZ
02359                     END-IF                                        EQW9ZWFZ
02360                  END-IF                                           EQW9ZWFZ
02361               END-IF                                              EQW9ZWFZ
02362            END-IF                                                 EQW9ZWFZ
02363         END-IF                                                    EQW9ZWFZ
02364 * POUR SORTIR LE DERNIER VÈHICULE TRACTEUR (4R OU CC)             EQW9ZWFZ
02365 * IL FAUT QUE LE NOMBRE                                           EQW9ZWFZ
02366 * DE VEHICULE TRACTE (REM OU CAR) SOIENT = ZERO                   EQW9ZWFZ
02367         IF OK                                                     EQW9ZWFZ
02368            IF ECR-CSECODCO(I) = 'D'                               EQW9ZWFZ
02369               IF ECR-STATYPCO (I) = '4R' OR 'CC'                  EQW9ZWFZ
02370                  IF COM-FB-NBRE-VEHI-TRACTEUR = 1                 EQW9ZWFZ
02371                     AND COM-FB-NBRE-VEHI-TRACTE > 0               EQW9ZWFZ
02372                     MOVE NOR-ALP        TO   ECR-CSECODCA(I)      EQW9ZWFZ
02373                     IF OK                                         EQW9ZWFZ
02374                        MOVE 'FB189'     TO   COM-GENE-MESANO      EQW9ZWFZ
02375                                           COM-CODERR              EQW9ZWFZ
02376                        MOVE CURSEUR     TO   ECR-CSECODCL(I)      EQW9ZWFZ
02377                        MOVE  1          TO   KONTROL              EQW9ZWFZ
02378                        GO TO  FIN-CONTROLE-LOGIQUE                EQW9ZWFZ
02379                     END-IF                                        EQW9ZWFZ
02380                  END-IF                                           EQW9ZWFZ
02381               END-IF                                              EQW9ZWFZ
02382            END-IF                                                 EQW9ZWFZ
02383         END-IF                                                    EQW9ZWFZ
02384      END-IF.                                                      EQW9ZWFZ
02385 *                                                                 EQW9ZWFZ
02386 * IMPOSSIBLE DE RENSEIGNER LE MOTIF SI VEHICULE EN COURS          EQW9ZWFZ
02387 *                                                                 EQW9ZWFZ
02388      IF OK                                                        EQW9ZWFZ
02389 *  SI LE MOTIF EST RENSEIGNE LE VEHICULE DOIT ETRE SORTIE         EQW9ZWFZ
02390         PERFORM VARYING I FROM 1 BY 1 UNTIL                       EQW9ZWFZ
02391            (ECR-MOTIFSCO (I) NOT = SPACES AND LOW-VALUE)          EQW9ZWFZ
02392            OR I = 10                                              EQW9ZWFZ
02393         END-PERFORM                                               EQW9ZWFZ
02394         IF ECR-MOTIFSCO (I) NOT = SPACES AND LOW-VALUE AND '?'    EQW9ZWFZ
02395            IF ECR-CSECODCO (I) NOT = 'D'                          EQW9ZWFZ
02396               IF ECR-DATSORDO (I) = SPACES                        EQW9ZWFZ
02397                  MOVE NOR-ALP        TO   ECR-CSECODCA (I)        EQW9ZWFZ
02398                  IF OK                                            EQW9ZWFZ
02399                     MOVE 'FB282'     TO   COM-GENE-MESANO         EQW9ZWFZ
02400                                           COM-CODERR              EQW9ZWFZ
02401                     MOVE CURSEUR     TO   ECR-CSECODCL (I)        EQW9ZWFZ
02402                     MOVE  1          TO   KONTROL                 EQW9ZWFZ
02403                     GO TO  FIN-CONTROLE-LOGIQUE                   EQW9ZWFZ
02404                  END-IF                                           EQW9ZWFZ
02405               END-IF                                              EQW9ZWFZ
02406            END-IF                                                 EQW9ZWFZ
02407         END-IF                                                    EQW9ZWFZ
02408      END-IF.                                                      EQW9ZWFZ

F7833 *  SI CODE ACTION A 'V' => LE SUSPENS DOIT ÍTRE                   EQW9ZWFZ
F7833 *                          EN DEMANDE D'ACCORD                    EQW9ZWFZ
F7833 *  AU NIVEAU DU MODULE DE CONTROLE TECHNIQUE ET DES ECRANS        EQW9ZWFZ
F7833 *  SUIVANTS, LE CODE SÈLECTION SERA GÈRÈ COMME 'M'                EQW9ZWFZ
F7833      IF OK                                                        EQW9ZWFZ
F7833         PERFORM VARYING I FROM 1 BY 1 UNTIL                       EQW9ZWFZ
F7833            ECR-CSECODCO (I) = 'V'                                 EQW9ZWFZ
F7833            OR I = 10                                              EQW9ZWFZ
F7833         END-PERFORM                                               EQW9ZWFZ
F7833         IF ECR-CSECODCO (I) = 'V'                                 EQW9ZWFZ
F7833            IF INF-ETAT OF TS-SUSPENS1 NOT = '3' AND '7'           EQW9ZWFZ
F7833               MOVE NOR-ALP        TO   ECR-CSECODCA (I)           EQW9ZWFZ
F7833               IF OK                                               EQW9ZWFZ
F7833                  MOVE 'FB066'     TO   COM-GENE-MESANO            EQW9ZWFZ
F7833                                        COM-CODERR                 EQW9ZWFZ
F7833                  MOVE CURSEUR     TO   ECR-CSECODCL (I)           EQW9ZWFZ
F7833                  MOVE  1          TO   KONTROL                    EQW9ZWFZ
F7833                  GO TO  FIN-CONTROLE-LOGIQUE                      EQW9ZWFZ
F7833               END-IF                                              EQW9ZWFZ
F7833            ELSE                                                   EQW9ZWFZ
F7833               MOVE 'M' TO ECR-CSECODCO (I)                        EQW9ZWFZ
F7833            END-IF                                                 EQW9ZWFZ
F7833         END-IF                                                    EQW9ZWFZ
F7833      END-IF.                                                      EQW9ZWFZ
02464 *                                                                 EQW9ZWFZ
02465 *    SI DOUBLE AFFICHAGE  AVEC MESSAGE  INFORMATIF :              EQW9ZWFZ
02466 *    IF ........                                                  EQW9ZWFZ
02467 *    AND ECRAN-MODIFIE                                            EQW9ZWFZ
02468 *        MOVE 'SQXXX'             TO COM-GENE-MESINF              EQW9ZWFZ
02469 *                                    COM-CODERR                   EQW9ZWFZ
02470 *        MOVE 'O'                 TO W-REAF                       EQW9ZWFZ
02471 *    END-IF.                                                      EQW9ZWFZ
02472 *                                                                 EQW9ZWFZ
02473  FIN-CONTROLE-LOGIQUE.  EXIT.                                     EQW9ZWFZ
02474 *                                                                 EQW9ZWFZ
02475 * RECHERCHE DU NIVEAU D'HABILITATION POUR ACCES VDON  BLOQUAGE    EQW9ZWFZ
02476 * SI < 8                                                          EQW9ZWFZ
02477 *                                                                 EQW9ZWFZ
02496 *--- TEST DU CODE AJOUT VEHICULE POUR APPEL A L'ECRAN D'AIDE      EQW9ZWFZ
02497  DETER-AIDE-AJTVEHI.                                              EQW9ZWFZ
02498 *------------------                                               EQW9ZWFZ
02499      IF ECR-AJOUTVEHO = '?'                                       EQW9ZWFZ
02500         MOVE 'O' TO WSS-APPEL-AIDE-AJTVEHI                        EQW9ZWFZ
02501      ELSE                                                         EQW9ZWFZ
02502         MOVE 'N' TO WSS-APPEL-AIDE-AJTVEHI                        EQW9ZWFZ
02503      END-IF.                                                      EQW9ZWFZ
02504                                                                   EQW9ZWFZ
02505  FDETER-AIDE-AJTVEHI.  EXIT.                                      EQW9ZWFZ
02506 *                                                                 EQW9ZWFZ
02507 *--- TEST DU CODE MOTIF SELECTION POUR APPEL A L'ECRAN D'AIDE     EQW9ZWFZ
02508  DETER-AIDE-AJTMOTI.                                              EQW9ZWFZ
02509 *------------------                                               EQW9ZWFZ
02510      IF ECR-MOTIFSCO (I) = '?'                                    EQW9ZWFZ
02511         MOVE 'O' TO WSS-APPEL-AIDE-MOTIF                          EQW9ZWFZ
02512      END-IF.                                                      EQW9ZWFZ
02513                                                                   EQW9ZWFZ
02514  FDETER-AIDE-AJTMOTI.  EXIT.                                      EQW9ZWFZ
02515 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02516 * TRAITEMENT DE LA TACHE * FB01 * TRAITEMENT NORMAL               EQW9ZWFZ
02517 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02518 *                                                                 EQW9ZWFZ
02519  TRAITEMENT-TACHE.                                                EQW9ZWFZ
02520 *                                                                 EQW9ZWFZ
02521      PERFORM TRAITEMENT-FICHIER THRU                              EQW9ZWFZ
02522              FIN-TRAITEMENT-FICHIER.                              EQW9ZWFZ
02523 *                                                                 EQW9ZWFZ
02524      IF  (COM-GENE-MESINF     = SPACES OR LOW-VALUE)              EQW9ZWFZ
02525      AND (COM90C00-MESINF NOT = SPACES AND LOW-VALUE)             EQW9ZWFZ
02526           MOVE COM90C00-MESINF TO COM-GENE-MESINF                 EQW9ZWFZ
02527                                   COM-CODERR                      EQW9ZWFZ
02528           MOVE 'O' TO W-REAF                                      EQW9ZWFZ
02529      END-IF.                                                      EQW9ZWFZ
02530      IF  (COM-GENE-MESANO     = SPACES OR LOW-VALUE)              EQW9ZWFZ
02531      AND (COM90C00-MESANO NOT = SPACES AND LOW-VALUE)             EQW9ZWFZ
02532           MOVE COM90C00-MESANO TO COM-GENE-MESANO                 EQW9ZWFZ
02533                                   COM-CODERR                      EQW9ZWFZ
02534           MOVE 'O' TO W-REAF                                      EQW9ZWFZ
02535      END-IF.                                                      EQW9ZWFZ
02536 *                                                                 EQW9ZWFZ
02537      PERFORM TRAITEMENT-COMMAREA THRU                             EQW9ZWFZ
02538              FIN-TRAITEMENT-COMMAREA.                             EQW9ZWFZ
02539 *                                                                 EQW9ZWFZ
02540      PERFORM TRAITEMENT-MAP THRU                                  EQW9ZWFZ
02541              FIN-TRAITEMENT-MAP.                                  EQW9ZWFZ
02542 *                                                                 EQW9ZWFZ
02543  FIN-TRAITEMENT-TACHE.  EXIT.                                     EQW9ZWFZ
02544 *                                                                 EQW9ZWFZ
02545 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02546 *  GESTION DES FICHIERS     * FB01 * TRAITEMENT NORMAL            EQW9ZWFZ
02547 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02548 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02549 *                                                               * EQW9ZWFZ
02550 *                -------------------------------                * EQW9ZWFZ
02551 *                -    GESTION DES FICHIERS     -                * EQW9ZWFZ
02552 *                -------------------------------                * EQW9ZWFZ
02553 *                               -                               * EQW9ZWFZ
02554 *           -----------------------------------------           * EQW9ZWFZ
02555 *           -            -             -            -           * EQW9ZWFZ
02556 * ---------------- ------------ ---------------- -------------- * EQW9ZWFZ
02557 * - DETERMINATION- - CREATION - - MODIFICATION - - SUPRESSION - * EQW9ZWFZ
02558 * ---------------- ------------ ---------------- -------------- * EQW9ZWFZ
02559 *                                                               * EQW9ZWFZ
02560 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02561  TRAITEMENT-FICHIER.                                              EQW9ZWFZ
02562 *------------------*                                              EQW9ZWFZ
02563 *                                                                 EQW9ZWFZ
02564      PERFORM MISE-A-JOUR-TS THRU                                  EQW9ZWFZ
02565              FIN-MISE-A-JOUR-TS.                                  EQW9ZWFZ
02566 *                                                                 EQW9ZWFZ
02567      PERFORM ECR-TS-SUSPENS THRU                                  EQW9ZWFZ
02568              FIN-ECR-TS-SUSPENS.                                  EQW9ZWFZ
02569 *    PERFORM ECR-TS-TECHNIQUE THRU                                EQW9ZWFZ
02570 *            FECR-TS-TECHNIQUE.                                   EQW9ZWFZ
02571      PERFORM LECT-TS-PRODUIT-TECH THRU FLECT-TS-PRODUIT-TECH.     EQW9ZWFZ
02572 *                                                                 EQW9ZWFZ
02573 *                                                                 EQW9ZWFZ
02574      PERFORM APPEL-MA90T00 THRU FAPPEL-MA90T00.                   EQW9ZWFZ
02575      IF  (COM-GENE-MESINF     = SPACES OR LOW-VALUE)              EQW9ZWFZ
02576      AND (COM90C00-MESINF NOT = SPACES AND LOW-VALUE)             EQW9ZWFZ
02577           MOVE COM90C00-MESINF TO COM-GENE-MESINF                 EQW9ZWFZ
02578                                   COM-CODERR                      EQW9ZWFZ
02579           MOVE 'O' TO W-REAF                                      EQW9ZWFZ
02580      END-IF.                                                      EQW9ZWFZ
02581                                                                   EQW9ZWFZ
02582      IF  (COM-GENE-MESANO     = SPACES OR LOW-VALUE)              EQW9ZWFZ
02583      AND (COM90C00-MESANO NOT = SPACES AND LOW-VALUE)             EQW9ZWFZ
02584           MOVE COM90C00-MESANO TO COM-GENE-MESANO                 EQW9ZWFZ
02585                                   COM-CODERR                      EQW9ZWFZ
02586           MOVE 'O' TO W-REAF                                      EQW9ZWFZ
02587      END-IF.                                                      EQW9ZWFZ
02588      IF INF-ETAT OF TS-SUSPENS1 NOT = '5' AND '7'                 EQW9ZWFZ
02589         IF (INF-ETAT OF TS-SUSPENS1 = '4' AND ECRAN-MODIFIE)      EQW9ZWFZ
02590         OR  INF-ETAT OF TS-SUSPENS1 NOT = '4'                     EQW9ZWFZ
02591             PERFORM APPEL-MA90T20 THRU                            EQW9ZWFZ
02592                     FAPPEL-MA90T20                                EQW9ZWFZ
02593         END-IF                                                    EQW9ZWFZ
02594      END-IF.                                                      EQW9ZWFZ
02595      IF  (MA90C20-NB-REJETS > 1)                                  EQW9ZWFZ
02596      AND (COM-GENE-MESINF = SPACES OR LOW-VALUE)                  EQW9ZWFZ
02597           MOVE 'FOH99'  TO COM-GENE-MESINF                        EQW9ZWFZ
02598                            COM-CODERR                             EQW9ZWFZ
02599           MOVE 'O' TO W-REAF                                      EQW9ZWFZ
02600      ELSE                                                         EQW9ZWFZ
02601          IF  (MA90C20-MESINF  NOT = SPACES AND LOW-VALUE)         EQW9ZWFZ
02602          AND (COM-GENE-MESINF = SPACES OR LOW-VALUE)              EQW9ZWFZ
02603               MOVE MA90C20-MESINF TO COM-GENE-MESINF              EQW9ZWFZ
02604                                      COM-CODERR                   EQW9ZWFZ
02605               MOVE 'O' TO W-REAF                                  EQW9ZWFZ
02606          END-IF                                                   EQW9ZWFZ
02607      END-IF.                                                      EQW9ZWFZ
02608      IF ECR-XCDECO  = 'REJ' AND MA90C20-NB-REJETS = 0             EQW9ZWFZ
02609         MOVE 'FOH98' TO COM-GENE-MESINF                           EQW9ZWFZ
02610                         COM-CODERR                                EQW9ZWFZ
02611         MOVE 'O'     TO  W-REAF                                   EQW9ZWFZ
02612         MOVE  SPACES TO  ECR-XCDECI                               EQW9ZWFZ
02613      END-IF.                                                      EQW9ZWFZ
02614 *                                                                 EQW9ZWFZ
02615  FIN-TRAITEMENT-FICHIER. EXIT.                                    EQW9ZWFZ
02616 *                                                                 EQW9ZWFZ
02617 ************************************************************      EQW9ZWFZ
02618 *   STOCKAGE DES INFORMATIONS DANS LA TS ET COMMAREA       *      EQW9ZWFZ
02619 ************************************************************      EQW9ZWFZ
02620  MISE-A-JOUR-TS.                                                  EQW9ZWFZ
02621 *                                                                 EQW9ZWFZ
02622 *                                                                 EQW9ZWFZ
02623 *--- CODE SELECTION                                               EQW9ZWFZ
02624 *                                                                 EQW9ZWFZ
02625      PERFORM VARYING IC FROM 1 BY 1                               EQW9ZWFZ
02626                      UNTIL IC > 10                                EQW9ZWFZ
02627                      OR ECR-CSECODCO (IC) NOT = SPACES            EQW9ZWFZ
02628                      OR (ECR-DATENTDO(IC) = SPACES OR LOW-VALUE)  EQW9ZWFZ
02629      END-PERFORM.                                                 EQW9ZWFZ
02630      IF IC NOT > 10                                               EQW9ZWFZ
02631         EVALUATE ECR-CSECODCO (IC)                                EQW9ZWFZ
02632           WHEN 'D'                                                EQW9ZWFZ
02633             IF ECR-MOTIFSCO (IC) NOT = '?'                        EQW9ZWFZ
02634                MOVE 'D'                TO COM-FB-CODE-ACTION      EQW9ZWFZ
02635                MOVE 'O'                TO W-REAF                  EQW9ZWFZ
02636                COMPUTE IG = (COM-ITEM-AFFICH - 1) * 10 + IC       EQW9ZWFZ
02637                MOVE WSS-RANTS (IG)     TO  COM-FB-RANG-TS-LIRE    EQW9ZWFZ
02638                MOVE WSS-TYPTS (IG)     TO  COM-FB-TYPE-TS-LIRE    EQW9ZWFZ
02639                PERFORM SORTIE-TS       THRU FIN-SORTIE-TS         EQW9ZWFZ
02640             END-IF                                                EQW9ZWFZ
02641           WHEN 'F'                                                EQW9ZWFZ
02642             MOVE 'F'                TO COM-FB-CODE-ACTION         EQW9ZWFZ
02643             COMPUTE IG = (COM-ITEM-AFFICH - 1) * 10 + IC          EQW9ZWFZ
02644             MOVE WSS-RANTS (IG)     TO  COM-FB-RANG-TS-LIRE       EQW9ZWFZ
02645             MOVE 'V'                TO  COM-FB-TYPE-TS-LIRE       EQW9ZWFZ
02646           WHEN 'M'                                                EQW9ZWFZ
02647             MOVE 'M'                TO COM-FB-CODE-ACTION         EQW9ZWFZ
02648             COMPUTE IG = (COM-ITEM-AFFICH - 1) * 10 + IC          EQW9ZWFZ
02649             MOVE WSS-RANTS (IG)     TO  COM-FB-RANG-TS-LIRE       EQW9ZWFZ
02650             MOVE WSS-TYPTS (IG)     TO  COM-FB-TYPE-TS-LIRE       EQW9ZWFZ
02651             EVALUATE WSS-STATYP (IG)                              EQW9ZWFZ
02652                WHEN 'CF'                                          EQW9ZWFZ
02653                   SUBTRACT  1    FROM COM-FB-NBRE-PERS-CF         EQW9ZWFZ
02654                WHEN 'CJ'                                          EQW9ZWFZ
02655                   SUBTRACT  1    FROM COM-FB-NBRE-PERS-CJ         EQW9ZWFZ
02656                WHEN 'PM'                                          EQW9ZWFZ
02657                   SUBTRACT  1    FROM COM-FB-NBRE-PERS-PM         EQW9ZWFZ
02658                WHEN 'ENSP'                                        EQW9ZWFZ
02659                   SUBTRACT  1    FROM COM-FB-NBRE-PERS-EN         EQW9ZWFZ
02660                WHEN 'ENSA'                                        EQW9ZWFZ
02661                   SUBTRACT  1    FROM COM-FB-NBRE-PERS-EN         EQW9ZWFZ
02662             END-EVALUATE                                          EQW9ZWFZ
02760           WHEN 'C'                                                FB01T00B
02761             MOVE 'C'                TO COM-FB-CODE-ACTION         FB01T00B
02762             COMPUTE IG = (COM-ITEM-AFFICH - 1) * 10 + IC          FB01T00B
02764             PERFORM CHANGEMENT-VEHI THRU FIN-CHANGEMENT-VEHI      FB01T00B
02765             MOVE COM-FB-RANG-MAX-TSVEHI TO COM-FB-RANG-TS-LIRE    FB01T00B
02766             MOVE WSS-TYPTS (IG)     TO  COM-FB-TYPE-TS-LIRE       FB01T00B
02663           WHEN 'R'                                                EQW9ZWFZ
02664             MOVE 'O'                TO W-REAF                     EQW9ZWFZ
02665             MOVE 'R'                TO COM-FB-CODE-ACTION         EQW9ZWFZ
02666             COMPUTE IG = (COM-ITEM-AFFICH - 1) * 10 + IC          EQW9ZWFZ
02667             PERFORM RESTAURATION-TS THRU FIN-RESTAURATION-TS      EQW9ZWFZ
02668           WHEN '?'                                                EQW9ZWFZ
02669             MOVE ' '                TO COM-FB-CODE-ACTION         EQW9ZWFZ
02670             MOVE 'O'                TO WSS-APPEL-AIDE-AJTVEHI     EQW9ZWFZ
02671             COMPUTE IG = (COM-ITEM-AFFICH - 1) * 10 + IC          EQW9ZWFZ
02672         END-EVALUATE                                              EQW9ZWFZ
02673      END-IF.                                                      EQW9ZWFZ
02674 *                                                                 EQW9ZWFZ
02675 *--- CODE AJOUT PERSONNE                                          EQW9ZWFZ
02676 *                                                                 EQW9ZWFZ
02677      IF ECR-AJOUTCONO                                             EQW9ZWFZ
02678        = 'O'                                                      EQW9ZWFZ
02679          MOVE 'A'                TO COM-FB-CODE-ACTION            EQW9ZWFZ
02680          MOVE ZERO               TO  COM-FB-RANG-TS-LIRE          EQW9ZWFZ
02681          MOVE 'P'                TO  COM-FB-TYPE-TS-LIRE          EQW9ZWFZ
02682      END-IF.                                                      EQW9ZWFZ
02683 *                                                                 EQW9ZWFZ
02684 *--- CODE AJOUT VEHICULE                                          EQW9ZWFZ
02685 *                                                                 EQW9ZWFZ
02686      EVALUATE ECR-AJOUTVEHO                                       EQW9ZWFZ
02687        WHEN '1'                                                   EQW9ZWFZ
02688          MOVE '1'                TO COM-FB-CODE-ACTION            EQW9ZWFZ
02689          MOVE ZERO               TO  COM-FB-RANG-TS-LIRE          EQW9ZWFZ
02690          MOVE 'V'                TO  COM-FB-TYPE-TS-LIRE          EQW9ZWFZ
02691        WHEN '2'                                                   EQW9ZWFZ
02692          MOVE '2'                TO COM-FB-CODE-ACTION            EQW9ZWFZ
02693          MOVE ZERO               TO  COM-FB-RANG-TS-LIRE          EQW9ZWFZ
02694          MOVE 'V'                TO  COM-FB-TYPE-TS-LIRE          EQW9ZWFZ
02695        WHEN '3'                                                   EQW9ZWFZ
02696          MOVE '3'                TO COM-FB-CODE-ACTION            EQW9ZWFZ
02697          MOVE ZERO               TO  COM-FB-RANG-TS-LIRE          EQW9ZWFZ
02698          MOVE 'V'                TO  COM-FB-TYPE-TS-LIRE          EQW9ZWFZ
02699        WHEN '4'                                                   EQW9ZWFZ
02700          MOVE '4'                TO COM-FB-CODE-ACTION            EQW9ZWFZ
02701          MOVE ZERO               TO  COM-FB-RANG-TS-LIRE          EQW9ZWFZ
02702          MOVE 'V'                TO  COM-FB-TYPE-TS-LIRE          EQW9ZWFZ
02703        WHEN '5'                                                   EQW9ZWFZ
02704          MOVE '5'                TO COM-FB-CODE-ACTION            EQW9ZWFZ
02705          MOVE ZERO               TO  COM-FB-RANG-TS-LIRE          EQW9ZWFZ
02706          MOVE 'V'                TO  COM-FB-TYPE-TS-LIRE          EQW9ZWFZ
02707        WHEN 'N'                                                   EQW9ZWFZ
02708          IF COM-FB-CODE-ACTION = '1' OR '2' OR '3' OR '4'         EQW9ZWFZ
02709             OR '5' OR '?'                                         EQW9ZWFZ
02710             MOVE ' '                TO COM-FB-CODE-ACTION         EQW9ZWFZ
02711             MOVE ZERO               TO  COM-FB-RANG-TS-LIRE       EQW9ZWFZ
02712          END-IF                                                   EQW9ZWFZ
02713        WHEN '?'                                                   EQW9ZWFZ
02714          MOVE '?'                TO COM-FB-CODE-ACTION            EQW9ZWFZ
02715          MOVE ZERO               TO COM-FB-RANG-TS-LIRE           EQW9ZWFZ
02716          MOVE 'O'                TO W-REAF                        EQW9ZWFZ
02717      END-EVALUATE.                                                EQW9ZWFZ
02718 *                                                                 EQW9ZWFZ
02719 *--- CODE MOTIF     (CAS SI UNIQUEMENT SAISIE DU MOTIF)           EQW9ZWFZ
02720 *                                                                 EQW9ZWFZ
02721      PERFORM VARYING IC FROM 1 BY 1                               EQW9ZWFZ
02722                      UNTIL IC > 10                                EQW9ZWFZ
02723                      OR (ECR-DATENTDO(IC) = SPACES OR LOW-VALUE)  EQW9ZWFZ
02724         COMPUTE IG = (COM-ITEM-AFFICH - 1) * 10 + IC              EQW9ZWFZ
02725         IF WSS-MOTIFC (IG) = LOW-VALUE                            EQW9ZWFZ
02726            MOVE SPACES      TO WSS-MOTIFC (IG)                    EQW9ZWFZ
02727         END-IF                                                    EQW9ZWFZ
02728         IF ((WSS-MOTIFC (IG) NOT = ECR-MOTIFSCO (IC))             EQW9ZWFZ
02729            OR (ECR-MOTIFSCO (IC) NOT = SPACES))                   EQW9ZWFZ
02730            AND (ECR-CSECODCO (IC) = SPACES)                       EQW9ZWFZ
02731            IF WSS-TYPTS (IG) = 'P'                                EQW9ZWFZ
02732               MOVE WSS-RANTS (IG)             TO RANG-TS-PERS     EQW9ZWFZ
02733               PERFORM READ-TS-PERSONNE THRU FREAD-TS-PERSONNE     EQW9ZWFZ
02734               IF ECR-MOTIFSCO (IC) NOT =                          EQW9ZWFZ
02735                              PERMTFC  OF DONNEES-PERSONNE (1)     EQW9ZWFZ
02736                  MOVE 'O'                   TO W-REAF             EQW9ZWFZ
02737                  PERFORM MAJ-MOTIF-TS       THRU FIN-MAJ-MOTIF-TS EQW9ZWFZ
02738                  MOVE ECR-MOTIFSCO (IC)     TO WSS-MOTIFC (IG)    EQW9ZWFZ
02739               END-IF                                              EQW9ZWFZ
02740            ELSE                                                   EQW9ZWFZ
02741               MOVE WSS-RANTS (IG)             TO RANG-TS-VEHI     EQW9ZWFZ
02742               PERFORM READ-TS-VEHICULE THRU FREAD-TS-VEHICULE     EQW9ZWFZ
02743               IF ECR-MOTIFSCO (IC) NOT =                          EQW9ZWFZ
02744                              VEHMTFC  OF TS-VEHICULE (1)          EQW9ZWFZ
02745                  MOVE 'O'                TO W-REAF                EQW9ZWFZ
02746                  PERFORM MAJ-MOTIF-TS       THRU FIN-MAJ-MOTIF-TS EQW9ZWFZ
02747                  MOVE ECR-MOTIFSCO (IC)     TO WSS-MOTIFC (IG)    EQW9ZWFZ
02748               END-IF                                              EQW9ZWFZ
02749            END-IF                                                 EQW9ZWFZ
02750         END-IF                                                    EQW9ZWFZ
02751      END-PERFORM.                                                 EQW9ZWFZ
02752 *                                                                 EQW9ZWFZ
02753      MOVE SPACES                    TO COM-MA-CODPOST.            EQW9ZWFZ
02754      MOVE SPACES                    TO COM-MA-COMMUNE.            EQW9ZWFZ
02755 *                                                                 EQW9ZWFZ
02756 *                                                                 EQW9ZWFZ
02757  FIN-MISE-A-JOUR-TS. EXIT.                                        EQW9ZWFZ
02758 *                                                                 EQW9ZWFZ
02759 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02760 * MISE ‡ JOURS DE LA TS LORS D'UNE RESTAURATION                 * EQW9ZWFZ
02761 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02762 *  MISE ‡ JOURS DE LA DATE DE SORTIE                              EQW9ZWFZ
02763 *  MISE ‡ JOURS DU MOTIF DE SORTIE                                EQW9ZWFZ
02764 *  MISE ‡ JOURS DU CODE ACTION DB2 (OCCURS 2)                     EQW9ZWFZ
02765  RESTAURATION-TS.                                                 EQW9ZWFZ
02766      IF WSS-TYPTS (IG) = 'P'                                      EQW9ZWFZ
02767         MOVE WSS-RANTS (IG)             TO RANG-TS-PERS           EQW9ZWFZ
02768         ADD 1          TO COM-FB-NBRE-PERS-ENC                    EQW9ZWFZ
02769         IF WSS-STATYP (IG) = 'CF'                                 EQW9ZWFZ
02770            ADD  1      TO COM-FB-NBRE-PERS-CF                     EQW9ZWFZ
02771         END-IF                                                    EQW9ZWFZ
02772         IF WSS-STATYP (IG) = 'CJ'                                 EQW9ZWFZ
02773            ADD  1      TO COM-FB-NBRE-PERS-CJ                     EQW9ZWFZ
02774         END-IF                                                    EQW9ZWFZ
02775         IF WSS-STATYP (IG) = 'PM'                                 EQW9ZWFZ
02776            ADD  1      TO COM-FB-NBRE-PERS-PM                     EQW9ZWFZ
02777         END-IF                                                    EQW9ZWFZ
02778         IF WSS-STATYP (IG) = 'ENSP' OR 'ENAP'                     EQW9ZWFZ
02779            ADD  1      TO COM-FB-NBRE-PERS-EN                     EQW9ZWFZ
02780         END-IF                                                    EQW9ZWFZ
02781         SUBTRACT  1    FROM COM-FB-NBRE-PERS-SOR                  EQW9ZWFZ
02782         PERFORM READ-TS-PERSONNE THRU FREAD-TS-PERSONNE           EQW9ZWFZ
02783         MOVE '99999999'                                           EQW9ZWFZ
02784                           TO RPERSORD OF DONNEES-PERSONNE (1)     EQW9ZWFZ
02785         MOVE ' '                                                  EQW9ZWFZ
02786                           TO PERMTFC  OF DONNEES-PERSONNE (1)     EQW9ZWFZ
02787         PERFORM ECR-TS-PERSONNE THRU FECR-TS-PERSONNE             EQW9ZWFZ
02788      ELSE                                                         EQW9ZWFZ
02789         MOVE WSS-RANTS (IG)             TO RANG-TS-VEHI           EQW9ZWFZ
02790         ADD 1          TO COM-FB-NBRE-VEHI-ENC                    EQW9ZWFZ
02791         IF WSS-STATYP (IG) = '4R' OR 'CC'                         EQW9ZWFZ
02792            ADD  1      TO COM-FB-NBRE-VEHI-TRACTEUR               EQW9ZWFZ
02793         END-IF                                                    EQW9ZWFZ
02794         IF WSS-STATYP (IG) = 'REM' OR 'CAR'                       EQW9ZWFZ
02795            ADD  1      TO COM-FB-NBRE-VEHI-TRACTE                 EQW9ZWFZ
02796         END-IF                                                    EQW9ZWFZ
02797         SUBTRACT  1    FROM COM-FB-NBRE-VEHI-SOR                  EQW9ZWFZ
02798         PERFORM READ-TS-VEHICULE THRU FREAD-TS-VEHICULE           EQW9ZWFZ
02799         MOVE '99999999'                                           EQW9ZWFZ
02800                           TO RVEHSORD OF TS-VEHICULE (1)          EQW9ZWFZ
02801         MOVE ' '                                                  EQW9ZWFZ
02802                           TO VEHMTFC  OF TS-VEHICULE (1)          EQW9ZWFZ
02803         PERFORM VARYING IND-GTI FROM 1 BY 1 UNTIL IND-GTI > 30    EQW9ZWFZ
                 IF GTIFLGC OF TS-VEHICULE (1, IND-GTI) =       'O'
02799               MOVE '99999999'                                     EQW9ZWFZ
02800                     TO RGTISORD OF TS-VEHICULE (1, IND-GTI)       EQW9ZWFZ
                 END-IF
              END-PERFORM
      *-----------
      * TRAITEMENT DU CAS OU L'ON RESTAURE UN VÈHICULE QUE L'ON VIENT DEFB01T00B
      * REPLACER :                                                      FB01T00B
      *  - ON REMET EN COURS LE VÈHICULE REMPLACÈ AVEC SES DONNÈES      FB01T00B
      *  - LE VÈHICULE REMPLAÁANT DEVIENT UN AJOUT DE VÈHICULE CLASSIQUEFB01T00B
      *    POUR LEQUEL ON NE DOIT PLUS TENIR COMPTE DES DONNÈES QU'IL   FB01T00B
      *    AVAIT RÈCUPERÈ DU VEHICULE REMPLACÈ, ON DOIT RESAISIR        FB01T00B
      *    CERTAINES DONNÈES DU VÈHICULE AJOUTÈ.                        FB01T00B
      *-----------
              IF VEHCHGC OF TS-VEHICULE (1) = '1'
                IF VEHORCX OF TS-VEHICULE(1) = ZERO
                   MOVE SPACES TO VEHCHGC OF TS-VEHICULE (1)
                ELSE
                   MOVE '2' TO VEHCHGC OF TS-VEHICULE (1)
                END-IF
02803           PERFORM ECR-TS-VEHICULE THRU FECR-TS-VEHICULE           EQW9ZWFZ
                MOVE ZERO   TO WSS-RANG-TS-CHANGER
                MOVE 'N'    TO WSS-FIN-VEHI-CHG
                PERFORM READ-TS-VEHI-CHANGER
                                          THRU FREAD-TS-VEHI-CHANGER
                   UNTIL VEHORDX OF TS-VEHICULE(1) = VEHORCX
                                          OF TS-VEHICULE-CHANGER(1)
                   OR WSS-FIN-VEHI-CHG = 'O'
      *----ICI??
      *----ICI?? VOIR SI ON DOIT VERIFIER DATE SORTI '99999999'
      *----ICI??
                IF WSS-FIN-VEHI-CHG = 'N'
                   MOVE SPACES TO VEHCHGC  OF TS-VEHICULE-CHANGER (1)
                   MOVE ZERO   TO VEHORCX  OF TS-VEHICULE-CHANGER (1)
                   MOVE SPACES TO VEHUSAC  OF TS-VEHICULE-CHANGER (1)
                   MOVE SPACES TO VEHPROC  OF TS-VEHICULE-CHANGER (1)
                   MOVE SPACES TO VEHUSFC  OF TS-VEHICULE-CHANGER (1)
                   MOVE SPACES TO VEHPRFC  OF TS-VEHICULE-CHANGER (1)
                   MOVE SPACES TO VEHFOUC  OF TS-VEHICULE-CHANGER (1)
                   MOVE SPACES TO VEHCRMC  OF TS-VEHICULE-CHANGER (1)
                   MOVE SPACES TO VEHCRFC  OF TS-VEHICULE-CHANGER (1)
                   MOVE SPACES TO ANTECEDENT-VEHICULE
                                           OF TS-VEHICULE-CHANGER (1)
                   MOVE SPACES TO ANTECEDENT-VEHICULE
                                           OF TS-VEHICULE-CHANGER (2)
                   MOVE SPACES TO COEFFICIENT-VEH
                                           OF TS-VEHICULE-CHANGER (1)
02803              PERFORM ECR-TS-VEHI-CHANGER THRU FECR-TS-VEHI-CHANGEREQW9ZWFZ
02804           END-IF                                                  EQW9ZWFZ
              ELSE
02803           PERFORM ECR-TS-VEHICULE THRU FECR-TS-VEHICULE           EQW9ZWFZ
02804         END-IF                                                    EQW9ZWFZ
02804      END-IF.                                                      EQW9ZWFZ
02805  FIN-RESTAURATION-TS.                                             EQW9ZWFZ
02911 *                                                                 FB01T00B
02912 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * FB01T00B
02913 * MISE ‡ JOURS DES TS LORS D'UN CHANGEMENT DE VEHICULE          * FB01T00B
02914 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * FB01T00B
02915 * LE CHANGEMENT SE FAIT EN 2 TEMPS :                              FB01T00B
02916 *  1/ MISE ‡ JOUR DE LA TS DU VÈHICULE REMPLACÈ                   FB01T00B
02917 *     - MISE ‡ JOURS DE LA DATE DE SORTIE                         FB01T00B
02918 *     - MISE ‡ JOURS DU MOTIF DE SORTIE                           FB01T00B
02919 *     - MISE ‡ JOURS DU CODE ACTION DB2 (OCCURS 2)                FB01T00B
02920 *     - MISE ‡ JOURS DU TOP DE CHANGEMENT VEHICULE                FB01T00B
02921 *  2/ CREATION DE LA TS DU VÈHICULE REMPLACANT                    FB01T00B
02922 *     - MISE ‡ JOURS DE LA DATE DE SORTIE                         FB01T00B
02923 *     - MISE ‡ JOURS DU MOTIF DE SORTIE                           FB01T00B
02924 *     - MISE ‡ JOURS DU CODE ACTION DB2 (OCCURS 2)                FB01T00B
02925 *     - MISE ‡ JOURS DU TOP DE CHANGEMENT VEHICULE                FB01T00B
02926                                                                   FB01T00B
02927  CHANGEMENT-VEHI.                                                 FB01T00B
02928 *  1/ MISE ‡ JOUR DE LA TS DU VÈHICULE REMPLACÈ                   FB01T00B
02929      MOVE WSS-RANTS (IG)             TO RANG-TS-VEHI.             FB01T00B
02930      ADD 1          TO COM-FB-NBRE-VEHI-SOR.                      FB01T00B
02931      SUBTRACT  1    FROM COM-FB-NBRE-VEHI-ENC.                    FB01T00B
02932      IF WSS-STATYP (IG) = '4R' OR 'CC'                            FB01T00B
02933         SUBTRACT  1    FROM COM-FB-NBRE-VEHI-TRACTEUR             FB01T00B
02934      END-IF.                                                      FB01T00B
02938      PERFORM READ-TS-VEHICULE THRU FREAD-TS-VEHICULE.             FB01T00B
02939      MOVE COM-FB-DATE-EFFET-1 TO RVEHSORD OF TS-VEHICULE      (1).FB01T00B
02940      MOVE 'R'                 TO VEHMTFC  OF TS-VEHICULE      (1).FB01T00B
02941      MOVE '1'                 TO VEHCHGC  OF TS-VEHICULE      (1).FB01T00B
02942      IF VEHACTC OF TS-VEHICULE (1) NOT = 'I'                      FB01T00B
02943         MOVE 'U'              TO VEHACTC  OF TS-VEHICULE (1)      FB01T00B
02944      END-IF.                                                      FB01T00B
02945      PERFORM ECR-TS-VEHICULE THRU FECR-TS-VEHICULE.               FB01T00B
02946                                                                   FB01T00B
02947 *  2/ CREATION DE LA TS DU VÈHICULE REMPLACANT                    FB01T00B
02948      MOVE SPACES TO DONNEES-VEHICULE OF TS-VEHICULE(2).           FB01T00B
02949      MOVE SPACES TO DONNEES-VEHICULE OF TS-VEHICULE(3).           FB01T00B
02950      MOVE SPACES TO DONNEES-VEHICULE OF TS-VEHICULE(4).           FB01T00B
02951      ADD 1 TO COM-FB-RANG-MAX-TSVEHI.                             FB01T00B
02952      ADD 1 TO COM-FB-NBRE-VEHI-ENC.                               FB01T00B
02953      ADD 1 TO COM-FB-ORDN-MAX-VEHI.                               FB01T00B
02954      MOVE COM-FB-ORDN-MAX-VEHI TO VEHORDX OF TS-VEHICULE(2).      FB01T00B
02955      IF INF-EFFET OF TS-SUSPENS1 NOT = SPACES AND LOW-VALUE       FB01T00B
02956         MOVE INF-EFFET OF TS-SUSPENS1 TO                          FB01T00B
02957                                      RVEHENTD OF TS-VEHICULE(1)   FB01T00B
02958         ELSE                                                      FB01T00B
02959            MOVE INF-EFFET OF TS-SUSPENS2 TO                       FB01T00B
02960                                      RVEHENTD OF TS-VEHICULE(1)   FB01T00B
02961      END-IF.                                                      FB01T00B
02962      MOVE '99999999' TO VEHSORD  OF TS-VEHICULE(1).               FB01T00B
02999      MOVE VEHORDX OF TS-VEHICULE(1) TO VEHORCX OF TS-VEHICULE(1). FB01T00B
02963      MOVE SPACES     TO RVEHORDX OF TS-VEHICULE(1).               FB01T00B
02969      IF VEHTYPC OF TS-VEHICULE(1) NOT = 'CC '                     FB01T00B
02964         MOVE SPACES  TO VEHCODC  OF TS-VEHICULE(1)                FB01T00B
02961      END-IF.                                                      FB01T00B
02965      MOVE SPACES     TO VEHMARL  OF TS-VEHICULE(1).               FB01T00B
02966      MOVE SPACES     TO VEHMODL  OF TS-VEHICULE(1).               FB01T00B
02967 *ON CONSERVE LA CYLINDRÈE POUR LES 2ROUES CAR ON NE PEUT REMPLACERFB01T00B
02968 *UN 2R QUE PAR UN 2R DE MÍME CATÈGORIE (50 CM3 PAR 50 CM3 ETC...) FB01T00B
02969      IF VEHTYPC OF TS-VEHICULE(1) NOT = '2R '                     FB01T00B
02973        MOVE SPACES     TO RVEHCYLN OF TS-VEHICULE(1)              FB01T00B
02974        MOVE SPACES     TO  VEHGENC OF TS-VEHICULE(1)              FB01T00B
02975      END-IF.                                                      FB01T00B
02976      MOVE SPACES     TO RVEHCIRD OF TS-VEHICULE(1).               FB01T00B
02977      MOVE SPACES     TO VEHIMMX  OF TS-VEHICULE(1).               FB01T00B
02978      MOVE SPACES     TO VEHAIMX  OF TS-VEHICULE(1).               FB01T00B
02979      MOVE SPACES     TO VEHFIMX  OF TS-VEHICULE(1).               FB01T00B
02980      MOVE SPACES     TO RVEHVALM OF TS-VEHICULE(1).               FB01T00B
02981      MOVE SPACES     TO VEHPOSC  OF TS-VEHICULE(1).               FB01T00B
02982      MOVE SPACES     TO VEHPRTC  OF TS-VEHICULE(1).               FB01T00B
02983      MOVE SPACES     TO VEHFORC  OF TS-VEHICULE(1) (2:1).         FB01T00B
02984      MOVE SPACES     TO RVEHPOIN OF TS-VEHICULE(1).               FB01T00B
02985      MOVE SPACES     TO VEHGROC  OF TS-VEHICULE(1).               FB01T00B
02986      MOVE SPACES     TO VEHGRFC  OF TS-VEHICULE(1).               FB01T00B
02987      MOVE SPACES     TO VEHCLAC  OF TS-VEHICULE(1).               FB01T00B
02988      MOVE SPACES     TO VEHCLFC  OF TS-VEHICULE(1).               FB01T00B
02989      MOVE SPACES     TO VEHMTFC  OF TS-VEHICULE(1).               FB01T00B
02990      MOVE SPACES     TO VEHMANC  OF TS-VEHICULE(1).               FB01T00B
02991      MOVE SPACES     TO VEHTARX  OF TS-VEHICULE(1).               FB01T00B
02992      MOVE SPACES     TO VEHCGAC  OF TS-VEHICULE(1).               FB01T00B
02993      MOVE SPACES     TO VEHPEFC  OF TS-VEHICULE(1).               FB01T00B
02994      MOVE SPACES     TO RVEHNAVN OF TS-VEHICULE(1).               FB01T00B
02995      MOVE 'I'        TO VEHACTC  OF TS-VEHICULE(1).               FB01T00B
02996      MOVE SPACES     TO VEHRSTC  OF TS-VEHICULE(1).               FB01T00B
02997      MOVE '2'        TO VEHCHGC  OF TS-VEHICULE(1).               FB01T00B
02998      MOVE SPACES     TO GARCODC  OF TS-VEHICULE(1).               FB01T00B
03000      MOVE SPACES     TO GARANTIE-VEH OF TS-VEHICULE(1).           FB01T00B
F36835     MOVE SPACES     TO VEHCGNC  OF TS-VEHICULE(1).               FB01T00B
F36835     MOVE SPACES     TO VEHCGPC  OF TS-VEHICULE(1).               FB01T00B
F36835     MOVE SPACES     TO VEHCGSC  OF TS-VEHICULE(1).               FB01T00B
U9949      MOVE SPACES     TO VEHNRJC  OF TS-VEHICULE(1).               FB01T00B

03009      IF VEHTYPC OF TS-VEHICULE(1) = '4R ' OR 'CC '                FB01T00B
03010         ADD 1 TO COM-FB-NBRE-VEHI-TRACTEUR                        FB01T00B
03011      END-IF.                                                      FB01T00B
03012      MOVE COM-FB-RANG-MAX-TSVEHI TO COM-FB-RANG-TS-LIRE.          FB01T00B
03013      MOVE COM-FB-RANG-MAX-TSVEHI TO RANG-TS-VEHI.                 FB01T00B
03014      PERFORM CREATION-TS-VEHICULE THRU FCREATION-TS-VEHICULE.     FB01T00B
03015  FIN-CHANGEMENT-VEHI.                                             FB01T00B
02806 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02807 * MISE A JOURS DE LA TS LORS D'UNE SORTIE                       * EQW9ZWFZ
02808 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02809 *  MISE ‡ JOURS DE LA DATE DE SORTIE                              EQW9ZWFZ
02810 *  MISE ‡ JOURS DU MOTIF DE SORTIE                                EQW9ZWFZ
02811 *  MISE ‡ JOURS DU CODE ACTION DB2 (OCCURS 2)                     EQW9ZWFZ
02812  SORTIE-TS.                                                       EQW9ZWFZ
02813      IF WSS-TYPTS (IG) = 'P'                                      EQW9ZWFZ
02814         MOVE WSS-RANTS (IG)             TO RANG-TS-PERS           EQW9ZWFZ
02815         ADD 1          TO COM-FB-NBRE-PERS-SOR                    EQW9ZWFZ
02816         SUBTRACT  1    FROM COM-FB-NBRE-PERS-ENC                  EQW9ZWFZ
02817         IF WSS-STATYP (IG) = 'CF'                                 EQW9ZWFZ
02818            SUBTRACT  1    FROM COM-FB-NBRE-PERS-CF                EQW9ZWFZ
02819         END-IF                                                    EQW9ZWFZ
02820         IF WSS-STATYP (IG) = 'CJ'                                 EQW9ZWFZ
02821            SUBTRACT  1    FROM COM-FB-NBRE-PERS-CJ                EQW9ZWFZ
02822         END-IF                                                    EQW9ZWFZ
02823         IF WSS-STATYP (IG) = 'PM'                                 EQW9ZWFZ
02824            SUBTRACT  1    FROM COM-FB-NBRE-PERS-PM                EQW9ZWFZ
02825         END-IF                                                    EQW9ZWFZ
02826         IF WSS-STATYP (IG) = 'ENSP' OR 'ENAP'                     EQW9ZWFZ
02827            SUBTRACT  1    FROM COM-FB-NBRE-PERS-EN                EQW9ZWFZ
02828         END-IF                                                    EQW9ZWFZ
02829         PERFORM READ-TS-PERSONNE THRU FREAD-TS-PERSONNE           EQW9ZWFZ
02830         MOVE COM-FB-DATE-EFFET-1        TO                        EQW9ZWFZ
02831                              RPERSORD OF DONNEES-PERSONNE (1)     EQW9ZWFZ
02832         MOVE ECR-MOTIFSCO (IC)                                    EQW9ZWFZ
02833                           TO PERMTFC  OF DONNEES-PERSONNE (1)     EQW9ZWFZ
02834         IF PERACTC OF DONNEES-PERSONNE (1) NOT = 'I'              EQW9ZWFZ
02835            MOVE 'U'                                               EQW9ZWFZ
02836                           TO PERACTC  OF DONNEES-PERSONNE (1)     EQW9ZWFZ
02837         END-IF                                                    EQW9ZWFZ
02838         PERFORM ECR-TS-PERSONNE THRU FECR-TS-PERSONNE             EQW9ZWFZ
02839      ELSE                                                         EQW9ZWFZ
02840         MOVE WSS-RANTS (IG)             TO RANG-TS-VEHI           EQW9ZWFZ
02841         ADD 1          TO COM-FB-NBRE-VEHI-SOR                    EQW9ZWFZ
02842         SUBTRACT  1    FROM COM-FB-NBRE-VEHI-ENC                  EQW9ZWFZ
02843         IF WSS-STATYP (IG) = '4R' OR 'CC'                         EQW9ZWFZ
02844            SUBTRACT  1    FROM COM-FB-NBRE-VEHI-TRACTEUR          EQW9ZWFZ
02845         END-IF                                                    EQW9ZWFZ
02846         IF WSS-STATYP (IG) = 'REM' OR 'CAR'                       EQW9ZWFZ
02847            SUBTRACT  1    FROM COM-FB-NBRE-VEHI-TRACTE            EQW9ZWFZ
02848         END-IF                                                    EQW9ZWFZ
02849         PERFORM READ-TS-VEHICULE THRU FREAD-TS-VEHICULE           EQW9ZWFZ
02850         MOVE COM-FB-DATE-EFFET-1        TO                        EQW9ZWFZ
02851                              RVEHSORD OF TS-VEHICULE (1)          EQW9ZWFZ
02852         MOVE ECR-MOTIFSCO (IC)                                    EQW9ZWFZ
02853                           TO VEHMTFC  OF TS-VEHICULE (1)          EQW9ZWFZ
02854         IF VEHACTC OF TS-VEHICULE (1) NOT = 'I'                   EQW9ZWFZ
02855            MOVE 'U'                                               EQW9ZWFZ
02856                           TO VEHACTC  OF TS-VEHICULE (1)          EQW9ZWFZ
02857         END-IF                                                    EQW9ZWFZ
02858         PERFORM ECR-TS-VEHICULE THRU FECR-TS-VEHICULE             EQW9ZWFZ
02859      END-IF.                                                      EQW9ZWFZ
02860  FIN-SORTIE-TS.                                                   EQW9ZWFZ
02861 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02862 * MISE A JOURS DE LA TS LORS D'UNE SORTIE                       * EQW9ZWFZ
02863 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02864 *                                                                 EQW9ZWFZ
02865 *  MISE ‡ JOURS DU MOTIF DE SORTIE                                EQW9ZWFZ
02866 *                                                                 EQW9ZWFZ
02867  MAJ-MOTIF-TS.                                                    EQW9ZWFZ
02868 *                                                                 EQW9ZWFZ
02869      IF WSS-TYPTS (IG) = 'P'                                      EQW9ZWFZ
02870         MOVE ECR-MOTIFSCO (IC)                                    EQW9ZWFZ
02871                           TO PERMTFC  OF DONNEES-PERSONNE (1)     EQW9ZWFZ
02872         PERFORM ECR-TS-PERSONNE THRU FECR-TS-PERSONNE             EQW9ZWFZ
02873      ELSE                                                         EQW9ZWFZ
02874         MOVE ECR-MOTIFSCO (IC)                                    EQW9ZWFZ
02875                           TO VEHMTFC  OF TS-VEHICULE (1)          EQW9ZWFZ
02876         PERFORM ECR-TS-VEHICULE THRU FECR-TS-VEHICULE             EQW9ZWFZ
02877      END-IF.                                                      EQW9ZWFZ
02878 *                                                                 EQW9ZWFZ
02879  FIN-MAJ-MOTIF-TS.                                                EQW9ZWFZ
02880 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02881 * APPEL AU MODULE CONTROLES TECHNIQUES                          * EQW9ZWFZ
02882 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02883 *                                                                 EQW9ZWFZ
02884  APPEL-MA90T00.                                                   EQW9ZWFZ
02885 *--------------                                                   EQW9ZWFZ
02886                                                                   EQW9ZWFZ
02887      MOVE SPACES           TO MAI90C00.                           EQW9ZWFZ
02888      MOVE NOM-TACHE        TO COM90C00-TACHE.                     EQW9ZWFZ
02889      MOVE IDENT-TS-APP     TO COM90C00-IDENT-TS.                  EQW9ZWFZ
02890      MOVE +1               TO COM90C00-ITEM-TS.                   EQW9ZWFZ
02891      MOVE SPACES           TO COM90C00-ACTION.                    EQW9ZWFZ
02892      MOVE COM-GENE-CODCIE  TO COM90C00-CIE.                       EQW9ZWFZ
02893      MOVE COM-GENE-TYPCICS TO COM90C00-TYPCICS.                   EQW9ZWFZ
02894      MOVE COM-MA           TO COM90C00-MAICOMM.                   EQW9ZWFZ
02895                                                                   EQW9ZWFZ
02896      EXEC CICS LINK PROGRAM  ('MA90T00')                          EQW9ZWFZ
02897                     COMMAREA (MAI90C00)                           EQW9ZWFZ
02898                     LENGTH   (LENGTH OF MAI90C00)                 EQW9ZWFZ
02899      END-EXEC.                                                    EQW9ZWFZ
02900                                                                   EQW9ZWFZ
02901      IF EIBRCODE = LOW-VALUE                                      EQW9ZWFZ
02902         MOVE COM90C00-MAICOMM TO COM-MA                           EQW9ZWFZ
02903      ELSE                                                         EQW9ZWFZ
02904         MOVE 'FB01 : ERREUR LINK MA90T00' TO MESS                 EQW9ZWFZ
02905         GO TO ABANDON-TACHE                                       EQW9ZWFZ
02906      END-IF.                                                      EQW9ZWFZ
02907 *                                                                 EQW9ZWFZ
02908  FAPPEL-MA90T00.                                                  EQW9ZWFZ
02909      EXIT.                                                        EQW9ZWFZ
02910 *                                                                 EQW9ZWFZ
02911 ***************************************************************   EQW9ZWFZ
02912 *          APPEL DU MODULE DE CONTROLE D'HABILITATION         *   EQW9ZWFZ
02913 ***************************************************************   EQW9ZWFZ
02914  APPEL-MA90T20.                                                   EQW9ZWFZ
02915 *--------------                                                   EQW9ZWFZ
02916      MOVE SPACES           TO MAI90C20.                           EQW9ZWFZ
02917      MOVE NOM-TACHE        TO MA90C20-TRANSAC.                    EQW9ZWFZ
02918      MOVE IDENT-TS-APP     TO MA90C20-IDENT-TS.                   EQW9ZWFZ
02919      MOVE IDENT-TS-CONF    TO MA90C20-IDENT-TS-CONF.              EQW9ZWFZ
02920      MOVE COM-GENE-LNGCNV  TO MA90C20-LG-TS-CONF.                 EQW9ZWFZ
02921      MOVE NOM-TACHE        TO MA90C20-TRANSAC.                    EQW9ZWFZ
02922      MOVE SPACES           TO MA90C20-ACTION.                     EQW9ZWFZ
02923      MOVE 'N'              TO MA90C20-MAJ-TS-APP.                 EQW9ZWFZ
02924      MOVE Z-COMMAREA-USER  TO MA90C20-MAICOMM.                    EQW9ZWFZ
02925      MOVE COM-GENE-TYPCICS TO MA90C20-TYPCICS.                    EQW9ZWFZ
02926      MOVE COM-GENE-CODCIE  TO MA90C20-CODCIE.                     EQW9ZWFZ
02927      MOVE '1'              TO MA90C20-SWAP.                       EQW9ZWFZ
02928      MOVE 'O'              TO MA90C20-RAB.                        EQW9ZWFZ
02929                                                                   EQW9ZWFZ
02930      EXEC CICS LINK PROGRAM  ('MA90T20')                          EQW9ZWFZ
02931                     COMMAREA (MAI90C20)                           EQW9ZWFZ
02932                     LENGTH   (LENGTH OF MAI90C20)                 EQW9ZWFZ
02933                     NOHANDLE                                      EQW9ZWFZ
02934      END-EXEC.                                                    EQW9ZWFZ
02935                                                                   EQW9ZWFZ
02936      IF EIBRCODE = LOW-VALUE                                      EQW9ZWFZ
02937         MOVE MA90C20-MAICOMM TO COM-MA                            EQW9ZWFZ
02938      ELSE                                                         EQW9ZWFZ
02939         MOVE 'FV01 : ERREUR LINK MA90T20' TO MESS                 EQW9ZWFZ
02940         GO TO ABANDON-TACHE                                       EQW9ZWFZ
02941      END-IF.                                                      EQW9ZWFZ
02942 *                                                                 EQW9ZWFZ
02943  FAPPEL-MA90T20.   EXIT.                                          EQW9ZWFZ
02944 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02945 * GESTION DE LA COMMAREA    * FB01 * TRAITEMENT NORMAL            EQW9ZWFZ
02946 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02947 *                                                                 EQW9ZWFZ
02948  TRAITEMENT-COMMAREA.                                             EQW9ZWFZ
02949 *                                                                 EQW9ZWFZ
02950 *    MOVE ZONE-MAP          TO ZONE-COMMAREA.                     EQW9ZWFZ
02951 *                                                                 EQW9ZWFZ
02952  FIN-TRAITEMENT-COMMAREA.  EXIT.                                  EQW9ZWFZ
02953 *                                                                 EQW9ZWFZ
02954 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02955 * DETERMINATION ECRAN SUIVANT * FB01 * TRAITEMENT NORMAL          EQW9ZWFZ
02956 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
02957  DETERMINATION-ECR-SUIV.                                          EQW9ZWFZ
02958 * -------------------- *                                          EQW9ZWFZ
02959 *  RECHERCHE DE L'ECRAN DANS LA TABLE DES CONVERSATIONS           EQW9ZWFZ
02960 *                                                                 EQW9ZWFZ
U3319  ++INCLUDE SQKCECRS
02982 *                                                                 EQW9ZWFZ
02983 *  ----------------------------------------------------------- *  EQW9ZWFZ
02984 *  GESTION DES PASSAGES AUX ECRANS OPTIONNELS SUIVANTS :          EQW9ZWFZ
02985 *  ----------------------------------------------------------- *  EQW9ZWFZ
02986 *                                                                 EQW9ZWFZ
02987 *?????                                                            EQW9ZWFZ
02988 *ICI TOP DE PRESENCE PM ETC......                                 EQW9ZWFZ
02989 *                                                                 EQW9ZWFZ
02990      IF ECR-AJOUTCONO = 'O'                                       EQW9ZWFZ
02991         MOVE 'FB02'                  TO  NOM-TACHE-XCTL           EQW9ZWFZ
02992         MOVE CODE-TRAITEMENT-NORMAL  TO  Z-FONCTION               EQW9ZWFZ
02993         MOVE  SPACES                 TO  COM-GENE-MESINF          EQW9ZWFZ
02994      END-IF.                                                      EQW9ZWFZ
02995 *                                                                 EQW9ZWFZ
02996      IF ECR-AJOUTVEHO = '1' OR '2' OR '3'                         EQW9ZWFZ
02997         MOVE 'FB04'                  TO  NOM-TACHE-XCTL           EQW9ZWFZ
02998         MOVE CODE-TRAITEMENT-NORMAL  TO  Z-FONCTION               EQW9ZWFZ
02999         MOVE  SPACES                 TO  COM-GENE-MESINF          EQW9ZWFZ
03000      END-IF.                                                      EQW9ZWFZ
03001 *                                                                 EQW9ZWFZ
03002      IF ECR-AJOUTVEHO = '4' OR '5'                                EQW9ZWFZ
03003         MOVE 'FB07'                  TO  NOM-TACHE-XCTL           EQW9ZWFZ
03004         MOVE CODE-TRAITEMENT-NORMAL  TO  Z-FONCTION               EQW9ZWFZ
03005         MOVE  SPACES                 TO  COM-GENE-MESINF          EQW9ZWFZ
03006      END-IF.                                                      EQW9ZWFZ
03007 *                                                                 EQW9ZWFZ
03008      PERFORM VARYING IC FROM 1 BY 1 UNTIL IC > 10                 EQW9ZWFZ
03009         OR (ECR-DATENTDO(IC) = SPACES OR LOW-VALUE)               EQW9ZWFZ
03010         IF ECR-CSECODCO (IC) = 'F'                                EQW9ZWFZ
03011            MOVE 'FB11'                  TO  NOM-TACHE-XCTL        EQW9ZWFZ
03012            MOVE CODE-TRAITEMENT-NORMAL  TO  Z-FONCTION            EQW9ZWFZ
03013         END-IF                                                    EQW9ZWFZ
03014         IF ECR-CSECODCO (IC) = 'M'                                EQW9ZWFZ
03015            EVALUATE COM-FB-TYPE-TS-LIRE                           EQW9ZWFZ
03016               WHEN 'P'                                            EQW9ZWFZ
03017                  IF COM-MA-CODCNV NOT = 'FB0001RE' AND 'FB0002RE' EQW9ZWFZ
03018                     MOVE 'FB02'              TO  NOM-TACHE-XCTL   EQW9ZWFZ
03019                  END-IF                                           EQW9ZWFZ
03020               WHEN 'V'                                            EQW9ZWFZ
03021                  IF COM-MA-CODCNV (7:2) = 'RE'                    EQW9ZWFZ
03022                     IF COM-MA-CODCNV (1:6) = 'FB0001'             EQW9ZWFZ
03023                        MOVE 'FB15'            TO NOM-TACHE-XCTL   EQW9ZWFZ
03024                     ELSE                                          EQW9ZWFZ
03025                        MOVE 'FB60'            TO NOM-TACHE-XCTL   EQW9ZWFZ
03026                     END-IF                                        EQW9ZWFZ
03027                  ELSE                                             EQW9ZWFZ
03028                     IF  ECR-STATYPCO (IC) = 'CAR' OR 'REM'        EQW9ZWFZ
03029                        MOVE 'FB07'              TO  NOM-TACHE-XCTLEQW9ZWFZ
03030                     ELSE                                          EQW9ZWFZ
03031                        MOVE 'FB04'              TO  NOM-TACHE-XCTLEQW9ZWFZ
03032                     END-IF                                        EQW9ZWFZ
03033                  END-IF                                           EQW9ZWFZ
03034            END-EVALUATE                                           EQW9ZWFZ
03035            MOVE CODE-TRAITEMENT-NORMAL  TO  Z-FONCTION            EQW9ZWFZ
03036         END-IF                                                    EQW9ZWFZ
03247         IF ECR-CSECODCO (IC) = 'C'                                FB01T00B
03248            MOVE 'FB04'                  TO  NOM-TACHE-XCTL        FB01T00B
03249            MOVE CODE-TRAITEMENT-NORMAL  TO  Z-FONCTION            FB01T00B
03250         END-IF                                                    FB01T00B
03037      END-PERFORM.                                                 EQW9ZWFZ
03038 *                                                                 EQW9ZWFZ
03039 * DEBRANCHEMENT VERS ECRAN AIDE CODE MOTIF                        EQW9ZWFZ
03040 * (TEXTE SOUS SPI)                                                EQW9ZWFZ
03041      IF WSS-APPEL-AIDE-MOTIF = 'O'                                EQW9ZWFZ
03042         MOVE 'MA84'  TO  NOM-TACHE-XCTL                           EQW9ZWFZ
03043         MOVE  SPACES                 TO  COM-GENE-MESINF          EQW9ZWFZ
03044                                          COM-GENE-MESANO          EQW9ZWFZ
03045                                          COM-CODERR               EQW9ZWFZ
03046         MOVE CODE-TRAITEMENT-NORMAL  TO  Z-FONCTION               EQW9ZWFZ
03047         MOVE 'O'                     TO COM-GENE-ECROPT           EQW9ZWFZ
03048         SUBTRACT  1                  FROM   IA                    EQW9ZWFZ
03049         MOVE IA                      TO   COM-MA-STD-CLICHE       EQW9ZWFZ
03050         MOVE 'N'                     TO   COM-GENE-REAF           EQW9ZWFZ
03051                                                                   EQW9ZWFZ
03052         MOVE 'MOTI'               TO   COM-MA-GENRE-TXT           EQW9ZWFZ
03053         MOVE COM-ITEM-AFFICH     TO COM-FB-PAGE-ENCOUR            EQW9ZWFZ
03054         GO TO FIN-DETERMINATION-ECR-SUIV                          EQW9ZWFZ
03055      END-IF.                                                      EQW9ZWFZ
03056 *                                                                 EQW9ZWFZ
03057 * DEBRANCHEMENT VERS ECRAN AIDE CODE AJOUT VEHICULE               EQW9ZWFZ
03058 * (TEXTE SOUS SPI)                                                EQW9ZWFZ
03059      IF WSS-APPEL-AIDE-AJTVEHI = 'O'                              EQW9ZWFZ
03060         MOVE 'MA84'  TO  NOM-TACHE-XCTL                           EQW9ZWFZ
03061         MOVE  SPACES                 TO  COM-GENE-MESINF          EQW9ZWFZ
03062                                          COM-GENE-MESANO          EQW9ZWFZ
03063                                          COM-CODERR               EQW9ZWFZ
03064         MOVE CODE-TRAITEMENT-NORMAL  TO  Z-FONCTION               EQW9ZWFZ
03065         MOVE 'O'                     TO COM-GENE-ECROPT           EQW9ZWFZ
03066         SUBTRACT  1                  FROM   IA                    EQW9ZWFZ
03067         MOVE IA                      TO   COM-MA-STD-CLICHE       EQW9ZWFZ
03068         MOVE 'N'                     TO   COM-GENE-REAF           EQW9ZWFZ
03069                                                                   EQW9ZWFZ
03070         MOVE 'ACTI'               TO   COM-MA-GENRE-TXT           EQW9ZWFZ
03071         MOVE COM-ITEM-AFFICH      TO COM-FB-PAGE-ENCOUR           EQW9ZWFZ
03072         GO TO FIN-DETERMINATION-ECR-SUIV                          EQW9ZWFZ
03073      END-IF.                                                      EQW9ZWFZ
03074      MOVE ZERO                   TO COM-FB-PAGE-ENCOUR.           EQW9ZWFZ
03075 *                                                                 EQW9ZWFZ
03076  FIN-DETERMINATION-ECR-SUIV.                                      EQW9ZWFZ
03077      EXIT.                                                        EQW9ZWFZ
03078 /                                                                 EQW9ZWFZ
03079 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
03080 * GESTION DE LA MAP         * FB01 * TRAITEMENT NORMAL            EQW9ZWFZ
03081 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
03082 *                                                                 EQW9ZWFZ
03083  TRAITEMENT-MAP.                                                  EQW9ZWFZ
03084 * ------------ *                                                  EQW9ZWFZ
03085 *                                                                 EQW9ZWFZ
03086  FIN-TRAITEMENT-MAP. EXIT.                                        EQW9ZWFZ
03087 /                                                                 EQW9ZWFZ
03088 ***************************************************************** EQW9ZWFZ
03089 ***************************************************************** EQW9ZWFZ
03090 ***********************  MODULE SORTIE  ************************* EQW9ZWFZ
03091 ***************************************************************** EQW9ZWFZ
03092 ***************************************************************** EQW9ZWFZ
03093 *                                                                 EQW9ZWFZ
03094 ***************************************************************** EQW9ZWFZ
03095 * MODULE DE SORTIE GENERALISE                                   * EQW9ZWFZ
03096 ***************************************************************** EQW9ZWFZ
03097 *                                                                 EQW9ZWFZ
03098  MODULE-SORTIE.                                                   EQW9ZWFZ
03099 *-------------*                                                   EQW9ZWFZ
03100      IF  TRAITEMENT-AUTOMATIQUE                                   EQW9ZWFZ
03101          MOVE     SPACES  TO  COM-GENE-NEWMEN                     EQW9ZWFZ
03102          PERFORM  SORTIE-AFFICHAGE-FORMAT THRU                    EQW9ZWFZ
03103                   FIN-SORTIE-AFFICHAGE-FORMAT                     EQW9ZWFZ
03104      END-IF.                                                      EQW9ZWFZ
03105      IF  NOT OK                                                   EQW9ZWFZ
03106          PERFORM  SORTIE-ERREUR THRU                              EQW9ZWFZ
03107                   FIN-SORTIE-ERREUR                               EQW9ZWFZ
03108      END-IF.                                                      EQW9ZWFZ
03109 *                                                                 EQW9ZWFZ
03110      IF  TRAITEMENT-NORMAL                                        EQW9ZWFZ
03111          PERFORM  SORTIE-SUITE THRU                               EQW9ZWFZ
03112                   FIN-SORTIE-SUITE                                EQW9ZWFZ
03113      END-IF.                                                      EQW9ZWFZ
03114 *                                                                 EQW9ZWFZ
03115      IF  LEVEL-SUP                                                EQW9ZWFZ
03116          PERFORM  SORTIE-LEVEL-SUPERIEUR THRU                     EQW9ZWFZ
03117                   FIN-SORTIE-LEVEL-SUPERIEUR                      EQW9ZWFZ
03118      END-IF.                                                      EQW9ZWFZ
03119 *                                                                 EQW9ZWFZ
03120      IF  LEVEL-SIGN                                               EQW9ZWFZ
03121          PERFORM  SORTIE-LEVEL-SIGNATURE THRU                     EQW9ZWFZ
03122                   FIN-SORTIE-LEVEL-SIGNATURE                      EQW9ZWFZ
03123      END-IF.                                                      EQW9ZWFZ
03124 *                                                                 EQW9ZWFZ
03125      IF  LEVEL-MAX OR JUMP                                        EQW9ZWFZ
03126          PERFORM  SORTIE-LEVEL-MAX THRU                           EQW9ZWFZ
03127                   FIN-SORTIE-LEVEL-MAX                            EQW9ZWFZ
03128      END-IF.                                                      EQW9ZWFZ
03129 *                                                                 EQW9ZWFZ
03130      IF  LEVEL-PREC                                               EQW9ZWFZ
03131          PERFORM  SORTIE-LEVEL-PREC THRU                          EQW9ZWFZ
03132                   FIN-SORTIE-LEVEL-PREC                           EQW9ZWFZ
03133      END-IF.                                                      EQW9ZWFZ
03134 *                                                                 EQW9ZWFZ
03135      IF  ERREUR-MANIPULATION                                      EQW9ZWFZ
03136          PERFORM  SORTIE-ERREUR-MANIP THRU                        EQW9ZWFZ
03137                   FIN-SORTIE-ERREUR-MANIP                         EQW9ZWFZ
03138      END-IF.                                                      EQW9ZWFZ
03139 *                                                                 EQW9ZWFZ
03140 * ABANDON * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
03141 *                                                                 EQW9ZWFZ
03142      MOVE 'SK57.ERREUR CODE FONCTION DANS MODULE-SORTIE' TO MESS  EQW9ZWFZ
03143      GO TO ABANDON-TACHE.                                         EQW9ZWFZ
03144 *                                                                 EQW9ZWFZ
03145  FIN-MODULE-SORTIE.   EXIT.                                       EQW9ZWFZ
03146 *                                                                 EQW9ZWFZ
03147 ***************************************************************** EQW9ZWFZ
03148 * AFFICHAGE DE LA MAP NORMALE PUIS  RETURN TRANSID AU MEME PGM  * EQW9ZWFZ
03149 ***************************************************************** EQW9ZWFZ
03150  SORTIE-AFFICHAGE-FORMAT.                                         EQW9ZWFZ
03151 *-----------------------*                                         EQW9ZWFZ
03152      IF COM-GENE-REAF = 'O'                                       EQW9ZWFZ
03153         PERFORM RESTAURATION-TS-ECRAN  THRU                       EQW9ZWFZ
03154                 FIN-RESTAURATION-TS-ECRAN                         EQW9ZWFZ
03155      ELSE                                                         EQW9ZWFZ
03156         PERFORM REMPLISSAGE-TS-ECRAN THRU                         EQW9ZWFZ
03157                 FIN-REMPLISSAGE-TS-ECRAN                          EQW9ZWFZ
03158      END-IF.                                                      EQW9ZWFZ
03159 *                                                                 EQW9ZWFZ
03160      IF COM-GENE-MESANO  NOT = SPACES AND                         EQW9ZWFZ
03161                                LOW-VALUE                          EQW9ZWFZ
03162         PERFORM LECTURE-ERREUR THRU                               EQW9ZWFZ
03163                 FIN-LECTURE-ERREUR                                EQW9ZWFZ
03164         MOVE    SPACES   TO COM-GENE-MESANO                       EQW9ZWFZ
03165         MOVE    W-ERREUR TO ECR-XMSGALO                           EQW9ZWFZ
03166      END-IF.                                                      EQW9ZWFZ
03167 *                                                                 EQW9ZWFZ
03168      PERFORM SEND-MAP THRU                                        EQW9ZWFZ
03169              FIN-SEND-MAP.                                        EQW9ZWFZ
03170 *                                                                 EQW9ZWFZ
03171      MOVE    SPACES    TO Z-COMMAREA-TACHE-JUMP.                  EQW9ZWFZ
03172      MOVE    NOM-TACHE TO NOM-TACHE-RETOUR.                       EQW9ZWFZ
03173 *                                                                 EQW9ZWFZ
03174      PERFORM RETOUR-COMMAREA THRU                                 EQW9ZWFZ
03175              FIN-RETOUR-COMMAREA.                                 EQW9ZWFZ
03176  FIN-SORTIE-AFFICHAGE-FORMAT.  EXIT.                              EQW9ZWFZ
03177 *                                                                 EQW9ZWFZ
03178 ***************************************************************** EQW9ZWFZ
03179 *  AFFICHAGE DE LA MAP EN ERREUR ET RETURN AU MEME PROGRAMME    * EQW9ZWFZ
03180 ***************************************************************** EQW9ZWFZ
03181  SORTIE-ERREUR.                                                   EQW9ZWFZ
03182 *-------------*                                                   EQW9ZWFZ
03183      IF COM-GENE-MESANO  NOT = SPACES AND                         EQW9ZWFZ
03184                                LOW-VALUE                          EQW9ZWFZ
03185         PERFORM LECTURE-ERREUR THRU                               EQW9ZWFZ
03186                 FIN-LECTURE-ERREUR                                EQW9ZWFZ
03187         MOVE    SPACES   TO COM-GENE-MESANO                       EQW9ZWFZ
03188         MOVE    W-ERREUR TO ECR-XMSGALO                           EQW9ZWFZ
03189      END-IF.                                                      EQW9ZWFZ
03190 *                                                                 EQW9ZWFZ
03191      IF ERREUR-DISPATCH                                           EQW9ZWFZ
03192         MOVE    SPACES        TO  Z-COMMAREA-SELECT               EQW9ZWFZ
03193 *       MOVE    SPACES        TO  COM-GENE-NEWMEN                 EQW9ZWFZ
03194         MOVE    ZONE-TS-ECRAN TO  Z-MAP                           EQW9ZWFZ
03195         PERFORM RESTAURATION-TS-ECRAN THRU                        EQW9ZWFZ
03196                 FIN-RESTAURATION-TS-ECRAN                         EQW9ZWFZ
03197         MOVE    CURSEUR       TO  ECR-XCDECL                      EQW9ZWFZ
03198         MOVE    W-ERREUR      TO  ECR-XMSGALO                     EQW9ZWFZ
03199         PERFORM SEND-MAP-ERREUR   THRU                            EQW9ZWFZ
03200                 FIN-SEND-MAP-ERREUR                               EQW9ZWFZ
03201      END-IF.                                                      EQW9ZWFZ
03202 *                                                                 EQW9ZWFZ
03203      IF ERREUR                                                    EQW9ZWFZ
03204         PERFORM RESTAURATION-MAP THRU                             EQW9ZWFZ
03205                 FIN-RESTAURATION-MAP                              EQW9ZWFZ
03206         PERFORM SEND-MAP-ERREUR-DATAONLY THRU                     EQW9ZWFZ
03207                 FIN-SEND-MAP-ERREUR-DATAONLY                      EQW9ZWFZ
03208      END-IF.                                                      EQW9ZWFZ
03209 *                                                                 EQW9ZWFZ
03210      IF ERREUR-SORTIE                                             EQW9ZWFZ
03211         PERFORM SEND-MAP-NO-ERASE-DATAONLY THRU                   EQW9ZWFZ
03212                 FIN-SEND-MAP-NO-ERASE-DATAONLY                    EQW9ZWFZ
03213      END-IF.                                                      EQW9ZWFZ
03214      MOVE SPACE     TO COM-GENE-REAF.                             EQW9ZWFZ
03215      MOVE NOM-TACHE TO NOM-TACHE-RETOUR.                          EQW9ZWFZ
03216      PERFORM RETOUR-COMMAREA THRU                                 EQW9ZWFZ
03217              FIN-RETOUR-COMMAREA.                                 EQW9ZWFZ
03218  FIN-SORTIE-ERREUR.  EXIT.                                        EQW9ZWFZ
03219 /                                                                 EQW9ZWFZ
03220 ***************************************************************** EQW9ZWFZ
03221 *  XCTL OU START POUR APPELER LE PROGRAMME SUIVANT              * EQW9ZWFZ
03222 ***************************************************************** EQW9ZWFZ
03223  SORTIE-SUITE.                                                    EQW9ZWFZ
03224 *------------*                                                    EQW9ZWFZ
03225 *                                                                 EQW9ZWFZ
03226 * ON NE PEUT PAS PASSER LA MAIN A L'ELEMENT DE CONVERSATION       EQW9ZWFZ
03227 * SUIVANT SI ON A ATTEINT LES 50 TRANSACTIONS POUR UNE            EQW9ZWFZ
03228 * CONVERSATION                                                    EQW9ZWFZ
03229 *                                                                 EQW9ZWFZ
03230      IF COM-GENE-INDCNV = 50                                      EQW9ZWFZ
03231         MOVE    'SQ004' TO COM-GENE-MESANO                        EQW9ZWFZ
03232                            COM-CODERR                             EQW9ZWFZ
03233         MOVE    2       TO KONTROL                                EQW9ZWFZ
03234         MOVE    ZONE-TS-ECRAN TO Z-MAP                            EQW9ZWFZ
03235         PERFORM RESTAURATION-TS-ECRAN THRU                        EQW9ZWFZ
03236                 FIN-RESTAURATION-TS-ECRAN                         EQW9ZWFZ
03237         PERFORM SORTIE-ERREUR THRU                                EQW9ZWFZ
03238                 FIN-SORTIE-ERREUR                                 EQW9ZWFZ
03239      END-IF.                                                      EQW9ZWFZ
03240      PERFORM DELETE-TS-ECRAN THRU                                 EQW9ZWFZ
03241              FIN-DELETE-TS-ECRAN.                                 EQW9ZWFZ
03242 *                     DOUBLE AFFICHAGE                            EQW9ZWFZ
03243      MOVE    SPACE    TO COM-GENE-REAF.                           EQW9ZWFZ
03244      MOVE    NOM-PROG TO COM-PGMPRC.                              EQW9ZWFZ
03245      PERFORM XCTL-PROG-COMMAREA THRU                              EQW9ZWFZ
03246              FIN-XCTL-PROG-COMMAREA.                              EQW9ZWFZ
03247 *                                                                 EQW9ZWFZ
03248  FIN-SORTIE-SUITE.   EXIT.                                        EQW9ZWFZ
03249 *                                                                 EQW9ZWFZ
03250 ***************************************************************** EQW9ZWFZ
03251 *    RETOUR APRES     PF3    AU MENU SUPERIEUR                    EQW9ZWFZ
03252 ***************************************************************** EQW9ZWFZ
03253 *                                                                 EQW9ZWFZ
03254  SORTIE-LEVEL-SUPERIEUR.                                          EQW9ZWFZ
03255 *----------------------*                                          EQW9ZWFZ
03256      PERFORM          DELETE-TS-PLAN THRU                         EQW9ZWFZ
03257                       FIN-DELETE-TS-PLAN.                         EQW9ZWFZ
03258 *  REMISE A BLANC DE LA PILE DES CONVERSATIONS                    EQW9ZWFZ
03259 *  ET DES ENCHAINEMENTS                                           EQW9ZWFZ
03260      MOVE SPACES TO COM-GENE-CNVPIL.                              EQW9ZWFZ
03261      MOVE SPACES TO COM-GENE-ENCCNV.                              EQW9ZWFZ
03262      MOVE ZERO   TO COM-GENE-INDCNV.                              EQW9ZWFZ
03263 *                   DOUBLE AFFICHAGE                              EQW9ZWFZ
03264      MOVE SPACE  TO COM-GENE-REAF.                                EQW9ZWFZ
03265 *  RECUPERATION DU DERNIER MENU                                   EQW9ZWFZ
03266      MOVE COM-GENE-PILMEN(COM-GENE-INDMEN) TO NOM-TACHE-START,    EQW9ZWFZ
03267                                               COM-GENE-NEWMEN.    EQW9ZWFZ
03268      IF   COM-GENE-TYPMEN(COM-GENE-INDMEN) = 'O'                  EQW9ZWFZ
03269      MOVE COM-GENE-EXPTRNID                TO NOM-TACHE-START     EQW9ZWFZ
03270      END-IF.                                                      EQW9ZWFZ
03271      MOVE LONG-COMMAREA  TO  LONG-START.                          EQW9ZWFZ
03272      MOVE EIBTRMID       TO  TERM-START.                          EQW9ZWFZ
03273      MOVE NOM-PROG       TO  COM-PGMPRC.                          EQW9ZWFZ
03274 *  DELETE DE LA TS ECRAN                                          EQW9ZWFZ
03275      PERFORM          DELETE-TS-ECRAN THRU                        EQW9ZWFZ
03276                       FIN-DELETE-TS-ECRAN.                        EQW9ZWFZ
03277 *    DELETE DES TS APPLICATIVES ET CONFIDENTIALITE CONVERSATION   EQW9ZWFZ
03278      PERFORM          DELETE-TS-CONF-CONV THRU                    EQW9ZWFZ
03279                       FIN-DELETE-TS-CONF-CONV.                    EQW9ZWFZ
03280      MOVE SPACES                 TO COM-GENE-SWPCNV.              EQW9ZWFZ
03281      PERFORM START-TACHE THRU                                     EQW9ZWFZ
03282              FIN-START-TACHE.                                     EQW9ZWFZ
03283      PERFORM RETOUR      THRU                                     EQW9ZWFZ
03284              FIN-RETOUR.                                          EQW9ZWFZ
03285  FIN-SORTIE-LEVEL-SUPERIEUR.   EXIT.                              EQW9ZWFZ
03286 *                                                                 EQW9ZWFZ
03287 ***************************************************************** EQW9ZWFZ
03288 *    RETOUR APRES PF12 AU NIVEAU SUPERIEUR DANS UNE CONVERSATION  EQW9ZWFZ
03289 ***************************************************************** EQW9ZWFZ
03290  SORTIE-LEVEL-PREC.                                               EQW9ZWFZ
03291 *-----------------*                                               EQW9ZWFZ
03292 *       ON NE PEUT PAS SORTIR DU PREMIER NIVEAU PAR PF12          EQW9ZWFZ
03293      IF COM-GENE-PILCNV(1) = NOM-TACHE                            EQW9ZWFZ
03294         MOVE 'SQ002' TO COM-GENE-MESANO                           EQW9ZWFZ
03295                         COM-CODERR                                EQW9ZWFZ
03296         MOVE 2       TO KONTROL                                   EQW9ZWFZ
03297         MOVE ZONE-TS-ECRAN TO Z-MAP                               EQW9ZWFZ
03298         PERFORM RESTAURATION-TS-ECRAN THRU                        EQW9ZWFZ
03299                 FIN-RESTAURATION-TS-ECRAN                         EQW9ZWFZ
03300         PERFORM SORTIE-ERREUR THRU                                EQW9ZWFZ
03301                 FIN-SORTIE-ERREUR                                 EQW9ZWFZ
03302      END-IF.                                                      EQW9ZWFZ
03303 *                                                                 EQW9ZWFZ
03304 *  REMISE A BLANC DU POSTE ACTUEL DANS LA PILE DES CONVERSATIONS  EQW9ZWFZ
03305 *                                                                 EQW9ZWFZ
03306      MOVE SPACES TO COM-GENE-PILCNV(COM-GENE-INDCNV).             EQW9ZWFZ
03307 *                                                                 EQW9ZWFZ
03308 * RECUPERATION DU NIVEAU SUPERIEUR DANS LA PILE DES CONVERSATIONS EQW9ZWFZ
03309 * ICI GESTION HORS FUE PERMETTANT EN EN ET RP DE NE PAS REAFFICHE EQW9ZWFZ
03310 * L'ÈCRAN PERSONNE FB02                                           EQW9ZWFZ
03311      SUBTRACT 1 FROM COM-GENE-INDCNV.                             EQW9ZWFZ
03312      MOVE COM-GENE-PILCNV(COM-GENE-INDCNV) TO NOM-TACHE-XCTL.     EQW9ZWFZ
03313 *  DELETE DE LA TS ECRAN                                          EQW9ZWFZ
03314      PERFORM          DELETE-TS-ECRAN THRU                        EQW9ZWFZ
03315                       FIN-DELETE-TS-ECRAN.                        EQW9ZWFZ
03316 *                                                                 EQW9ZWFZ
03317      MOVE SPACE        TO COM-GENE-REAF.                          EQW9ZWFZ
03318 *                                                                 EQW9ZWFZ
03319      MOVE NOM-PROG     TO COM-PGMPRC.                             EQW9ZWFZ
03320      PERFORM XCTL-PROG-COMMAREA THRU                              EQW9ZWFZ
03321              FIN-XCTL-PROG-COMMAREA.                              EQW9ZWFZ
03322  FIN-SORTIE-LEVEL-PREC.   EXIT.                                   EQW9ZWFZ
03323 *                                                                 EQW9ZWFZ
03324 ***************************************************************** EQW9ZWFZ
03325 *    RETOUR APRES CLEAR (OU SI EIBCALEN = 0) AU PROGRAMME DE      EQW9ZWFZ
03326 *    SIGNATURE TOUJOURS PAR START                                 EQW9ZWFZ
03327 ***************************************************************** EQW9ZWFZ
03328  SORTIE-LEVEL-SIGNATURE.                                          EQW9ZWFZ
03329 *----------------------*                                          EQW9ZWFZ
03330      PERFORM          DELETE-TS-PLAN THRU                         EQW9ZWFZ
03331                       FIN-DELETE-TS-PLAN.                         EQW9ZWFZ
03332 *    DELETE DES TS APPLICATIVES      CONVERSATION                 EQW9ZWFZ
03333 *    DELETE DE LA TS CONFIDENTIALITE CONVERSATION                 EQW9ZWFZ
03334      PERFORM          DELETE-TS-CONF-CONV THRU                    EQW9ZWFZ
03335                       FIN-DELETE-TS-CONF-CONV.                    EQW9ZWFZ
03336      MOVE LONG-COMMAREA  TO  LONG-START.                          EQW9ZWFZ
03337      MOVE EIBTRMID       TO  TERM-START.                          EQW9ZWFZ
03338      MOVE 'AA00'         TO  NOM-TACHE-START.                     EQW9ZWFZ
03339      MOVE NOM-PROG       TO  COM-PGMPRC.                          EQW9ZWFZ
03340 *    DELETE DE LA TS ECRAN                                        EQW9ZWFZ
03341      PERFORM DELETE-TS-ECRAN THRU                                 EQW9ZWFZ
03342              FIN-DELETE-TS-ECRAN.                                 EQW9ZWFZ
03343 *                                                                 EQW9ZWFZ
03344      MOVE SPACES                 TO COM-GENE-SWPCNV.              EQW9ZWFZ
03345      MOVE    SPACE       TO COM-GENE-REAF.                        EQW9ZWFZ
03346 *                                                                 EQW9ZWFZ
03347      PERFORM START-TACHE THRU                                     EQW9ZWFZ
03348              FIN-START-TACHE.                                     EQW9ZWFZ
03349      PERFORM RETOUR      THRU                                     EQW9ZWFZ
03350              FIN-RETOUR.                                          EQW9ZWFZ
03351 *                                                                 EQW9ZWFZ
03352  FIN-SORTIE-LEVEL-SIGNATURE.         EXIT.                        EQW9ZWFZ
03353 *                                                                 EQW9ZWFZ
03354 ***************************************************************** EQW9ZWFZ
03355 *    RETOUR APRES PF4 AU MENU  PRINCIPAL                          EQW9ZWFZ
03356 ***************************************************************** EQW9ZWFZ
03357 *                                                                 EQW9ZWFZ
03358  SORTIE-LEVEL-MAX.                                                EQW9ZWFZ
03359 *----------------*                                                EQW9ZWFZ
03360      PERFORM          DELETE-TS-PLAN THRU                         EQW9ZWFZ
03361                       FIN-DELETE-TS-PLAN.                         EQW9ZWFZ
03362 *  REMISE A BLANC DE LA PILE DES CONVERSATIONS                    EQW9ZWFZ
03363 *  ET DES ENCHAINEMENTS                                           EQW9ZWFZ
03364      MOVE SPACES TO COM-GENE-CNVPIL.                              EQW9ZWFZ
03365      MOVE SPACES TO COM-GENE-ENCCNV.                              EQW9ZWFZ
03366      MOVE ZERO   TO COM-GENE-INDCNV.                              EQW9ZWFZ
03367 *  RECUPERATION DU MENU PRINCIPAL                                 EQW9ZWFZ
03368 *  REMISE A ZERO DE L'INDICE MENU                                 EQW9ZWFZ
03369 *  REMISE A BLANC DE LA PILE DES MENUS                            EQW9ZWFZ
03370 *                                                                 EQW9ZWFZ
03371      MOVE COM-GENE-PILMEN(1) TO COM-GENE-NEWMEN.                  EQW9ZWFZ
03372      MOVE COM-GENE-EXPTRNID  TO NOM-TACHE-START.                  EQW9ZWFZ
03373 */                                                                EQW9ZWFZ
03374      MOVE ZERO   TO COM-GENE-INDMEN.                              EQW9ZWFZ
03375      MOVE SPACES TO COM-GENE-MENPIL.                              EQW9ZWFZ
03376 *                                                                 EQW9ZWFZ
03377      MOVE SPACE        TO COM-GENE-REAF.                          EQW9ZWFZ
03378 *  DELETE DE LA TS ECRAN                                          EQW9ZWFZ
03379      PERFORM          DELETE-TS-ECRAN THRU                        EQW9ZWFZ
03380                       FIN-DELETE-TS-ECRAN.                        EQW9ZWFZ
03381      MOVE    LONG-COMMAREA  TO  LONG-START.                       EQW9ZWFZ
03382      MOVE    EIBTRMID       TO  TERM-START.                       EQW9ZWFZ
03383      MOVE    NOM-PROG       TO  COM-PGMPRC.                       EQW9ZWFZ
03384 *    DELETE  DES TS APPLICATIVES ET CONFIDENTIALITE CONVERSATION  EQW9ZWFZ
03385      PERFORM DELETE-TS-CONF-CONV THRU                             EQW9ZWFZ
03386              FIN-DELETE-TS-CONF-CONV.                             EQW9ZWFZ
03387      MOVE SPACES                 TO COM-GENE-SWPCNV.              EQW9ZWFZ
03388      PERFORM START-TACHE THRU                                     EQW9ZWFZ
03389              FIN-START-TACHE.                                     EQW9ZWFZ
03390      PERFORM RETOUR      THRU                                     EQW9ZWFZ
03391              FIN-RETOUR.                                          EQW9ZWFZ
03392  FIN-SORTIE-LEVEL-MAX.  EXIT.                                     EQW9ZWFZ
03393 *                                                                 EQW9ZWFZ
03394 ***************************************************************** EQW9ZWFZ
03395 * SORTIE ERREUR MANIPULATION DES TOUCHES FONCTION               * EQW9ZWFZ
03396 ***************************************************************** EQW9ZWFZ
03397  SORTIE-ERREUR-MANIP.                                             EQW9ZWFZ
03398 *-------------------*                                             EQW9ZWFZ
03399      MOVE   'SQ007'        TO COM-GENE-MESANO                     EQW9ZWFZ
03400                               COM-CODERR                          EQW9ZWFZ
03401      MOVE    2             TO KONTROL                             EQW9ZWFZ
03402      MOVE    ZONE-TS-ECRAN TO Z-MAP                               EQW9ZWFZ
03403      PERFORM RESTAURATION-TS-ECRAN THRU                           EQW9ZWFZ
03404              FIN-RESTAURATION-TS-ECRAN                            EQW9ZWFZ
03405      PERFORM SORTIE-ERREUR THRU                                   EQW9ZWFZ
03406              FIN-SORTIE-ERREUR.                                   EQW9ZWFZ
03407  FIN-SORTIE-ERREUR-MANIP.  EXIT.                                  EQW9ZWFZ
03408 *                                                                 EQW9ZWFZ
03409      EXIT.                                                        EQW9ZWFZ
03410 * **************************************************              EQW9ZWFZ
03411 *    RESTAURATION-MAP  PAR RESTAURATION TS-ECRAN                  EQW9ZWFZ
03412 * **************************************************              EQW9ZWFZ
03413 *                                                                 EQW9ZWFZ
03414 ***************************************************************** EQW9ZWFZ
03415 *    RESTAURATION MAP                                             EQW9ZWFZ
03416 ***************************************************************** EQW9ZWFZ
03417 *                                                                 EQW9ZWFZ
03418  RESTAURATION-MAP.                                                EQW9ZWFZ
03419 *                                                                 EQW9ZWFZ
03420      PERFORM RESTAURATION-TS-ECRAN THRU                           EQW9ZWFZ
03421              FIN-RESTAURATION-TS-ECRAN.                           EQW9ZWFZ
03422 *                                                                 EQW9ZWFZ
03423      MOVE LOW-VALUE  TO  ECR-XTRMTRACO.                           EQW9ZWFZ
03424      MOVE LOW-VALUE  TO  ECR-XAPPLILO.                            EQW9ZWFZ
03425      MOVE LOW-VALUE  TO  ECR-XJOURDO.                             EQW9ZWFZ
03426      MOVE LOW-VALUE  TO  ECR-XRACFLO.                             EQW9ZWFZ
03427      MOVE LOW-VALUE  TO  ECR-XHEUREDO.                            EQW9ZWFZ
03428      MOVE LOW-VALUE  TO  ECR-GESCLIO.                             EQW9ZWFZ
03429      MOVE LOW-VALUE  TO  ECR-RAICO.                               EQW9ZWFZ
03430      MOVE LOW-VALUE  TO  ECR-NOMCO.                               EQW9ZWFZ
03431 *                                                                 EQW9ZWFZ
03432      MOVE 0 TO IL.                                                EQW9ZWFZ
03433      PERFORM BOUCLE-RTMP1  THRU                                   EQW9ZWFZ
03434              FIN-BOUCLE-RTMP1  10 TIMES.                          EQW9ZWFZ
03435 *                                                                 EQW9ZWFZ
03436      MOVE LOW-VALUE  TO  ECR-AJOUTCONO.                           EQW9ZWFZ
03437      MOVE LOW-VALUE  TO  ECR-AJOUTVEHO.                           EQW9ZWFZ
03438      MOVE LOW-VALUE  TO  ECR-XCDECO.                              EQW9ZWFZ
03439 *                                                                 EQW9ZWFZ
03440  FIN-RESTAURATION-MAP.  EXIT.                                     EQW9ZWFZ
03441               EJECT                                               EQW9ZWFZ
03442 *                                                                 EQW9ZWFZ
03443  BOUCLE-RTMP1.                                                    EQW9ZWFZ
03444      ADD 1 TO IL.                                                 EQW9ZWFZ
03445      MOVE LOW-VALUE  TO ECR-CSECODCO (IL).                        EQW9ZWFZ
03446      MOVE LOW-VALUE  TO ECR-IDENTIFO (IL).                        EQW9ZWFZ
03447      MOVE LOW-VALUE  TO ECR-DATENTDO (IL).                        EQW9ZWFZ
03448      MOVE LOW-VALUE  TO ECR-DATSORDO (IL).                        EQW9ZWFZ
03449      MOVE LOW-VALUE  TO ECR-MOTIFSCO (IL).                        EQW9ZWFZ
03450      MOVE LOW-VALUE  TO ECR-STATYPCO (IL).                        EQW9ZWFZ
03451      MOVE LOW-VALUE  TO ECR-PTSCRMXO (IL).                        EQW9ZWFZ
03452 *                                                                 EQW9ZWFZ
03453  FIN-BOUCLE-RTMP1.  EXIT.                                         EQW9ZWFZ
03454 *                                                                 EQW9ZWFZ
03455 *                                                                 EQW9ZWFZ
03456 ******************************************************************EQW9ZWFZ
03457 * MISE ‡ JOUR DES PERSONNES DANS LA TS TECHNIQUE                 *EQW9ZWFZ
03458 ******************************************************************EQW9ZWFZ
03459 *                                                                 EQW9ZWFZ
03460  ENREG-TS-TECH-PERS.                                              EQW9ZWFZ
03461 *-------------------                                              EQW9ZWFZ
03462      MOVE SPACES              TO   WSS-IDENTIFIANT                EQW9ZWFZ
03463                                    WSS-NOM.                       EQW9ZWFZ
03464      MOVE 'P'                 TO   PERTYPTS OF TAB-PERS (I-PERS). EQW9ZWFZ
03465      MOVE I-PERS              TO   PERRANTS OF TAB-PERS (I-PERS). EQW9ZWFZ
03466      MOVE PERNOML  OF DONNEES-PERSONNE (1) TO WSS-NOM.            EQW9ZWFZ
03467      IF WSS-NOM NOT = SPACES                                      EQW9ZWFZ
03468         PERFORM VARYING IC FROM 20 BY -1                          EQW9ZWFZ
03469            UNTIL WSS-NOM(IC:1) NOT = SPACE                        EQW9ZWFZ
03470         END-PERFORM                                               EQW9ZWFZ
03471         MOVE WSS-NOM(1:IC)       TO   WSS-IDENTIFIANT             EQW9ZWFZ
03472      ELSE                                                         EQW9ZWFZ
03473         MOVE WSS-NOM             TO   WSS-IDENTIFIANT             EQW9ZWFZ
03474         MOVE ZERO                TO   IC                          EQW9ZWFZ
03475      END-IF.                                                      EQW9ZWFZ
03476      ADD 1                    TO   IC.                            EQW9ZWFZ
03477      MOVE ' '                 TO   WSS-IDENTIFIANT(IC:1).         EQW9ZWFZ
03478      ADD 1                    TO   IC.                            EQW9ZWFZ
03479      COMPUTE IE = 30 - IC.                                        EQW9ZWFZ
03480      MOVE PERPREL OF DONNEES-PERSONNE (1)                         EQW9ZWFZ
03481                               TO   WSS-IDENTIFIANT(IC:IE).        EQW9ZWFZ
03482      MOVE WSS-IDENTIFIANT     TO   PERIDENT OF TAB-PERS (I-PERS). EQW9ZWFZ
03483      MOVE RPERENTD OF DONNEES-PERSONNE (1)                        EQW9ZWFZ
03484                               TO   RPERENTD OF TAB-PERS (I-PERS). EQW9ZWFZ
03485      MOVE RPERSORD OF DONNEES-PERSONNE (1)                        EQW9ZWFZ
03486                               TO RPERSORD OF TAB-PERS (I-PERS).   EQW9ZWFZ
03487      MOVE PERMTFC  OF DONNEES-PERSONNE (1)                        EQW9ZWFZ
03488                               TO PERMTFC  OF TAB-PERS (I-PERS).   EQW9ZWFZ

F37105     MOVE 'N' TO WSS-TOP-PERMIS.
03489      PERFORM VARYING IC FROM 1 BY 1 UNTIL IC > 4                  EQW9ZWFZ
F37105        IF PRMTYPC OF DONNEES-PERSONNE (1, IC) > SPACE            EQW9ZWFZ
F37105           MOVE 'O' TO WSS-TOP-PERMIS
03501            MOVE PRMTYPC OF DONNEES-PERSONNE (1 , IC)              EQW9ZWFZ
03502                               TO PRMTYPC OF TAB-PERS (I-PERS, IC) EQW9ZWFZ
F37105        ELSE
F37105           MOVE SPACE         TO PRMTYPC OF TAB-PERS (I-PERS, IC) EQW9ZWFZ
03503         END-IF                                                    EQW9ZWFZ
03504      END-PERFORM.                                                 EQW9ZWFZ
F37105     IF PERSTAC OF DONNEES-PERSONNE (1)  = 'EN'                   EQW9ZWFZ
F37105        IF WSS-TOP-PERMIS = 'O'
F37105              MOVE 'ENAP'     TO PERSTAC OF TAB-PERS (I-PERS)     EQW9ZWFZ
F37105        ELSE
F37105              MOVE 'ENSP'     TO PERSTAC OF TAB-PERS (I-PERS)     EQW9ZWFZ
F37105        END-IF
F37105     ELSE
F37105        MOVE PERSTAC OF DONNEES-PERSONNE (1)                      EQW9ZWFZ
F37105                              TO PERSTAC OF TAB-PERS (I-PERS)     EQW9ZWFZ
F37105     END-IF.

03505      MOVE SPACES              TO  RPERPTST  OF TAB-PERS (I-PERS). EQW9ZWFZ
03506      MOVE PERACTC OF DONNEES-PERSONNE (1)                         EQW9ZWFZ
03507                               TO  PERACTC   OF TAB-PERS (I-PERS). EQW9ZWFZ
03508 *                                                                 EQW9ZWFZ
03509  FENREG-TS-TECH-PERS.                                             EQW9ZWFZ
03510      EXIT.                                                        EQW9ZWFZ
03511 *                                                                 EQW9ZWFZ
03512 ******************************************************************EQW9ZWFZ
03513 * MISE ‡ JOUR DES VEHICULES DANS LA TS TECHNIQUE                 *EQW9ZWFZ
03514 ******************************************************************EQW9ZWFZ
03515 *                                                                 EQW9ZWFZ
03516  ENREG-TS-TECH-VEHI.                                              EQW9ZWFZ
03517 *-------------------                                              EQW9ZWFZ
03518      MOVE SPACES              TO   WSS-IDENTIFIANT                EQW9ZWFZ
03519                                    WSS-IMMAT.                     EQW9ZWFZ
03520      MOVE 'V'                 TO   VEHTYPTS OF TAB-VEHI (I-VEHI). EQW9ZWFZ
03521      MOVE I-VEHI              TO   VEHRANTS OF TAB-VEHI (I-VEHI). EQW9ZWFZ
03522      MOVE VEHIMMX  OF TS-VEHICULE (1) TO WSS-IMMAT.               EQW9ZWFZ
03523      IF WSS-IMMAT NOT  = SPACES                                   EQW9ZWFZ
03524         PERFORM VARYING IC FROM 10 BY -1                          EQW9ZWFZ
03525            UNTIL WSS-IMMAT(IC:1) NOT = SPACE                      EQW9ZWFZ
03526         END-PERFORM                                               EQW9ZWFZ
03527         MOVE WSS-IMMAT(1:IC)     TO   WSS-IDENTIFIANT             EQW9ZWFZ
03528      ELSE                                                         EQW9ZWFZ
03529         MOVE WSS-IMMAT           TO   WSS-IDENTIFIANT             EQW9ZWFZ
03530         MOVE ZERO                TO   IC                          EQW9ZWFZ
03531      END-IF.                                                      EQW9ZWFZ
03532      ADD 1                    TO   IC.                            EQW9ZWFZ
03533      MOVE ' '                 TO   WSS-IDENTIFIANT(IC:1).         EQW9ZWFZ
03534      ADD 1                    TO   IC.                            EQW9ZWFZ
03535      COMPUTE IE = 30 - IC.                                        EQW9ZWFZ
03536      IF VEHTYPC OF TS-VEHICULE (1) = '4R ' OR '2R '               EQW9ZWFZ
03537         MOVE VEHCODC OF TS-VEHICULE (1)                           EQW9ZWFZ
03538                               TO   WSS-IDENTIFIANT(IC:IE)         EQW9ZWFZ
03539      END-IF.                                                      EQW9ZWFZ
03540      IF VEHTYPC OF TS-VEHICULE (1)                                EQW9ZWFZ
03541                                          = 'CC ' OR 'REM' OR 'CAR'EQW9ZWFZ
03542         MOVE VEHMARL OF TS-VEHICULE (1)                           EQW9ZWFZ
03543                               TO   WSS-IDENTIFIANT(IC:IE)         EQW9ZWFZ
03544      END-IF.                                                      EQW9ZWFZ
03545      MOVE WSS-IDENTIFIANT     TO   VEHIDENT OF TAB-VEHI (I-VEHI). EQW9ZWFZ
03546      MOVE RVEHENTD OF TS-VEHICULE (1)                             EQW9ZWFZ
03547                               TO   RVEHENTD OF TAB-VEHI (I-VEHI). EQW9ZWFZ
03548      MOVE RVEHSORD OF TS-VEHICULE (1)                             EQW9ZWFZ
03549                               TO   RVEHSORD OF TAB-VEHI (I-VEHI). EQW9ZWFZ
03550      MOVE VEHMTFC  OF TS-VEHICULE (1)                             EQW9ZWFZ
03551                               TO   VEHMTFC  OF TAB-VEHI (I-VEHI). EQW9ZWFZ
03552      MOVE VEHTYPC  OF TS-VEHICULE (1)                             EQW9ZWFZ
03553                               TO   VEHTYPC  OF TAB-VEHI (I-VEHI). EQW9ZWFZ
03554      MOVE VEHFORC  OF TS-VEHICULE (1)                             EQW9ZWFZ
03555                               TO   VEHFORC  OF TAB-VEHI (I-VEHI). EQW9ZWFZ
03556      IF VEHGENC OF TS-VEHICULE (1) NOT = SPACES AND      LOW-VALUEEQW9ZWFZ
03557         MOVE VEHGENC  OF TS-VEHICULE (1)                          EQW9ZWFZ
03558                               TO   VEHGENC  OF TAB-VEHI (I-VEHI)  EQW9ZWFZ
03559      ELSE                                                         EQW9ZWFZ
03560         IF VEHGENC OF TS-VEHICULE (2) NOT = SPACES                EQW9ZWFZ
03561                                              AND LOW-VALUE        EQW9ZWFZ
03562            MOVE VEHGENC  OF TS-VEHICULE (2)                       EQW9ZWFZ
03563                               TO   VEHGENC  OF TAB-VEHI (I-VEHI)  EQW9ZWFZ
03564      END-IF.                                                      EQW9ZWFZ
03565      IF RVEHCYLN OF TS-VEHICULE(1) NOT = SPACES AND      LOW-VALUEEQW9ZWFZ
03566         MOVE RVEHCYLN  OF TS-VEHICULE (1)                         EQW9ZWFZ
03567                               TO  RVEHCYLN  OF TAB-VEHI (I-VEHI)  EQW9ZWFZ
03568      ELSE                                                         EQW9ZWFZ
03569         IF RVEHCYLN OF TS-VEHICULE (2) NOT = SPACES               EQW9ZWFZ
03570                                              AND LOW-VALUE        EQW9ZWFZ
03571            MOVE RVEHCYLN  OF TS-VEHICULE (2)                      EQW9ZWFZ
03572                               TO  RVEHCYLN  OF TAB-VEHI (I-VEHI)  EQW9ZWFZ
03573      END-IF.                                                      EQW9ZWFZ
03574      MOVE VEHACTC  OF TS-VEHICULE (1)                             EQW9ZWFZ
03575                               TO   VEHACTC  OF TAB-VEHI (I-VEHI). EQW9ZWFZ
03790      MOVE VEHRSTC  OF TS-VEHICULE (1)                             FB01T00B
03791                               TO   VEHRSTC  OF TAB-VEHI (I-VEHI). FB01T00B
03792      MOVE VEHCHGC  OF TS-VEHICULE (1)                             FB01T00B
03793                               TO   VEHCHGC  OF TAB-VEHI (I-VEHI). FB01T00B
03576      IF VEHCRFC OF TS-VEHICULE (1) = SPACES OR LOW-VALUE          EQW9ZWFZ
03577         MOVE VEHCRMC  OF TS-VEHICULE (1)                          EQW9ZWFZ
03578                               TO   VEHCRMC  OF TAB-VEHI (I-VEHI)  EQW9ZWFZ
03579      ELSE                                                         EQW9ZWFZ
03580         MOVE VEHCRFC  OF TS-VEHICULE (1)                          EQW9ZWFZ
03581                               TO   VEHCRMC  OF TAB-VEHI (I-VEHI)  EQW9ZWFZ
03582      END-IF.                                                      EQW9ZWFZ
03583      PERFORM VARYING I FROM 1 BY 1 UNTIL I = 10                   EQW9ZWFZ
03584              OR COVTYPC  OF TS-VEHICULE (1, I) = '1'              EQW9ZWFZ
03585      END-PERFORM.                                                 EQW9ZWFZ
03586      IF I = 10                                                    EQW9ZWFZ
03587         MOVE SPACES                                               EQW9ZWFZ
03588                               TO   RVEHCRMT OF TAB-VEHI (I-VEHI)  EQW9ZWFZ
03589      END-IF.                                                      EQW9ZWFZ
03590      IF COVTYPC  OF TS-VEHICULE (1, I) = '1'                      EQW9ZWFZ
03591         IF RCOVTAUT OF TS-VEHICULE (1, I)                         EQW9ZWFZ
03592                                       = SPACES OR LOW-VALUE       EQW9ZWFZ
03593            MOVE SPACES                                            EQW9ZWFZ
03594                               TO   RVEHCRMT OF TAB-VEHI (I-VEHI)  EQW9ZWFZ
03595         ELSE                                                      EQW9ZWFZ
03596            MOVE RCOVTAUT OF TS-VEHICULE (1, I)                    EQW9ZWFZ
03597                               TO   RVEHCRMT OF TAB-VEHI (I-VEHI)  EQW9ZWFZ
03598         END-IF                                                    EQW9ZWFZ
03599         IF RCOVTAFT OF TS-VEHICULE (1, I)                         EQW9ZWFZ
03600                                   NOT = SPACES AND LOW-VALUE      EQW9ZWFZ
03601            MOVE RCOVTAFT OF TS-VEHICULE (1, I)                    EQW9ZWFZ
03602                               TO   RVEHCRMT OF TAB-VEHI (I-VEHI)  EQW9ZWFZ
03603         END-IF                                                    EQW9ZWFZ
03604      END-IF.                                                      EQW9ZWFZ
03605 *                                                                 EQW9ZWFZ
03606  FENREG-TS-TECH-VEHI.                                             EQW9ZWFZ
03607      EXIT.                                                        EQW9ZWFZ
03608 ******************************************************************EQW9ZWFZ
03609 * SUPPRESSION DE LE TS TECHNIQUE                                 *EQW9ZWFZ
03610 ******************************************************************EQW9ZWFZ
03611 *                                                                 EQW9ZWFZ
03612  DEL-TS-TECHNIQUE.                                                EQW9ZWFZ
03613 *-----------------                                                EQW9ZWFZ
03614      MOVE EIBTRMID           TO   TS-TECHNIQUE-EIBTRMID.          EQW9ZWFZ
03615      MOVE IDENT-TS-TECHNIQUE TO   IDENT-TS.                       EQW9ZWFZ
03616      PERFORM DELETE-TS       THRU FIN-DELETE-TS.                  EQW9ZWFZ
03617      MOVE +0                 TO   COM-FB-RANG-MAX-TSTECH.         EQW9ZWFZ
03618 *                                                                 EQW9ZWFZ
03619  FDEL-TS-TECHNIQUE.                                               EQW9ZWFZ
03620      EXIT.                                                        EQW9ZWFZ
03621 *                                                                 EQW9ZWFZ
03622 ******************************************************************EQW9ZWFZ
03623 *   LECTURE DES ITEMS DE LA TS SUSPENS                           *EQW9ZWFZ
03624 ******************************************************************EQW9ZWFZ
03625 *                                                                 EQW9ZWFZ
03626  LECT-TS-SUSPENS.                                                 EQW9ZWFZ
03627                                                                   EQW9ZWFZ
03628 *** LECTURE ITEM 1 ***                                            EQW9ZWFZ
03629      MOVE +1 TO RANG-TS.                                          EQW9ZWFZ
03630      EXEC CICS READQ TS QUEUE   (IDENT-TS-APP)                    EQW9ZWFZ
03631                         INTO    (TS-SUSPENS1)                     EQW9ZWFZ
03632                         LENGTH  (LONG-TS-SUSPENS)                 EQW9ZWFZ
03633                         ITEM    (RANG-TS)                         EQW9ZWFZ
03634                         NOHANDLE                                  EQW9ZWFZ
03635      END-EXEC.                                                    EQW9ZWFZ
03636      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03637         MOVE 'SUS1 : ERREUR READ TS-SUSPENS1' TO MESS             EQW9ZWFZ
03638         GO TO ABANDON-TACHE                                       EQW9ZWFZ
03639      ELSE                                                         EQW9ZWFZ
03640         MOVE SEGTRA OF TS-SUSPENS1 TO FBMISPTR-IT1                EQW9ZWFZ
03641      END-IF.                                                      EQW9ZWFZ
03642                                                                   EQW9ZWFZ
03643 *** LECTURE ITEM 2 ***                                            EQW9ZWFZ
03644      MOVE +2 TO RANG-TS.                                          EQW9ZWFZ
03645      EXEC CICS READQ TS QUEUE   (IDENT-TS-APP)                    EQW9ZWFZ
03646                         INTO    (TS-SUSPENS2)                     EQW9ZWFZ
03647                         LENGTH  (LONG-TS-SUSPENS)                 EQW9ZWFZ
03648                         ITEM    (RANG-TS)                         EQW9ZWFZ
03649                         NOHANDLE                                  EQW9ZWFZ
03650      END-EXEC.                                                    EQW9ZWFZ
03651      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03652         MOVE 'SUS2 : ERREUR READ TS-SUSPENS2' TO MESS             EQW9ZWFZ
03653         GO TO ABANDON-TACHE                                       EQW9ZWFZ
03654      ELSE                                                         EQW9ZWFZ
03655         MOVE SEGTRA OF TS-SUSPENS2 TO FBMISPTR-IT2                EQW9ZWFZ
03656      END-IF.                                                      EQW9ZWFZ
03657 *                                                                 EQW9ZWFZ
03658  FLECT-TS-SUSPENS. EXIT.                                          EQW9ZWFZ
03659 *                                                                 EQW9ZWFZ
03660 ***************************************************************   EQW9ZWFZ
03661 *            ECRITURE TS SUSPENS                              *   EQW9ZWFZ
03662 ***************************************************************   EQW9ZWFZ
03663  ECR-TS-SUSPENS.                                                  EQW9ZWFZ
03664                                                                   EQW9ZWFZ
03665      MOVE +1 TO RANG-TS.                                          EQW9ZWFZ
03666      MOVE COM-FB-NBRE-VEHI-ENC TO NBRE-VEHI-ENC OF FBMISPTR-IT1.  EQW9ZWFZ
03667      MOVE FBMISPTR-IT1 TO SEGTRA OF TS-SUSPENS1.                  EQW9ZWFZ
03668      EXEC CICS WRITEQ TS QUEUE (IDENT-TS-APP)                     EQW9ZWFZ
03669                          FROM  (TS-SUSPENS1)                      EQW9ZWFZ
03670                          LENGTH (LENGTH OF TS-SUSPENS1)           EQW9ZWFZ
03671                          ITEM  (RANG-TS)                          EQW9ZWFZ
03672                          REWRITE                                  EQW9ZWFZ
03673                          MAIN                                     EQW9ZWFZ
03674                          NOHANDLE                                 EQW9ZWFZ
03675      END-EXEC.                                                    EQW9ZWFZ
03676      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03677         MOVE 'WDL1 ERR.WRITE TS-SUSPENS1' TO MESS                 EQW9ZWFZ
03678         GO TO ABANDON-TACHE                                       EQW9ZWFZ
03679      END-IF.                                                      EQW9ZWFZ
03680                                                                   EQW9ZWFZ
03681  FIN-ECR-TS-SUSPENS. EXIT.                                        EQW9ZWFZ
03682 *                                                                 EQW9ZWFZ
03683 ******************************************************************EQW9ZWFZ
03684 * ECRITURE DE LA TS TECHNIQUE                                    *EQW9ZWFZ
03685 ******************************************************************EQW9ZWFZ
03686 *                                                                 EQW9ZWFZ
03687  ECR-TS-TECHNIQUE.                                                EQW9ZWFZ
03688 *-----------------                                                EQW9ZWFZ
03689 *                                                                 EQW9ZWFZ
03690      MOVE COM-FB-IDENT-TSTECH    TO IDENT-TS-TECHNIQUE.           EQW9ZWFZ
03691      MOVE +1                     TO COM-FB-RANG-MAX-TSTECH.       EQW9ZWFZ
03692      MOVE COM-FB-RANG-MAX-TSTECH TO RANG-TS-TECH.                 EQW9ZWFZ
03693      EXEC CICS WRITEQ TS QUEUE  (IDENT-TS-TECHNIQUE)              EQW9ZWFZ
03694                          FROM   (TS-TECHNIQUE)                    EQW9ZWFZ
03695                          LENGTH (LENGTH  OF  TS-TECHNIQUE)        EQW9ZWFZ
03696                          ITEM   (RANG-TS-TECH)                    EQW9ZWFZ
03697                          MAIN                                     EQW9ZWFZ
03698                          NOHANDLE                                 EQW9ZWFZ
03699      END-EXEC.                                                    EQW9ZWFZ
03700                                                                   EQW9ZWFZ
03701      IF EIBRCODE  NOT = LOW-VALUE                                 EQW9ZWFZ
03702         MOVE 'FBET :PB ECRITURE TS TECHNIQUE' TO MESS             EQW9ZWFZ
03703         GO TO ABANDON-TACHE                                       EQW9ZWFZ
03704      END-IF.                                                      EQW9ZWFZ
03705 *                                                                 EQW9ZWFZ
03706  FECR-TS-TECHNIQUE.                                               EQW9ZWFZ
03707      EXIT.                                                        EQW9ZWFZ
03708 *                                                                 EQW9ZWFZ
03709 ***************************************************************** EQW9ZWFZ
03928 * CREATION DE LA TS VEHICULE EN CAS DE CHANGEMENT DE VEHICULE   * FB01T00B
03929 ***************************************************************** FB01T00B
03930  CREATION-TS-VEHICULE.                                            FB01T00B
03931 *                                                                 FB01T00B
03932      EXEC  CICS  WRITEQ TS  QUEUE(COM-FB-IDENT-TSVEHI)            FB01T00B
03933                             FROM(TS-VEHICULE)                     FB01T00B
03934                             LENGTH (LENGTH OF TS-VEHICULE)        FB01T00B
03935                             ITEM(RANG-TS-VEHI)                    FB01T00B
03936                             NOHANDLE                              FB01T00B
03937      END-EXEC.                                                    FB01T00B
03938      IF EIBRCODE NOT = LOW-VALUE                                  FB01T00B
03939         MOVE 'FBCV :PB CREATION TS VEHICULE ' TO MESS             FB01T00B
03940         GO TO ABANDON-TACHE                                       FB01T00B
03941      END-IF.                                                      FB01T00B
03942 *                                                                 FB01T00B
03943  FCREATION-TS-VEHICULE.                                           FB01T00B
03944      EXIT.                                                        FB01T00B
03945 *                                                                 FB01T00B
03946 ***************************************************************** FB01T00B
03710 * ECRITURE DE LA TS VEHICULE                                    * EQW9ZWFZ
03711 ***************************************************************** EQW9ZWFZ
03712  ECR-TS-VEHICULE.                                                 EQW9ZWFZ
03713 *                                                                 EQW9ZWFZ
03714      EXEC  CICS  WRITEQ TS  QUEUE(COM-FB-IDENT-TSVEHI)            EQW9ZWFZ
03715                             FROM(TS-VEHICULE)                     EQW9ZWFZ
03716                             LENGTH (LENGTH OF TS-VEHICULE)        EQW9ZWFZ
03717                             ITEM(RANG-TS-VEHI)                    EQW9ZWFZ
03718                             REWRITE                               EQW9ZWFZ
03719                             NOHANDLE                              EQW9ZWFZ
03720      END-EXEC.                                                    EQW9ZWFZ
03721      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03722         MOVE 'FB01 :PB ECRITURE TS VEHICULE ' TO MESS             EQW9ZWFZ
03723         GO TO ABANDON-TACHE                                       EQW9ZWFZ
03724      END-IF.                                                      EQW9ZWFZ
03725 *                                                                 EQW9ZWFZ
03726  FECR-TS-VEHICULE.                                                EQW9ZWFZ
03727      EXIT.                                                        EQW9ZWFZ
03728 ***************************************************************** EQW9ZWFZ
03710 * ECRITURE DE LA TS VEHICULE CHANGER                            * EQW9ZWFZ
03711 ***************************************************************** EQW9ZWFZ
03712  ECR-TS-VEHI-CHANGER.                                             EQW9ZWFZ
03713 *                                                                 EQW9ZWFZ
03714      EXEC  CICS  WRITEQ TS  QUEUE(COM-FB-IDENT-TSVEHI)            EQW9ZWFZ
03715                             FROM(TS-VEHICULE-CHANGER)             EQW9ZWFZ
03716                             LENGTH (LENGTH OF TS-VEHICULE-CHANGER)EQW9ZWFZ
03717                             ITEM(WSS-RANG-TS-CHANGER)             EQW9ZWFZ
03718                             REWRITE                               EQW9ZWFZ
03719                             NOHANDLE                              EQW9ZWFZ
03720      END-EXEC.                                                    EQW9ZWFZ
03721      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03722         MOVE 'FBC1 :PB ECRITURE TS VEHICULE CHANGER' TO MESS      EQW9ZWFZ
03723         GO TO ABANDON-TACHE                                       EQW9ZWFZ
03724      END-IF.                                                      EQW9ZWFZ
03725 *                                                                 EQW9ZWFZ
03726  FECR-TS-VEHI-CHANGER.                                            EQW9ZWFZ
03727      EXIT.                                                        EQW9ZWFZ
03728 ***************************************************************** EQW9ZWFZ
03729 * ECRITURE DE LA TS PERSONNE                                    * EQW9ZWFZ
03730 ***************************************************************** EQW9ZWFZ
03731  ECR-TS-PERSONNE.                                                 EQW9ZWFZ
03732 *                                                                 EQW9ZWFZ
03733      EXEC  CICS  WRITEQ TS  QUEUE(COM-FB-IDENT-TSPERS)            EQW9ZWFZ
03734                             FROM(TS-PERSONNE)                     EQW9ZWFZ
03735                             LENGTH (LENGTH OF TS-PERSONNE)        EQW9ZWFZ
03736                             ITEM(RANG-TS-PERS)                    EQW9ZWFZ
03737                             REWRITE                               EQW9ZWFZ
03738                             NOHANDLE                              EQW9ZWFZ
03739      END-EXEC.                                                    EQW9ZWFZ
03740      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03741         MOVE 'FB01 :PB ECRITURE TS PERSONNE ' TO MESS             EQW9ZWFZ
03742         GO TO ABANDON-TACHE                                       EQW9ZWFZ
03743      END-IF.                                                      EQW9ZWFZ
03744 *                                                                 EQW9ZWFZ
03745  FECR-TS-PERSONNE.                                                EQW9ZWFZ
03746      EXIT.                                                        EQW9ZWFZ
03747 ******************************************************************EQW9ZWFZ
03748 *   LECTURE DE LA TS TECHNIQUE                                   *EQW9ZWFZ
03749 ******************************************************************EQW9ZWFZ
03750 *                                                                 EQW9ZWFZ
03751  READ-TS-TECHNIQUE.                                               EQW9ZWFZ
03752 *------------------                                               EQW9ZWFZ
03753 *** LECTURE RANG 1 ***                                            EQW9ZWFZ
03754      MOVE +1 TO RANG-TS-TECH.                                     EQW9ZWFZ
03755      EXEC CICS READQ TS QUEUE   (COM-FB-IDENT-TSTECH)             EQW9ZWFZ
03756                         INTO    (TS-TECHNIQUE)                    EQW9ZWFZ
03757                         LENGTH  (LENGTH OF TS-TECHNIQUE)          EQW9ZWFZ
03758                         ITEM    (RANG-TS-TECH)                    EQW9ZWFZ
03759                         NOHANDLE                                  EQW9ZWFZ
03760      END-EXEC.                                                    EQW9ZWFZ
03761      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03762         MOVE 'TER1 : ERREUR READ TS TECHNIQUE' TO MESS            EQW9ZWFZ
03763         GO TO ABANDON-TACHE                                       EQW9ZWFZ
03764      END-IF.                                                      EQW9ZWFZ
03765 *                                                                 EQW9ZWFZ
03766  FIN-READ-TS-TECHNIQUE.  EXIT.                                    EQW9ZWFZ
03767 *                                                                 EQW9ZWFZ
03768 ******************************************************************EQW9ZWFZ
03769 *   LECTURE DE LA TS PERSONNE                                    *EQW9ZWFZ
03770 ******************************************************************EQW9ZWFZ
03771 *                                                                 EQW9ZWFZ
03772  READ-TS-PERSONNE.                                                EQW9ZWFZ
03773 *-----------------                                                EQW9ZWFZ
03774      EXEC CICS READQ TS QUEUE   (COM-FB-IDENT-TSPERS)             EQW9ZWFZ
03775                         INTO    (TS-PERSONNE)                     EQW9ZWFZ
03776                         LENGTH  (LENGTH OF TS-PERSONNE)           EQW9ZWFZ
03777                         ITEM    (RANG-TS-PERS)                    EQW9ZWFZ
03778                         NOHANDLE                                  EQW9ZWFZ
03779      END-EXEC.                                                    EQW9ZWFZ
03780      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03781         IF RANG-TS-PERS > COM-FB-RANG-MAX-TSPERS                  EQW9ZWFZ
03782            MOVE 'O'        TO WSS-FIN-PERS                        EQW9ZWFZ
03783         ELSE                                                      EQW9ZWFZ
03784            MOVE 'PER1 : ERREUR READ TS PERSONNE' TO MESS          EQW9ZWFZ
03785            GO TO ABANDON-TACHE                                    EQW9ZWFZ
03786         END-IF                                                    EQW9ZWFZ
03787      END-IF.                                                      EQW9ZWFZ
03788 *                                                                 EQW9ZWFZ
03789  FREAD-TS-PERSONNE.                                               EQW9ZWFZ
03790      EXIT.                                                        EQW9ZWFZ
03791 ******************************************************************EQW9ZWFZ
03792 *   LECTURE DE LA TS PERSONNE   ET SELECTION DES ENCOURS         *EQW9ZWFZ
03793 *   POUR L'AFFICHAGE                                             *EQW9ZWFZ
03794 ******************************************************************EQW9ZWFZ
03795 *                                                                 EQW9ZWFZ
03796  READ-TS-PERSONNE-ENC.                                            EQW9ZWFZ
03797 *---------------------*                                           EQW9ZWFZ
03798      ADD 1       TO RANG-TS-PERS.                                 EQW9ZWFZ
03799      EXEC CICS READQ TS QUEUE   (COM-FB-IDENT-TSPERS)             EQW9ZWFZ
03800                         INTO    (TS-PERSONNE)                     EQW9ZWFZ
03801                         LENGTH  (LENGTH OF TS-PERSONNE)           EQW9ZWFZ
03802                         ITEM    (RANG-TS-PERS)                    EQW9ZWFZ
03803                         NOHANDLE                                  EQW9ZWFZ
03804      END-EXEC.                                                    EQW9ZWFZ
03805      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03806         IF RANG-TS-PERS > COM-FB-RANG-MAX-TSPERS                  EQW9ZWFZ
03807            MOVE 'O' TO WSS-FIN-PERS                               EQW9ZWFZ
03808            IF COM-FB-NBRE-PERS-ENC > ZERO                         EQW9ZWFZ
03809            AND (COM-FB-NBRE-VEHI-ENC > ZERO                       EQW9ZWFZ
03810               OR COM-FB-NBRE-PERS-SOR > ZERO                      EQW9ZWFZ
03811               OR COM-FB-NBRE-VEHI-SOR > ZERO)                     EQW9ZWFZ
03812            PERFORM CREAT-SEPARATION-AFF THRU FCREAT-SEPARATION-AFFEQW9ZWFZ
03813            END-IF                                                 EQW9ZWFZ
03814         ELSE                                                      EQW9ZWFZ
03815            MOVE 'PER1 : ERREUR READ TS PERSONNE' TO MESS          EQW9ZWFZ
03816            GO TO ABANDON-TACHE                                    EQW9ZWFZ
03817         END-IF                                                    EQW9ZWFZ
03818      ELSE                                                         EQW9ZWFZ
03819         IF RANG-TS-PERS > COM-FB-RANG-MAX-TSPERS                  EQW9ZWFZ
03820            MOVE 'O' TO WSS-FIN-PERS                               EQW9ZWFZ
03821            IF COM-FB-NBRE-PERS-ENC > ZERO                         EQW9ZWFZ
03822            AND (COM-FB-NBRE-VEHI-ENC > ZERO                       EQW9ZWFZ
03823               OR COM-FB-NBRE-PERS-SOR > ZERO                      EQW9ZWFZ
03824               OR COM-FB-NBRE-VEHI-SOR > ZERO)                     EQW9ZWFZ
03825            PERFORM CREAT-SEPARATION-AFF THRU FCREAT-SEPARATION-AFFEQW9ZWFZ
03826            END-IF                                                 EQW9ZWFZ
03827         ELSE                                                      EQW9ZWFZ
03828            IF RPERSORD OF DONNEES-PERSONNE (1) = '99999999'       EQW9ZWFZ
03829               PERFORM CALCUL-COMPTEUR-TYPE-PERS                   EQW9ZWFZ
03830                 THRU FCALCUL-COMPTEUR-TYPE-PERS                   EQW9ZWFZ
03831             PERFORM CHARG-TAB-AFF-PERS THRU FIN-CHARG-TAB-AFF-PERSEQW9ZWFZ
03832            END-IF                                                 EQW9ZWFZ
03833         END-IF                                                    EQW9ZWFZ
03834      END-IF.                                                      EQW9ZWFZ
03835 *                                                                 EQW9ZWFZ
03836  FREAD-TS-PERSONNE-ENC.                                           EQW9ZWFZ
03837      EXIT.                                                        EQW9ZWFZ
03838 ******************************************************************EQW9ZWFZ
03839 *   LECTURE DE LA TS PERSONNE   ET SELECTION DES SORTIE          *EQW9ZWFZ
03840 *   POUR L'AFFICHAGE                                             *EQW9ZWFZ
03841 ******************************************************************EQW9ZWFZ
03842 *                                                                 EQW9ZWFZ
03843  READ-TS-PERSONNE-SOR.                                            EQW9ZWFZ
03844 *---------------------                                            EQW9ZWFZ
03845      ADD 1       TO RANG-TS-PERS.                                 EQW9ZWFZ
03846      EXEC CICS READQ TS QUEUE   (COM-FB-IDENT-TSPERS)             EQW9ZWFZ
03847                         INTO    (TS-PERSONNE)                     EQW9ZWFZ
03848                         LENGTH  (LENGTH OF TS-PERSONNE)           EQW9ZWFZ
03849                         ITEM    (RANG-TS-PERS)                    EQW9ZWFZ
03850                         NOHANDLE                                  EQW9ZWFZ
03851      END-EXEC.                                                    EQW9ZWFZ
03852      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03853         IF RANG-TS-PERS > COM-FB-RANG-MAX-TSPERS                  EQW9ZWFZ
03854            MOVE 'O' TO WSS-FIN-PERS                               EQW9ZWFZ
03855            IF COM-FB-NBRE-PERS-SOR > ZERO                         EQW9ZWFZ
03856            AND COM-FB-NBRE-VEHI-SOR > ZERO                        EQW9ZWFZ
03857            PERFORM CREAT-SEPARATION-AFF THRU FCREAT-SEPARATION-AFFEQW9ZWFZ
03858            END-IF                                                 EQW9ZWFZ
03859         ELSE                                                      EQW9ZWFZ
03860            MOVE 'PER1 : ERREUR READ TS PERSONNE' TO MESS          EQW9ZWFZ
03861            GO TO ABANDON-TACHE                                    EQW9ZWFZ
03862         END-IF                                                    EQW9ZWFZ
03863      ELSE                                                         EQW9ZWFZ
03864         IF RANG-TS-PERS > COM-FB-RANG-MAX-TSPERS                  EQW9ZWFZ
03865            MOVE 'O' TO WSS-FIN-PERS                               EQW9ZWFZ
03866            IF COM-FB-NBRE-PERS-SOR > ZERO                         EQW9ZWFZ
03867            AND COM-FB-NBRE-VEHI-SOR > ZERO                        EQW9ZWFZ
03868            PERFORM CREAT-SEPARATION-AFF THRU FCREAT-SEPARATION-AFFEQW9ZWFZ
03869            END-IF                                                 EQW9ZWFZ
03870         ELSE                                                      EQW9ZWFZ
03871          IF RPERSORD OF DONNEES-PERSONNE(1) NOT = '99999999'      EQW9ZWFZ
03872             PERFORM CHARG-TAB-AFF-PERS THRU FIN-CHARG-TAB-AFF-PERSEQW9ZWFZ
03873          END-IF                                                   EQW9ZWFZ
03874         END-IF                                                    EQW9ZWFZ
03875      END-IF.                                                      EQW9ZWFZ
03876 *                                                                 EQW9ZWFZ
03877  FREAD-TS-PERSONNE-SOR.                                           EQW9ZWFZ
03878      EXIT.                                                        EQW9ZWFZ
03879 *                                                                 EQW9ZWFZ
03880 ******************************************************************EQW9ZWFZ
03881 *   LECTURE DE LA TS VEHICULE                                    *EQW9ZWFZ
03882 ******************************************************************EQW9ZWFZ
03883 *                                                                 EQW9ZWFZ
03884  READ-TS-VEHICULE.                                                EQW9ZWFZ
03885 *-----------------                                                EQW9ZWFZ
03886      EXEC CICS READQ TS QUEUE   (COM-FB-IDENT-TSVEHI)             EQW9ZWFZ
03887                         INTO    (TS-VEHICULE)                     EQW9ZWFZ
03888                         LENGTH  (LENGTH OF TS-VEHICULE)           EQW9ZWFZ
03889                         ITEM    (RANG-TS-VEHI)                    EQW9ZWFZ
03890                         NOHANDLE                                  EQW9ZWFZ
03891      END-EXEC.                                                    EQW9ZWFZ
03892      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03893         IF RANG-TS-VEHI > COM-FB-RANG-MAX-TSVEHI                  EQW9ZWFZ
03894            MOVE 'O'        TO WSS-FIN-VEHI                        EQW9ZWFZ
03895         ELSE                                                      EQW9ZWFZ
03896            MOVE 'VEH1 : ERREUR READ TS VEHICULE' TO MESS          EQW9ZWFZ
03897            GO TO ABANDON-TACHE                                    EQW9ZWFZ
03898         END-IF                                                    EQW9ZWFZ
03899      END-IF.                                                      EQW9ZWFZ
03900 *                                                                 EQW9ZWFZ
03901  FREAD-TS-VEHICULE.                                               EQW9ZWFZ
03902      EXIT.                                                        EQW9ZWFZ
03903 ******************************************************************EQW9ZWFZ
03881 *   LECTURE DE LA TS VEHICULE CHANGER                            *EQW9ZWFZ
03882 ******************************************************************EQW9ZWFZ
03883 *                                                                 EQW9ZWFZ
03884  READ-TS-VEHI-CHANGER.                                            EQW9ZWFZ
03885 *---------------------                                            EQW9ZWFZ
           ADD 1 TO WSS-RANG-TS-CHANGER.

03886      EXEC CICS READQ TS QUEUE   (COM-FB-IDENT-TSVEHI)             EQW9ZWFZ
03887                         INTO    (TS-VEHICULE-CHANGER)             EQW9ZWFZ
03888                         LENGTH  (LENGTH OF TS-VEHICULE-CHANGER)   EQW9ZWFZ
03889                         ITEM    (WSS-RANG-TS-CHANGER)             EQW9ZWFZ
03890                         NOHANDLE                                  EQW9ZWFZ
03891      END-EXEC.                                                    EQW9ZWFZ
03892      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03893         IF WSS-RANG-TS-CHANGER > COM-FB-RANG-MAX-TSVEHI           EQW9ZWFZ
03894            MOVE 'O'        TO WSS-FIN-VEHI-CHG                    EQW9ZWFZ
03895         ELSE                                                      EQW9ZWFZ
03896            MOVE 'VECH1 : ERREUR READ TS VEHICULE CHANGER' TO MESS EQW9ZWFZ
03897            GO TO ABANDON-TACHE                                    EQW9ZWFZ
03898         END-IF                                                    EQW9ZWFZ
03899      END-IF.                                                      EQW9ZWFZ
03900 *                                                                 EQW9ZWFZ
03901  FREAD-TS-VEHI-CHANGER.                                           EQW9ZWFZ
03902      EXIT.                                                        EQW9ZWFZ
03903 ******************************************************************EQW9ZWFZ
03904 *   LECTURE DE LA TS VEHICULE EN SELECTIONNANT LES ENCOURS       *EQW9ZWFZ
03905 *   POUR L'AFFICHAGE                                             *EQW9ZWFZ
03906 ******************************************************************EQW9ZWFZ
03907 *                                                                 EQW9ZWFZ
03908  READ-TS-VEHICULE-ENC.                                            EQW9ZWFZ
03909 *--------------------                                             EQW9ZWFZ
03910      ADD 1       TO RANG-TS-VEHI.                                 EQW9ZWFZ
03911      EXEC CICS READQ TS QUEUE   (COM-FB-IDENT-TSVEHI)             EQW9ZWFZ
03912                         INTO    (TS-VEHICULE)                     EQW9ZWFZ
03913                         LENGTH  (COM-FB-LONG-TSVEHI)              EQW9ZWFZ
03914                         ITEM    (RANG-TS-VEHI)                    EQW9ZWFZ
03915                         NOHANDLE                                  EQW9ZWFZ
03916      END-EXEC.                                                    EQW9ZWFZ
03917      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03918         IF RANG-TS-VEHI > COM-FB-RANG-MAX-TSVEHI                  EQW9ZWFZ
03919            MOVE 'O' TO WSS-FIN-VEHI                               EQW9ZWFZ
03920            IF COM-FB-NBRE-VEHI-ENC > ZERO                         EQW9ZWFZ
03921            AND (COM-FB-NBRE-PERS-SOR > ZERO                       EQW9ZWFZ
03922               OR COM-FB-NBRE-VEHI-SOR > ZERO)                     EQW9ZWFZ
03923            PERFORM CREAT-SEPARATION-AFF THRU FCREAT-SEPARATION-AFFEQW9ZWFZ
03924            END-IF                                                 EQW9ZWFZ
03925         ELSE                                                      EQW9ZWFZ
03926            MOVE 'VER1 : ERREUR READ TS VEHICULE' TO MESS          EQW9ZWFZ
03927            GO TO ABANDON-TACHE                                    EQW9ZWFZ
03928         END-IF                                                    EQW9ZWFZ
03929      ELSE                                                         EQW9ZWFZ
03930         IF RANG-TS-VEHI > COM-FB-RANG-MAX-TSVEHI                  EQW9ZWFZ
03931            MOVE 'O' TO WSS-FIN-VEHI                               EQW9ZWFZ
03932            IF COM-FB-NBRE-VEHI-ENC > ZERO                         EQW9ZWFZ
03933            AND (COM-FB-NBRE-PERS-SOR > ZERO                       EQW9ZWFZ
03934               OR COM-FB-NBRE-VEHI-SOR > ZERO)                     EQW9ZWFZ
03935            PERFORM CREAT-SEPARATION-AFF THRU FCREAT-SEPARATION-AFFEQW9ZWFZ
03936            END-IF                                                 EQW9ZWFZ
03937         ELSE                                                      EQW9ZWFZ
03938            IF RVEHSORD OF TS-VEHICULE (1) = '99999999'            EQW9ZWFZ
03939             PERFORM CHARG-TAB-AFF-VEHI THRU FIN-CHARG-TAB-AFF-VEHIEQW9ZWFZ
03940            END-IF                                                 EQW9ZWFZ
03941         END-IF                                                    EQW9ZWFZ
03942      END-IF.                                                      EQW9ZWFZ
03943 *                                                                 EQW9ZWFZ
03944  FREAD-TS-VEHICULE-ENC.                                           EQW9ZWFZ
03945      EXIT.                                                        EQW9ZWFZ
03946 *                                                                 EQW9ZWFZ
03947 ******************************************************************EQW9ZWFZ
03948 *   LECTURE DE LA TS VEHICULE EN SELECTIONNANT LES SORTIES       *EQW9ZWFZ
03949 *   POUR L'AFFICHAGE                                             *EQW9ZWFZ
03950 ******************************************************************EQW9ZWFZ
03951 *                                                                 EQW9ZWFZ
03952  READ-TS-VEHICULE-SOR.                                            EQW9ZWFZ
03953 *--------------------                                             EQW9ZWFZ
03954      ADD 1       TO RANG-TS-VEHI.                                 EQW9ZWFZ
03955      EXEC CICS READQ TS QUEUE   (COM-FB-IDENT-TSVEHI)             EQW9ZWFZ
03956                         INTO    (TS-VEHICULE)                     EQW9ZWFZ
03957                         LENGTH  (LENGTH OF TS-VEHICULE)           EQW9ZWFZ
03958                         ITEM    (RANG-TS-VEHI)                    EQW9ZWFZ
03959                         NOHANDLE                                  EQW9ZWFZ
03960      END-EXEC.                                                    EQW9ZWFZ
03961      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
03962         IF RANG-TS-VEHI > COM-FB-RANG-MAX-TSVEHI                  EQW9ZWFZ
03963            MOVE 'O' TO WSS-FIN-VEHI                               EQW9ZWFZ
03964         ELSE                                                      EQW9ZWFZ
03965            MOVE 'VER1 : ERREUR READ TS VEHICULE' TO MESS          EQW9ZWFZ
03966            GO TO ABANDON-TACHE                                    EQW9ZWFZ
03967         END-IF                                                    EQW9ZWFZ
03968      ELSE                                                         EQW9ZWFZ
03969         IF RANG-TS-VEHI > COM-FB-RANG-MAX-TSVEHI                  EQW9ZWFZ
03970            MOVE 'O' TO WSS-FIN-VEHI                               EQW9ZWFZ
03971         ELSE                                                      EQW9ZWFZ
03972            IF RVEHSORD OF TS-VEHICULE (1) NOT = '99999999'        EQW9ZWFZ
03973             PERFORM CHARG-TAB-AFF-VEHI THRU FIN-CHARG-TAB-AFF-VEHIEQW9ZWFZ
03974            END-IF                                                 EQW9ZWFZ
03975         END-IF                                                    EQW9ZWFZ
03976      END-IF.                                                      EQW9ZWFZ
03977 *                                                                 EQW9ZWFZ
03978  FREAD-TS-VEHICULE-SOR.                                           EQW9ZWFZ
03979      EXIT.                                                        EQW9ZWFZ
03980 *                                                                 EQW9ZWFZ
03981 * CALCUL DES COMPTEURS DE TYPE DE PERSONNE                        EQW9ZWFZ
03982 *                                                                 EQW9ZWFZ
03983  CALCUL-COMPTEUR-TYPE-PERS.                                       EQW9ZWFZ
03984 *                                                                 EQW9ZWFZ
03985 * 1ER CAS : LE STATUT A DEJA ETE ECRIT EN TS MAIS L'UTILISATEUR   EQW9ZWFZ
03986 *           SAISIT UN STATUT DIFFERENT (EN AJOUT OU EN MODIF)     EQW9ZWFZ
03987 * 2E CAS : LE STATUT N'A PAS ETE ECRIT EN TS ET L'UTILISATEUR     EQW9ZWFZ
03988 *          SAISIT UN STATUT (EN AJOUT)                            EQW9ZWFZ
03989                                                                   EQW9ZWFZ
03990      IF PERSTAC OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW9ZWFZ
03991            EVALUATE PERSTAC OF TS-PERSONNE(1)                     EQW9ZWFZ
03992                 WHEN 'PM'                                         EQW9ZWFZ
03993                     ADD 1 TO COM-FB-NBRE-PERS-PM                  EQW9ZWFZ
03994                 WHEN 'CF'                                         EQW9ZWFZ
03995                     ADD 1 TO COM-FB-NBRE-PERS-CF                  EQW9ZWFZ
03996                 WHEN 'CJ'                                         EQW9ZWFZ
03997                     ADD 1 TO COM-FB-NBRE-PERS-CJ                  EQW9ZWFZ
03998                 WHEN 'EN'                                         EQW9ZWFZ
03999                     ADD 1 TO COM-FB-NBRE-PERS-EN                  EQW9ZWFZ
04000            END-EVALUATE                                           EQW9ZWFZ
04001      END-IF.                                                      EQW9ZWFZ
04002 *                                                                 EQW9ZWFZ
04003  FCALCUL-COMPTEUR-TYPE-PERS.    EXIT.                             EQW9ZWFZ
04004 ***************************************************************** EQW9ZWFZ
04005 * DELETE       DE LA TS DE CONFIDENTIALITE CONVERSATION           EQW9ZWFZ
04006 ***************************************************************** EQW9ZWFZ
04007  DELETE-TS-CONF-CONV.                                             EQW9ZWFZ
04008 *-------------------*                                             EQW9ZWFZ
04009      MOVE IDENT-TS-CONF TO   IDENT-TS.                            EQW9ZWFZ
04010      PERFORM DELETE-TS  THRU FIN-DELETE-TS.                       EQW9ZWFZ
04011 *                                                                 EQW9ZWFZ
DELTS  ++INCLUDE MAIDELTS
DELTS *
04012  FIN-DELETE-TS-CONF-CONV.                                         EQW9ZWFZ
04013      EXIT.                                                        EQW9ZWFZ
04014 *                                                                 EQW9ZWFZ
04015 *          DELETE  DE LA TS 'PLAN'                                EQW9ZWFZ
04016  ++INCLUDE SQKCPLDE                                               EQW9ZWFZ
04017 *                                                                 EQW9ZWFZ
04018 ***************************************************************** EQW9ZWFZ
04019 *  APPEL DES ORDRES CICS LES PLUS USITES                        * EQW9ZWFZ
04020 ***************************************************************** EQW9ZWFZ
04021 *                                                                 EQW9ZWFZ
04022 ****************************************************************  EQW9ZWFZ
04023 * RETOUR AVEC COMMAREA                                            EQW9ZWFZ
04024 ***************************************************************** EQW9ZWFZ
04025 *                                                                 EQW9ZWFZ
04026  ++INCLUDE SQKCRTCO                                               EQW9ZWFZ
04027 *                                                                 EQW9ZWFZ
04028 ****************************************************************  EQW9ZWFZ
04029 * RETOUR A CICS                                                   EQW9ZWFZ
04030 ***************************************************************** EQW9ZWFZ
04031 *                                                                 EQW9ZWFZ
04032  ++INCLUDE SQKCRTNO                                               EQW9ZWFZ
04033 ***************************************************************   EQW9ZWFZ
04034 * SEND MAP ERREUR                                                 EQW9ZWFZ
04035 ***************************************************************   EQW9ZWFZ
04036 *                                                                 EQW9ZWFZ
04037  ++INCLUDE SQKCSMER                                               EQW9ZWFZ
04038 *                                                                 EQW9ZWFZ
04039 ***************************************************************** EQW9ZWFZ
04040 * ENVOI MAP SIMPLE : SEND-MAP     ET   SEND-MAP-CURSOR            EQW9ZWFZ
04041 ***************************************************************** EQW9ZWFZ
04042 *                                                                 EQW9ZWFZ
04043  ++INCLUDE SQKCSM00                                               EQW9ZWFZ
04044 *                                                                 EQW9ZWFZ
04045 ****************************************************************  EQW9ZWFZ
04046 *  PASSAGE DU CONTROLE A UNE AUTRE TACHE                          EQW9ZWFZ
04047 ****************************************************************  EQW9ZWFZ
04048 *                                                                 EQW9ZWFZ
04049  ++INCLUDE SQKCSTRT                                               EQW9ZWFZ
04050 *                                                                 EQW9ZWFZ
04051 ***************************************************************** EQW9ZWFZ
04052 * PASSAGE DU CONTROL A UN NOUVEAU PROGRAMME                       EQW9ZWFZ
04053 ***************************************************************** EQW9ZWFZ
04054 *                                                                 EQW9ZWFZ
04055  ++INCLUDE SQKCXCTL                                               EQW9ZWFZ
04056 *                                                                 EQW9ZWFZ
04057 ***************************************************************** EQW9ZWFZ
04058 * RETRIEVE DES DATA EN PROVENANCE D'UN START                      EQW9ZWFZ
04059 ***************************************************************** EQW9ZWFZ
04060 *                                                                 EQW9ZWFZ
04061  ++INCLUDE SQKCRETR                                               EQW9ZWFZ
04062 *                                                                 EQW9ZWFZ
04063 ****************************************************************  EQW9ZWFZ
04064 *  PASSAGE DU CONTROLE A UN PROGRAMME DE LA MEME TACHE            EQW9ZWFZ
04065 ****************************************************************  EQW9ZWFZ
04066 *                                                                 EQW9ZWFZ
04067  ++INCLUDE SQKCLNKB                                               EQW9ZWFZ
04068 *                                                                 EQW9ZWFZ
04069 ***************************************************************** EQW9ZWFZ
04070 * CONSULTATION DE LA TEMPORARY STORAGE                            EQW9ZWFZ
04071 ***************************************************************** EQW9ZWFZ
04072 *                                                                 EQW9ZWFZ
04073  ++INCLUDE SQKCTSRD                                               EQW9ZWFZ
04074 *                                                                 EQW9ZWFZ
04075 ***************************************************************** EQW9ZWFZ
04076 * CONSULTATION DE LA TEMPORARY STORAGE                            EQW9ZWFZ
04077 ***************************************************************** EQW9ZWFZ
04078 *                                                                 EQW9ZWFZ
04079  ++INCLUDE SQKCTRDB                                               EQW9ZWFZ
04080 *                                                                 EQW9ZWFZ
04081  ++INCLUDE SQKCTSPL                                               EQW9ZWFZ
04082 ***************************************************************** EQW9ZWFZ
04083 * DELETE       DE LA TEMPORARY STORAGE                            EQW9ZWFZ
04084 ***************************************************************** EQW9ZWFZ
04085 *                                                                 EQW9ZWFZ
04086  ++INCLUDE SQKCTSDE                                               EQW9ZWFZ
04087 *                                                                 EQW9ZWFZ
04088 ***************************************************************** EQW9ZWFZ
04089 * ENVOI MAP SANS ERASE DATAONLY                                   EQW9ZWFZ
04090 ***************************************************************** EQW9ZWFZ
04091 *                                                                 EQW9ZWFZ
04092  ++INCLUDE SQKCSMDO                                               EQW9ZWFZ
04093 *                                                                 EQW9ZWFZ
04094 ***************************************************************** EQW9ZWFZ
04095 * SEND MAP ERREUR MDT OFF                                         EQW9ZWFZ
04096 ***************************************************************** EQW9ZWFZ
04097 *                                                                 EQW9ZWFZ
04098  ++INCLUDE SQKCSEDO                                               EQW9ZWFZ
04099 *                                                                 EQW9ZWFZ
04100 *                                                                 EQW9ZWFZ
04101 ***************************************************************** EQW9ZWFZ
04102 *   MODULES DE CONTROLE ET DE TRAITEMENT SPECIFIQUES            * EQW9ZWFZ
04103 ***************************************************************** EQW9ZWFZ
04104 *                                                                 EQW9ZWFZ
04105 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
04106 * APPEL DU MODULE DE DETERMINATION DES DONNEES FB90T11        *   EQW9ZWFZ
04107 * BUT : APPELER LE MODULE QUI VA ALLER CHERCHER EN TABLE OU VA  * EQW9ZWFZ
04108 * CALCULER LES COEFFICIENTS ET VALEURS NECESSAIRES A LA         * EQW9ZWFZ
04109 * TARIFICATION                                                  * EQW9ZWFZ
04110 * (N.B.: MODULE EGALEMENT APPELE DANS LA TARIFICATION)          * EQW9ZWFZ
04111 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9ZWFZ
04112 *                                                                 EQW9ZWFZ
04113  APPEL-MODULE-DET.                                                EQW9ZWFZ
04114                                                                   EQW9ZWFZ
04115      MOVE  SPACE                  TO FBI90C11.                    EQW9ZWFZ
04116      MOVE COM-GENE-TYPCICS        TO FB90C11-TYPCICS.             EQW9ZWFZ
04117      MOVE IDENT-TS-APP            TO FB90C11-ID-TS-SUSPENS.       EQW9ZWFZ
04118      MOVE IDENT-TS-CNTPROD        TO FB90C11-ID-TS-CONTRAT.       EQW9ZWFZ
04119      MOVE COM-FB-IDENT-TSCONT     TO FB90C11-IDENT-TSCNTPROD.     EQW9ZWFZ
04120      MOVE COM-FB-IDENT-TSPERS     TO FB90C11-IDENT-TSPERS.        EQW9ZWFZ
04121      MOVE COM-FB-IDENT-TSVEHI     TO FB90C11-IDENT-TSVEHI.        EQW9ZWFZ
04122      MOVE COM-FB-IDENT-TSTECH     TO FB90C11-IDENT-TSTECH.        EQW9ZWFZ
04123      MOVE COM-FB-NBRE-PERS-ENC    TO FB90C11-NBRE-PERS.           EQW9ZWFZ
04124      MOVE COM-FB-NBRE-VEHI-ENC    TO FB90C11-NBRE-VEHI.           EQW9ZWFZ
04125      MOVE COM-FB-NBRE-VEHI-TRACTEUR                               EQW9ZWFZ
04126                                   TO FB90C11-NBRE-VEHI-TRACTEUR.  EQW9ZWFZ
04127      MOVE COM-FB-NBRE-VEHI-SOR    TO FB90C11-NBRE-VEHI-SOR.       EQW9ZWFZ
04128      MOVE COM-FB-DATE-EFFET-1     TO FB90C11-EFFET-1.             EQW9ZWFZ
F37105     MOVE COM-FB-VALEUR-BUTOIR    TO FB90C11-VALEUR-BUTOIR.       EQW9ZWFZ
04129      IF NOM-CLI OF TS-SUSPENS1 NOT = SPACES AND LOW-VALUE         EQW9ZWFZ
04130         MOVE NOM-CLI OF TS-SUSPENS1                               EQW9ZWFZ
04131                                   TO FB90C11-NUMCLI               EQW9ZWFZ
04132      ELSE                                                         EQW9ZWFZ
04133         MOVE NOM-CLI OF TS-SUSPENS2                               EQW9ZWFZ
04134                                   TO FB90C11-NUMCLI               EQW9ZWFZ
04135      END-IF.                                                      EQW9ZWFZ
04136                                                                   EQW9ZWFZ
04137      EXEC CICS LINK PROGRAM  ('FB90T11')                          EQW9ZWFZ
04138                     COMMAREA (FBI90C11)                           EQW9ZWFZ
04139                     LENGTH   (LENGTH OF FBI90C11)                 EQW9ZWFZ
04140      END-EXEC.                                                    EQW9ZWFZ
04141                                                                   EQW9ZWFZ
04142      IF EIBRCODE NOT = LOW-VALUE                                  EQW9ZWFZ
04143         MOVE 'FB01.ERREUR LINK FB90T11' TO MESS                   EQW9ZWFZ
04144         GO TO ABANDON-TACHE                                       EQW9ZWFZ
04145      END-IF.                                                      EQW9ZWFZ
04146 *                                                                 EQW9ZWFZ
04147  FAPPEL-MODULE-DET. EXIT.                                         EQW9ZWFZ
04148 /                                                                 EQW9ZWFZ
04149 ******************************************************************EQW9ZWFZ
04150 * ACCES A L'INTERFACE AUAAL00 QUI CONSTRUIT                       EQW9ZWFZ
04151 * LA TS DE CONFIDENTIALITE DE LA CONVERSATION : COM-GENE-CODCNV   EQW9ZWFZ
04152 ******************************************************************EQW9ZWFZ
04153  INTERFACE-CONFIDENTIALITE.                                       EQW9ZWFZ
04154 *-------------------------*                                       EQW9ZWFZ
04155      MOVE SPACES                    TO COM-AU-AUAAC.              EQW9ZWFZ
04156      MOVE COM-GENE-CODCIE-PRINCIPAL TO COM-AU-CIE.                EQW9ZWFZ
04157      MOVE COM-GENE-CODSIT           TO COM-AU-SITE.               EQW9ZWFZ
04158      MOVE COM-GENE-CODCNV           TO COM-AU-CONVERS.            EQW9ZWFZ
04159      MOVE COM-GENE-CODUSR           TO COM-AU-USAGER.             EQW9ZWFZ
04160 *              COM-AU-SWAP   (1 : SWAP N∞1 / 2 : SWAP N∞2)        EQW9ZWFZ
04161      MOVE '1'                       TO COM-AU-SWAP.               EQW9ZWFZ
04162 *              COM-AU-TYPETS (M : MENU / C : CONVERSATION)        EQW9ZWFZ
04163      MOVE 'C'                       TO COM-AU-TYPETS.             EQW9ZWFZ
04164 *                                                                 EQW9ZWFZ
04165      EXEC CICS LINK PROGRAM  ('AUAAL00')                          EQW9ZWFZ
04166                     COMMAREA (COM-AU-AUAAC)                       EQW9ZWFZ
04167                     LENGTH   (COM-AU-LONG-AUAAC)                  EQW9ZWFZ
04168      END-EXEC.                                                    EQW9ZWFZ
04169 *                                                                 EQW9ZWFZ
04170      MOVE COM-AU-LONG-TS          TO COM-GENE-LNGCNV.             EQW9ZWFZ
04171      MOVE COM-AU-MESSAGE          TO COM-GENE-MESANO.             EQW9ZWFZ
04172  FIN-INTERFACE-CONFIDENTIALITE. EXIT.                             EQW9ZWFZ
04173 *                                                                 EQW9ZWFZ
04174 ******************************************************************EQW9ZWFZ
04175 * ACCES A L'INTERFACE AUAAL04 QUI REFAIT  UN CONTROLE D'ACCES     EQW9ZWFZ
04176 * (MEME CONTROLE QU'AU NIVEAU  MENU )                             EQW9ZWFZ
04177 ******************************************************************EQW9ZWFZ
04178  INTERFACE-CONTROLE-ACCES.                                        EQW9ZWFZ
04179 *------------------------*                                        EQW9ZWFZ
04180      MOVE SPACES                    TO COM-AU-AUAAC.              EQW9ZWFZ
04181      MOVE COM-GENE-CODCIE-PRINCIPAL TO COM-AU-CIE.                EQW9ZWFZ
04182      MOVE COM-GENE-CODSIT           TO COM-AU-SITE.               EQW9ZWFZ
04183      MOVE COM-GENE-CODCNV           TO COM-AU-CONVERS.            EQW9ZWFZ
04184      MOVE COM-GENE-CODUSR           TO COM-AU-USAGER.             EQW9ZWFZ
04185 *              COM-AU-SWAP   (1 : SWAP N∞1 / 2 : SWAP N∞2)        EQW9ZWFZ
04186      MOVE '1'                       TO COM-AU-SWAP.               EQW9ZWFZ
04187 *                                                                 EQW9ZWFZ
04188      EXEC CICS LINK PROGRAM  ('AUAAL04')                          EQW9ZWFZ
04189                     COMMAREA (COM-AU-AUAAC)                       EQW9ZWFZ
04190                     LENGTH   (COM-AU-LONG-AUAAC)                  EQW9ZWFZ
04191      END-EXEC.                                                    EQW9ZWFZ
04192  FIN-INTERFACE-CONTROLE-ACCES.  EXIT.                             EQW9ZWFZ
04193 /                                                                 EQW9ZWFZ
04194 ***************************************************************** EQW9ZWFZ
04195 *   LECTURE DES MESSAGES D'INFORMATION ET D'ANOMALIE            * EQW9ZWFZ
04196 ***************************************************************** EQW9ZWFZ
04197  LECTURE-ERREUR.                                                  EQW9ZWFZ
04198 *--------------*                                                  EQW9ZWFZ
04199      MOVE  SPACES                 TO XSPIPARM.                    EQW9ZWFZ
04200      IF    COM-GENE-MESINF NOT = SPACES AND LOW-VALUE             EQW9ZWFZ
04201            MOVE COM-GENE-MESINF   TO W-CODERR                     EQW9ZWFZ
04202            MOVE '*CD'             TO EL-DEMANDES OF XSPIPARM      EQW9ZWFZ
04203      ELSE                                                         EQW9ZWFZ
04204            MOVE COM-GENE-MESANO   TO W-CODERR                     EQW9ZWFZ
04205      END-IF.                                                      EQW9ZWFZ
04206      MOVE  'GP'                   TO FONCTION  OF XSPIPARM.       EQW9ZWFZ
04207      MOVE  'MSGETUDE'             TO CODTAB    OF XSPIPARM.       EQW9ZWFZ
04208      MOVE  '= '                   TO OPERATEUR OF XSPIPARM.       EQW9ZWFZ
04209      MOVE   W-CODERR              TO REF-POSTE OF XSPIPARM.       EQW9ZWFZ
04210      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9ZWFZ
04211      IF  RETCOD OF XSPIPARM  = ZERO                               EQW9ZWFZ
04212          MOVE 0       TO CODE-RETOUR                              EQW9ZWFZ
04213          MOVE IOAREA  OF XSPIPARM TO W-ERREUR                     EQW9ZWFZ
04214          IF   COM-GENE-MESINF = SPACES OR LOW-VALUE               EQW9ZWFZ
04215               MOVE SPACES  TO W-CODERR                            EQW9ZWFZ
04216          END-IF                                                   EQW9ZWFZ
04217          MOVE SPACES  TO W-SUFERR                                 EQW9ZWFZ
04218      ELSE                                                         EQW9ZWFZ
04219          MOVE SPACES  TO W-LIBERR                                 EQW9ZWFZ
04220                          W-SUFERR                                 EQW9ZWFZ
04221          MOVE 1       TO CODE-RETOUR                              EQW9ZWFZ
04222      END-IF.                                                      EQW9ZWFZ
04223  FIN-LECTURE-ERREUR.  EXIT.                                       EQW9ZWFZ
04224 *                                                                 EQW9ZWFZ
04225 ***************************************************************** EQW9ZWFZ
04226 * ACCES SPITAB                                                    EQW9ZWFZ
04227 ***************************************************************** EQW9ZWFZ
04228  ++INCLUDE SQKCSPI2                                               EQW9ZWFZ
04229 ***************************************************************** EQW9ZWFZ
04230 * SORTIE ABANDON POUR ERREURS    NON PREVUES                      EQW9ZWFZ
04231 ***************************************************************** EQW9ZWFZ
04232  ABANDON-TACHE.                                                   EQW9ZWFZ
04233  ++INCLUDE SQKCMROB                                               EQW9ZWFZ

U3319  ++INCLUDE SQKCCON2                                               EFUTSV6O
04234 ** FIN DE PROGRAMME  FB01T00  CREE LE  01/02/02  A  13:58  .      EQW9ZWFZ
