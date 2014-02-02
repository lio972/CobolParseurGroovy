00001  IDENTIFICATION DIVISION.                                         13/02/02
00002  PROGRAM-ID.                     FB02T00.                         EQW8LBVR
00003 * MODULE EN COURS DE MAINTENANCE PAR   : ...................... !    LV001
00004 * PREVU POUR ETRE MIS EN PRODUCTION LE : ../../..               ! EQW8LBVR
00005 * N.B. : ...................................................... ! EQW8LBVR
00006 *-02-11-93------------------------------------------------------+ EQW8LBVR
00007 * NOM DU LOAD MODULE : FB02T00                CREE LE 00/09/19    EQW8LBVR
00008 * NOM DE TRANSACTION : FB02                        A 10:18        EQW8LBVR
00009 * NOM DE MAP         : FB02M0Z                                    EQW8LBVR
00010 * NOM DU PGM BATCH   : ........ (SI MODULE COMMUN TP/BATCH)       EQW8LBVR
00011 * AUTEUR             : FABRICE KELLER                             EQW8LBVR
00012 * LOGON TSO          : EXT17                                      EQW8LBVR
00013 *                                                                 EQW8LBVR
00014 *                      REFONTE AUTOMOBILE                         EQW8LBVR
00015 *                      COORDONNEES DU CONDUCTEUR                  EQW8LBVR
00016 *                                                                 EQW8LBVR
00017 * TITRE              : FB02T00                                    EQW8LBVR
00018 *---------------------------------------------------------------+ EQW8LBVR
00019 * CNC$PROG MAINT     : TK    090990 AJOUT TRAITEMENT TS PLAN      EQW8LBVR
00020 *                    :              POUR  EXIT DE SELECTION DYNA  EQW8LBVR
00021 *   09/09/90         :              MIQUE DE PLAN                 EQW8LBVR
00022 *                    : POUR TOUTE   CONVERSATION (SAUF MENU)      EQW8LBVR
00023 *---------------------------------------------------------------+ EQW8LBVR
00024 * CNC$PROG MAINT     : TK    AJOUT ++INCLUDE SQKCPLDE             EQW8LBVR
00025 *                    :       AJOUT APPEL DELETE-TS-PLAN DANS      EQW8LBVR
00026 *  09-10-90          :       SORTIE-LEVEL-(SUP/MAX/SIGNATURE)     EQW8LBVR
00027 *                    :       ET ABANDON-TACHE (SQKCMERO)          EQW8LBVR
00028 *                    : POUR TOUTE   CONVERSATION (SAUF MENU)      EQW8LBVR
00029 *---------------------------------------------------------------+ EQW8LBVR
00030 *                          >> BUT <<                            ! EQW8LBVR
00031 * ............................................................. ! EQW8LBVR
00032 *                                                               ! EQW8LBVR
00033 *                       >> SYNOPTIQUE <<                        ! EQW8LBVR
00034 *----------+-------+--------------------------------------------+ EQW8LBVR
00035 * RESSOURCE! M/O/A !               DESCRIPTION                  ! EQW8LBVR
00036 *----------+-------+--------------------------------------------+ EQW8LBVR
00037 *          !       !                                            ! EQW8LBVR
00038 *----------+-------+----------+---------------------------------+ EQW8LBVR
00039 * PROGR.   ! MODE  ! COMMAREA !        DESCRIPTION              ! EQW8LBVR
00040 * APPELANT ! APPEL ! TRANSMISE!                                 ! EQW8LBVR
00041 *----------+-------+----------+---------------------------------+ EQW8LBVR
00042 *          !       !          !                                 ! EQW8LBVR
00043 *----------+-------+----------+---------------------------------+ EQW8LBVR
00044 * PROGR.   ! MODE  ! COMMAREA !        DESCRIPTION              ! EQW8LBVR
00045 * APPELE   ! APPEL ! TRANSMISE!                                 ! EQW8LBVR
00046 *----------+-------+----------+---------------------------------+ EQW8LBVR
00047 *          !       !          !                                 ! EQW8LBVR
00048 *----------+-------+----------+---------------------------------+ EQW8LBVR
00049 * ERREUR   !             MESSAGE OU TRAITEMENT                  ! EQW8LBVR
00050 *----------+----------------------------------------------------+ EQW8LBVR
00051 *          !                                                    ! EQW8LBVR
00052 *----------+----------------------------------------------------+ EQW8LBVR
00053 *             >> STRUCTURE GENERALE DU PROGRAMME <<             ! EQW8LBVR
00054 * ............................................................. ! EQW8LBVR
00055 * ............................................................. ! EQW8LBVR
00056 *                                                               ! EQW8LBVR
00057 *                       >> MAINTENANCE <<                       ! EQW8LBVR
00058 *-------------+-------------------------------------------------+ EQW8LBVR
00059 * DATE/AUTEUR !         DESCRIPTION DE LA MODIFICATION          ! EQW8LBVR
00060 *-------------+-------------------------------------------------+ EQW8LBVR
00061 * 99/99/99    !                                                 ! EQW8LBVR
00062 * AUTEUR      !                                                 ! EQW8LBVR
00063 * LEVEL       !                                                 ! EQW8LBVR
00064 *-------------+-------------------------------------------------+ EQW8LBVR
00061 * 03/06/2002  ! FD 21856: SI MODIF DE LA DATE PERMIS ET DU NOM  ! EQW8LBVR
00062 * C.LANDRODIE ! OU PRENOM OU DATE NAISSANCE : CHGT DE CONDUCTEUR! EQW8LBVR
00064 *-------------+-------------------------------------------------+ EQW8LBVR
00061 * 04/12/2002  ! FD 22375: CORRECTION ANOMALIE SUR CONTROLE STATU! EQW8LBVR
00062 * A.BERNARD   ! INCOMPATIBLES - MESSAGE FB088                   ! EQW8LBVR
00064 *-------------+-------------------------------------------------+ EQW8LBVR
U3319 * 22/04/2003  ! FD U3319 : EXT.AACONV + MODIF ALIM. BANDEAU     ! EFU4RQMP
U3319 * O.LASEIGNE  !            CLIENT                               ! EFU4RQMP
00064 *-------------+-------------------------------------------------+ EQW8LBVR
F2980 * 04/12/2003  ! FD 32980 : EVOLUTION HORS SERIE PRO             ! EFU4RQMP
F2980 * B.MELLON    !                                                 ! EFU4RQMP
00064 *-------------+-------------------------------------------------+ EQW8LBVR
U3319 * 14/11/2003  ! FD 33576 : EVOLUTION MESURES DE SOUSCRIPTION    ! EFU4RQMP
F3576 * A.LEMAITRE  !                                                 ! EFU4RQMP
00064 *-------------+-------------------------------------------------+ EQW8LBVR
F39250* 19/05/2004  ! FD 39250 : PROFESSION ET USAGES GENERAUX        ! EFU4RQMP
F39250* F LE ROUX   !            AJOUT PROFESSION 14                  ! EFU4RQMP
F39250*-------------+-------------------------------------------------+ EQW8LBVR
DELTS * B PORTEFAIX ! 30/12/2004 AJOUT INCLUDE DELETE TS              !
      *-------------+-------------------------------------------------+
00065 *                       *************                           ! EQW8LBVR
00066 *                       ** LEGENDE **                           ! EQW8LBVR
00067 *                       *************                           ! EQW8LBVR
00068 *                                                               ! EQW8LBVR
00069 * RESSOURCE       : FICHIER , BASE , TS                         ! EQW8LBVR
00070 *                   - INDIQUER NOM COBOL ET DDNAME SI DIFFERENT ! EQW8LBVR
00071 *                   - SI POSSIBLE CODE DU DICTIONAIRE           ! EQW8LBVR
00072 *                   - DONNER PSB SI BASE                        ! EQW8LBVR
00073 *                                                               ! EQW8LBVR
00074 * M/O/A           : MODE / ORGANISATION / ACCES                 ! EQW8LBVR
00075 *                    !         !           !                    ! EQW8LBVR
00076 *                    !         !           +---> D = DIRECT     ! EQW8LBVR
00077 *                    !         !                 S = SEQUENTIEL ! EQW8LBVR
00078 *                    !         +---> S = SEQUENTIELLE           ! EQW8LBVR
00079 *                    !               V = VSAM                   ! EQW8LBVR
00080 *                    !               X = AUTRE                  ! EQW8LBVR
00081 *                    +---> L = LECTURE                          ! EQW8LBVR
00082 *                          E = ECRITURE                         ! EQW8LBVR
00083 *                          M = MISE A JOUR                      ! EQW8LBVR
00084 *                                                               ! EQW8LBVR
00085 * MODE D'APPEL    : CALL , XCTL , LINK                          ! EQW8LBVR
00086 *                                                               ! EQW8LBVR
00087 * COMMAREA TRANS. : - NOM DU NIVEAU "01"                        ! EQW8LBVR
00088 *                   - LONGUEUR DE LA COMMAREA                   ! EQW8LBVR
00089 *                                                               ! EQW8LBVR
00090 * MAINTENANCE     : LE LEVEL D'UNE MAINTENANCE CORRESPOND AU 1ER! EQW8LBVR
00091 *                   N∞ DE LEVEL D'INSTRUCTION CORRESPONDANT A   ! EQW8LBVR
00092 *                   CETTE MAINTENANCE.                          ! EQW8LBVR
00093 *===============================================================+ EQW8LBVR
00094  ENVIRONMENT DIVISION.                                            EQW8LBVR
00095  CONFIGURATION SECTION.                                           EQW8LBVR
00096  SPECIAL-NAMES.                                                   EQW8LBVR
00097      DECIMAL-POINT IS COMMA.                                      EQW8LBVR
00098  DATA DIVISION.                                                   EQW8LBVR
00099  WORKING-STORAGE SECTION.                                         EQW8LBVR
00100 *                                                                 EQW8LBVR
00101 ***************************************************************** EQW8LBVR
00102 *   ZONES DE PILOTAGE DU SQUELETTE                              * EQW8LBVR
00103 *   ATTENTION: CE DOIT ETRE LE PREMIER INCLUDE                  * EQW8LBVR
00104 ***************************************************************** EQW8LBVR
00105  ++INCLUDE SQKWDV0B                                               EQW8LBVR
U3319  01  IA-SAUVE               PIC S9(3) COMP-3   VALUE +0.
00106 /                                                                 EQW8LBVR
00107 ***************************************************************** EQW8LBVR
00108 * COMMAREA POUR APPEL D'INTERFACE DE CONFIDENTIALITE (AUAAL00)  * EQW8LBVR
00109 ***************************************************************** EQW8LBVR
00110  01  AUAAC.                                                       EQW8LBVR
00111  ++INCLUDE AUAAC                                                  EQW8LBVR
00112 /                                                                 EQW8LBVR
00113 ***************************************************************** EQW8LBVR
00114 *   IDENTIFICATION DES TABLES SPI POUR ACCES AUX TABLES         * EQW8LBVR
00115 *   DE TYPE MENU OU CONVERSATION                                * EQW8LBVR
00116 ***************************************************************** EQW8LBVR
00117  01  IDENT-TABLE.                                                 EQW8LBVR
00118      05  TABLE-PREF         PIC X(06).                            EQW8LBVR
00119      05  TABLE-SUFF         PIC X(02).                            EQW8LBVR
00120 ***************************************************************** EQW8LBVR
00121 *   IDENTIFICATION DE LA TS DE CONFIDENTIALITE                  * EQW8LBVR
00122 ***************************************************************** EQW8LBVR
00123  01  IDENT-TS-CONF.                                               EQW8LBVR
00124      05  CONF-TS-PREF       PIC X(04).                            EQW8LBVR
00125      05  CONF-TS-SUFF.                                            EQW8LBVR
00126          10  CONF-TS-CONV   PIC X(03).                            EQW8LBVR
00127          10  FILLER         PIC X(01) VALUE '1'.                  EQW8LBVR
00128 ***************************************************************** EQW8LBVR
00129 *   IDENTIFICATION DE LA TS APPLICATIVE                         * EQW8LBVR
00130 ***************************************************************** EQW8LBVR
00131  01  IDENT-TS-APP.                                                EQW8LBVR
00132      05  APP-TS-PREF        PIC X(04).                            EQW8LBVR
00133      05  APP-TS-SUFF.                                             EQW8LBVR
00134          10  APP-TS-CONV    PIC X(03).                            EQW8LBVR
00135          10  FILLER         PIC X(01) VALUE '1'.                  EQW8LBVR
00136 ***************************************************************** EQW8LBVR
00137 *   IDENTIFICATION DE LA TS DE PAGINATION                       * EQW8LBVR
00138 ***************************************************************** EQW8LBVR
00139  01  IDENT-TS-PAGE.                                               EQW8LBVR
00140      05  PAGE-TS-PREF       PIC X(04).                            EQW8LBVR
00141      05  PAGE-TS-SUFF.                                            EQW8LBVR
00142          10  PAGE-TS-CONV   PIC X(03).                            EQW8LBVR
00143          10  FILLER         PIC X(01) VALUE '1'.                  EQW8LBVR
00144 ******************* POUR CONVERSATION *************************** EQW8LBVR
00145 *TK0909 POUR EXIT-SELECTION-DE-PLAN : DESCRIPTION DE LA TS        EQW8LBVR
00146 ***************************************************************** EQW8LBVR
00147  ++INCLUDE SQKWPLTS                                               EQW8LBVR
00148 */                                                                EQW8LBVR
00149 /                                                                 EQW8LBVR
00150 ***************************************************************** EQW8LBVR
00151 *   DESCRIPTION DE LA TS DE CONFIDENTIALITE  CONVERSATION       * EQW8LBVR
00152 ***************************************************************** EQW8LBVR
00153  01  AUAAIW.                                                      EQW8LBVR
00154  ++INCLUDE AUAAIW                                                 EQW8LBVR
DELTS *
DELTS *  TABLE DES TS A DELETER
DELTS  ++INCLUDE CCMADLTS
00155 /                                                                 EQW8LBVR
00156 ***************************************************************** EQW8LBVR
00157 *   DESCRIPTION DE LA TABLE DES CONVERSATIONS (SPITAB)          * EQW8LBVR
00158 ***************************************************************** EQW8LBVR
00159  ++INCLUDE CCAACONV                                               EQW8LBVR
U3319  ++INCLUDE CCAACON2                                               EFUTSUGF
00160 *                                                                 EQW8LBVR
00161 ***************************************************************** EQW8LBVR
00162 *   DESCRIPTION DE LA TS SUSPENS                                * EQW8LBVR
00163 ***************************************************************** EQW8LBVR
00164  ++INCLUDE MAILONG                                                EQW8LBVR
00165 * ITEM 1                                                          EQW8LBVR
00166  01  TS-SUSPENS1.                                                 EQW8LBVR
00167  ++INCLUDE MASP                                                   EQW8LBVR
00168 * ITEM 2                                                          EQW8LBVR
00169  01  TS-SUSPENS2.                                                 EQW8LBVR
00170  ++INCLUDE MASP                                                   EQW8LBVR
00171 *                                                                 EQW8LBVR
00172 ***************************************************************** EQW8LBVR
00173 *   DESCRIPTION DE L'ORG 40A REFONTE AUTOMOBILE GFA             * EQW8LBVR
00174 ***************************************************************** EQW8LBVR
00175 * TS PERSONNE                                                     EQW8LBVR
00176  01 TS-PERSONNE.                                                  EQW8LBVR
00177  ++INCLUDE FBIPERS                                                EQW8LBVR
00178                                                                   EQW8LBVR
00179 ***************************************************************** EQW8LBVR
00180 *   DESCRIPTION DU SEGMENT TRANSIT                              * EQW8LBVR
00181 ***************************************************************** EQW8LBVR
00182 * TS TRANSIT                                                      EQW8LBVR
00183 * ITEM 1                                                          EQW8LBVR
00184  01 FBMISPTR-IT1.                                                 EQW8LBVR
00185  ++INCLUDE FBMISPTR                                               EQW8LBVR
00186                                                                   EQW8LBVR
00187 * ITEM 2                                                          EQW8LBVR
00188  01 FBMISPTR-IT2.                                                 EQW8LBVR
00189  ++INCLUDE FBMISPTR                                               EQW8LBVR
00190                                                                   EQW8LBVR
00191 ***************************************************************** EQW8LBVR
00192 *   DESCRIPTION DE LA TS DATE                                   * EQW8LBVR
00193 ***************************************************************** EQW8LBVR
00194  ++INCLUDE MAIDC1CA                                               EQW8LBVR
00195                                                                   EQW8LBVR
00196 ***************************************************************** EQW8LBVR
00197 *   ZONES NECESSAIRES AU MODULE DES CONTROLES TECHNIQUES        * EQW8LBVR
00198 ***************************************************************** EQW8LBVR
00199  01 MAI90C00.                                                     EQW8LBVR
00200  ++INCLUDE MAI90C00                                               EQW8LBVR
00201                                                                   EQW8LBVR
00202 ***************************************************************** EQW8LBVR
00203 *   ZONES NECESSAIRES AU MODULE DES CONTROLES HABILITATION      * EQW8LBVR
00204 ***************************************************************** EQW8LBVR
00205  01 MAI90C20.                                                     EQW8LBVR
00206  ++INCLUDE MAI90C20                                               EQW8LBVR
00207 /                                                                 EQW8LBVR
00208 ***************************************************************** EQW8LBVR
00209 *   DECRIRE   ICI   LES   ZONES   SPECIFIQUES   AU   PROGRAMME  * EQW8LBVR
00210 ***************************************************************** EQW8LBVR
00211 * GESTION ET CALCUL DE DATES                                      EQW8LBVR
00212                                                                   EQW8LBVR
00213  01  WSS-AGEMIN               PIC 9(2).                           EQW8LBVR
00214                                                                   EQW8LBVR
00215  01  WSS-DATE1-SAMJ.                                              EQW8LBVR
00216      05  WSS-DATE1-SAMJ-SA    PIC 9(4).                           EQW8LBVR
00217      05  WSS-DATE1-SAMJ-M     PIC 9(2).                           EQW8LBVR
00218      05  WSS-DATE1-SAMJ-J     PIC 9(2).                           EQW8LBVR
00219                                                                   EQW8LBVR
00220  01  WSS-DATE-MINI-SAMJ.                                          EQW8LBVR
00221      05  WSS-DATE-MINI-SAMJ-SA    PIC 9(4).                       EQW8LBVR
00222      05  WSS-DATE-MINI-SAMJ-M     PIC 9(2).                       EQW8LBVR
00223      05  WSS-DATE-MINI-SAMJ-J     PIC 9(2).                       EQW8LBVR
00224                                                                   EQW8LBVR
00225  01  WSS-DATE-NAIS-SAMJ.                                          EQW8LBVR
00226      05  WSS-DATE-NAIS-SAMJ-SA     PIC 9(4).                      EQW8LBVR
00227      05  WSS-DATE-NAIS-SAMJ-M      PIC 9(2).                      EQW8LBVR
00228      05  WSS-DATE-NAIS-SAMJ-J      PIC 9(2).                      EQW8LBVR
00229                                                                   EQW8LBVR
00230  01  WSS-DATE-PERM-SAMJ.                                          EQW8LBVR
00231      05  WSS-DATE-PERM-SAMJ-SA     PIC 9(4).                      EQW8LBVR
00232      05  WSS-DATE-PERM-SAMJ-M      PIC 9(2).                      EQW8LBVR
00233      05  WSS-DATE-PERM-SAMJ-J      PIC 9(2).                      EQW8LBVR
00234                                                                   EQW8LBVR
00230  01  WSS-DATE-PERM2-SAMJ.                                         EQW8LBVR
00231      05  WSS-DATE-PERM2-SAMJ-SA     PIC 9(4).                     EQW8LBVR
00232      05  WSS-DATE-PERM2-SAMJ-M      PIC 9(2).                     EQW8LBVR
00233      05  WSS-DATE-PERM2-SAMJ-J      PIC 9(2).                     EQW8LBVR
00234                                                                   EQW8LBVR
00235  01  WSS-DATJOUR.                                                 EQW8LBVR
00236      05 WSS-DATJOUR-J              PIC X(02).                     EQW8LBVR
00237      05 WSS-DATJOUR-M              PIC X(02).                     EQW8LBVR
00238      05 WSS-DATJOUR-S              PIC X(02).                     EQW8LBVR
00239      05 WSS-DATJOUR-A              PIC X(02).                     EQW8LBVR
00240                                                                   EQW8LBVR
00241  01  WSS-DATJOUR-SAMJ-9.                                          EQW8LBVR
00242      05 WSS-DATJOUR-S-9            PIC 9(2).                      EQW8LBVR
00243      05 WSS-DATJOUR-A-9            PIC 9(2).                      EQW8LBVR
00244      05 WSS-DATJOUR-M-9            PIC 9(2).                      EQW8LBVR
00245      05 WSS-DATJOUR-J-9            PIC 9(2).                      EQW8LBVR
00246                                                                   EQW8LBVR
00247  01 WSS-SAMJ.                                                     EQW8LBVR
00248      05 WSS-SAMJ-SA       PIC X(4).                               EQW8LBVR
00249      05 WSS-SAMJ-M        PIC X(2).                               EQW8LBVR
00250      05 WSS-SAMJ-J        PIC X(2).                               EQW8LBVR
00251                                                                   EQW8LBVR
00252  01 WSS-JMSA.                                                     EQW8LBVR
00253      05 WSS-JMSA-J        PIC X(2).                               EQW8LBVR
00254      05 WSS-JMSA-M        PIC X(2).                               EQW8LBVR
00255      05 WSS-JMSA-SA       PIC X(4).                               EQW8LBVR
00256                                                                   EQW8LBVR
00257  01 WSS-DATE-A-VERIFIER.                                          EQW8LBVR
00258     05 WSS-DATE-A-VERIFIER-JJ  PIC X(02).                         EQW8LBVR
00259     05 WSS-DATE-A-VERIFIER-MM  PIC X(02).                         EQW8LBVR
00260     05 WSS-DATE-A-VERIFIER-SS  PIC X(02).                         EQW8LBVR
00261     05 WSS-DATE-A-VERIFIER-AA  PIC X(02).                         EQW8LBVR
00262                                                                   EQW8LBVR
00263  01 WSS-NBRE-VEHI-4R2RCC       PIC 9(2) VALUE ZERO.               EQW8LBVR
00264  01 WSS-TYPE-PERMIS            PIC X(3).                          EQW8LBVR
00265  01 W-STATUT                   PIC X(2).                          EQW8LBVR
00266  01 WSS-DATE-JOUR-SAMJ         PIC 9(08).                         EQW8LBVR
00267  01 WSS-TYPE-PERMIS-OK         PIC X.                             EQW8LBVR
00268  01 WSS-DATE-PERMIS-OK         PIC X.                             EQW8LBVR
00269  01 WSS-DATE-OK                PIC X.                             EQW8LBVR
00270  01 WSS-APPEL-AIDE-STATUT      PIC X.                             EQW8LBVR
00271  01 WSS-APPEL-AIDE-PROFESSION  PIC X.                             EQW8LBVR
00272  01 WSS-NB-POINT-INTERO        PIC 9(2) VALUE ZERO.               EQW8LBVR
00273  01 WSS-NBSALARIES             PIC 9(3) VALUE ZERO.               EQW8LBVR
00274  01 WSS-NBMOIS-INFO            PIC 9(2) VALUE ZERO.               EQW8LBVR
00275  01 WSS-NBMOIS-INTER           PIC 9(2) VALUE ZERO.               EQW8LBVR
00276  01 WSS-NBENFANTS              PIC 9(2) VALUE ZERO.               EQW8LBVR
00277  01 WSS-NBJOURS-RETRAIT        PIC 9(3) VALUE ZERO.               EQW8LBVR
00278  01 WSS-NBCONTRATS             PIC 9(2) VALUE ZERO.               EQW8LBVR
00279  01 WSS-READ-TSPERS            PIC X.                             EQW8LBVR
00280  01 NBR-MODIF-COND             PIC 9(01) VALUE ZERO.              EQW8LBVR
00281  01 WSS-MODIF-PERMIS           PIC X.                             EQW8LBVR
00282  01 WSS-CONTROLE-AAC-OK        PIC X.                             EQW8LBVR
00283                                                                   EQW8LBVR
00284 * ZONES DE RECUPERATION DU NUMERO DE CLIENT                       EQW8LBVR
00285  01 W-GESCLI.                                                     EQW8LBVR
00286     05 W-GES                 PIC X(06).                           EQW8LBVR
00287     05 W-CLI                 PIC X(05).                           EQW8LBVR
00288                                                                   EQW8LBVR
00289     05 IND1                    PIC 99.                            EQW8LBVR
00290     05 IND2                    PIC 99.                            EQW8LBVR
00291     05 IND3                    PIC 99.                            EQW8LBVR
00292                                                                   EQW8LBVR
00293 * ZONES TRAVAIL POUR CONTROLE D'ACCES                             EQW8LBVR
00294  01  Z-CONTROLE-ACCES.                                            EQW8LBVR
00295      05  Z-CODE-REGIME-AS     PIC X(05).                          EQW8LBVR
00296      05  Z-CODE-REGIME-CA     PIC X(05).                          EQW8LBVR
00297      05  Z-COMPTEUR-TS        PIC S9(2) COMP.                     EQW8LBVR
00298      05  Z-AFFICHAGE-SELECTIF PIC X(03).                          EQW8LBVR
00299      05  Z-AFFICHER-OPTION    PIC X(03).                          EQW8LBVR
00300 *                                                                 EQW8LBVR
00301 ***************************************************************** EQW8LBVR
00302 * SPITAB                                                        * EQW8LBVR
00303 ***************************************************************** EQW8LBVR
00304 ** INCLUDE DE LA TABLE DES STATUTS FBSTAT01                       EQW8LBVR
00305  ++INCLUDE CCFBSTAT                                               EQW8LBVR
00306                                                                   EQW8LBVR
00307 ** INCLUDE DE LA TABLE DES CODES PROFESSIONS FBPROF01             EQW8LBVR
00308  ++INCLUDE CCFBPROF                                               EQW8LBVR
00309                                                                   EQW8LBVR
00310 ** INCLUDE DE LA TABLE DES CATEGORIES DE PERMIS FBPERM01          EQW8LBVR
00311  ++INCLUDE CCFBPERM                                               EQW8LBVR
00312                                                                   EQW8LBVR
00313 ***************************************************************** EQW8LBVR
00314 *   ZONES GENERALES OBLIGATOIRES                                * EQW8LBVR
00315 ***************************************************************** EQW8LBVR
00316 *   ZONES DE TEST DU CODE-RETOUR CICS  :  EIBRCODE                EQW8LBVR
00317  ++INCLUDE SQKWEIB0                                               EQW8LBVR
00318 ***************************************************************** EQW8LBVR
00319 * ZONES DATE/HEURE ET NOM DE TERMINAL/CODE TRANSACTION            EQW8LBVR
00320 ***************************************************************** EQW8LBVR
00321  ++INCLUDE SQKWDATH                                               EQW8LBVR
00322 ***************************************************************** EQW8LBVR
00323 * ZONES BMS     (TOUCHES FONCTION ET ATTRIBUTS)                 * EQW8LBVR
00324 ***************************************************************** EQW8LBVR
00325 *                                                               * EQW8LBVR
00326 *                 DEFINITION DES ATTRIBUTS                      * EQW8LBVR
00327 *         MDT ON                             MDT OFF            * EQW8LBVR
00328 *                                                               * EQW8LBVR
00329 *        BRT-ALP-FSET                        BRT-ALP            * EQW8LBVR
00330 *        BRT-NUM-FSET                        BRT-NUM            * EQW8LBVR
00331 *        BRT-PRO-FSET                        BRT-PRO            * EQW8LBVR
00332 *        DRK-ALP-FSET                        DRK-ALP            * EQW8LBVR
00333 *        DRK-PRO-FSET                        DRK-PRO            * EQW8LBVR
00334 *        DRK-PRO-RSET                        DRK-RSET           * EQW8LBVR
00335 *        NOR-ALP-FSET                        NOR-ALP            * EQW8LBVR
00336 *        NOR-NUM-FSET                        NOR-NUM            * EQW8LBVR
00337 *        NOR-PRO-FSET                        NOR-PRO            * EQW8LBVR
00338 ***************************************************************** EQW8LBVR
00339 *300890    ++INCLUDE SQKWECRA  A: SANS PF9,  B: AVEC  PF9         EQW8LBVR
00340  ++INCLUDE SQKWECRA                                               EQW8LBVR
00341 ***************************************************************** EQW8LBVR
00342 *   ZONES DE CONTROLE ET DE TRAITEMENT SPECIFIQUES              * EQW8LBVR
00343 ***************************************************************** EQW8LBVR
00344 *                                                                 EQW8LBVR
00345 ***************************************************************** EQW8LBVR
00346 *   LONGUEUR DE LA COMMAREA                                     * EQW8LBVR
00347 ***************************************************************** EQW8LBVR
00348  01  COM-GENE-LONG-COMMAREA           PIC S9(4) COMP VALUE +4096. EQW8LBVR
00349 *                                                                 EQW8LBVR
00350 ***************************************************************** EQW8LBVR
00351 *   ZONES DE COMMAREA POUR APPEL A SPITAB                       * EQW8LBVR
00352 ***************************************************************** EQW8LBVR
00353  01  XSPIPARM.                                                    EQW8LBVR
00354  ++INCLUDE SPIPARTP                                               EQW8LBVR
00355                                                                   EQW8LBVR
00356 ***************************************************************** EQW8LBVR
00357 *   MODULE K200LDAT :GESTION DES DATES                          * EQW8LBVR
00358 ***************************************************************** EQW8LBVR
00359  01  K2COM-DATES.                                                 EQW8LBVR
00360  ++INCLUDE K2IWDATE                                               EQW8LBVR
00361                                                                   EQW8LBVR
00362 ***************************************************************** EQW8LBVR
00363 *   ZONES SPECIFIQUES A L'APPEL DU PROGRAMME DE CADRAGE         * EQW8LBVR
00364 ***************************************************************** EQW8LBVR
00365  ++INCLUDE CCMTCADR.                                              EQW8LBVR
00366  01  LONG-XKMTCADR PIC S9(4) COMP VALUE +60.                      EQW8LBVR
00367 /                                                                 EQW8LBVR
00368 *                                                                 EQW8LBVR
00369 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* EQW8LBVR
00370 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* EQW8LBVR
00371 *   COMMAREA GENERALE DES APPLICATIONS CONCORDE ( SQKWCOMM )    * EQW8LBVR
00372 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* EQW8LBVR
00373 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* EQW8LBVR
00374  ++INCLUDE SQKWCOMM                                               EQW8LBVR
00375  ++INCLUDE MAICOMM                                                EQW8LBVR
00376  ++INCLUDE FBICOMM                                                EQW8LBVR
00377 *                                                                 EQW8LBVR
00378 ***************************************************************** EQW8LBVR
00379 *    ZONES DE LA MAP  FB02M0                                      EQW8LBVR
00380 ***************************************************************** EQW8LBVR
00381  01  FILLER  PIC X(16) VALUE '*** MAP FB02 ***'.                  EQW8LBVR
00382 *                                                                 EQW8LBVR
00383  01  Z-MAP.                                                       EQW8LBVR
00384 *                                                                 EQW8LBVR
00385  ++INCLUDE FB02M0Z                                                EQW8LBVR
00386 *                                                                 EQW8LBVR
00387 ***************************************************************** EQW8LBVR
00388 *     DESCRIPTION DE LA TS-ECRAN   MDT OFF                        EQW8LBVR
00389 ***************************************************************** EQW8LBVR
00390  ++INCLUDE SQKWTSMA                                               EQW8LBVR
00391      02  TS-FB02M00O REDEFINES ZONE-TS-ECRAN.                     EQW8LBVR
00392          10 FILLER PIC X(12).                                     EQW8LBVR
00393          10 TS-ECR-XTRMTRACL   COMP PIC S9(4).                    EQW8LBVR
00394          10 TS-ECR-XTRMTRACA   PIC X.                             EQW8LBVR
00395          10 TS-ECR-XTRMTRACO   PIC X(9).                          EQW8LBVR
00396          10 TS-ECR-XAPPLILL    COMP PIC S9(4).                    EQW8LBVR
00397          10 TS-ECR-XAPPLILA    PIC X.                             EQW8LBVR
00398          10 TS-ECR-XAPPLILO    PIC X(30).                         EQW8LBVR
00399          10 TS-ECR-XJOURDL     COMP PIC S9(4).                    EQW8LBVR
00400          10 TS-ECR-XJOURDA     PIC X.                             EQW8LBVR
00401          10 TS-ECR-XJOURDO     PIC X(8).                          EQW8LBVR
00402          10 TS-ECR-XRACFLL     COMP PIC S9(4).                    EQW8LBVR
00403          10 TS-ECR-XRACFLA     PIC X.                             EQW8LBVR
00404          10 TS-ECR-XRACFLO     PIC X(15).                         EQW8LBVR
00405          10 TS-ECR-XHEUREDL    COMP PIC S9(4).                    EQW8LBVR
00406          10 TS-ECR-XHEUREDA    PIC X.                             EQW8LBVR
00407          10 TS-ECR-XHEUREDO    PIC X(8).                          EQW8LBVR
00408          10 TS-ECR-GESCLIL     COMP PIC S9(4).                    EQW8LBVR
00409          10 TS-ECR-GESCLIA     PIC X.                             EQW8LBVR
00410          10 TS-ECR-GESCLIO     PIC X(11).                         EQW8LBVR
00411          10 TS-ECR-RAICL       COMP PIC S9(4).                    EQW8LBVR
00412          10 TS-ECR-RAICA       PIC X.                             EQW8LBVR
00413          10 TS-ECR-RAICO       PIC X(3).                          EQW8LBVR
00414          10 TS-ECR-NOMCL       COMP PIC S9(4).                    EQW8LBVR
00415          10 TS-ECR-NOMCA       PIC X.                             EQW8LBVR
00416          10 TS-ECR-NOMCO       PIC X(30).                         EQW8LBVR
00417          10 TS-ECR-PERNUMXL    COMP PIC S9(4).                    EQW8LBVR
00418          10 TS-ECR-PERNUMXA    PIC X.                             EQW8LBVR
00419          10 TS-ECR-PERNUMXO    PIC X(6).                          EQW8LBVR
00420          10 TS-ECR-PERSTACL    COMP PIC S9(4).                    EQW8LBVR
00421          10 TS-ECR-PERSTACA    PIC X.                             EQW8LBVR
00422          10 TS-ECR-PERSTACO    PIC X(2).                          EQW8LBVR
00423          10 TS-ECR-PERSALNL    COMP PIC S9(4).                    EQW8LBVR
00424          10 TS-ECR-PERSALNA    PIC X.                             EQW8LBVR
00425          10 TS-ECR-PERSALNO    PIC X(3).                          EQW8LBVR
00426          10 TS-ECR-PERTITLL    COMP PIC S9(4).                    EQW8LBVR
00427          10 TS-ECR-PERTITLA    PIC X.                             EQW8LBVR
00428          10 TS-ECR-PERTITLO    PIC X(3).                          EQW8LBVR
00429          10 TS-ECR-PERNOMLL    COMP PIC S9(4).                    EQW8LBVR
00430          10 TS-ECR-PERNOMLA    PIC X.                             EQW8LBVR
00431          10 TS-ECR-PERNOMLO    PIC X(20).                         EQW8LBVR
00432          10 TS-ECR-PERPRELL    COMP PIC S9(4).                    EQW8LBVR
00433          10 TS-ECR-PERPRELA    PIC X.                             EQW8LBVR
00434          10 TS-ECR-PERPRELO    PIC X(20).                         EQW8LBVR
00435          10 TS-ECR-PERNAIDL    COMP PIC S9(4).                    EQW8LBVR
00436          10 TS-ECR-PERNAIDA    PIC X.                             EQW8LBVR
00437          10 TS-ECR-PERNAIDO    PIC X(8).                          EQW8LBVR
00438          10 TS-ECR-PERSEXCL    COMP PIC S9(4).                    EQW8LBVR
00439          10 TS-ECR-PERSEXCA    PIC X.                             EQW8LBVR
00440          10 TS-ECR-PERSEXCO    PIC X.                             EQW8LBVR
00441          10 TS-ECR-PERENCNL    COMP PIC S9(4).                    EQW8LBVR
00442          10 TS-ECR-PERENCNA    PIC X.                             EQW8LBVR
00443          10 TS-ECR-PERENCNO    PIC X(2).                          EQW8LBVR
00444          10 TS-ECR-PERMATCL    COMP PIC S9(4).                    EQW8LBVR
00445          10 TS-ECR-PERMATCA    PIC X.                             EQW8LBVR
00446          10 TS-ECR-PERMATCO    PIC X.                             EQW8LBVR
00447          10 TS-ECR-PERPROCL    COMP PIC S9(4).                    EQW8LBVR
00448          10 TS-ECR-PERPROCA    PIC X.                             EQW8LBVR
00449          10 TS-ECR-PERPROCO    PIC X(2).                          EQW8LBVR
00450          10 TS-ECR-PERPROLL    COMP PIC S9(4).                    EQW8LBVR
00451          10 TS-ECR-PERPROLA    PIC X.                             EQW8LBVR
00452          10 TS-ECR-PERPROLO    PIC X(30).                         EQW8LBVR
00453          10 TS-ECR-PRMTYPC1L   COMP PIC S9(4).                    EQW8LBVR
00454          10 TS-ECR-PRMTYPC1A   PIC X.                             EQW8LBVR
00455          10 TS-ECR-PRMTYPC1O   PIC X(3).                          EQW8LBVR
00456          10 TS-ECR-PRMOBTD1L   COMP PIC S9(4).                    EQW8LBVR
00457          10 TS-ECR-PRMOBTD1A   PIC X.                             EQW8LBVR
00458          10 TS-ECR-PRMOBTD1O   PIC X(8).                          EQW8LBVR
00459          10 TS-ECR-PRMTYPC2L   COMP PIC S9(4).                    EQW8LBVR
00460          10 TS-ECR-PRMTYPC2A   PIC X.                             EQW8LBVR
00461          10 TS-ECR-PRMTYPC2O   PIC X(3).                          EQW8LBVR
00462          10 TS-ECR-PRMOBTD2L   COMP PIC S9(4).                    EQW8LBVR
00463          10 TS-ECR-PRMOBTD2A   PIC X.                             EQW8LBVR
00464          10 TS-ECR-PRMOBTD2O   PIC X(8).                          EQW8LBVR
00465          10 TS-ECR-PERCOACL    COMP PIC S9(4).                    EQW8LBVR
00466          10 TS-ECR-PERCOACA    PIC X.                             EQW8LBVR
00467          10 TS-ECR-PERCOACO    PIC X.                             EQW8LBVR
00468          10 TS-ECR-ANPANCNL    COMP PIC S9(4).                    EQW8LBVR
00469          10 TS-ECR-ANPANCNA    PIC X.                             EQW8LBVR
00470          10 TS-ECR-ANPANCNO    PIC X(2).                          EQW8LBVR
00471          10 TS-ECR-ANPINDCL    COMP PIC S9(4).                    EQW8LBVR
00472          10 TS-ECR-ANPINDCA    PIC X.                             EQW8LBVR
00473          10 TS-ECR-ANPINDCO    PIC X.                             EQW8LBVR
00477          10 TS-ECR-ANPMOTLL    COMP PIC S9(4).                    EQW8LBVR
00478          10 TS-ECR-ANPMOTLA    PIC X.                             EQW8LBVR
F3576          10 TS-ECR-ANPMOTLO    PIC X(20).                         EQW8LBVR
00474          10 TS-ECR-ANPNBJNL    COMP PIC S9(4).                    EQW8LBVR
00475          10 TS-ECR-ANPNBJNA    PIC X.                             EQW8LBVR
00476          10 TS-ECR-ANPNBJNO    PIC X(3).                          EQW8LBVR
00480          10 TS-ECR-XCDECL      COMP PIC S9(4).                    EQW8LBVR
00481          10 TS-ECR-XCDECA      PIC X.                             EQW8LBVR
00482          10 TS-ECR-XCDECO      PIC X(9).                          EQW8LBVR
00483          10 TS-ECR-XMSGILL     COMP PIC S9(4).                    EQW8LBVR
00484          10 TS-ECR-XMSGILA     PIC X.                             EQW8LBVR
00485          10 TS-ECR-XMSGILO     PIC X(59).                         EQW8LBVR
00486          10 TS-ECR-XMSGALL     COMP PIC S9(4).                    EQW8LBVR
00487          10 TS-ECR-XMSGALA     PIC X.                             EQW8LBVR
00488          10 TS-ECR-XMSGALO     PIC X(79).                         EQW8LBVR
00489 *                                                                 EQW8LBVR
00490 ***************************************************************** EQW8LBVR
00491 *     ZONE BUFFER D'ENTREE/SORTIE                                 EQW8LBVR
00492 ***************************************************************** EQW8LBVR
00493 *                                                                 EQW8LBVR
00494 *                                                                 EQW8LBVR
00495 ***************************************************************** EQW8LBVR
00496 * ZONE D'INTERFACE POUR LA GESTION DES ERREURS NON RECOUVRABLES   EQW8LBVR
00497 ***************************************************************** EQW8LBVR
00498  ++INCLUDE SQKWERRO                                               EQW8LBVR
00499 ***************************************************************** EQW8LBVR
00500 *  ZONES DE GESTION DES FICHIERS                                  EQW8LBVR
00501 ***************************************************************** EQW8LBVR
00502 *                                                                 EQW8LBVR
00503 /                                                                 EQW8LBVR
00504 ***************************************************************** EQW8LBVR
00505 ***************************************************************** EQW8LBVR
00506 **********************  LINKAGE SECTION ************************* EQW8LBVR
00507 ***************************************************************** EQW8LBVR
00508 ***************************************************************** EQW8LBVR
00509 *                                                                 EQW8LBVR
00510  LINKAGE SECTION.                                                 EQW8LBVR
00511 *---------------*    DFHEIBLK ; DFHCOMMAREA.                      EQW8LBVR
00512  01  DFHCOMMAREA.                                                 EQW8LBVR
00513      02  FILLER             PIC X(4096).                          EQW8LBVR
00514 *                                                                 EQW8LBVR
00515 ***************************************************************   EQW8LBVR
00516 *        ZONES ADRESSABLES EXTERNES A LA TACHE                    EQW8LBVR
00517 ***************************************************************   EQW8LBVR
00518 *    USER                                                         EQW8LBVR
00519  01  LINK-USER              PIC X(4000).                          EQW8LBVR
00520 /                                                                 EQW8LBVR
00521  PROCEDURE DIVISION.                                              EQW8LBVR
00522 *------------------*                                              EQW8LBVR
00523 *                                                                 EQW8LBVR
00524 ***************************************************************** EQW8LBVR
00525 *              T R A M E   DU   P R O G R A M M E               * EQW8LBVR
00526 ***************************************************************** EQW8LBVR
00527 *                                                                 EQW8LBVR
00528 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
00529 *                                                               * EQW8LBVR
00530 *           -----------------------------------------           * EQW8LBVR
00531 *           I MODULE DE BASE DE LA TRANSACTION FB02 I       *     EQW8LBVR
00532 *           -----------------------------------------           * EQW8LBVR
00533 *                               I                               * EQW8LBVR
00534 *           -----------------------------------------           * EQW8LBVR
00535 *           I                   I                   I           * EQW8LBVR
00536 *   --------V--------   --------V--------   --------V--------   * EQW8LBVR
00537 *   I    ENTREE     I   I  TRAITEMENT   I   I     SORTIE    I   * EQW8LBVR
00538 *   -----------------   -----------------   -----------------   * EQW8LBVR
00539 *                                                               * EQW8LBVR
00540 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
00541  MODULE-FB02.                                                     EQW8LBVR
00542 *-----------*                                                     EQW8LBVR
00543      PERFORM MODULE-ENTREE THRU                                   EQW8LBVR
00544              FIN-MODULE-ENTREE.                                   EQW8LBVR
00545 *                                                                 EQW8LBVR
00546      IF TRAITEMENT                                                EQW8LBVR
00547         PERFORM MODULE-TRAITEMENT THRU                            EQW8LBVR
00548                 FIN-MODULE-TRAITEMENT                             EQW8LBVR
00549      END-IF.                                                      EQW8LBVR
00550 *                                                                 EQW8LBVR
00551      PERFORM MODULE-SORTIE THRU                                   EQW8LBVR
00552              FIN-MODULE-SORTIE.                                   EQW8LBVR
00553 *                                                                 EQW8LBVR
00554  FIN-MODULE-FB02.  EXIT.                                          EQW8LBVR
00555 /                                                                 EQW8LBVR
00556 ***************************************************************** EQW8LBVR
00557 ***************************************************************** EQW8LBVR
00558 ***********************  MODULE ENTREE  ************************* EQW8LBVR
00559 ***************************************************************** EQW8LBVR
00560 ***************************************************************** EQW8LBVR
00561 *                                                                 EQW8LBVR
00562 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
00563 *                                                               * EQW8LBVR
00564 *                    -----------------------                    * EQW8LBVR
00565 *                    I   MODULE D'ENTREE   I                    * EQW8LBVR
00566 *                    -----------------------                    * EQW8LBVR
00567 *                               I                               * EQW8LBVR
00568 *       ---------------------------------------------           * EQW8LBVR
00569 *       I          I         I                      I           * EQW8LBVR
00570 *  ----------  --------  ----------    -----------------------  * EQW8LBVR
00571 *  I HANDLE I  I USER I  I ADRESS I    I RECEPTION-MESSAGE   I  * EQW8LBVR
00572 *  ----------  --------  ----------    -----------------------  * EQW8LBVR
00573 *                                                               * EQW8LBVR
00574 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
00575 *                                                                 EQW8LBVR
00576  MODULE-ENTREE.                                                   EQW8LBVR
00577 *-------------*                                                   EQW8LBVR
00578      PERFORM INIT-USER          THRU                              EQW8LBVR
00579              FIN-INIT-USER.                                       EQW8LBVR
00580 *                                                                 EQW8LBVR
00581      PERFORM INIT-ADDRESS       THRU                              EQW8LBVR
00582              FIN-INIT-ADDRESS.                                    EQW8LBVR
00583 *                                                                 EQW8LBVR
00584      PERFORM RECEPTION-MESSAGE  THRU                              EQW8LBVR
00585              FIN-RECEPTION-MESSAGE.                               EQW8LBVR
00586 *                                                                 EQW8LBVR
00587  FIN-MODULE-ENTREE.   EXIT.                                       EQW8LBVR
00588 *                                                                 EQW8LBVR
00589 ***************************************************************** EQW8LBVR
00590 *    INITIALISATION DES ZONES QUI NE PEUVENT PAS ETRE EN VALUE  * EQW8LBVR
00591 ***************************************************************** EQW8LBVR
00592  INIT-USER.                                                       EQW8LBVR
00593 *---------*                                                       EQW8LBVR
00594      MOVE LOW-VALUE TO Z-MAP.                                     EQW8LBVR
00595      MOVE   'FB02'  TO NOM-TACHE,                                 EQW8LBVR
00596                        NOM-TACHE-MAP,                             EQW8LBVR
00597                        NOM-TACHE-MAPSET,                          EQW8LBVR
00598                        NOM-TACHE-PROG.                            EQW8LBVR
00599      MOVE 'AA00'    TO NOM-LEVEL-SIGN.                            EQW8LBVR
00600      MOVE EIBTRMID  TO TRMID-TS-ECRAN,                            EQW8LBVR
00601                        W-XTERMIC.                                 EQW8LBVR
00602      MOVE NOM-TACHE TO W-XTRANSC,                                 EQW8LBVR
00603                        TRNID-TS-ECRAN.                            EQW8LBVR
00604      PERFORM RETRIEVE-DATA THRU                                   EQW8LBVR
00605              FIN-RETRIEVE-DATA.                                   EQW8LBVR
00606 *                     TROUVE : CODE-RETOUR = 0                    EQW8LBVR
00607      IF TROUVE                                                    EQW8LBVR
00608         MOVE 1 TO TYPE-PASSAGE                                    EQW8LBVR
00609      END-IF.                                                      EQW8LBVR
00610 *                                                                 EQW8LBVR
00611 *  SI ON N'EST PAS PASSE PAR LA SIGNATURE                         EQW8LBVR
00612 *  ET SI ON N'A PAS ETE APPELE PAR START  ERREUR                  EQW8LBVR
00613 *                                                                 EQW8LBVR
00614 *           TYPE-PASSAGE = 1  ===> PASSAGE-PAR-START              EQW8LBVR
00615      IF NOT PASSAGE-PAR-START                                     EQW8LBVR
00616         IF  EIBCALEN  = 0                                         EQW8LBVR
00617 *           MESSAGE ERREUR; RETOUR A SIGNATURE                    EQW8LBVR
00618             MOVE 'SQ001'         TO COM-GENE-MESANO               EQW8LBVR
00619                                     COM-CODERR                    EQW8LBVR
00620             MOVE CODE-LEVEL-SIGN TO Z-FONCTION                    EQW8LBVR
00621             PERFORM MODULE-SORTIE   THRU                          EQW8LBVR
00622                     FIN-MODULE-SORTIE                             EQW8LBVR
00623         ELSE                                                      EQW8LBVR
00624 *          ICI  ON RECUPERE LA COMMAREA PASSEE PAR XCTL OU RETURN EQW8LBVR
00625            MOVE DFHCOMMAREA      TO Z-COMMAREA                    EQW8LBVR
00626         END-IF                                                    EQW8LBVR
00627      END-IF.                                                      EQW8LBVR
00628 *                                                                 EQW8LBVR
00629 * RECUPERATION DU CODE COMPAGNIE POUR IDENTIFICATION DES TABLES   EQW8LBVR
00630 *    AAMENUXX   ET  AACONVXX                                      EQW8LBVR
00631 *                                                                 EQW8LBVR
00632      MOVE COM-GENE-CODCIE-PRINCIPAL      TO TABLE-SUFF.           EQW8LBVR
00633 *                                                                 EQW8LBVR
00634 *                                                                 EQW8LBVR
00635 * IDENTIFICATION DE LA TS DE PAGINATION                           EQW8LBVR
00636 *                                                                 EQW8LBVR
00637      MOVE EIBTRMID   TO PAGE-TS-PREF.                             EQW8LBVR
00638      MOVE 'PAG'      TO PAGE-TS-CONV.                             EQW8LBVR
00639 *                                                                 EQW8LBVR
00640 * IDENTIFICATION DE LA TS DE CONFIDENTIALITE                      EQW8LBVR
00641 *                                                                 EQW8LBVR
00642      MOVE EIBTRMID   TO CONF-TS-PREF.                             EQW8LBVR
00643      MOVE 'CF0'      TO CONF-TS-CONV.                             EQW8LBVR
00644 *                                                                 EQW8LBVR
00645 * IDENTIFICATION DE  LA TS DE L'APPLICATIVE                       EQW8LBVR
00646 *                                                                 EQW8LBVR
00647      MOVE EIBTRMID   TO APP-TS-PREF.                              EQW8LBVR
00648      MOVE 'APP'      TO APP-TS-CONV.                              EQW8LBVR
00649      MOVE   'NON'    TO DEBUGGIN.                                 EQW8LBVR
00650 ****************** POUR CONVERSATION **************************** EQW8LBVR
00651 *TK191190 POUR EXIT-SELECTION-DE-PLAN : IDENTIFICATION DE TS-PLAN EQW8LBVR
00652 ***************************************************************** EQW8LBVR
00653 *  POUR CONVERSATION SANS  SQL : LE PLAN PROPOSE = 'UTILPLN'      EQW8LBVR
00654  ++INCLUDE SQKCPLIU                                               EQW8LBVR
00655 */                                                                EQW8LBVR
00656  FIN-INIT-USER.  EXIT.                                            EQW8LBVR
00657 *                                                                 EQW8LBVR
00658 ***************************************************************   EQW8LBVR
00659 * INIT-ADDRESS.  ADRESSAGE DES ZONES SYSTEME DE CICS              EQW8LBVR
00660 *                CREATION TS PLAN POUR EXIT-SELECTION-PLAN        EQW8LBVR
00661 ***************************************************************   EQW8LBVR
00662  ++INCLUDE SQKCADDB                                               EQW8LBVR
00663 */                                                                EQW8LBVR
00664 /                                                                 EQW8LBVR
00665 ***************************************************************** EQW8LBVR
00666 * DETERMINATION DU TRAITEMENT EN FONCTION DE L'ENVIRONNEMENT    * EQW8LBVR
00667 ***************************************************************** EQW8LBVR
00668  RECEPTION-MESSAGE.                                               EQW8LBVR
00669 *-----------------*                                               EQW8LBVR
00670 *                                      PASSAGE PAR START          EQW8LBVR
00671      IF  PASSAGE-PAR-START                                        EQW8LBVR
00672          PERFORM DELETE-TS-ECRAN THRU                             EQW8LBVR
00673                  FIN-DELETE-TS-ECRAN                              EQW8LBVR
00674          MOVE    CODE-TRAITEMENT-AUTOMATIQUE TO Z-FONCTION        EQW8LBVR
00675          MOVE    1               TO NUMERO-PASSAGE                EQW8LBVR
00676          GO TO                   FIN-RECEPTION-MESSAGE            EQW8LBVR
00677      END-IF.                                                      EQW8LBVR
00678 *                                            AUTRE TACHE          EQW8LBVR
00679      IF  EIBTRNID NOT = NOM-TACHE                                 EQW8LBVR
00680          PERFORM DELETE-TS-ECRAN THRU                             EQW8LBVR
00681                  FIN-DELETE-TS-ECRAN                              EQW8LBVR
00682          MOVE   CODE-TRAITEMENT-AUTOMATIQUE TO Z-FONCTION         EQW8LBVR
00683          MOVE   1               TO NUMERO-PASSAGE                 EQW8LBVR
00684          GO TO                  FIN-RECEPTION-MESSAGE             EQW8LBVR
00685      END-IF.                                                      EQW8LBVR
00686 *                                            FAST PATH            EQW8LBVR
00687      IF EIBTRNID = Z-COMMAREA-TACHE-JUMP                          EQW8LBVR
00688         PERFORM DELETE-TS-ECRAN THRU                              EQW8LBVR
00689                 FIN-DELETE-TS-ECRAN                               EQW8LBVR
00690         MOVE    SPACES          TO  Z-COMMAREA-TACHE-JUMP         EQW8LBVR
00691         MOVE    CODE-TRAITEMENT-AUTOMATIQUE TO Z-FONCTION         EQW8LBVR
00692         MOVE    1               TO NUMERO-PASSAGE                 EQW8LBVR
00693         GO TO                   FIN-RECEPTION-MESSAGE             EQW8LBVR
00694      END-IF.                                                      EQW8LBVR
00695 *                                                                 EQW8LBVR
00696      PERFORM RECEIVE-MAP THRU                                     EQW8LBVR
00697              FIN-RECEIVE-MAP.                                     EQW8LBVR
00698      MOVE    EIBAID             TO WORKAID.                       EQW8LBVR
00699 *                                            LEVEL-MAX            EQW8LBVR
00700      IF  TOUCHE-PF4 OR TOUCHE-PF16                                EQW8LBVR
00701          MOVE CODE-LEVEL-MAX TO Z-FONCTION                        EQW8LBVR
00702      END-IF.                                                      EQW8LBVR
00703 *                                            DERNIER-AFFICHAGE    EQW8LBVR
00704 *    REAFFICHAGE DE L'ECRAN A PARTIR DE LA                        EQW8LBVR
00705 *    TS-ECRAN                                                     EQW8LBVR
00706 *                                            DERNIER-AFFICHAGE    EQW8LBVR
00707      IF  TOUCHE-PF5 OR TOUCHE-PF17                                EQW8LBVR
00708          MOVE CODE-LAST-AFF TO Z-FONCTION                         EQW8LBVR
00709          MOVE LOW-VALUE     TO Z-MAP                              EQW8LBVR
00710          MOVE NOM-TACHE     TO NOM-TACHE-RETOUR                   EQW8LBVR
00711          MOVE SPACES        TO COM-GENE-REAF                      EQW8LBVR
00712          PERFORM FUSION-TS-ECRAN THRU                             EQW8LBVR
00713                  FIN-FUSION-TS-ECRAN                              EQW8LBVR
00714          PERFORM SEND-MAP THRU                                    EQW8LBVR
00715                  FIN-SEND-MAP                                     EQW8LBVR
00716          PERFORM RETOUR-COMMAREA THRU                             EQW8LBVR
00717                  FIN-RETOUR-COMMAREA                              EQW8LBVR
00718      END-IF.                                                      EQW8LBVR
00719 *                                                                 EQW8LBVR
00720 * RECUPERATION DU CONTENU DE LA TS SI LA SORTIE N'EST PAS         EQW8LBVR
00721 * DEFINITIVE (LA SORTIE NE PEUT ETRE SURE QUE DANS                EQW8LBVR
00722 * MODULE-SORTIE)                                                  EQW8LBVR
00723 *                                                                 EQW8LBVR
00724          PERFORM FUSION-TS-ECRAN THRU                             EQW8LBVR
00725                  FIN-FUSION-TS-ECRAN.                             EQW8LBVR
00726 *                                            LEVEL-PREC           EQW8LBVR
00727 * POUR LES CONVERSATIONS SEULEMENT           LEVEL-PREC           EQW8LBVR
00728 *                                            LEVEL-PREC           EQW8LBVR
00729      IF  TOUCHE-PF12 OR TOUCHE-PF24                               EQW8LBVR
00730          MOVE CODE-LEVEL-PREC TO Z-FONCTION                       EQW8LBVR
00731      END-IF.                                                      EQW8LBVR
00732 *                                            AIDE                 EQW8LBVR
00733      IF  TOUCHE-PF1 OR TOUCHE-PF13                                EQW8LBVR
00734          MOVE CODE-TRAITEMENT-NORMAL TO Z-FONCTION                EQW8LBVR
00735      END-IF.                                                      EQW8LBVR
00736 *                                            LEVEL-SUP            EQW8LBVR
00737      IF  TOUCHE-PF3 OR TOUCHE-PF15                                EQW8LBVR
00738          MOVE CODE-LEVEL-SUP TO Z-FONCTION                        EQW8LBVR
00739      END-IF.                                                      EQW8LBVR
00740 *                                            SUITE                EQW8LBVR
00741      IF  TOUCHE-ENTER                                             EQW8LBVR
00742          MOVE CODE-TRAITEMENT-NORMAL TO Z-FONCTION                EQW8LBVR
00743      END-IF.                                                      EQW8LBVR
00744 *                                            SIGNATURE            EQW8LBVR
00745      IF  TOUCHE-CLEAR                                             EQW8LBVR
00746          MOVE CODE-LEVEL-SIGN        TO Z-FONCTION                EQW8LBVR
00747          GO TO FIN-RECEPTION-MESSAGE                              EQW8LBVR
00748      END-IF.                                                      EQW8LBVR
00749 *                                                                 EQW8LBVR
00750  FIN-RECEPTION-MESSAGE.  EXIT.                                    EQW8LBVR
00751 /                                                                 EQW8LBVR
00752 ***************************************************************** EQW8LBVR
00753 * RECEIVE-MAP. RECEPTION DE LA MAP                                EQW8LBVR
00754 ***************************************************************** EQW8LBVR
00755 *                                                                 EQW8LBVR
00756  ++INCLUDE SQKCRECV                                               EQW8LBVR
00757 /                                                                 EQW8LBVR
00758 ***************************************************************** EQW8LBVR
00759 *        SECTION  DE PROGRAMME POUR LE TRAITEMENT DU MDT/OFF      EQW8LBVR
00760 ***************************************************************** EQW8LBVR
00761  ++INCLUDE SQKCMDTB                                               EQW8LBVR
00762 /                                                                 EQW8LBVR
00763 ***************************************************************** EQW8LBVR
00764 *   SECTION  DE PROGRAMME POUR L'ECRITURE DE LA TS PLAN           EQW8LBVR
00765 ***************************************************************** EQW8LBVR
00766  ++INCLUDE SQKCWRPL                                               EQW8LBVR
00767 /                                                                 EQW8LBVR
00768 ***************************************************************** EQW8LBVR
00769 *        MISE-A-JOUR-TS-ECRAN     GENEREE                         EQW8LBVR
00770 ***************************************************************** EQW8LBVR
00771 *                                                                 EQW8LBVR
00772  MISE-A-JOUR-TS-ECRAN.                                            EQW8LBVR
00773 *                                                                 EQW8LBVR
00774 *                                                                 EQW8LBVR
00775      IF ECR-XTRMTRACL    = ZEROS AND                              EQW8LBVR
00776         ECR-XTRMTRACA  NOT = EFFACE-FIN-ZONE                      EQW8LBVR
00777         MOVE TS-ECR-XTRMTRACO TO ECR-XTRMTRACO                    EQW8LBVR
00778         MOVE TS-ECR-XTRMTRACA TO ECR-XTRMTRACA                    EQW8LBVR
00779      ELSE                                                         EQW8LBVR
00780         MOVE ECR-XTRMTRACO     TO TS-ECR-XTRMTRACO                EQW8LBVR
00781         MOVE LOW-VALUE      TO TS-ECR-XTRMTRACA                   EQW8LBVR
00782      END-IF.                                                      EQW8LBVR
00783 *                                                                 EQW8LBVR
00784      IF ECR-XAPPLILL    = ZEROS AND                               EQW8LBVR
00785         ECR-XAPPLILA  NOT = EFFACE-FIN-ZONE                       EQW8LBVR
00786         MOVE TS-ECR-XAPPLILO TO ECR-XAPPLILO                      EQW8LBVR
00787         MOVE TS-ECR-XAPPLILA TO ECR-XAPPLILA                      EQW8LBVR
00788      ELSE                                                         EQW8LBVR
00789         MOVE ECR-XAPPLILO     TO TS-ECR-XAPPLILO                  EQW8LBVR
00790         MOVE LOW-VALUE      TO TS-ECR-XAPPLILA                    EQW8LBVR
00791      END-IF.                                                      EQW8LBVR
00792 *                                                                 EQW8LBVR
00793      IF ECR-XJOURDL    = ZEROS AND                                EQW8LBVR
00794         ECR-XJOURDA  NOT = EFFACE-FIN-ZONE                        EQW8LBVR
00795         MOVE TS-ECR-XJOURDO TO ECR-XJOURDO                        EQW8LBVR
00796         MOVE TS-ECR-XJOURDA TO ECR-XJOURDA                        EQW8LBVR
00797      ELSE                                                         EQW8LBVR
00798         MOVE ECR-XJOURDO     TO TS-ECR-XJOURDO                    EQW8LBVR
00799         MOVE LOW-VALUE      TO TS-ECR-XJOURDA                     EQW8LBVR
00800      END-IF.                                                      EQW8LBVR
00801 *                                                                 EQW8LBVR
00802      IF ECR-XRACFLL    = ZEROS AND                                EQW8LBVR
00803         ECR-XRACFLA  NOT = EFFACE-FIN-ZONE                        EQW8LBVR
00804         MOVE TS-ECR-XRACFLO TO ECR-XRACFLO                        EQW8LBVR
00805         MOVE TS-ECR-XRACFLA TO ECR-XRACFLA                        EQW8LBVR
00806      ELSE                                                         EQW8LBVR
00807         MOVE ECR-XRACFLO     TO TS-ECR-XRACFLO                    EQW8LBVR
00808         MOVE LOW-VALUE      TO TS-ECR-XRACFLA                     EQW8LBVR
00809      END-IF.                                                      EQW8LBVR
00810 *                                                                 EQW8LBVR
00811      IF ECR-XHEUREDL    = ZEROS AND                               EQW8LBVR
00812         ECR-XHEUREDA  NOT = EFFACE-FIN-ZONE                       EQW8LBVR
00813         MOVE TS-ECR-XHEUREDO TO ECR-XHEUREDO                      EQW8LBVR
00814         MOVE TS-ECR-XHEUREDA TO ECR-XHEUREDA                      EQW8LBVR
00815      ELSE                                                         EQW8LBVR
00816         MOVE ECR-XHEUREDO     TO TS-ECR-XHEUREDO                  EQW8LBVR
00817         MOVE LOW-VALUE      TO TS-ECR-XHEUREDA                    EQW8LBVR
00818      END-IF.                                                      EQW8LBVR
00819 *                                                                 EQW8LBVR
00820      IF ECR-GESCLIL    = ZEROS AND                                EQW8LBVR
00821         ECR-GESCLIA  NOT = EFFACE-FIN-ZONE                        EQW8LBVR
00822         MOVE TS-ECR-GESCLIO TO ECR-GESCLIO                        EQW8LBVR
00823         MOVE TS-ECR-GESCLIA TO ECR-GESCLIA                        EQW8LBVR
00824      ELSE                                                         EQW8LBVR
00825         MOVE ECR-GESCLIO     TO TS-ECR-GESCLIO                    EQW8LBVR
00826         MOVE LOW-VALUE      TO TS-ECR-GESCLIA                     EQW8LBVR
00827         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00828      END-IF.                                                      EQW8LBVR
00829 *                                                                 EQW8LBVR
00830      IF ECR-RAICL    = ZEROS AND                                  EQW8LBVR
00831         ECR-RAICA  NOT = EFFACE-FIN-ZONE                          EQW8LBVR
00832         MOVE TS-ECR-RAICO TO ECR-RAICO                            EQW8LBVR
00833         MOVE TS-ECR-RAICA TO ECR-RAICA                            EQW8LBVR
00834      ELSE                                                         EQW8LBVR
00835         MOVE ECR-RAICO     TO TS-ECR-RAICO                        EQW8LBVR
00836         MOVE LOW-VALUE      TO TS-ECR-RAICA                       EQW8LBVR
00837         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00838      END-IF.                                                      EQW8LBVR
00839 *                                                                 EQW8LBVR
00840      IF ECR-NOMCL    = ZEROS AND                                  EQW8LBVR
00841         ECR-NOMCA  NOT = EFFACE-FIN-ZONE                          EQW8LBVR
00842         MOVE TS-ECR-NOMCO TO ECR-NOMCO                            EQW8LBVR
00843         MOVE TS-ECR-NOMCA TO ECR-NOMCA                            EQW8LBVR
00844      ELSE                                                         EQW8LBVR
00845         MOVE ECR-NOMCO     TO TS-ECR-NOMCO                        EQW8LBVR
00846         MOVE LOW-VALUE      TO TS-ECR-NOMCA                       EQW8LBVR
00847         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00848      END-IF.                                                      EQW8LBVR
00849 *                                                                 EQW8LBVR
00850      IF ECR-PERNUMXL  = ZEROS AND                                 EQW8LBVR
00851         ECR-PERNUMXA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
00852         MOVE TS-ECR-PERNUMXO TO ECR-PERNUMXO                      EQW8LBVR
00853         MOVE TS-ECR-PERNUMXA TO ECR-PERNUMXA                      EQW8LBVR
00854      ELSE                                                         EQW8LBVR
00855         MOVE ECR-PERNUMXO   TO TS-ECR-PERNUMXO                    EQW8LBVR
00856         MOVE LOW-VALUE      TO TS-ECR-PERNUMXA                    EQW8LBVR
00857         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00858      END-IF.                                                      EQW8LBVR
00859 *                                                                 EQW8LBVR
00860      IF ECR-PERSTACL    = ZEROS AND                               EQW8LBVR
00861         ECR-PERSTACA  NOT = EFFACE-FIN-ZONE                       EQW8LBVR
00862         MOVE TS-ECR-PERSTACO TO ECR-PERSTACO                      EQW8LBVR
00863         MOVE TS-ECR-PERSTACA TO ECR-PERSTACA                      EQW8LBVR
00864      ELSE                                                         EQW8LBVR
00865         MOVE ECR-PERSTACO     TO TS-ECR-PERSTACO                  EQW8LBVR
00866         MOVE LOW-VALUE      TO TS-ECR-PERSTACA                    EQW8LBVR
00867         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00868      END-IF.                                                      EQW8LBVR
00869 *                                                                 EQW8LBVR
00870      IF ECR-PERSALNL  = ZEROS AND                                 EQW8LBVR
00871         ECR-PERSALNA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
00872         MOVE TS-ECR-PERSALNO TO ECR-PERSALNO                      EQW8LBVR
00873         MOVE TS-ECR-PERSALNA TO ECR-PERSALNA                      EQW8LBVR
00874      ELSE                                                         EQW8LBVR
00875         MOVE ECR-PERSALNO   TO TS-ECR-PERSALNO                    EQW8LBVR
00876         MOVE LOW-VALUE      TO TS-ECR-PERSALNA                    EQW8LBVR
00877         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00878      END-IF.                                                      EQW8LBVR
00879 *                                                                 EQW8LBVR
00880      IF ECR-PERTITLL = ZEROS AND                                  EQW8LBVR
00881         ECR-PERTITLA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
00882         MOVE TS-ECR-PERTITLO TO ECR-PERTITLO                      EQW8LBVR
00883         MOVE TS-ECR-PERTITLA TO ECR-PERTITLA                      EQW8LBVR
00884      ELSE                                                         EQW8LBVR
00885         MOVE ECR-PERTITLO  TO TS-ECR-PERTITLO                     EQW8LBVR
00886         MOVE LOW-VALUE      TO TS-ECR-PERTITLA                    EQW8LBVR
00887         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00888      END-IF.                                                      EQW8LBVR
00889 *                                                                 EQW8LBVR
00890      IF ECR-PERNOMLL   = ZEROS AND                                EQW8LBVR
00891         ECR-PERNOMLA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
00892         MOVE TS-ECR-PERNOMLO TO ECR-PERNOMLO                      EQW8LBVR
00893         MOVE TS-ECR-PERNOMLA TO ECR-PERNOMLA                      EQW8LBVR
00894      ELSE                                                         EQW8LBVR
00895         MOVE ECR-PERNOMLO    TO TS-ECR-PERNOMLO                   EQW8LBVR
00896         MOVE LOW-VALUE      TO TS-ECR-PERNOMLA                    EQW8LBVR
00897         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00898      END-IF.                                                      EQW8LBVR
00899 *                                                                 EQW8LBVR
00900      IF ECR-PERPRELL  = ZEROS AND                                 EQW8LBVR
00901         ECR-PERPRELA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
00902         MOVE TS-ECR-PERPRELO TO ECR-PERPRELO                      EQW8LBVR
00903         MOVE TS-ECR-PERPRELA TO ECR-PERPRELA                      EQW8LBVR
00904      ELSE                                                         EQW8LBVR
00905         MOVE ECR-PERPRELO   TO TS-ECR-PERPRELO                    EQW8LBVR
00906         MOVE LOW-VALUE      TO TS-ECR-PERPRELA                    EQW8LBVR
00907         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00908      END-IF.                                                      EQW8LBVR
00909 *                                                                 EQW8LBVR
00910      IF ECR-PERNAIDL  = ZEROS AND                                 EQW8LBVR
00911         ECR-PERNAIDA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
00912         MOVE TS-ECR-PERNAIDO TO ECR-PERNAIDO                      EQW8LBVR
00913         MOVE TS-ECR-PERNAIDA TO ECR-PERNAIDA                      EQW8LBVR
00914      ELSE                                                         EQW8LBVR
00915         MOVE ECR-PERNAIDO   TO TS-ECR-PERNAIDO                    EQW8LBVR
00916         MOVE LOW-VALUE      TO TS-ECR-PERNAIDA                    EQW8LBVR
00917         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00918      END-IF.                                                      EQW8LBVR
00919 *                                                                 EQW8LBVR
00920      IF ECR-PERSEXCL    = ZEROS AND                               EQW8LBVR
00921         ECR-PERSEXCA  NOT = EFFACE-FIN-ZONE                       EQW8LBVR
00922         MOVE TS-ECR-PERSEXCO TO ECR-PERSEXCO                      EQW8LBVR
00923         MOVE TS-ECR-PERSEXCA TO ECR-PERSEXCA                      EQW8LBVR
00924      ELSE                                                         EQW8LBVR
00925         MOVE ECR-PERSEXCO     TO TS-ECR-PERSEXCO                  EQW8LBVR
00926         MOVE LOW-VALUE      TO TS-ECR-PERSEXCA                    EQW8LBVR
00927         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00928      END-IF.                                                      EQW8LBVR
00929 *                                                                 EQW8LBVR
00930      IF ECR-PERENCNL  = ZEROS AND                                 EQW8LBVR
00931         ECR-PERENCNA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
00932         MOVE TS-ECR-PERENCNO TO ECR-PERENCNO                      EQW8LBVR
00933         MOVE TS-ECR-PERENCNA TO ECR-PERENCNA                      EQW8LBVR
00934      ELSE                                                         EQW8LBVR
00935         MOVE ECR-PERENCNO   TO TS-ECR-PERENCNO                    EQW8LBVR
00936         MOVE LOW-VALUE      TO TS-ECR-PERENCNA                    EQW8LBVR
00937         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00938      END-IF.                                                      EQW8LBVR
00939 *                                                                 EQW8LBVR
00940      IF ECR-PERMATCL  = ZEROS AND                                 EQW8LBVR
00941         ECR-PERMATCA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
00942         MOVE TS-ECR-PERMATCO TO ECR-PERMATCO                      EQW8LBVR
00943         MOVE TS-ECR-PERMATCA TO ECR-PERMATCA                      EQW8LBVR
00944      ELSE                                                         EQW8LBVR
00945         MOVE ECR-PERMATCO   TO TS-ECR-PERMATCO                    EQW8LBVR
00946         MOVE LOW-VALUE      TO TS-ECR-PERMATCA                    EQW8LBVR
00947         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00948      END-IF.                                                      EQW8LBVR
00949 *                                                                 EQW8LBVR
00950      IF ECR-PERPROCL  = ZEROS AND                                 EQW8LBVR
00951         ECR-PERPROCA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
00952         MOVE TS-ECR-PERPROCO TO ECR-PERPROCO                      EQW8LBVR
00953         MOVE TS-ECR-PERPROCA TO ECR-PERPROCA                      EQW8LBVR
00954      ELSE                                                         EQW8LBVR
00955         MOVE ECR-PERPROCO   TO TS-ECR-PERPROCO                    EQW8LBVR
00956         MOVE LOW-VALUE      TO TS-ECR-PERPROCA                    EQW8LBVR
00957         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00958      END-IF.                                                      EQW8LBVR
00959 *                                                                 EQW8LBVR
00960      IF ECR-PERPROLL  = ZEROS AND                                 EQW8LBVR
00961         ECR-PERPROLA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
00962         MOVE TS-ECR-PERPROLO TO ECR-PERPROLO                      EQW8LBVR
00963         MOVE TS-ECR-PERPROLA TO ECR-PERPROLA                      EQW8LBVR
00964      ELSE                                                         EQW8LBVR
00965         MOVE ECR-PERPROLO   TO TS-ECR-PERPROLO                    EQW8LBVR
00966         MOVE LOW-VALUE      TO TS-ECR-PERPROLA                    EQW8LBVR
00967         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00968      END-IF.                                                      EQW8LBVR
00969 *                                                                 EQW8LBVR
00970      IF ECR-PRMTYPC1L   = ZEROS AND                               EQW8LBVR
00971         ECR-PRMTYPC1A NOT = EFFACE-FIN-ZONE                       EQW8LBVR
00972         MOVE TS-ECR-PRMTYPC1O TO ECR-PRMTYPC1O                    EQW8LBVR
00973         MOVE TS-ECR-PRMTYPC1A TO ECR-PRMTYPC1A                    EQW8LBVR
00974      ELSE                                                         EQW8LBVR
00975         MOVE ECR-PRMTYPC1O    TO TS-ECR-PRMTYPC1O                 EQW8LBVR
00976         MOVE LOW-VALUE      TO TS-ECR-PRMTYPC1A                   EQW8LBVR
00977         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00978      END-IF.                                                      EQW8LBVR
00979 *                                                                 EQW8LBVR
00980      IF ECR-PRMOBTD1L = ZEROS AND                                 EQW8LBVR
00981         ECR-PRMOBTD1A NOT = EFFACE-FIN-ZONE                       EQW8LBVR
00982         MOVE TS-ECR-PRMOBTD1O TO ECR-PRMOBTD1O                    EQW8LBVR
00983         MOVE TS-ECR-PRMOBTD1A TO ECR-PRMOBTD1A                    EQW8LBVR
00984      ELSE                                                         EQW8LBVR
00985         MOVE ECR-PRMOBTD1O  TO TS-ECR-PRMOBTD1O                   EQW8LBVR
00986         MOVE LOW-VALUE      TO TS-ECR-PRMOBTD1A                   EQW8LBVR
00987         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00988      END-IF.                                                      EQW8LBVR
00989 *                                                                 EQW8LBVR
00990      IF ECR-PRMTYPC2L = ZEROS AND                                 EQW8LBVR
00991         ECR-PRMTYPC2A NOT = EFFACE-FIN-ZONE                       EQW8LBVR
00992         MOVE TS-ECR-PRMTYPC2O TO ECR-PRMTYPC2O                    EQW8LBVR
00993         MOVE TS-ECR-PRMTYPC2A TO ECR-PRMTYPC2A                    EQW8LBVR
00994      ELSE                                                         EQW8LBVR
00995         MOVE ECR-PRMTYPC2O TO TS-ECR-PRMTYPC2O                    EQW8LBVR
00996         MOVE LOW-VALUE      TO TS-ECR-PRMTYPC2A                   EQW8LBVR
00997         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
00998      END-IF.                                                      EQW8LBVR
00999 *                                                                 EQW8LBVR
01000      IF ECR-PRMOBTD2L = ZEROS AND                                 EQW8LBVR
01001         ECR-PRMOBTD2A NOT = EFFACE-FIN-ZONE                       EQW8LBVR
01002         MOVE TS-ECR-PRMOBTD2O TO ECR-PRMOBTD2O                    EQW8LBVR
01003         MOVE TS-ECR-PRMOBTD2A TO ECR-PRMOBTD2A                    EQW8LBVR
01004      ELSE                                                         EQW8LBVR
01005         MOVE ECR-PRMOBTD2O  TO TS-ECR-PRMOBTD2O                   EQW8LBVR
01006         MOVE LOW-VALUE      TO TS-ECR-PRMOBTD2A                   EQW8LBVR
01007         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
01008      END-IF.                                                      EQW8LBVR
01009 *                                                                 EQW8LBVR
01010      IF ECR-PERCOACL = ZEROS AND                                  EQW8LBVR
01011         ECR-PERCOACA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
01012         MOVE TS-ECR-PERCOACO TO ECR-PERCOACO                      EQW8LBVR
01013         MOVE TS-ECR-PERCOACA TO ECR-PERCOACA                      EQW8LBVR
01014      ELSE                                                         EQW8LBVR
01015         MOVE ECR-PERCOACO  TO TS-ECR-PERCOACO                     EQW8LBVR
01016         MOVE LOW-VALUE      TO TS-ECR-PERCOACA                    EQW8LBVR
01017         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
01018      END-IF.                                                      EQW8LBVR
01019 *                                                                 EQW8LBVR
01020      IF ECR-ANPANCNL = ZEROS AND                                  EQW8LBVR
01021         ECR-ANPANCNA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
01022         MOVE TS-ECR-ANPANCNO TO ECR-ANPANCNO                      EQW8LBVR
01023         MOVE TS-ECR-ANPANCNA TO ECR-ANPANCNA                      EQW8LBVR
01024      ELSE                                                         EQW8LBVR
01025         MOVE ECR-ANPANCNO  TO TS-ECR-ANPANCNO                     EQW8LBVR
01026         MOVE LOW-VALUE      TO TS-ECR-ANPANCNA                    EQW8LBVR
01027         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
01028      END-IF.                                                      EQW8LBVR
01029 *                                                                 EQW8LBVR
01030      IF ECR-ANPINDCL = ZEROS AND                                  EQW8LBVR
01031         ECR-ANPINDCA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
01032         MOVE TS-ECR-ANPINDCO TO ECR-ANPINDCO                      EQW8LBVR
01033         MOVE TS-ECR-ANPINDCA TO ECR-ANPINDCA                      EQW8LBVR
01034      ELSE                                                         EQW8LBVR
01035         MOVE ECR-ANPINDCO  TO TS-ECR-ANPINDCO                     EQW8LBVR
01036         MOVE LOW-VALUE      TO TS-ECR-ANPINDCA                    EQW8LBVR
01037         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
01038      END-IF.                                                      EQW8LBVR
01039 *                                                                 EQW8LBVR
01040      IF ECR-ANPNBJNL = ZEROS AND                                  EQW8LBVR
01041         ECR-ANPNBJNA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
01042         MOVE TS-ECR-ANPNBJNO TO ECR-ANPNBJNO                      EQW8LBVR
01043         MOVE TS-ECR-ANPNBJNA TO ECR-ANPNBJNA                      EQW8LBVR
01044      ELSE                                                         EQW8LBVR
01045         MOVE ECR-ANPNBJNO  TO TS-ECR-ANPNBJNO                     EQW8LBVR
01046         MOVE LOW-VALUE      TO TS-ECR-ANPNBJNA                    EQW8LBVR
01047         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
01048      END-IF.                                                      EQW8LBVR
01049 *                                                                 EQW8LBVR
01050      IF ECR-ANPMOTLL = ZEROS AND                                  EQW8LBVR
01051         ECR-ANPMOTLA NOT = EFFACE-FIN-ZONE                        EQW8LBVR
01052         MOVE TS-ECR-ANPMOTLO TO ECR-ANPMOTLO                      EQW8LBVR
01053         MOVE TS-ECR-ANPMOTLA TO ECR-ANPMOTLA                      EQW8LBVR
01054      ELSE                                                         EQW8LBVR
01055         MOVE ECR-ANPMOTLO  TO TS-ECR-ANPMOTLO                     EQW8LBVR
01056         MOVE LOW-VALUE      TO TS-ECR-ANPMOTLA                    EQW8LBVR
01057         MOVE '2'            TO ETAT-ECRAN                         EQW8LBVR
01058      END-IF.                                                      EQW8LBVR
01059 *                                                                 EQW8LBVR
01060      IF ECR-XCDECL    = ZEROS AND                                 EQW8LBVR
01061         ECR-XCDECA  NOT = EFFACE-FIN-ZONE                         EQW8LBVR
01062         MOVE TS-ECR-XCDECO TO ECR-XCDECO                          EQW8LBVR
01063         MOVE TS-ECR-XCDECA TO ECR-XCDECA                          EQW8LBVR
01064      ELSE                                                         EQW8LBVR
01065         MOVE ECR-XCDECO     TO TS-ECR-XCDECO                      EQW8LBVR
01066         MOVE LOW-VALUE      TO TS-ECR-XCDECA                      EQW8LBVR
01067      END-IF.                                                      EQW8LBVR
01068 *                                                                 EQW8LBVR
01069                                                                   EQW8LBVR
01070 *    MOVE Z-TIMER-TIMJOU    TO MTIMJOUI TS-MTIMJOUI.              EQW8LBVR
01071 *    MISE A BLANC DES MESSAGES. TK071290.CNC$MATS.                EQW8LBVR
01072      MOVE SPACES            TO ECR-XMSGILO ECR-XMSGALO.           EQW8LBVR
01073                                                                   EQW8LBVR
01074  FIN-MISE-A-JOUR-TS-ECRAN.  EXIT.                                 EQW8LBVR
01075                                                                   EQW8LBVR
01076 ******************************************************************EQW8LBVR
01077 *   MODULE DE TRAITEMENT                                         *EQW8LBVR
01078 ******************************************************************EQW8LBVR
01079  MODULE-TRAITEMENT.                                               EQW8LBVR
01080                                                                   EQW8LBVR
01081      MOVE SPACES TO DONNEES-PERSONNE OF TS-PERSONNE(1).           EQW8LBVR
01082                                                                   EQW8LBVR
01083      IF  COM-GENE-CODCNV = SPACES OR LOW-VALUE                    EQW8LBVR
01084          PERFORM READ-TS-SUSPENS-DL1 THRU FIN-READ-TS-SUSPENS-DL1 EQW8LBVR
01085          PERFORM DETER-READ-TSPERS THRU FDETER-READ-TSPERS        EQW8LBVR
01086          IF WSS-READ-TSPERS = 'O'                                 EQW8LBVR
01087             PERFORM READ-TS-PERSONNE THRU FIN-READ-TS-PERSONNE    EQW8LBVR
01088          END-IF                                                   EQW8LBVR
01089 * LA 1ERE FOIS QU'ON ENTRE AU CONTRAT, ON ACCEDE DIRECTEMENT A    EQW8LBVR
01090 * L'ECRAN FB02 ET ON PREAFFICHE CF COMME STATUT.                  EQW8LBVR
01091 * ET ON PRÈAFFICHE LES DONNÈES TITRE / NOM /PRENOM                EQW8LBVR
01092          IF COM-FB-TACHE-START = 'FB02'                           EQW8LBVR
01093             MOVE 'CF'    TO PERSTAC OF TS-PERSONNE(1)             EQW8LBVR
01094             MOVE ZERO             TO IND3                         EQW8LBVR
01095             MOVE SPACES  TO PERTITL OF TS-PERSONNE(1)             EQW8LBVR
01096             MOVE SPACES  TO PERNOML OF TS-PERSONNE(1)             EQW8LBVR
01097             MOVE SPACES  TO PERPREL OF TS-PERSONNE(1)             EQW8LBVR
01098             PERFORM VARYING IND1 FROM 1 BY 1                      EQW8LBVR
01099                  UNTIL (IND1 = 31) OR                             EQW8LBVR
01100                  (NOM-NOMC OF TS-SUSPENS1 (IND1:1) NOT = SPACES)  EQW8LBVR
01101                COMPUTE IND2 = IND1 + 1                            EQW8LBVR
01102             END-PERFORM                                           EQW8LBVR
01103             IF IND2 < 1                                           EQW8LBVR
01104                MOVE 1        TO IND2                              EQW8LBVR
01105             END-IF                                                EQW8LBVR
01106             PERFORM VARYING IND1 FROM IND2 BY 1                   EQW8LBVR
01107                 UNTIL (IND1 > 31)                                 EQW8LBVR
01108                 OR (NOM-NOMC OF TS-SUSPENS1 (IND1:1) = SPACES)    EQW8LBVR
01109                 ADD 1                TO IND3                      EQW8LBVR
01110                 MOVE NOM-NOMC OF TS-SUSPENS1 (IND1:1)             EQW8LBVR
01111                          TO PERNOML OF TS-PERSONNE(1) (IND3:1)    EQW8LBVR
01112             END-PERFORM                                           EQW8LBVR
01113             ADD 1          TO IND1                                EQW8LBVR
01114             PERFORM VARYING IND2 FROM IND1 BY 1 UNTIL             EQW8LBVR
01115                  (IND1 > 31) OR                                   EQW8LBVR
01116                  (IND2 > 31) OR                                   EQW8LBVR
01117                  (NOM-NOMC OF TS-SUSPENS1 (IND2:1) NOT = SPACES)  EQW8LBVR
01118                 ADD 1          TO IND1                            EQW8LBVR
01119             END-PERFORM                                           EQW8LBVR
01120             PERFORM VARYING IND3 FROM 1 BY 1 UNTIL (IND1 > 31)    EQW8LBVR
01121                                                    OR (IND3 > 20) EQW8LBVR
01122                 MOVE NOM-NOMC OF TS-SUSPENS1 (IND1:1)             EQW8LBVR
01123                          TO PERPREL OF TS-PERSONNE(1) (IND3:1)    EQW8LBVR
01124                 ADD 1                TO IND1                      EQW8LBVR
01125             END-PERFORM                                           EQW8LBVR
01126             MOVE NOM-RAIC OF TS-SUSPENS1                          EQW8LBVR
01127                          TO PERTITL OF TS-PERSONNE(1)             EQW8LBVR
01128             MOVE SPACES  TO COM-FB-TACHE-START                    EQW8LBVR
01129          END-IF                                                   EQW8LBVR
01130      END-IF.                                                      EQW8LBVR
01131                                                                   EQW8LBVR
01132      IF  TRAITEMENT-NORMAL                                        EQW8LBVR
01133          PERFORM M-TRAITEMENT-NORMAL THRU                         EQW8LBVR
01134                  FIN-M-TRAITEMENT-NORMAL                          EQW8LBVR
01135      END-IF.                                                      EQW8LBVR
01136                                                                   EQW8LBVR
01137      IF  TRAITEMENT-AUTOMATIQUE                                   EQW8LBVR
01138          PERFORM M-TRAITEMENT-AUTOMATIQUE THRU                    EQW8LBVR
01139                  FIN-M-TRAITEMENT-AUTOMATIQUE                     EQW8LBVR
01140      END-IF.                                                      EQW8LBVR
01141                                                                   EQW8LBVR
01142  FIN-MODULE-TRAITEMENT.  EXIT.                                    EQW8LBVR
01143                                                                   EQW8LBVR
01144  DETER-READ-TSPERS.                                               EQW8LBVR
01145                                                                   EQW8LBVR
01146      MOVE 'N' TO WSS-READ-TSPERS.                                 EQW8LBVR
01147      IF COM-FB-CODE-ACTION = 'A'                                  EQW8LBVR
01148         IF COM-FB-RANG-MAX-TSPERS NOT = ZERO AND                  EQW8LBVR
01149            COM-FB-RANG-TS-LIRE = COM-FB-RANG-MAX-TSPERS           EQW8LBVR
01150            MOVE 'O' TO WSS-READ-TSPERS                            EQW8LBVR
01151         END-IF                                                    EQW8LBVR
01152      ELSE                                                         EQW8LBVR
01153         MOVE 'O' TO WSS-READ-TSPERS                               EQW8LBVR
01154      END-IF.                                                      EQW8LBVR
01155                                                                   EQW8LBVR
01156  FDETER-READ-TSPERS.  EXIT.                                       EQW8LBVR
01157                                                                   EQW8LBVR
01158 ******************************************************************EQW8LBVR
01159 *   LECTURE DES ITEMS DE LA TS SUSPENS                           *EQW8LBVR
01160 ******************************************************************EQW8LBVR
01161  READ-TS-SUSPENS-DL1.                                             EQW8LBVR
01162                                                                   EQW8LBVR
01163 *** LECTURE ITEM 1 ***                                            EQW8LBVR
01164      MOVE +1 TO RANG-TS.                                          EQW8LBVR
01165      EXEC CICS READQ TS QUEUE   (IDENT-TS-APP)                    EQW8LBVR
01166                         INTO    (TS-SUSPENS1)                     EQW8LBVR
01167                         LENGTH  (LONG-TS-SUSPENS)                 EQW8LBVR
01168                         ITEM    (RANG-TS)                         EQW8LBVR
01169                         NOHANDLE                                  EQW8LBVR
01170      END-EXEC.                                                    EQW8LBVR
01171      IF EIBRCODE NOT = LOW-VALUE                                  EQW8LBVR
01172         MOVE 'FBR1 : ERREUR READ TS-SUSPENS1' TO MESS             EQW8LBVR
01173         GO TO ABANDON-TACHE                                       EQW8LBVR
01174      ELSE                                                         EQW8LBVR
01175         MOVE SEGTRA OF TS-SUSPENS1 TO FBMISPTR-IT1                EQW8LBVR
01176      END-IF.                                                      EQW8LBVR
01177                                                                   EQW8LBVR
01178 *** LECTURE ITEM 2 ***                                            EQW8LBVR
01179      MOVE +2 TO RANG-TS.                                          EQW8LBVR
01180      EXEC CICS READQ TS QUEUE   (IDENT-TS-APP)                    EQW8LBVR
01181                         INTO    (TS-SUSPENS2)                     EQW8LBVR
01182                         LENGTH  (LONG-TS-SUSPENS)                 EQW8LBVR
01183                         ITEM    (RANG-TS)                         EQW8LBVR
01184                         NOHANDLE                                  EQW8LBVR
01185      END-EXEC.                                                    EQW8LBVR
01186      IF EIBRCODE NOT = LOW-VALUE                                  EQW8LBVR
01187         MOVE 'FBR2 : ERREUR READ TS-SUSPENS2' TO MESS             EQW8LBVR
01188         GO TO ABANDON-TACHE                                       EQW8LBVR
01189      ELSE                                                         EQW8LBVR
01190         MOVE SEGTRA OF TS-SUSPENS2 TO FBMISPTR-IT2                EQW8LBVR
01191      END-IF.                                                      EQW8LBVR
01192                                                                   EQW8LBVR
01193  FIN-READ-TS-SUSPENS-DL1. EXIT.                                   EQW8LBVR
01194                                                                   EQW8LBVR
01195 ******************************************************************EQW8LBVR
01196 *   LECTURE DE LA TS PERSONNE                                   * EQW8LBVR
01197 ******************************************************************EQW8LBVR
01198  READ-TS-PERSONNE.                                                EQW8LBVR
01199                                                                   EQW8LBVR
01200 *** LECTURE RANG COM-FB-RANG-TS-PERS ***                          EQW8LBVR
01201      EXEC CICS READQ TS QUEUE   (COM-FB-IDENT-TSPERS)             EQW8LBVR
01202                         INTO    (TS-PERSONNE)                     EQW8LBVR
01203                         LENGTH  (LENGTH OF TS-PERSONNE)           EQW8LBVR
01204                         ITEM    (COM-FB-RANG-TS-LIRE)             EQW8LBVR
01205                         NOHANDLE                                  EQW8LBVR
01206      END-EXEC.                                                    EQW8LBVR
01207      IF EIBRCODE NOT = LOW-VALUE                                  EQW8LBVR
01208         MOVE 'PER1 : ERREUR READ TS PERSONNE' TO MESS             EQW8LBVR
01209         GO TO ABANDON-TACHE                                       EQW8LBVR
01210      END-IF.                                                      EQW8LBVR
01211                                                                   EQW8LBVR
01212  FIN-READ-TS-PERSONNE. EXIT.                                      EQW8LBVR
01213                                                                   EQW8LBVR
01214 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
01215 * MODULE DE BASE   * FB02 * TRAITEMENT AUTOMATIQUE                EQW8LBVR
01216 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
01217 *                                                               * EQW8LBVR
01218 *                -------------------------------                * EQW8LBVR
01219 *                I   TRAITEMENT AUTOMATIQUE    I                * EQW8LBVR
01220 *                -------------------------------                * EQW8LBVR
01221 *                               I                               * EQW8LBVR
01222 *                               I                               * EQW8LBVR
01223 *                -------------------------------                * EQW8LBVR
01224 *                I  REMPLISSAGE FORMAT ECRAN   I                * EQW8LBVR
01225 *                -------------------------------                * EQW8LBVR
01226 *                                                               * EQW8LBVR
01227 *                                                               * EQW8LBVR
01228 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
01229                                                                   EQW8LBVR
01230  M-TRAITEMENT-AUTOMATIQUE.                                        EQW8LBVR
01231                                                                   EQW8LBVR
01232 * RECHERCHE DES ENCHAINEMENTS DES CONVERSATIONS                   EQW8LBVR
01233 * SI PREMIERE TRANSACTION DE LA CONVERSATION                      EQW8LBVR
01234                                                                   EQW8LBVR
01235      IF COM-GENE-CODCNV NOT = SPACES AND                          EQW8LBVR
01236                               LOW-VALUE                           EQW8LBVR
01237         PERFORM INIT-CONV THRU                                    EQW8LBVR
01238                 FIN-INIT-CONV                                     EQW8LBVR
01239      ELSE                                                         EQW8LBVR
01240         MOVE COM-GENE-LNGCNV TO LONG-TS                           EQW8LBVR
01241         PERFORM READ-TS-CONF THRU                                 EQW8LBVR
01242                 FIN-READ-TS-CONF                                  EQW8LBVR
01243         PERFORM CONTROLE-CONV     THRU                            EQW8LBVR
01244                 FIN-CONTROLE-CONV                                 EQW8LBVR
01245      END-IF.                                                      EQW8LBVR
01246 *                                                                 EQW8LBVR
01247      PERFORM INIT-RECH-CONV-LIBRE THRU                            EQW8LBVR
01248              FIN-INIT-RECH-CONV-LIBRE.                            EQW8LBVR
01249 *                                                                 EQW8LBVR
01250      PERFORM REMPLISSAGE-FORMAT-ECRAN THRU                        EQW8LBVR
01251              FIN-REMPLISSAGE-FORMAT-ECRAN.                        EQW8LBVR
01252                                                                   EQW8LBVR
01253  FIN-M-TRAITEMENT-AUTOMATIQUE. EXIT.                              EQW8LBVR
01254                                                                   EQW8LBVR
01255 ***************************************************************   EQW8LBVR
01256 * LECTURE TABLE AACONVXX POSTE = COM-GENE-CODCONV POUR AVOIR      EQW8LBVR
01257 * LE LIBELLE  DE LA CONVERSATION                                  EQW8LBVR
01258 ***************************************************************   EQW8LBVR
01259  INIT-CONV.                                                       EQW8LBVR
01260                                                                   EQW8LBVR
01261      MOVE  SPACES                   TO XSPIPARM.                  EQW8LBVR
01262      MOVE 'AACONV'                  TO TABLE-PREF.                EQW8LBVR
01263      MOVE COM-GENE-CODCIE-PRINCIPAL TO TABLE-SUFF.                EQW8LBVR
01264      MOVE COM-GENE-CODCNV           TO REF-POSTE OF XSPIPARM.     EQW8LBVR
01265      PERFORM LECT-SPI-CONV THRU                                   EQW8LBVR
01266              FIN-LECT-SPI-CONV.                                   EQW8LBVR
01267      IF NON-TROUVE                                                EQW8LBVR
01268         MOVE  1      TO KONTROL                                   EQW8LBVR
01269         MOVE 'SQ006' TO COM-GENE-MESANO                           EQW8LBVR
01270                         COM-CODERR                                EQW8LBVR
01271         PERFORM MODULE-SORTIE THRU                                EQW8LBVR
01272                 FIN-MODULE-SORTIE                                 EQW8LBVR
01273      END-IF.                                                      EQW8LBVR
01274                                                                   EQW8LBVR
01275 * SI TROUVE                                                       EQW8LBVR
01276      MOVE LIB-CONV   TO COM-GENE-LIBCNV.                          EQW8LBVR
01277                                                                   EQW8LBVR
01278 * SI ON EST DANS LA PREMIERE TRANSACTION D'UNE CONVERSATION       EQW8LBVR
01279 * 1- ON PASSE DANS L'INTERFACE  AUAAL04  POUR  CONTROLE D'ACCES   EQW8LBVR
01280 *             SI   OK VERS 2. SI NON  LEVEL-SUPERIEUR             EQW8LBVR
01281 * 2- ON ACCEDE A L'INTERFACE POUR CONSTRUCTION DE LA TS           EQW8LBVR
01282 *    DE CONFIDENTIALITE                                           EQW8LBVR
01283 * 3- ON STOCKE EN COMMAREA TOUS LES ENCHAINEMENTS POSSIBLES POUR  EQW8LBVR
01284 *    CETTE CONVERSATION SI ELLE EST AUTORISEE                     EQW8LBVR
01285                                                                   EQW8LBVR
01286      PERFORM INTERFACE-CONTROLE-ACCES  THRU                       EQW8LBVR
01287              FIN-INTERFACE-CONTROLE-ACCES.                        EQW8LBVR
01288      IF COM-AU-MESSAGE NOT = SPACES AND LOW-VALUE                 EQW8LBVR
01289         MOVE    1      TO KONTROL                                 EQW8LBVR
01290         MOVE    COM-AU-MESSAGE TO COM-GENE-MESANO                 EQW8LBVR
01291                                   COM-CODERR                      EQW8LBVR
01292                                                                   EQW8LBVR
01293         PERFORM SORTIE-LEVEL-SUPERIEUR  THRU                      EQW8LBVR
01294             FIN-SORTIE-LEVEL-SUPERIEUR                            EQW8LBVR
01295      END-IF.                                                      EQW8LBVR
01296                                                                   EQW8LBVR
01297 *    FABRICATION  DE LA TS CONFIDENTIALITE CONVERSATION           EQW8LBVR
01298 *            PAR L'INTERFACE AUAAL00                              EQW8LBVR
01299                                                                   EQW8LBVR
01300      PERFORM INTERFACE-CONFIDENTIALITE THRU                       EQW8LBVR
01301              FIN-INTERFACE-CONFIDENTIALITE.                       EQW8LBVR
U3319      MOVE    COM-GENE-CODCNV  TO  COM-GENE-CODCNV-SAUVE.          EFUTSQP3
01302      MOVE    SPACES        TO COM-GENE-CODCNV.                    EQW8LBVR
01303      MOVE COM-GENE-LNGCNV TO LONG-TS.                             EQW8LBVR
01304      PERFORM READ-TS-CONF THRU                                    EQW8LBVR
01305              FIN-READ-TS-CONF.                                    EQW8LBVR
01306      MOVE    1 TO IA.                                             EQW8LBVR
01307      PERFORM STOCKAGE-ENCH-CONV  THRU                             EQW8LBVR
01308              FIN-STOCKAGE-ENCH-CONV 51 TIMES.                     EQW8LBVR
01309                                                                   EQW8LBVR
01310  FIN-INIT-CONV. EXIT.                                             EQW8LBVR
01311                                                                   EQW8LBVR
01312 * **********************************************************      EQW8LBVR
01313  CONTROLE-CONV.                                                   EQW8LBVR
01314 *-------------*                                                   EQW8LBVR
01315 *     VOIR LA TECHNIQUE  DANS LE MEME PARAGRAPHE CONTROLE-CONV    EQW8LBVR
01316 *     DU PROGRAMME  DE  MENU UNIQUE                               EQW8LBVR
01317 *                                                                 EQW8LBVR
01318 *    IF CONTROLE NON OK                                           EQW8LBVR
01319 *           MOVE 'SQ016' TO COM-GENE-MESANO                       EQW8LBVR
01320 *                           COM-CODERR                            EQW8LBVR
01321 *           PERFORM SORTIE-LEVEL-SUPERIEUR THRU                   EQW8LBVR
01322 *                   FIN-SORTIE-LEVEL-SUPERIEUR                    EQW8LBVR
01323 *    END-IF.                                                      EQW8LBVR
01324 *                                                                 EQW8LBVR
01325  FIN-CONTROLE-CONV.  EXIT.                                        EQW8LBVR
01326 *                                                                 EQW8LBVR
01327 * **********************************************************      EQW8LBVR
01328                                                                   EQW8LBVR
01329  LECT-SPI-CONV.                                                   EQW8LBVR
01330 *-------------*                                                   EQW8LBVR
01331      MOVE  'GP'                   TO FONCTION  OF XSPIPARM.       EQW8LBVR
01332      MOVE  IDENT-TABLE            TO CODTAB    OF XSPIPARM.       EQW8LBVR
01333      MOVE  '= '                   TO OPERATEUR OF XSPIPARM.       EQW8LBVR
01334      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW8LBVR
01335      MOVE SPACES                  TO TAB-AACONV.                  EQW8LBVR
01336      IF  RETCOD OF XSPIPARM  = ZERO                               EQW8LBVR
01337          MOVE 0                   TO CODE-RETOUR                  EQW8LBVR
01338          MOVE IOAREA OF XSPIPARM  TO TAB-AACONV                   EQW8LBVR
01339      ELSE                                                         EQW8LBVR
01340          MOVE 1 TO CODE-RETOUR                                    EQW8LBVR
01341      END-IF.                                                      EQW8LBVR
01342  FIN-LECT-SPI-CONV. EXIT.                                         EQW8LBVR
01343                                                                   EQW8LBVR
01344 ************************************************************      EQW8LBVR
01345  INIT-RECH-CONV-LIBRE.                                            EQW8LBVR
01346 * ------------------ *                                            EQW8LBVR
01347      MOVE 1  TO IA.                                               EQW8LBVR
01348      PERFORM RECHERCHE-CONV-LIBRE THRU                            EQW8LBVR
01349              FIN-RECHERCHE-CONV-LIBRE                             EQW8LBVR
01350              UNTIL COM-GENE-PILCNV(IA) = SPACES OR                EQW8LBVR
01351                    LOW-VALUE                                      EQW8LBVR
01352              OR IA > 50                                           EQW8LBVR
01353              OR COM-GENE-PILCNV(IA) = NOM-TACHE.                  EQW8LBVR
01354      IF IA > 50                                                   EQW8LBVR
01355         MOVE 1 TO KONTROL                                         EQW8LBVR
01356         MOVE 'SQ005'   TO COM-GENE-MESANO                         EQW8LBVR
01357                           COM-CODERR                              EQW8LBVR
01358         PERFORM MODULE-SORTIE THRU                                EQW8LBVR
01359                 FIN-MODULE-SORTIE                                 EQW8LBVR
01360      ELSE                                                         EQW8LBVR
01361         MOVE NOM-TACHE TO COM-GENE-PILCNV(IA)                     EQW8LBVR
01362         MOVE IA        TO COM-GENE-INDCNV                         EQW8LBVR
01363      END-IF.                                                      EQW8LBVR
01364  FIN-INIT-RECH-CONV-LIBRE.  EXIT.                                 EQW8LBVR
01365 *                                                                 EQW8LBVR
01366 ***************************************************************   EQW8LBVR
01367 * RECHERCHE DANS LA COMMAREA D'UN POSTE LIBRE POUR STOCKER LE     EQW8LBVR
01368 * CODE TRANSACTION DE LA CONVERSATION EN COURS                    EQW8LBVR
01369 ***************************************************************   EQW8LBVR
01370                                                                   EQW8LBVR
01371  RECHERCHE-CONV-LIBRE.                                            EQW8LBVR
01372      IF COM-GENE-PILCNV(IA) NOT = SPACES AND LOW-VALUE            EQW8LBVR
01373         ADD 1 TO IA                                               EQW8LBVR
01374      END-IF.                                                      EQW8LBVR
01375  FIN-RECHERCHE-CONV-LIBRE. EXIT.                                  EQW8LBVR
01376                                                                   EQW8LBVR
01377 ***************************************************************   EQW8LBVR
01378                                                                   EQW8LBVR
01379  STOCKAGE-ENCH-CONV.                                              EQW8LBVR
01380       MOVE COD-TRN-ECR(IA)  TO COM-GENE-ENCCNV-CODTRN(IA).        EQW8LBVR
01381       MOVE COD-MNE-ECR(IA)  TO COM-GENE-ENCCNV-CODMNE(IA).        EQW8LBVR
01382       ADD 1 TO IA.                                                EQW8LBVR
01383  FIN-STOCKAGE-ENCH-CONV. EXIT.                                    EQW8LBVR
01384                                                                   EQW8LBVR
01385 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
01386 *                                                               * EQW8LBVR
01387 *                -------------------------------                * EQW8LBVR
01388 *                I   REMPLISSAGE DE L'ECRAN    I                * EQW8LBVR
01389 *                -------------------------------                * EQW8LBVR
01390 *                               I                               * EQW8LBVR
01391 *           -----------------------------------------           * EQW8LBVR
01392 *           I                   I                   I           * EQW8LBVR
01393 * ----------------------   -------------   -----------------    * EQW8LBVR
01394 * I ZONES OBLIGATOIRES I   I PROTEGEES I   I NON PROTEGEES I    * EQW8LBVR
01395 * ----------------------   -------------   -----------------    * EQW8LBVR
01396 *                                                               * EQW8LBVR
01397 ***************************************************************** EQW8LBVR
01398 * REMPLISSAGE ECRAN   * FB02 * TRAITEMENT AUTOMATIQUE           * EQW8LBVR
01399 ***************************************************************** EQW8LBVR
01400                                                                   EQW8LBVR
01401  REMPLISSAGE-FORMAT-ECRAN.                                        EQW8LBVR
01402                                                                   EQW8LBVR
01403       PERFORM REMPLISSAGE-ZONES-OBLIGATOIRES THRU                 EQW8LBVR
01404               FIN-REMP-ZONES-OBL.                                 EQW8LBVR
01405                                                                   EQW8LBVR
01406       PERFORM REMPLISSAGE-ZONES-PROTEGEES THRU                    EQW8LBVR
01407               FIN-REMP-ZONES-PROT.                                EQW8LBVR
01408                                                                   EQW8LBVR
01409       PERFORM REMPLISSAGE-ZONES-NO-PROTEGEES THRU                 EQW8LBVR
01410               FIN-REMP-ZONES-NO-PROT.                             EQW8LBVR
01411                                                                   EQW8LBVR
01412       PERFORM RESTAURATION-ATTRIBUTS THRU                         EQW8LBVR
01413               FIN-RESTAURATION-ATTRIBUTS.                         EQW8LBVR
01414                                                                   EQW8LBVR
01415  FIN-REMPLISSAGE-FORMAT-ECRAN.  EXIT.                             EQW8LBVR
01416 /                                                                 EQW8LBVR
01417  REMPLISSAGE-ZONES-OBLIGATOIRES.                                  EQW8LBVR
01418 * ---------------------------- *                                  EQW8LBVR
01419      MOVE Z-TIMER-DATJOU        TO   ECR-XJOURDO.                 EQW8LBVR
01420      MOVE Z-TIMER-TIMJOU        TO   ECR-XHEUREDO.                EQW8LBVR
01421      MOVE W-XTRMTRAC            TO   ECR-XTRMTRACO.               EQW8LBVR
01422      MOVE COM-GENE-LIBUSR       TO   ECR-XRACFLO.                 EQW8LBVR
01423      MOVE COM-GENE-LIBCNV       TO   ECR-XAPPLILO.                EQW8LBVR
01424                                                                   EQW8LBVR
01425 *  VERIFICATION DE LA PRESENCE DE CODES MESSAGES EN COMMAREA      EQW8LBVR
01426 *  SI OUI AFFICHAGE                                               EQW8LBVR
01427 *         REMISE A BLANC EN  COMMAREA                             EQW8LBVR
01428                                                                   EQW8LBVR
01429      IF COM-GENE-MESINF  NOT = SPACES AND                         EQW8LBVR
01430                                LOW-VALUE                          EQW8LBVR
01431         PERFORM LECTURE-ERREUR THRU                               EQW8LBVR
01432                 FIN-LECTURE-ERREUR                                EQW8LBVR
01433         MOVE    SPACES   TO COM-GENE-MESINF                       EQW8LBVR
01434         MOVE    W-ERREUR TO ECR-XMSGILO                           EQW8LBVR
01435      END-IF.                                                      EQW8LBVR
01436      IF COM-GENE-MESANO  NOT = SPACES AND                         EQW8LBVR
01437                                LOW-VALUE                          EQW8LBVR
01438         PERFORM LECTURE-ERREUR THRU                               EQW8LBVR
01439                 FIN-LECTURE-ERREUR                                EQW8LBVR
01440         MOVE SPACES      TO COM-GENE-MESANO                       EQW8LBVR
01441         MOVE W-ERREUR    TO ECR-XMSGALO                           EQW8LBVR
01442      END-IF.                                                      EQW8LBVR
01443  FIN-REMP-ZONES-OBL.  EXIT.                                       EQW8LBVR
01444 /                                                                 EQW8LBVR
01445  REMPLISSAGE-ZONES-PROTEGEES.                                     EQW8LBVR
01446                                                                   EQW8LBVR
01447      MOVE COM-GENE-LIBCNV TO ECR-XAPPLILO.                        EQW8LBVR
01448                                                                   EQW8LBVR
01449 *---CODE INTERMEDIAIRE ET NUMERO CLIENT                           EQW8LBVR
01450      MOVE INF-GES OF TS-SUSPENS1  TO  W-GES.                      EQW8LBVR
01451      MOVE NOM-CLI OF TS-SUSPENS1  TO  W-CLI.                      EQW8LBVR
U3319  ++INCLUDE MAIPCLI
01452      MOVE W-GESCLI              TO  ECR-GESCLIO.                  EQW8LBVR
01453                                                                   EQW8LBVR
01454 *---QUALITE DU CLIENT                                             EQW8LBVR
01455      MOVE NOM-RAIC OF TS-SUSPENS1 TO  ECR-RAICO.                  EQW8LBVR
01456                                                                   EQW8LBVR
01457 *---NOM DU CLIENT                                                 EQW8LBVR
01458      MOVE NOM-NOMC OF TS-SUSPENS1 TO  ECR-NOMCO.                  EQW8LBVR
01459                                                                   EQW8LBVR
01460  FIN-REMP-ZONES-PROT. EXIT.                                       EQW8LBVR
01461 /                                                                 EQW8LBVR
01462  REMPLISSAGE-ZONES-NO-PROTEGEES.                                  EQW8LBVR
01463                                                                   EQW8LBVR
01464                                                                   EQW8LBVR
01465 *---IDENTIFIANT CLIENT                                            EQW8LBVR
01466      IF PERNUMX OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW8LBVR
01467         MOVE PERNUMX OF TS-PERSONNE(1) TO ECR-PERNUMXO            EQW8LBVR
01468      ELSE                                                         EQW8LBVR
01469         IF PERNUMX OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01470            MOVE PERNUMX OF TS-PERSONNE(2) TO ECR-PERNUMXO         EQW8LBVR
01471         END-IF                                                    EQW8LBVR
01472      END-IF.                                                      EQW8LBVR
01473                                                                   EQW8LBVR
01474 *---STATUT DE LA PERSONNE                                         EQW8LBVR
01475      IF PERSTAC OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW8LBVR
01476         MOVE PERSTAC OF TS-PERSONNE(1) TO ECR-PERSTACO            EQW8LBVR
01477      ELSE                                                         EQW8LBVR
01478         IF PERSTAC OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01479            MOVE PERSTAC OF TS-PERSONNE(2) TO ECR-PERSTACO         EQW8LBVR
01480         END-IF                                                    EQW8LBVR
01481      END-IF.                                                      EQW8LBVR
01482                                                                   EQW8LBVR
01483 *---NOMBRE DE SALARIES                                            EQW8LBVR
01484      IF PERSALN OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW8LBVR
01485         MOVE PERSALN OF TS-PERSONNE(1) TO ECR-PERSALNO            EQW8LBVR
01486      ELSE                                                         EQW8LBVR
01487         IF PERSALN OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01488            MOVE PERSALN OF TS-PERSONNE(2) TO ECR-PERSALNO         EQW8LBVR
01489         END-IF                                                    EQW8LBVR
01490      END-IF.                                                      EQW8LBVR
01491                                                                   EQW8LBVR
01492 *---TITRE DE LA PERSONNE                                          EQW8LBVR
01493      IF PERTITL OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW8LBVR
01494         MOVE PERTITL OF TS-PERSONNE(1) TO ECR-PERTITLO            EQW8LBVR
01495      ELSE                                                         EQW8LBVR
01496         IF PERTITL OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01497            MOVE PERTITL OF TS-PERSONNE(2) TO ECR-PERTITLO         EQW8LBVR
01498         END-IF                                                    EQW8LBVR
01499      END-IF.                                                      EQW8LBVR
01500                                                                   EQW8LBVR
01501 *---NOM DE LA PERSONNE                                            EQW8LBVR
01502      IF PERNOML OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW8LBVR
01503         MOVE PERNOML OF TS-PERSONNE(1) TO ECR-PERNOMLO            EQW8LBVR
01504      ELSE                                                         EQW8LBVR
01505         IF PERNOML OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01506            MOVE PERNOML OF TS-PERSONNE(2) TO ECR-PERNOMLO         EQW8LBVR
01507         END-IF                                                    EQW8LBVR
01508      END-IF.                                                      EQW8LBVR
01509                                                                   EQW8LBVR
01510 *---PRENOM DE LA PERSONNE                                         EQW8LBVR
01511      IF PERPREL OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW8LBVR
01512         MOVE PERPREL OF TS-PERSONNE(1) TO ECR-PERPRELO            EQW8LBVR
01513      ELSE                                                         EQW8LBVR
01514         IF PERPREL OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01515            MOVE PERPREL OF TS-PERSONNE(2) TO ECR-PERPRELO         EQW8LBVR
01516         END-IF                                                    EQW8LBVR
01517      END-IF.                                                      EQW8LBVR
01518                                                                   EQW8LBVR
01519 *---DATE DE NAISSANCE                                             EQW8LBVR
01520      IF RPERNAID OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE     EQW8LBVR
01521         MOVE PERNAID OF TS-PERSONNE(1) (1:4) TO ECR-PERNAIDO (5:4)EQW8LBVR
01522         MOVE PERNAID OF TS-PERSONNE(1) (5:2) TO ECR-PERNAIDO (3:2)EQW8LBVR
01523         MOVE PERNAID OF TS-PERSONNE(1) (7:2) TO ECR-PERNAIDO (1:2)EQW8LBVR
01524      ELSE                                                         EQW8LBVR
01525         IF RPERNAID OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE  EQW8LBVR
01526            MOVE PERNAID OF TS-PERSONNE(2) (1:4) TO                EQW8LBVR
01527                                                 ECR-PERNAIDO (5:4)EQW8LBVR
01528            MOVE PERNAID OF TS-PERSONNE(2) (5:2) TO                EQW8LBVR
01529                                                 ECR-PERNAIDO (3:2)EQW8LBVR
01530            MOVE PERNAID OF TS-PERSONNE(2) (7:2) TO                EQW8LBVR
01531                                                 ECR-PERNAIDO (1:2)EQW8LBVR
01532         END-IF                                                    EQW8LBVR
01533      END-IF.                                                      EQW8LBVR
01534                                                                   EQW8LBVR
01535 *---SEXE DE LA PERSONNE                                           EQW8LBVR
01536      IF PERSEXC OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW8LBVR
01537         MOVE PERSEXC OF TS-PERSONNE(1) TO ECR-PERSEXCO            EQW8LBVR
01538      ELSE                                                         EQW8LBVR
01539         IF PERSEXC OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01540            MOVE PERSEXC OF TS-PERSONNE(2) TO ECR-PERSEXCO         EQW8LBVR
01541         END-IF                                                    EQW8LBVR
01542      END-IF.                                                      EQW8LBVR
01543                                                                   EQW8LBVR
01544 *---NOMBRE D'ENFANTS A CHARGE                                     EQW8LBVR
01545      IF RPERENCN OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE     EQW8LBVR
01546         MOVE RPERENCN OF TS-PERSONNE(1) TO ECR-PERENCNO           EQW8LBVR
01547      ELSE                                                         EQW8LBVR
01548         IF RPERENCN OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE  EQW8LBVR
01549            MOVE RPERENCN OF TS-PERSONNE(2) TO ECR-PERENCNO        EQW8LBVR
01550         END-IF                                                    EQW8LBVR
01551      END-IF.                                                      EQW8LBVR
01552                                                                   EQW8LBVR
01553 *---SITUATION MATRIMONIALE                                        EQW8LBVR
01554      IF PERMATC OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW8LBVR
01555         MOVE PERMATC OF TS-PERSONNE(1) TO ECR-PERMATCO            EQW8LBVR
01556      ELSE                                                         EQW8LBVR
01557         IF PERMATC OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01558            MOVE PERMATC OF TS-PERSONNE(2) TO ECR-PERMATCO         EQW8LBVR
01559         END-IF                                                    EQW8LBVR
01560      END-IF.                                                      EQW8LBVR
01561                                                                   EQW8LBVR
01562 *---CODE PROFESSION                                               EQW8LBVR
01563      IF PERPROC OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW8LBVR
01564         MOVE PERPROC OF TS-PERSONNE(1) TO ECR-PERPROCO            EQW8LBVR
01565      ELSE                                                         EQW8LBVR
01566         IF PERPROC OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01567            MOVE PERPROC OF TS-PERSONNE(2) TO ECR-PERPROCO         EQW8LBVR
01568         END-IF                                                    EQW8LBVR
01569      END-IF.                                                      EQW8LBVR
01570                                                                   EQW8LBVR
01571 *---LIBELLE PROFESSION                                            EQW8LBVR
01572      IF PERPROL OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW8LBVR
01573         MOVE PERPROL OF TS-PERSONNE(1) TO ECR-PERPROLO            EQW8LBVR
01574      ELSE                                                         EQW8LBVR
01575         IF PERPROL OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01576            MOVE PERPROL OF TS-PERSONNE(2) TO ECR-PERPROLO         EQW8LBVR
01577         END-IF                                                    EQW8LBVR
01578      END-IF.                                                      EQW8LBVR
01579                                                                   EQW8LBVR
01580 *---PERMIS/BREVETS + DATE OBTENTION                               EQW8LBVR
01581      IF PRMTYPC OF TS-PERSONNE(1, 1) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01582         MOVE PRMTYPC OF TS-PERSONNE(1, 1) TO ECR-PRMTYPC1O        EQW8LBVR
01583      ELSE                                                         EQW8LBVR
01584         IF PRMTYPC OF TS-PERSONNE(2, 1) NOT = SPACES AND LOW-VALUEEQW8LBVR
01585            MOVE PRMTYPC OF TS-PERSONNE(2, 1) TO ECR-PRMTYPC1O     EQW8LBVR
01586         END-IF                                                    EQW8LBVR
01587      END-IF.                                                      EQW8LBVR
01588                                                                   EQW8LBVR
01589      IF RPRMOBTD OF TS-PERSONNE(1, 1) NOT = SPACES AND LOW-VALUE  EQW8LBVR
01590         MOVE RPRMOBTD OF TS-PERSONNE(1, 1)(1:4) TO                EQW8LBVR
01591                                              ECR-PRMOBTD1O(5:4)   EQW8LBVR
01592         MOVE RPRMOBTD OF TS-PERSONNE(1, 1)(5:2) TO                EQW8LBVR
01593                                              ECR-PRMOBTD1O(3:2)   EQW8LBVR
01594         MOVE RPRMOBTD OF TS-PERSONNE(1, 1)(7:2) TO                EQW8LBVR
01595                                              ECR-PRMOBTD1O(1:2)   EQW8LBVR
01596      ELSE                                                         EQW8LBVR
01597        IF RPRMOBTD OF TS-PERSONNE(2, 1) NOT = SPACES AND LOW-VALUEEQW8LBVR
01598           MOVE RPRMOBTD OF TS-PERSONNE(2, 1)(1:4) TO              EQW8LBVR
01599                                                 ECR-PRMOBTD1O(5:4)EQW8LBVR
01600           MOVE RPRMOBTD OF TS-PERSONNE(2, 1)(5:2) TO              EQW8LBVR
01601                                                 ECR-PRMOBTD1O(3:2)EQW8LBVR
01602           MOVE RPRMOBTD OF TS-PERSONNE(2, 1)(7:2) TO              EQW8LBVR
01603                                                 ECR-PRMOBTD1O(1:2)EQW8LBVR
01604        END-IF                                                     EQW8LBVR
01605      END-IF.                                                      EQW8LBVR
01606                                                                   EQW8LBVR
01607      IF PRMTYPC OF TS-PERSONNE(1, 2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01608         MOVE PRMTYPC OF TS-PERSONNE(1, 2) TO ECR-PRMTYPC2O        EQW8LBVR
01609      ELSE                                                         EQW8LBVR
01610         IF PRMTYPC OF TS-PERSONNE(2, 2) NOT = SPACES AND LOW-VALUEEQW8LBVR
01611            MOVE PRMTYPC OF TS-PERSONNE(2, 2) TO ECR-PRMTYPC2O     EQW8LBVR
01612         END-IF                                                    EQW8LBVR
01613      END-IF.                                                      EQW8LBVR
01614                                                                   EQW8LBVR
01615      IF RPRMOBTD OF TS-PERSONNE(1, 2) NOT = SPACES AND LOW-VALUE  EQW8LBVR
01616         MOVE RPRMOBTD OF TS-PERSONNE(1, 2)(1:4) TO                EQW8LBVR
01617                                              ECR-PRMOBTD2O(5:4)   EQW8LBVR
01618         MOVE RPRMOBTD OF TS-PERSONNE(1, 2)(5:2) TO                EQW8LBVR
01619                                              ECR-PRMOBTD2O(3:2)   EQW8LBVR
01620         MOVE RPRMOBTD OF TS-PERSONNE(1, 2)(7:2) TO                EQW8LBVR
01621                                              ECR-PRMOBTD2O(1:2)   EQW8LBVR
01622      ELSE                                                         EQW8LBVR
01623        IF RPRMOBTD OF TS-PERSONNE(2, 2) NOT = SPACES AND LOW-VALUEEQW8LBVR
01624           MOVE RPRMOBTD OF TS-PERSONNE(2, 2)(1:4) TO              EQW8LBVR
01625                                                 ECR-PRMOBTD2O(5:4)EQW8LBVR
01626           MOVE RPRMOBTD OF TS-PERSONNE(2, 2)(5:2) TO              EQW8LBVR
01627                                                 ECR-PRMOBTD2O(3:2)EQW8LBVR
01628           MOVE RPRMOBTD OF TS-PERSONNE(2, 2)(7:2) TO              EQW8LBVR
01629                                                 ECR-PRMOBTD2O(1:2)EQW8LBVR
01630        END-IF                                                     EQW8LBVR
01631      END-IF.                                                      EQW8LBVR
01632                                                                   EQW8LBVR
01633 *---INDICATEUR CONDUITE ACCOMPAGNEE                               EQW8LBVR
01634      IF PERCOAC OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW8LBVR
01635         MOVE PERCOAC OF TS-PERSONNE(1) TO ECR-PERCOACO            EQW8LBVR
01636      ELSE                                                         EQW8LBVR
01637         IF PERCOAC OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01638            MOVE PERCOAC OF TS-PERSONNE(2) TO ECR-PERCOACO         EQW8LBVR
01639         END-IF                                                    EQW8LBVR
01640      END-IF.                                                      EQW8LBVR
01641                                                                   EQW8LBVR
01642 *---NOMBRE DE MOIS DU RELEVE D'INFOS                              EQW8LBVR
01643      IF RANPANCN OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE     EQW8LBVR
01644         MOVE RANPANCN OF TS-PERSONNE(1) TO ECR-ANPANCNO           EQW8LBVR
01645      ELSE                                                         EQW8LBVR
01646         IF RANPANCN OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE  EQW8LBVR
01647            MOVE RANPANCN OF TS-PERSONNE(2) TO ECR-ANPANCNO        EQW8LBVR
01648         END-IF                                                    EQW8LBVR
01649      END-IF.                                                      EQW8LBVR
01650                                                                   EQW8LBVR
F3576 *---INDICATEUR ANNULATION OU SUSPENSION DE PERMIS                 EQW8LBVR
01652      IF ANPINDC OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW8LBVR
01653         MOVE ANPINDC OF TS-PERSONNE(1) TO ECR-ANPINDCO            EQW8LBVR
01654      ELSE                                                         EQW8LBVR
01655         IF ANPINDC OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01656            MOVE ANPINDC OF TS-PERSONNE(2) TO ECR-ANPINDCO         EQW8LBVR
01657         END-IF                                                    EQW8LBVR
01658      END-IF.                                                      EQW8LBVR
01659                                                                   EQW8LBVR
01660 *---NOMBRE DE JOURS DE RETRAIT DE PERMIS                          EQW8LBVR
01661      IF RANPNBJN OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE     EQW8LBVR
01662         MOVE RANPNBJN OF TS-PERSONNE(1) TO ECR-ANPNBJNO           EQW8LBVR
01663      ELSE                                                         EQW8LBVR
01664         IF RANPNBJN OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE  EQW8LBVR
01665            MOVE RANPNBJN OF TS-PERSONNE(2) TO ECR-ANPNBJNO        EQW8LBVR
01666         END-IF                                                    EQW8LBVR
01667      END-IF.                                                      EQW8LBVR
01668                                                                   EQW8LBVR
01669 *---MOTIF DU RETRAIT DE PERMIS                                    EQW8LBVR
01670      IF ANPMOTL OF TS-PERSONNE(1) NOT = SPACES AND LOW-VALUE      EQW8LBVR
01671         MOVE ANPMOTL OF TS-PERSONNE(1) TO ECR-ANPMOTLO            EQW8LBVR
01672      ELSE                                                         EQW8LBVR
01673         IF ANPMOTL OF TS-PERSONNE(2) NOT = SPACES AND LOW-VALUE   EQW8LBVR
01674            MOVE ANPMOTL OF TS-PERSONNE(2) TO ECR-ANPMOTLO         EQW8LBVR
01675         END-IF                                                    EQW8LBVR
01676      END-IF.                                                      EQW8LBVR
01677                                                                   EQW8LBVR
01678  FIN-REMP-ZONES-NO-PROT. EXIT.                                    EQW8LBVR
01679 /                                                                 EQW8LBVR
01680 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
01681 * MODULE DE BASE    * FB02 * TRAITEMENT NORMAL                  * EQW8LBVR
01682 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
01683 *                                                               * EQW8LBVR
01684 *              -----------------------------------              * EQW8LBVR
01685 *              I  TRAITEMENT            NORMAL   I              * EQW8LBVR
01686 *              -----------------------------------              * EQW8LBVR
01687 *                               I                               * EQW8LBVR
01688 *           -----------------------------------------           * EQW8LBVR
01689 *           I                   I                   I           * EQW8LBVR
01690 * -------------------- -------------------- ------------------- * EQW8LBVR
01691 * I CONTROLE SYNTAXE I I CONTROLE LOGIQUE I I TRAITEMENT TACHE I* EQW8LBVR
01692 * -------------------- -------------------- ------------------- * EQW8LBVR
01693 *                                                   I           * EQW8LBVR
01694 *                                           ------------------- * EQW8LBVR
01695 *                                           I DETERMI-ECR-SUIV I* EQW8LBVR
01696 *                                           ------------------- * EQW8LBVR
01697 *                                                               * EQW8LBVR
01698 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
01699                                                                   EQW8LBVR
01700  M-TRAITEMENT-NORMAL.                                             EQW8LBVR
01701 * ----------------- *                                             EQW8LBVR
01702                                                                   EQW8LBVR
01703       PERFORM RESTAURATION-ATTRIBUTS THRU                         EQW8LBVR
01704               FIN-RESTAURATION-ATTRIBUTS.                         EQW8LBVR
01705                                                                   EQW8LBVR
01706       PERFORM CONTROLE-SYNTAXE THRU                               EQW8LBVR
01707               FIN-CONTROLE-SYNTAXE.                               EQW8LBVR
01708                                                                   EQW8LBVR
01709 * PAS DE CONTROLE SI LE SUSPENS EST EMIS                          EQW8LBVR
01710       IF OK                                                       EQW8LBVR
01711       AND (INF-ETAT OF TS-SUSPENS1 NOT = '5' AND '7')             EQW8LBVR
01712          PERFORM CONTROLE-LOGIQUE THRU                            EQW8LBVR
01713                  FIN-CONTROLE-LOGIQUE                             EQW8LBVR
01714       END-IF.                                                     EQW8LBVR
01715                                                                   EQW8LBVR
01716       IF OK                                                       EQW8LBVR
01717          IF  ECRAN-MODIFIE                                        EQW8LBVR
01718          OR (NOT ECRAN-MODIFIE AND COM-GENE-REAF = ' ')           EQW8LBVR
01719             PERFORM TRAITEMENT-TACHE THRU                         EQW8LBVR
01720                     FIN-TRAITEMENT-TACHE                          EQW8LBVR
01721          END-IF                                                   EQW8LBVR
01722          EVALUATE  TRUE                                           EQW8LBVR
01723             WHEN  W-REAF        =  ' '                            EQW8LBVR
01724              AND  COM-GENE-REAF =  'O'                            EQW8LBVR
01725                   MOVE SPACES            TO  COM-GENE-REAF        EQW8LBVR
01726                   MOVE SPACES            TO  COM-GENE-MESINF      EQW8LBVR
01727             WHEN  W-REAF        =  ' '                            EQW8LBVR
01728              AND  COM-GENE-REAF =  ' '                            EQW8LBVR
01729                   CONTINUE                                        EQW8LBVR
01730             WHEN  W-REAF        =  'O'                            EQW8LBVR
01731              AND  COM-GENE-REAF =  ' '                            EQW8LBVR
01732                   MOVE 'O'               TO  COM-GENE-REAF        EQW8LBVR
01733             WHEN  W-REAF        =  'O'                            EQW8LBVR
01734              AND  COM-GENE-REAF =  'O'                            EQW8LBVR
01735                   EVALUATE  TRUE                                  EQW8LBVR
01736                       WHEN  ECRAN-MODIFIE                         EQW8LBVR
01737                             CONTINUE                              EQW8LBVR
01738                       WHEN  OTHER                                 EQW8LBVR
01739                             MOVE SPACES      TO  COM-GENE-REAF    EQW8LBVR
01740                             MOVE SPACES      TO  COM-GENE-MESINF  EQW8LBVR
01741                  END-EVALUATE                                     EQW8LBVR
01742          END-EVALUATE                                             EQW8LBVR
01743          IF  COM-GENE-REAF   =  'O'                               EQW8LBVR
01744              MOVE CODE-TRAITEMENT-AUTOMATIQUE  TO  Z-FONCTION     EQW8LBVR
01745          ELSE                                                     EQW8LBVR
01746              IF  ECR-XCDECO = LOW-VALUE OR  SPACES                EQW8LBVR
01747                  PERFORM  DETERMINATION-ECR-SUIV  THRU            EQW8LBVR
01748                           FIN-DETERMINATION-ECR-SUIV              EQW8LBVR
01749              END-IF                                               EQW8LBVR
01750          END-IF                                                   EQW8LBVR
01751      END-IF.                                                      EQW8LBVR
01752  FIN-M-TRAITEMENT-NORMAL. EXIT.                                   EQW8LBVR
01753 /                                                                 EQW8LBVR
01754 ***************************************************************** EQW8LBVR
01755 *    RESTAURATION DES ATTRIBUTS AVANT CONTROLES                   EQW8LBVR
01756 ***************************************************************** EQW8LBVR
01757  RESTAURATION-ATTRIBUTS.                                          EQW8LBVR
01758                                                                   EQW8LBVR
01759      MOVE NOR-ASK    TO  ECR-XTRMTRACA.                           EQW8LBVR
01760      MOVE NOR-ASK    TO  ECR-XAPPLILA.                            EQW8LBVR
01761      MOVE NOR-ASK    TO  ECR-XJOURDA.                             EQW8LBVR
01762      MOVE NOR-ASK    TO  ECR-XRACFLA.                             EQW8LBVR
01763      MOVE NOR-ASK    TO  ECR-XHEUREDA.                            EQW8LBVR
01764      MOVE NOR-ASK    TO  ECR-GESCLIA.                             EQW8LBVR
01765      MOVE NOR-ASK    TO  ECR-RAICA.                               EQW8LBVR
01766      MOVE NOR-ASK    TO  ECR-NOMCA.                               EQW8LBVR
01767                                                                   EQW8LBVR
01768      MOVE BRT-ALP    TO  ECR-XCDECA.                              EQW8LBVR
01769                                                                   EQW8LBVR
01770      IF COM-MA-IND-BLOCAGE = 'O'                                  EQW8LBVR
01771         MOVE NOR-ASK    TO  ECR-PERNUMXA                          EQW8LBVR
01772         MOVE NOR-ASK    TO  ECR-PERSTACA                          EQW8LBVR
01773         MOVE NOR-ASK    TO  ECR-PERSALNA                          EQW8LBVR
01774         MOVE NOR-ASK    TO  ECR-PERTITLA                          EQW8LBVR
01775         MOVE NOR-ASK    TO  ECR-PERNOMLA                          EQW8LBVR
01776         MOVE NOR-ASK    TO  ECR-PERPRELA                          EQW8LBVR
01777         MOVE NOR-ASK    TO  ECR-PERNAIDA                          EQW8LBVR
01778         MOVE NOR-ASK    TO  ECR-PERSEXCA                          EQW8LBVR
01779         MOVE NOR-ASK    TO  ECR-PERENCNA                          EQW8LBVR
01780         MOVE NOR-ASK    TO  ECR-PERMATCA                          EQW8LBVR
01781         MOVE NOR-ASK    TO  ECR-PERPROCA                          EQW8LBVR
01782         MOVE NOR-ASK    TO  ECR-PERPROLA                          EQW8LBVR
01783         MOVE NOR-ASK    TO  ECR-PRMTYPC1A                         EQW8LBVR
01784         MOVE NOR-ASK    TO  ECR-PRMOBTD1A                         EQW8LBVR
01785         MOVE NOR-ASK    TO  ECR-PRMTYPC2A                         EQW8LBVR
01786         MOVE NOR-ASK    TO  ECR-PRMOBTD2A                         EQW8LBVR
01787         MOVE NOR-ASK    TO  ECR-PERCOACA                          EQW8LBVR
01788         MOVE NOR-ASK    TO  ECR-ANPANCNA                          EQW8LBVR
01789         MOVE NOR-ASK    TO  ECR-ANPINDCA                          EQW8LBVR
01790         MOVE NOR-ASK    TO  ECR-ANPNBJNA                          EQW8LBVR
01791         MOVE NOR-ASK    TO  ECR-ANPMOTLA                          EQW8LBVR
01792      ELSE                                                         EQW8LBVR
01793         MOVE BRT-ALP    TO  ECR-PERNUMXA                          EQW8LBVR
01794         MOVE BRT-ALP    TO  ECR-PERSTACA                          EQW8LBVR
01795         MOVE BRT-ALP    TO  ECR-PERSALNA                          EQW8LBVR
01796         MOVE BRT-ALP    TO  ECR-PERTITLA                          EQW8LBVR
01797         MOVE BRT-ALP    TO  ECR-PERNOMLA                          EQW8LBVR
01798         MOVE BRT-ALP    TO  ECR-PERPRELA                          EQW8LBVR
01799         MOVE BRT-ALP    TO  ECR-PERNAIDA                          EQW8LBVR
01800         MOVE BRT-ALP    TO  ECR-PERSEXCA                          EQW8LBVR
01801         MOVE BRT-ALP    TO  ECR-PERENCNA                          EQW8LBVR
01802         MOVE BRT-ALP    TO  ECR-PERMATCA                          EQW8LBVR
01803         MOVE BRT-ALP    TO  ECR-PERPROCA                          EQW8LBVR
01804         MOVE BRT-ALP    TO  ECR-PERPROLA                          EQW8LBVR
01805         MOVE BRT-ALP    TO  ECR-PRMTYPC1A                         EQW8LBVR
01806         MOVE BRT-ALP    TO  ECR-PRMOBTD1A                         EQW8LBVR
01807         MOVE BRT-ALP    TO  ECR-PRMTYPC2A                         EQW8LBVR
01808         MOVE BRT-ALP    TO  ECR-PRMOBTD2A                         EQW8LBVR
01809         MOVE BRT-ALP    TO  ECR-PERCOACA                          EQW8LBVR
01810         MOVE BRT-ALP    TO  ECR-ANPANCNA                          EQW8LBVR
01811         MOVE BRT-ALP    TO  ECR-ANPINDCA                          EQW8LBVR
01812         MOVE BRT-ALP    TO  ECR-ANPNBJNA                          EQW8LBVR
01813         MOVE BRT-ALP    TO  ECR-ANPMOTLA                          EQW8LBVR
01814      END-IF.                                                      EQW8LBVR
01815                                                                   EQW8LBVR
01816  FIN-RESTAURATION-ATTRIBUTS.  EXIT.                               EQW8LBVR
01817               EJECT                                               EQW8LBVR
01818                                                                   EQW8LBVR
01819 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
01820 * CONTROLES SYNTAXIQUES * FB02 * TRAITEMENT NORMAL                EQW8LBVR
01821 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
01822  CONTROLE-SYNTAXE.                                                EQW8LBVR
01823                                                                   EQW8LBVR
01824 * TEST DES CODES MNEMONIQUES POUR L'ENCHAINEMENT DES              EQW8LBVR
01825 * TRANSACTIONS DANS UNE CONVERSATION                              EQW8LBVR
01826                                                                   EQW8LBVR
01827      IF ECR-XCDECO NOT = LOW-VALUE AND SPACES                     EQW8LBVR
01828         IF ECR-XCDECO = 'AIDE' OR 'STAT' OR 'PROF'                EQW8LBVR
01829            MOVE 'MA84'                 TO NOM-TACHE-XCTL          EQW8LBVR
01830            IF ECR-XCDECO = 'AIDE'                                 EQW8LBVR
01831               MOVE 'AIDE'              TO COM-MA-GENRE-TXT        EQW8LBVR
01832            END-IF                                                 EQW8LBVR
01833            IF ECR-XCDECO = 'STAT'                                 EQW8LBVR
01834               MOVE 'STAT'              TO COM-MA-GENRE-TXT        EQW8LBVR
01835            END-IF                                                 EQW8LBVR
01836            IF ECR-XCDECO = 'PROF'                                 EQW8LBVR
01837               MOVE 'PROF'              TO COM-MA-GENRE-TXT        EQW8LBVR
01838            END-IF                                                 EQW8LBVR
01839         ELSE                                                      EQW8LBVR
U3319            PERFORM  CONTROLE-CODE-COMMANDE  THRU                  EFUTSQP3
U3319                     FIN-CONTROLE-CODE-COMMANDE                    EFUTSQP3
01865         END-IF                                                    EQW8LBVR
01866      END-IF.                                                      EQW8LBVR
01867                                                                   EQW8LBVR
01868 *---MISE A BLANC DES ZONES A LOW-VALUE                            EQW8LBVR
01869                                                                   EQW8LBVR
01870      IF ECR-PERNUMXO = LOW-VALUE                                  EQW8LBVR
01871         MOVE SPACES TO ECR-PERNUMXO                               EQW8LBVR
01872      END-IF.                                                      EQW8LBVR
01873      IF ECR-PERSTACO = LOW-VALUE                                  EQW8LBVR
01874         MOVE SPACES TO ECR-PERSTACO                               EQW8LBVR
01875      END-IF.                                                      EQW8LBVR
01876      IF ECR-PERSALNO = LOW-VALUE                                  EQW8LBVR
01877         MOVE SPACES TO ECR-PERSALNO                               EQW8LBVR
01878      END-IF.                                                      EQW8LBVR
01879      IF ECR-PERTITLO = LOW-VALUE                                  EQW8LBVR
01880         MOVE SPACES TO ECR-PERTITLO                               EQW8LBVR
01881      END-IF.                                                      EQW8LBVR
01882      IF ECR-PERNOMLO = LOW-VALUE                                  EQW8LBVR
01883         MOVE SPACES TO ECR-PERNOMLO                               EQW8LBVR
01884      END-IF.                                                      EQW8LBVR
01885      IF ECR-PERPRELO = LOW-VALUE                                  EQW8LBVR
01886         MOVE SPACES TO ECR-PERPRELO                               EQW8LBVR
01887      END-IF.                                                      EQW8LBVR
01888      IF ECR-PERSEXCO = LOW-VALUE                                  EQW8LBVR
01889         MOVE SPACES TO ECR-PERSEXCO                               EQW8LBVR
01890      END-IF.                                                      EQW8LBVR
01891      IF ECR-PERNAIDO = LOW-VALUE                                  EQW8LBVR
01892         MOVE SPACES TO ECR-PERNAIDO                               EQW8LBVR
01893      END-IF.                                                      EQW8LBVR
01894      IF ECR-PERPROCO = LOW-VALUE                                  EQW8LBVR
01895         MOVE SPACES TO ECR-PERPROCO                               EQW8LBVR
01896      END-IF.                                                      EQW8LBVR
01897      IF ECR-PERPROLO = LOW-VALUE                                  EQW8LBVR
01898         MOVE SPACES TO ECR-PERPROLO                               EQW8LBVR
01899      END-IF.                                                      EQW8LBVR
01900      IF ECR-PERMATCO = LOW-VALUE                                  EQW8LBVR
01901         MOVE SPACES TO ECR-PERMATCO                               EQW8LBVR
01902      END-IF.                                                      EQW8LBVR
01903      IF ECR-PERENCNO = LOW-VALUE                                  EQW8LBVR
01904         MOVE SPACES TO ECR-PERENCNO                               EQW8LBVR
01905      END-IF.                                                      EQW8LBVR
01906      IF ECR-PRMTYPC1O = LOW-VALUE                                 EQW8LBVR
01907         MOVE SPACES TO ECR-PRMTYPC1O                              EQW8LBVR
01908      END-IF.                                                      EQW8LBVR
01909      IF ECR-PRMOBTD1O = LOW-VALUE                                 EQW8LBVR
01910         MOVE SPACES TO ECR-PRMOBTD1O                              EQW8LBVR
01911      END-IF.                                                      EQW8LBVR
01912      IF ECR-PRMTYPC2O = LOW-VALUE                                 EQW8LBVR
01913         MOVE SPACES TO ECR-PRMTYPC2O                              EQW8LBVR
01914      END-IF.                                                      EQW8LBVR
01915      IF ECR-PRMOBTD2O = LOW-VALUE                                 EQW8LBVR
01916         MOVE SPACES TO ECR-PRMOBTD2O                              EQW8LBVR
01917      END-IF.                                                      EQW8LBVR
01918      IF ECR-PERCOACO = LOW-VALUE                                  EQW8LBVR
01919         MOVE SPACES TO ECR-PERCOACO                               EQW8LBVR
01920      END-IF.                                                      EQW8LBVR
01921      IF ECR-ANPANCNO = LOW-VALUE                                  EQW8LBVR
01922         MOVE SPACES TO ECR-ANPANCNO                               EQW8LBVR
01923      END-IF.                                                      EQW8LBVR
01924      IF ECR-ANPINDCO = LOW-VALUE                                  EQW8LBVR
01925         MOVE SPACES TO ECR-ANPINDCO                               EQW8LBVR
01926      END-IF.                                                      EQW8LBVR
01927      IF ECR-ANPNBJNO = LOW-VALUE                                  EQW8LBVR
01928         MOVE SPACES TO ECR-ANPNBJNO                               EQW8LBVR
01929      END-IF.                                                      EQW8LBVR
01930      IF ECR-ANPMOTLO = LOW-VALUE                                  EQW8LBVR
01931         MOVE SPACES TO ECR-ANPMOTLO                               EQW8LBVR
01932      END-IF.                                                      EQW8LBVR
01933                                                                   EQW8LBVR
01934 *---ALIMENTATION DATE DU JOUR                                     EQW8LBVR
01935      MOVE COM-MA-DT-JOURJJ TO WSS-DATJOUR-J.                      EQW8LBVR
01936      MOVE COM-MA-DT-JOURMM TO WSS-DATJOUR-M.                      EQW8LBVR
01937      MOVE COM-MA-DT-JOURSS TO WSS-DATJOUR-S.                      EQW8LBVR
01938      MOVE COM-MA-DT-JOURAA TO WSS-DATJOUR-A.                      EQW8LBVR
01939      MOVE COM-MA-DT-JOUR   TO WSS-DATE-JOUR-SAMJ.                 EQW8LBVR
01940                                                                   EQW8LBVR
01941 *--- DETERMINATION DE L'APPEL A L'ECRAN D'AIDE DES STATUTS        EQW8LBVR
01942      PERFORM DETER-AIDE-STATUT THRU FDETER-AIDE-STATUT.           EQW8LBVR
01943                                                                   EQW8LBVR
01944 *--- STATUT DE LA PERSONNE                                        EQW8LBVR
01945      MOVE ECR-PERSTACO TO W-STATUT.                               EQW8LBVR
01946      IF W-STATUT NOT = SPACES   AND                               EQW8LBVR
01947         W-STATUT(1:1) NOT = '?' AND                               EQW8LBVR
01948         W-STATUT(2:1) NOT = '?' AND                               EQW8LBVR
01949         ECR-XCDECO = (SPACES OR LOW-VALUE)                        EQW8LBVR
01950         MOVE  SPACES                 TO XSPIPARM                  EQW8LBVR
01951         MOVE  'GP'                   TO FONCTION  OF XSPIPARM     EQW8LBVR
01952         MOVE  'FBSTAT'                  TO TABLE-PREF             EQW8LBVR
01953         MOVE  COM-GENE-CODCIE-PRINCIPAL TO TABLE-SUFF             EQW8LBVR
01954         MOVE  IDENT-TABLE            TO CODTAB OF XSPIPARM        EQW8LBVR
01955         MOVE  '= '                   TO OPERATEUR OF XSPIPARM     EQW8LBVR
01956         MOVE  ECR-PERSTACO           TO REF-POSTE OF XSPIPARM     EQW8LBVR
01957         PERFORM ACCES-SPI THRU FIN-ACCES-SPI                      EQW8LBVR
01958         IF RETCOD OF XSPIPARM  = ZERO                             EQW8LBVR
01959            MOVE IOAREA  OF XSPIPARM TO FBSTAT01                   EQW8LBVR
01960         ELSE                                                      EQW8LBVR
01961            MOVE NOR-ALP TO ECR-PERSTACA                           EQW8LBVR
01962            MOVE 'FB001' TO COM-GENE-MESANO                        EQW8LBVR
01963                            COM-CODERR                             EQW8LBVR
01964            MOVE CURSEUR TO ECR-PERSTACL                           EQW8LBVR
01965            MOVE 1       TO KONTROL                                EQW8LBVR
01966            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
01967         END-IF                                                    EQW8LBVR
01968      END-IF.                                                      EQW8LBVR
01969                                                                   EQW8LBVR
01970 *--- NOMBRE DE SALARIES                                           EQW8LBVR
01971      IF ECR-PERSALNO NOT = SPACES                                 EQW8LBVR
01972         MOVE ECR-PERSALNO  TO C-XKMTENTREE                        EQW8LBVR
01973         MOVE 3             TO C-XKMTLONG                          EQW8LBVR
01974         MOVE 0             TO C-XKMTDECIMALE                      EQW8LBVR
01975         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW8LBVR
01976         IF C-XKMTRETCOD NOT = 0                                   EQW8LBVR
01977            MOVE NOR-ALP TO ECR-PERSALNA                           EQW8LBVR
01978            MOVE 'FB002' TO COM-GENE-MESANO                        EQW8LBVR
01979                            COM-CODERR                             EQW8LBVR
01980            MOVE CURSEUR TO ECR-PERSALNL                           EQW8LBVR
01981            MOVE 1       TO KONTROL                                EQW8LBVR
01982            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
01983         ELSE                                                      EQW8LBVR
01984            MOVE C-XKMTNUM-0D TO WSS-NBSALARIES                    EQW8LBVR
01985            MOVE C-XKMTSORTIE TO ECR-PERSALNO                      EQW8LBVR
01986         END-IF                                                    EQW8LBVR
01987      END-IF.                                                      EQW8LBVR
01988                                                                   EQW8LBVR
01989 *--- TITRE DE LA PERSONNE                                         EQW8LBVR
01990      IF ECR-PERTITLO NOT = SPACE                                  EQW8LBVR
01991         IF ECR-PERTITLO NOT = 'MR ' AND 'MME' AND 'MLE'           EQW8LBVR
01992            MOVE NOR-ALP TO ECR-PERTITLA                           EQW8LBVR
01993            MOVE 'FB003' TO COM-GENE-MESANO                        EQW8LBVR
01994                            COM-CODERR                             EQW8LBVR
01995            MOVE CURSEUR TO ECR-PERTITLL                           EQW8LBVR
01996            MOVE 1       TO KONTROL                                EQW8LBVR
01997            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
01998         END-IF                                                    EQW8LBVR
01999      END-IF.                                                      EQW8LBVR
02000                                                                   EQW8LBVR
02001 *--- DATE DE NAISSANCE DOIT ETRE NUMERIQUE ET DE FORMAT DATE      EQW8LBVR
02002      IF ECR-PERNAIDO NOT = SPACES                                 EQW8LBVR
02003         MOVE ECR-PERNAIDO TO WSS-DATE-A-VERIFIER                  EQW8LBVR
02004         PERFORM VERIF-DATE THRU FVERIF-DATE                       EQW8LBVR
02005         IF WSS-DATE-OK = 'N'                                      EQW8LBVR
02006            MOVE NOR-ALP TO ECR-PERNAIDA                           EQW8LBVR
02007            MOVE 'FB005' TO COM-GENE-MESANO                        EQW8LBVR
02008                            COM-CODERR                             EQW8LBVR
02009            MOVE CURSEUR TO ECR-PERNAIDL                           EQW8LBVR
02010            MOVE 1       TO KONTROL                                EQW8LBVR
02011            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
02012         END-IF                                                    EQW8LBVR
02013      END-IF.                                                      EQW8LBVR
02014                                                                   EQW8LBVR
02015 *--- SEXE DE LA PERSONNE                                          EQW8LBVR
02016      IF ECR-PERSEXCO NOT = SPACES                                 EQW8LBVR
02017         IF ECR-PERSEXCO NOT = 'M' AND 'F'                         EQW8LBVR
02018            MOVE NOR-ALP TO ECR-PERSEXCA                           EQW8LBVR
02019            MOVE 'FB004' TO COM-GENE-MESANO                        EQW8LBVR
02020                            COM-CODERR                             EQW8LBVR
02021            MOVE CURSEUR TO ECR-PERSEXCL                           EQW8LBVR
02022            MOVE 1       TO KONTROL                                EQW8LBVR
02023            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
02024         END-IF                                                    EQW8LBVR
02025      END-IF.                                                      EQW8LBVR
02026                                                                   EQW8LBVR
02027 *--- NOMBRE D'ENFANTS A CHARGE DOIT ETRE NUMERIQUE                EQW8LBVR
02028      IF ECR-PERENCNO NOT = SPACES                                 EQW8LBVR
02029         MOVE ECR-PERENCNO  TO C-XKMTENTREE                        EQW8LBVR
02030         MOVE 2             TO C-XKMTLONG                          EQW8LBVR
02031         MOVE 0             TO C-XKMTDECIMALE                      EQW8LBVR
02032         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW8LBVR
02033         IF C-XKMTRETCOD NOT = 0                                   EQW8LBVR
02034            MOVE NOR-ALP TO ECR-PERENCNA                           EQW8LBVR
02035            MOVE 'FB008' TO COM-GENE-MESANO                        EQW8LBVR
02036                            COM-CODERR                             EQW8LBVR
02037            MOVE CURSEUR TO ECR-PERENCNL                           EQW8LBVR
02038            MOVE 1       TO KONTROL                                EQW8LBVR
02039            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
02040         ELSE                                                      EQW8LBVR
02041            MOVE C-XKMTNUM-0D TO WSS-NBENFANTS                     EQW8LBVR
02042            MOVE C-XKMTSORTIE TO ECR-PERENCNO                      EQW8LBVR
02043         END-IF                                                    EQW8LBVR
02044      END-IF.                                                      EQW8LBVR
02045                                                                   EQW8LBVR
02046 *--- SITUATION MATRIMONIALE                                       EQW8LBVR
02047      IF ECR-PERMATCO NOT = SPACE                                  EQW8LBVR
02048         IF ECR-PERMATCO NOT = 'C' AND 'M'                         EQW8LBVR
02049            MOVE NOR-ALP TO ECR-PERMATCA                           EQW8LBVR
02050            MOVE 'FB007' TO COM-GENE-MESANO                        EQW8LBVR
02051                            COM-CODERR                             EQW8LBVR
02052            MOVE CURSEUR TO ECR-PERMATCL                           EQW8LBVR
02053            MOVE 1       TO KONTROL                                EQW8LBVR
02054            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
02055         END-IF                                                    EQW8LBVR
02056      END-IF.                                                      EQW8LBVR
02057                                                                   EQW8LBVR
02058 *--- DETERMINATION DE L'APPEL A ECRAN D'AIDE DES CODES PROFESSIONSEQW8LBVR
02059      PERFORM DETER-AIDE-PROFESSION THRU FDETER-AIDE-PROFESSION.   EQW8LBVR
02060                                                                   EQW8LBVR
02061 *--- CODE PROFESSION DOIT ETRE NUMERIQUE                          EQW8LBVR
02062      IF ECR-PERPROCO NOT = SPACES AND                             EQW8LBVR
02063         ECR-PERPROCO NOT NUMERIC AND                              EQW8LBVR
02064         ECR-PERPROCO(1:1) NOT = '?' AND                           EQW8LBVR
02065         ECR-PERPROCO(2:1) NOT = '?'                               EQW8LBVR
02066         MOVE NOR-ALP TO ECR-PERPROCA                              EQW8LBVR
02067         MOVE 'FB006' TO COM-GENE-MESANO                           EQW8LBVR
02068                         COM-CODERR                                EQW8LBVR
02069         MOVE CURSEUR TO ECR-PERPROCL                              EQW8LBVR
02070         MOVE 1       TO KONTROL                                   EQW8LBVR
02071         GO TO FIN-CONTROLE-SYNTAXE                                EQW8LBVR
02072      END-IF.                                                      EQW8LBVR
02073                                                                   EQW8LBVR
02074 *--- LE CODE PROFESSION DOIT EXISTER DANS LA TABLE FBPROF01       EQW8LBVR
02075      IF ECR-PERPROCO NOT = SPACES AND                             EQW8LBVR
02076         ECR-PERPROCO(1:1) NOT = '?' AND                           EQW8LBVR
02077         ECR-PERPROCO(2:1) NOT = '?'                               EQW8LBVR
02078         MOVE  SPACES                 TO XSPIPARM                  EQW8LBVR
02079         MOVE  'GP'                   TO FONCTION  OF XSPIPARM     EQW8LBVR
02080         MOVE  'FBPROF'                  TO TABLE-PREF             EQW8LBVR
02081         MOVE  COM-GENE-CODCIE-PRINCIPAL TO TABLE-SUFF             EQW8LBVR
02082         MOVE  IDENT-TABLE            TO CODTAB OF XSPIPARM        EQW8LBVR
02083         MOVE  '= '                   TO OPERATEUR OF XSPIPARM     EQW8LBVR
02084         MOVE  ECR-PERPROCO           TO REF-POSTE OF XSPIPARM     EQW8LBVR
02085         PERFORM ACCES-SPI THRU FIN-ACCES-SPI                      EQW8LBVR
02086         IF RETCOD OF XSPIPARM  = ZERO                             EQW8LBVR
02087            MOVE IOAREA  OF XSPIPARM TO FBPROF01                   EQW8LBVR
02088         ELSE                                                      EQW8LBVR
02089            MOVE NOR-ALP TO ECR-PERPROCA                           EQW8LBVR
02090            MOVE 'FB006' TO COM-GENE-MESANO                        EQW8LBVR
02091                            COM-CODERR                             EQW8LBVR
02092            MOVE CURSEUR TO ECR-PERPROCL                           EQW8LBVR
02093            MOVE 1       TO KONTROL                                EQW8LBVR
02094            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
02095         END-IF                                                    EQW8LBVR
02096      END-IF.                                                      EQW8LBVR
02097                                                                   EQW8LBVR
02098 *--- LA CATEGORIE DE PERMIS DOIT EXISTER DANS LA TABLE FBPERM01   EQW8LBVR
02099      IF ECR-PRMTYPC1O NOT = SPACES                                EQW8LBVR
02100         MOVE ECR-PRMTYPC1O TO WSS-TYPE-PERMIS                     EQW8LBVR
02101         PERFORM VERIF-PERMIS THRU FIN-VERIF-PERMIS                EQW8LBVR
02102         IF RETCOD OF XSPIPARM  NOT = ZERO                         EQW8LBVR
02103            MOVE NOR-ALP TO ECR-PRMTYPC1A                          EQW8LBVR
02104            MOVE 'FB009' TO COM-GENE-MESANO                        EQW8LBVR
02105                            COM-CODERR                             EQW8LBVR
02106            MOVE CURSEUR TO ECR-PRMTYPC1L                          EQW8LBVR
02107            MOVE 1       TO KONTROL                                EQW8LBVR
02108            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
02109         END-IF                                                    EQW8LBVR
02110      END-IF.                                                      EQW8LBVR
02111                                                                   EQW8LBVR
02112      IF ECR-PRMTYPC2O NOT = SPACES                                EQW8LBVR
02113         MOVE ECR-PRMTYPC2O TO WSS-TYPE-PERMIS                     EQW8LBVR
02114         PERFORM VERIF-PERMIS THRU FIN-VERIF-PERMIS                EQW8LBVR
02115         IF RETCOD OF XSPIPARM  NOT = ZERO                         EQW8LBVR
02116            MOVE NOR-ALP TO ECR-PRMTYPC2A                          EQW8LBVR
02117            MOVE 'FB009' TO COM-GENE-MESANO                        EQW8LBVR
02118                            COM-CODERR                             EQW8LBVR
02119            MOVE CURSEUR TO ECR-PRMTYPC2L                          EQW8LBVR
02120            MOVE 1       TO KONTROL                                EQW8LBVR
02121            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
02122         END-IF                                                    EQW8LBVR
02123      END-IF.                                                      EQW8LBVR
02124                                                                   EQW8LBVR
02125 *--- DATE DE PERMIS DOIT ETRE NUMERIQUE ET AU FORMAT DATE         EQW8LBVR
02126      IF ECR-PRMOBTD1O NOT = SPACES                                EQW8LBVR
02127         MOVE ECR-PRMOBTD1O TO WSS-DATE-A-VERIFIER                 EQW8LBVR
02128         PERFORM VERIF-DATE THRU FVERIF-DATE                       EQW8LBVR
02129         IF WSS-DATE-OK = 'N'                                      EQW8LBVR
02130            MOVE NOR-ALP TO ECR-PRMOBTD1A                          EQW8LBVR
02131            MOVE 'FB010' TO COM-GENE-MESANO                        EQW8LBVR
02132                            COM-CODERR                             EQW8LBVR
02133            MOVE CURSEUR TO ECR-PRMOBTD1L                          EQW8LBVR
02134            MOVE 1       TO KONTROL                                EQW8LBVR
02135            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
02136         END-IF                                                    EQW8LBVR
02137      END-IF.                                                      EQW8LBVR
02138                                                                   EQW8LBVR
02139      IF ECR-PRMOBTD2O NOT = SPACES                                EQW8LBVR
02140         MOVE ECR-PRMOBTD2O TO WSS-DATE-A-VERIFIER                 EQW8LBVR
02141         PERFORM VERIF-DATE THRU FVERIF-DATE                       EQW8LBVR
02142         IF WSS-DATE-OK = 'N'                                      EQW8LBVR
02143            MOVE NOR-ALP TO ECR-PRMOBTD2A                          EQW8LBVR
02144            MOVE 'FB010' TO COM-GENE-MESANO                        EQW8LBVR
02145                            COM-CODERR                             EQW8LBVR
02146            MOVE CURSEUR TO ECR-PRMOBTD2L                          EQW8LBVR
02147            MOVE 1       TO KONTROL                                EQW8LBVR
02148            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
02149         END-IF                                                    EQW8LBVR
02150      END-IF.                                                      EQW8LBVR
02151                                                                   EQW8LBVR
02152 *--- CONDUITE ACCOMPAGNEE                                         EQW8LBVR
02153      IF ECR-PERCOACO NOT = SPACES                                 EQW8LBVR
02154         IF ECR-PERCOACO NOT = 'O' AND 'N'                         EQW8LBVR
02155            MOVE NOR-ALP TO ECR-PERCOACA                           EQW8LBVR
02156            MOVE 'FB011' TO COM-GENE-MESANO                        EQW8LBVR
02157                            COM-CODERR                             EQW8LBVR
02158            MOVE CURSEUR TO ECR-PERCOACL                           EQW8LBVR
02159            MOVE 1       TO KONTROL                                EQW8LBVR
02160            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
02161         END-IF                                                    EQW8LBVR
02162      END-IF.                                                      EQW8LBVR
02163                                                                   EQW8LBVR
02164 *--- NOMBRE DE MOIS DU RELEVE D'INFORMATION                       EQW8LBVR
02165      IF ECR-ANPANCNO NOT = SPACES                                 EQW8LBVR
02166         MOVE ECR-ANPANCNO  TO C-XKMTENTREE                        EQW8LBVR
02167         MOVE 2             TO C-XKMTLONG                          EQW8LBVR
02168         MOVE 0             TO C-XKMTDECIMALE                      EQW8LBVR
02169         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW8LBVR
02170         IF C-XKMTRETCOD NOT = 0                                   EQW8LBVR
02171            MOVE NOR-ALP TO ECR-ANPANCNA                           EQW8LBVR
02172            MOVE 'FB015' TO COM-GENE-MESANO                        EQW8LBVR
02173                            COM-CODERR                             EQW8LBVR
02174            MOVE CURSEUR TO ECR-ANPANCNL                           EQW8LBVR
02175            MOVE 1       TO KONTROL                                EQW8LBVR
02176            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
02177         ELSE                                                      EQW8LBVR
02178            MOVE C-XKMTNUM-0D TO WSS-NBMOIS-INFO                   EQW8LBVR
02179            MOVE C-XKMTSORTIE TO ECR-ANPANCNO                      EQW8LBVR
02180            IF WSS-NBMOIS-INFO  > 36                               EQW8LBVR
02181               MOVE NOR-ALP TO ECR-ANPANCNA                        EQW8LBVR
02182               MOVE 'FB015' TO COM-GENE-MESANO                     EQW8LBVR
02183                               COM-CODERR                          EQW8LBVR
02184               MOVE CURSEUR TO ECR-ANPANCNL                        EQW8LBVR
02185               MOVE 1       TO KONTROL                             EQW8LBVR
02186               GO TO FIN-CONTROLE-SYNTAXE                          EQW8LBVR
02187            END-IF                                                 EQW8LBVR
02188         END-IF                                                    EQW8LBVR
02189      END-IF.                                                      EQW8LBVR
02190                                                                   EQW8LBVR
F3576 *--- INDICATEUR ANNULATION OU SUSPENSION DE PERMIS                EQW8LBVR
02192      IF ECR-ANPINDCO NOT = SPACES                                 EQW8LBVR
F3576         IF ECR-ANPINDCO NOT = 'A' AND 'S' AND 'N'                 EQW8LBVR
02194            MOVE NOR-ALP TO ECR-ANPINDCA                           EQW8LBVR
02195            MOVE 'FB472' TO COM-GENE-MESANO                        EQW8LBVR
02196                            COM-CODERR                             EQW8LBVR
02197            MOVE CURSEUR TO ECR-ANPINDCL                           EQW8LBVR
02198            MOVE 1       TO KONTROL                                EQW8LBVR
02199            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
02200         END-IF                                                    EQW8LBVR
02201      END-IF.                                                      EQW8LBVR
02202                                                                   EQW8LBVR
02203 *--- NOMBRE DE JOURS DE RETRAIT DE PERMIS                         EQW8LBVR
02204      IF ECR-ANPNBJNO NOT = SPACES                                 EQW8LBVR
02205         MOVE ECR-ANPNBJNO  TO C-XKMTENTREE                        EQW8LBVR
02206         MOVE 3             TO C-XKMTLONG                          EQW8LBVR
02207         MOVE 0             TO C-XKMTDECIMALE                      EQW8LBVR
02208         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW8LBVR
02209         IF C-XKMTRETCOD NOT = 0                                   EQW8LBVR
02210            MOVE NOR-ALP TO ECR-ANPNBJNA                           EQW8LBVR
02211            MOVE 'FB474' TO COM-GENE-MESANO                        EQW8LBVR
02212                            COM-CODERR                             EQW8LBVR
02213            MOVE CURSEUR TO ECR-ANPNBJNL                           EQW8LBVR
02214            MOVE 1       TO KONTROL                                EQW8LBVR
02215            GO TO FIN-CONTROLE-SYNTAXE                             EQW8LBVR
02216         ELSE                                                      EQW8LBVR
02217            MOVE C-XKMTNUM-0D TO WSS-NBJOURS-RETRAIT               EQW8LBVR
02218            MOVE C-XKMTSORTIE TO ECR-ANPNBJNO                      EQW8LBVR
02219            IF WSS-NBJOURS-RETRAIT < 31                            EQW8LBVR
02220               MOVE NOR-ALP TO ECR-ANPNBJNA                        EQW8LBVR
02221               MOVE 'FB474' TO COM-GENE-MESANO                     EQW8LBVR
02222                               COM-CODERR                          EQW8LBVR
02223               MOVE CURSEUR TO ECR-ANPNBJNL                        EQW8LBVR
02224               MOVE 1       TO KONTROL                             EQW8LBVR
02225               GO TO FIN-CONTROLE-SYNTAXE                          EQW8LBVR
02226            END-IF                                                 EQW8LBVR
02227         END-IF                                                    EQW8LBVR
02228      END-IF.                                                      EQW8LBVR
02229                                                                   EQW8LBVR
02230                                                                   EQW8LBVR
02231  FIN-CONTROLE-SYNTAXE.  EXIT.                                     EQW8LBVR
02232                                                                   EQW8LBVR
02233 ***************************************************************** EQW8LBVR
02234 * CONTROLES LOGIQUES    * FB02 * TRAITEMENT NORMAL * *            EQW8LBVR
02235 ***************************************************************** EQW8LBVR
02236  CONTROLE-LOGIQUE.                                                EQW8LBVR
02237                                                                   EQW8LBVR
02238 *--- STATUT OBLIGATOIRE                                           EQW8LBVR
02239      IF ECR-PERSTACO = SPACES                                     EQW8LBVR
02240         MOVE NOR-ALP TO ECR-PERSTACA                              EQW8LBVR
02241         MOVE 'FB237' TO COM-GENE-MESANO                           EQW8LBVR
02242                         COM-CODERR                                EQW8LBVR
02243         MOVE CURSEUR TO ECR-PERSTACL                              EQW8LBVR
02244         MOVE 1       TO KONTROL                                   EQW8LBVR
02245         GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02246      END-IF.                                                      EQW8LBVR
02247                                                                   EQW8LBVR
02248 *--- STATUT : 1 SEUL CF ET 1 SEUL CJ AU CONTRAT                   EQW8LBVR
02249      IF ECR-PERSTACO NOT = SPACE                                  EQW8LBVR
02250         IF (ECR-PERSTACO = 'CF' AND COM-FB-NBRE-PERS-CF > 0) OR   EQW8LBVR
02251            (ECR-PERSTACO = 'CJ' AND COM-FB-NBRE-PERS-CJ > 0)      EQW8LBVR
02252            MOVE NOR-ALP TO ECR-PERSTACA                           EQW8LBVR
02253            MOVE 'FB087' TO COM-GENE-MESANO                        EQW8LBVR
02254                            COM-CODERR                             EQW8LBVR
02255            MOVE CURSEUR TO ECR-PERSTACL                           EQW8LBVR
02256            MOVE 1       TO KONTROL                                EQW8LBVR
02257            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02258         END-IF                                                    EQW8LBVR
02259      END-IF.                                                      EQW8LBVR
02260                                                                   EQW8LBVR
02261 *--- STATUT : 1 SEUL PM AU CONTRAT                                EQW8LBVR
02262      IF ECR-PERSTACO NOT = SPACE                                  EQW8LBVR
02263         IF (ECR-PERSTACO = 'PM' AND COM-FB-NBRE-PERS-PM > 0)      EQW8LBVR
02264            MOVE NOR-ALP TO ECR-PERSTACA                           EQW8LBVR
02265            MOVE 'FB299' TO COM-GENE-MESANO                        EQW8LBVR
02266                            COM-CODERR                             EQW8LBVR
02267            MOVE CURSEUR TO ECR-PERSTACL                           EQW8LBVR
02268            MOVE 1       TO KONTROL                                EQW8LBVR
02269            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02270         END-IF                                                    EQW8LBVR
02271      END-IF.                                                      EQW8LBVR
02272                                                                   EQW8LBVR
02273 *--- STATUT CF, CJ ET EN INCOMPATIBLE AVEC PM                     EQW8LBVR
02274      IF ECR-PERSTACO NOT = SPACE                                  EQW8LBVR
02275         IF ECR-PERSTACO = 'PM' AND                                EQW8LBVR
02276            (COM-FB-NBRE-PERS-CF > 0 OR                            EQW8LBVR
02277            COM-FB-NBRE-PERS-CJ > 0 OR                             EQW8LBVR
02278            COM-FB-NBRE-PERS-EN > 0)                               EQW8LBVR
02279            MOVE NOR-ALP TO ECR-PERSTACA                           EQW8LBVR
02280            MOVE 'FB088' TO COM-GENE-MESANO                        EQW8LBVR
02281                            COM-CODERR                             EQW8LBVR
02282            MOVE CURSEUR TO ECR-PERSTACL                           EQW8LBVR
02283            MOVE 1       TO KONTROL                                EQW8LBVR
02284            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02285         END-IF                                                    EQW8LBVR
02275         IF (ECR-PERSTACO = 'CF' OR 'CJ' OR 'EN') AND              EQW8LBVR
02276            (COM-FB-NBRE-PERS-PM > 0)                              EQW8LBVR
02279            MOVE NOR-ALP TO ECR-PERSTACA                           EQW8LBVR
02280            MOVE 'FB088' TO COM-GENE-MESANO                        EQW8LBVR
02281                            COM-CODERR                             EQW8LBVR
02282            MOVE CURSEUR TO ECR-PERSTACL                           EQW8LBVR
02283            MOVE 1       TO KONTROL                                EQW8LBVR
02284            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02285         END-IF                                                    EQW8LBVR
02286      END-IF.                                                      EQW8LBVR
02287                                                                   EQW8LBVR
02288 *--- NOMBRE DE VÈHICULE TRACTEUR DOIT ÍTRE <= 1 SI STATUT PM      EQW8LBVR
F2980 *    IF ECR-PERSTACO NOT = SPACE                                  EQW8LBVR
F2980 *          COMPUTE WSS-NBRE-VEHI-4R2RCC = COM-FB-NBRE-VEHI-ENC    EQW8LBVR
F2980 *                                  - COM-FB-NBRE-VEHI-TRACTE      EQW8LBVR
F2980 *       IF ECR-PERSTACO = 'PM' AND                                EQW8LBVR
F2980 *          WSS-NBRE-VEHI-4R2RCC > 1                               EQW8LBVR
F2980 *          MOVE NOR-ALP TO ECR-PERSTACA                           EQW8LBVR
F2980 *          MOVE 'FB191' TO COM-GENE-MESANO                        EQW8LBVR
F2980 *                          COM-CODERR                             EQW8LBVR
F2980 *          MOVE CURSEUR TO ECR-PERSTACL                           EQW8LBVR
F2980 *          MOVE 1       TO KONTROL                                EQW8LBVR
F2980 *          GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
F2980 *       END-IF                                                    EQW8LBVR
F2980 *    END-IF.                                                      EQW8LBVR
02302                                                                   EQW8LBVR
02303 *--- NB DE SALARIES NON RENSEIGNE SI STATUT DIFFERENT DE 'PM'     EQW8LBVR
02304      IF ECR-PERSALNO NOT = SPACES AND                             EQW8LBVR
02305         ECR-PERSTACO NOT = 'PM'                                   EQW8LBVR
02306         MOVE NOR-ALP TO ECR-PERSALNA                              EQW8LBVR
02307         MOVE 'FB020' TO COM-GENE-MESANO                           EQW8LBVR
02308                         COM-CODERR                                EQW8LBVR
02309         MOVE CURSEUR TO ECR-PERSALNL                              EQW8LBVR
02310         MOVE 1       TO KONTROL                                   EQW8LBVR
02311         GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02312      END-IF.                                                      EQW8LBVR
02313                                                                   EQW8LBVR
02314 *--- SAISIE DU NOM INTERDITE SI STATUT 'PM' ET TITRE ABSENT       EQW8LBVR
02315      IF ECR-PERNOMLO NOT = SPACES AND ECR-PERTITLO = SPACES AND   EQW8LBVR
02316         ECR-PERSTACO = 'PM'                                       EQW8LBVR
02317         MOVE NOR-ALP TO ECR-PERNOMLA                              EQW8LBVR
02318         MOVE 'FB300' TO COM-GENE-MESANO                           EQW8LBVR
02319                         COM-CODERR                                EQW8LBVR
02320         MOVE CURSEUR TO ECR-PERNOMLL                              EQW8LBVR
02321         MOVE 1       TO KONTROL                                   EQW8LBVR
02322         GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02323      END-IF.                                                      EQW8LBVR
02324                                                                   EQW8LBVR
02325 *--- SAISIE DU PRENOM INTERDITE SI STATUT 'PM' ET TITRE ABSENT    EQW8LBVR
02326      IF ECR-PERPRELO NOT = SPACES AND ECR-PERTITLO = SPACES AND   EQW8LBVR
02327         ECR-PERSTACO = 'PM'                                       EQW8LBVR
02328         MOVE NOR-ALP TO ECR-PERPRELA                              EQW8LBVR
02329         MOVE 'FB301' TO COM-GENE-MESANO                           EQW8LBVR
02330                         COM-CODERR                                EQW8LBVR
02331         MOVE CURSEUR TO ECR-PERPRELL                              EQW8LBVR
02332         MOVE 1       TO KONTROL                                   EQW8LBVR
02333         GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02334      END-IF.                                                      EQW8LBVR
02335                                                                   EQW8LBVR
02336 *--- SAISIE DE LA DATE DE NAISSANCE INTERDITE SI STATUT 'PM' ET   EQW8LBVR
02337 *--- TITRE ABSENT                                                 EQW8LBVR
02338      IF ECR-PERNAIDO NOT = SPACES AND ECR-PERTITLO = SPACES AND   EQW8LBVR
02339         ECR-PERSTACO = 'PM'                                       EQW8LBVR
02340         MOVE NOR-ALP TO ECR-PERNAIDA                              EQW8LBVR
02341         MOVE 'FB302' TO COM-GENE-MESANO                           EQW8LBVR
02342                         COM-CODERR                                EQW8LBVR
02343         MOVE CURSEUR TO ECR-PERNAIDL                              EQW8LBVR
02344         MOVE 1       TO KONTROL                                   EQW8LBVR
02345         GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02346      END-IF.                                                      EQW8LBVR
02347                                                                   EQW8LBVR
02348 *--- DATE DU JOUR - DATE DE NAISSANCE >= 14 ANS                   EQW8LBVR
02349      IF ECR-PERNAIDO NOT = SPACES                                 EQW8LBVR
02350         MOVE 14 TO WSS-AGEMIN                                     EQW8LBVR
02351         MOVE ECR-PERNAIDO(5:4) TO WSS-DATE-NAIS-SAMJ-SA           EQW8LBVR
02352         MOVE ECR-PERNAIDO(3:2) TO WSS-DATE-NAIS-SAMJ-M            EQW8LBVR
02353         MOVE ECR-PERNAIDO(1:2) TO WSS-DATE-NAIS-SAMJ-J            EQW8LBVR
02354         PERFORM CALCUL-DATE-MINIMUM THRU FCALCUL-DATE-MINIMUM     EQW8LBVR
02355         IF WSS-DATE-NAIS-SAMJ > WSS-DATE-MINI-SAMJ                EQW8LBVR
02356            MOVE NOR-ALP TO ECR-PERNAIDA                           EQW8LBVR
02357            MOVE 'FB021' TO COM-GENE-MESANO                        EQW8LBVR
02358                            COM-CODERR                             EQW8LBVR
02359            MOVE CURSEUR TO ECR-PERNAIDL                           EQW8LBVR
02360            MOVE 1       TO KONTROL                                EQW8LBVR
02361            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02362         END-IF                                                    EQW8LBVR
02363      END-IF.                                                      EQW8LBVR
02364                                                                   EQW8LBVR
02365 *--- EN AVENANT, INTERDICTION DE MODIFIER PLUS D'UNE INFORMATION  EQW8LBVR
02366 *--- PARMI LE NOM, LE PRENOM ET LA DATE DE NAISSANCE              EQW8LBVR
02367      IF (COM-FB-CODE-ACTION = 'M') AND                            EQW8LBVR
02368         (PERACTC OF TS-PERSONNE(1) NOT = 'I')                     EQW8LBVR
02369         PERFORM DETER-MODIF-CONDUCTEUR THRU                       EQW8LBVR
02370                                          FDETER-MODIF-CONDUCTEUR  EQW8LBVR
F1856         IF NBR-MODIF-COND > 1                                     EQW8LBVR
02372            MOVE NOR-ALP TO ECR-PERNOMLA                           EQW8LBVR
02373            MOVE 'FB089' TO COM-GENE-MESANO                        EQW8LBVR
02374                            COM-CODERR                             EQW8LBVR
02375            MOVE CURSEUR TO ECR-PERNOMLL                           EQW8LBVR
02376            MOVE 1       TO KONTROL                                EQW8LBVR
02377            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02378         END-IF                                                    EQW8LBVR
02379      END-IF.                                                      EQW8LBVR
02380                                                                   EQW8LBVR
02381 *--- LE SEXE DOIT ÍTRE COMPATIBLE AVEC LE TITRE                   EQW8LBVR
02382      IF ECR-PERTITLO NOT = SPACES AND ECR-PERSEXCO NOT = SPACES   EQW8LBVR
02383         IF ECR-PERTITLO = 'MR '                                   EQW8LBVR
02384            IF ECR-PERSEXCO NOT = 'M'                              EQW8LBVR
02385               MOVE NOR-ALP TO ECR-PERSEXCA                        EQW8LBVR
02386               MOVE 'FB290' TO COM-GENE-MESANO                     EQW8LBVR
02387                               COM-CODERR                          EQW8LBVR
02388               MOVE CURSEUR TO ECR-PERSEXCL                        EQW8LBVR
02389               MOVE 1       TO KONTROL                             EQW8LBVR
02390               GO TO FIN-CONTROLE-LOGIQUE                          EQW8LBVR
02391            END-IF                                                 EQW8LBVR
02392         END-IF                                                    EQW8LBVR
02393         IF ECR-PERTITLO = 'MME' OR 'MLE'                          EQW8LBVR
02394            IF ECR-PERSEXCO NOT = 'F'                              EQW8LBVR
02395               MOVE NOR-ALP TO ECR-PERSEXCA                        EQW8LBVR
02396               MOVE 'FB290' TO COM-GENE-MESANO                     EQW8LBVR
02397                               COM-CODERR                          EQW8LBVR
02398               MOVE CURSEUR TO ECR-PERSEXCL                        EQW8LBVR
02399               MOVE 1       TO KONTROL                             EQW8LBVR
02400               GO TO FIN-CONTROLE-LOGIQUE                          EQW8LBVR
02401            END-IF                                                 EQW8LBVR
02402         END-IF                                                    EQW8LBVR
02403      END-IF.                                                      EQW8LBVR
02404                                                                   EQW8LBVR
02405 *--- SAISIE DU SEXE INTERDITE SI STATUT 'PM' ET TITRE ABSENT      EQW8LBVR
02406      IF ECR-PERSEXCO NOT = SPACES AND ECR-PERTITLO = SPACES AND   EQW8LBVR
02407         ECR-PERSTACO = 'PM'                                       EQW8LBVR
02408         MOVE NOR-ALP TO ECR-PERSEXCA                              EQW8LBVR
02409         MOVE 'FB303' TO COM-GENE-MESANO                           EQW8LBVR
02410                         COM-CODERR                                EQW8LBVR
02411         MOVE CURSEUR TO ECR-PERSEXCL                              EQW8LBVR
02412         MOVE 1       TO KONTROL                                   EQW8LBVR
02413         GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02414      END-IF.                                                      EQW8LBVR
02415                                                                   EQW8LBVR
02416 *--- NOMBRE D'ENFANTS NON RENSEIGNE SI STATUT 'PM'                EQW8LBVR
02417      IF ECR-PERENCNO NOT = SPACES AND                             EQW8LBVR
02418         ECR-PERSTACO = 'PM'                                       EQW8LBVR
02419         MOVE NOR-ALP TO ECR-PERENCNA                              EQW8LBVR
02420         MOVE 'FB023' TO COM-GENE-MESANO                           EQW8LBVR
02421                         COM-CODERR                                EQW8LBVR
02422         MOVE CURSEUR TO ECR-PERENCNL                              EQW8LBVR
02423         MOVE 1       TO KONTROL                                   EQW8LBVR
02424         GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02425      END-IF.                                                      EQW8LBVR
02426                                                                   EQW8LBVR
02427 *--- SITUATION MATRIMONIALE NON RENSEIGNE SI STATUT = 'PM'        EQW8LBVR
02428      IF ECR-PERMATCO NOT = SPACES AND                             EQW8LBVR
02429         ECR-PERSTACO = 'PM'                                       EQW8LBVR
02430         MOVE NOR-ALP TO ECR-PERMATCA                              EQW8LBVR
02431         MOVE 'FB022' TO COM-GENE-MESANO                           EQW8LBVR
02432                         COM-CODERR                                EQW8LBVR
02433         MOVE CURSEUR TO ECR-PERMATCL                              EQW8LBVR
02434         MOVE 1       TO KONTROL                                   EQW8LBVR
02435         GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02436      END-IF.                                                      EQW8LBVR
02437                                                                   EQW8LBVR
02438 *--- SI STATUT PM SEUL LES CODES PROFESSION SUIVANTS SONT POSSIBLEEQW8LBVR
F39250*--- '01','02','09','10','11','12','13', AJOUT '14'               EQW8LBVR
02440      IF WSS-APPEL-AIDE-PROFESSION = 'N'                           EQW8LBVR
02441         IF (ECR-PERSTACO = 'PM') AND                              EQW8LBVR
02442            (ECR-PERPROCO NOT = SPACES) AND                        EQW8LBVR
02443            (ECR-PERPROCO NOT = '01' AND '02' AND '09' AND '10'    EQW8LBVR
F39250                           AND '11' AND '12' AND '13' AND '14')   EQW8LBVR
02445            MOVE NOR-ALP TO ECR-PERPROCA                           EQW8LBVR
02446            MOVE 'FB293' TO COM-GENE-MESANO                        EQW8LBVR
02447                            COM-CODERR                             EQW8LBVR
02448            MOVE CURSEUR TO ECR-PERPROCL                           EQW8LBVR
02449            MOVE 1       TO KONTROL                                EQW8LBVR
02450            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02451         END-IF                                                    EQW8LBVR
02452      END-IF.                                                      EQW8LBVR
02453                                                                   EQW8LBVR
02454 *--- LIBELLE PROFESSION IMPOSSIBLE SANS SON CODE                  EQW8LBVR
02455      IF WSS-APPEL-AIDE-PROFESSION = 'N'                           EQW8LBVR
02456         IF ECR-PERPROCO = SPACES AND ECR-PERPROLO NOT = SPACES    EQW8LBVR
02457            MOVE NOR-ALP TO ECR-PERPROLA                           EQW8LBVR
02458            MOVE 'FB304' TO COM-GENE-MESANO                        EQW8LBVR
02459                            COM-CODERR                             EQW8LBVR
02460            MOVE CURSEUR TO ECR-PERPROLL                           EQW8LBVR
02461            MOVE 1       TO KONTROL                                EQW8LBVR
02462            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02463         END-IF                                                    EQW8LBVR
02464      END-IF.                                                      EQW8LBVR
02465                                                                   EQW8LBVR
02466 *--- SAISIE LIBELLE PROFESSION OBLIGATOIRE SI CODE PROFESSION = 13EQW8LBVR
02467 *    IF ECR-PERPROCO = '13' AND ECR-PERPROLO = SPACES             EQW8LBVR
02468 *       MOVE NOR-ALP TO ECR-PERPROLA                              EQW8LBVR
02469 *       MOVE 'FB305' TO COM-GENE-MESANO                           EQW8LBVR
02470 *                       COM-CODERR                                EQW8LBVR
02471 *       MOVE CURSEUR TO ECR-PERPROLL                              EQW8LBVR
02472 *       MOVE 1       TO KONTROL                                   EQW8LBVR
02473 *       GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02474 *    END-IF.                                                      EQW8LBVR
02475 *                                                                 EQW8LBVR
02476 *--- ON RENSEIGNE LE LIBELLE PROFESSION A PARTIR DE FBPROF01      EQW8LBVR
02477                                                                   EQW8LBVR
02478      MOVE SPACES                     TO PERPROL OF TS-PERSONNE(2).EQW8LBVR
02479      IF (ECR-PERPROCO NOT = SPACES) AND                           EQW8LBVR
02480 *    IF (ECR-PERPROCO NOT = SPACES AND '13') AND                  EQW8LBVR
02481         WSS-APPEL-AIDE-PROFESSION = 'N'                           EQW8LBVR
02482         MOVE  SPACES                 TO XSPIPARM                  EQW8LBVR
02483         MOVE  'GP'                   TO FONCTION  OF XSPIPARM     EQW8LBVR
02484         MOVE  'FBPROF'                  TO TABLE-PREF             EQW8LBVR
02485         MOVE  COM-GENE-CODCIE-PRINCIPAL TO TABLE-SUFF             EQW8LBVR
02486         MOVE  IDENT-TABLE            TO CODTAB OF XSPIPARM        EQW8LBVR
02487         MOVE  '= '                   TO OPERATEUR OF XSPIPARM     EQW8LBVR
02488         MOVE  ECR-PERPROCO           TO REF-POSTE OF XSPIPARM     EQW8LBVR
02489         PERFORM ACCES-SPI THRU FIN-ACCES-SPI                      EQW8LBVR
02490         IF RETCOD OF XSPIPARM  = ZERO                             EQW8LBVR
02491            MOVE IOAREA  OF XSPIPARM TO FBPROF01                   EQW8LBVR
02492            MOVE PROLIBL OF FBPROF01 TO PERPROL OF TS-PERSONNE(2)  EQW8LBVR
02493         END-IF                                                    EQW8LBVR
02494      END-IF.                                                      EQW8LBVR
02495                                                                   EQW8LBVR
02496 *--- SAISIE LIBELLE PROFESSION INTERDITE SI CODE PROFESSION <> 13 EQW8LBVR
02497      IF WSS-APPEL-AIDE-PROFESSION = 'N'                           EQW8LBVR
02498 *       IF ECR-PERPROCO NOT = '13' AND ECR-PERPROLO NOT = SPACES  EQW8LBVR
02499         IF ECR-PERPROLO NOT = SPACES                              EQW8LBVR
02500           AND (ECR-PERPROLO NOT = PERPROL OF TS-PERSONNE(2))      EQW8LBVR
02501            MOVE NOR-ALP TO ECR-PERPROLA                           EQW8LBVR
02502            MOVE 'FB306' TO COM-GENE-MESANO                        EQW8LBVR
02503                            COM-CODERR                             EQW8LBVR
02504            MOVE CURSEUR TO ECR-PERPROLL                           EQW8LBVR
02505            MOVE 1       TO KONTROL                                EQW8LBVR
02506            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02507         END-IF                                                    EQW8LBVR
02508      END-IF.                                                      EQW8LBVR
02509                                                                   EQW8LBVR
02510 *--- TYPE DE PERMIS NON RENSEIGNE SI STATUT = 'PM' SANS           EQW8LBVR
02511 *    CONDUCTEUR DESIGNE                                           EQW8LBVR
02512      IF ECR-PRMTYPC1O NOT = SPACES AND                            EQW8LBVR
02513         ECR-PERSTACO = 'PM' AND ECR-PERTITLO = SPACES             EQW8LBVR
02514         MOVE NOR-ALP TO ECR-PRMTYPC1A                             EQW8LBVR
02515         MOVE 'FB295' TO COM-GENE-MESANO                           EQW8LBVR
02516                         COM-CODERR                                EQW8LBVR
02517         MOVE CURSEUR TO ECR-PRMTYPC1L                             EQW8LBVR
02518         MOVE 1       TO KONTROL                                   EQW8LBVR
02519         GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02520      END-IF.                                                      EQW8LBVR
02521                                                                   EQW8LBVR
02522 *--- TYPE DE PERMIS NON RENSEIGNE SI STATUT = 'PM' SANS           EQW8LBVR
02523 *    CONDUCTEUR DESIGNE                                           EQW8LBVR
02524      IF ECR-PRMTYPC2O NOT = SPACES AND                            EQW8LBVR
02525         ECR-PERSTACO = 'PM' AND ECR-PERTITLO = SPACES             EQW8LBVR
02526         MOVE NOR-ALP TO ECR-PRMTYPC2A                             EQW8LBVR
02527         MOVE 'FB295' TO COM-GENE-MESANO                           EQW8LBVR
02528                         COM-CODERR                                EQW8LBVR
02529         MOVE CURSEUR TO ECR-PRMTYPC2L                             EQW8LBVR
02530         MOVE 1       TO KONTROL                                   EQW8LBVR
02531         GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02532      END-IF.                                                      EQW8LBVR
02533                                                                   EQW8LBVR
02534 *--- EN AVENANT, INTERDICTION DE SUPPRIMER LES PERMIS B ET A, LE  EQW8LBVR
02535 *    BSR NE PEUT ETRE REMPLACE QUE PAR LE B, A OU A1, LE PERMIS A1EQW8LBVR
02536 *    PAR A OU B                                                   EQW8LBVR
02537      IF (COM-FB-CODE-ACTION = 'M') AND                            EQW8LBVR
02538         (PERACTC OF TS-PERSONNE(1) NOT = 'I') AND                 EQW8LBVR
02525         ((ECR-PERSTACO NOT = 'PM') OR                             EQW8LBVR
02525          (ECR-PERTITLO > SPACES))                                 EQW8LBVR
02539         PERFORM DETER-MODIF-PERMIS THRU FDETER-MODIF-PERMIS       EQW8LBVR
02540         IF WSS-MODIF-PERMIS = 'O'                                 EQW8LBVR
02541            MOVE 'FB090' TO COM-GENE-MESANO                        EQW8LBVR
02542                            COM-CODERR                             EQW8LBVR
02543            MOVE 1       TO KONTROL                                EQW8LBVR
02544            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02545         END-IF                                                    EQW8LBVR
02546      END-IF.                                                      EQW8LBVR
02547                                                                   EQW8LBVR
02548 *--- SAISIE DE 2 PERMIS IDENTIQUES IMPOSSIBLE                     EQW8LBVR
02549      IF ECR-PRMTYPC1O NOT = SPACES AND                            EQW8LBVR
02550         ECR-PRMTYPC2O NOT = SPACES                                EQW8LBVR
02551         IF ECR-PRMTYPC1O = ECR-PRMTYPC2O                          EQW8LBVR
02552            MOVE NOR-ALP TO ECR-PRMTYPC1A                          EQW8LBVR
02553            MOVE 'FB092' TO COM-GENE-MESANO                        EQW8LBVR
02554                            COM-CODERR                             EQW8LBVR
02555            MOVE CURSEUR TO ECR-PRMTYPC1L                          EQW8LBVR
02556            MOVE 1       TO KONTROL                                EQW8LBVR
02557            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02558         END-IF                                                    EQW8LBVR
02559      END-IF.                                                      EQW8LBVR
02560                                                                   EQW8LBVR
02561 *--- SAISIE DE 2 PERMIS MOTO INTERDITE                            EQW8LBVR
02562      IF ECR-PRMTYPC1O NOT = SPACES AND                            EQW8LBVR
02563         ECR-PRMTYPC2O NOT = SPACES                                EQW8LBVR
02564         IF (ECR-PRMTYPC1O = 'A  ' OR 'A1 ' OR 'BSR') AND          EQW8LBVR
02565            (ECR-PRMTYPC2O = 'A  ' OR 'A1 ' OR 'BSR')              EQW8LBVR
02566            MOVE NOR-ALP TO ECR-PRMTYPC1A                          EQW8LBVR
02567            MOVE 'FB091' TO COM-GENE-MESANO                        EQW8LBVR
02568                            COM-CODERR                             EQW8LBVR
02569            MOVE CURSEUR TO ECR-PRMTYPC1L                          EQW8LBVR
02570            MOVE 1       TO KONTROL                                EQW8LBVR
02571            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02572         END-IF                                                    EQW8LBVR
02573      END-IF.                                                      EQW8LBVR
02574                                                                   EQW8LBVR
02575 *--- CONTROLE DES DATES DE PERMIS                                 EQW8LBVR
02576      IF ECR-PRMOBTD1O NOT = SPACES                                EQW8LBVR
02577         MOVE ECR-PRMOBTD1O(1:2) TO WSS-DATE-PERM-SAMJ-J           EQW8LBVR
02578         MOVE ECR-PRMOBTD1O(3:2) TO WSS-DATE-PERM-SAMJ-M           EQW8LBVR
02579         MOVE ECR-PRMOBTD1O(5:4) TO WSS-DATE-PERM-SAMJ-SA          EQW8LBVR
02580         MOVE ECR-PRMTYPC1O TO WSS-TYPE-PERMIS                     EQW8LBVR
02581         PERFORM CONTROLE-DATE-PERMIS THRU FCONTROLE-DATE-PERMIS   EQW8LBVR
02582         IF WSS-DATE-PERMIS-OK = 'A'                               EQW8LBVR
02583            MOVE NOR-ALP TO ECR-PRMOBTD1A                          EQW8LBVR
02584            MOVE 'FB307' TO COM-GENE-MESANO                        EQW8LBVR
02585                            COM-CODERR                             EQW8LBVR
02586            MOVE CURSEUR TO ECR-PRMOBTD1L                          EQW8LBVR
02587            MOVE 1       TO KONTROL                                EQW8LBVR
02588            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02589         END-IF                                                    EQW8LBVR
02590         IF WSS-DATE-PERMIS-OK = 'J'                               EQW8LBVR
02591            MOVE NOR-ALP TO ECR-PRMOBTD1A                          EQW8LBVR
02592            MOVE 'FB377' TO COM-GENE-MESANO                        EQW8LBVR
02593                            COM-CODERR                             EQW8LBVR
02594            MOVE CURSEUR TO ECR-PRMOBTD1L                          EQW8LBVR
02595            MOVE 1       TO KONTROL                                EQW8LBVR
02596            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02597         END-IF                                                    EQW8LBVR
02598      END-IF.                                                      EQW8LBVR
02599                                                                   EQW8LBVR
02600      IF ECR-PRMOBTD2O NOT = SPACES                                EQW8LBVR
02601         MOVE ECR-PRMOBTD2O(1:2) TO WSS-DATE-PERM-SAMJ-J           EQW8LBVR
02602         MOVE ECR-PRMOBTD2O(3:2) TO WSS-DATE-PERM-SAMJ-M           EQW8LBVR
02603         MOVE ECR-PRMOBTD2O(5:4) TO WSS-DATE-PERM-SAMJ-SA          EQW8LBVR
02604         MOVE ECR-PRMTYPC2O TO WSS-TYPE-PERMIS                     EQW8LBVR
02605         PERFORM CONTROLE-DATE-PERMIS THRU FCONTROLE-DATE-PERMIS   EQW8LBVR
02606         IF WSS-DATE-PERMIS-OK = 'A'                               EQW8LBVR
02607            MOVE NOR-ALP TO ECR-PRMOBTD2A                          EQW8LBVR
02608            MOVE 'FB307' TO COM-GENE-MESANO                        EQW8LBVR
02609                            COM-CODERR                             EQW8LBVR
02610            MOVE CURSEUR TO ECR-PRMOBTD2L                          EQW8LBVR
02611            MOVE 1       TO KONTROL                                EQW8LBVR
02612            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02613         END-IF                                                    EQW8LBVR
02614         IF WSS-DATE-PERMIS-OK = 'J'                               EQW8LBVR
02615            MOVE NOR-ALP TO ECR-PRMOBTD2A                          EQW8LBVR
02616            MOVE 'FB377' TO COM-GENE-MESANO                        EQW8LBVR
02617                            COM-CODERR                             EQW8LBVR
02618            MOVE CURSEUR TO ECR-PRMOBTD2L                          EQW8LBVR
02619            MOVE 1       TO KONTROL                                EQW8LBVR
02620            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02621         END-IF                                                    EQW8LBVR
02622      END-IF.                                                      EQW8LBVR
02623                                                                   EQW8LBVR
02624 *--- SAISIE DU PERMIS SANS DATE D'OBTENTION INTERDITE             EQW8LBVR
02625      IF ECR-PRMTYPC1O NOT = SPACES                                EQW8LBVR
02626         IF ECR-PRMOBTD1O = SPACE                                  EQW8LBVR
02627            MOVE NOR-ALP TO ECR-PRMTYPC1A                          EQW8LBVR
02628            MOVE 'FB093' TO COM-GENE-MESANO                        EQW8LBVR
02629                            COM-CODERR                             EQW8LBVR
02630            MOVE CURSEUR TO ECR-PRMTYPC1L                          EQW8LBVR
02631            MOVE 1       TO KONTROL                                EQW8LBVR
02632            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02633         END-IF                                                    EQW8LBVR
02634      END-IF.                                                      EQW8LBVR
02635                                                                   EQW8LBVR
02636      IF ECR-PRMTYPC2O NOT = SPACES                                EQW8LBVR
02637         IF ECR-PRMOBTD2O = SPACE                                  EQW8LBVR
02638            MOVE NOR-ALP TO ECR-PRMTYPC2A                          EQW8LBVR
02639            MOVE 'FB093' TO COM-GENE-MESANO                        EQW8LBVR
02640                            COM-CODERR                             EQW8LBVR
02641            MOVE CURSEUR TO ECR-PRMTYPC2L                          EQW8LBVR
02642            MOVE 1       TO KONTROL                                EQW8LBVR
02643            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02644         END-IF                                                    EQW8LBVR
02645      END-IF.                                                      EQW8LBVR
02646                                                                   EQW8LBVR
02647 *--- SAISIE DATE D'OBTENTION SANS PERMIS IMPOSSIBLE               EQW8LBVR
02648      IF ECR-PRMOBTD1O NOT = SPACES                                EQW8LBVR
02649         IF ECR-PRMTYPC1O = SPACES                                 EQW8LBVR
02650            MOVE NOR-ALP TO ECR-PRMOBTD1A                          EQW8LBVR
02651            MOVE 'FB094' TO COM-GENE-MESANO                        EQW8LBVR
02652                            COM-CODERR                             EQW8LBVR
02653            MOVE CURSEUR TO ECR-PRMOBTD1L                          EQW8LBVR
02654            MOVE 1       TO KONTROL                                EQW8LBVR
02655            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02656         END-IF                                                    EQW8LBVR
02657      END-IF.                                                      EQW8LBVR
02658                                                                   EQW8LBVR
02659      IF ECR-PRMOBTD2O NOT = SPACES                                EQW8LBVR
02660         IF ECR-PRMTYPC2O = SPACES                                 EQW8LBVR
02661            MOVE NOR-ALP TO ECR-PRMOBTD2A                          EQW8LBVR
02662            MOVE 'FB094' TO COM-GENE-MESANO                        EQW8LBVR
02663                            COM-CODERR                             EQW8LBVR
02664            MOVE CURSEUR TO ECR-PRMOBTD2L                          EQW8LBVR
02665            MOVE 1       TO KONTROL                                EQW8LBVR
02666            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02667         END-IF                                                    EQW8LBVR
02668      END-IF.                                                      EQW8LBVR
02669                                                                   EQW8LBVR
02670 *--- CONDUITE ACCOMPAGNÈE INTERDITE SI STATUT = 'PM'              EQW8LBVR
02671      IF ECR-PERCOACO NOT = SPACES AND                             EQW8LBVR
02672         ECR-PERSTACO = 'PM'                                       EQW8LBVR
02673         MOVE NOR-ALP TO ECR-PERCOACA                              EQW8LBVR
02674         MOVE 'FB308' TO COM-GENE-MESANO                           EQW8LBVR
02675                         COM-CODERR                                EQW8LBVR
02676         MOVE CURSEUR TO ECR-PERCOACL                              EQW8LBVR
02677         MOVE 1       TO KONTROL                                   EQW8LBVR
02678         GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02679      END-IF.                                                      EQW8LBVR
02680                                                                   EQW8LBVR
02681 *--- SI 'O' A CONDUITE ACCOMPAGNEE ALORS CONDUCTEUR DE 18 ANS OU  EQW8LBVR
02682 *    PLUS                                                         EQW8LBVR
02683      IF ECR-PERCOACO = 'O'                                        EQW8LBVR
02684         MOVE 18 TO WSS-AGEMIN                                     EQW8LBVR
02685         MOVE ECR-PERNAIDO(5:4) TO WSS-DATE-NAIS-SAMJ-SA           EQW8LBVR
02686         MOVE ECR-PERNAIDO(3:2) TO WSS-DATE-NAIS-SAMJ-M            EQW8LBVR
02687         MOVE ECR-PERNAIDO(1:2) TO WSS-DATE-NAIS-SAMJ-J            EQW8LBVR
02688         PERFORM CALCUL-DATE-MINIMUM THRU FCALCUL-DATE-MINIMUM     EQW8LBVR
02689         IF WSS-DATE-NAIS-SAMJ > WSS-DATE-MINI-SAMJ                EQW8LBVR
02690            MOVE NOR-ALP TO ECR-PERCOACA                           EQW8LBVR
02691            MOVE 'FB024' TO COM-GENE-MESANO                        EQW8LBVR
02692                            COM-CODERR                             EQW8LBVR
02693            MOVE CURSEUR TO ECR-PERCOACL                           EQW8LBVR
02694            MOVE 1       TO KONTROL                                EQW8LBVR
02695            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02696         END-IF                                                    EQW8LBVR
02697      END-IF.                                                      EQW8LBVR
02698                                                                   EQW8LBVR
02699 *--- SI 'O' A CONDUITE ACCOMPAGNEE ALORS CONDUCTEUR MUNI DU       EQW8LBVR
02700 *    PERMIS B DEPUIS MOINS D'UN AN                                EQW8LBVR
02701      IF ECR-PERCOACO = 'O'                                        EQW8LBVR
02702         IF ECR-PRMTYPC1O NOT = 'B ' AND ECR-PRMTYPC2O NOT = 'B '  EQW8LBVR
02703            MOVE NOR-ALP TO ECR-PERCOACA                           EQW8LBVR
02704            MOVE 'FB025' TO COM-GENE-MESANO                        EQW8LBVR
02705                            COM-CODERR                             EQW8LBVR
02706            MOVE CURSEUR TO ECR-PERCOACL                           EQW8LBVR
02707            MOVE 1       TO KONTROL                                EQW8LBVR
02708            GO TO FIN-CONTROLE-LOGIQUE                             EQW8LBVR
02709         ELSE                                                      EQW8LBVR
02710            IF ECR-PRMTYPC1O = 'B'                                 EQW8LBVR
02711               MOVE ECR-PRMOBTD1O(5:4) TO WSS-DATE-PERM-SAMJ-SA    EQW8LBVR
02712               MOVE ECR-PRMOBTD1O(3:2) TO WSS-DATE-PERM-SAMJ-M     EQW8LBVR
02713               MOVE ECR-PRMOBTD1O(1:2) TO WSS-DATE-PERM-SAMJ-J     EQW8LBVR
02714               PERFORM CONTROLE-AAC THRU FCONTROLE-AAC             EQW8LBVR
02715               IF WSS-CONTROLE-AAC-OK = 'N'                        EQW8LBVR
02716                  MOVE NOR-ALP TO ECR-PERCOACA                     EQW8LBVR
02717                  MOVE 'FB025' TO COM-GENE-MESANO                  EQW8LBVR
02718                                  COM-CODERR                       EQW8LBVR
02719                  MOVE CURSEUR TO ECR-PERCOACL                     EQW8LBVR
02720                  MOVE 1       TO KONTROL                          EQW8LBVR
02721                  GO TO FIN-CONTROLE-LOGIQUE                       EQW8LBVR
02722               END-IF                                              EQW8LBVR
02723            END-IF                                                 EQW8LBVR
02724            IF ECR-PRMTYPC2O = 'B'                                 EQW8LBVR
02725               MOVE ECR-PRMOBTD2O(5:4) TO WSS-DATE-PERM-SAMJ-SA    EQW8LBVR
02726               MOVE ECR-PRMOBTD2O(3:2) TO WSS-DATE-PERM-SAMJ-M     EQW8LBVR
02727               MOVE ECR-PRMOBTD2O(1:2) TO WSS-DATE-PERM-SAMJ-J     EQW8LBVR
02728               PERFORM CONTROLE-AAC THRU FCONTROLE-AAC             EQW8LBVR
02729               IF WSS-CONTROLE-AAC-OK = 'N'                        EQW8LBVR
02730                  MOVE NOR-ALP TO ECR-PERCOACA                     EQW8LBVR
02731                  MOVE 'FB025' TO COM-GENE-MESANO                  EQW8LBVR
02732                                  COM-CODERR                       EQW8LBVR
02733                  MOVE CURSEUR TO ECR-PERCOACL                     EQW8LBVR
02734                  MOVE 1       TO KONTROL                          EQW8LBVR
02735                  GO TO FIN-CONTROLE-LOGIQUE                       EQW8LBVR
02736               END-IF                                              EQW8LBVR
02737            END-IF                                                 EQW8LBVR
02738         END-IF                                                    EQW8LBVR
02739      END-IF.                                                      EQW8LBVR
02740                                                                   EQW8LBVR
02752 *--- MOTIF DE RETRAIT INTERDIT SI PAS DE RETRAIT                  EQW8LBVR
02753      IF ECR-ANPMOTLO NOT = SPACES AND                             EQW8LBVR
02754         ECR-ANPINDCO = 'N'                                        EQW8LBVR
02755         MOVE NOR-ALP TO ECR-ANPMOTLA                              EQW8LBVR
F3576         MOVE 'FB473' TO COM-GENE-MESANO                           EQW8LBVR
02757                         COM-CODERR                                EQW8LBVR
02758         MOVE CURSEUR TO ECR-ANPMOTLL                              EQW8LBVR
02759         MOVE 1       TO KONTROL                                   EQW8LBVR
02760         GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02761      END-IF.                                                      EQW8LBVR
02762                                                                   EQW8LBVR
02741 *--- NB DE JOURS DE RETRAIT INTERDIT SI PAS DE RETRAIT            EQW8LBVR
02742      IF ECR-ANPNBJNO NOT = SPACES AND                             EQW8LBVR
F3576         (ECR-ANPINDCO = 'N' OR 'A')                               EQW8LBVR
02744         MOVE NOR-ALP TO ECR-ANPNBJNA                              EQW8LBVR
02745         MOVE 'FB475' TO COM-GENE-MESANO                           EQW8LBVR
02746                         COM-CODERR                                EQW8LBVR
02747         MOVE CURSEUR TO ECR-ANPNBJNL                              EQW8LBVR
02748         MOVE 1       TO KONTROL                                   EQW8LBVR
02749         GO TO FIN-CONTROLE-LOGIQUE                                EQW8LBVR
02750      END-IF.                                                      EQW8LBVR
02751                                                                   EQW8LBVR
02763                                                                   EQW8LBVR
02764  FIN-CONTROLE-LOGIQUE.  EXIT.                                     EQW8LBVR
02765 *--------                                                         EQW8LBVR
02766  CADRAGE.                                                         EQW8LBVR
02767 *--------                                                         EQW8LBVR
02768      MOVE ZERO TO C-XKMTRETCOD.                                   EQW8LBVR
02769      EXEC CICS LINK PROGRAM  ('XKMTCADR')                         EQW8LBVR
02770                     COMMAREA (XKMTWCOMMA)                         EQW8LBVR
02771                     LENGTH   (LENGTH OF XKMTWCOMMA)               EQW8LBVR
02772      END-EXEC.                                                    EQW8LBVR
02773      IF EIBRCODE NOT = LOW-VALUE                                  EQW8LBVR
02774         MOVE 'NKXK - ERREUR LINK XKMTCADR' TO MESS                EQW8LBVR
02775         GO TO ABANDON-TACHE                                       EQW8LBVR
02776      END-IF.                                                      EQW8LBVR
02777 *                                                                 EQW8LBVR
02778  FIN-CADRAGE.   EXIT.                                             EQW8LBVR
02779 /                                                                 EQW8LBVR
02780                                                                   EQW8LBVR
02781 **************************************************************    EQW8LBVR
02782 *    TEST DU STATUT POUR SAVOIR SI IL Y APPEL DE L'ECRAN     *    EQW8LBVR
02783 *                D'AIDE FB                                   *    EQW8LBVR
02784 **************************************************************    EQW8LBVR
02785  DETER-AIDE-STATUT.                                               EQW8LBVR
02786                                                                   EQW8LBVR
02787      MOVE ZERO  TO WSS-NB-POINT-INTERO.                           EQW8LBVR
02788      INSPECT ECR-PERSTACO TALLYING WSS-NB-POINT-INTERO            EQW8LBVR
02789                          FOR ALL '?'.                             EQW8LBVR
02790                                                                   EQW8LBVR
02791      IF WSS-NB-POINT-INTERO = ZERO                                EQW8LBVR
02792         MOVE 'N'   TO WSS-APPEL-AIDE-STATUT                       EQW8LBVR
02793      ELSE                                                         EQW8LBVR
02794         MOVE 'O'   TO WSS-APPEL-AIDE-STATUT                       EQW8LBVR
02795      END-IF.                                                      EQW8LBVR
02796                                                                   EQW8LBVR
02797  FDETER-AIDE-STATUT. EXIT.                                        EQW8LBVR
02798                                                                   EQW8LBVR
02799 **************************************************************    EQW8LBVR
02800 *    TEST DU CODE PROFESSION POUR SAVOIR SI IL Y APPEL DE    *    EQW8LBVR
02801 *                L'ECRAN D'AIDE FB                           *    EQW8LBVR
02802 **************************************************************    EQW8LBVR
02803  DETER-AIDE-PROFESSION.                                           EQW8LBVR
02804                                                                   EQW8LBVR
02805      MOVE ZERO  TO WSS-NB-POINT-INTERO.                           EQW8LBVR
02806      INSPECT ECR-PERPROCO TALLYING WSS-NB-POINT-INTERO            EQW8LBVR
02807                          FOR ALL '?'.                             EQW8LBVR
02808                                                                   EQW8LBVR
02809      IF WSS-NB-POINT-INTERO = ZERO                                EQW8LBVR
02810         MOVE 'N'   TO WSS-APPEL-AIDE-PROFESSION                   EQW8LBVR
02811      ELSE                                                         EQW8LBVR
02812         MOVE 'O'   TO WSS-APPEL-AIDE-PROFESSION                   EQW8LBVR
02813      END-IF.                                                      EQW8LBVR
02814                                                                   EQW8LBVR
02815  FDETER-AIDE-PROFESSION. EXIT.                                    EQW8LBVR
02816                                                                   EQW8LBVR
02817 ****************************************************************  EQW8LBVR
02818 *    CALCUL DE LA DATE MINIMUM POUR LES DATES DE NAISSANCE DES *  EQW8LBVR
02819 *    CONDUCTEURS QUI DOIVENT AVOIR AU MOINS 'WSS-AGEMIN' ANS   *  EQW8LBVR
02820 ****************************************************************  EQW8LBVR
02821  CALCUL-DATE-MINIMUM.                                             EQW8LBVR
02822 *                                                                 EQW8LBVR
02823      MOVE WSS-DATJOUR-S      TO WSS-DATE1-SAMJ-SA(1:2).           EQW8LBVR
02824      MOVE WSS-DATJOUR-A      TO WSS-DATE1-SAMJ-SA(3:2).           EQW8LBVR
02825      MOVE WSS-DATJOUR-M      TO WSS-DATE1-SAMJ-M.                 EQW8LBVR
02826      MOVE WSS-DATJOUR-J      TO WSS-DATE1-SAMJ-J.                 EQW8LBVR
02827                                                                   EQW8LBVR
02828      COMPUTE WSS-DATE1-SAMJ-SA = WSS-DATE1-SAMJ-SA                EQW8LBVR
02829                                  - WSS-AGEMIN.                    EQW8LBVR
02830                                                                   EQW8LBVR
02831      MOVE WSS-DATE1-SAMJ    TO WSS-DATE-MINI-SAMJ.                EQW8LBVR
02832 *                                                                 EQW8LBVR
02833  FCALCUL-DATE-MINIMUM.  EXIT.                                     EQW8LBVR
02834 *                                                                 EQW8LBVR
02835 ******************************************************************EQW8LBVR
02836 * CONTROLE DES DATES DE PERMIS                                   *EQW8LBVR
02837 * DATE DU JOUR >= DATE DE PERMIS (SINON, WSS-DATE-PERMIS-OK=J)   *EQW8LBVR
02838 * DATE DE PERMIS > DATE DE NAISSANCE + AGE MINIMUM D'OBTENTION   *EQW8LBVR
02839 *                                      PERMIS                    *EQW8LBVR
02840 *                                (SINON, WSS-DATE-PERMIS-OK=A)   *EQW8LBVR
02841 ******************************************************************EQW8LBVR
02842  CONTROLE-DATE-PERMIS.                                            EQW8LBVR
02843 * DATE DU JOUR                                                    EQW8LBVR
02844      MOVE WSS-DATJOUR-S      TO WSS-DATJOUR-S-9.                  EQW8LBVR
02845      MOVE WSS-DATJOUR-A      TO WSS-DATJOUR-A-9.                  EQW8LBVR
02846      MOVE WSS-DATJOUR-M      TO WSS-DATJOUR-M-9.                  EQW8LBVR
02847      MOVE WSS-DATJOUR-J      TO WSS-DATJOUR-J-9.                  EQW8LBVR
02848 * DATE DE NAISSANCE                                               EQW8LBVR
02849      MOVE ECR-PERNAIDO(5:4) TO WSS-DATE-NAIS-SAMJ-SA.             EQW8LBVR
02850      MOVE ECR-PERNAIDO(3:2) TO WSS-DATE-NAIS-SAMJ-M.              EQW8LBVR
02851      MOVE ECR-PERNAIDO(1:2) TO WSS-DATE-NAIS-SAMJ-J.              EQW8LBVR
02852 * TYPE DE PERMIS                                                  EQW8LBVR
02853      EVALUATE WSS-TYPE-PERMIS                                     EQW8LBVR
02854         WHEN 'A  '                                                EQW8LBVR
02855             MOVE 18 TO WSS-AGEMIN                                 EQW8LBVR
02856         WHEN 'B  '                                                EQW8LBVR
02857             MOVE 18 TO WSS-AGEMIN                                 EQW8LBVR
02858         WHEN 'A1 '                                                EQW8LBVR
02859             MOVE 16 TO WSS-AGEMIN                                 EQW8LBVR
02860         WHEN 'BSR'                                                EQW8LBVR
02861             MOVE 14 TO WSS-AGEMIN                                 EQW8LBVR
02862      END-EVALUATE.                                                EQW8LBVR
02863      COMPUTE  WSS-DATE-MINI-SAMJ-SA = WSS-DATE-NAIS-SAMJ-SA       EQW8LBVR
02864                                   + WSS-AGEMIN.                   EQW8LBVR
02865      MOVE WSS-DATE-NAIS-SAMJ-M TO WSS-DATE-MINI-SAMJ-M.           EQW8LBVR
02866      MOVE WSS-DATE-NAIS-SAMJ-J TO WSS-DATE-MINI-SAMJ-J.           EQW8LBVR
02867      IF WSS-DATJOUR-SAMJ-9 < WSS-DATE-PERM-SAMJ                   EQW8LBVR
02868         MOVE 'J' TO WSS-DATE-PERMIS-OK                            EQW8LBVR
02869      ELSE                                                         EQW8LBVR
02870         IF WSS-DATE-PERM-SAMJ < WSS-DATE-MINI-SAMJ                EQW8LBVR
02871            MOVE 'A' TO WSS-DATE-PERMIS-OK                         EQW8LBVR
02872         ELSE                                                      EQW8LBVR
02873            MOVE 'O' TO WSS-DATE-PERMIS-OK                         EQW8LBVR
02874         END-IF                                                    EQW8LBVR
02875      END-IF.                                                      EQW8LBVR
02876 *                                                                 EQW8LBVR
02877  FCONTROLE-DATE-PERMIS.  EXIT.                                    EQW8LBVR
02878 *                                                                 EQW8LBVR
02879 ***************************************************************   EQW8LBVR
02880 * MODULE DE CONTROLE DE VALIDITE D'UNE DATE SAISIE JJMMSSAA   *   EQW8LBVR
02881 ***************************************************************   EQW8LBVR
02882  VERIF-DATE.                                                      EQW8LBVR
02883      MOVE SPACES              TO K2COM-DATES.                     EQW8LBVR
02884      MOVE '1'                 TO K2-FONC.                         EQW8LBVR
02885      MOVE WSS-DATE-A-VERIFIER TO K2-DATE1.                        EQW8LBVR
02886      MOVE '1'                 TO K2-FORM1.                        EQW8LBVR
02887      EXEC CICS LINK PROGRAM  ('K200LDATE')                        EQW8LBVR
02888                     COMMAREA (K2COM-DATES)                        EQW8LBVR
02889                     LENGTH   (LENGTH OF K2COM-DATES)              EQW8LBVR
02890      END-EXEC.                                                    EQW8LBVR
02891                                                                   EQW8LBVR
02892      IF K2-RETCOD = '0'                                           EQW8LBVR
02893         MOVE 'O' TO WSS-DATE-OK                                   EQW8LBVR
02894      ELSE                                                         EQW8LBVR
02895         MOVE 'N' TO WSS-DATE-OK                                   EQW8LBVR
02896      END-IF.                                                      EQW8LBVR
02897                                                                   EQW8LBVR
02898  FVERIF-DATE. EXIT.                                               EQW8LBVR
02899                                                                   EQW8LBVR
02900 ******************************************************************EQW8LBVR
02901 * LA CATEGORIE DU PERMIS DOIT ETRE DANS LA TABLE FBPERM01        *EQW8LBVR
02902 ******************************************************************EQW8LBVR
02903  VERIF-PERMIS.                                                    EQW8LBVR
02904      MOVE  SPACES                 TO XSPIPARM                     EQW8LBVR
02905      MOVE  'GP'                   TO FONCTION  OF XSPIPARM        EQW8LBVR
02906      MOVE  'FBPERM'                  TO TABLE-PREF                EQW8LBVR
02907      MOVE  COM-GENE-CODCIE-PRINCIPAL TO TABLE-SUFF                EQW8LBVR
02908      MOVE  IDENT-TABLE            TO CODTAB OF XSPIPARM           EQW8LBVR
02909      MOVE  '= '                   TO OPERATEUR OF XSPIPARM        EQW8LBVR
02910      MOVE   WSS-TYPE-PERMIS       TO REF-POSTE OF XSPIPARM        EQW8LBVR
02911      PERFORM ACCES-SPI THRU FIN-ACCES-SPI                         EQW8LBVR
02912      IF RETCOD OF XSPIPARM  = ZERO                                EQW8LBVR
02913         MOVE IOAREA  OF XSPIPARM TO FBPERM01                      EQW8LBVR
02914         MOVE 'O' TO WSS-TYPE-PERMIS-OK                            EQW8LBVR
02915      END-IF.                                                      EQW8LBVR
02916                                                                   EQW8LBVR
02917  FIN-VERIF-PERMIS.  EXIT.                                         EQW8LBVR
02918                                                                   EQW8LBVR
02919  DETER-MODIF-CONDUCTEUR.                                          EQW8LBVR
02920 *----------------------*                                          EQW8LBVR
      *
      *-----------------------------------------------------------------------
      * CONTROLE DES 4 ZONES ECRAN :
      * - NOM DU CONDUCTEUR
      * - PRENOM DU CONDUTEUR
      * - DATE DE NAISSANCE
      * - DATE D'OBTENTION DU OU DES PERMIS
      *
02921 * SI PLUS D'UNE DE CES DONNEES A ETE MODIFIEE : IL S'AGIT D'UN CAS DE   VR
02922 * CHANGEMENT DE CONDUCTEUR.                                             VR
      *-----------------------------------------------------------------------
02935                                                                   EQW8LBVR
F1856      MOVE 0 TO NBR-MODIF-COND.                                    EQW8LBVR
F1856      IF ECR-PERNOMLO NOT = PERNOML OF TS-PERSONNE(4)              EQW8LBVR
F1856      OR (ECR-PERPRELO NOT = PERPREL OF TS-PERSONNE(4))
F1856         ADD 1 TO NBR-MODIF-COND                                   EQW8LBVR
F1856      END-IF.                                                      EQW8LBVR
F1856                                                                   EQW8LBVR
F1856      IF WSS-DATE-NAIS-SAMJ NOT = PERNAID OF TS-PERSONNE(4)        EQW8LBVR
F1856         ADD 1 TO NBR-MODIF-COND                                   EQW8LBVR
F1856      END-IF.                                                      EQW8LBVR
F1856 *
F1856 * SI LES DATES D'OBTENTION DES DEUX PERMIS SOTN INVERSEES, IL NE
F1856 * S'AGIT PAS D'UN CHANGEMENT DE CONDUCTEUR
F1856 *
F1856      MOVE ECR-PRMOBTD1O (1:2) TO WSS-DATE-PERM-SAMJ-J.
F1856      MOVE ECR-PRMOBTD1O (3:2) TO WSS-DATE-PERM-SAMJ-M.
F1856      MOVE ECR-PRMOBTD1O (5:4) TO WSS-DATE-PERM-SAMJ-SA.
F1856
F1856      MOVE ECR-PRMOBTD2O (1:2) TO WSS-DATE-PERM2-SAMJ-J.
F1856      MOVE ECR-PRMOBTD2O (3:2) TO WSS-DATE-PERM2-SAMJ-M.
F1856      MOVE ECR-PRMOBTD2O (5:4) TO WSS-DATE-PERM2-SAMJ-SA.
F1856
F1856      IF  WSS-DATE-PERM-SAMJ  = RPRMOBTD OF TS-PERSONNE (4 , 2)          VR
F1856      AND (WSS-DATE-PERM2-SAMJ = RPRMOBTD OF TS-PERSONNE (4 , 1))        VR
F1856          CONTINUE                                                       VR
F1856      ELSE                                                               VR
F1856          IF   RPRMOBTD OF TS-PERSONNE (4 , 1) > SPACES
F1856          AND (RPRMOBTD OF TS-PERSONNE (4 , 1) NOT =
F1856               WSS-DATE-PERM-SAMJ AND WSS-DATE-PERM2-SAMJ)
F1856               ADD 1 TO NBR-MODIF-COND                                   VR
F1856          END-IF
F1856
F1856          IF   RPRMOBTD OF TS-PERSONNE (4 , 1) NOT > SPACES
F1856          AND (WSS-DATE-PERM-SAMJ NUMERIC)                               VR
F1856               ADD 1 TO NBR-MODIF-COND                                   VR
F1856          END-IF
F1856
F1856          IF   RPRMOBTD OF TS-PERSONNE (4 , 2) > SPACES
F1856          AND (WSS-DATE-PERM2-SAMJ NOT = RPRMOBTD
F1856                                           OF TS-PERSONNE (4 , 2))
F1856               ADD 1 TO NBR-MODIF-COND                                   VR
F1856          END-IF
F1856
F1856          IF   RPRMOBTD OF TS-PERSONNE (4 , 2) NOT > SPACES
F1856          AND (WSS-DATE-PERM2-SAMJ NUMERIC)                              VR
F1856               ADD 1 TO NBR-MODIF-COND                                   VR
F1856          END-IF
F1856      END-IF.
02935                                                                   EQW8LBVR
02936  FDETER-MODIF-CONDUCTEUR.  EXIT.                                  EQW8LBVR
02937                                                                   EQW8LBVR
02938  DETER-MODIF-PERMIS.                                              EQW8LBVR
02939 *------------------*                                              EQW8LBVR
02940      MOVE 'N' TO WSS-MODIF-PERMIS.                                EQW8LBVR
02941                                                                   EQW8LBVR
02942         IF PRMTYPC OF TS-PERSONNE(4, 1) = 'A  '                   EQW8LBVR
02943            IF ECR-PRMTYPC1O NOT = 'A  '                           EQW8LBVR
02944               MOVE 'O' TO WSS-MODIF-PERMIS                        EQW8LBVR
02945               MOVE NOR-ALP TO ECR-PRMTYPC1A                       EQW8LBVR
02946               MOVE CURSEUR TO ECR-PRMTYPC1L                       EQW8LBVR
02947               GO TO FDETER-MODIF-PERMIS                           EQW8LBVR
02948            END-IF                                                 EQW8LBVR
02949         END-IF.                                                   EQW8LBVR
02950                                                                   EQW8LBVR
02951         IF PRMTYPC OF TS-PERSONNE(4, 2) = 'A  '                   EQW8LBVR
02952            IF ECR-PRMTYPC2O NOT = 'A  '                           EQW8LBVR
02953               MOVE 'O' TO WSS-MODIF-PERMIS                        EQW8LBVR
02954               MOVE NOR-ALP TO ECR-PRMTYPC2A                       EQW8LBVR
02955               MOVE CURSEUR TO ECR-PRMTYPC2L                       EQW8LBVR
02956               GO TO FDETER-MODIF-PERMIS                           EQW8LBVR
02957            END-IF                                                 EQW8LBVR
02958         END-IF.                                                   EQW8LBVR
02959                                                                   EQW8LBVR
02960         IF PRMTYPC OF TS-PERSONNE(4, 1) = 'B  '                   EQW8LBVR
02961            IF ECR-PRMTYPC1O NOT = 'B  '                           EQW8LBVR
02962               MOVE 'O' TO WSS-MODIF-PERMIS                        EQW8LBVR
02963               MOVE NOR-ALP TO ECR-PRMTYPC1A                       EQW8LBVR
02964               MOVE CURSEUR TO ECR-PRMTYPC1L                       EQW8LBVR
02965               GO TO FDETER-MODIF-PERMIS                           EQW8LBVR
02966            END-IF                                                 EQW8LBVR
02967         END-IF.                                                   EQW8LBVR
02968                                                                   EQW8LBVR
02969         IF PRMTYPC OF TS-PERSONNE(4, 2) = 'B  '                   EQW8LBVR
02970            IF ECR-PRMTYPC2O NOT = 'B  '                           EQW8LBVR
02971               MOVE 'O' TO WSS-MODIF-PERMIS                        EQW8LBVR
02972               MOVE NOR-ALP TO ECR-PRMTYPC2A                       EQW8LBVR
02973               MOVE CURSEUR TO ECR-PRMTYPC2L                       EQW8LBVR
02974               GO TO FDETER-MODIF-PERMIS                           EQW8LBVR
02975            END-IF                                                 EQW8LBVR
02976         END-IF.                                                   EQW8LBVR
02977                                                                   EQW8LBVR
02978         IF PRMTYPC OF TS-PERSONNE(4, 1) = 'BSR'                   EQW8LBVR
02979            IF ECR-PRMTYPC1O NOT = 'A  ' AND 'B  ' AND 'A1 ' AND   EQW8LBVR
02980                                    'BSR'                          EQW8LBVR
02981               MOVE 'O' TO WSS-MODIF-PERMIS                        EQW8LBVR
02982               MOVE NOR-ALP TO ECR-PRMTYPC1A                       EQW8LBVR
02983               MOVE CURSEUR TO ECR-PRMTYPC1L                       EQW8LBVR
02984               GO TO FDETER-MODIF-PERMIS                           EQW8LBVR
02985            END-IF                                                 EQW8LBVR
02986         END-IF.                                                   EQW8LBVR
02987                                                                   EQW8LBVR
02988         IF PRMTYPC OF TS-PERSONNE(4, 2) = 'BSR'                   EQW8LBVR
02989            IF ECR-PRMTYPC2O NOT = 'A  ' AND 'B  ' AND 'A1 ' AND   EQW8LBVR
02990                                    'BSR'                          EQW8LBVR
02991               MOVE 'O' TO WSS-MODIF-PERMIS                        EQW8LBVR
02992               MOVE NOR-ALP TO ECR-PRMTYPC2A                       EQW8LBVR
02993               MOVE CURSEUR TO ECR-PRMTYPC2L                       EQW8LBVR
02994               GO TO FDETER-MODIF-PERMIS                           EQW8LBVR
02995            END-IF                                                 EQW8LBVR
02996         END-IF.                                                   EQW8LBVR
02997                                                                   EQW8LBVR
02998         IF PRMTYPC OF TS-PERSONNE(4, 1) = 'A1 '                   EQW8LBVR
02999            IF ECR-PRMTYPC1O NOT = 'A  ' AND 'B  ' AND 'A1 '       EQW8LBVR
03000               MOVE 'O' TO WSS-MODIF-PERMIS                        EQW8LBVR
03001               MOVE NOR-ALP TO ECR-PRMTYPC1A                       EQW8LBVR
03002               MOVE CURSEUR TO ECR-PRMTYPC1L                       EQW8LBVR
03003               GO TO FDETER-MODIF-PERMIS                           EQW8LBVR
03004            END-IF                                                 EQW8LBVR
03005         END-IF.                                                   EQW8LBVR
03006                                                                   EQW8LBVR
03007         IF PRMTYPC OF TS-PERSONNE(4, 2) = 'A1 '                   EQW8LBVR
03008            IF ECR-PRMTYPC2O NOT = 'A  ' AND 'B  ' AND 'A1 '       EQW8LBVR
03009               MOVE 'O' TO WSS-MODIF-PERMIS                        EQW8LBVR
03010               MOVE NOR-ALP TO ECR-PRMTYPC2A                       EQW8LBVR
03011               MOVE CURSEUR TO ECR-PRMTYPC2L                       EQW8LBVR
03012               GO TO FDETER-MODIF-PERMIS                           EQW8LBVR
03013            END-IF                                                 EQW8LBVR
03014         END-IF.                                                   EQW8LBVR
03015                                                                   EQW8LBVR
03016  FDETER-MODIF-PERMIS.  EXIT.                                      EQW8LBVR
03017                                                                   EQW8LBVR
03018  CONTROLE-AAC.                                                    EQW8LBVR
03019 *------------*                                                    EQW8LBVR
03020 * DATE DU JOUR                                                    EQW8LBVR
03021      MOVE WSS-DATJOUR-S      TO WSS-DATE1-SAMJ-SA(1:2).           EQW8LBVR
03022      MOVE WSS-DATJOUR-A      TO WSS-DATE1-SAMJ-SA(3:2).           EQW8LBVR
03023      MOVE WSS-DATJOUR-M      TO WSS-DATE1-SAMJ-M.                 EQW8LBVR
03024      MOVE WSS-DATJOUR-J      TO WSS-DATE1-SAMJ-J.                 EQW8LBVR
03025                                                                   EQW8LBVR
03026      COMPUTE  WSS-DATE1-SAMJ-SA = WSS-DATE1-SAMJ-SA - 1.          EQW8LBVR
03027                                                                   EQW8LBVR
03028      IF WSS-DATE-PERM-SAMJ NOT > WSS-DATE1-SAMJ                   EQW8LBVR
03029         MOVE 'N' TO WSS-CONTROLE-AAC-OK                           EQW8LBVR
03030      ELSE                                                         EQW8LBVR
03031         MOVE 'O' TO WSS-CONTROLE-AAC-OK                           EQW8LBVR
03032      END-IF.                                                      EQW8LBVR
03033                                                                   EQW8LBVR
03034  FCONTROLE-AAC.  EXIT.                                            EQW8LBVR
03035                                                                   EQW8LBVR
03036 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
03037 * TRAITEMENT DE LA TACHE * FB02 * TRAITEMENT NORMAL               EQW8LBVR
03038 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
03039  TRAITEMENT-TACHE.                                                EQW8LBVR
03040                                                                   EQW8LBVR
03041      IF ECRAN-MODIFIE                                             EQW8LBVR
03042         MOVE 'O' TO W-REAF                                        EQW8LBVR
03043         MOVE 'N' TO COM-MA-IND-EMISSION                           EQW8LBVR
03044         MOVE 'O' TO COM-MA-IND-MODIF                              EQW8LBVR
03045      END-IF.                                                      EQW8LBVR
03046                                                                   EQW8LBVR
03047      PERFORM TRAITEMENT-FICHIER THRU                              EQW8LBVR
03048              FIN-TRAITEMENT-FICHIER.                              EQW8LBVR
03049                                                                   EQW8LBVR
03050  FIN-TRAITEMENT-TACHE.  EXIT.                                     EQW8LBVR
03051 *                                                                 EQW8LBVR
03052 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
03053 *  GESTION DES FICHIERS     * FB02 * TRAITEMENT NORMAL            EQW8LBVR
03054 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
03055 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
03056 *                                                               * EQW8LBVR
03057 *                -------------------------------                * EQW8LBVR
03058 *                -    GESTION DES FICHIERS     -                * EQW8LBVR
03059 *                -------------------------------                * EQW8LBVR
03060 *                               -                               * EQW8LBVR
03061 *           -----------------------------------------           * EQW8LBVR
03062 *           -            -             -            -           * EQW8LBVR
03063 * ---------------- ------------ ---------------- -------------- * EQW8LBVR
03064 * - DETERMINATION- - CREATION - - MODIFICATION - - SUPRESSION - * EQW8LBVR
03065 * ---------------- ------------ ---------------- -------------- * EQW8LBVR
03066 *                                                               * EQW8LBVR
03067 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
03068  TRAITEMENT-FICHIER.                                              EQW8LBVR
03069                                                                   EQW8LBVR
03070      PERFORM MAJ-CODE-ACTION-DB2  THRU FMAJ-CODE-ACTION-DB2.      EQW8LBVR
03071                                                                   EQW8LBVR
03072      PERFORM MISE-A-JOUR-TS       THRU FIN-MISE-A-JOUR-TS.        EQW8LBVR
03073                                                                   EQW8LBVR
03074      PERFORM TRAITEMENT-COMMAREA THRU FIN-TRAITEMENT-COMMAREA.    EQW8LBVR
03075                                                                   EQW8LBVR
03076 * TS-PERSONNE DEJA ECRITE MAIS L'UTILISATEUR DECIDE D'EFFACER     EQW8LBVR
03077 * TOUTES LES ZONES DE SAISIES : REECRITURE DE LA TS A BLANC ET    EQW8LBVR
03078 * DESTRUCTION DE SON RANG                                         EQW8LBVR
03079      IF COM-FB-CODE-ACTION = 'A'                         AND      EQW8LBVR
03080         (DONNEES-PERSONNE OF TS-PERSONNE(1) = SPACES OR           EQW8LBVR
03081                                               LOW-VALUE) AND      EQW8LBVR
03082         COM-FB-RANG-TS-LIRE NOT = ZERO                   AND      EQW8LBVR
03083         COM-FB-RANG-TS-LIRE = COM-FB-RANG-MAX-TSPERS              EQW8LBVR
03084         MOVE SPACES TO DONNEES-PERSONNE OF TS-PERSONNE(1)         EQW8LBVR
03085                        DONNEES-PERSONNE OF TS-PERSONNE(2)         EQW8LBVR
03086                        DONNEES-PERSONNE OF TS-PERSONNE(3)         EQW8LBVR
03087                        DONNEES-PERSONNE OF TS-PERSONNE(4)         EQW8LBVR
03088         PERFORM REECRITURE-TS-PERSONNE THRU                       EQW8LBVR
03089                                        FIN-REECRITURE-TS-PERSONNE EQW8LBVR
03090         SUBTRACT 1 FROM COM-FB-RANG-MAX-TSPERS                    EQW8LBVR
03091         MOVE ZERO TO COM-FB-RANG-TS-LIRE                          EQW8LBVR
03092      END-IF.                                                      EQW8LBVR
03093                                                                   EQW8LBVR
03094 * EN AJOUT, ON ECRIT LA TS PERSONNE LA 1ERE FOIS QUE L'UTILISATEUREQW8LBVR
03095 * SAISIT LES ZONES ECRAN ET ON RENSEIGNE LES DATES D'ENTREE ET DE EQW8LBVR
03096 * SORTIE DE CETTE PERSONNE                                        EQW8LBVR
03097      IF (DONNEES-PERSONNE OF TS-PERSONNE(1) NOT = SPACES AND      EQW8LBVR
03098                                                  LOW-VALUE) AND   EQW8LBVR
03099         WSS-READ-TSPERS = 'N'                                     EQW8LBVR
03100         PERFORM ECRITURE-TS-SUSPENS-DL1 THRU                      EQW8LBVR
03101                                       FIN-ECRITURE-TS-SUSPENS-DL1 EQW8LBVR
03102         IF INF-EFFET OF TS-SUSPENS1 NOT = SPACES AND LOW-VALUE    EQW8LBVR
03103            MOVE INF-EFFET OF TS-SUSPENS1 TO                       EQW8LBVR
03104                                          PERENTD OF TS-PERSONNE(1)EQW8LBVR
03105            ELSE                                                   EQW8LBVR
03106               MOVE INF-EFFET OF TS-SUSPENS2 TO                    EQW8LBVR
03107                                          PERENTD OF TS-PERSONNE(1)EQW8LBVR
03108         END-IF                                                    EQW8LBVR
03109         MOVE '99999999' TO PERSORD OF TS-PERSONNE(1)              EQW8LBVR
03110                                                                   EQW8LBVR
03111         PERFORM ECRITURE-TS-PERSONNE THRU FIN-ECRITURE-TS-PERSONNEEQW8LBVR
03112      END-IF.                                                      EQW8LBVR
03113                                                                   EQW8LBVR
03114 * EN AJOUT OU EN MODIF, SI LA TS PERSONNE EXISTE DEJA, ON LA      EQW8LBVR
03115 * REECRIT                                                         EQW8LBVR
03116      IF WSS-READ-TSPERS = 'O'                                     EQW8LBVR
03117         PERFORM REECRITURE-TS-SUSPENS-DL1 THRU                    EQW8LBVR
03118                                      FIN-REECRITURE-TS-SUSPENS-DL1EQW8LBVR
03119         PERFORM REECRITURE-TS-PERSONNE THRU                       EQW8LBVR
03120                                      FIN-REECRITURE-TS-PERSONNE   EQW8LBVR
03121      END-IF.                                                      EQW8LBVR
03122                                                                   EQW8LBVR
03123      PERFORM APPEL-MA90T00 THRU FIN-APPEL-MA90T00.                EQW8LBVR
03124      IF  (COM-GENE-MESINF     = SPACES OR LOW-VALUE)              EQW8LBVR
03125      AND (COM90C00-MESINF NOT = SPACES AND LOW-VALUE)             EQW8LBVR
03126           MOVE COM90C00-MESINF TO COM-GENE-MESINF                 EQW8LBVR
03127                                   COM-CODERR                      EQW8LBVR
03128           MOVE 'O' TO W-REAF                                      EQW8LBVR
03129      END-IF.                                                      EQW8LBVR
03130                                                                   EQW8LBVR
03131      IF  (COM-GENE-MESANO     = SPACES OR LOW-VALUE)              EQW8LBVR
03132      AND (COM90C00-MESANO NOT = SPACES AND LOW-VALUE)             EQW8LBVR
03133           MOVE COM90C00-MESANO TO COM-GENE-MESANO                 EQW8LBVR
03134                                   COM-CODERR                      EQW8LBVR
03135           MOVE 'O' TO W-REAF                                      EQW8LBVR
03136      END-IF.                                                      EQW8LBVR
03137      IF INF-ETAT OF TS-SUSPENS1 NOT = '5' AND '7'                 EQW8LBVR
03138         IF (INF-ETAT OF TS-SUSPENS1 = '4' AND ECRAN-MODIFIE)      EQW8LBVR
03139         OR  INF-ETAT OF TS-SUSPENS1 NOT = '4'                     EQW8LBVR
03140             PERFORM APPEL-MA90T20 THRU                            EQW8LBVR
03141                     FIN-APPEL-MA90T20                             EQW8LBVR
03142         END-IF                                                    EQW8LBVR
03143      END-IF.                                                      EQW8LBVR
03144      IF  (MA90C20-NB-REJETS > 1)                                  EQW8LBVR
03145      AND (COM-GENE-MESINF = SPACES OR LOW-VALUE)                  EQW8LBVR
03146           MOVE 'FOH99'  TO COM-GENE-MESINF                        EQW8LBVR
03147                            COM-CODERR                             EQW8LBVR
03148           MOVE 'O' TO W-REAF                                      EQW8LBVR
03149      ELSE                                                         EQW8LBVR
03150          IF  (MA90C20-MESINF  NOT = SPACES AND LOW-VALUE)         EQW8LBVR
03151          AND (COM-GENE-MESINF = SPACES OR LOW-VALUE)              EQW8LBVR
03152               MOVE MA90C20-MESINF TO COM-GENE-MESINF              EQW8LBVR
03153                                      COM-CODERR                   EQW8LBVR
03154               MOVE 'O' TO W-REAF                                  EQW8LBVR
03155          END-IF                                                   EQW8LBVR
03156      END-IF.                                                      EQW8LBVR
03157      IF ECR-XCDECO = 'REJ' AND MA90C20-NB-REJETS = 0              EQW8LBVR
03158         MOVE 'FOH98' TO COM-GENE-MESINF                           EQW8LBVR
03159                         COM-CODERR                                EQW8LBVR
03160         MOVE 'O'     TO  W-REAF                                   EQW8LBVR
03161         MOVE  SPACES TO ECR-XCDECO                                EQW8LBVR
03162      END-IF.                                                      EQW8LBVR
03163                                                                   EQW8LBVR
03164  FIN-TRAITEMENT-FICHIER. EXIT.                                    EQW8LBVR
03165                                                                   EQW8LBVR
03166  MAJ-CODE-ACTION-DB2.                                             EQW8LBVR
03167 *------------------*                                              EQW8LBVR
03168                                                                   EQW8LBVR
03169      EVALUATE COM-FB-CODE-ACTION                                  EQW8LBVR
03170         WHEN 'M'                                                  EQW8LBVR
03171               IF PERACTC OF TS-PERSONNE(1) NOT = 'I'              EQW8LBVR
03172                  MOVE 'U' TO PERACTC OF TS-PERSONNE(1)            EQW8LBVR
03173               END-IF                                              EQW8LBVR
03174         WHEN 'A'                                                  EQW8LBVR
03175               MOVE 'I' TO PERACTC OF TS-PERSONNE(1)               EQW8LBVR
03176      END-EVALUATE.                                                EQW8LBVR
03177                                                                   EQW8LBVR
03178  FMAJ-CODE-ACTION-DB2.  EXIT.                                     EQW8LBVR
03179 *                                                                 EQW8LBVR
03180 ***************************************************************   EQW8LBVR
03181 *          STOCKAGE DES INFORMATIONS DANS LA TS               *   EQW8LBVR
03182 ***************************************************************   EQW8LBVR
03183  MISE-A-JOUR-TS.                                                  EQW8LBVR
03184                                                                   EQW8LBVR
03185 *---IDENTIFIANT DE LA PERSONNE                                    EQW8LBVR
03186      IF ECR-PERNUMXO NOT = SPACES                                 EQW8LBVR
03187         MOVE ECR-PERNUMXO TO PERNUMX OF TS-PERSONNE(1)            EQW8LBVR
03188      ELSE                                                         EQW8LBVR
03189         MOVE SPACES TO PERNUMX OF TS-PERSONNE(1)                  EQW8LBVR
03190      END-IF.                                                      EQW8LBVR
03191                                                                   EQW8LBVR
03192 *---STATUT PERSONNE                                               EQW8LBVR
03193      IF ECR-PERSTACO NOT = SPACES                                 EQW8LBVR
03194         MOVE ECR-PERSTACO TO PERSTAC OF TS-PERSONNE(1)            EQW8LBVR
03195      ELSE                                                         EQW8LBVR
03196         MOVE SPACES     TO PERSTAC OF TS-PERSONNE(1)              EQW8LBVR
03197      END-IF.                                                      EQW8LBVR
03198                                                                   EQW8LBVR
03199 *---NOMBRE DE SALARIES                                            EQW8LBVR
03200      IF ECR-PERSALNO NOT = SPACES                                 EQW8LBVR
03201         MOVE WSS-NBSALARIES TO PERSALN OF TS-PERSONNE(1)          EQW8LBVR
03202      ELSE                                                         EQW8LBVR
03203         MOVE SPACES     TO RPERSALN OF TS-PERSONNE(1)             EQW8LBVR
03204      END-IF.                                                      EQW8LBVR
03205                                                                   EQW8LBVR
03206 *---TITRE DE LA PERSONNE                                          EQW8LBVR
03207      IF ECR-PERTITLO NOT = SPACES                                 EQW8LBVR
03208         MOVE ECR-PERTITLO TO PERTITL OF TS-PERSONNE(1)            EQW8LBVR
03209      ELSE                                                         EQW8LBVR
03210         MOVE SPACES TO PERTITL OF TS-PERSONNE(1)                  EQW8LBVR
03211      END-IF.                                                      EQW8LBVR
03212                                                                   EQW8LBVR
03213 *---NOM DE LA PERSONNE                                            EQW8LBVR
03214      IF ECR-PERNOMLO NOT = SPACES                                 EQW8LBVR
03215         MOVE ECR-PERNOMLO TO PERNOML OF TS-PERSONNE(1)            EQW8LBVR
03216      ELSE                                                         EQW8LBVR
03217         MOVE SPACES TO PERNOML OF TS-PERSONNE(1)                  EQW8LBVR
03218      END-IF.                                                      EQW8LBVR
03219                                                                   EQW8LBVR
03220 *---PRENOM DE LA PERSONNE                                         EQW8LBVR
03221      IF ECR-PERPRELO NOT = SPACES                                 EQW8LBVR
03222         MOVE ECR-PERPRELO TO PERPREL OF TS-PERSONNE(1)            EQW8LBVR
03223      ELSE                                                         EQW8LBVR
03224         MOVE SPACES TO PERPREL OF TS-PERSONNE(1)                  EQW8LBVR
03225      END-IF.                                                      EQW8LBVR
03226                                                                   EQW8LBVR
03227 *---DATE DE NAISSANCE                                             EQW8LBVR
03228      IF ECR-PERNAIDO NOT = SPACES                                 EQW8LBVR
03229         MOVE ECR-PERNAIDO TO WSS-JMSA                             EQW8LBVR
03230         MOVE WSS-JMSA-J   TO WSS-SAMJ-J                           EQW8LBVR
03231         MOVE WSS-JMSA-M   TO WSS-SAMJ-M                           EQW8LBVR
03232         MOVE WSS-JMSA-SA  TO WSS-SAMJ-SA                          EQW8LBVR
03233         MOVE WSS-SAMJ TO PERNAID OF TS-PERSONNE(1)                EQW8LBVR
03234      ELSE                                                         EQW8LBVR
03235         MOVE SPACES TO RPERNAID OF TS-PERSONNE(1)                 EQW8LBVR
03236      END-IF.                                                      EQW8LBVR
03237                                                                   EQW8LBVR
03238 *---SEXE DE LA PERSONNE                                           EQW8LBVR
03239      IF ECR-PERSEXCO NOT = SPACES                                 EQW8LBVR
03240         MOVE ECR-PERSEXCO TO PERSEXC OF TS-PERSONNE(1)            EQW8LBVR
03241      ELSE                                                         EQW8LBVR
03242         MOVE SPACE TO PERSEXC OF TS-PERSONNE(1)                   EQW8LBVR
03243      END-IF.                                                      EQW8LBVR
03244                                                                   EQW8LBVR
03245 *---NOMBRE D'ENFANTS A CHARGE                                     EQW8LBVR
03246      IF ECR-PERENCNO NOT = SPACES                                 EQW8LBVR
03247          MOVE WSS-NBENFANTS TO PERENCN OF TS-PERSONNE(1)          EQW8LBVR
03248      ELSE                                                         EQW8LBVR
03249          MOVE SPACES TO RPERENCN OF TS-PERSONNE(1)                EQW8LBVR
03250      END-IF.                                                      EQW8LBVR
03251                                                                   EQW8LBVR
03252 *---SITUATION MATRIMONIALE                                        EQW8LBVR
03253      IF ECR-PERMATCO NOT = SPACES                                 EQW8LBVR
03254         MOVE ECR-PERMATCO TO PERMATC OF TS-PERSONNE(1)            EQW8LBVR
03255      ELSE                                                         EQW8LBVR
03256         MOVE SPACES TO PERMATC OF TS-PERSONNE(1)                  EQW8LBVR
03257      END-IF.                                                      EQW8LBVR
03258                                                                   EQW8LBVR
03259 *---PROFESSION                                                    EQW8LBVR
03260      IF ECR-PERPROCO NOT = SPACES                                 EQW8LBVR
03261         MOVE ECR-PERPROCO TO PERPROC OF TS-PERSONNE(1)            EQW8LBVR
03262      ELSE                                                         EQW8LBVR
03263         MOVE SPACES TO PERPROC OF TS-PERSONNE(1)                  EQW8LBVR
03264      END-IF.                                                      EQW8LBVR
03265                                                                   EQW8LBVR
03266 *---LIBELLÈ PROFESSION                                            EQW8LBVR
03267      IF ECR-PERPROLO NOT = SPACES                                 EQW8LBVR
03268         MOVE ECR-PERPROLO TO PERPROL OF TS-PERSONNE(1)            EQW8LBVR
03269      ELSE                                                         EQW8LBVR
03270         MOVE SPACES TO PERPROL OF TS-PERSONNE(1)                  EQW8LBVR
03271      END-IF.                                                      EQW8LBVR
03272                                                                   EQW8LBVR
03273 *---CATEGORIE DU PERMIS                                           EQW8LBVR
03274      IF ECR-PRMTYPC1O NOT = SPACES                                EQW8LBVR
03275         MOVE ECR-PRMTYPC1O TO PRMTYPC OF TS-PERSONNE(1, 1)        EQW8LBVR
03276      ELSE                                                         EQW8LBVR
03277         MOVE SPACES TO PRMTYPC OF TS-PERSONNE(1, 1)               EQW8LBVR
03278      END-IF.                                                      EQW8LBVR
03279                                                                   EQW8LBVR
03280      IF ECR-PRMTYPC2O NOT = SPACES                                EQW8LBVR
03281         MOVE ECR-PRMTYPC2O TO PRMTYPC OF TS-PERSONNE(1, 2)        EQW8LBVR
03282      ELSE                                                         EQW8LBVR
03283         MOVE SPACES TO PRMTYPC OF TS-PERSONNE(1, 2)               EQW8LBVR
03284      END-IF.                                                      EQW8LBVR
03285                                                                   EQW8LBVR
03286 *---DATES DE PERMIS                                               EQW8LBVR
03287      IF ECR-PRMOBTD1O NOT = SPACES                                EQW8LBVR
03288         MOVE ECR-PRMOBTD1O TO WSS-JMSA                            EQW8LBVR
03289         MOVE WSS-JMSA-J   TO WSS-SAMJ-J                           EQW8LBVR
03290         MOVE WSS-JMSA-M   TO WSS-SAMJ-M                           EQW8LBVR
03291         MOVE WSS-JMSA-SA  TO WSS-SAMJ-SA                          EQW8LBVR
03292         MOVE WSS-SAMJ TO RPRMOBTD OF TS-PERSONNE(1, 1)            EQW8LBVR
03293      ELSE                                                         EQW8LBVR
03294         MOVE SPACES TO RPRMOBTD OF TS-PERSONNE(1, 1)              EQW8LBVR
03295      END-IF.                                                      EQW8LBVR
03296                                                                   EQW8LBVR
03297      IF ECR-PRMOBTD2O NOT = SPACES                                EQW8LBVR
03298         MOVE ECR-PRMOBTD2O TO WSS-JMSA                            EQW8LBVR
03299         MOVE WSS-JMSA-J   TO WSS-SAMJ-J                           EQW8LBVR
03300         MOVE WSS-JMSA-M   TO WSS-SAMJ-M                           EQW8LBVR
03301         MOVE WSS-JMSA-SA  TO WSS-SAMJ-SA                          EQW8LBVR
03302         MOVE WSS-SAMJ TO RPRMOBTD OF TS-PERSONNE(1, 2)            EQW8LBVR
03303      ELSE                                                         EQW8LBVR
03304         MOVE SPACES TO RPRMOBTD OF TS-PERSONNE(1, 2)              EQW8LBVR
03305      END-IF.                                                      EQW8LBVR
03306                                                                   EQW8LBVR
03307 *---CONDUITE ACCOMPAGNEE                                          EQW8LBVR
03308      IF ECR-PERCOACO NOT = SPACES                                 EQW8LBVR
03309         MOVE ECR-PERCOACO TO PERCOAC OF TS-PERSONNE(1)            EQW8LBVR
03310      ELSE                                                         EQW8LBVR
03311         MOVE SPACES TO PERCOAC OF TS-PERSONNE(1)                  EQW8LBVR
03312      END-IF.                                                      EQW8LBVR
03313                                                                   EQW8LBVR
03314 *---NOMBRE DE MOIS DU RELEVE D'INFO                               EQW8LBVR
03315      IF ECR-ANPANCNO NOT = SPACES                                 EQW8LBVR
03316         MOVE WSS-NBMOIS-INFO TO ANPANCN OF TS-PERSONNE(1)         EQW8LBVR
03317      ELSE                                                         EQW8LBVR
03318         MOVE SPACES TO RANPANCN OF TS-PERSONNE(1)                 EQW8LBVR
03319      END-IF.                                                      EQW8LBVR
03320                                                                   EQW8LBVR
03321 *---INDICATEUR RETRAIT DE PERMIS                                  EQW8LBVR
03322      IF ECR-ANPINDCO NOT = SPACES                                 EQW8LBVR
03323         MOVE ECR-ANPINDCO TO ANPINDC OF TS-PERSONNE(1)            EQW8LBVR
03324      ELSE                                                         EQW8LBVR
03325         MOVE SPACES TO ANPINDC OF TS-PERSONNE(1)                  EQW8LBVR
03326      END-IF.                                                      EQW8LBVR
03327                                                                   EQW8LBVR
03328 *---NOMBRE DE JOURS DE RETRAIT DE PERMIS                          EQW8LBVR
03329      IF ECR-ANPNBJNO NOT = SPACES                                 EQW8LBVR
03330         MOVE WSS-NBJOURS-RETRAIT TO ANPNBJN OF TS-PERSONNE(1)     EQW8LBVR
03331      ELSE                                                         EQW8LBVR
03332         MOVE SPACES TO RANPNBJN OF TS-PERSONNE(1)                 EQW8LBVR
03333      END-IF.                                                      EQW8LBVR
03334                                                                   EQW8LBVR
03335 *---MOTIF DU RETRAIT DE PERMIS                                    EQW8LBVR
03336      IF ECR-ANPMOTLO NOT = SPACES                                 EQW8LBVR
03337         MOVE ECR-ANPMOTLO TO ANPMOTL OF TS-PERSONNE(1)            EQW8LBVR
03338      ELSE                                                         EQW8LBVR
03339         MOVE SPACES TO ANPMOTL OF TS-PERSONNE(1)                  EQW8LBVR
03340      END-IF.                                                      EQW8LBVR
03341                                                                   EQW8LBVR
03342                                                                   EQW8LBVR
03343  FIN-MISE-A-JOUR-TS. EXIT.                                        EQW8LBVR
03344 ***************************************************************   EQW8LBVR
03345 *          APPEL DU MODULE DE CONTROLE TECHNIQUE              *   EQW8LBVR
03346 ***************************************************************   EQW8LBVR
03347  APPEL-MA90T00.                                                   EQW8LBVR
03348                                                                   EQW8LBVR
03349      MOVE SPACES TO MAI90C00.                                     EQW8LBVR
03350      MOVE NOM-TACHE        TO COM90C00-TACHE.                     EQW8LBVR
03351      MOVE IDENT-TS-APP     TO COM90C00-IDENT-TS.                  EQW8LBVR
03352      MOVE +1               TO COM90C00-ITEM-TS.                   EQW8LBVR
03353      MOVE SPACES           TO COM90C00-ACTION.                    EQW8LBVR
03354      MOVE COM-GENE-CODCIE  TO COM90C00-CIE.                       EQW8LBVR
03355      MOVE COM-GENE-TYPCICS TO COM90C00-TYPCICS.                   EQW8LBVR
03356      MOVE COM-MA           TO COM90C00-MAICOMM.                   EQW8LBVR
03357      EXEC CICS LINK PROGRAM  ('MA90T00')                          EQW8LBVR
03358                     COMMAREA (MAI90C00)                           EQW8LBVR
03359                     LENGTH   (LENGTH OF MAI90C00)                 EQW8LBVR
03360      END-EXEC.                                                    EQW8LBVR
03361                                                                   EQW8LBVR
03362      IF EIBRCODE = LOW-VALUE                                      EQW8LBVR
03363         MOVE COM90C00-MAICOMM TO COM-MA                           EQW8LBVR
03364      ELSE                                                         EQW8LBVR
03365         MOVE 'FO02 : ERREUR LINK MA90T00' TO MESS                 EQW8LBVR
03366         GO TO ABANDON-TACHE                                       EQW8LBVR
03367      END-IF.                                                      EQW8LBVR
03368 *                                                                 EQW8LBVR
03369  FIN-APPEL-MA90T00.   EXIT.                                       EQW8LBVR
03370                                                                   EQW8LBVR
03371 ***************************************************************   EQW8LBVR
03372 *          APPEL DU MODULE DE CONTROLE D'HABILITATION         *   EQW8LBVR
03373 ***************************************************************   EQW8LBVR
03374  APPEL-MA90T20.                                                   EQW8LBVR
03375                                                                   EQW8LBVR
03376      MOVE SPACES               TO  MAI90C20.                      EQW8LBVR
03377      MOVE IDENT-TS-APP         TO  MA90C20-IDENT-TS.              EQW8LBVR
03378      MOVE IDENT-TS-CONF        TO  MA90C20-IDENT-TS-CONF.         EQW8LBVR
03379      MOVE COM-GENE-LNGCNV      TO  MA90C20-LG-TS-CONF.            EQW8LBVR
03380      MOVE NOM-TACHE            TO  MA90C20-TRANSAC.               EQW8LBVR
03381      MOVE SPACES               TO  MA90C20-ACTION.                EQW8LBVR
03382      MOVE 'N'                  TO  MA90C20-MAJ-TS-APP.            EQW8LBVR
03383      MOVE Z-COMMAREA-USER      TO  MA90C20-MAICOMM.               EQW8LBVR
03384      MOVE COM-GENE-TYPCICS     TO  MA90C20-TYPCICS.               EQW8LBVR
03385      MOVE COM-GENE-CODCIE      TO  MA90C20-CODCIE.                EQW8LBVR
03386      MOVE '1'                  TO  MA90C20-SWAP.                  EQW8LBVR
03387      MOVE 'C'                  TO  MA90C20-TYPETS.                EQW8LBVR
03388      MOVE 'O'                  TO  MA90C20-RAB.                   EQW8LBVR
03389      EXEC CICS LINK PROGRAM  ('MA90T20')                          EQW8LBVR
03390                     COMMAREA (MAI90C20)                           EQW8LBVR
03391                     LENGTH   (LONG-MA90C20)                       EQW8LBVR
03392      END-EXEC.                                                    EQW8LBVR
03393      IF EIBRCODE NOT = LOW-VALUE                                  EQW8LBVR
03394         MOVE 'SK72.ERREUR LINK-MA90T20' TO MESS                   EQW8LBVR
03395         GO TO ABANDON-TACHE                                       EQW8LBVR
03396      END-IF.                                                      EQW8LBVR
03397      MOVE MA90C20-MAICOMM      TO  Z-COMMAREA-USER.               EQW8LBVR
03398                                                                   EQW8LBVR
03399  FIN-APPEL-MA90T20. EXIT.                                         EQW8LBVR
03400                                                                   EQW8LBVR
03401 ***************************************************************   EQW8LBVR
03402 *            ECRITURE TS SUSPENS                              *   EQW8LBVR
03403 ***************************************************************   EQW8LBVR
03404  ECRITURE-TS-SUSPENS-DL1.                                         EQW8LBVR
03405                                                                   EQW8LBVR
03406 * ITEM 1                                                          EQW8LBVR
03407      MOVE +1 TO RANG-TS.                                          EQW8LBVR
03408      MOVE FBMISPTR-IT1 TO SEGTRA OF TS-SUSPENS1.                  EQW8LBVR
03409      EXEC CICS WRITEQ TS QUEUE (IDENT-TS-APP)                     EQW8LBVR
03410                          FROM  (TS-SUSPENS1)                      EQW8LBVR
03411                          LENGTH (LONG-TS-SUSPENS)                 EQW8LBVR
03412                          ITEM  (RANG-TS)                          EQW8LBVR
03413                          MAIN                                     EQW8LBVR
03414                          NOHANDLE                                 EQW8LBVR
03415      END-EXEC.                                                    EQW8LBVR
03416      IF EIBRCODE NOT = LOW-VALUE                                  EQW8LBVR
03417         MOVE 'FBE1 ERR.WRITE TS-SUSPENS1' TO MESS                 EQW8LBVR
03418         GO TO ABANDON-TACHE                                       EQW8LBVR
03419      END-IF.                                                      EQW8LBVR
03420                                                                   EQW8LBVR
03421 * ITEM 2                                                          EQW8LBVR
03422      MOVE +2 TO RANG-TS.                                          EQW8LBVR
03423      MOVE FBMISPTR-IT2 TO SEGTRA OF TS-SUSPENS2.                  EQW8LBVR
03424      EXEC CICS WRITEQ TS QUEUE (IDENT-TS-APP)                     EQW8LBVR
03425                          FROM  (TS-SUSPENS2)                      EQW8LBVR
03426                          LENGTH (LONG-TS-SUSPENS)                 EQW8LBVR
03427                          ITEM  (RANG-TS)                          EQW8LBVR
03428                          MAIN                                     EQW8LBVR
03429                          NOHANDLE                                 EQW8LBVR
03430      END-EXEC.                                                    EQW8LBVR
03431      IF EIBRCODE NOT = LOW-VALUE                                  EQW8LBVR
03432         MOVE 'FBE2 ERR.WRITE TS-SUSPENS2' TO MESS                 EQW8LBVR
03433         GO TO ABANDON-TACHE                                       EQW8LBVR
03434      END-IF.                                                      EQW8LBVR
03435                                                                   EQW8LBVR
03436  FIN-ECRITURE-TS-SUSPENS-DL1.  EXIT.                              EQW8LBVR
03437                                                                   EQW8LBVR
03438 ***************************************************************   EQW8LBVR
03439 *            ECRITURE TS PERSONNE                             *   EQW8LBVR
03440 ***************************************************************   EQW8LBVR
03441  ECRITURE-TS-PERSONNE.                                            EQW8LBVR
03442                                                                   EQW8LBVR
03443      ADD 1 TO COM-FB-RANG-MAX-TSPERS.                             EQW8LBVR
03444      ADD 1 TO COM-FB-NBRE-PERS-ENC.                               EQW8LBVR
03445      MOVE COM-FB-RANG-MAX-TSPERS TO COM-FB-RANG-TS-LIRE.          EQW8LBVR
03446      ADD 1 TO COM-FB-ORDN-MAX-PERS.                               EQW8LBVR
03447      MOVE COM-FB-ORDN-MAX-PERS TO PERORDX OF TS-PERSONNE(2).      EQW8LBVR
03448      EXEC CICS WRITEQ TS QUEUE (COM-FB-IDENT-TSPERS)              EQW8LBVR
03449                          FROM  (TS-PERSONNE)                      EQW8LBVR
03450                          LENGTH (LENGTH OF TS-PERSONNE)           EQW8LBVR
03451                          ITEM  (COM-FB-RANG-TS-LIRE)              EQW8LBVR
03452                          NOHANDLE                                 EQW8LBVR
03453      END-EXEC.                                                    EQW8LBVR
03454      IF EIBRCODE NOT = LOW-VALUE                                  EQW8LBVR
03455         MOVE 'PRR1 ERR.WRITE TS-PERSONNE' TO MESS                 EQW8LBVR
03456         GO TO ABANDON-TACHE                                       EQW8LBVR
03457      END-IF.                                                      EQW8LBVR
03458                                                                   EQW8LBVR
03459  FIN-ECRITURE-TS-PERSONNE. EXIT.                                  EQW8LBVR
03460                                                                   EQW8LBVR
03461 ***************************************************************   EQW8LBVR
03462 *          REECRITURE TS SUSPENS                              *   EQW8LBVR
03463 ***************************************************************   EQW8LBVR
03464  REECRITURE-TS-SUSPENS-DL1.                                       EQW8LBVR
03465                                                                   EQW8LBVR
03466 * ITEM 1                                                          EQW8LBVR
03467      MOVE +1 TO RANG-TS.                                          EQW8LBVR
03468      MOVE FBMISPTR-IT1 TO SEGTRA OF TS-SUSPENS1.                  EQW8LBVR
03469      EXEC CICS WRITEQ TS QUEUE (IDENT-TS-APP)                     EQW8LBVR
03470                          FROM  (TS-SUSPENS1)                      EQW8LBVR
03471                          LENGTH (LONG-TS-SUSPENS)                 EQW8LBVR
03472                          ITEM  (RANG-TS)                          EQW8LBVR
03473                          REWRITE                                  EQW8LBVR
03474                          MAIN                                     EQW8LBVR
03475                          NOHANDLE                                 EQW8LBVR
03476      END-EXEC.                                                    EQW8LBVR
03477      IF EIBRCODE NOT = LOW-VALUE                                  EQW8LBVR
03478         MOVE 'FBE1 ERR.REWRITE TS-SUSPENS1' TO MESS               EQW8LBVR
03479         GO TO ABANDON-TACHE                                       EQW8LBVR
03480      END-IF.                                                      EQW8LBVR
03481                                                                   EQW8LBVR
03482 * ITEM 2                                                          EQW8LBVR
03483      MOVE +2 TO RANG-TS.                                          EQW8LBVR
03484      MOVE FBMISPTR-IT2 TO SEGTRA OF TS-SUSPENS2.                  EQW8LBVR
03485      EXEC CICS WRITEQ TS QUEUE (IDENT-TS-APP)                     EQW8LBVR
03486                          FROM  (TS-SUSPENS2)                      EQW8LBVR
03487                          LENGTH (LONG-TS-SUSPENS)                 EQW8LBVR
03488                          ITEM  (RANG-TS)                          EQW8LBVR
03489                          REWRITE                                  EQW8LBVR
03490                          MAIN                                     EQW8LBVR
03491                          NOHANDLE                                 EQW8LBVR
03492      END-EXEC.                                                    EQW8LBVR
03493      IF EIBRCODE NOT = LOW-VALUE                                  EQW8LBVR
03494         MOVE 'FBE2 ERR.REWRITE TS-SUSPENS2' TO MESS               EQW8LBVR
03495         GO TO ABANDON-TACHE                                       EQW8LBVR
03496      END-IF.                                                      EQW8LBVR
03497                                                                   EQW8LBVR
03498  FIN-REECRITURE-TS-SUSPENS-DL1.  EXIT.                            EQW8LBVR
03499                                                                   EQW8LBVR
03500 ***************************************************************   EQW8LBVR
03501 *          REECRITURE TS PERSONNE                             *   EQW8LBVR
03502 ***************************************************************   EQW8LBVR
03503  REECRITURE-TS-PERSONNE.                                          EQW8LBVR
03504                                                                   EQW8LBVR
03505      EXEC CICS WRITEQ TS QUEUE (COM-FB-IDENT-TSPERS)              EQW8LBVR
03506                          FROM  (TS-PERSONNE)                      EQW8LBVR
03507                          LENGTH (LENGTH OF TS-PERSONNE)           EQW8LBVR
03508                          ITEM  (COM-FB-RANG-TS-LIRE)              EQW8LBVR
03509                          REWRITE                                  EQW8LBVR
03510                          NOHANDLE                                 EQW8LBVR
03511      END-EXEC.                                                    EQW8LBVR
03512      IF EIBRCODE NOT = LOW-VALUE                                  EQW8LBVR
03513         MOVE 'PRR2 ERR.REWRITE TS-PERSONNE' TO MESS               EQW8LBVR
03514         GO TO ABANDON-TACHE                                       EQW8LBVR
03515      END-IF.                                                      EQW8LBVR
03516                                                                   EQW8LBVR
03517  FIN-REECRITURE-TS-PERSONNE. EXIT.                                EQW8LBVR
03518 ***************************************************************** EQW8LBVR
03519 * GESTION DE LA COMMAREA    * FB02 * TRAITEMENT NORMAL          * EQW8LBVR
03520 ***************************************************************** EQW8LBVR
03521 *                                                                 EQW8LBVR
03522  TRAITEMENT-COMMAREA.                                             EQW8LBVR
03523 *                                                                 EQW8LBVR
03524 *                                                                 EQW8LBVR
03525  FIN-TRAITEMENT-COMMAREA.  EXIT.                                  EQW8LBVR
03526 /                                                                 EQW8LBVR
03527 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
03528 * DETERMINATION ECRAN SUIVANT * FB02 * TRAITEMENT NORMAL          EQW8LBVR
03529 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
03530  DETERMINATION-ECR-SUIV.                                          EQW8LBVR
03531                                                                   EQW8LBVR
03532 *  RECHERCHE DE L'ECRAN DANS LA TABLE DES CONVERSATIONS           EQW8LBVR
03532 *                                                                 EQW8LBVR
U3319  ++INCLUDE SQKCECRS
03533                                                                   EQW8LBVR
03557 *  ----------------------------------------------------------- *  EQW8LBVR
03558 *  GESTION DES PASSAGES AUX ECRANS OPTIONNELS SUIVANTS :          EQW8LBVR
03559 *  ----------------------------------------------------------- *  EQW8LBVR
03560 *                                                                 EQW8LBVR
03561 * DEBRANCHEMENT VERS ECRAN AIDE STATUT ET PROFESSION              EQW8LBVR
03562 * (TEXTE SOUS SPI)                                                EQW8LBVR
03563      IF WSS-APPEL-AIDE-STATUT = 'O' OR                            EQW8LBVR
03564         WSS-APPEL-AIDE-PROFESSION = 'O'                           EQW8LBVR
03565         MOVE 'MA84'  TO  NOM-TACHE-XCTL                           EQW8LBVR
03566         MOVE  SPACES                 TO  COM-GENE-MESINF          EQW8LBVR
03567                                          COM-GENE-MESANO          EQW8LBVR
03568                                          COM-CODERR               EQW8LBVR
03569         MOVE CODE-TRAITEMENT-NORMAL  TO  Z-FONCTION               EQW8LBVR
03570         MOVE 'O'                     TO COM-GENE-ECROPT           EQW8LBVR
03571         SUBTRACT  1                  FROM   IA                    EQW8LBVR
03572         MOVE IA                      TO   COM-MA-STD-CLICHE       EQW8LBVR
03573         MOVE 'N'                     TO   COM-GENE-REAF           EQW8LBVR
03574                                                                   EQW8LBVR
03575         IF WSS-APPEL-AIDE-STATUT = 'O'                            EQW8LBVR
03576            MOVE 'STAT'               TO   COM-MA-GENRE-TXT        EQW8LBVR
03577         END-IF                                                    EQW8LBVR
03578         IF WSS-APPEL-AIDE-PROFESSION = 'O'                        EQW8LBVR
03579            MOVE 'PROF'               TO   COM-MA-GENRE-TXT        EQW8LBVR
03580         END-IF                                                    EQW8LBVR
03581         GO TO FIN-DETERMINATION-ECR-SUIV                          EQW8LBVR
03582      END-IF.                                                      EQW8LBVR
03583                                                                   EQW8LBVR
03584      MOVE SPACES TO COM-GENE-PILCNV(COM-GENE-INDCNV).             EQW8LBVR
03585 *                                                                 EQW8LBVR
03586      MOVE 'FB01'  TO  NOM-TACHE-XCTL.                             EQW8LBVR
03587      MOVE CODE-TRAITEMENT-NORMAL  TO  Z-FONCTION.                 EQW8LBVR
03588      GO TO FIN-DETERMINATION-ECR-SUIV.                            EQW8LBVR
03589 *                                                                 EQW8LBVR
03590  FIN-DETERMINATION-ECR-SUIV.                                      EQW8LBVR
03591      EXIT.                                                        EQW8LBVR
03592 /                                                                 EQW8LBVR
03593 *                                                                 EQW8LBVR
03594 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
03595 * GESTION DE LA MAP         * FB02 * TRAITEMENT NORMAL            EQW8LBVR
03596 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
03597 *                                                                 EQW8LBVR
03598  TRAITEMENT-MAP.                                                  EQW8LBVR
03599 * ------------ *                                                  EQW8LBVR
03600 *                                                                 EQW8LBVR
03601  FIN-TRAITEMENT-MAP. EXIT.                                        EQW8LBVR
03602 /                                                                 EQW8LBVR
03603 ***************************************************************** EQW8LBVR
03604 ***************************************************************** EQW8LBVR
03605 ***********************  MODULE SORTIE  ************************* EQW8LBVR
03606 ***************************************************************** EQW8LBVR
03607 ***************************************************************** EQW8LBVR
03608 *                                                                 EQW8LBVR
03609 ***************************************************************** EQW8LBVR
03610 * MODULE DE SORTIE GENERALISE                                   * EQW8LBVR
03611 ***************************************************************** EQW8LBVR
03612 *                                                                 EQW8LBVR
03613  MODULE-SORTIE.                                                   EQW8LBVR
03614 *-------------*                                                   EQW8LBVR
03615      IF  TRAITEMENT-AUTOMATIQUE                                   EQW8LBVR
03616          MOVE     SPACES TO   COM-GENE-NEWMEN                     EQW8LBVR
03617          PERFORM  SORTIE-AFFICHAGE-FORMAT THRU                    EQW8LBVR
03618                   FIN-SORTIE-AFFICHAGE-FORMAT                     EQW8LBVR
03619      END-IF.                                                      EQW8LBVR
03620      IF  NOT OK                                                   EQW8LBVR
03621          PERFORM  SORTIE-ERREUR THRU                              EQW8LBVR
03622                   FIN-SORTIE-ERREUR                               EQW8LBVR
03623      END-IF.                                                      EQW8LBVR
03624 *                                                                 EQW8LBVR
03625      IF  TRAITEMENT-NORMAL                                        EQW8LBVR
03626          PERFORM  SORTIE-SUITE THRU                               EQW8LBVR
03627                   FIN-SORTIE-SUITE                                EQW8LBVR
03628      END-IF.                                                      EQW8LBVR
03629 *                                                                 EQW8LBVR
03630      IF  LEVEL-SUP                                                EQW8LBVR
03631          PERFORM  SORTIE-LEVEL-SUPERIEUR THRU                     EQW8LBVR
03632                   FIN-SORTIE-LEVEL-SUPERIEUR                      EQW8LBVR
03633      END-IF.                                                      EQW8LBVR
03634 *                                                                 EQW8LBVR
03635      IF  LEVEL-SIGN                                               EQW8LBVR
03636          PERFORM  SORTIE-LEVEL-SIGNATURE THRU                     EQW8LBVR
03637                   FIN-SORTIE-LEVEL-SIGNATURE                      EQW8LBVR
03638      END-IF.                                                      EQW8LBVR
03639 *                                                                 EQW8LBVR
03640      IF  LEVEL-MAX OR JUMP                                        EQW8LBVR
03641          PERFORM  SORTIE-LEVEL-MAX THRU                           EQW8LBVR
03642                   FIN-SORTIE-LEVEL-MAX                            EQW8LBVR
03643      END-IF.                                                      EQW8LBVR
03644 *                                                                 EQW8LBVR
03645      IF  LEVEL-PREC                                               EQW8LBVR
03646          PERFORM  SORTIE-LEVEL-PREC THRU                          EQW8LBVR
03647                   FIN-SORTIE-LEVEL-PREC                           EQW8LBVR
03648      END-IF.                                                      EQW8LBVR
03649 *                                                                 EQW8LBVR
03650      IF  ERREUR-MANIPULATION                                      EQW8LBVR
03651          PERFORM  SORTIE-ERREUR-MANIP THRU                        EQW8LBVR
03652                   FIN-SORTIE-ERREUR-MANIP                         EQW8LBVR
03653      END-IF.                                                      EQW8LBVR
03654 *                                                                 EQW8LBVR
03655 * ABANDON * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW8LBVR
03656 *                                                                 EQW8LBVR
03657      MOVE 'SK57.ERREUR CODE FONCTION DANS MODULE-SORTIE' TO MESS  EQW8LBVR
03658      GO TO ABANDON-TACHE.                                         EQW8LBVR
03659 *                                                                 EQW8LBVR
03660  FIN-MODULE-SORTIE. EXIT.                                         EQW8LBVR
03661 *                                                                 EQW8LBVR
03662 ***************************************************************** EQW8LBVR
03663 * AFFICHAGE DE LA MAP NORMALE PUIS  RETURN TRANSID AU MEME PGM  * EQW8LBVR
03664 ***************************************************************** EQW8LBVR
03665  SORTIE-AFFICHAGE-FORMAT.                                         EQW8LBVR
03666 *-----------------------*                                         EQW8LBVR
03667      IF COM-GENE-REAF = 'O'                                       EQW8LBVR
03668         PERFORM RESTAURATION-TS-ECRAN  THRU                       EQW8LBVR
03669                 FIN-RESTAURATION-TS-ECRAN                         EQW8LBVR
03670      ELSE                                                         EQW8LBVR
03671         PERFORM REMPLISSAGE-TS-ECRAN THRU                         EQW8LBVR
03672                 FIN-REMPLISSAGE-TS-ECRAN                          EQW8LBVR
03673      END-IF.                                                      EQW8LBVR
03674 *                                                                 EQW8LBVR
03675      IF COM-GENE-MESANO  NOT = SPACES AND                         EQW8LBVR
03676                                LOW-VALUE                          EQW8LBVR
03677         PERFORM LECTURE-ERREUR THRU                               EQW8LBVR
03678                 FIN-LECTURE-ERREUR                                EQW8LBVR
03679         MOVE    SPACES   TO COM-GENE-MESANO                       EQW8LBVR
03680         MOVE    W-ERREUR TO ECR-XMSGALO                           EQW8LBVR
03681      END-IF.                                                      EQW8LBVR
03682 *                                                                 EQW8LBVR
03683      PERFORM SEND-MAP THRU                                        EQW8LBVR
03684              FIN-SEND-MAP.                                        EQW8LBVR
03685 *                                                                 EQW8LBVR
03686      MOVE    SPACES    TO Z-COMMAREA-TACHE-JUMP.                  EQW8LBVR
03687      MOVE    NOM-TACHE TO NOM-TACHE-RETOUR.                       EQW8LBVR
03688 *                                                                 EQW8LBVR
03689      PERFORM RETOUR-COMMAREA THRU                                 EQW8LBVR
03690              FIN-RETOUR-COMMAREA.                                 EQW8LBVR
03691  FIN-SORTIE-AFFICHAGE-FORMAT.  EXIT.                              EQW8LBVR
03692 *                                                                 EQW8LBVR
03693 ***************************************************************** EQW8LBVR
03694 *  AFFICHAGE DE LA MAP EN ERREUR ET RETURN AU MEME PROGRAMME    * EQW8LBVR
03695 ***************************************************************** EQW8LBVR
03696  SORTIE-ERREUR.                                                   EQW8LBVR
03697 *-------------*                                                   EQW8LBVR
03698      IF COM-GENE-MESANO  NOT = SPACES AND                         EQW8LBVR
03699                                LOW-VALUE                          EQW8LBVR
03700         PERFORM LECTURE-ERREUR THRU                               EQW8LBVR
03701                 FIN-LECTURE-ERREUR                                EQW8LBVR
03702         MOVE    SPACES   TO COM-GENE-MESANO                       EQW8LBVR
03703         MOVE    W-ERREUR TO ECR-XMSGALO                           EQW8LBVR
03704      END-IF.                                                      EQW8LBVR
03705 *                                                                 EQW8LBVR
03706      IF ERREUR-DISPATCH                                           EQW8LBVR
03707         MOVE    SPACES        TO  Z-COMMAREA-SELECT               EQW8LBVR
03708 *       MOVE    SPACES        TO  COM-GENE-NEWMEN                 EQW8LBVR
03709         MOVE    ZONE-TS-ECRAN TO  Z-MAP                           EQW8LBVR
03710         PERFORM RESTAURATION-TS-ECRAN THRU                        EQW8LBVR
03711                 FIN-RESTAURATION-TS-ECRAN                         EQW8LBVR
03712         MOVE    CURSEUR       TO  ECR-XCDECL                      EQW8LBVR
03713         MOVE    W-ERREUR      TO  ECR-XMSGALO                     EQW8LBVR
03714         PERFORM SEND-MAP-ERREUR   THRU                            EQW8LBVR
03715                 FIN-SEND-MAP-ERREUR                               EQW8LBVR
03716      END-IF.                                                      EQW8LBVR
03717 *                                                                 EQW8LBVR
03718      IF ERREUR                                                    EQW8LBVR
03719         PERFORM RESTAURATION-MAP THRU                             EQW8LBVR
03720                 FIN-RESTAURATION-MAP                              EQW8LBVR
03721         PERFORM SEND-MAP-ERREUR-DATAONLY THRU                     EQW8LBVR
03722                 FIN-SEND-MAP-ERREUR-DATAONLY                      EQW8LBVR
03723      END-IF.                                                      EQW8LBVR
03724 *                                                                 EQW8LBVR
03725      IF ERREUR-SORTIE                                             EQW8LBVR
03726         PERFORM SEND-MAP-NO-ERASE-DATAONLY THRU                   EQW8LBVR
03727                 FIN-SEND-MAP-NO-ERASE-DATAONLY                    EQW8LBVR
03728      END-IF.                                                      EQW8LBVR
03729      MOVE SPACES    TO COM-GENE-REAF.                             EQW8LBVR
03730      MOVE NOM-TACHE TO NOM-TACHE-RETOUR.                          EQW8LBVR
03731      PERFORM RETOUR-COMMAREA THRU                                 EQW8LBVR
03732              FIN-RETOUR-COMMAREA.                                 EQW8LBVR
03733  FIN-SORTIE-ERREUR.  EXIT.                                        EQW8LBVR
03734 /                                                                 EQW8LBVR
03735 ***************************************************************** EQW8LBVR
03736 *  XCTL OU START POUR APPELER LE PROGRAMME SUIVANT              * EQW8LBVR
03737 ***************************************************************** EQW8LBVR
03738  SORTIE-SUITE.                                                    EQW8LBVR
03739 *------------*                                                    EQW8LBVR
03740 *                                                                 EQW8LBVR
03741 * ON NE PEUT PAS PASSER LA MAIN A L'ELEMENT DE CONVERSATION       EQW8LBVR
03742 * SUIVANT SI ON A ATTEINT LES 50 TRANSACTIONS POUR UNE            EQW8LBVR
03743 * CONVERSATION                                                    EQW8LBVR
03744 *                                                                 EQW8LBVR
03745      IF COM-GENE-INDCNV = 50                                      EQW8LBVR
03746         MOVE    'SQ004' TO COM-GENE-MESANO                        EQW8LBVR
03747                            COM-CODERR                             EQW8LBVR
03748         MOVE    2       TO KONTROL                                EQW8LBVR
03749         MOVE    ZONE-TS-ECRAN TO Z-MAP                            EQW8LBVR
03750         PERFORM RESTAURATION-TS-ECRAN THRU                        EQW8LBVR
03751                 FIN-RESTAURATION-TS-ECRAN                         EQW8LBVR
03752         PERFORM SORTIE-ERREUR THRU                                EQW8LBVR
03753                 FIN-SORTIE-ERREUR                                 EQW8LBVR
03754      END-IF.                                                      EQW8LBVR
03755      PERFORM DELETE-TS-ECRAN THRU                                 EQW8LBVR
03756              FIN-DELETE-TS-ECRAN.                                 EQW8LBVR
03757 *                     DOUBLE AFFICHAGE                            EQW8LBVR
03758      MOVE    SPACES   TO COM-GENE-REAF.                           EQW8LBVR
03759      MOVE    NOM-PROG TO COM-PGMPRC.                              EQW8LBVR
03760      PERFORM XCTL-PROG-COMMAREA THRU                              EQW8LBVR
03761              FIN-XCTL-PROG-COMMAREA.                              EQW8LBVR
03762 *                                                                 EQW8LBVR
03763  FIN-SORTIE-SUITE.   EXIT.                                        EQW8LBVR
03764 *                                                                 EQW8LBVR
03765 ***************************************************************** EQW8LBVR
03766 *    RETOUR APRES     PF3    AU MENU SUPERIEUR                    EQW8LBVR
03767 ***************************************************************** EQW8LBVR
03768 *                                                                 EQW8LBVR
03769  SORTIE-LEVEL-SUPERIEUR.                                          EQW8LBVR
03770 *----------------------*                                          EQW8LBVR
03771      PERFORM          DELETE-TS-PLAN THRU                         EQW8LBVR
03772                       FIN-DELETE-TS-PLAN.                         EQW8LBVR
03773 *  REMISE A BLANC DE LA PILE DES CONVERSATIONS                    EQW8LBVR
03774 *  ET DES ENCHAINEMENTS                                           EQW8LBVR
03775      MOVE SPACES TO COM-GENE-CNVPIL.                              EQW8LBVR
03776      MOVE SPACES TO COM-GENE-ENCCNV.                              EQW8LBVR
03777      MOVE ZERO   TO COM-GENE-INDCNV.                              EQW8LBVR
03778 *                   DOUBLE AFFICHAGE                              EQW8LBVR
03779      MOVE SPACES TO COM-GENE-REAF.                                EQW8LBVR
03780 *  RECUPERATION DU DERNIER MENU                                   EQW8LBVR
03781      MOVE COM-GENE-PILMEN(COM-GENE-INDMEN) TO NOM-TACHE-START,    EQW8LBVR
03782                                               COM-GENE-NEWMEN.    EQW8LBVR
03783      IF   COM-GENE-TYPMEN(COM-GENE-INDMEN) = 'O'                  EQW8LBVR
03784      MOVE COM-GENE-EXPTRNID                TO NOM-TACHE-START     EQW8LBVR
03785      END-IF.                                                      EQW8LBVR
03786      MOVE LONG-COMMAREA  TO  LONG-START.                          EQW8LBVR
03787      MOVE EIBTRMID       TO  TERM-START.                          EQW8LBVR
03788      MOVE NOM-PROG       TO  COM-PGMPRC.                          EQW8LBVR
03789 *  DELETE DE LA TS ECRAN                                          EQW8LBVR
03790      PERFORM          DELETE-TS-ECRAN THRU                        EQW8LBVR
03791                       FIN-DELETE-TS-ECRAN.                        EQW8LBVR
03792 *    DELETE DES TS APPLICATIVES ET CONFIDENTIALITE CONVERSATION   EQW8LBVR
03793      PERFORM          DELETE-TS-CONF-CONV THRU                    EQW8LBVR
03794                       FIN-DELETE-TS-CONF-CONV.                    EQW8LBVR
03795      MOVE SPACES                 TO COM-GENE-SWPCNV.              EQW8LBVR
03796      PERFORM START-TACHE THRU                                     EQW8LBVR
03797              FIN-START-TACHE.                                     EQW8LBVR
03798      PERFORM RETOUR      THRU                                     EQW8LBVR
03799              FIN-RETOUR.                                          EQW8LBVR
03800  FIN-SORTIE-LEVEL-SUPERIEUR.   EXIT.                              EQW8LBVR
03801 *                                                                 EQW8LBVR
03802 ***************************************************************** EQW8LBVR
03803 *    RETOUR APRES PF12 AU NIVEAU SUPERIEUR DANS UNE CONVERSATION  EQW8LBVR
03804 ***************************************************************** EQW8LBVR
03805  SORTIE-LEVEL-PREC.                                               EQW8LBVR
03806 *-----------------*                                               EQW8LBVR
03807 *       ON NE PEUT PAS SORTIR DU PREMIER NIVEAU PAR PF12          EQW8LBVR
03808      IF COM-GENE-PILCNV(1) = NOM-TACHE                            EQW8LBVR
03809         MOVE 'SQ002' TO COM-GENE-MESANO                           EQW8LBVR
03810                         COM-CODERR                                EQW8LBVR
03811         MOVE 2       TO KONTROL                                   EQW8LBVR
03812         MOVE ZONE-TS-ECRAN TO Z-MAP                               EQW8LBVR
03813         PERFORM RESTAURATION-TS-ECRAN THRU                        EQW8LBVR
03814                 FIN-RESTAURATION-TS-ECRAN                         EQW8LBVR
03815         PERFORM SORTIE-ERREUR THRU                                EQW8LBVR
03816                 FIN-SORTIE-ERREUR                                 EQW8LBVR
03817      END-IF.                                                      EQW8LBVR
03818 *                                                                 EQW8LBVR
03819 *  REMISE A BLANC DU POSTE ACTUEL DANS LA PILE DES CONVERSATIONS  EQW8LBVR
03820 *                                                                 EQW8LBVR
03821      MOVE SPACES TO COM-GENE-PILCNV(COM-GENE-INDCNV).             EQW8LBVR
03822 *                                                                 EQW8LBVR
03823 * RECUPERATION DU NIVEAU SUPERIEUR DANS LA PILE DES CONVERSATIONS EQW8LBVR
03824      SUBTRACT 1 FROM COM-GENE-INDCNV.                             EQW8LBVR
03825      MOVE COM-GENE-PILCNV(COM-GENE-INDCNV) TO NOM-TACHE-XCTL.     EQW8LBVR
03826 *  DELETE DE LA TS ECRAN                                          EQW8LBVR
03827      PERFORM          DELETE-TS-ECRAN THRU                        EQW8LBVR
03828                       FIN-DELETE-TS-ECRAN.                        EQW8LBVR
03829 *                                                                 EQW8LBVR
03830      MOVE SPACES       TO COM-GENE-REAF.                          EQW8LBVR
03831 *                                                                 EQW8LBVR
03832      MOVE NOM-PROG     TO COM-PGMPRC.                             EQW8LBVR
03833      PERFORM XCTL-PROG-COMMAREA THRU                              EQW8LBVR
03834              FIN-XCTL-PROG-COMMAREA.                              EQW8LBVR
03835  FIN-SORTIE-LEVEL-PREC.   EXIT.                                   EQW8LBVR
03836 *                                                                 EQW8LBVR
03837 ***************************************************************** EQW8LBVR
03838 *    RETOUR APRES CLEAR (OU SI EIBCALEN = 0) AU PROGRAMME DE      EQW8LBVR
03839 *    SIGNATURE TOUJOURS PAR START                                 EQW8LBVR
03840 ***************************************************************** EQW8LBVR
03841  SORTIE-LEVEL-SIGNATURE.                                          EQW8LBVR
03842 *----------------------*                                          EQW8LBVR
03843      PERFORM          DELETE-TS-PLAN THRU                         EQW8LBVR
03844                       FIN-DELETE-TS-PLAN.                         EQW8LBVR
03845 *    DELETE DES TS APPLICATIVES      CONVERSATION                 EQW8LBVR
03846 *    DELETE DE LA TS CONFIDENTIALITE CONVERSATION                 EQW8LBVR
03847      PERFORM          DELETE-TS-CONF-CONV THRU                    EQW8LBVR
03848                       FIN-DELETE-TS-CONF-CONV.                    EQW8LBVR
03849      MOVE LONG-COMMAREA  TO  LONG-START.                          EQW8LBVR
03850      MOVE EIBTRMID       TO  TERM-START.                          EQW8LBVR
03851      MOVE 'AA00'         TO  NOM-TACHE-START.                     EQW8LBVR
03852      MOVE NOM-PROG       TO  COM-PGMPRC.                          EQW8LBVR
03853 *    DELETE DE LA TS ECRAN                                        EQW8LBVR
03854      PERFORM DELETE-TS-ECRAN THRU                                 EQW8LBVR
03855              FIN-DELETE-TS-ECRAN.                                 EQW8LBVR
03856 *                                                                 EQW8LBVR
03857      MOVE SPACES                 TO COM-GENE-SWPCNV.              EQW8LBVR
03858      MOVE    SPACES      TO COM-GENE-REAF.                        EQW8LBVR
03859 *                                                                 EQW8LBVR
03860      PERFORM START-TACHE THRU                                     EQW8LBVR
03861              FIN-START-TACHE.                                     EQW8LBVR
03862      PERFORM RETOUR      THRU                                     EQW8LBVR
03863              FIN-RETOUR.                                          EQW8LBVR
03864 *                                                                 EQW8LBVR
03865  FIN-SORTIE-LEVEL-SIGNATURE.         EXIT.                        EQW8LBVR
03866 *                                                                 EQW8LBVR
03867 ***************************************************************** EQW8LBVR
03868 *    RETOUR APRES PF4 AU MENU  PRINCIPAL                          EQW8LBVR
03869 ***************************************************************** EQW8LBVR
03870 *                                                                 EQW8LBVR
03871  SORTIE-LEVEL-MAX.                                                EQW8LBVR
03872 *----------------*                                                EQW8LBVR
03873      PERFORM          DELETE-TS-PLAN THRU                         EQW8LBVR
03874                       FIN-DELETE-TS-PLAN.                         EQW8LBVR
03875 *  REMISE A BLANC DE LA PILE DES CONVERSATIONS                    EQW8LBVR
03876 *  ET DES ENCHAINEMENTS                                           EQW8LBVR
03877      MOVE SPACES TO COM-GENE-CNVPIL.                              EQW8LBVR
03878      MOVE SPACES TO COM-GENE-ENCCNV.                              EQW8LBVR
03879      MOVE ZERO   TO COM-GENE-INDCNV.                              EQW8LBVR
03880 *  RECUPERATION DU MENU PRINCIPAL                                 EQW8LBVR
03881 *  REMISE A ZERO DE L'INDICE MENU                                 EQW8LBVR
03882 *  REMISE A BLANC DE LA PILE DES MENUS                            EQW8LBVR
03883 *                                                                 EQW8LBVR
03884      MOVE COM-GENE-PILMEN(1) TO COM-GENE-NEWMEN.                  EQW8LBVR
03885      MOVE COM-GENE-EXPTRNID  TO NOM-TACHE-START.                  EQW8LBVR
03886 */                                                                EQW8LBVR
03887      MOVE ZERO   TO COM-GENE-INDMEN.                              EQW8LBVR
03888      MOVE SPACES TO COM-GENE-MENPIL.                              EQW8LBVR
03889 *                                                                 EQW8LBVR
03890      MOVE SPACES       TO COM-GENE-REAF.                          EQW8LBVR
03891 *  DELETE DE LA TS ECRAN                                          EQW8LBVR
03892      PERFORM          DELETE-TS-ECRAN THRU                        EQW8LBVR
03893                       FIN-DELETE-TS-ECRAN.                        EQW8LBVR
03894      MOVE    LONG-COMMAREA  TO  LONG-START.                       EQW8LBVR
03895      MOVE    EIBTRMID       TO  TERM-START.                       EQW8LBVR
03896      MOVE    NOM-PROG       TO  COM-PGMPRC.                       EQW8LBVR
03897 *    DELETE  DES TS APPLICATIVES ET CONFIDENTIALITE CONVERSATION  EQW8LBVR
03898      PERFORM DELETE-TS-CONF-CONV THRU                             EQW8LBVR
03899              FIN-DELETE-TS-CONF-CONV.                             EQW8LBVR
03900      MOVE SPACES                 TO COM-GENE-SWPCNV.              EQW8LBVR
03901      PERFORM START-TACHE THRU                                     EQW8LBVR
03902              FIN-START-TACHE.                                     EQW8LBVR
03903      PERFORM RETOUR      THRU                                     EQW8LBVR
03904              FIN-RETOUR.                                          EQW8LBVR
03905  FIN-SORTIE-LEVEL-MAX.  EXIT.                                     EQW8LBVR
03906 *                                                                 EQW8LBVR
03907 ***************************************************************** EQW8LBVR
03908 * SORTIE ERREUR MANIPULATION DES TOUCHES FONCTION               * EQW8LBVR
03909 ***************************************************************** EQW8LBVR
03910  SORTIE-ERREUR-MANIP.                                             EQW8LBVR
03911 *-------------------*                                             EQW8LBVR
03912      MOVE   'SQ007'        TO COM-GENE-MESANO                     EQW8LBVR
03913                               COM-CODERR                          EQW8LBVR
03914      MOVE    2             TO KONTROL                             EQW8LBVR
03915      MOVE    ZONE-TS-ECRAN TO Z-MAP                               EQW8LBVR
03916      PERFORM RESTAURATION-TS-ECRAN THRU                           EQW8LBVR
03917              FIN-RESTAURATION-TS-ECRAN                            EQW8LBVR
03918      PERFORM SORTIE-ERREUR THRU                                   EQW8LBVR
03919              FIN-SORTIE-ERREUR.                                   EQW8LBVR
03920  FIN-SORTIE-ERREUR-MANIP.  EXIT.                                  EQW8LBVR
03921 *                                                                 EQW8LBVR
03922      EXIT.                                                        EQW8LBVR
03923 * **************************************************              EQW8LBVR
03924 *    RESTAURATION-MAP  PAR RESTAURATION TS-ECRAN                  EQW8LBVR
03925 * **************************************************              EQW8LBVR
03926 *                                                                 EQW8LBVR
03927 ***************************************************************** EQW8LBVR
03928 *    RESTAURATION MAP                                             EQW8LBVR
03929 ***************************************************************** EQW8LBVR
03930 *                                                                 EQW8LBVR
03931  RESTAURATION-MAP.                                                EQW8LBVR
03932 *                                                                 EQW8LBVR
03933      PERFORM RESTAURATION-TS-ECRAN THRU                           EQW8LBVR
03934              FIN-RESTAURATION-TS-ECRAN.                           EQW8LBVR
03935 *                                                                 EQW8LBVR
03936      MOVE LOW-VALUE  TO  ECR-XTRMTRACO.                           EQW8LBVR
03937      MOVE LOW-VALUE  TO  ECR-XAPPLILO.                            EQW8LBVR
03938      MOVE LOW-VALUE  TO  ECR-XJOURDO.                             EQW8LBVR
03939      MOVE LOW-VALUE  TO  ECR-XRACFLO.                             EQW8LBVR
03940      MOVE LOW-VALUE  TO  ECR-XHEUREDO.                            EQW8LBVR
03941      MOVE LOW-VALUE  TO  ECR-GESCLIO.                             EQW8LBVR
03942      MOVE LOW-VALUE  TO  ECR-RAICO.                               EQW8LBVR
03943      MOVE LOW-VALUE  TO  ECR-NOMCO.                               EQW8LBVR
03944      MOVE LOW-VALUE  TO  ECR-PERNUMXO.                            EQW8LBVR
03945      MOVE LOW-VALUE  TO  ECR-PERSTACO.                            EQW8LBVR
03946      MOVE LOW-VALUE  TO  ECR-PERSALNO.                            EQW8LBVR
03947      MOVE LOW-VALUE  TO  ECR-PERTITLO.                            EQW8LBVR
03948      MOVE LOW-VALUE  TO  ECR-PERNOMLO.                            EQW8LBVR
03949      MOVE LOW-VALUE  TO  ECR-PERPRELO.                            EQW8LBVR
03950      MOVE LOW-VALUE  TO  ECR-PERNAIDO.                            EQW8LBVR
03951      MOVE LOW-VALUE  TO  ECR-PERSEXCO.                            EQW8LBVR
03952      MOVE LOW-VALUE  TO  ECR-PERENCNO.                            EQW8LBVR
03953      MOVE LOW-VALUE  TO  ECR-PERMATCO.                            EQW8LBVR
03954      MOVE LOW-VALUE  TO  ECR-PERPROCO.                            EQW8LBVR
03955      MOVE LOW-VALUE  TO  ECR-PERPROLO.                            EQW8LBVR
03956      MOVE LOW-VALUE  TO  ECR-PRMTYPC1O.                           EQW8LBVR
03957      MOVE LOW-VALUE  TO  ECR-PRMOBTD1O.                           EQW8LBVR
03958      MOVE LOW-VALUE  TO  ECR-PRMTYPC2O.                           EQW8LBVR
03959      MOVE LOW-VALUE  TO  ECR-PRMOBTD2O.                           EQW8LBVR
03960      MOVE LOW-VALUE  TO  ECR-PERCOACO.                            EQW8LBVR
03961      MOVE LOW-VALUE  TO  ECR-ANPANCNO.                            EQW8LBVR
03962      MOVE LOW-VALUE  TO  ECR-ANPINDCO.                            EQW8LBVR
03963      MOVE LOW-VALUE  TO  ECR-ANPNBJNO.                            EQW8LBVR
03964      MOVE LOW-VALUE  TO  ECR-ANPMOTLO.                            EQW8LBVR
03965      MOVE LOW-VALUE  TO  ECR-XCDECO.                              EQW8LBVR
03966 *                                                                 EQW8LBVR
03967  FIN-RESTAURATION-MAP.  EXIT.                                     EQW8LBVR
03968               EJECT                                               EQW8LBVR
03969 *                                                                 EQW8LBVR
03970 ***************************************************************** EQW8LBVR
03971 * DELETE       DE LA TS DE CONFIDENTIALITE CONVERSATION           EQW8LBVR
03972 ***************************************************************** EQW8LBVR
03973  DELETE-TS-CONF-CONV.                                             EQW8LBVR
03974 *-------------------*                                             EQW8LBVR
03975      MOVE    IDENT-TS-CONF  TO IDENT-TS.                          EQW8LBVR
03976      PERFORM DELETE-TS THRU                                       EQW8LBVR
03977              FIN-DELETE-TS.                                       EQW8LBVR
DELTS *
DELTS  ++INCLUDE MAIDELTS
DELTS *
03978  FIN-DELETE-TS-CONF-CONV.   EXIT.                                 EQW8LBVR
03979 *                                                                 EQW8LBVR
03980 *          DELETE  DE LA TS 'PLAN'                                EQW8LBVR
03981  ++INCLUDE SQKCPLDE                                               EQW8LBVR
03982 *                                                                 EQW8LBVR
03983 ***************************************************************** EQW8LBVR
03984 *  APPEL DES ORDRES CICS LES PLUS USITES                        * EQW8LBVR
03985 ***************************************************************** EQW8LBVR
03986 *                                                                 EQW8LBVR
03987 ****************************************************************  EQW8LBVR
03988 * RETOUR AVEC COMMAREA                                            EQW8LBVR
03989 ***************************************************************** EQW8LBVR
03990 *                                                                 EQW8LBVR
03991  ++INCLUDE SQKCRTCO                                               EQW8LBVR
03992 *                                                                 EQW8LBVR
03993 ****************************************************************  EQW8LBVR
03994 * RETOUR A CICS                                                   EQW8LBVR
03995 ***************************************************************** EQW8LBVR
03996 *                                                                 EQW8LBVR
03997  ++INCLUDE SQKCRTNO                                               EQW8LBVR
03998 ***************************************************************   EQW8LBVR
03999 * SEND MAP ERREUR                                                 EQW8LBVR
04000 ***************************************************************   EQW8LBVR
04001 *                                                                 EQW8LBVR
04002  ++INCLUDE SQKCSMER                                               EQW8LBVR
04003 *                                                                 EQW8LBVR
04004 ***************************************************************** EQW8LBVR
04005 * ENVOI MAP SIMPLE : SEND-MAP     ET   SEND-MAP-CURSOR            EQW8LBVR
04006 ***************************************************************** EQW8LBVR
04007 *                                                                 EQW8LBVR
04008  ++INCLUDE SQKCSM00                                               EQW8LBVR
04009 *                                                                 EQW8LBVR
04010 ****************************************************************  EQW8LBVR
04011 *  PASSAGE DU CONTROLE A UNE AUTRE TACHE                          EQW8LBVR
04012 ****************************************************************  EQW8LBVR
04013 *                                                                 EQW8LBVR
04014  ++INCLUDE SQKCSTRT                                               EQW8LBVR
04015 *                                                                 EQW8LBVR
04016 ***************************************************************** EQW8LBVR
04017 * PASSAGE DU CONTROL A UN NOUVEAU PROGRAMME                       EQW8LBVR
04018 ***************************************************************** EQW8LBVR
04019 *                                                                 EQW8LBVR
04020  ++INCLUDE SQKCXCTL                                               EQW8LBVR
04021 *                                                                 EQW8LBVR
04022 ***************************************************************** EQW8LBVR
04023 * RETRIEVE DES DATA EN PROVENANCE D'UN START                      EQW8LBVR
04024 ***************************************************************** EQW8LBVR
04025 *                                                                 EQW8LBVR
04026  ++INCLUDE SQKCRETR                                               EQW8LBVR
04027 *                                                                 EQW8LBVR
04028 ****************************************************************  EQW8LBVR
04029 *  PASSAGE DU CONTROLE A UN PROGRAMME DE LA MEME TACHE            EQW8LBVR
04030 ****************************************************************  EQW8LBVR
04031 *                                                                 EQW8LBVR
04032  ++INCLUDE SQKCLNKB                                               EQW8LBVR
04033 *                                                                 EQW8LBVR
04034 ***************************************************************** EQW8LBVR
04035 * CONSULTATION DE LA TEMPORARY STORAGE                            EQW8LBVR
04036 ***************************************************************** EQW8LBVR
04037 *                                                                 EQW8LBVR
04038  ++INCLUDE SQKCTRDB                                               EQW8LBVR
04039 *                                                                 EQW8LBVR
04040  ++INCLUDE SQKCTSPL                                               EQW8LBVR
04041 ***************************************************************** EQW8LBVR
04042 * DELETE       DE LA TEMPORARY STORAGE                            EQW8LBVR
04043 ***************************************************************** EQW8LBVR
04044 *                                                                 EQW8LBVR
04045  ++INCLUDE SQKCTSDE                                               EQW8LBVR
04046 *                                                                 EQW8LBVR
04047 ***************************************************************** EQW8LBVR
04048 * ENVOI MAP SANS ERASE DATAONLY                                   EQW8LBVR
04049 ***************************************************************** EQW8LBVR
04050 *                                                                 EQW8LBVR
04051  ++INCLUDE SQKCSMDO                                               EQW8LBVR
04052 *                                                                 EQW8LBVR
04053 ***************************************************************** EQW8LBVR
04054 * SEND MAP ERREUR MDT OFF                                         EQW8LBVR
04055 ***************************************************************** EQW8LBVR
04056 *                                                                 EQW8LBVR
04057  ++INCLUDE SQKCSEDO                                               EQW8LBVR
04058 *                                                                 EQW8LBVR
04059 *                                                                 EQW8LBVR
04060 ***************************************************************** EQW8LBVR
04061 *   MODULES DE CONTROLE ET DE TRAITEMENT SPECIFIQUES            * EQW8LBVR
04062 ***************************************************************** EQW8LBVR
04063 *                                                                 EQW8LBVR
04064 /                                                                 EQW8LBVR
04065 ******************************************************************EQW8LBVR
04066 * ACCES A L'INTERFACE AUAAL00 QUI CONSTRUIT                       EQW8LBVR
04067 * LA TS DE CONFIDENTIALITE DE LA CONVERSATION : COM-GENE-CODCNV   EQW8LBVR
04068 ******************************************************************EQW8LBVR
04069  INTERFACE-CONFIDENTIALITE.                                       EQW8LBVR
04070 *-------------------------*                                       EQW8LBVR
04071      MOVE SPACES                    TO COM-AU-AUAAC.              EQW8LBVR
04072      MOVE COM-GENE-CODCIE-PRINCIPAL TO COM-AU-CIE.                EQW8LBVR
04073      MOVE COM-GENE-CODSIT           TO COM-AU-SITE.               EQW8LBVR
04074      MOVE COM-GENE-CODCNV           TO COM-AU-CONVERS.            EQW8LBVR
04075      MOVE COM-GENE-CODUSR           TO COM-AU-USAGER.             EQW8LBVR
04076 *              COM-AU-SWAP   (1 : SWAP N∞1 / 2 : SWAP N∞2)        EQW8LBVR
04077      MOVE '1'                       TO COM-AU-SWAP.               EQW8LBVR
04078 *              COM-AU-TYPETS (M : MENU / C : CONVERSATION)        EQW8LBVR
04079      MOVE 'C'                       TO COM-AU-TYPETS.             EQW8LBVR
04080 *                                                                 EQW8LBVR
04081      EXEC CICS LINK PROGRAM  ('AUAAL00')                          EQW8LBVR
04082                     COMMAREA (COM-AU-AUAAC)                       EQW8LBVR
04083                     LENGTH   (COM-AU-LONG-AUAAC)                  EQW8LBVR
04084      END-EXEC.                                                    EQW8LBVR
04085 *                                                                 EQW8LBVR
04086      MOVE COM-AU-LONG-TS          TO COM-GENE-LNGCNV.             EQW8LBVR
04087      MOVE COM-AU-MESSAGE          TO COM-GENE-MESANO.             EQW8LBVR
04088  FIN-INTERFACE-CONFIDENTIALITE. EXIT.                             EQW8LBVR
04089 *                                                                 EQW8LBVR
04090 ******************************************************************EQW8LBVR
04091 * ACCES A L'INTERFACE AUAAL04 QUI REFAIT  UN CONTROLE D'ACCES     EQW8LBVR
04092 * (MEME CONTROLE QU'AU NIVEAU  MENU )                             EQW8LBVR
04093 ******************************************************************EQW8LBVR
04094  INTERFACE-CONTROLE-ACCES.                                        EQW8LBVR
04095 *------------------------*                                        EQW8LBVR
04096      MOVE SPACES                    TO COM-AU-AUAAC.              EQW8LBVR
04097      MOVE COM-GENE-CODCIE-PRINCIPAL TO COM-AU-CIE.                EQW8LBVR
04098      MOVE COM-GENE-CODSIT           TO COM-AU-SITE.               EQW8LBVR
04099      MOVE COM-GENE-CODCNV           TO COM-AU-CONVERS.            EQW8LBVR
04100      MOVE COM-GENE-CODUSR           TO COM-AU-USAGER.             EQW8LBVR
04101 *              COM-AU-SWAP   (1 : SWAP N∞1 / 2 : SWAP N∞2)        EQW8LBVR
04102      MOVE '1'                       TO COM-AU-SWAP.               EQW8LBVR
04103 *                                                                 EQW8LBVR
04104      EXEC CICS LINK PROGRAM  ('AUAAL04')                          EQW8LBVR
04105                     COMMAREA (COM-AU-AUAAC)                       EQW8LBVR
04106                     LENGTH   (COM-AU-LONG-AUAAC)                  EQW8LBVR
04107      END-EXEC.                                                    EQW8LBVR
04108  FIN-INTERFACE-CONTROLE-ACCES.  EXIT.                             EQW8LBVR
04109 /                                                                 EQW8LBVR
04110 ***************************************************************** EQW8LBVR
04111 *   LECTURE DES MESSAGES D'INFORMATION ET D'ANOMALIE            * EQW8LBVR
04112 ***************************************************************** EQW8LBVR
04113  LECTURE-ERREUR.                                                  EQW8LBVR
04114 *--------------*                                                  EQW8LBVR
04115      MOVE  SPACES                 TO XSPIPARM.                    EQW8LBVR
04116      IF    COM-GENE-MESINF NOT = SPACES AND LOW-VALUE             EQW8LBVR
04117            MOVE COM-GENE-MESINF   TO W-CODERR                     EQW8LBVR
04118            MOVE '*CD'             TO EL-DEMANDES OF XSPIPARM      EQW8LBVR
04119      ELSE                                                         EQW8LBVR
04120            MOVE COM-GENE-MESANO   TO W-CODERR                     EQW8LBVR
04121      END-IF.                                                      EQW8LBVR
04122      MOVE  'GP'                   TO FONCTION  OF XSPIPARM.       EQW8LBVR
04123      MOVE  'MSGETUDE'             TO CODTAB    OF XSPIPARM.       EQW8LBVR
04124      MOVE  '= '                   TO OPERATEUR OF XSPIPARM.       EQW8LBVR
04125      MOVE   W-CODERR              TO REF-POSTE OF XSPIPARM.       EQW8LBVR
04126      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW8LBVR
04127      IF  RETCOD OF XSPIPARM  = ZERO                               EQW8LBVR
04128          MOVE 0       TO CODE-RETOUR                              EQW8LBVR
04129          MOVE IOAREA  OF XSPIPARM TO W-ERREUR                     EQW8LBVR
04130          IF   COM-GENE-MESINF = SPACES OR LOW-VALUE               EQW8LBVR
04131               MOVE SPACES TO W-CODERR                             EQW8LBVR
04132          END-IF                                                   EQW8LBVR
04133          MOVE SPACES TO W-SUFERR                                  EQW8LBVR
04134      ELSE                                                         EQW8LBVR
04135          MOVE SPACES TO W-LIBERR                                  EQW8LBVR
04136                          W-SUFERR                                 EQW8LBVR
04137          MOVE 1       TO CODE-RETOUR                              EQW8LBVR
04138      END-IF.                                                      EQW8LBVR
04139  FIN-LECTURE-ERREUR.  EXIT.                                       EQW8LBVR
04140 *                                                                 EQW8LBVR
04141 ***************************************************************** EQW8LBVR
04142 * ACCES SPITAB                                                    EQW8LBVR
04143 ***************************************************************** EQW8LBVR
04144  ++INCLUDE SQKCSPI2                                               EQW8LBVR
04145 ***************************************************************** EQW8LBVR
04146 * SORTIE ABANDON POUR ERREURS    NON PREVUES                      EQW8LBVR
04147 ***************************************************************** EQW8LBVR
04148  ABANDON-TACHE.                                                   EQW8LBVR
04149  ++INCLUDE SQKCMROB                                               EQW8LBVR
U3319  ++INCLUDE SQKCCON2                                               EFUTSV6O
04150 ** FIN DE PROGRAMME  FB02T00  CREE LE  01/02/08  A  15:52  .      EQW8LBVR
