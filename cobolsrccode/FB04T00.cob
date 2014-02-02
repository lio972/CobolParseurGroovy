00001  IDENTIFICATION DIVISION.                                         13/02/02
00002  PROGRAM-ID.                     FB04T00.                         EQW9Z0MQ
      *-02-11-93------------------------------------------------------+
00003 * MODULE EN COURS DE MAINTENANCE PAR   : .....................  !    LV001
00004 * PREVU POUR ETRE MIS EN PRODUCTION LE : ../../..               ! EQW9Z0MQ
00005 * N.B. : .....................................................  ! EQW9Z0MQ
00006 *-02-11-93------------------------------------------------------+ EQW9Z0MQ
00007 * NOM DU LOAD MODULE : FB04T00                CREE LE 01/03/2001  EQW9Z0MQ
00008 * NOM DE TRANSACTION : FB04                        A 09:39        EQW9Z0MQ
00009 * NOM DE MAP         : FB04M0Z                                    EQW9Z0MQ
00010 * NOM DU PGM BATCH   : ........ (SI MODULE COMMUN TP/BATCH)       EQW9Z0MQ
00011 * AUTEUR             : B.BAURËS                                   EQW9Z0MQ
00012 * LOGON TSO          : ETDA4                                      EQW9Z0MQ
00013 *                                                                 EQW9Z0MQ
00014 *                      CARACTERISTIQUES/ANTECEDENTS 4R, 2R, C-CAR EQW9Z0MQ
00015 *                                                                 EQW9Z0MQ
00016 * TITRE              : FB04T00                                    EQW9Z0MQ
00017 *---------------------------------------------------------------+ EQW9Z0MQ
00018 * CNC$PROG MAINT     : TK    090990 AJOUT TRAITEMENT TS PLAN      EQW9Z0MQ
00019 *                    :              POUR  EXIT DE SELECTION DYNA  EQW9Z0MQ
00020 *   09/09/90         :              MIQUE DE PLAN                 EQW9Z0MQ
00021 *                    : POUR TOUTE   CONVERSATION (SAUF MENU)      EQW9Z0MQ
00022 *---------------------------------------------------------------+ EQW9Z0MQ
      * CNC$PROG MAINT     : TK    AJOUT ++INCLUDE SQKCPLDE           +
      *                    :       AJOUT APPEL DELETE-TS-PLAN DANS    +
      *  09-10-90          :       SORTIE-LEVEL-(SUP/MAX/SIGNATURE)   +
      *                    :       ET ABANDON-TACHE (SQKCMERO)        +
      *                    : POUR TOUTE   CONVERSATION (SAUF MENU)    +
00028 *---------------------------------------------------------------+ EQW9Z0MQ
      *                          >> BUT <<                            !
      * ............................................................. !
00031 *                                                               ! EQW9Z0MQ
00032 *                       >> SYNOPTIQUE <<                        ! EQW9Z0MQ
00033 *----------+-------+--------------------------------------------+ EQW9Z0MQ
00034 * RESSOURCE! M/O/A !               DESCRIPTION                  ! EQW9Z0MQ
00035 *----------+-------+--------------------------------------------+ EQW9Z0MQ
00036 *          !       !                                            ! EQW9Z0MQ
00037 *----------+-------+----------+---------------------------------+ EQW9Z0MQ
00038 * PROGR.   ! MODE  ! COMMAREA !        DESCRIPTION              ! EQW9Z0MQ
00039 * APPELANT ! APPEL ! TRANSMISE!                                 ! EQW9Z0MQ
00040 *----------+-------+----------+---------------------------------+ EQW9Z0MQ
00041 *          !       !          !                                 ! EQW9Z0MQ
00042 *----------+-------+----------+---------------------------------+ EQW9Z0MQ
00043 * PROGR.   ! MODE  ! COMMAREA !        DESCRIPTION              ! EQW9Z0MQ
00044 * APPELE   ! APPEL ! TRANSMISE!                                 ! EQW9Z0MQ
00045 *----------+-------+----------+---------------------------------+ EQW9Z0MQ
00046 *          !       !          !                                 ! EQW9Z0MQ
00047 *----------+-------+----------+---------------------------------+ EQW9Z0MQ
00048 * ERREUR   !             MESSAGE OU TRAITEMENT                  ! EQW9Z0MQ
00049 *----------+----------------------------------------------------+ EQW9Z0MQ
00050 *          !                                                    ! EQW9Z0MQ
00051 *----------+----------------------------------------------------+ EQW9Z0MQ
00052 *             >> STRUCTURE GENERALE DU PROGRAMME <<             ! EQW9Z0MQ
00053 * ............................................................. ! EQW9Z0MQ
00054 * ............................................................. ! EQW9Z0MQ
00055 *                                                               ! EQW9Z0MQ
00056 *                       >> MAINTENANCE <<                       ! EQW9Z0MQ
00057 *-------------+-------------------------------------------------+ EQW9Z0MQ
00058 * DATE/AUTEUR !         DESCRIPTION DE LA MODIFICATION          ! EQW9Z0MQ
00059 *-------------+-------------------------------------------------+ EQW9Z0MQ
      * 03/05/2002  ! FD 22107 : CORRECTION CALCUL ARRONDI CRM        !
      * O. LASEIGNE !                                                 !
00063 *-------------+-------------------------------------------------+ EQW9Z0MQ
00060 * 14/06/2002  !  DPI 22779: EN CAS DE REMPLACEMENT, SI LE VÈHI- ! EQW90UNO
00061 * C.LANDRODIE !  CULE EST EN INSERT, ACCES ‡ L'ÈCRAN ANTÈCÈDENTS! EQW90UNO
00062 *             !  VÈHICULES (SI RÈPONSE 'O' DANS SINISTRES SUR   ! EQW90UNO
00062 *             !  LES 36 DERNIERS MOIS).                         ! EQW90UNO
00063 *-------------+-------------------------------------------------+ EQW90UNO
      * B.BAURES    ! DPI 23007: NE PAS DEBRANCHER SUR L'ÈCRAN SINISTR!
      * 19/06/2002  ! EN AV ET RV SANS AJOUT DE VEHICULE              !
00063 *-------------+-------------------------------------------------+ EQW9Z0MQ
      * C.LANDRODIE ! DPI 22755: COMMUNE DU LIEU DE GARAGE: RECHERCHE ! 00007100
      * 20/06/2002  ! DANS GEBCOMM SUR LES 30 PREMIERS CARACTERES DE  ! 00007200
      *             ! LA ZONE DU NOM DE LA COMMUNE.                   ! 00007300
      *-------------+-------------------------------------------------+
      * D.GALLAY    ! DPI 22931: SUPPRESSION FORCAGE ‡ 'O' DU CODE    ! 00007100
      * 12/07/2002  ! GARANTIE DANS LA CAS DE VEHICULE 2R > ‡ 125 CM3 ! 00007200
      *-------------+-------------------------------------------------+
      * C.LANDRODIE ! DPI 23215: AJOUT DES ZONES ÈCRAN GROUPE ET      ! 00007100
      * 17/07/2002  ! CLASSE DU VÈHICULE.                             ! 00007200
      *-------------+-------------------------------------------------+
      * B.BAURES    ! DPI 23216: OPTION CHANGEMENT DE VEHICULE        ! 00007100
      * 30/07/2002  !                                                 ! 00007200
      *-------------+-------------------------------------------------+
      * A.BERNARD   ! DPI 22110: PHASE PRE-AFFICHAGE FRANCHISE DOC    ! 00007100
      * 30/08/2002  !                                                 ! 00007200
      *-------------+-------------------------------------------------+
      * D.CARDON    ! DPI 22110: FORCAGE GENRE ET CLASSE DANS ITEM 1  ! 00007100
      * 18/12/2002  ! QUAND REDETERMINATION DANS ITEM 2               ! 00007200
      *-------------+-------------------------------------------------+
      * D.CARDON    ! DPI U3236: REDETERMINATION CODE INSEE           ! 00007100
      * 27/12/2002  !                                                 ! 00007200
      *-------------+-------------------------------------------------+
      * F.LE ROUX   ! DPI 28556: SUPPRESSION USAGE TOURNEES           ! 00007100
      * 08/03/2003  !                                                 ! 00007200
      *-------------+-------------------------------------------------+
      *  KANCEL     ! FD 28919 : MISE NOUVELLE ECRAN D'AIDE POUR LA   ! 00008800
      * 14/04/2003  !            RECHERCHE DE LA COMPAGNIE DU         ! 00008900
      *             !            PRECEDENT CONTRAT.                   ! 00009000
      *-------------+-------------------------------------------------+ 00009100
00100 * 12/05/2003  ! FD F9674 : REMISE ‡ BLANC DU CRM + DATE CRM     ! FB04T00O
00101 * D.CARDON    ! SI SAISIE SUR 2R < 80 CM3                       ! FB04T00O
00102 *-------------+-------------------------------------------------+ FB04T00O
U3319 * 22/04/2003  ! FD U3319 : EXT.AACONV + MODIF ALIM. BANDEAU     ! EFU4RQMP
U3319 * O.LASEIGNE  !            CLIENT                               ! EFU4RQMP
00063 *-------------+-------------------------------------------------+ EQW9Z48F
F9674 * 20/05/2003  ! FD F9674 : ANO CODE ANCIENNE CIE                ! FB04T00O
F9674 * D.CARDON    !            + DATE ANCIEN CRM INVALIDE           ! FB04T00O
00102 *-------------+-------------------------------------------------+ FB04T00O
F9674 * 22/08/2003  ! DPI32033 : USAGE TOURNEES EN 2 ROUES : PASSAGE  ! 00010700
F9674 * TH SECHER   ! CTRL-USAGE EN HABILI POUR AJOUT 2R EN AV/RV     ! 00010800
00102 *-------------+-------------------------------------------------+ 00010900
F2980 * 10/12/2003  ! DPI32980 : EVOL HORS SERIE PRO                  ! 00010700
F2980 * B MELLON    !                                                 ! 00010800
00102 *-------------+-------------------------------------------------+ 00010900
F3576 * 14/11/2003  ! FD 33576 : EVOLUTION MESURES DE SOUSCRIPTION    ! EFU4RQMP
F3576 * A. LEMAITRE !            GESTION DE LA DATE D'ACQUISITION DU  ! EFU4RQMP
F3576 *             !            VEHICULE                             ! EFU4RQMP
F3576 *-------------+-------------------------------------------------+
33295 * 17/11/2003  ! FD 33295 : FUE AVENANT HORS SERIE               ! MA90T21V
33295 * E.T.        !  => CONTROLE DU NOUVEAU CODE FORCAGE 'F'        ! MA90T21V
33295 *-------------+-------------------------------------------------+ 00051500
33295 * 02/02/2004  ! FD U3921 : CALCUL DE BONUS MOYEN ERRONE         ! MA90T21V
33295 * JL MARTEAU  !  CAR ZONE DE WORKING TROP PETITE                ! MA90T21V
33295 *-------------+-------------------------------------------------+ 00051500
U3925 * 04/02/2004  ! FD U3925 : CORRECTIF ANOMALIE SAISIE STATUT     ! MA90T21V
U3925 * O LASEIGNE  !  ANCIEN SOUSCRIPTEUR                            ! MA90T21V
U3925 *-------------+-------------------------------------------------+ 00051500
F35611* 27/02/2004  ! FD 35611 : RECOMPIL CAR MODIF FB90C09           ! MA90T21V
F35611* D.CARDON    !                                                 ! MA90T21V
F35611*-------------+-------------------------------------------------+ 00051500
F36835* 05/04/2004  ! FD 36835 : MISE A NIVEAU CONVERGENES            ! MA90T21V
F36835* B MELLON    !            AJOUT DE GARANTIES                   ! MA90T21V
F36835*-------------+-------------------------------------------------+ 00051500
F36835* 07/05/2004  ! FD U4058 : ANO FORCAGE F SI SINISTRE A 'O'      ! MA90T21V
F36835* JL MARTEAU  !            ET PAS D'ANTECEDANT                  ! MA90T21V
F36835*-------------+-------------------------------------------------+ 00051500
F39250* 28/05/2004  ! DPI 39250: PROFESSION ET USAGES AGENTS GENERAUX ! MA90T21V
F39250* F LE ROUX   !            USAGE 14 INTERDIT POUR 2 ROUES       ! MA90T21V
F39250*-------------+-------------------------------------------------+ 00051500
U4080 * 02/06/2004  ! DPI U4080: MODIFICATION FRANCHISES 2 ROUES      ! MA90T21V
U4080 * B MELLON    !                                                 ! MA90T21V
F39250*-------------+-------------------------------------------------+ 00051500
36835 * 25/06/2004  ! FD 36835 : MISE A NIVEAU CONVERGENES            ! MA90T21V
36835 * B MELLON    !            CARTE GRISE                          ! MA90T21V
36835 *-------------+-------------------------------------------------+ 00051500
39101 * 13/07/2004  ! FD 39101 : SUITE ‡ TRANSCO CARROSSERIE BK5      ! MA90T21V
39101 * D CARDON    ! DU GENRE D ‡ M, NE PAS FAIRE LE CTL FB110       ! MA90T21V
39101 *-------------+-------------------------------------------------+ 00051500
U4080 * 03/08/2004  ! DPI U4080: MODIFICATION FRANCHISES 2 ROUES      ! MA90T21V
U4080 * B MELLON    !                                                 ! MA90T21V
U4080 *-------------+-------------------------------------------------+ 00051500
U4080 * 09/08/2004  ! DPI U4151: LA SAISIE DU SOUSCRIPTEUR N'EST      ! MA90T21V
U4080 * JL MARTEAU  ! PARFOIS PAS PRISE EN COMPTE...                  ! MA90T21V
U4080 *-------------+-------------------------------------------------+ 00051500
U4172 * 30/08/2004  ! DPI U4172: LE GROUE ET LA CLASSE DES 2 ROUES    ! MA90T21V
U4172 * JL MARTEAU  ! NE SONT PAS RAFRAICHIS EN CAS DE CHANGEMENT     ! MA90T21V
U4080 *-------------+-------------------------------------------------+ 00051500
F41702* 11/10/2004  ! F41702 : NOUVELLE TARIF DES GARANTIES DOM VOL   ! MA90T21V
F41702* TH. SECHER  !                                                 ! MA90T21V
F41702*-------------+-------------------------------------------------+ 00051500
AD548 * 27/10/2004  ! F35548 : GALILEO : LEVER LES CONTROLES SUR LES  ! MA90T21V
AD548 * A. DEBAETS  ! ANTECEDENTS SAUF SI AJOUT DE VEHICULE           ! MA90T21V
AD548 *-------------+-------------------------------------------------+ 00051500
DELTS * B PORTEFAIX ! 31/12/2004 AJOUT INCLUDE DELETE TS              !
      *-------------+-------------------------------------------------+
F52844* 24/11/2005  ! FD 52844 : CTL TITULAIRE CARTE GRISE            ! 00000900
F52844* VANDEVORDE  !                                                 ! 00000910
      *-------------+-------------------------------------------------+
PDO89 * M BALDIT    ! MISE A NIVEAU DES REFERENTIELS POSTAUX          !
PDO89 * 21/10/2009  ! COMMUNE ALIMENTEE PAR LIGACHL AU LIEU DE ADRCOML!
      *-------------+-------------------------------------------------+
PDO89 * 16/12/2009  ! MISE A NIVEAU DES REFERENTIELS POSTAUX          !
PDO89 * M.BALDIT    ! COMMUNE ALIMENTEE PAR ADRCOML                   !
PDO89 *-------------+-------------------------------------------------+
00064 *                       *************                           ! EQW9Z0MQ
00065 *                       ** LEGENDE **                           ! EQW9Z0MQ
00066 *                       *************                           ! EQW9Z0MQ
00067 *                                                               ! EQW9Z0MQ
00068 * RESSOURCE       : FICHIER , BASE , TS                         ! EQW9Z0MQ
00069 *                   - INDIQUER NOM COBOL ET DDNAME SI DIFFERENT ! EQW9Z0MQ
00070 *                   - SI POSSIBLE CODE DU DICTIONAIRE           ! EQW9Z0MQ
00071 *                   - DONNER PSB SI BASE                        ! EQW9Z0MQ
00072 *                                                               ! EQW9Z0MQ
00073 * M/O/A           : MODE / ORGANISATION / ACCES                 ! EQW9Z0MQ
00074 *                    !         !           !                    ! EQW9Z0MQ
00075 *                    !         !           +---> D = DIRECT     ! EQW9Z0MQ
00076 *                    !         !                 S = SEQUENTIEL ! EQW9Z0MQ
00077 *                    !         +---> S = SEQUENTIELLE           ! EQW9Z0MQ
00078 *                    !               V = VSAM                   ! EQW9Z0MQ
00079 *                    !               X = AUTRE                  ! EQW9Z0MQ
00080 *                    +---> L = LECTURE                          ! EQW9Z0MQ
00081 *                          E = ECRITURE                         ! EQW9Z0MQ
00082 *                          M = MISE A JOUR                      ! EQW9Z0MQ
00083 *                                                               ! EQW9Z0MQ
00084 * MODE D'APPEL    : CALL , XCTL , LINK                          ! EQW9Z0MQ
00085 *                                                               ! EQW9Z0MQ
00086 * COMMAREA TRANS. : - NOM DU NIVEAU "01"                        ! EQW9Z0MQ
00087 *                   - LONGUEUR DE LA COMMAREA                   ! EQW9Z0MQ
00088 *                                                               ! EQW9Z0MQ
00089 * MAINTENANCE     : LE LEVEL D'UNE MAINTENANCE CORRESPOND AU 1ER! EQW9Z0MQ
00090 *                   N∞ DE LEVEL D'INSTRUCTION CORRESPONDANT A   ! EQW9Z0MQ
00091 *                   CETTE MAINTENANCE.                          ! EQW9Z0MQ
00092 *===============================================================+ EQW9Z0MQ
00093  ENVIRONMENT DIVISION.                                            EQW9Z0MQ
00094  CONFIGURATION SECTION.                                           EQW9Z0MQ
00095  SPECIAL-NAMES.                                                   EQW9Z0MQ
00096      DECIMAL-POINT IS COMMA.                                      EQW9Z0MQ
00097  DATA DIVISION.                                                   EQW9Z0MQ
00098  WORKING-STORAGE SECTION.                                         EQW9Z0MQ
00099 *                                                                 EQW9Z0MQ
00100 ***************************************************************** EQW9Z0MQ
00101 *   ZONES DE PILOTAGE DU SQUELETTE                              * EQW9Z0MQ
00102 *   ATTENTION: CE DOIT ETRE LE PREMIER INCLUDE                  * EQW9Z0MQ
00103 ***************************************************************** EQW9Z0MQ
00104  ++INCLUDE SQKWDV0B                                               EQW9Z0MQ
U3319  01  IA-SAUVE               PIC S9(3) COMP-3   VALUE +0.
00105 /                                                                 EQW9Z0MQ
00106 ***************************************************************** EQW9Z0MQ
00107 * COMMAREA POUR APPEL D'INTERFACE DE CONFIDENTIALITE (AUAAL00)  * EQW9Z0MQ
00108 ***************************************************************** EQW9Z0MQ
00109  01  AUAAC.                                                       EQW9Z0MQ
00110  ++INCLUDE AUAAC                                                  EQW9Z0MQ
00111 /                                                                 EQW9Z0MQ
00112 ***************************************************************** EQW9Z0MQ
00113 *   IDENTIFICATION DES TABLES SPI POUR ACCES AUX TABLES         * EQW9Z0MQ
00114 *   DE TYPE MENU OU CONVERSATION                                * EQW9Z0MQ
00115 ***************************************************************** EQW9Z0MQ
00116  01  IDENT-TABLE.                                                 EQW9Z0MQ
00117      05  TABLE-PREF         PIC X(06).                            EQW9Z0MQ
00118      05  TABLE-SUFF         PIC X(02).                            EQW9Z0MQ
00119 ***************************************************************** EQW9Z0MQ
00120 *   IDENTIFICATION DE LA TS DE CONFIDENTIALITE                  * EQW9Z0MQ
00121 ***************************************************************** EQW9Z0MQ
00122  01  IDENT-TS-CONF.                                               EQW9Z0MQ
00123      05  CONF-TS-PREF       PIC X(04).                            EQW9Z0MQ
00124      05  CONF-TS-SUFF.                                            EQW9Z0MQ
00125          10  CONF-TS-CONV   PIC X(03).                            EQW9Z0MQ
00126          10  FILLER         PIC X(01) VALUE '1'.                  EQW9Z0MQ
00127 ***************************************************************** EQW9Z0MQ
00128 *   IDENTIFICATION DE LA TS APPLICATIVE                         * EQW9Z0MQ
00129 ***************************************************************** EQW9Z0MQ
00130  01  IDENT-TS-APP.                                                EQW9Z0MQ
00131      05  APP-TS-PREF        PIC X(04).                            EQW9Z0MQ
00132      05  APP-TS-SUFF.                                             EQW9Z0MQ
00133          10  APP-TS-CONV    PIC X(03).                            EQW9Z0MQ
00134          10  FILLER         PIC X(01) VALUE '1'.                  EQW9Z0MQ
00135 ***************************************************************** EQW9Z0MQ
00136 *   IDENTIFICATION DE LA TS DE PAGINATION                       * EQW9Z0MQ
00137 ***************************************************************** EQW9Z0MQ
00138  01  IDENT-TS-PAGE.                                               EQW9Z0MQ
00139      05  PAGE-TS-PREF       PIC X(04).                            EQW9Z0MQ
00140      05  PAGE-TS-SUFF.                                            EQW9Z0MQ
00141          10  PAGE-TS-CONV   PIC X(03).                            EQW9Z0MQ
00142          10  FILLER         PIC X(01) VALUE '1'.                  EQW9Z0MQ
00143 ******************* POUR CONVERSATION *************************** EQW9Z0MQ
00144 *TK0909 POUR EXIT-SELECTION-DE-PLAN : DESCRIPTION DE LA TS        EQW9Z0MQ
00145 ***************************************************************** EQW9Z0MQ
00146  ++INCLUDE SQKWPLTS                                               EQW9Z0MQ
00147 */                                                                EQW9Z0MQ
00148 /                                                                 EQW9Z0MQ
00149 ***************************************************************** EQW9Z0MQ
00150 *   DESCRIPTION DE LA TS DE CONFIDENTIALITE  CONVERSATION       * EQW9Z0MQ
00151 ***************************************************************** EQW9Z0MQ
00152  01  AUAAIW.                                                      EQW9Z0MQ
00153  ++INCLUDE AUAAIW                                                 EQW9Z0MQ
00154 /                                                                 EQW9Z0MQ
DELTS *
DELTS *  TABLE DES TS A DELETER
DELTS  ++INCLUDE CCMADLTS
DELTS *
00155 ***************************************************************** EQW9Z0MQ
00156 *   DESCRIPTION DE LA TABLE DES CONVERSATIONS (SPITAB)          * EQW9Z0MQ
00157 ***************************************************************** EQW9Z0MQ
00158  ++INCLUDE CCAACONV                                               EQW9Z0MQ
U3319  ++INCLUDE CCAACON2                                               EFUTSUGF
00159 /                                                                 EQW9Z0MQ
00160 ***************************************************************** EQW9Z0MQ
00161 *   DESCRIPTION DE LA TS SUSPENS                                * EQW9Z0MQ
00162 ***************************************************************** EQW9Z0MQ
00163  ++INCLUDE MAILONG                                                EQW9Z0MQ
00164  01  TS-SUSPENS1.                                                 EQW9Z0MQ
00165  ++INCLUDE MASP                                                   EQW9Z0MQ
00166  01  TS-SUSPENS2.                                                 EQW9Z0MQ
00167  ++INCLUDE MASP                                                   EQW9Z0MQ
00168 *                                                                 EQW9Z0MQ
00169 ***************************************************************** EQW9Z0MQ
00170 *   DESCRIPTION DE LA TS TECHNIQUE                              * EQW9Z0MQ
00171 ***************************************************************** EQW9Z0MQ
00172 * TS TECHNIQUE                                                    EQW9Z0MQ
00173 *-------------                                                    EQW9Z0MQ
00174  01 TS-TECHNIQUE.                                                 EQW9Z0MQ
00175  ++INCLUDE FBITECH                                                EQW9Z0MQ
00176 *                                                                 EQW9Z0MQ
00177 ***************************************************************** EQW9Z0MQ
00178 *   DESCRIPTION DE L'ORG 40A REFONTE AUTOMOBILE GFA             * EQW9Z0MQ
00179 ***************************************************************** EQW9Z0MQ
00180 * TS CONTRAT                                                      EQW9Z0MQ
00181 *-----------                                                      EQW9Z0MQ
00182  01 TS-CNTPROD.                                                   EQW9Z0MQ
00183  ++INCLUDE FBICONT                                                EQW9Z0MQ
00184 * TS PERSONNE                                                     EQW9Z0MQ
00185 *------------                                                     EQW9Z0MQ
00186  01 TS-PERSONNE.                                                  EQW9Z0MQ
00187  ++INCLUDE FBIPERS                                                EQW9Z0MQ
00188 * TS VEHICULE                                                     EQW9Z0MQ
00189 *------------                                                     EQW9Z0MQ
00190  01 TS-VEHICULE.                                                  EQW9Z0MQ
00191  ++INCLUDE FBIVEHI                                                EQW9Z0MQ
00188 * TS VEHICULE CHANGER                                             EQW9Z0MQ
00189 *--------------------                                             EQW9Z0MQ
00190  01 TS-VEHICULE-CHANGER.                                          EQW9Z0MQ
00191  ++INCLUDE FBIVEHI                                                EQW9Z0MQ
00192 *                                                                 EQW9Z0MQ
00193 ***************************************************************** EQW9Z0MQ
00194 *   DESCRIPTION DU SEGMENT TRANSIT                              * EQW9Z0MQ
00195 ***************************************************************** EQW9Z0MQ
00196 * TS TRANSIT ITEM 1                                               EQW9Z0MQ
00197  01 FBMISPTR-IT1.                                                 EQW9Z0MQ
00198  ++INCLUDE FBMISPTR                                               EQW9Z0MQ
00199 ***************************************************************** EQW9Z0MQ
00200 *   ZONES NECESSAIRES AU MODULE DES CONTROLES TECHNIQUES        * EQW9Z0MQ
00201 ***************************************************************** EQW9Z0MQ
00202  01 MAI90C00.                                                     EQW9Z0MQ
00203  ++INCLUDE MAI90C00                                               EQW9Z0MQ
00204 *                                                                 EQW9Z0MQ
00205 ***************************************************************** EQW9Z0MQ
00206 *   ZONES NECESSAIRES AU MODULE DES CONTROLES HABILITATION      * EQW9Z0MQ
00207 ***************************************************************** EQW9Z0MQ
00208  01 MAI90C20.                                                     EQW9Z0MQ
00209  ++INCLUDE MAI90C20                                               EQW9Z0MQ
00210 *                                                                 EQW9Z0MQ
00211 ***************************************************************** EQW9Z0MQ
00212 *   ZONES NECESSAIRES AU MODULE DE RECHERCHE DES CONNEXES       * EQW9Z0MQ
00213 ***************************************************************** EQW9Z0MQ
00214  01 FBI90C09.                                                     EQW9Z0MQ
00215  ++INCLUDE FB90C09                                                EQW9Z0MQ
00216 *                                                                 EQW9Z0MQ
00217 * INDICATEUR DE RECHERCHE DE CONNEXES                             EQW9Z0MQ
00218 *   - '0' RECHERCHE DE CONNEXES OK                                EQW9Z0MQ
00219 *   - '1' RECHERCHE DE CONNEXES NON OK                            EQW9Z0MQ
00220 *   - '2' PAS DE RECHERCHE DE CONNEXES                            EQW9Z0MQ
00221                                                                   EQW9Z0MQ
00222                                                                   EQW9Z0MQ
00223 ***************************************************************** EQW9Z0MQ
00224 *   DECRIRE   ICI   LES   ZONES   SPECIFIQUES   AU   PROGRAMME  * EQW9Z0MQ
00225 ***************************************************************** EQW9Z0MQ
00271  01  W-GESCLI.                                                    FB04T00O
00272      05  W-GES                PIC X(6).                           FB04T00O
00273      05  W-CLI                PIC X(5).                           FB04T00O
00229                                                                   EQW9Z0MQ
36835  01  W-STATUT                 PIC X(2).                           EQW9Z0MQ
36835  01  NB-VEHI-TRACTEUR         PIC 9(2).                           EQW9Z0MQ
00229                                                                   EQW9Z0MQ
00230  01  IND-B                    PIC S9(3) COMP-3.                   EQW9Z0MQ
00231  01  IND-A                    PIC S9(3) COMP-3.                   EQW9Z0MQ
00232  01  IND-C                    PIC S9(3) COMP-3.                   EQW9Z0MQ
00233  01  IND-D                    PIC S9(3) COMP-3.                   EQW9Z0MQ
00234  01  WSS-READ-TSVEHI          PIC X.                              EQW9Z0MQ
00279  01  WSS-READ-TSPERS          PIC X.                              EQW8LBVR
00235  01  WSS-NBR                  PIC 9(2).                           EQW9Z0MQ
00236  01  WSS-CYL                  PIC 9(5).                           EQW9Z0MQ
00237  01  WSS-CYLX REDEFINES WSS-CYL     PIC X(5).                     EQW9Z0MQ
00236  01  WSS-NOUV-CYL             PIC 9(5).                           EQW9Z0MQ
00237  01  WSS-NOUV-CYLX REDEFINES WSS-NOUV-CYL     PIC X(5).           EQW9Z0MQ
00237  01  WSS-NOUV-GENRE           PIC X(1).                           EQW9Z0MQ
00238  01  WSS-CYLZ                 PIC Z(4)9.                          EQW9Z0MQ
00239  01  WSS-CYLZ4                PIC Z(4)9.                          EQW9Z0MQ
00240  01  WSS-VALNEUF              PIC 9(7).                           EQW9Z0MQ
00241  01  WSS-VALNEUFX REDEFINES WSS-VALNEUF PIC X(7).                 EQW9Z0MQ
00242  01  WSS-VALNEUFZ             PIC Z(6)9.                          EQW9Z0MQ
00243  01  WSS-VALNEUFZ-11          PIC Z(10)9.                         EQW9Z0MQ
00244  01  WSS-CRM                  PIC 9(3).                           EQW9Z0MQ
00245  01  WSS-CRMZ                 PIC Z(2)9.                          EQW9Z0MQ
U3921  01  WSS-CRM-MOYEN            PIC 9(5).                           00069600
U3921  01  WSS-CRM-MOYENZ           PIC Z(4)9.                          00069700
00248  01  WSS-NBMOIS-INTER         PIC 9(2).                           EQW9Z0MQ
00249  01  WSS-CODE-INSEE.                                              EQW9Z0MQ
00250      05 WSS-CODE-DEPT         PIC X(02).                          EQW9Z0MQ
00251      05 WSS-CODE-COMM         PIC X(05).                          EQW9Z0MQ
00252  01  WSS-CODE-POSTAL-SAISI    PIC X(05).                          EQW9Z0MQ
00253  01  WSS-CODE-POSTAL          PIC X(05).                          EQW9Z0MQ
00254  01  WSS-COMMUNE-SAISIE       PIC X(30).                          EQW9Z0MQ
00255  01  WSS-COMMUNE              PIC X(30).                          EQW9Z0MQ
PDO89  01  WS-LIGACHL               PIC X(26).
00256  01  WSS-MARQUE-VEHI          PIC X(15).                          EQW9Z0MQ
00257  01  WSS-PUISSANCE-VEHI       PIC X(4).                           EQW9Z0MQ
00258  01  WSS-ANCIRC               PIC X(4).                           EQW9Z0MQ
00259  01  WSS-MODELE               PIC X(20).                          EQW9Z0MQ
00260  01  WSS-MODELE-VERSION       PIC X(30).                          EQW9Z0MQ
00261  01  WSS-ZONE-RC-VOL          PIC X(2).                           EQW9Z0MQ
00262  01  WSS-CHANGE-VEHICULE      PIC X(1).                           EQW9Z0MQ
00263  01  WSS-CYLINDREE            PIC 9(05).                          EQW9Z0MQ
00264  01  IND-GTI2                 PIC 9(2).                           EQW9Z0MQ
U4080  01  I-GTI-CODE               PIC 9(2).                           EQW9Z0MQ
00265  01  WSS-CODE-GTI             PIC X(03).                          EQW9Z0MQ
00266  01  WSS-FLAG-GTI             PIC X(03).                          EQW9Z0MQ
00267  01  WSS-GTI-TROUVE           PIC X(01).                          EQW9Z0MQ
00268  01  WSS-FRANCHISE-TROUVE     PIC X(01).                          EQW9Z0MQ
00269  01  WSS-MONTANT-TROUVE       PIC X(01).                          EQW9Z0MQ
00270  01  WSS-CLASSE               PIC X(01).                          EQW9Z0MQ
00271  01  WSS-ANCIENNETE           PIC 9(08).                          EQW9Z0MQ
00272  01  WSS-DATE-JOUR            PIC X(08).                          EQW9Z0MQ
00273  01  WSS-DATE-JOUR-9 REDEFINES WSS-DATE-JOUR PIC 9(8).            EQW9Z0MQ
F3576  01  WSS-DATE-TEST            PIC X(06).                          EQW9Z0MQ
00274  01  WSS-MODIF-CODE-CIE       PIC X.                              EQW9Z0MQ
00275  01  WSS-MODIF-CODE-AUTO      PIC X.                              EQW9Z0MQ
00275  01  WSS-FB-NBRE-VEHI-USA     PIC S99.                            EQW9Z0MQ
00275  01  WSS-USAGE-PROF           PIC XXX.                            EQW9Z0MQ
00276                                                                   EQW9Z0MQ
00277  01  WSS-DATES-FOZONE.                                            EQW9Z0MQ
00278      05 W-EFFET               PIC X(8).                           EQW9Z0MQ
00279      05 WSS-JMSA              PIC 9(8).                           EQW9Z0MQ
00280      05 WSS-JMSA-X            REDEFINES WSS-JMSA.                 EQW9Z0MQ
00281         10 WSS-JMSA-J         PIC X(2).                           EQW9Z0MQ
00282         10 WSS-JMSA-M         PIC X(2).                           EQW9Z0MQ
00283         10 WSS-JMSA-SA        PIC 9(04).                          EQW9Z0MQ
00284         10 WSS-JMSA-SA-X      REDEFINES WSS-JMSA-SA.              EQW9Z0MQ
00285            15 WSS-JMSA-S      PIC X(2).                           EQW9Z0MQ
00286            15 WSS-JMSA-A      PIC X(2).                           EQW9Z0MQ
00287                                                                   EQW9Z0MQ
00288      05 WSS-SAMJ              PIC 9(8).                           EQW9Z0MQ
00289      05 WSS-SAMJ-X            REDEFINES WSS-SAMJ.                 EQW9Z0MQ
00290         10 WSS-SAMJ-SA        PIC 9(04).                          EQW9Z0MQ
00291         10 WSS-SAMJ-SA-X      REDEFINES WSS-SAMJ-SA.              EQW9Z0MQ
00292            15 WSS-SAMJ-S      PIC X(2).                           EQW9Z0MQ
00293            15 WSS-SAMJ-A      PIC X(2).                           EQW9Z0MQ
00294         10 WSS-SAMJ-M         PIC X(2).                           EQW9Z0MQ
00295         10 WSS-SAMJ-J         PIC X(2).                           EQW9Z0MQ
00296                                                                   EQW9Z0MQ
00297  01  WSS-DATE-SAMJ.                                               EQW9Z0MQ
00298      05 WSS-DATE-SAMJ-S       PIC 9(2).                           EQW9Z0MQ
00299      05 WSS-DATE-SAMJ-A       PIC 9(2).                           EQW9Z0MQ
00300      05 WSS-DATE-SAMJ-M       PIC 9(2).                           EQW9Z0MQ
00301      05 WSS-DATE-SAMJ-J       PIC 9(2).                           EQW9Z0MQ
00302                                                                   EQW9Z0MQ
00303  01  WSS-DATE-MOINS-1-AN.                                         EQW9Z0MQ
00304      05 WSS-DATE-S-1-AN     PIC 9(2).                             EQW9Z0MQ
00305      05 WSS-DATE-A-1-AN     PIC 9(2).                             EQW9Z0MQ
00306      05 WSS-DATE-M-1-AN     PIC 9(2).                             EQW9Z0MQ
00307      05 WSS-DATE-J-1-AN     PIC 9(2).                             EQW9Z0MQ
00308                                                                   EQW9Z0MQ
00309  01  WSS-RESIL.                                                   EQW9Z0MQ
00310      05 WSS-RESIL-SSAA        PIC X(4).                           EQW9Z0MQ
00311      05 WSS-RESIL-MM          PIC X(2).                           EQW9Z0MQ
00312                                                                   EQW9Z0MQ
00313  01  WSS-ACQUI.                                                   EQW9Z0MQ
00314      05 WSS-ACQUI-SSAA        PIC X(4).                           EQW9Z0MQ
00315      05 WSS-ACQUI-MM          PIC X(2).                           EQW9Z0MQ
00316                                                                   EQW9Z0MQ
00317  01  WSS-ACQUI2.                                                  EQW9Z0MQ
00318      05 WSS-ACQUI2-MM         PIC X(2).                           EQW9Z0MQ
00319      05 WSS-ACQUI2-SSAA       PIC X(4).                           EQW9Z0MQ
00320                                                                   EQW9Z0MQ
00321  01  WSS-DATE-TRAV            PIC X(6).                           EQW9Z0MQ
00322  01  WSS-DATE50               PIC X(4).                           EQW9Z0MQ
00323  01  WSS-DATE-AFFNO           PIC X(6).                           EQW9Z0MQ
00324  01  WSS-DATE-RESIL           PIC X(6).                           EQW9Z0MQ
00325  01  WSS-DATE-ACQUI           PIC X(6).                           EQW9Z0MQ
00327  01  WSS-VEHCIRD              PIC X(8).                           EQW9Z0MQ
F3576  01  WSS-VEHACQD              PIC X(8).                           EQW9Z0MQ
00328                                                                   EQW9Z0MQ
00329  01  WSS-DATE-JOUR-SAMJ.                                          EQW9Z0MQ
00330      05 WSS-DATE-JOUR-SAMJ-S  PIC 9(2).                           EQW9Z0MQ
00331      05 WSS-DATE-JOUR-SAMJ-A  PIC 9(2).                           EQW9Z0MQ
00332      05 WSS-DATE-JOUR-SAMJ-M  PIC 9(2).                           EQW9Z0MQ
00333      05 WSS-DATE-JOUR-SAMJ-J  PIC 9(2).                           EQW9Z0MQ
00334                                                                   EQW9Z0MQ
00335  01  WSS-FB-DATJOUR.                                              EQW9Z0MQ
00336      05 WSS-FB-DATJOUR-JJ     PIC X(02).                          EQW9Z0MQ
00337      05 WSS-FB-DATJOUR-MM     PIC X(02).                          EQW9Z0MQ
00338      05 WSS-FB-DATJOUR-SS     PIC X(02).                          EQW9Z0MQ
00339      05 WSS-FB-DATJOUR-AA     PIC X(02).                          EQW9Z0MQ
00340                                                                   EQW9Z0MQ
00341  01  WSS-SSAAMMJJ.                                                EQW9Z0MQ
00342      05  WSS-SSAA             PIC X(4).                           EQW9Z0MQ
00343      05  WSS-MM               PIC X(2).                           EQW9Z0MQ
00344      05  WSS-JJ               PIC X(2).                           EQW9Z0MQ
00345                                                                   EQW9Z0MQ
00346  01  WSS-JJMMSSAA.                                                EQW9Z0MQ
00347      05  WSS-JJ               PIC X(2).                           EQW9Z0MQ
00348      05  WSS-MM               PIC X(2).                           EQW9Z0MQ
00349      05  WSS-SSAA             PIC X(4).                           EQW9Z0MQ
00350                                                                   EQW9Z0MQ
00351  01  WSS-ANVBOND-MMSSAA.                                          EQW9Z0MQ
00352      05  WSS-ANVBOND-MM       PIC X(2).                           EQW9Z0MQ
00353      05  WSS-ANVBOND-SS       PIC X(2).                           EQW9Z0MQ
00354      05  WSS-ANVBOND-AA       PIC X(2).                           EQW9Z0MQ
00355                                                                   EQW9Z0MQ
00356  01  WSS-ANVABOD-SSAA.                                            EQW9Z0MQ
00357      05  WSS-ANVABOD-SS       PIC X(2).                           EQW9Z0MQ
00358      05  WSS-ANVABOD-AA       PIC X(2).                           EQW9Z0MQ
00359                                                                   EQW9Z0MQ
00360  01  WSS-DATE-A-VERIFIER.                                         EQW9Z0MQ
00361      05 WSS-DATE-A-VERIFIER-JJ  PIC X(2).                         EQW9Z0MQ
00362      05 WSS-DATE-A-VERIFIER-MM  PIC X(2).                         EQW9Z0MQ
00363      05 WSS-DATE-A-VERIFIER-SS  PIC X(2).                         EQW9Z0MQ
00364      05 WSS-DATE-A-VERIFIER-AA  PIC X(2).                         EQW9Z0MQ
00365                                                                   EQW9Z0MQ
00366  01  WSS-DATE-OK                 PIC X.                           EQW9Z0MQ
00367  01  WSS-APPEL-AIDE-CDVEHI       PIC X VALUE 'N'.                 EQW9Z0MQ
00368  01  WSS-APPEL-AIDE-GENRE        PIC X VALUE 'N'.                 EQW9Z0MQ
00369  01  WSS-APPEL-AIDE-USAGE        PIC X VALUE 'N'.                 EQW9Z0MQ
00370  01  WSS-APPEL-AIDE-PROT         PIC X VALUE 'N'.                 EQW9Z0MQ
00371  01  WSS-APPEL-AIDE-CP           PIC X VALUE 'N'.                 EQW9Z0MQ
00372  01  WSS-APPEL-AIDE-COM          PIC X VALUE 'N'.                 EQW9Z0MQ
00373  01  WSS-APPEL-AIDE-FORM         PIC X VALUE 'N'.                 EQW9Z0MQ
00374  01  WSS-APPEL-AIDE-CIE          PIC X VALUE 'N'.                 EQW9Z0MQ
00374  01  WSS-APPEL-AIDE-CHOIX-ENFANT PIC X VALUE 'N'.                 EQW9Z0MQ
00375  01  WSS-CDCIE-TROUVE            PIC X(1).                        EQW9Z0MQ
00376  01  WSS-NB-POINT-INTERRO        PIC 9(2) VALUE ZERO.             EQW9Z0MQ
00377  01  POS                         PIC 9(02).                       EQW9Z0MQ
00378  01  J                           PIC 9(02).                       EQW9Z0MQ
00379  01  WSS-BLANC-TROUVER           PIC X(01).                       EQW9Z0MQ
00380  01  WSS-CDVEHI-TROUVE           PIC X(01).                       EQW9Z0MQ
00381  01  WSS-GENRE-TROUVE            PIC X(01).                       EQW9Z0MQ
00382  01  WSS-CLASSE-TROUVE           PIC X(01).                       EQW9Z0MQ
00383  01  WSS-GRP-TROUVE              PIC X(01).                       EQW9Z0MQ
00383  01  WSS-ENSEMBLE-2R-ORIGINE     PIC X(02).                       EQW9Z0MQ
00383  01  WSS-ENSEMBLE-2R-CHANGER     PIC X(02).                       EQW9Z0MQ
       01  WSS-FIN-VEHI                PIC X.
00224  01  WSS-FIN-PERS                PIC X.                           EQW92BG2
       01  WSS-ORDRE-PERS              PIC 9(03).
       01  RWSS-ORDRE-PERS REDEFINES WSS-ORDRE-PERS PIC X(03).
36835  01  WSS-NOM                     PIC X(20).
36835  01  WSS-PRENOM                  PIC X(20).
F52844 01  WSS-STATUT                  PIC X(02).
36835  01  WSS-TAMPON                  PIC X(20).
36835  01  WSS-NOM-CG                  PIC X(20).
36835  01  WSS-PRENOM-CG               PIC X(20).
AD548  01  WSS-CTRL-ANTECEDENT         PIC X(03).
00384 *                                                                 EQW9Z0MQ
F8556  01 WSS-VEHUSAC.
F8556      05 DEB-VEHUSAC           PIC X(1).
F8556      05 FIN-VEHUSAC           PIC X(2).

00385 *    SAUVEGARDE EN WORKING DE LA TS-VEHICULE DU VEHICULE TRAITE   EQW9Z0MQ
00386  01 WSS-TS-VEHICULE.                                              EQW9Z0MQ
00387  ++INCLUDE FBIVEHI                                                EQW9Z0MQ
00388 *                                                                 EQW9Z0MQ
00389 *    SAUVEGARDE EN WORKING DU RANG DE LA TS-VEHICULE DU VEHICULE  EQW9Z0MQ
00390 *    TRAITE                                                       EQW9Z0MQ
00391  01 WSS-RANG-TS-LIRE                 PIC S9(4) COMP.              EQW9Z0MQ
00392 *                                                                 EQW9Z0MQ
00391  01 WSS-RANG-TS-CHANGER              PIC S9(4) COMP.              EQW9Z0MQ
00392 *                                                                 EQW9Z0MQ
00393 *    DATE D'ACQUISITION DU BONUS 50 LA PLUS ANCIENNE              EQW9Z0MQ
00394  01 WSS-DATE50-MIN                   PIC X(08).                   EQW9Z0MQ
00395 *                                                                 EQW9Z0MQ
00396  01 WSS-RECH-COEFTECH.                                            EQW9Z0MQ
00397      05 WSS-COE-TYPE                 PIC X(01).                   EQW9Z0MQ
00398      05 I-OCCURS                     PIC S9(4) COMP.              EQW9Z0MQ
00399      05 I-COE-TYPE                   PIC S9(4) COMP.              EQW9Z0MQ
00400 *                                                                 EQW9Z0MQ
00401 *    NB MAX DE COEFFICIENTS VEHICULE                              EQW9Z0MQ
00402 *                                                                 EQW9Z0MQ
00403      05 NB-COEV-MAX                  PIC 9(2)                     EQW9Z0MQ
00404                                      VALUE 10.                    EQW9Z0MQ
00405 *                                                                 EQW9Z0MQ
00188  01  I                               PIC S9(4) COMP.              EQW92BG2
00188  01  I-RANG                          PIC S9(4) COMP.              EQW92BG2
00406                                                                   EQW9Z0MQ
00407  01  OK-ZONIER                PIC X.                              EQW9Z0MQ
00408                                                                   EQW9Z0MQ
00409  01  WSS-INDICATEUR-ETAT.                                         EQW9Z0MQ
00410      05 WSS-ETAT-TABLE        PIC X(01).                          EQW9Z0MQ
00411         88 POSTE-PAS-TROUVE   VALUE '0'.                          EQW9Z0MQ
00412         88 POSTE-TROUVE       VALUE '1'.                          EQW9Z0MQ
00413         88 DEBUT-TABLE        VALUE '8'.                          EQW9Z0MQ
00414         88 PROBLEME-TABLE     VALUE '9'.                          EQW9Z0MQ
00415                                                                   EQW9Z0MQ
00416  01  PB-DETERMINATION         PIC  XXX.                           EQW9Z0MQ
00417      88 YA-PB-DETERMINATION   VALUE 'OUI'.                        EQW9Z0MQ
00418      88 YA-PAS-PB-DETERMINATION VALUE 'NON'.                      EQW9Z0MQ
00419                                                                   EQW9Z0MQ
00420 * INDICATEUR DE MODIF DE LIBELLE ET CODE COMPAGNIE                EQW9Z0MQ
00421  01  MODIF-LIB                PIC   X.                            EQW9Z0MQ
00422      88 MODIF-LIB-OUI         VALUE 'O'.                          EQW9Z0MQ
00423      88 MODIF-LIB-NON         VALUE 'N'.                          EQW9Z0MQ
00424                                                                   EQW9Z0MQ
00425  01  MODIF-CIE                PIC   X.                            EQW9Z0MQ
00426      88 MODIF-CIE-OUI         VALUE 'O'.                          EQW9Z0MQ
00427      88 MODIF-CIE-NON         VALUE 'N'.                          EQW9Z0MQ
00428                                                                   EQW9Z0MQ
      * INDICATEUR DE RECHERCHE DE PERSONNE
       01  TOP-RECHERCHE-PERSONNE   PIC   X.
           88 PERSONNE-TROUVE       VALUE 'O'.
           88 PERSONNE-NON-TROUVE   VALUE 'N'.

36835  01  TOP-RECHERCHE-TITULAIRE  PIC   X.
36835      88 TITULAIRE-TROUVE      VALUE 'O'.
36835      88 TITULAIRE-NON-TROUVE  VALUE 'N'.

00429 * ZONES TRAVAIL POUR CONTROLE D'ACCES                             EQW9Z0MQ
00430  01  Z-CONTROLE-ACCES.                                            EQW9Z0MQ
00431      05  Z-CODE-REGIME-AS     PIC X(05).                          EQW9Z0MQ
00432      05  Z-CODE-REGIME-CA     PIC X(05).                          EQW9Z0MQ
00433      05  Z-COMPTEUR-TS        PIC S9(2) COMP.                     EQW9Z0MQ
00434      05  Z-AFFICHAGE-SELECTIF PIC X(03).                          EQW9Z0MQ
00435      05  Z-AFFICHER-OPTION    PIC X(03).                          EQW9Z0MQ
00436 *                                                                 EQW9Z0MQ
33295  01  WS-FORCAGE              PIC X.

00437 *---TABLE FOZONE01                                                EQW9Z0MQ
00438  ++INCLUDE CCFOZONE                                               EQW9Z0MQ
00439                                                                   EQW9Z0MQ
00440 *--- TABLE MIAUTO01                                               EQW9Z0MQ
00441  ++INCLUDE CCMIAUTO                                               EQW9Z0MQ
00442  01  WSS-CLE-MIAUTO.                                              EQW9Z0MQ
00443      05  CLE-CDAUTO PIC X(7).                                     EQW9Z0MQ
00444                                                                   EQW9Z0MQ
00445 *---TABLE FVVEHI01                                                EQW9Z0MQ
00446  01 CCFVVEHI.                                                     EQW9Z0MQ
00447  ++INCLUDE CCFVVEHI                                               EQW9Z0MQ
00448                                                                   EQW9Z0MQ
00449 *--- TABLE FBGENR01                                               EQW9Z0MQ
00450  ++INCLUDE CCFBGENR                                               EQW9Z0MQ
00451  01  WSS-CLE-FBGENR.                                              EQW9Z0MQ
00452      05 CLE-GENTYPV PIC X(3).                                     EQW9Z0MQ
00453      05 CLE-GENRE   PIC X(1).                                     EQW9Z0MQ
00454                                                                   EQW9Z0MQ
00455 *--- TABLE FBUSAP01                                               EQW9Z0MQ
00456  ++INCLUDE CCFBUSAP                                               EQW9Z0MQ
00457  01  WSS-CLE-FBUSAP.                                              EQW9Z0MQ
00458      05 CLE-USATYPV PIC X(3).                                     EQW9Z0MQ
00459      05 CLE-USAGE   PIC X(3).                                     EQW9Z0MQ
00460                                                                   EQW9Z0MQ
00461 *--- TABLE FBPROT01                                               EQW9Z0MQ
00462  ++INCLUDE CCFBPROT                                               EQW9Z0MQ
00463  01  WSS-CLE-FBPROT.                                              EQW9Z0MQ
00464      05 CLE-PRTTYPV PIC X(3).                                     EQW9Z0MQ
00465      05 CLE-NIVEAU  PIC X(1).                                     EQW9Z0MQ
00466                                                                   EQW9Z0MQ
00467 *--- TABLE FBFORM01                                               EQW9Z0MQ
00468  ++INCLUDE CCFBFORM                                               EQW9Z0MQ
00469  01  WSS-CLE-FBFORM.                                              EQW9Z0MQ
00470      05 CLE-FORTYPV PIC X(3).                                     EQW9Z0MQ
00471      05 CLE-FORMULE PIC X(2).                                     EQW9Z0MQ
00472                                                                   EQW9Z0MQ
00473 *--- TABLE GECGTA99                                               EQW9Z0MQ
00474  ++INCLUDE CCGECGTA                                               EQW9Z0MQ
00475                                                                   EQW9Z0MQ
00476 *--- TABLE FB2CLA                                                 EQW9Z0MQ
00477  ++INCLUDE CCFB2CLA                                               EQW9Z0MQ
00478  01  WSS-CLE-FB2CLA.                                              EQW9Z0MQ
00479      05  WSS-CLE-GENRE-FB2CLA PIC X.                              EQW9Z0MQ
00480      05  WSS-CLE-VAL-FB2CLA   PIC 9(11).                          EQW9Z0MQ
00481                                                                   EQW9Z0MQ
00482 *--- TABLE FB2GRP                                                 EQW9Z0MQ
00483  ++INCLUDE CCFB2GRP                                               EQW9Z0MQ
00484  01  WSS-CLE-FB2GRP.                                              EQW9Z0MQ
00485      05  WSS-CLE-GENRE-FB2GRP PIC X.                              EQW9Z0MQ
00486      05  WSS-CLE-CYL-FB2GRP   PIC 9(5).                           EQW9Z0MQ
00487                                                                   EQW9Z0MQ
00488 *--- TABLE FB4CLA                                                 EQW9Z0MQ
00489  ++INCLUDE CCFB4CLA                                               EQW9Z0MQ
00490  01  WSS-CLE-FB4CLA.                                              EQW9Z0MQ
00491      05  WSS-CLE-GENRE-FB4CLA PIC X.                              EQW9Z0MQ
00492      05  WSS-CLE-VAL-FB4CLA   PIC 9(11).                          EQW9Z0MQ
00493                                                                   EQW9Z0MQ
00494 *--- TABLE FB4GRP                                                 EQW9Z0MQ
00495  ++INCLUDE CCFB4GRP                                               EQW9Z0MQ
00496  01  WSS-CLE-FB4GRP.                                              EQW9Z0MQ
00497      05  WSS-CLE-GENRE-FB4GRP PIC X.                              EQW9Z0MQ
00498      05  WSS-CLE-CYL-FB4GRP   PIC 9(5).                           EQW9Z0MQ
00499                                                                   EQW9Z0MQ
00500 *---TABLE FBFOGA01                                                EQW9Z0MQ
00501  ++INCLUDE CCFBFOGA                                               EQW9Z0MQ
00502  01  WSS-CLE-FBFOGA.                                              EQW9Z0MQ
00503      05 CLE-FOGTYVC PIC X(3).                                     EQW9Z0MQ
00504      05 CLE-FOGFORC PIC X(2).                                     EQW9Z0MQ
00505      05 CLE-FOGGTIC PIC X(3).                                     EQW9Z0MQ
00506                                                                   EQW9Z0MQ
F41702*--- TABLE FB4FRM01                                               EQW9Z0MQ
F41702 ++INCLUDE CCFB4FRM                                               EQW9Z0MQ
F41702 01  WSS-CLE-FB4FRM.                                              EQW9Z0MQ
F41702     05 CLE-FM4PRMC PIC X(4).                                     EQW9Z0MQ
F41702     05 CLE-FM4TYPE PIC X(3).                                     EQW9Z0MQ
F41702     05 CLE-FM4CLAC PIC X(1).                                     EQW9Z0MQ
F41702     05 CLE-FM4GARA PIC X(3).                                     EQW9Z0MQ
F41702     05 CLE-FM4NIVF PIC X(1).                                     EQW9Z0MQ
00513                                                                   EQW9Z0MQ
00522 *--- TABLE FB2FRA01                                               EQW9Z0MQ
00523  ++INCLUDE CCFB2FRA                                               EQW9Z0MQ
00524  01  WSS-CLE-FB2FRA.                                              EQW9Z0MQ
00525      05 CLE-FR2PRMC PIC X(4).                                     EQW9Z0MQ
00526      05 CLE-FR2GTIC PIC X(3).                                     EQW9Z0MQ
00527      05 CLE-FR2CLAC PIC X(1).                                     EQW9Z0MQ
00528      05 CLE-FR2PRTC PIC X(1).                                     EQW9Z0MQ
00529                                                                   EQW9Z0MQ
00530 *--- TABLE FBMTLT01                                               EQW9Z0MQ
00531  ++INCLUDE CCFBMTLT                                               EQW9Z0MQ
00532  01  WSS-CLE-FBMTLT.                                              EQW9Z0MQ
00533      05 CLE-MTLTYVC PIC X(3).                                     EQW9Z0MQ
00534      05 CLE-MTLGTIC PIC X(3).                                     EQW9Z0MQ
00535                                                                   EQW9Z0MQ
00536 *--- TABLE FB4BDG01                                               EQW9Z0MQ
00537  ++INCLUDE CCFB4BDG                                               EQW9Z0MQ
00538  01  WSS-CLE-FB4BDG.                                              EQW9Z0MQ
00539      05 CLE-BD4PRMC PIC X(4).                                     EQW9Z0MQ
00540      05 CLE-BD4CLAC PIC X(1).                                     EQW9Z0MQ
00541                                                                   EQW9Z0MQ
F41702*--- TABLE FB4FVM01                                               EQW9Z0MQ
F41702 ++INCLUDE CCFB4FVM                                               EQW9Z0MQ
F41702 01  WSS-CLE-FB4FVM.                                              EQW9Z0MQ
00545      05 CLE-FV4PRMC PIC X(4).                                     EQW9Z0MQ
00546      05 CLE-FV4CLAC PIC X(1).                                     EQW9Z0MQ
F41702     05 CLE-FV4TARI PIC X(1).                                     EQW9Z0MQ
00547                                                                   EQW9Z0MQ
F41702*--- TABLE FB4FDM01                                               EQW9Z0MQ
F41702 ++INCLUDE CCFB4FDM                                               EQW9Z0MQ
F41702 01  WSS-CLE-FB4FDM.                                              EQW9Z0MQ
00551      05 CLE-FD4PRMC PIC X(4).                                     EQW9Z0MQ
00552      05 CLE-FD4CLAC PIC X(1).                                     EQW9Z0MQ
F41702     05 CLE-FD4TARI PIC X(1).                                     EQW9Z0MQ
00553                                                                   EQW9Z0MQ
00554 ***************************************************************** EQW9Z0MQ
00555 *   DESCRIPTION DES ZONES POUR ACCES AUX TABLES DB2             * EQW9Z0MQ
00556 ***************************************************************** EQW9Z0MQ
00557 *                                                                 EQW9Z0MQ
00558      EXEC SQL                                                     EQW9Z0MQ
00559           INCLUDE SQLCA                                           EQW9Z0MQ
00560      END-EXEC.                                                    EQW9Z0MQ
00561 *                                                                 EQW9Z0MQ
00562      EXEC SQL                                                     EQW9Z0MQ
00563           INCLUDE GEBCOMM                                         EQW9Z0MQ
00564      END-EXEC.                                                    EQW9Z0MQ
00565                                                                   EQW9Z0MQ
00566 *---GESTION DES ABANDONS ET INCLUDE DE DCLGEN                     EQW9Z0MQ
00567  01 WMESS-DB2.                                                    EQW9Z0MQ
00568     03 WDB2-MESS.                                                 EQW9Z0MQ
00569        05 WDB2-MESS1   PIC X(03).                                 EQW9Z0MQ
00570        05 WDB2-MESS2   PIC X(01).                                 EQW9Z0MQ
00571        05 WDB2-MESS3   PIC X(45).                                 EQW9Z0MQ
00572     03 FILLER          PIC X VALUE SPACES.                        EQW9Z0MQ
00573     03 WDB2-CODE       PIC --------9.                             EQW9Z0MQ
00574                                                                   EQW9Z0MQ
00575 ***************************************************************** EQW9Z0MQ
00576 *   ZONES GENERALES OBLIGATOIRES                                * EQW9Z0MQ
00577 ***************************************************************** EQW9Z0MQ
00578 *   ZONES DE TEST DU CODE-RETOUR CICS  :  EIBRCODE                EQW9Z0MQ
00579  ++INCLUDE SQKWEIB0                                               EQW9Z0MQ
00580 ***************************************************************** EQW9Z0MQ
00581 * ZONES DATE/HEURE ET NOM DE TERMINAL/CODE TRANSACTION            EQW9Z0MQ
00582 ***************************************************************** EQW9Z0MQ
00583  ++INCLUDE SQKWDATH                                               EQW9Z0MQ
00584 ***************************************************************** EQW9Z0MQ
00585 * ZONES BMS     (TOUCHES FONCTION ET ATTRIBUTS)                 * EQW9Z0MQ
00586 ***************************************************************** EQW9Z0MQ
00587 *                                                               * EQW9Z0MQ
00588 *                 DEFINITION DES ATTRIBUTS                      * EQW9Z0MQ
00589 *         MDT ON                             MDT OFF            * EQW9Z0MQ
00590 *                                                               * EQW9Z0MQ
00591 *        BRT-ALP-FSET                        BRT-ALP            * EQW9Z0MQ
00592 *        BRT-NUM-FSET                        BRT-NUM            * EQW9Z0MQ
00593 *        BRT-PRO-FSET                        BRT-PRO            * EQW9Z0MQ
00594 *        DRK-ALP-FSET                        DRK-ALP            * EQW9Z0MQ
00595 *        DRK-PRO-FSET                        DRK-PRO            * EQW9Z0MQ
00596 *        DRK-PRO-RSET                        DRK-RSET           * EQW9Z0MQ
00597 *        NOR-ALP-FSET                        NOR-ALP            * EQW9Z0MQ
00598 *        NOR-NUM-FSET                        NOR-NUM            * EQW9Z0MQ
00599 *        NOR-PRO-FSET                        NOR-PRO            * EQW9Z0MQ
00600 ***************************************************************** EQW9Z0MQ
00601 *300890    ++INCLUDE SQKWECRA  A: SANS PF9,  B: AVEC  PF9         EQW9Z0MQ
00602  ++INCLUDE SQKWECRA                                               EQW9Z0MQ
00603 ***************************************************************** EQW9Z0MQ
00604 *   ZONES DE CONTROLE ET DE TRAITEMENT SPECIFIQUES              * EQW9Z0MQ
00605 ***************************************************************** EQW9Z0MQ
00606 *                                                                 EQW9Z0MQ
00607 ***************************************************************** EQW9Z0MQ
00608 *   LONGUEUR DE LA COMMAREA                                     * EQW9Z0MQ
00609 ***************************************************************** EQW9Z0MQ
00610  01  COM-GENE-LONG-COMMAREA           PIC S9(4) COMP VALUE +4096. EQW9Z0MQ
00611 *                                                                 EQW9Z0MQ
00612 ***************************************************************** EQW9Z0MQ
00613 *   ZONES DE COMMAREA POUR APPEL A SPITAB                       * EQW9Z0MQ
00614 ***************************************************************** EQW9Z0MQ
00615  01  XSPIPARM.                                                    EQW9Z0MQ
00616  ++INCLUDE SPIPARTP                                               EQW9Z0MQ
00617 /                                                                 EQW9Z0MQ
00618 ***************************************************************** EQW9Z0MQ
00619 *   MODULE K200LDAT :GESTION DES DATES                          * EQW9Z0MQ
00620 ***************************************************************** EQW9Z0MQ
00621  01  K2COM-DATES.                                                 EQW9Z0MQ
00622  ++INCLUDE K2IWDATE                                               EQW9Z0MQ
00623                                                                   EQW9Z0MQ
00624 ***************************************************************** EQW9Z0MQ
00625 *   ZONES SPECIFIQUES A L'APPEL DU PROGRAMME DE CADRAGE         * EQW9Z0MQ
00626 ***************************************************************** EQW9Z0MQ
00627  ++INCLUDE CCMTCADR.                                              EQW9Z0MQ
00628  01  LONG-XKMTCADR PIC S9(4) COMP VALUE +60.                      EQW9Z0MQ
00629                                                                   EQW9Z0MQ
00630 *                                                                 EQW9Z0MQ
00631 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* EQW9Z0MQ
00632 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* EQW9Z0MQ
00633 *   COMMAREA GENERALE DES APPLICATIONS CONCORDE ( SQKWCOMM )    * EQW9Z0MQ
00634 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* EQW9Z0MQ
00635 *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-* EQW9Z0MQ
00636  ++INCLUDE SQKWCOMM                                               EQW9Z0MQ
00637  ++INCLUDE MAICOMM                                                EQW9Z0MQ
00638  ++INCLUDE FBICOMM                                                EQW9Z0MQ
00639 *                                                                 EQW9Z0MQ
00640 ***************************************************************** EQW9Z0MQ
00641 *    ZONES DE LA MAP  FB04M0                                      EQW9Z0MQ
00642 ***************************************************************** EQW9Z0MQ
00643  01  FILLER  PIC X(16) VALUE '*** MAP FB04 ***'.                  EQW9Z0MQ
00644 *                                                                 EQW9Z0MQ
00645  01  Z-MAP.                                                       EQW9Z0MQ
00646 *                                                                 EQW9Z0MQ
00647  ++INCLUDE FB04M0Z                                                EQW9Z0MQ
00648 *                                                                 EQW9Z0MQ
00649 ***************************************************************** EQW9Z0MQ
00650 *     DESCRIPTION DE LA TS-ECRAN   MDT OFF                        EQW9Z0MQ
00651 ***************************************************************** EQW9Z0MQ
00652  ++INCLUDE SQKWTSMA                                               EQW9Z0MQ
00653      02  TS-FB04M00O REDEFINES ZONE-TS-ECRAN.                     EQW9Z0MQ
00654          10 FILLER PIC X(12).                                     EQW9Z0MQ
00655          10 TS-ECR-XTRMTRACL   COMP PIC S9(4).                    EQW9Z0MQ
00656          10 TS-ECR-XTRMTRACA   PIC X.                             EQW9Z0MQ
00657          10 TS-ECR-XTRMTRACO   PIC X(9).                          EQW9Z0MQ
00658          10 TS-ECR-XAPPLILL    COMP PIC S9(4).                    EQW9Z0MQ
00659          10 TS-ECR-XAPPLILA    PIC X.                             EQW9Z0MQ
00660          10 TS-ECR-XAPPLILO    PIC X(30).                         EQW9Z0MQ
00661          10 TS-ECR-XJOURDL     COMP PIC S9(4).                    EQW9Z0MQ
00662          10 TS-ECR-XJOURDA     PIC X.                             EQW9Z0MQ
00663          10 TS-ECR-XJOURDO     PIC X(8).                          EQW9Z0MQ
00664          10 TS-ECR-XRACFLL     COMP PIC S9(4).                    EQW9Z0MQ
00665          10 TS-ECR-XRACFLA     PIC X.                             EQW9Z0MQ
00666          10 TS-ECR-XRACFLO     PIC X(15).                         EQW9Z0MQ
00667          10 TS-ECR-XHEUREDL    COMP PIC S9(4).                    EQW9Z0MQ
00668          10 TS-ECR-XHEUREDA    PIC X.                             EQW9Z0MQ
00669          10 TS-ECR-XHEUREDO    PIC X(8).                          EQW9Z0MQ
00670          10 TS-ECR-GESCLIL     COMP PIC S9(4).                    EQW9Z0MQ
00671          10 TS-ECR-GESCLIA     PIC X.                             EQW9Z0MQ
00672          10 TS-ECR-GESCLIO     PIC X(11).                         EQW9Z0MQ
00673          10 TS-ECR-RAICL       COMP PIC S9(4).                    EQW9Z0MQ
00674          10 TS-ECR-RAICA       PIC X.                             EQW9Z0MQ
00675          10 TS-ECR-RAICO       PIC X(3).                          EQW9Z0MQ
00676          10 TS-ECR-NOMCL       COMP PIC S9(4).                    EQW9Z0MQ
00677          10 TS-ECR-NOMCA       PIC X.                             EQW9Z0MQ
00678          10 TS-ECR-NOMCO       PIC X(31).                         EQW9Z0MQ
36835          10 TS-ECR-VEHCGNCL    COMP PIC S9(4).                    EQW9Z0MQ
36835          10 TS-ECR-VEHCGNCA    PIC X.                             EQW9Z0MQ
36835          10 TS-ECR-VEHCGNCO    PIC X(20).                         EQW9Z0MQ
36835          10 TS-ECR-VEHCGPCL    COMP PIC S9(4).                    EQW9Z0MQ
36835          10 TS-ECR-VEHCGPCA    PIC X.                             EQW9Z0MQ
36835          10 TS-ECR-VEHCGPCO    PIC X(20).                         EQW9Z0MQ
36835          10 TS-ECR-VEHCGSCL    COMP PIC S9(4).                    EQW9Z0MQ
36835          10 TS-ECR-VEHCGSCA    PIC X.                             EQW9Z0MQ
36835          10 TS-ECR-VEHCGSCO    PIC X(2).                          EQW9Z0MQ
00679          10 TS-ECR-VEHCODCL    COMP PIC S9(4).                    EQW9Z0MQ
00680          10 TS-ECR-VEHCODCA    PIC X.                             EQW9Z0MQ
00681          10 TS-ECR-VEHCODCO    PIC X(7).                          EQW9Z0MQ
00682          10 TS-ECR-VEHTYPCL    COMP PIC S9(4).                    EQW9Z0MQ
00683          10 TS-ECR-VEHTYPCA    PIC X.                             EQW9Z0MQ
00684          10 TS-ECR-VEHTYPCO    PIC X(3).                          EQW9Z0MQ
00685          10 TS-ECR-VEHGENCL    COMP PIC S9(4).                    EQW9Z0MQ
00686          10 TS-ECR-VEHGENCA    PIC X.                             EQW9Z0MQ
00687          10 TS-ECR-VEHGENCO    PIC X.                             EQW9Z0MQ
00688          10 TS-ECR-VEHMARLL    COMP PIC S9(4).                    EQW9Z0MQ
00689          10 TS-ECR-VEHMARLA    PIC X.                             EQW9Z0MQ
00690          10 TS-ECR-VEHMARLO    PIC X(15).                         EQW9Z0MQ
00691          10 TS-ECR-VEHMODLL    COMP PIC S9(4).                    EQW9Z0MQ
00692          10 TS-ECR-VEHMODLA    PIC X.                             EQW9Z0MQ
00693          10 TS-ECR-VEHMODLO    PIC X(30).                         EQW9Z0MQ
00694          10 TS-ECR-VEHCYLNL    COMP PIC S9(4).                    EQW9Z0MQ
00695          10 TS-ECR-VEHCYLNA    PIC X.                             EQW9Z0MQ
00696          10 TS-ECR-VEHCYLNO    PIC X(5).                          EQW9Z0MQ
00697          10 TS-ECR-VEHUSACL    COMP PIC S9(4).                    EQW9Z0MQ
00698          10 TS-ECR-VEHUSACA    PIC X.                             EQW9Z0MQ
00699          10 TS-ECR-VEHUSACO    PIC X(3).                          EQW9Z0MQ
00700          10 TS-ECR-VEHIMMXL    COMP PIC S9(4).                    EQW9Z0MQ
00701          10 TS-ECR-VEHIMMXA    PIC X.                             EQW9Z0MQ
00702          10 TS-ECR-VEHIMMXO    PIC X(10).                         EQW9Z0MQ
00703          10 TS-ECR-VEHCIRDL    COMP PIC S9(4).                    EQW9Z0MQ
00704          10 TS-ECR-VEHCIRDA    PIC X.                             EQW9Z0MQ
00705          10 TS-ECR-VEHCIRDO    PIC X(6).                          EQW9Z0MQ
F3576          10 TS-ECR-VEHACQDL    COMP PIC S9(4).                    EQW9Z0MQ
F3576          10 TS-ECR-VEHACQDA    PIC X.                             EQW9Z0MQ
F3576          10 TS-ECR-VEHACQDO    PIC X(6).                          EQW9Z0MQ
00706          10 TS-ECR-VEHVALML    COMP PIC S9(4).                    EQW9Z0MQ
00707          10 TS-ECR-VEHVALMA    PIC X.                             EQW9Z0MQ
00708          10 TS-ECR-VEHVALMO    PIC X(7).                          EQW9Z0MQ
F3215          10 TS-ECR-VEHGROCL    COMP PIC S9(4).
F3215          10 TS-ECR-VEHGROCA    PIC X.
F3215          10 TS-ECR-VEHGROCO    PIC X(2).
F3215          10 TS-ECR-VEHCLACL    COMP PIC S9(4).
F3215          10 TS-ECR-VEHCLACA    PIC X.
F3215          10 TS-ECR-VEHCLACO    PIC X(1).
00709          10 TS-ECR-VEHPRTCL    COMP PIC S9(4).                    EQW9Z0MQ
00710          10 TS-ECR-VEHPRTCA    PIC X.                             EQW9Z0MQ
00711          10 TS-ECR-VEHPRTCO    PIC X.                             EQW9Z0MQ
00712          10 TS-ECR-GARCODCL    COMP PIC S9(4).                    EQW9Z0MQ
00713          10 TS-ECR-GARCODCA    PIC X.                             EQW9Z0MQ
00714          10 TS-ECR-GARCODCO    PIC X.                             EQW9Z0MQ
00715          10 TS-ECR-VEHPOSCL    COMP PIC S9(4).                    EQW9Z0MQ
00716          10 TS-ECR-VEHPOSCA    PIC X.                             EQW9Z0MQ
00717          10 TS-ECR-VEHPOSCO    PIC X.                             EQW9Z0MQ
00718          10 TS-ECR-VEHPEFCL    COMP PIC S9(4).                    EQW9Z0MQ
00719          10 TS-ECR-VEHPEFCA    PIC X.                             EQW9Z0MQ
00720          10 TS-ECR-VEHPEFCO    PIC X.                             EQW9Z0MQ
00721          10 TS-ECR-GARCOPCL    COMP PIC S9(4).                    EQW9Z0MQ
00722          10 TS-ECR-GARCOPCA    PIC X.                             EQW9Z0MQ
00723          10 TS-ECR-GARCOPCO    PIC X(5).                          EQW9Z0MQ
00724          10 TS-ECR-GARVILLL    COMP PIC S9(4).                    EQW9Z0MQ
00725          10 TS-ECR-GARVILLA    PIC X.                             EQW9Z0MQ
00726          10 TS-ECR-GARVILLO    PIC X(30).                         EQW9Z0MQ
00727          10 TS-ECR-ANVREPCL    COMP PIC S9(4).                    EQW9Z0MQ
00728          10 TS-ECR-ANVREPCA    PIC X.                             EQW9Z0MQ
00729          10 TS-ECR-ANVREPCO    PIC X.                             EQW9Z0MQ
00730          10 TS-ECR-VEHFORCL    COMP PIC S9(4).                    EQW9Z0MQ
00731          10 TS-ECR-VEHFORCA    PIC X.                             EQW9Z0MQ
00732          10 TS-ECR-VEHFORCO    PIC X(2).                          EQW9Z0MQ
00733          10 TS-ECR-ANVNUMXL    COMP PIC S9(4).                    EQW9Z0MQ
00734          10 TS-ECR-ANVNUMXA    PIC X.                             EQW9Z0MQ
00735          10 TS-ECR-ANVNUMXO    PIC X(15).                         EQW9Z0MQ
00697          10 TS-ECR-ANVSOUSL    COMP PIC S9(4).                    EQW90UNO
00698          10 TS-ECR-ANVSOUSA    PIC X.                             EQW90UNO
00699          10 TS-ECR-ANVSOUSO    PIC X(04).                         EQW90UNO
00697          10 TS-ECR-ANVSOUNL    COMP PIC S9(4).                    EQW90UNO
00698          10 TS-ECR-ANVSOUNA    PIC X.                             EQW90UNO
00699          10 TS-ECR-ANVSOUNO    PIC X(03).                         EQW90UNO
00736          10 TS-ECR-ANVCIEXL    COMP PIC S9(4).                    EQW9Z0MQ
00737          10 TS-ECR-ANVCIEXA    PIC X.                             EQW9Z0MQ
00738          10 TS-ECR-ANVCIEXO    PIC X(3).                          EQW9Z0MQ
00739          10 TS-ECR-ANVCIELL    COMP PIC S9(4).                    EQW9Z0MQ
00740          10 TS-ECR-ANVCIELA    PIC X.                             EQW9Z0MQ
00741          10 TS-ECR-ANVCIELO    PIC X(35).                         EQW9Z0MQ
00742          10 TS-ECR-ANVANCNL    COMP PIC S9(4).                    EQW9Z0MQ
00743          10 TS-ECR-ANVANCNA    PIC X.                             EQW9Z0MQ
00744          10 TS-ECR-ANVANCNO    PIC X(2).                          EQW9Z0MQ
00745          10 TS-ECR-ANVINTNL    COMP PIC S9(4).                    EQW9Z0MQ
00746          10 TS-ECR-ANVINTNA    PIC X.                             EQW9Z0MQ
00747          10 TS-ECR-ANVINTNO    PIC X(2).                          EQW9Z0MQ
00748          10 TS-ECR-ANVRESDL    COMP PIC S9(4).                    EQW9Z0MQ
00749          10 TS-ECR-ANVRESDA    PIC X.                             EQW9Z0MQ
00750          10 TS-ECR-ANVRESDO    PIC X(6).                          EQW9Z0MQ
00751          10 TS-ECR-SIVINDCL    COMP PIC S9(4).                    EQW9Z0MQ
00752          10 TS-ECR-SIVINDCA    PIC X.                             EQW9Z0MQ
00753          10 TS-ECR-SIVINDCO    PIC X.                             EQW9Z0MQ
00748          10 TS-ECR-ANVMTRCL    COMP PIC S9(4).                    EQW9Z0MQ
00749          10 TS-ECR-ANVMTRCA    PIC X.                             EQW9Z0MQ
00750          10 TS-ECR-ANVMTRCO    PIC X.                             EQW9Z0MQ
00754          10 TS-ECR-ANVBONTL    COMP PIC S9(4).                    EQW9Z0MQ
00755          10 TS-ECR-ANVBONTA    PIC X.                             EQW9Z0MQ
00756          10 TS-ECR-ANVBONTO    PIC X(3).                          EQW9Z0MQ
00757          10 TS-ECR-ANVBONDL    COMP PIC S9(4).                    EQW9Z0MQ
00758          10 TS-ECR-ANVBONDA    PIC X.                             EQW9Z0MQ
00759          10 TS-ECR-ANVBONDO    PIC X(6).                          EQW9Z0MQ
00760          10 TS-ECR-ANVABODL    COMP PIC S9(4).                    EQW9Z0MQ
00761          10 TS-ECR-ANVABODA    PIC X.                             EQW9Z0MQ
00762          10 TS-ECR-ANVABODO    PIC X(4).                          EQW9Z0MQ
00763          10 TS-ECR-XCDECL      COMP PIC S9(4).                    EQW9Z0MQ
00764          10 TS-ECR-XCDECA      PIC X.                             EQW9Z0MQ
00765          10 TS-ECR-XCDECO      PIC X(9).                          EQW9Z0MQ
00766          10 TS-ECR-XMSGILL     COMP PIC S9(4).                    EQW9Z0MQ
00767          10 TS-ECR-XMSGILA     PIC X.                             EQW9Z0MQ
00768          10 TS-ECR-XMSGILO     PIC X(59).                         EQW9Z0MQ
00769          10 TS-ECR-XMSGALL     COMP PIC S9(4).                    EQW9Z0MQ
00770          10 TS-ECR-XMSGALA     PIC X.                             EQW9Z0MQ
00771          10 TS-ECR-XMSGALO     PIC X(79).                         EQW9Z0MQ
00772 *                                                                 EQW9Z0MQ
00773 ***************************************************************** EQW9Z0MQ
00774 *     ZONE BUFFER D'ENTREE/SORTIE                                 EQW9Z0MQ
00775 ***************************************************************** EQW9Z0MQ
00776 *                                                                 EQW9Z0MQ
00777 *                                                                 EQW9Z0MQ
00778 ***************************************************************** EQW9Z0MQ
00779 * ZONE D'INTERFACE POUR LA GESTION DES ERREURS NON RECOUVRABLES   EQW9Z0MQ
00780 ***************************************************************** EQW9Z0MQ
00781  ++INCLUDE SQKWERRO                                               EQW9Z0MQ
00782 ***************************************************************** EQW9Z0MQ
00783 *  ZONES DE GESTION DES FICHIERS                                  EQW9Z0MQ
00784 ***************************************************************** EQW9Z0MQ
00785 *                                                                 EQW9Z0MQ
00786 /                                                                 EQW9Z0MQ
00787 ***************************************************************** EQW9Z0MQ
00788 ***************************************************************** EQW9Z0MQ
00789 **********************  LINKAGE SECTION ************************* EQW9Z0MQ
00790 ***************************************************************** EQW9Z0MQ
00791 ***************************************************************** EQW9Z0MQ
00792 *                                                                 EQW9Z0MQ
00793  LINKAGE SECTION.                                                 EQW9Z0MQ
00794 *---------------*    DFHEIBLK ; DFHCOMMAREA.                      EQW9Z0MQ
00795  01  DFHCOMMAREA.                                                 EQW9Z0MQ
00796      02  FILLER             PIC X(4096).                          EQW9Z0MQ
00797 *                                                                 EQW9Z0MQ
00798 ***************************************************************   EQW9Z0MQ
00799 *        ZONES ADRESSABLES EXTERNES A LA TACHE                    EQW9Z0MQ
00800 ***************************************************************   EQW9Z0MQ
00801 *    USER                                                         EQW9Z0MQ
00802  01  LINK-USER              PIC X(4000).                          EQW9Z0MQ
00803 /                                                                 EQW9Z0MQ
00804  PROCEDURE DIVISION.                                              EQW9Z0MQ
00805 *------------------*                                              EQW9Z0MQ
00806 *                                                                 EQW9Z0MQ
00807 ***************************************************************** EQW9Z0MQ
00808 *              T R A M E   DU   P R O G R A M M E               * EQW9Z0MQ
00809 ***************************************************************** EQW9Z0MQ
00810 *                                                                 EQW9Z0MQ
00811 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
00812 *                                                               * EQW9Z0MQ
00813 *           -----------------------------------------           * EQW9Z0MQ
00814 *           I MODULE DE BASE DE LA TRANSACTION FB04 I       *     EQW9Z0MQ
00815 *           -----------------------------------------           * EQW9Z0MQ
00816 *                               I                               * EQW9Z0MQ
00817 *           -----------------------------------------           * EQW9Z0MQ
00818 *           I                   I                   I           * EQW9Z0MQ
00819 *   --------V--------   --------V--------   --------V--------   * EQW9Z0MQ
00820 *   I    ENTREE     I   I  TRAITEMENT   I   I     SORTIE    I   * EQW9Z0MQ
00821 *   -----------------   -----------------   -----------------   * EQW9Z0MQ
00822 *                                                               * EQW9Z0MQ
00823 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
00824  MODULE-FB04.                                                     EQW9Z0MQ
00825 *-----------*                                                     EQW9Z0MQ
00826      PERFORM MODULE-ENTREE THRU                                   EQW9Z0MQ
00827              FIN-MODULE-ENTREE.                                   EQW9Z0MQ
00828 *                                                                 EQW9Z0MQ
00829      IF TRAITEMENT                                                EQW9Z0MQ
00830         PERFORM MODULE-TRAITEMENT THRU                            EQW9Z0MQ
00831                 FIN-MODULE-TRAITEMENT                             EQW9Z0MQ
00832      END-IF.                                                      EQW9Z0MQ
00833 *                                                                 EQW9Z0MQ
00834      PERFORM MODULE-SORTIE THRU                                   EQW9Z0MQ
00835              FIN-MODULE-SORTIE.                                   EQW9Z0MQ
00836 *                                                                 EQW9Z0MQ
00837  FIN-MODULE-FB04.                                                 EQW9Z0MQ
00838      EXIT.                                                        EQW9Z0MQ
00839 /                                                                 EQW9Z0MQ
00840 ***************************************************************** EQW9Z0MQ
00841 ***************************************************************** EQW9Z0MQ
00842 ***********************  MODULE ENTREE  ************************* EQW9Z0MQ
00843 ***************************************************************** EQW9Z0MQ
00844 ***************************************************************** EQW9Z0MQ
00845 *                                                                 EQW9Z0MQ
00846 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
00847 *                                                               * EQW9Z0MQ
00848 *                    -----------------------                    * EQW9Z0MQ
00849 *                    I   MODULE D'ENTREE   I                    * EQW9Z0MQ
00850 *                    -----------------------                    * EQW9Z0MQ
00851 *                               I                               * EQW9Z0MQ
00852 *       ---------------------------------------------           * EQW9Z0MQ
00853 *       I          I         I                      I           * EQW9Z0MQ
00854 *  ----------  --------  ----------    -----------------------  * EQW9Z0MQ
00855 *  I HANDLE I  I USER I  I ADRESS I    I RECEPTION-MESSAGE   I  * EQW9Z0MQ
00856 *  ----------  --------  ----------    -----------------------  * EQW9Z0MQ
00857 *                                                               * EQW9Z0MQ
00858 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
00859 *                                                                 EQW9Z0MQ
00860  MODULE-ENTREE.                                                   EQW9Z0MQ
00861 *-------------*                                                   EQW9Z0MQ
00862      PERFORM INIT-USER          THRU                              EQW9Z0MQ
00863              FIN-INIT-USER.                                       EQW9Z0MQ
00864 *                                                                 EQW9Z0MQ
00865      PERFORM INIT-ADDRESS       THRU                              EQW9Z0MQ
00866              FIN-INIT-ADDRESS.                                    EQW9Z0MQ
00867 *                                                                 EQW9Z0MQ
00868      PERFORM RECEPTION-MESSAGE  THRU                              EQW9Z0MQ
00869              FIN-RECEPTION-MESSAGE.                               EQW9Z0MQ
00870 *                                                                 EQW9Z0MQ
00871  FIN-MODULE-ENTREE.                                               EQW9Z0MQ
00872      EXIT.                                                        EQW9Z0MQ
00873 *                                                                 EQW9Z0MQ
00874 ***************************************************************** EQW9Z0MQ
00875 *    INITIALISATION DES ZONES QUI NE PEUVENT PAS ETRE EN VALUE  * EQW9Z0MQ
00876 ***************************************************************** EQW9Z0MQ
00877  INIT-USER.                                                       EQW9Z0MQ
00878 *---------*                                                       EQW9Z0MQ
00879      MOVE LOW-VALUE TO Z-MAP.                                     EQW9Z0MQ
00880      MOVE   'FB04'  TO NOM-TACHE,                                 EQW9Z0MQ
00881                        NOM-TACHE-MAP,                             EQW9Z0MQ
00882                        NOM-TACHE-MAPSET,                          EQW9Z0MQ
00883                        NOM-TACHE-PROG.                            EQW9Z0MQ
00884      MOVE 'AA00'    TO NOM-LEVEL-SIGN.                            EQW9Z0MQ
00885      MOVE EIBTRMID  TO TRMID-TS-ECRAN,                            EQW9Z0MQ
00886                        W-XTERMIC.                                 EQW9Z0MQ
00887      MOVE NOM-TACHE TO W-XTRANSC,                                 EQW9Z0MQ
00888                        TRNID-TS-ECRAN.                            EQW9Z0MQ
00889      PERFORM RETRIEVE-DATA THRU                                   EQW9Z0MQ
00890              FIN-RETRIEVE-DATA.                                   EQW9Z0MQ
00891 *                     TROUVE : CODE-RETOUR = 0                    EQW9Z0MQ
00892      IF TROUVE                                                    EQW9Z0MQ
00893         MOVE 1 TO TYPE-PASSAGE                                    EQW9Z0MQ
00894      END-IF.                                                      EQW9Z0MQ
00895 *                                                                 EQW9Z0MQ
00896 *  SI ON N'EST PAS PASSE PAR LA SIGNATURE                         EQW9Z0MQ
00897 *  ET SI ON N'A PAS ETE APPELE PAR START  ERREUR                  EQW9Z0MQ
00898 *                                                                 EQW9Z0MQ
00899 *           TYPE-PASSAGE = 1  ===> PASSAGE-PAR-START              EQW9Z0MQ
00900      IF NOT PASSAGE-PAR-START                                     EQW9Z0MQ
00901         IF  EIBCALEN  = 0                                         EQW9Z0MQ
00902 *           MESSAGE ERREUR; RETOUR A SIGNATURE                    EQW9Z0MQ
00903             MOVE 'SQ001'         TO COM-GENE-MESANO               EQW9Z0MQ
00904                                     COM-CODERR                    EQW9Z0MQ
00905             MOVE CODE-LEVEL-SIGN TO Z-FONCTION                    EQW9Z0MQ
00906             PERFORM MODULE-SORTIE   THRU                          EQW9Z0MQ
00907                     FIN-MODULE-SORTIE                             EQW9Z0MQ
00908         ELSE                                                      EQW9Z0MQ
00909 *          ICI  ON RECUPERE LA COMMAREA PASSEE PAR XCTL OU RETURN EQW9Z0MQ
00910            MOVE DFHCOMMAREA      TO Z-COMMAREA                    EQW9Z0MQ
00911         END-IF                                                    EQW9Z0MQ
00912      END-IF.                                                      EQW9Z0MQ
00913 *                                                                 EQW9Z0MQ
00914 * RECUPERATION DU CODE COMPAGNIE POUR IDENTIFICATION DES TABLES   EQW9Z0MQ
00915 *    AAMENUXX   ET  AACONVXX                                      EQW9Z0MQ
00916 *                                                                 EQW9Z0MQ
00917      MOVE COM-GENE-CODCIE-PRINCIPAL      TO TABLE-SUFF.           EQW9Z0MQ
00918 *                                                                 EQW9Z0MQ
00919 *                                                                 EQW9Z0MQ
00920 * IDENTIFICATION DE LA TS DE PAGINATION                           EQW9Z0MQ
00921 *                                                                 EQW9Z0MQ
00922      MOVE EIBTRMID   TO PAGE-TS-PREF.                             EQW9Z0MQ
00923      MOVE 'PAG'      TO PAGE-TS-CONV.                             EQW9Z0MQ
00924 *                                                                 EQW9Z0MQ
00925 * IDENTIFICATION DE LA TS DE CONFIDENTIALITE                      EQW9Z0MQ
00926 *                                                                 EQW9Z0MQ
00927      MOVE EIBTRMID   TO CONF-TS-PREF.                             EQW9Z0MQ
00928      MOVE 'CF0'      TO CONF-TS-CONV.                             EQW9Z0MQ
00929 *                                                                 EQW9Z0MQ
00930 * IDENTIFICATION DE  LA TS DE L'APPLICATIVE                       EQW9Z0MQ
00931 *                                                                 EQW9Z0MQ
00932      MOVE EIBTRMID   TO APP-TS-PREF.                              EQW9Z0MQ
00933      MOVE 'APP'      TO APP-TS-CONV.                              EQW9Z0MQ
00934      MOVE   'NON'    TO DEBUGGIN.                                 EQW9Z0MQ
00935 ****************** POUR CONVERSATION **************************** EQW9Z0MQ
00936 *TK191190 POUR EXIT-SELECTION-DE-PLAN : IDENTIFICATION DE TS-PLAN EQW9Z0MQ
00937 ***************************************************************** EQW9Z0MQ
00938 *  POUR CONVERSATION SANS  SQL : LE PLAN PROPOSE = 'UTILPLN'      EQW9Z0MQ
00939  ++INCLUDE SQKCPLIU                                               EQW9Z0MQ
00940 */                                                                EQW9Z0MQ
00941  FIN-INIT-USER.                                                   EQW9Z0MQ
00942      EXIT.                                                        EQW9Z0MQ
00943 *                                                                 EQW9Z0MQ
00944 ***************************************************************   EQW9Z0MQ
00945 * INIT-ADDRESS.  ADRESSAGE DES ZONES SYSTEME DE CICS              EQW9Z0MQ
00946 *                CREATION TS PLAN POUR EXIT-SELECTION-PLAN        EQW9Z0MQ
00947 ***************************************************************   EQW9Z0MQ
00948  ++INCLUDE SQKCADDB                                               EQW9Z0MQ
00949 */                                                                EQW9Z0MQ
00950 /                                                                 EQW9Z0MQ
00951 ***************************************************************** EQW9Z0MQ
00952 * DETERMINATION DU TRAITEMENT EN FONCTION DE L'ENVIRONNEMENT    * EQW9Z0MQ
00953 ***************************************************************** EQW9Z0MQ
00954  RECEPTION-MESSAGE.                                               EQW9Z0MQ
00955 *-----------------*                                               EQW9Z0MQ
00956 *                                      PASSAGE PAR START          EQW9Z0MQ
00957      IF  PASSAGE-PAR-START                                        EQW9Z0MQ
00958          PERFORM DELETE-TS-ECRAN THRU                             EQW9Z0MQ
00959                  FIN-DELETE-TS-ECRAN                              EQW9Z0MQ
00960          MOVE    CODE-TRAITEMENT-AUTOMATIQUE TO Z-FONCTION        EQW9Z0MQ
00961          MOVE    1               TO NUMERO-PASSAGE                EQW9Z0MQ
00962          GO TO                   FIN-RECEPTION-MESSAGE            EQW9Z0MQ
00963      END-IF.                                                      EQW9Z0MQ
00964 *                                            AUTRE TACHE          EQW9Z0MQ
00965      IF  EIBTRNID NOT = NOM-TACHE                                 EQW9Z0MQ
00966          PERFORM DELETE-TS-ECRAN THRU                             EQW9Z0MQ
00967                  FIN-DELETE-TS-ECRAN                              EQW9Z0MQ
00968          MOVE   CODE-TRAITEMENT-AUTOMATIQUE TO Z-FONCTION         EQW9Z0MQ
00969          MOVE   1               TO NUMERO-PASSAGE                 EQW9Z0MQ
00970          GO TO                  FIN-RECEPTION-MESSAGE             EQW9Z0MQ
00971      END-IF.                                                      EQW9Z0MQ
00972 *                                            FAST PATH            EQW9Z0MQ
00973      IF EIBTRNID = Z-COMMAREA-TACHE-JUMP                          EQW9Z0MQ
00974         PERFORM DELETE-TS-ECRAN THRU                              EQW9Z0MQ
00975                 FIN-DELETE-TS-ECRAN                               EQW9Z0MQ
00976         MOVE    SPACES          TO  Z-COMMAREA-TACHE-JUMP         EQW9Z0MQ
00977         MOVE    CODE-TRAITEMENT-AUTOMATIQUE TO Z-FONCTION         EQW9Z0MQ
00978         MOVE    1               TO NUMERO-PASSAGE                 EQW9Z0MQ
00979         GO TO                   FIN-RECEPTION-MESSAGE             EQW9Z0MQ
00980      END-IF.                                                      EQW9Z0MQ
00981 *                                                                 EQW9Z0MQ
00982      PERFORM RECEIVE-MAP THRU                                     EQW9Z0MQ
00983              FIN-RECEIVE-MAP.                                     EQW9Z0MQ
00984      MOVE    EIBAID             TO WORKAID.                       EQW9Z0MQ
00985 *                                            LEVEL-MAX            EQW9Z0MQ
00986      IF  TOUCHE-PF4 OR TOUCHE-PF16                                EQW9Z0MQ
00987          MOVE CODE-LEVEL-MAX TO Z-FONCTION                        EQW9Z0MQ
00988      END-IF.                                                      EQW9Z0MQ
00989 *                                            DERNIER-AFFICHAGE    EQW9Z0MQ
00990 *    REAFFICHAGE DE L'ECRAN A PARTIR DE LA                        EQW9Z0MQ
00991 *    TS-ECRAN                                                     EQW9Z0MQ
00992 *                                            DERNIER-AFFICHAGE    EQW9Z0MQ
00993      IF  TOUCHE-PF5 OR TOUCHE-PF17                                EQW9Z0MQ
00994          MOVE CODE-LAST-AFF TO Z-FONCTION                         EQW9Z0MQ
00995          MOVE LOW-VALUE     TO Z-MAP                              EQW9Z0MQ
00996          MOVE NOM-TACHE     TO NOM-TACHE-RETOUR                   EQW9Z0MQ
00997          MOVE SPACES        TO COM-GENE-REAF                      EQW9Z0MQ
00998          PERFORM FUSION-TS-ECRAN THRU                             EQW9Z0MQ
00999                  FIN-FUSION-TS-ECRAN                              EQW9Z0MQ
01000          PERFORM SEND-MAP THRU                                    EQW9Z0MQ
01001                  FIN-SEND-MAP                                     EQW9Z0MQ
01002          PERFORM RETOUR-COMMAREA THRU                             EQW9Z0MQ
01003                  FIN-RETOUR-COMMAREA                              EQW9Z0MQ
01004      END-IF.                                                      EQW9Z0MQ
01005 *                                                                 EQW9Z0MQ
01006 * RECUPERATION DU CONTENU DE LA TS SI LA SORTIE N'EST PAS         EQW9Z0MQ
01007 * DEFINITIVE (LA SORTIE NE PEUT ETRE SURE QUE DANS                EQW9Z0MQ
01008 * MODULE-SORTIE)                                                  EQW9Z0MQ
01009 *                                                                 EQW9Z0MQ
01010          PERFORM FUSION-TS-ECRAN THRU                             EQW9Z0MQ
01011                  FIN-FUSION-TS-ECRAN.                             EQW9Z0MQ
01012 *                                            LEVEL-PREC           EQW9Z0MQ
01013 * POUR LES CONVERSATIONS SEULEMENT           LEVEL-PREC           EQW9Z0MQ
01014 *                                            LEVEL-PREC           EQW9Z0MQ
01015      IF  TOUCHE-PF12 OR TOUCHE-PF24                               EQW9Z0MQ
01016          MOVE CODE-LEVEL-PREC TO Z-FONCTION                       EQW9Z0MQ
01017      END-IF.                                                      EQW9Z0MQ
01018 *                                            LEVEL-SUP            EQW9Z0MQ
01019      IF  TOUCHE-PF3 OR TOUCHE-PF15                                EQW9Z0MQ
01020          MOVE CODE-LEVEL-SUP TO Z-FONCTION                        EQW9Z0MQ
01021      END-IF.                                                      EQW9Z0MQ
01022 *                                            SUITE                EQW9Z0MQ
01023      IF  TOUCHE-ENTER                                             EQW9Z0MQ
01024          MOVE CODE-TRAITEMENT-NORMAL TO Z-FONCTION                EQW9Z0MQ
01025      END-IF.                                                      EQW9Z0MQ
01026 *                                            SIGNATURE            EQW9Z0MQ
01027      IF  TOUCHE-CLEAR                                             EQW9Z0MQ
01028          MOVE CODE-LEVEL-SIGN        TO Z-FONCTION                EQW9Z0MQ
01029          GO TO FIN-RECEPTION-MESSAGE                              EQW9Z0MQ
01030      END-IF.                                                      EQW9Z0MQ
01031 *                                                                 EQW9Z0MQ
01032  FIN-RECEPTION-MESSAGE.                                           EQW9Z0MQ
01033      EXIT.                                                        EQW9Z0MQ
01034 /                                                                 EQW9Z0MQ
01035 ***************************************************************** EQW9Z0MQ
01036 * RECEIVE-MAP. RECEPTION DE LA MAP                                EQW9Z0MQ
01037 ***************************************************************** EQW9Z0MQ
01038 *                                                                 EQW9Z0MQ
01039  ++INCLUDE SQKCRECV                                               EQW9Z0MQ
01040 /                                                                 EQW9Z0MQ
01041 ***************************************************************** EQW9Z0MQ
01042 *        SECTION  DE PROGRAMME POUR LE TRAITEMENT DU MDT/OFF      EQW9Z0MQ
01043 ***************************************************************** EQW9Z0MQ
01044  ++INCLUDE SQKCMDTB                                               EQW9Z0MQ
01045 /                                                                 EQW9Z0MQ
01046 ***************************************************************** EQW9Z0MQ
01047 *   SECTION  DE PROGRAMME POUR L'ECRITURE DE LA TS PLAN           EQW9Z0MQ
01048 ***************************************************************** EQW9Z0MQ
01049  ++INCLUDE SQKCWRPL                                               EQW9Z0MQ
01050 /                                                                 EQW9Z0MQ
01051 ***************************************************************** EQW9Z0MQ
01052 *        MISE-A-JOUR-TS-ECRAN     GENEREE                         EQW9Z0MQ
01053 ***************************************************************** EQW9Z0MQ
01054 *                                                                 EQW9Z0MQ
01055  MISE-A-JOUR-TS-ECRAN.                                            EQW9Z0MQ
01056 *---------------------                                            EQW9Z0MQ
01057 *                                                                 EQW9Z0MQ
01058      IF ECR-XTRMTRACL    = ZEROS AND                              EQW9Z0MQ
01059         ECR-XTRMTRACA  NOT = EFFACE-FIN-ZONE                      EQW9Z0MQ
01060         MOVE TS-ECR-XTRMTRACO TO ECR-XTRMTRACO                    EQW9Z0MQ
01061         MOVE TS-ECR-XTRMTRACA TO ECR-XTRMTRACA                    EQW9Z0MQ
01062      ELSE                                                         EQW9Z0MQ
01063         MOVE ECR-XTRMTRACO    TO TS-ECR-XTRMTRACO                 EQW9Z0MQ
01064         MOVE LOW-VALUE        TO TS-ECR-XTRMTRACA                 EQW9Z0MQ
01065      END-IF.                                                      EQW9Z0MQ
01066 *                                                                 EQW9Z0MQ
01067      IF ECR-XAPPLILL    = ZEROS AND                               EQW9Z0MQ
01068         ECR-XAPPLILA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01069         MOVE TS-ECR-XAPPLILO TO ECR-XAPPLILO                      EQW9Z0MQ
01070         MOVE TS-ECR-XAPPLILA TO ECR-XAPPLILA                      EQW9Z0MQ
01071      ELSE                                                         EQW9Z0MQ
01072         MOVE ECR-XAPPLILO    TO TS-ECR-XAPPLILO                   EQW9Z0MQ
01073         MOVE LOW-VALUE       TO TS-ECR-XAPPLILA                   EQW9Z0MQ
01074      END-IF.                                                      EQW9Z0MQ
01075 *                                                                 EQW9Z0MQ
01076      IF ECR-XJOURDL    = ZEROS AND                                EQW9Z0MQ
01077         ECR-XJOURDA  NOT = EFFACE-FIN-ZONE                        EQW9Z0MQ
01078         MOVE TS-ECR-XJOURDO TO ECR-XJOURDO                        EQW9Z0MQ
01079         MOVE TS-ECR-XJOURDA TO ECR-XJOURDA                        EQW9Z0MQ
01080      ELSE                                                         EQW9Z0MQ
01081         MOVE ECR-XJOURDO    TO TS-ECR-XJOURDO                     EQW9Z0MQ
01082         MOVE LOW-VALUE      TO TS-ECR-XJOURDA                     EQW9Z0MQ
01083      END-IF.                                                      EQW9Z0MQ
01084 *                                                                 EQW9Z0MQ
01085      IF ECR-XRACFLL    = ZEROS AND                                EQW9Z0MQ
01086         ECR-XRACFLA  NOT = EFFACE-FIN-ZONE                        EQW9Z0MQ
01087         MOVE TS-ECR-XRACFLO TO ECR-XRACFLO                        EQW9Z0MQ
01088         MOVE TS-ECR-XRACFLA TO ECR-XRACFLA                        EQW9Z0MQ
01089      ELSE                                                         EQW9Z0MQ
01090         MOVE ECR-XRACFLO    TO TS-ECR-XRACFLO                     EQW9Z0MQ
01091         MOVE LOW-VALUE      TO TS-ECR-XRACFLA                     EQW9Z0MQ
01092      END-IF.                                                      EQW9Z0MQ
01093 *                                                                 EQW9Z0MQ
01094      IF ECR-XHEUREDL    = ZEROS AND                               EQW9Z0MQ
01095         ECR-XHEUREDA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01096         MOVE TS-ECR-XHEUREDO TO ECR-XHEUREDO                      EQW9Z0MQ
01097         MOVE TS-ECR-XHEUREDA TO ECR-XHEUREDA                      EQW9Z0MQ
01098      ELSE                                                         EQW9Z0MQ
01099         MOVE ECR-XHEUREDO    TO TS-ECR-XHEUREDO                   EQW9Z0MQ
01100         MOVE LOW-VALUE       TO TS-ECR-XHEUREDA                   EQW9Z0MQ
01101      END-IF.                                                      EQW9Z0MQ
01102 *                                                                 EQW9Z0MQ
01103      IF ECR-GESCLIL    = ZEROS AND                                EQW9Z0MQ
01104         ECR-GESCLIA  NOT = EFFACE-FIN-ZONE                        EQW9Z0MQ
01105         MOVE TS-ECR-GESCLIO TO ECR-GESCLIO                        EQW9Z0MQ
01106         MOVE TS-ECR-GESCLIA TO ECR-GESCLIA                        EQW9Z0MQ
01107      ELSE                                                         EQW9Z0MQ
01108         MOVE ECR-GESCLIO    TO TS-ECR-GESCLIO                     EQW9Z0MQ
01109         MOVE LOW-VALUE      TO TS-ECR-GESCLIA                     EQW9Z0MQ
01110         MOVE '2'            TO ETAT-ECRAN                         EQW9Z0MQ
01111      END-IF.                                                      EQW9Z0MQ
01112 *                                                                 EQW9Z0MQ
01113      IF ECR-RAICL    = ZEROS AND                                  EQW9Z0MQ
01114         ECR-RAICA  NOT = EFFACE-FIN-ZONE                          EQW9Z0MQ
01115         MOVE TS-ECR-RAICO TO ECR-RAICO                            EQW9Z0MQ
01116         MOVE TS-ECR-RAICA TO ECR-RAICA                            EQW9Z0MQ
01117      ELSE                                                         EQW9Z0MQ
01118         MOVE ECR-RAICO    TO TS-ECR-RAICO                         EQW9Z0MQ
01119         MOVE LOW-VALUE    TO TS-ECR-RAICA                         EQW9Z0MQ
01120         MOVE '2'          TO ETAT-ECRAN                           EQW9Z0MQ
01121      END-IF.                                                      EQW9Z0MQ
01122 *                                                                 EQW9Z0MQ
01123      IF ECR-NOMCL    = ZEROS AND                                  EQW9Z0MQ
01124         ECR-NOMCA  NOT = EFFACE-FIN-ZONE                          EQW9Z0MQ
01125         MOVE TS-ECR-NOMCO TO ECR-NOMCO                            EQW9Z0MQ
01126         MOVE TS-ECR-NOMCA TO ECR-NOMCA                            EQW9Z0MQ
01127      ELSE                                                         EQW9Z0MQ
01128         MOVE ECR-NOMCO    TO TS-ECR-NOMCO                         EQW9Z0MQ
01129         MOVE LOW-VALUE    TO TS-ECR-NOMCA                         EQW9Z0MQ
01130         MOVE '2'          TO ETAT-ECRAN                           EQW9Z0MQ
01131      END-IF.                                                      EQW9Z0MQ
01132 *                                                                 EQW9Z0MQ
36835      IF ECR-VEHCGNCL    = ZEROS AND                               EQW9Z0MQ
36835         ECR-VEHCGNCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
36835         MOVE TS-ECR-VEHCGNCO TO ECR-VEHCGNCO                      EQW9Z0MQ
36835         MOVE TS-ECR-VEHCGNCA TO ECR-VEHCGNCA                      EQW9Z0MQ
36835      ELSE                                                         EQW9Z0MQ
36835         MOVE ECR-VEHCGNCO    TO TS-ECR-VEHCGNCO                   EQW9Z0MQ
36835         MOVE LOW-VALUE    TO TS-ECR-VEHCGNCA                      EQW9Z0MQ
36835         MOVE '2'          TO ETAT-ECRAN                           EQW9Z0MQ
36835      END-IF.                                                      EQW9Z0MQ
01132 *                                                                 EQW9Z0MQ
36835      IF ECR-VEHCGPCL    = ZEROS AND                               EQW9Z0MQ
36835         ECR-VEHCGPCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
36835         MOVE TS-ECR-VEHCGPCO TO ECR-VEHCGPCO                      EQW9Z0MQ
36835         MOVE TS-ECR-VEHCGPCA TO ECR-VEHCGPCA                      EQW9Z0MQ
36835      ELSE                                                         EQW9Z0MQ
36835         MOVE ECR-VEHCGPCO    TO TS-ECR-VEHCGPCO                   EQW9Z0MQ
36835         MOVE LOW-VALUE    TO TS-ECR-VEHCGPCA                      EQW9Z0MQ
36835         MOVE '2'          TO ETAT-ECRAN                           EQW9Z0MQ
36835      END-IF.                                                      EQW9Z0MQ
01132 *                                                                 EQW9Z0MQ
36835      IF ECR-VEHCGSCL    = ZEROS AND                               EQW9Z0MQ
36835         ECR-VEHCGSCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
36835         MOVE TS-ECR-VEHCGSCO TO ECR-VEHCGSCO                      EQW9Z0MQ
36835         MOVE TS-ECR-VEHCGSCA TO ECR-VEHCGSCA                      EQW9Z0MQ
36835      ELSE                                                         EQW9Z0MQ
36835         MOVE ECR-VEHCGSCO    TO TS-ECR-VEHCGSCO                   EQW9Z0MQ
36835         MOVE LOW-VALUE    TO TS-ECR-VEHCGSCA                      EQW9Z0MQ
36835         MOVE '2'          TO ETAT-ECRAN                           EQW9Z0MQ
36835      END-IF.                                                      EQW9Z0MQ
01132 *                                                                 EQW9Z0MQ
01133      IF ECR-VEHCODCL    = ZEROS AND                               EQW9Z0MQ
01134         ECR-VEHCODCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01135         MOVE TS-ECR-VEHCODCO TO ECR-VEHCODCO                      EQW9Z0MQ
01136         MOVE TS-ECR-VEHCODCA TO ECR-VEHCODCA                      EQW9Z0MQ
01137      ELSE                                                         EQW9Z0MQ
01138         MOVE ECR-VEHCODCO    TO TS-ECR-VEHCODCO                   EQW9Z0MQ
01139         MOVE LOW-VALUE       TO TS-ECR-VEHCODCA                   EQW9Z0MQ
01140         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01141      END-IF.                                                      EQW9Z0MQ
01142 *                                                                 EQW9Z0MQ
01143      IF ECR-VEHTYPCL    = ZEROS AND                               EQW9Z0MQ
01144         ECR-VEHTYPCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01145         MOVE TS-ECR-VEHTYPCO TO ECR-VEHTYPCO                      EQW9Z0MQ
01146         MOVE TS-ECR-VEHTYPCA TO ECR-VEHTYPCA                      EQW9Z0MQ
01147      ELSE                                                         EQW9Z0MQ
01148         MOVE ECR-VEHTYPCO    TO TS-ECR-VEHTYPCO                   EQW9Z0MQ
01149         MOVE LOW-VALUE       TO TS-ECR-VEHTYPCA                   EQW9Z0MQ
01150         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01151      END-IF.                                                      EQW9Z0MQ
01152 *                                                                 EQW9Z0MQ
01153      IF ECR-VEHGENCL    = ZEROS AND                               EQW9Z0MQ
01154         ECR-VEHGENCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01155         MOVE TS-ECR-VEHGENCO TO ECR-VEHGENCO                      EQW9Z0MQ
01156         MOVE TS-ECR-VEHGENCA TO ECR-VEHGENCA                      EQW9Z0MQ
01157      ELSE                                                         EQW9Z0MQ
01158         MOVE ECR-VEHGENCO    TO TS-ECR-VEHGENCO                   EQW9Z0MQ
01159         MOVE LOW-VALUE       TO TS-ECR-VEHGENCA                   EQW9Z0MQ
01160         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01161      END-IF.                                                      EQW9Z0MQ
01162 *                                                                 EQW9Z0MQ
01163      IF ECR-VEHMARLL    = ZEROS AND                               EQW9Z0MQ
01164         ECR-VEHMARLA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01165         MOVE TS-ECR-VEHMARLO TO ECR-VEHMARLO                      EQW9Z0MQ
01166         MOVE TS-ECR-VEHMARLA TO ECR-VEHMARLA                      EQW9Z0MQ
01167      ELSE                                                         EQW9Z0MQ
01168         MOVE ECR-VEHMARLO    TO TS-ECR-VEHMARLO                   EQW9Z0MQ
01169         MOVE LOW-VALUE       TO TS-ECR-VEHMARLA                   EQW9Z0MQ
01170         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01171      END-IF.                                                      EQW9Z0MQ
01172 *                                                                 EQW9Z0MQ
01173      IF ECR-VEHMODLL    = ZEROS AND                               EQW9Z0MQ
01174         ECR-VEHMODLA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01175         MOVE TS-ECR-VEHMODLO TO ECR-VEHMODLO                      EQW9Z0MQ
01176         MOVE TS-ECR-VEHMODLA TO ECR-VEHMODLA                      EQW9Z0MQ
01177      ELSE                                                         EQW9Z0MQ
01178         MOVE ECR-VEHMODLO    TO TS-ECR-VEHMODLO                   EQW9Z0MQ
01179         MOVE LOW-VALUE       TO TS-ECR-VEHMODLA                   EQW9Z0MQ
01180         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01181      END-IF.                                                      EQW9Z0MQ
01182 *                                                                 EQW9Z0MQ
01183      IF ECR-VEHCYLNL    = ZEROS AND                               EQW9Z0MQ
01184         ECR-VEHCYLNA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01185         MOVE TS-ECR-VEHCYLNO TO ECR-VEHCYLNO                      EQW9Z0MQ
01186         MOVE TS-ECR-VEHCYLNA TO ECR-VEHCYLNA                      EQW9Z0MQ
01187      ELSE                                                         EQW9Z0MQ
01188         MOVE ECR-VEHCYLNO    TO TS-ECR-VEHCYLNO                   EQW9Z0MQ
01189         MOVE LOW-VALUE       TO TS-ECR-VEHCYLNA                   EQW9Z0MQ
01190         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01191      END-IF.                                                      EQW9Z0MQ
01192 *                                                                 EQW9Z0MQ
01193      IF ECR-VEHUSACL    = ZEROS AND                               EQW9Z0MQ
01194         ECR-VEHUSACA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01195         MOVE TS-ECR-VEHUSACO TO ECR-VEHUSACO                      EQW9Z0MQ
01196         MOVE TS-ECR-VEHUSACA TO ECR-VEHUSACA                      EQW9Z0MQ
01197      ELSE                                                         EQW9Z0MQ
01198         MOVE ECR-VEHUSACO    TO TS-ECR-VEHUSACO                   EQW9Z0MQ
01199         MOVE LOW-VALUE       TO TS-ECR-VEHUSACA                   EQW9Z0MQ
01200         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01201      END-IF.                                                      EQW9Z0MQ
01202 *                                                                 EQW9Z0MQ
01203      IF ECR-VEHIMMXL    = ZEROS AND                               EQW9Z0MQ
01204         ECR-VEHIMMXA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01205         MOVE TS-ECR-VEHIMMXO TO ECR-VEHIMMXO                      EQW9Z0MQ
01206         MOVE TS-ECR-VEHIMMXA TO ECR-VEHIMMXA                      EQW9Z0MQ
01207      ELSE                                                         EQW9Z0MQ
01208         MOVE ECR-VEHIMMXO    TO TS-ECR-VEHIMMXO                   EQW9Z0MQ
01209         MOVE LOW-VALUE       TO TS-ECR-VEHIMMXA                   EQW9Z0MQ
01210         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01211      END-IF.                                                      EQW9Z0MQ
01212 *                                                                 EQW9Z0MQ
01213      IF ECR-VEHCIRDL    = ZEROS AND                               EQW9Z0MQ
01214         ECR-VEHCIRDA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01215         MOVE TS-ECR-VEHCIRDO TO ECR-VEHCIRDO                      EQW9Z0MQ
01216         MOVE TS-ECR-VEHCIRDA TO ECR-VEHCIRDA                      EQW9Z0MQ
01217      ELSE                                                         EQW9Z0MQ
01218         MOVE ECR-VEHCIRDO    TO TS-ECR-VEHCIRDO                   EQW9Z0MQ
01219         MOVE LOW-VALUE       TO TS-ECR-VEHCIRDA                   EQW9Z0MQ
01220         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01221      END-IF.                                                      EQW9Z0MQ
01222 *                                                                 EQW9Z0MQ
F3576      IF ECR-VEHACQDL    = ZEROS AND                               EQW9Z0MQ
F3576         ECR-VEHACQDA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
F3576         MOVE TS-ECR-VEHACQDO TO ECR-VEHACQDO                      EQW9Z0MQ
F3576         MOVE TS-ECR-VEHACQDA TO ECR-VEHACQDA                      EQW9Z0MQ
F3576      ELSE                                                         EQW9Z0MQ
F3576         MOVE ECR-VEHACQDO    TO TS-ECR-VEHACQDO                   EQW9Z0MQ
F3576         MOVE LOW-VALUE       TO TS-ECR-VEHACQDA                   EQW9Z0MQ
F3576         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
F3576      END-IF.                                                      EQW9Z0MQ
F3576 *                                                                 EQW9Z0MQ
01223      IF ECR-VEHVALML    = ZEROS AND                               EQW9Z0MQ
01224         ECR-VEHVALMA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01225         MOVE TS-ECR-VEHVALMO TO ECR-VEHVALMO                      EQW9Z0MQ
01226         MOVE TS-ECR-VEHVALMA TO ECR-VEHVALMA                      EQW9Z0MQ
01227      ELSE                                                         EQW9Z0MQ
01228         MOVE ECR-VEHVALMO    TO TS-ECR-VEHVALMO                   EQW9Z0MQ
01229         MOVE LOW-VALUE       TO TS-ECR-VEHVALMA                   EQW9Z0MQ
01230         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01231      END-IF.                                                      EQW9Z0MQ
01232 *                                                                 EQW9Z0MQ
F3215      IF ECR-VEHGROCL    = ZEROS AND                               EQW9Z0MQ
F3215         ECR-VEHGROCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
F3215         MOVE TS-ECR-VEHGROCO TO ECR-VEHGROCO                      EQW9Z0MQ
F3215         MOVE TS-ECR-VEHGROCA TO ECR-VEHGROCA                      EQW9Z0MQ
F3215      ELSE                                                         EQW9Z0MQ
F3215         MOVE ECR-VEHGROCO    TO TS-ECR-VEHGROCO                   EQW9Z0MQ
F3215         MOVE LOW-VALUE       TO TS-ECR-VEHGROCA                   EQW9Z0MQ
F3215         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
F3215      END-IF.                                                      EQW9Z0MQ
F3215 *                                                                 EQW9Z0MQ
F3215      IF ECR-VEHCLACL    = ZEROS AND                               EQW9Z0MQ
F3215         ECR-VEHCLACA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
F3215         MOVE TS-ECR-VEHCLACO TO ECR-VEHCLACO                      EQW9Z0MQ
F3215         MOVE TS-ECR-VEHCLACA TO ECR-VEHCLACA                      EQW9Z0MQ
F3215      ELSE                                                         EQW9Z0MQ
F3215         MOVE ECR-VEHCLACO    TO TS-ECR-VEHCLACO                   EQW9Z0MQ
F3215         MOVE LOW-VALUE       TO TS-ECR-VEHCLACA                   EQW9Z0MQ
F3215         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
F3215      END-IF.                                                      EQW9Z0MQ
F3215 *                                                                 EQW9Z0MQ
01233      IF ECR-VEHPRTCL    = ZEROS AND                               EQW9Z0MQ
01234         ECR-VEHPRTCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01235         MOVE TS-ECR-VEHPRTCO TO ECR-VEHPRTCO                      EQW9Z0MQ
01236         MOVE TS-ECR-VEHPRTCA TO ECR-VEHPRTCA                      EQW9Z0MQ
01237      ELSE                                                         EQW9Z0MQ
01238         MOVE ECR-VEHPRTCO    TO TS-ECR-VEHPRTCO                   EQW9Z0MQ
01239         MOVE LOW-VALUE       TO TS-ECR-VEHPRTCA                   EQW9Z0MQ
01240         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01241      END-IF.                                                      EQW9Z0MQ
01242 *                                                                 EQW9Z0MQ
01243      IF ECR-GARCODCL    = ZEROS AND                               EQW9Z0MQ
01244         ECR-GARCODCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01245         MOVE TS-ECR-GARCODCO TO ECR-GARCODCO                      EQW9Z0MQ
01246         MOVE TS-ECR-GARCODCA TO ECR-GARCODCA                      EQW9Z0MQ
01247      ELSE                                                         EQW9Z0MQ
01248         MOVE ECR-GARCODCO    TO TS-ECR-GARCODCO                   EQW9Z0MQ
01249         MOVE LOW-VALUE       TO TS-ECR-GARCODCA                   EQW9Z0MQ
01250         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01251      END-IF.                                                      EQW9Z0MQ
01252 *                                                                 EQW9Z0MQ
01253      IF ECR-VEHPOSCL    = ZEROS AND                               EQW9Z0MQ
01254         ECR-VEHPOSCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01255         MOVE TS-ECR-VEHPOSCO TO ECR-VEHPOSCO                      EQW9Z0MQ
01256         MOVE TS-ECR-VEHPOSCA TO ECR-VEHPOSCA                      EQW9Z0MQ
01257      ELSE                                                         EQW9Z0MQ
01258         MOVE ECR-VEHPOSCO    TO TS-ECR-VEHPOSCO                   EQW9Z0MQ
01259         MOVE LOW-VALUE       TO TS-ECR-VEHPOSCA                   EQW9Z0MQ
01260         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01261      END-IF.                                                      EQW9Z0MQ
01262 *                                                                 EQW9Z0MQ
01263      IF ECR-VEHPEFCL    = ZEROS AND                               EQW9Z0MQ
01264         ECR-VEHPEFCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01265         MOVE TS-ECR-VEHPEFCO TO ECR-VEHPEFCO                      EQW9Z0MQ
01266         MOVE TS-ECR-VEHPEFCA TO ECR-VEHPEFCA                      EQW9Z0MQ
01267      ELSE                                                         EQW9Z0MQ
01268         MOVE ECR-VEHPEFCO    TO TS-ECR-VEHPEFCO                   EQW9Z0MQ
01269         MOVE LOW-VALUE       TO TS-ECR-VEHPEFCA                   EQW9Z0MQ
01270         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01271      END-IF.                                                      EQW9Z0MQ
01272 *                                                                 EQW9Z0MQ
01273      IF ECR-GARCOPCL    = ZEROS AND                               EQW9Z0MQ
01274         ECR-GARCOPCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01275         MOVE TS-ECR-GARCOPCO TO ECR-GARCOPCO                      EQW9Z0MQ
01276         MOVE TS-ECR-GARCOPCA TO ECR-GARCOPCA                      EQW9Z0MQ
01277      ELSE                                                         EQW9Z0MQ
01278         MOVE ECR-GARCOPCO    TO TS-ECR-GARCOPCO                   EQW9Z0MQ
01279         MOVE LOW-VALUE       TO TS-ECR-GARCOPCA                   EQW9Z0MQ
01280         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01281      END-IF.                                                      EQW9Z0MQ
01282 *                                                                 EQW9Z0MQ
01283      IF ECR-GARVILLL    = ZEROS AND                               EQW9Z0MQ
01284         ECR-GARVILLA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01285         MOVE TS-ECR-GARVILLO TO ECR-GARVILLO                      EQW9Z0MQ
01286         MOVE TS-ECR-GARVILLA TO ECR-GARVILLA                      EQW9Z0MQ
01287      ELSE                                                         EQW9Z0MQ
01288         MOVE ECR-GARVILLO    TO TS-ECR-GARVILLO                   EQW9Z0MQ
01289         MOVE LOW-VALUE       TO TS-ECR-GARVILLA                   EQW9Z0MQ
01290         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01291      END-IF.                                                      EQW9Z0MQ
01292 *                                                                 EQW9Z0MQ
01293      IF ECR-ANVREPCL    = ZEROS AND                               EQW9Z0MQ
01294         ECR-ANVREPCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01295         MOVE TS-ECR-ANVREPCO TO ECR-ANVREPCO                      EQW9Z0MQ
01296         MOVE TS-ECR-ANVREPCA TO ECR-ANVREPCA                      EQW9Z0MQ
01297      ELSE                                                         EQW9Z0MQ
01298         MOVE ECR-ANVREPCO    TO TS-ECR-ANVREPCO                   EQW9Z0MQ
01299         MOVE LOW-VALUE       TO TS-ECR-ANVREPCA                   EQW9Z0MQ
01300         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01301      END-IF.                                                      EQW9Z0MQ
01302 *                                                                 EQW9Z0MQ
01303      IF ECR-VEHFORCL    = ZEROS AND                               EQW9Z0MQ
01304         ECR-VEHFORCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01305         MOVE TS-ECR-VEHFORCO TO ECR-VEHFORCO                      EQW9Z0MQ
01306         MOVE TS-ECR-VEHFORCA TO ECR-VEHFORCA                      EQW9Z0MQ
01307      ELSE                                                         EQW9Z0MQ
01308         MOVE ECR-VEHFORCO    TO TS-ECR-VEHFORCO                   EQW9Z0MQ
01309         MOVE LOW-VALUE       TO TS-ECR-VEHFORCA                   EQW9Z0MQ
01310         MOVE '2'             TO ETAT-ECRAN                        EQW9Z0MQ
01311      END-IF.                                                      EQW9Z0MQ
01312 *                                                                 EQW9Z0MQ
01313      IF ECR-ANVNUMXL    = ZEROS AND                               EQW9Z0MQ
01314         ECR-ANVNUMXA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01315         MOVE TS-ECR-ANVNUMXO  TO ECR-ANVNUMXO                     EQW9Z0MQ
01316         MOVE TS-ECR-ANVNUMXA  TO ECR-ANVNUMXA                     EQW9Z0MQ
01317      ELSE                                                         EQW9Z0MQ
01318         MOVE ECR-ANVNUMXO     TO TS-ECR-ANVNUMXO                  EQW9Z0MQ
01319         MOVE LOW-VALUE        TO TS-ECR-ANVNUMXA                  EQW9Z0MQ
01320         MOVE '2'              TO ETAT-ECRAN                       EQW9Z0MQ
01321      END-IF.                                                      EQW9Z0MQ
01322 *                                                                 EQW9Z0MQ
01287      IF ECR-ANVSOUSL    = ZEROS AND                               EQW90UNO
01288         ECR-ANVSOUSA  NOT = EFFACE-FIN-ZONE                       EQW90UNO
01289         MOVE TS-ECR-ANVSOUSO  TO ECR-ANVSOUSO                     EQW90UNO
01290         MOVE TS-ECR-ANVSOUSA  TO ECR-ANVSOUSA                     EQW90UNO
01291      ELSE                                                         EQW90UNO
01292         MOVE ECR-ANVSOUSO     TO TS-ECR-ANVSOUSO                  EQW90UNO
01293         MOVE LOW-VALUE        TO TS-ECR-ANVSOUSA                  EQW90UNO
01294         MOVE '2'              TO ETAT-ECRAN                       EQW90UNO
01295      END-IF.                                                      EQW90UNO
01322 *                                                                 EQW9Z0MQ
01287      IF ECR-ANVSOUNL    = ZEROS AND                               EQW90UNO
01288         ECR-ANVSOUNA  NOT = EFFACE-FIN-ZONE                       EQW90UNO
01289         MOVE TS-ECR-ANVSOUNO  TO ECR-ANVSOUNO                     EQW90UNO
01290         MOVE TS-ECR-ANVSOUNA  TO ECR-ANVSOUNA                     EQW90UNO
01291      ELSE                                                         EQW90UNO
01292         MOVE ECR-ANVSOUNO     TO TS-ECR-ANVSOUNO                  EQW90UNO
01293         MOVE LOW-VALUE        TO TS-ECR-ANVSOUNA                  EQW90UNO
01294         MOVE '2'              TO ETAT-ECRAN                       EQW90UNO
01295      END-IF.                                                      EQW90UNO
01322 *                                                                 EQW9Z0MQ
01323      IF ECR-ANVCIEXL    = ZEROS AND                               EQW9Z0MQ
01324         ECR-ANVCIEXA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01325         SET MODIF-CIE-NON     TO TRUE                             EQW9Z0MQ
01326         MOVE TS-ECR-ANVCIEXO  TO ECR-ANVCIEXO                     EQW9Z0MQ
01327         MOVE TS-ECR-ANVCIEXA  TO ECR-ANVCIEXA                     EQW9Z0MQ
01328      ELSE                                                         EQW9Z0MQ
01329         MOVE ECR-ANVCIEXO     TO TS-ECR-ANVCIEXO                  EQW9Z0MQ
01330         SET MODIF-CIE-OUI     TO TRUE                             EQW9Z0MQ
01331         MOVE LOW-VALUE        TO TS-ECR-ANVCIEXA                  EQW9Z0MQ
01332         MOVE '2'              TO ETAT-ECRAN                       EQW9Z0MQ
01333      END-IF.                                                      EQW9Z0MQ
01334 *                                                                 EQW9Z0MQ
01335      IF ECR-ANVCIELL    = ZEROS AND                               EQW9Z0MQ
01336         ECR-ANVCIELA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01337         SET MODIF-LIB-NON     TO TRUE                             EQW9Z0MQ
01338         MOVE TS-ECR-ANVCIELO  TO ECR-ANVCIELO                     EQW9Z0MQ
01339         MOVE TS-ECR-ANVCIELA  TO ECR-ANVCIELA                     EQW9Z0MQ
01340      ELSE                                                         EQW9Z0MQ
01341         MOVE ECR-ANVCIELO     TO TS-ECR-ANVCIELO                  EQW9Z0MQ
01342         SET MODIF-LIB-OUI     TO TRUE                             EQW9Z0MQ
01343         MOVE LOW-VALUE        TO TS-ECR-ANVCIELA                  EQW9Z0MQ
01344         MOVE '2'              TO ETAT-ECRAN                       EQW9Z0MQ
01345      END-IF.                                                      EQW9Z0MQ
01346 *                                                                 EQW9Z0MQ
01347      IF ECR-ANVANCNL    = ZEROS AND                               EQW9Z0MQ
01348         ECR-ANVANCNA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01349         MOVE TS-ECR-ANVANCNO  TO ECR-ANVANCNO                     EQW9Z0MQ
01350         MOVE TS-ECR-ANVANCNA  TO ECR-ANVANCNA                     EQW9Z0MQ
01351      ELSE                                                         EQW9Z0MQ
01352         MOVE ECR-ANVANCNO     TO TS-ECR-ANVANCNO                  EQW9Z0MQ
01353         MOVE LOW-VALUE        TO TS-ECR-ANVANCNA                  EQW9Z0MQ
01354         MOVE '2'              TO ETAT-ECRAN                       EQW9Z0MQ
01355      END-IF.                                                      EQW9Z0MQ
01356 *                                                                 EQW9Z0MQ
01357      IF ECR-ANVINTNL    = ZEROS AND                               EQW9Z0MQ
01358         ECR-ANVINTNA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01359         MOVE TS-ECR-ANVINTNO  TO ECR-ANVINTNO                     EQW9Z0MQ
01360         MOVE TS-ECR-ANVINTNA  TO ECR-ANVINTNA                     EQW9Z0MQ
01361      ELSE                                                         EQW9Z0MQ
01362         MOVE ECR-ANVINTNO     TO TS-ECR-ANVINTNO                  EQW9Z0MQ
01363         MOVE LOW-VALUE        TO TS-ECR-ANVINTNA                  EQW9Z0MQ
01364         MOVE '2'              TO ETAT-ECRAN                       EQW9Z0MQ
01365      END-IF.                                                      EQW9Z0MQ
01366 *                                                                 EQW9Z0MQ
01367      IF ECR-ANVRESDL    = ZEROS AND                               EQW9Z0MQ
01368         ECR-ANVRESDA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01369         MOVE TS-ECR-ANVRESDO  TO ECR-ANVRESDO                     EQW9Z0MQ
01370         MOVE TS-ECR-ANVRESDA  TO ECR-ANVRESDA                     EQW9Z0MQ
01371      ELSE                                                         EQW9Z0MQ
01372         MOVE ECR-ANVRESDO     TO TS-ECR-ANVRESDO                  EQW9Z0MQ
01373         MOVE LOW-VALUE        TO TS-ECR-ANVRESDA                  EQW9Z0MQ
01374         MOVE '2'              TO ETAT-ECRAN                       EQW9Z0MQ
01375      END-IF.                                                      EQW9Z0MQ
01376 *                                                                 EQW9Z0MQ
01377      IF ECR-SIVINDCL    = ZEROS AND                               EQW9Z0MQ
01378         ECR-SIVINDCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01379         MOVE TS-ECR-SIVINDCO  TO ECR-SIVINDCO                     EQW9Z0MQ
01380         MOVE TS-ECR-SIVINDCA  TO ECR-SIVINDCA                     EQW9Z0MQ
01381      ELSE                                                         EQW9Z0MQ
01382         MOVE ECR-SIVINDCO     TO TS-ECR-SIVINDCO                  EQW9Z0MQ
01383         MOVE LOW-VALUE        TO TS-ECR-SIVINDCA                  EQW9Z0MQ
01384         MOVE '2'              TO ETAT-ECRAN                       EQW9Z0MQ
01385      END-IF.                                                      EQW9Z0MQ
01386 *                                                                 EQW9Z0MQ
01367      IF ECR-ANVMTRCL    = ZEROS AND                               EQW9Z0MQ
01368         ECR-ANVMTRCA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01369         MOVE TS-ECR-ANVMTRCO  TO ECR-ANVMTRCO                     EQW9Z0MQ
01370         MOVE TS-ECR-ANVMTRCA  TO ECR-ANVMTRCA                     EQW9Z0MQ
01371      ELSE                                                         EQW9Z0MQ
01372         MOVE ECR-ANVMTRCO     TO TS-ECR-ANVMTRCO                  EQW9Z0MQ
01373         MOVE LOW-VALUE        TO TS-ECR-ANVMTRCA                  EQW9Z0MQ
01374         MOVE '2'              TO ETAT-ECRAN                       EQW9Z0MQ
01375      END-IF.                                                      EQW9Z0MQ
01376 *                                                                 EQW9Z0MQ
01387      IF ECR-ANVBONTL    = ZEROS AND                               EQW9Z0MQ
01388         ECR-ANVBONTA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01389         MOVE TS-ECR-ANVBONTO  TO ECR-ANVBONTO                     EQW9Z0MQ
01390         MOVE TS-ECR-ANVBONTA  TO ECR-ANVBONTA                     EQW9Z0MQ
01391      ELSE                                                         EQW9Z0MQ
01392         MOVE ECR-ANVBONTO     TO TS-ECR-ANVBONTO                  EQW9Z0MQ
01393         MOVE LOW-VALUE        TO TS-ECR-ANVBONTA                  EQW9Z0MQ
01394         MOVE '2'              TO ETAT-ECRAN                       EQW9Z0MQ
01395      END-IF.                                                      EQW9Z0MQ
01396 *                                                                 EQW9Z0MQ
01397      IF ECR-ANVBONDL    = ZEROS AND                               EQW9Z0MQ
01398         ECR-ANVBONDA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01399         MOVE TS-ECR-ANVBONDO  TO ECR-ANVBONDO                     EQW9Z0MQ
01400         MOVE TS-ECR-ANVBONDA  TO ECR-ANVBONDA                     EQW9Z0MQ
01401      ELSE                                                         EQW9Z0MQ
01402         MOVE ECR-ANVBONDO     TO TS-ECR-ANVBONDO                  EQW9Z0MQ
01403         MOVE LOW-VALUE        TO TS-ECR-ANVBONDA                  EQW9Z0MQ
01404         MOVE '2'              TO ETAT-ECRAN                       EQW9Z0MQ
01405      END-IF.                                                      EQW9Z0MQ
01406 *                                                                 EQW9Z0MQ
01407      IF ECR-ANVABODL    = ZEROS AND                               EQW9Z0MQ
01408         ECR-ANVABODA  NOT = EFFACE-FIN-ZONE                       EQW9Z0MQ
01409         MOVE TS-ECR-ANVABODO  TO ECR-ANVABODO                     EQW9Z0MQ
01410         MOVE TS-ECR-ANVABODA  TO ECR-ANVABODA                     EQW9Z0MQ
01411      ELSE                                                         EQW9Z0MQ
01412         MOVE ECR-ANVABODO     TO TS-ECR-ANVABODO                  EQW9Z0MQ
01413         MOVE LOW-VALUE        TO TS-ECR-ANVABODA                  EQW9Z0MQ
01414         MOVE '2'              TO ETAT-ECRAN                       EQW9Z0MQ
01415      END-IF.                                                      EQW9Z0MQ
01416 *                                                                 EQW9Z0MQ
01417      IF ECR-XCDECL    = ZEROS AND                                 EQW9Z0MQ
01418         ECR-XCDECA  NOT = EFFACE-FIN-ZONE                         EQW9Z0MQ
01419         MOVE TS-ECR-XCDECO TO ECR-XCDECO                          EQW9Z0MQ
01420         MOVE TS-ECR-XCDECA TO ECR-XCDECA                          EQW9Z0MQ
01421      ELSE                                                         EQW9Z0MQ
01422         MOVE ECR-XCDECO    TO TS-ECR-XCDECO                       EQW9Z0MQ
01423         MOVE LOW-VALUE     TO TS-ECR-XCDECA                       EQW9Z0MQ
01424      END-IF.                                                      EQW9Z0MQ
01425 *                                                                 EQW9Z0MQ
01426 *    MOVE Z-TIMER-TIMJOU    TO MTIMJOUI TS-MTIMJOUI.              EQW9Z0MQ
01427 *    MISE A BLANC DES MESSAGES. TK071290.CNC$MATS.                EQW9Z0MQ
01428      MOVE SPACES            TO ECR-XMSGILO  ECR-XMSGALO.          EQW9Z0MQ
01429  FIN-MISE-A-JOUR-TS-ECRAN.                                        EQW9Z0MQ
01430      EXIT.                                                        EQW9Z0MQ
01431 *                                                                 EQW9Z0MQ
01432 ***************************************************************** EQW9Z0MQ
01433 ***************************************************************** EQW9Z0MQ
01434 ***********************  MODULE TRAITEMENT ********************** EQW9Z0MQ
01435 ***************************************************************** EQW9Z0MQ
01436 ***************************************************************** EQW9Z0MQ
01437 *                                                                 EQW9Z0MQ
01438  MODULE-TRAITEMENT.                                               EQW9Z0MQ
01439 *-----------------*                                               EQW9Z0MQ
01440                                                                   EQW9Z0MQ
01441      MOVE SPACES TO DONNEES-VEHICULE OF TS-VEHICULE(1).           EQW9Z0MQ
01081      MOVE SPACES TO DONNEES-PERSONNE OF TS-PERSONNE(1).           EQW8LBVR
01442                                                                   EQW9Z0MQ
01443      IF  COM-GENE-CODCNV = SPACES OR LOW-VALUE                    EQW9Z0MQ
01444          PERFORM READ-TS-SUSPENS-DL1 THRU FIN-READ-TS-SUSPENS-DL1 EQW9Z0MQ
01445          PERFORM DETER-READ-TSVEHI   THRU FIN-DETER-READ-TSVEHI   EQW9Z0MQ
01446          IF WSS-READ-TSVEHI = 'O'                                 EQW9Z0MQ
01447             PERFORM READ-TS-VEHICULE THRU FIN-READ-TS-VEHICULE    EQW9Z0MQ
01448          END-IF                                                   EQW9Z0MQ
01449      END-IF.                                                      EQW9Z0MQ
01450 *                                                                 EQW9Z0MQ
01451      IF  TRAITEMENT-NORMAL                                        EQW9Z0MQ
01452          PERFORM M-TRAITEMENT-NORMAL THRU                         EQW9Z0MQ
01453                  FIN-M-TRAITEMENT-NORMAL                          EQW9Z0MQ
01454      END-IF.                                                      EQW9Z0MQ
01455 *                                                                 EQW9Z0MQ
01456      IF  TRAITEMENT-AUTOMATIQUE                                   EQW9Z0MQ
01457          PERFORM M-TRAITEMENT-AUTOMATIQUE THRU                    EQW9Z0MQ
01458                  FIN-M-TRAITEMENT-AUTOMATIQUE                     EQW9Z0MQ
01459      END-IF.                                                      EQW9Z0MQ
01460 *                                                                 EQW9Z0MQ
01461  FIN-MODULE-TRAITEMENT.                                           EQW9Z0MQ
01462      EXIT.                                                        EQW9Z0MQ
01463 *                                                                 EQW9Z0MQ
01464 ******************************************************************EQW9Z0MQ
01465 *   LECTURE DES ITEMS DE LA TS SUSPENS                           *EQW9Z0MQ
01466 ******************************************************************EQW9Z0MQ
01467  READ-TS-SUSPENS-DL1.                                             EQW9Z0MQ
01468 *--------------------                                             EQW9Z0MQ
01469 *** LECTURE ITEM 1 ***                                            EQW9Z0MQ
01470      MOVE +1 TO RANG-TS.                                          EQW9Z0MQ
01471      EXEC CICS READQ TS QUEUE   (IDENT-TS-APP)                    EQW9Z0MQ
01472                         INTO    (TS-SUSPENS1)                     EQW9Z0MQ
01473                         LENGTH  (LONG-TS-SUSPENS)                 EQW9Z0MQ
01474                         ITEM    (RANG-TS)                         EQW9Z0MQ
01475                         NOHANDLE                                  EQW9Z0MQ
01476      END-EXEC.                                                    EQW9Z0MQ
01477      IF EIBRCODE NOT = LOW-VALUE                                  EQW9Z0MQ
01478         MOVE 'FBR1 : ERREUR READ TS-SUSPENS1' TO MESS             EQW9Z0MQ
01479         GO TO ABANDON-TACHE                                       EQW9Z0MQ
01480      ELSE                                                         EQW9Z0MQ
01481         MOVE SEGTRA OF TS-SUSPENS1 TO FBMISPTR-IT1                EQW9Z0MQ
01482      END-IF.                                                      EQW9Z0MQ
01483                                                                   EQW9Z0MQ
01484 *** LECTURE ITEM 2 ***                                            EQW9Z0MQ
01485      MOVE +2 TO RANG-TS.                                          EQW9Z0MQ
01486      EXEC CICS READQ TS QUEUE   (IDENT-TS-APP)                    EQW9Z0MQ
01487                         INTO    (TS-SUSPENS2)                     EQW9Z0MQ
01488                         LENGTH  (LONG-TS-SUSPENS)                 EQW9Z0MQ
01489                         ITEM    (RANG-TS)                         EQW9Z0MQ
01490                         NOHANDLE                                  EQW9Z0MQ
01491      END-EXEC.                                                    EQW9Z0MQ
01492      IF EIBRCODE NOT = LOW-VALUE                                  EQW9Z0MQ
01493         MOVE 'FBR2 : ERREUR READ TS-SUSPENS2' TO MESS             EQW9Z0MQ
01494         GO TO ABANDON-TACHE                                       EQW9Z0MQ
01495      END-IF.                                                      EQW9Z0MQ
01496                                                                   EQW9Z0MQ
01497  FIN-READ-TS-SUSPENS-DL1.                                         EQW9Z0MQ
01498      EXIT.                                                        EQW9Z0MQ
01499 *                                                                 EQW9Z0MQ
01500 ******************************************************************EQW9Z0MQ
01501 *   DETERMINATION DE LA LECTURE OU PAS DE LA TS VEHICULE         *EQW9Z0MQ
01502 ******************************************************************EQW9Z0MQ
01503  DETER-READ-TSVEHI.                                               EQW9Z0MQ
01504 *------------------                                               EQW9Z0MQ
01505      MOVE 'N' TO WSS-READ-TSVEHI.                                 EQW9Z0MQ
01506      IF COM-FB-CODE-ACTION = '1' OR '2' OR '3'                    EQW9Z0MQ
01507         IF COM-FB-RANG-MAX-TSVEHI NOT = ZERO AND                  EQW9Z0MQ
01508            COM-FB-RANG-TS-LIRE = COM-FB-RANG-MAX-TSVEHI           EQW9Z0MQ
01509            MOVE 'O' TO WSS-READ-TSVEHI                            EQW9Z0MQ
01510         END-IF                                                    EQW9Z0MQ
01511      ELSE                                                         EQW9Z0MQ
01512         MOVE 'O' TO WSS-READ-TSVEHI                               EQW9Z0MQ
01513      END-IF.                                                      EQW9Z0MQ
01514                                                                   EQW9Z0MQ
01515  FIN-DETER-READ-TSVEHI.                                           EQW9Z0MQ
01516      EXIT.                                                        EQW9Z0MQ
01517 *                                                                 EQW9Z0MQ
01518 ******************************************************************EQW9Z0MQ
01519 *   LECTURE DE LA TS VEHICULE                                   * EQW9Z0MQ
01520 ******************************************************************EQW9Z0MQ
01521  READ-TS-VEHICULE.                                                EQW9Z0MQ
01522 *-----------------                                                EQW9Z0MQ
01523 *** LECTURE RANG COM-FB-RANG-TS-LIRE ***                          EQW9Z0MQ
01524      EXEC CICS READQ TS QUEUE   (COM-FB-IDENT-TSVEHI)             EQW9Z0MQ
01525                         INTO    (TS-VEHICULE)                     EQW9Z0MQ
01526                         LENGTH  (LENGTH OF TS-VEHICULE)           EQW9Z0MQ
01527                         ITEM    (COM-FB-RANG-TS-LIRE)             EQW9Z0MQ
01528                         NOHANDLE                                  EQW9Z0MQ
01529      END-EXEC.                                                    EQW9Z0MQ
01530      IF EIBRCODE NOT = LOW-VALUE                                  EQW9Z0MQ
01531         MOVE 'VER1 : ERREUR READ TS VEHICULE' TO MESS             EQW9Z0MQ
01532         GO TO ABANDON-TACHE                                       EQW9Z0MQ
01533      END-IF.                                                      EQW9Z0MQ
01534                                                                   EQW9Z0MQ
01535  FIN-READ-TS-VEHICULE.                                            EQW9Z0MQ
01536      EXIT.                                                        EQW9Z0MQ
01537 *                                                                 EQW9Z0MQ
01538 ******************************************************************EQW9Z0MQ
01519 *   LECTURE DE LA TS VEHICULE CHANGER                           * EQW9Z0MQ
01520 ******************************************************************EQW9Z0MQ
01521  READ-TS-VEHI-CHANGER.                                            EQW9Z0MQ
01522 *---------------------                                            EQW9Z0MQ
01523      ADD 1 TO WSS-RANG-TS-CHANGER.                                EQW9Z0MQ
01524      EXEC CICS READQ TS QUEUE   (COM-FB-IDENT-TSVEHI)             EQW9Z0MQ
01525                         INTO    (TS-VEHICULE-CHANGER)             EQW9Z0MQ
01526                         LENGTH  (LENGTH OF TS-VEHICULE-CHANGER)   EQW9Z0MQ
01527                         ITEM    (WSS-RANG-TS-CHANGER)             EQW9Z0MQ
01528                         NOHANDLE                                  EQW9Z0MQ
01529      END-EXEC.                                                    EQW9Z0MQ
01530      IF EIBRCODE NOT = LOW-VALUE                                  EQW9Z0MQ
              IF WSS-RANG-TS-CHANGER > COM-FB-RANG-MAX-TSVEHI
                 MOVE 'O'        TO WSS-FIN-VEHI
              ELSE
                 MOVE 'VEC1 : ERREUR READ TS VEHICULE CHANGER' TO MESS
                 GO TO ABANDON-TACHE
              END-IF
01533      END-IF.                                                      EQW9Z0MQ
01534                                                                   EQW9Z0MQ
01535  FIN-READ-TS-VEHI-CHANGER.                                        EQW9Z0MQ
01536      EXIT.                                                        EQW9Z0MQ
01537 *                                                                 EQW9Z0MQ
01538 ******************************************************************EQW9Z0MQ
01539 *   LECTURE DE LA TS TECHNIQUE                                  * EQW9Z0MQ
01540 ******************************************************************EQW9Z0MQ
01541  READ-TS-TECHNIQUE.                                               EQW9Z0MQ
01542 *------------------                                               EQW9Z0MQ
01543 *** LECTURE RANG COM-FB-RANG-TS-LIRE ***                          EQW9Z0MQ
01544      MOVE +1 TO RANG-TS.                                          EQW9Z0MQ
01545      EXEC CICS READQ TS QUEUE   (COM-FB-IDENT-TSTECH)             EQW9Z0MQ
01546                         INTO    (TS-TECHNIQUE)                    EQW9Z0MQ
01547                         LENGTH  (LENGTH OF TS-TECHNIQUE)          EQW9Z0MQ
01548                         ITEM    (RANG-TS)                         EQW9Z0MQ
01549                         NOHANDLE                                  EQW9Z0MQ
01550      END-EXEC.                                                    EQW9Z0MQ
01551      IF EIBRCODE NOT = LOW-VALUE                                  EQW9Z0MQ
01552         MOVE 'TEC1 : ERREUR READ TS TECHNIQUE' TO MESS            EQW9Z0MQ
01553         GO TO ABANDON-TACHE                                       EQW9Z0MQ
01554      END-IF.                                                      EQW9Z0MQ
01555                                                                   EQW9Z0MQ
01556  FIN-READ-TS-TECHNIQUE.                                           EQW9Z0MQ
01557      EXIT.                                                        EQW9Z0MQ
01194 /                                                                 EQW8LBVR
01195 ******************************************************************EQW8LBVR
01196 *   LECTURE DE LA TS PERSONNE                                   * EQW8LBVR
01197 ******************************************************************EQW8LBVR
01198  READ-TS-PERSONNE.                                                EQW8LBVR
01200 *** LECTURE RANG COM-FB-RANG-TS-PERS ***                          EQW8LBVR
01201      EXEC CICS READQ TS QUEUE   (COM-FB-IDENT-TSPERS)             EQW8LBVR
01202                         INTO    (TS-PERSONNE)                     EQW8LBVR
01203                         LENGTH  (LENGTH OF TS-PERSONNE)           EQW8LBVR
                              ITEM    (COM-FB-RANG-TSPERS)
01205                         NOHANDLE                                  EQW8LBVR
01206      END-EXEC.                                                    EQW8LBVR
01207      IF EIBRCODE NOT = LOW-VALUE                                  EQW8LBVR
01208         MOVE 'PER1 : ERREUR READ TS PERSONNE' TO MESS             EQW8LBVR
01209         GO TO ABANDON-TACHE                                       EQW8LBVR
01210      END-IF.                                                      EQW8LBVR
01211                                                                   EQW8LBVR
01212  FIN-READ-TS-PERSONNE. EXIT.                                      EQW8LBVR
01558 /                                                                 EQW9Z0MQ
      ******************************************************************
      *   VERIFICATION PRESENCE ANCIEN SOUSCRIPTEUR AU CONTRAT         *
      ******************************************************************
       VERIF-PERSONNE.
           IF  RPERSORD OF TS-PERSONNE(1) > SPACES
           AND PERSORD OF TS-PERSONNE(1) = '99999999'
               IF  ECR-ANVSOUSO(1:2) NOT = 'EN'
                   IF  (PERSTAC OF TS-PERSONNE(1) = ECR-ANVSOUSO(1:2))
                   OR  (PERSTAC OF TS-PERSONNE(1) = 'PM' AND
                                     ECR-ANVSOUSO = 'SOUS')
                       SET PERSONNE-TROUVE TO TRUE
                   END-IF
               ELSE
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 4
                           OR PRMTYPC OF TS-PERSONNE(1, I) > SPACES
                   END-PERFORM
                   IF  I NOT > 4
                       SET PERSONNE-TROUVE TO TRUE
                   END-IF
               END-IF
           END-IF.

       FIN-VERIF-PERSONNE. EXIT.
      /
01559 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
01560 * MODULE DE BASE   * FB04 * TRAITEMENT AUTOMATIQUE                EQW9Z0MQ
01561 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
01562 *                                                               * EQW9Z0MQ
01563 *                -------------------------------                * EQW9Z0MQ
01564 *                I   TRAITEMENT AUTOMATIQUE    I                * EQW9Z0MQ
01565 *                -------------------------------                * EQW9Z0MQ
01566 *                               I                               * EQW9Z0MQ
01567 *                               I                               * EQW9Z0MQ
01568 *                -------------------------------                * EQW9Z0MQ
01569 *                I  REMPLISSAGE FORMAT ECRAN   I                * EQW9Z0MQ
01570 *                -------------------------------                * EQW9Z0MQ
01571 *                                                               * EQW9Z0MQ
01572 *                                                               * EQW9Z0MQ
01573 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
01574 *                                                                 EQW9Z0MQ
01575  M-TRAITEMENT-AUTOMATIQUE.                                        EQW9Z0MQ
01576 *------------------------*                                        EQW9Z0MQ
01577 *                                                                 EQW9Z0MQ
01578 * RECHERCHE DES ENCHAINEMENTS DES CONVERSATIONS                   EQW9Z0MQ
01579 * SI PREMIERE TRANSACTION DE LA CONVERSATION                      EQW9Z0MQ
01580 *                                                                 EQW9Z0MQ
01581      IF COM-GENE-CODCNV NOT = SPACES AND LOW-VALUE                EQW9Z0MQ
01582         PERFORM INIT-CONV     THRU FIN-INIT-CONV                  EQW9Z0MQ
01583      ELSE                                                         EQW9Z0MQ
01584         MOVE COM-GENE-LNGCNV  TO   LONG-TS                        EQW9Z0MQ
01585         PERFORM READ-TS-CONF  THRU FIN-READ-TS-CONF               EQW9Z0MQ
01586 *       PERFORM CONTROLE-CONV THRU FIN-CONTROLE-CONV              EQW9Z0MQ
01587      END-IF.                                                      EQW9Z0MQ
01588      PERFORM INIT-RECH-CONV-LIBRE THRU                            EQW9Z0MQ
01589              FIN-INIT-RECH-CONV-LIBRE.                            EQW9Z0MQ
01590      PERFORM REMPLISSAGE-FORMAT-ECRAN THRU                        EQW9Z0MQ
01591              FIN-REMPLISSAGE-FORMAT-ECRAN.                        EQW9Z0MQ
01592 *                                                                 EQW9Z0MQ
01593  FIN-M-TRAITEMENT-AUTOMATIQUE.                                    EQW9Z0MQ
01594      EXIT.                                                        EQW9Z0MQ
01595 *                                                                 EQW9Z0MQ
01596 *                                                                 EQW9Z0MQ
01597 ***************************************************************   EQW9Z0MQ
01598 * LECTURE TABLE AACONVXX POSTE = COM-GENE-CODCONV POUR AVOIR      EQW9Z0MQ
01599 * LE LIBELLE  DE LA CONVERSATION                                  EQW9Z0MQ
01600 ***************************************************************   EQW9Z0MQ
01601  INIT-CONV.                                                       EQW9Z0MQ
01602 *---------*                                                       EQW9Z0MQ
01603      MOVE  SPACES                   TO XSPIPARM.                  EQW9Z0MQ
01604      MOVE 'AACONV'                  TO TABLE-PREF.                EQW9Z0MQ
01605      MOVE COM-GENE-CODCIE-PRINCIPAL TO TABLE-SUFF.                EQW9Z0MQ
01606      MOVE COM-GENE-CODCNV           TO REF-POSTE OF XSPIPARM.     EQW9Z0MQ
01607      PERFORM LECT-SPI-CONV THRU                                   EQW9Z0MQ
01608              FIN-LECT-SPI-CONV.                                   EQW9Z0MQ
01609      IF NON-TROUVE                                                EQW9Z0MQ
01610         MOVE  1      TO KONTROL                                   EQW9Z0MQ
01611         MOVE 'SQ006' TO COM-GENE-MESANO                           EQW9Z0MQ
01612                         COM-CODERR                                EQW9Z0MQ
01613         PERFORM MODULE-SORTIE THRU                                EQW9Z0MQ
01614                 FIN-MODULE-SORTIE                                 EQW9Z0MQ
01615      END-IF.                                                      EQW9Z0MQ
01616 * SI TROUVE                                                       EQW9Z0MQ
01617      MOVE LIB-CONV   TO COM-GENE-LIBCNV.                          EQW9Z0MQ
01618 *                                                                 EQW9Z0MQ
01619 * SI ON EST DANS LA PREMIERE TRANSACTION D'UNE CONVERSATION       EQW9Z0MQ
01620 * 1- ON PASSE DANS L'INTERFACE  AUAAL04  POUR  CONTROLE D'ACCES   EQW9Z0MQ
01621 *             SI   OK VERS 2. SI NON  LEVEL-SUPERIEUR             EQW9Z0MQ
01622 * 2- ON ACCEDE A L'INTERFACE POUR CONSTRUCTION DE LA TS           EQW9Z0MQ
01623 *    DE CONFIDENTIALITE                                           EQW9Z0MQ
01624 * 3- ON STOCKE EN COMMAREA TOUS LES ENCHAINEMENTS POSSIBLES POUR  EQW9Z0MQ
01625 *    CETTE CONVERSATION SI ELLE EST AUTORISEE                     EQW9Z0MQ
01626 *                                                                 EQW9Z0MQ
01627      PERFORM INTERFACE-CONTROLE-ACCES  THRU                       EQW9Z0MQ
01628              FIN-INTERFACE-CONTROLE-ACCES.                        EQW9Z0MQ
01629      IF COM-AU-MESSAGE NOT = SPACES AND LOW-VALUE                 EQW9Z0MQ
01630         MOVE    1      TO KONTROL                                 EQW9Z0MQ
01631         MOVE    COM-AU-MESSAGE TO COM-GENE-MESANO                 EQW9Z0MQ
01632                                   COM-CODERR                      EQW9Z0MQ
01633 *                                                                 EQW9Z0MQ
01634         PERFORM SORTIE-LEVEL-SUPERIEUR  THRU                      EQW9Z0MQ
01635             FIN-SORTIE-LEVEL-SUPERIEUR                            EQW9Z0MQ
01636      END-IF.                                                      EQW9Z0MQ
01637 *                                                                 EQW9Z0MQ
01638 *    FABRICATION  DE LA TS CONFIDENTIALITE CONVERSATION           EQW9Z0MQ
01639 *            PAR L'INTERFACE AUAAL00                              EQW9Z0MQ
01640      PERFORM INTERFACE-CONFIDENTIALITE THRU                       EQW9Z0MQ
01641              FIN-INTERFACE-CONFIDENTIALITE.                       EQW9Z0MQ
U3319      MOVE    COM-GENE-CODCNV  TO  COM-GENE-CODCNV-SAUVE.          EFUTSQP3
01642      MOVE    SPACES        TO COM-GENE-CODCNV.                    EQW9Z0MQ
01643      MOVE COM-GENE-LNGCNV TO LONG-TS.                             EQW9Z0MQ
01644      PERFORM READ-TS-CONF THRU                                    EQW9Z0MQ
01645              FIN-READ-TS-CONF.                                    EQW9Z0MQ
01646      MOVE    1 TO IA.                                             EQW9Z0MQ
01647      PERFORM STOCKAGE-ENCH-CONV  THRU                             EQW9Z0MQ
01648              FIN-STOCKAGE-ENCH-CONV 51 TIMES.                     EQW9Z0MQ
01649 *                                                                 EQW9Z0MQ
01650  FIN-INIT-CONV.                                                   EQW9Z0MQ
01651      EXIT.                                                        EQW9Z0MQ
01652 *                                                                 EQW9Z0MQ
01653 *                                                                 EQW9Z0MQ
01654 * **********************************************************      EQW9Z0MQ
01655 *CONTROLE-CONV.                                                   EQW9Z0MQ
01656 *-------------*                                                   EQW9Z0MQ
01657 *     VOIR LA TECHNIQUE  DANS LE MEME PARAGRAPHE CONTROLE-CONV    EQW9Z0MQ
01658 *     DU PROGRAMME  DE  MENU UNIQUE                               EQW9Z0MQ
01659 *                                                                 EQW9Z0MQ
01660 *    IF CONTROLE NON OK                                           EQW9Z0MQ
01661 *           MOVE 'SQ016' TO COM-GENE-MESANO                       EQW9Z0MQ
01662 *                           COM-CODERR                            EQW9Z0MQ
01663 *           PERFORM SORTIE-LEVEL-SUPERIEUR THRU                   EQW9Z0MQ
01664 *                   FIN-SORTIE-LEVEL-SUPERIEUR                    EQW9Z0MQ
01665 *    END-IF.                                                      EQW9Z0MQ
01666 *                                                                 EQW9Z0MQ
01667 *FIN-CONTROLE-CONV.  EXIT.                                        EQW9Z0MQ
01669 *                                                                 EQW9Z0MQ
01670 * **********************************************************      EQW9Z0MQ
01671  LECT-SPI-CONV.                                                   EQW9Z0MQ
01672 *-------------*                                                   EQW9Z0MQ
01673      MOVE  'GP'                   TO FONCTION  OF XSPIPARM.       EQW9Z0MQ
01674      MOVE  IDENT-TABLE            TO CODTAB    OF XSPIPARM.       EQW9Z0MQ
01675      MOVE  '= '                   TO OPERATEUR OF XSPIPARM.       EQW9Z0MQ
01676      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
01677      MOVE SPACES                  TO TAB-AACONV.                  EQW9Z0MQ
01678      IF  RETCOD OF XSPIPARM  = ZERO                               EQW9Z0MQ
01679          MOVE 0                   TO CODE-RETOUR                  EQW9Z0MQ
01680          MOVE IOAREA OF XSPIPARM  TO TAB-AACONV                   EQW9Z0MQ
01681      ELSE                                                         EQW9Z0MQ
01682          MOVE 1 TO CODE-RETOUR                                    EQW9Z0MQ
01683      END-IF.                                                      EQW9Z0MQ
01684  FIN-LECT-SPI-CONV.                                               EQW9Z0MQ
01685      EXIT.                                                        EQW9Z0MQ
01686 * **********************************************************      EQW9Z0MQ
01687  INIT-RECH-CONV-LIBRE.                                            EQW9Z0MQ
01688 * ------------------ *                                            EQW9Z0MQ
01689      MOVE 1  TO IA.                                               EQW9Z0MQ
01690      PERFORM RECHERCHE-CONV-LIBRE THRU                            EQW9Z0MQ
01691              FIN-RECHERCHE-CONV-LIBRE                             EQW9Z0MQ
01692              UNTIL COM-GENE-PILCNV(IA) = SPACES OR                EQW9Z0MQ
01693                    LOW-VALUE                                      EQW9Z0MQ
01694              OR IA > 50                                           EQW9Z0MQ
01695              OR COM-GENE-PILCNV(IA) = NOM-TACHE.                  EQW9Z0MQ
01696      IF IA > 50                                                   EQW9Z0MQ
01697         MOVE 1 TO KONTROL                                         EQW9Z0MQ
01698         MOVE 'SQ005'   TO COM-GENE-MESANO                         EQW9Z0MQ
01699                           COM-CODERR                              EQW9Z0MQ
01700         PERFORM MODULE-SORTIE THRU                                EQW9Z0MQ
01701                 FIN-MODULE-SORTIE                                 EQW9Z0MQ
01702      ELSE                                                         EQW9Z0MQ
01703         MOVE NOM-TACHE TO COM-GENE-PILCNV(IA)                     EQW9Z0MQ
01704         MOVE IA        TO COM-GENE-INDCNV                         EQW9Z0MQ
01705      END-IF.                                                      EQW9Z0MQ
01706  FIN-INIT-RECH-CONV-LIBRE.                                        EQW9Z0MQ
01707      EXIT.                                                        EQW9Z0MQ
01708 *                                                                 EQW9Z0MQ
01709 ***************************************************************   EQW9Z0MQ
01710 * RECHERCHE DANS LA COMMAREA D'UN POSTE LIBRE POUR STOCKER LE     EQW9Z0MQ
01711 * CODE TRANSACTION DE LA CONVERSATION EN COURS                    EQW9Z0MQ
01712 ***************************************************************   EQW9Z0MQ
01713  RECHERCHE-CONV-LIBRE.                                            EQW9Z0MQ
01714 *---------------------                                            EQW9Z0MQ
01715      IF COM-GENE-PILCNV(IA) NOT = SPACES AND LOW-VALUE            EQW9Z0MQ
01716         ADD 1 TO IA                                               EQW9Z0MQ
01717      END-IF.                                                      EQW9Z0MQ
01718  FIN-RECHERCHE-CONV-LIBRE.                                        EQW9Z0MQ
01719      EXIT.                                                        EQW9Z0MQ
01720 ***************************************************************   EQW9Z0MQ
01721  STOCKAGE-ENCH-CONV.                                              EQW9Z0MQ
01722 *-------------------                                              EQW9Z0MQ
01723      MOVE COD-TRN-ECR(IA)  TO COM-GENE-ENCCNV-CODTRN(IA).         EQW9Z0MQ
01724      MOVE COD-MNE-ECR(IA)  TO COM-GENE-ENCCNV-CODMNE(IA).         EQW9Z0MQ
01725      ADD 1 TO IA.                                                 EQW9Z0MQ
01726  FIN-STOCKAGE-ENCH-CONV.                                          EQW9Z0MQ
01727      EXIT.                                                        EQW9Z0MQ
01728 *                                                                 EQW9Z0MQ
01729 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
01730 *                                                               * EQW9Z0MQ
01731 *                -------------------------------                * EQW9Z0MQ
01732 *                I   REMPLISSAGE DE L'ECRAN    I                * EQW9Z0MQ
01733 *                -------------------------------                * EQW9Z0MQ
01734 *                               I                               * EQW9Z0MQ
01735 *           -----------------------------------------           * EQW9Z0MQ
01736 *           I                   I                   I           * EQW9Z0MQ
01737 * ----------------------   -------------   -----------------    * EQW9Z0MQ
01738 * I ZONES OBLIGATOIRES I   I PROTEGEES I   I NON PROTEGEES I    * EQW9Z0MQ
01739 * ----------------------   -------------   -----------------    * EQW9Z0MQ
01740 *                                                               * EQW9Z0MQ
01741 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
01742 * REMPLISSAGE ECRAN   * FB04 * TRAITEMENT AUTOMATIQUE             EQW9Z0MQ
01743 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
01744 *                                                                 EQW9Z0MQ
01745  REMPLISSAGE-FORMAT-ECRAN.                                        EQW9Z0MQ
01746 * ---------------------- *                                        EQW9Z0MQ
01747       PERFORM REMPLISSAGE-ZONES-OBLIGATOIRES THRU                 EQW9Z0MQ
01748               FIN-REMP-ZONES-OBL.                                 EQW9Z0MQ
01749 *                                                                 EQW9Z0MQ
01750       PERFORM REMPLISSAGE-ZONES-PROTEGEES THRU                    EQW9Z0MQ
01751               FIN-REMP-ZONES-PROT.                                EQW9Z0MQ
01752 *                                                                 EQW9Z0MQ
01753       PERFORM REMPLISSAGE-ZONES-NO-PROTEGEES THRU                 EQW9Z0MQ
01754               FIN-REMP-ZONES-NO-PROT.                             EQW9Z0MQ
01755 *                                                                 EQW9Z0MQ
01756       PERFORM RESTAURATION-ATTRIBUTS THRU                         EQW9Z0MQ
01757               FIN-RESTAURATION-ATTRIBUTS.                         EQW9Z0MQ
01758 *                                                                 EQW9Z0MQ
01759 *     PERFORM LECTURE-FICHIER THRU                                EQW9Z0MQ
01760 *             FIN-LECTURE-FICHIER.                                EQW9Z0MQ
01761 *                                                                 EQW9Z0MQ
01762  FIN-REMPLISSAGE-FORMAT-ECRAN.                                    EQW9Z0MQ
01763      EXIT.                                                        EQW9Z0MQ
01764 *                                                                 EQW9Z0MQ
01765 *                                                                 EQW9Z0MQ
01766 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
01767 * ZONES OBLIGATOIRES  * FB04 * TRAITEMENT AUTOMATIQUE             EQW9Z0MQ
01768 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
01769 *                                                                 EQW9Z0MQ
01770  REMPLISSAGE-ZONES-OBLIGATOIRES.                                  EQW9Z0MQ
01771 * ---------------------------- *                                  EQW9Z0MQ
01772      MOVE Z-TIMER-DATJOU        TO   ECR-XJOURDO.                 EQW9Z0MQ
01773      MOVE Z-TIMER-TIMJOU        TO   ECR-XHEUREDO.                EQW9Z0MQ
01774      MOVE W-XTRMTRAC            TO   ECR-XTRMTRACO.               EQW9Z0MQ
01775      MOVE COM-GENE-LIBUSR       TO   ECR-XRACFLO.                 EQW9Z0MQ
01776      MOVE COM-GENE-LIBCNV       TO   ECR-XAPPLILO.                EQW9Z0MQ
01777 *                                                                 EQW9Z0MQ
01778 *  VERIFICATION DE LA PRESENCE DE CODES MESSAGES EN COMMAREA      EQW9Z0MQ
01779 *  SI OUI AFFICHAGE                                               EQW9Z0MQ
01780 *         REMISE A BLANC EN  COMMAREA                             EQW9Z0MQ
01781      IF COM-GENE-MESINF  NOT = SPACES AND                         EQW9Z0MQ
01782                                LOW-VALUE                          EQW9Z0MQ
01783         PERFORM LECTURE-ERREUR THRU                               EQW9Z0MQ
01784                 FIN-LECTURE-ERREUR                                EQW9Z0MQ
01785         MOVE    SPACES   TO COM-GENE-MESINF                       EQW9Z0MQ
01786         MOVE    W-ERREUR TO ECR-XMSGILO                           EQW9Z0MQ
01787      END-IF.                                                      EQW9Z0MQ
01788      IF COM-GENE-MESANO  NOT = SPACES AND                         EQW9Z0MQ
01789                                LOW-VALUE                          EQW9Z0MQ
01790         PERFORM LECTURE-ERREUR THRU                               EQW9Z0MQ
01791                 FIN-LECTURE-ERREUR                                EQW9Z0MQ
01792         MOVE SPACES      TO COM-GENE-MESANO                       EQW9Z0MQ
01793         MOVE W-ERREUR    TO ECR-XMSGALO                           EQW9Z0MQ
01794      END-IF.                                                      EQW9Z0MQ
01795  FIN-REMP-ZONES-OBL.                                              EQW9Z0MQ
01796      EXIT.                                                        EQW9Z0MQ
01797 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
01798 * ZONES PROTEGEES  * FB04 * TRAITEMENT AUTOMATIQUE                EQW9Z0MQ
01799 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
01800  REMPLISSAGE-ZONES-PROTEGEES.                                     EQW9Z0MQ
01801 *----------------------------                                     EQW9Z0MQ
01802      MOVE COM-GENE-LIBCNV         TO  ECR-XAPPLILO.               EQW9Z0MQ
01803                                                                   EQW9Z0MQ
01804 *---CODE INTERMEDIAIRE ET NUMERO CLIENT                           EQW9Z0MQ
01983      MOVE INF-GES OF TS-SUSPENS1  TO  W-GES.                      FB04T00O
01984      MOVE NOM-CLI OF TS-SUSPENS1  TO  W-CLI.                      FB04T00O
U3319  ++INCLUDE MAIPCLI
01986      MOVE W-GESCLI                TO  ECR-GESCLIO.                FB04T00O
01808                                                                   EQW9Z0MQ
01809 *---QUALITE DU CLIENT                                             EQW9Z0MQ
01810      MOVE NOM-RAIC OF TS-SUSPENS1 TO  ECR-RAICO.                  EQW9Z0MQ
01811                                                                   EQW9Z0MQ
01812 *---NOM DU CLIENT                                                 EQW9Z0MQ
01813      MOVE NOM-NOMC OF TS-SUSPENS1 TO  ECR-NOMCO.                  EQW9Z0MQ
01814                                                                   EQW9Z0MQ
01815 *---TYPE DU VEHICULE                                              EQW9Z0MQ
01816      EVALUATE COM-FB-CODE-ACTION                                  EQW9Z0MQ
01817         WHEN '1'                                                  EQW9Z0MQ
01818            MOVE '4R '             TO  ECR-VEHTYPCO                EQW9Z0MQ
01819            IF ECR-VEHFORCO = SPACES OR LOW-VALUE                  EQW9Z0MQ
01820               MOVE 'V '           TO  ECR-VEHFORCO                EQW9Z0MQ
01821            END-IF                                                 EQW9Z0MQ
01822         WHEN '2'                                                  EQW9Z0MQ
01823            MOVE '2R '             TO  ECR-VEHTYPCO                EQW9Z0MQ
01824            IF ECR-VEHFORCO = SPACES OR LOW-VALUE                  EQW9Z0MQ
01825               MOVE 'M '           TO  ECR-VEHFORCO                EQW9Z0MQ
01826            END-IF                                                 EQW9Z0MQ
01827         WHEN '3'                                                  EQW9Z0MQ
01828            MOVE 'CC '             TO  ECR-VEHTYPCO                EQW9Z0MQ
01829            IF ECR-VEHFORCO = SPACES OR LOW-VALUE                  EQW9Z0MQ
01830               MOVE 'V '           TO  ECR-VEHFORCO                EQW9Z0MQ
01831            END-IF                                                 EQW9Z0MQ
01832            MOVE '9999999'         TO  ECR-VEHCODCO                EQW9Z0MQ
01833         WHEN 'M'                                                  EQW9Z0MQ
01834            MOVE VEHTYPC OF TS-VEHICULE(1) TO ECR-VEHTYPCO         EQW9Z0MQ
01833         WHEN 'C'                                                  EQW9Z0MQ
01834            MOVE VEHTYPC OF TS-VEHICULE(1) TO ECR-VEHTYPCO         EQW9Z0MQ
01835      END-EVALUATE.                                                EQW9Z0MQ
01836                                                                   EQW9Z0MQ
F3215 *---GROUPE DU VEHICULE                                            EQW9Z0MQ
F3215      IF VEHGROC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE
F3215         MOVE VEHGROC OF TS-VEHICULE(1) TO  ECR-VEHGROCO           EQW9Z0MQ
F3215      ELSE
F3215         IF VEHGROC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE
F3215            MOVE VEHGROC OF TS-VEHICULE(2) TO  ECR-VEHGROCO        EQW9Z0MQ
F3215         END-IF                                                    EQW9Z0MQ
F3215      END-IF.                                                      EQW9Z0MQ
01814                                                                   EQW9Z0MQ
F3215 *---CLASSE DU VEHICULE                                            EQW9Z0MQ
F3215      IF VEHCLAC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE
F3215         MOVE VEHCLAC OF TS-VEHICULE(1) TO  ECR-VEHCLACO           EQW9Z0MQ
F3215      ELSE
F3215         IF VEHCLAC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE
F3215            MOVE VEHCLAC OF TS-VEHICULE(2) TO  ECR-VEHCLACO        EQW9Z0MQ
F3215         END-IF                                                    EQW9Z0MQ
F3215      END-IF.                                                      EQW9Z0MQ
01814                                                                   EQW9Z0MQ
01837  FIN-REMP-ZONES-PROT.                                             EQW9Z0MQ
01838      EXIT.                                                        EQW9Z0MQ
01839 *                                                                 EQW9Z0MQ
01840 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
01841 * ZONES NON PROTEGEES * FB04 * TRAITEMENT AUTOMATIQUE             EQW9Z0MQ
01842 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
01843  REMPLISSAGE-ZONES-NO-PROTEGEES.                                  EQW9Z0MQ
01844 *------------------------------*                                  EQW9Z0MQ
01845                                                                   EQW9Z0MQ
36835                                                                   EQW90RZ9
36835 *---NOM DE LA PERSONNE TITULAIRE DE LA CARTE GRISE                EQW90RZ9
36835      IF VEHCGNC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW90RZ9
36835         MOVE VEHCGNC OF TS-VEHICULE(1) TO ECR-VEHCGNCO            EQW90RZ9
36835      ELSE                                                         EQW90RZ9
36835         IF VEHCGNC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW90RZ9
36835            MOVE VEHCGNC OF TS-VEHICULE(2) TO ECR-VEHCGNCO         EQW90RZ9
36835         END-IF                                                    EQW90RZ9
36835      END-IF.                                                      EQW90RZ9
36835                                                                   EQW90RZ9
36835 *---PRENOM DE LA PERSONNE TITULAIRE DE LA CARTE GRISE             EQW90RZ9
36835      IF VEHCGPC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW90RZ9
36835         MOVE VEHCGPC OF TS-VEHICULE(1) TO ECR-VEHCGPCO            EQW90RZ9
36835      ELSE                                                         EQW90RZ9
36835         IF VEHCGPC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW90RZ9
36835            MOVE VEHCGPC OF TS-VEHICULE(2) TO ECR-VEHCGPCO         EQW90RZ9
36835         END-IF                                                    EQW90RZ9
36835      END-IF.                                                      EQW90RZ9
36835
36835 *---STATUT DE LA PERSONNE TITULAIRE DE LA CARTE GRISE             EQW90RZ9
36835      IF VEHCGSC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW90RZ9
36835         MOVE VEHCGSC OF TS-VEHICULE(1) TO ECR-VEHCGSCO            EQW90RZ9
36835      ELSE                                                         EQW90RZ9
36835         IF VEHCGSC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW90RZ9
36835            MOVE VEHCGSC OF TS-VEHICULE(2) TO ECR-VEHCGSCO         EQW90RZ9
36835         END-IF                                                    EQW90RZ9
36835      END-IF.                                                      EQW90RZ9
36835                                                                   EQW90RZ9
01846 *---CODE AUTO                                                     EQW9Z0MQ
01847      IF WSS-APPEL-AIDE-CDVEHI = 'N'                               EQW9Z0MQ
01848         IF VEHCODC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
01849            MOVE VEHCODC OF TS-VEHICULE(1) TO ECR-VEHCODCO         EQW9Z0MQ
01850         ELSE                                                      EQW9Z0MQ
01851            IF VEHCODC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUEEQW9Z0MQ
01852               MOVE VEHCODC OF TS-VEHICULE(2) TO ECR-VEHCODCO      EQW9Z0MQ
01853            END-IF                                                 EQW9Z0MQ
01854         END-IF                                                    EQW9Z0MQ
01855      END-IF.                                                      EQW9Z0MQ
01856                                                                   EQW9Z0MQ
01857 *---ON A 2 ECRAN D'AIDE VEHICULE                                  EQW9Z0MQ
01858 *---CODE AUTO RECUPERE DE L'ECRAN D'AIDE VEHICULE FV08 OU VE10    EQW9Z0MQ
01859 *---COM-MA-CODAUTO RENSEIGNÈ                                      EQW9Z0MQ
01860      IF COM-MA-CODAUTO NOT = SPACES AND LOW-VALUE                 EQW9Z0MQ
01861         MOVE COM-MA-CODAUTO TO ECR-VEHCODCO                       EQW9Z0MQ
01862         MOVE SPACES         TO VEHGENC  OF TS-VEHICULE(1)         EQW9Z0MQ
01863         MOVE SPACES         TO VEHMARL  OF TS-VEHICULE(1)         EQW9Z0MQ
01864         MOVE SPACES         TO VEHMODL  OF TS-VEHICULE(1)         EQW9Z0MQ
01865         MOVE SPACES         TO RVEHCYLN OF TS-VEHICULE(1)         EQW9Z0MQ
01866         MOVE SPACES         TO COM-MA-CODAUTO                     EQW9Z0MQ
01867      END-IF.                                                      EQW9Z0MQ
01868                                                                   EQW9Z0MQ
01869 *---GENRE DU VEHICULE                                             EQW9Z0MQ
01870      IF WSS-APPEL-AIDE-GENRE = 'N'                                EQW9Z0MQ
01871         IF VEHGENC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
01872            MOVE VEHGENC OF TS-VEHICULE(1) TO ECR-VEHGENCO         EQW9Z0MQ
01873         ELSE                                                      EQW9Z0MQ
01874            IF VEHGENC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUEEQW9Z0MQ
01875               MOVE VEHGENC OF TS-VEHICULE(2) TO ECR-VEHGENCO      EQW9Z0MQ
01876            END-IF                                                 EQW9Z0MQ
01877         END-IF                                                    EQW9Z0MQ
01878      END-IF.                                                      EQW9Z0MQ
01879                                                                   EQW9Z0MQ
01880 *---MARQUE                                                        EQW9Z0MQ
01881      IF VEHMARL OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
01882         MOVE VEHMARL OF TS-VEHICULE(1) TO ECR-VEHMARLO            EQW9Z0MQ
01883      ELSE                                                         EQW9Z0MQ
01884         IF VEHMARL OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
01885            MOVE VEHMARL OF TS-VEHICULE(2) TO ECR-VEHMARLO         EQW9Z0MQ
01886         END-IF                                                    EQW9Z0MQ
01887      END-IF.                                                      EQW9Z0MQ
01888                                                                   EQW9Z0MQ
01889 *---MODELE                                                        EQW9Z0MQ
01890      IF VEHMODL OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
01891         MOVE VEHMODL OF TS-VEHICULE(1) TO ECR-VEHMODLO            EQW9Z0MQ
01892      ELSE                                                         EQW9Z0MQ
01893         IF VEHMODL OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
01894            MOVE VEHMODL OF TS-VEHICULE(2) TO ECR-VEHMODLO         EQW9Z0MQ
01895         END-IF                                                    EQW9Z0MQ
01896      END-IF.                                                      EQW9Z0MQ
01897                                                                   EQW9Z0MQ
01898 *---CYLINDRE/PUISSANCE                                            EQW9Z0MQ
01899      IF RVEHCYLN OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE     EQW9Z0MQ
01900         MOVE VEHCYLN OF TS-VEHICULE(1) TO WSS-CYLZ                EQW9Z0MQ
01901         MOVE WSS-CYLZ                  TO ECR-VEHCYLNO            EQW9Z0MQ
01902      ELSE                                                         EQW9Z0MQ
01903         IF RVEHCYLN OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
01904            MOVE VEHCYLN OF TS-VEHICULE(2) TO WSS-CYLZ             EQW9Z0MQ
01905            MOVE WSS-CYLZ                  TO ECR-VEHCYLNO         EQW9Z0MQ
01906         END-IF                                                    EQW9Z0MQ
01907      END-IF.                                                      EQW9Z0MQ
01908                                                                   EQW9Z0MQ
01909 *---USAGE DU VEHICULE                                             EQW9Z0MQ
01910      IF WSS-APPEL-AIDE-USAGE = 'N'                                EQW9Z0MQ
01911        IF (VEHUSAC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE)  EQW9Z0MQ
01912          OR (VEHPROC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE)EQW9Z0MQ
01913            MOVE VEHUSAC OF TS-VEHICULE(1) TO ECR-VEHUSACO(1:1)    EQW9Z0MQ
01914            MOVE VEHPROC OF TS-VEHICULE(1) TO ECR-VEHUSACO(2:2)    EQW9Z0MQ
01915        ELSE                                                       EQW9Z0MQ
01916          IF (VEHUSAC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE)EQW9Z0MQ
01917          OR (VEHPROC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE)EQW9Z0MQ
01918               MOVE VEHUSAC OF TS-VEHICULE(2) TO ECR-VEHUSACO(1:1) EQW9Z0MQ
01919               MOVE VEHPROC OF TS-VEHICULE(2) TO ECR-VEHUSACO(2:2) EQW9Z0MQ
01920          END-IF                                                   EQW9Z0MQ
01921        END-IF                                                     EQW9Z0MQ
01922      END-IF.                                                      EQW9Z0MQ
01923                                                                   EQW9Z0MQ
01924 *---IMMATRICULATION                                               EQW9Z0MQ
01925      IF VEHIMMX OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
01926         MOVE VEHIMMX OF TS-VEHICULE(1) TO ECR-VEHIMMXO            EQW9Z0MQ
01927      ELSE                                                         EQW9Z0MQ
01928         IF VEHIMMX OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
01929            MOVE VEHIMMX OF TS-VEHICULE(2) TO ECR-VEHIMMXO         EQW9Z0MQ
01930         END-IF                                                    EQW9Z0MQ
01931      END-IF.                                                      EQW9Z0MQ
01932                                                                   EQW9Z0MQ
01933 *---DATE DE PREMIERE MISE EN CIRCULATION                          EQW9Z0MQ
01934      IF RVEHCIRD OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE     EQW9Z0MQ
01935         MOVE RVEHCIRD OF TS-VEHICULE(1)  TO WSS-SSAAMMJJ          EQW9Z0MQ
01936         MOVE CORRESPONDING WSS-SSAAMMJJ  TO WSS-JJMMSSAA          EQW9Z0MQ
01937         MOVE WSS-JJMMSSAA(3:6)           TO ECR-VEHCIRDO          EQW9Z0MQ
01938      ELSE                                                         EQW9Z0MQ
01939         IF RVEHCIRD OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
01940            MOVE RVEHCIRD OF TS-VEHICULE(2)  TO WSS-SSAAMMJJ       EQW9Z0MQ
01941            MOVE CORRESPONDING WSS-SSAAMMJJ  TO WSS-JJMMSSAA       EQW9Z0MQ
01942            MOVE WSS-JJMMSSAA(3:6)           TO ECR-VEHCIRDO       EQW9Z0MQ
01943         END-IF                                                    EQW9Z0MQ
01944      END-IF.                                                      EQW9Z0MQ
01945                                                                   EQW9Z0MQ
F3576 *---DATE D'ACQUISITION DU VEHICULE                                EQW9Z0MQ
F3576      IF RVEHACQD OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE     EQW9Z0MQ
F3576         MOVE RVEHACQD OF TS-VEHICULE(1)  TO WSS-SSAAMMJJ          EQW9Z0MQ
F3576         MOVE CORRESPONDING WSS-SSAAMMJJ  TO WSS-JJMMSSAA          EQW9Z0MQ
F3576         MOVE WSS-JJMMSSAA(3:6)           TO ECR-VEHACQDO          EQW9Z0MQ
F3576      ELSE                                                         EQW9Z0MQ
F3576         IF RVEHACQD OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
F3576            MOVE RVEHACQD OF TS-VEHICULE(2)  TO WSS-SSAAMMJJ       EQW9Z0MQ
F3576            MOVE CORRESPONDING WSS-SSAAMMJJ  TO WSS-JJMMSSAA       EQW9Z0MQ
F3576            MOVE WSS-JJMMSSAA(3:6)           TO ECR-VEHACQDO       EQW9Z0MQ
F3576         END-IF                                                    EQW9Z0MQ
F3576      END-IF.                                                      EQW9Z0MQ
F3576                                                                   EQW9Z0MQ
01946 *---VALEUR ‡ NEUF                                                 EQW9Z0MQ
01947      IF RVEHVALM OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE     EQW9Z0MQ
01948         MOVE VEHVALM OF TS-VEHICULE(1) TO WSS-VALNEUFZ            EQW9Z0MQ
01949         MOVE WSS-VALNEUFZ              TO ECR-VEHVALMO            EQW9Z0MQ
01950      ELSE                                                         EQW9Z0MQ
01951         IF RVEHVALM OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
01952            MOVE VEHVALM OF TS-VEHICULE(2) TO WSS-VALNEUFZ         EQW9Z0MQ
01953            MOVE WSS-VALNEUFZ              TO ECR-VEHVALMO         EQW9Z0MQ
01954         END-IF                                                    EQW9Z0MQ
01955      END-IF.                                                      EQW9Z0MQ
01956                                                                   EQW9Z0MQ
01957 *---TYPE DE PROTECTION VOL                                        EQW9Z0MQ
01958      IF WSS-APPEL-AIDE-PROT = 'N'                                 EQW9Z0MQ
01959         IF VEHPRTC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
01960            MOVE VEHPRTC OF TS-VEHICULE(1) TO ECR-VEHPRTCO         EQW9Z0MQ
01961         ELSE                                                      EQW9Z0MQ
01962            IF VEHPRTC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUEEQW9Z0MQ
01963               MOVE VEHPRTC OF TS-VEHICULE(2) TO ECR-VEHPRTCO      EQW9Z0MQ
01964            END-IF                                                 EQW9Z0MQ
01965         END-IF                                                    EQW9Z0MQ
01966      END-IF.                                                      EQW9Z0MQ
01967                                                                   EQW9Z0MQ
01968 *---TYPE DE GARAGE                                                EQW9Z0MQ
01969      IF GARCODC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
01970         MOVE GARCODC OF TS-VEHICULE(1) TO ECR-GARCODCO            EQW9Z0MQ
01971      ELSE                                                         EQW9Z0MQ
01972         IF GARCODC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
01973            MOVE GARCODC OF TS-VEHICULE(2) TO ECR-GARCODCO         EQW9Z0MQ
01974         END-IF                                                    EQW9Z0MQ
01975      END-IF.                                                      EQW9Z0MQ
01976                                                                   EQW9Z0MQ
01977 *---MODE D'ACQUISITION (LOA/LLD)                                  EQW9Z0MQ
01978      IF VEHPOSC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
01979         MOVE VEHPOSC OF TS-VEHICULE(1) TO ECR-VEHPOSCO            EQW9Z0MQ
01980      ELSE                                                         EQW9Z0MQ
01981         IF VEHPOSC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
01982            MOVE VEHPOSC OF TS-VEHICULE(2) TO ECR-VEHPOSCO         EQW9Z0MQ
01983         END-IF                                                    EQW9Z0MQ
01984      END-IF.                                                      EQW9Z0MQ
01985                                                                   EQW9Z0MQ
01986 *---INDICATEUR GARANTIE PERTE FINANCIERES                         EQW9Z0MQ
01987      IF VEHPEFC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
01988         MOVE VEHPEFC OF TS-VEHICULE(1) TO ECR-VEHPEFCO            EQW9Z0MQ
01989      ELSE                                                         EQW9Z0MQ
01990         IF VEHPEFC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
01991            MOVE VEHPEFC OF TS-VEHICULE(2) TO ECR-VEHPEFCO         EQW9Z0MQ
01992         END-IF                                                    EQW9Z0MQ
01993      END-IF.                                                      EQW9Z0MQ
01994                                                                   EQW9Z0MQ
01995 *---CODE POSTAL                                                   EQW9Z0MQ
01996      IF WSS-APPEL-AIDE-CP = 'N'                                   EQW9Z0MQ
01997         IF GARCOPC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
01998            MOVE GARCOPC OF TS-VEHICULE(1) TO ECR-GARCOPCO         EQW9Z0MQ
01999         ELSE                                                      EQW9Z0MQ
02000            IF GARCOPC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUEEQW9Z0MQ
02001               MOVE GARCOPC OF TS-VEHICULE(2) TO ECR-GARCOPCO      EQW9Z0MQ
02002            END-IF                                                 EQW9Z0MQ
02003         END-IF                                                    EQW9Z0MQ
02004      END-IF.                                                      EQW9Z0MQ
02005                                                                   EQW9Z0MQ
02006 *---CODE POSTAL RECUPERE DE L'ECRAN D'AIDE MA85                   EQW9Z0MQ
02007      IF COM-MA-CODPOST NOT = SPACES AND LOW-VALUE                 EQW9Z0MQ
02008         MOVE COM-MA-CODPOST TO ECR-GARCOPCO                       EQW9Z0MQ
02009         MOVE SPACES         TO COM-MA-CODPOST                     EQW9Z0MQ
02010      END-IF.                                                      EQW9Z0MQ
02011                                                                   EQW9Z0MQ
02012 *---COMMUNE DU LIEU DE GARAGE                                     EQW9Z0MQ
02013      IF WSS-APPEL-AIDE-COM = 'N'                                  EQW9Z0MQ
02014         IF GARVILL OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
02015            MOVE GARVILL OF TS-VEHICULE(1) TO ECR-GARVILLO         EQW9Z0MQ
02016         ELSE                                                      EQW9Z0MQ
02017            IF GARVILL OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUEEQW9Z0MQ
02018               MOVE GARVILL OF TS-VEHICULE(2) TO ECR-GARVILLO      EQW9Z0MQ
02019            END-IF                                                 EQW9Z0MQ
02020         END-IF                                                    EQW9Z0MQ
02021      END-IF.                                                      EQW9Z0MQ
02022                                                                   EQW9Z0MQ
02023 *---COMMUNE RECUPERE DE L'ECRAN D'AIDE MA85                       EQW9Z0MQ
02024      IF COM-MA-COMMUNE NOT = SPACES AND LOW-VALUE                 EQW9Z0MQ
02025         MOVE COM-MA-COMMUNE TO ECR-GARVILLO                       EQW9Z0MQ
02026         MOVE SPACES         TO COM-MA-COMMUNE                     EQW9Z0MQ
02027      END-IF.                                                      EQW9Z0MQ
02028                                                                   EQW9Z0MQ
02029 *---INDICATEUR VEHICULE SUPPLEMENTAIRE AU FOYER                   EQW9Z0MQ
02030      IF ANVREPC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
02031         MOVE ANVREPC OF TS-VEHICULE(1) TO ECR-ANVREPCO            EQW9Z0MQ
02032      ELSE                                                         EQW9Z0MQ
02033         IF ANVREPC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
02034            MOVE ANVREPC OF TS-VEHICULE(2) TO ECR-ANVREPCO         EQW9Z0MQ
02035         END-IF                                                    EQW9Z0MQ
02036      END-IF.                                                      EQW9Z0MQ
02037                                                                   EQW9Z0MQ
02038                                                                   EQW9Z0MQ
02039 *---FORMULE                                                       EQW9Z0MQ
02040      IF WSS-APPEL-AIDE-FORM = 'N'                                 EQW9Z0MQ
02041         IF VEHFORC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
02042            MOVE VEHFORC OF TS-VEHICULE(1) TO ECR-VEHFORCO         EQW9Z0MQ
02043         ELSE                                                      EQW9Z0MQ
02044            IF VEHFORC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUEEQW9Z0MQ
02045               MOVE VEHFORC OF TS-VEHICULE(2) TO ECR-VEHFORCO      EQW9Z0MQ
02046            END-IF                                                 EQW9Z0MQ
02047         END-IF                                                    EQW9Z0MQ
02048      END-IF.                                                      EQW9Z0MQ
02049                                                                   EQW9Z0MQ
02050 *---NUMERO DE CONTRAT PRECEDENT                                   EQW9Z0MQ
02051      IF ANVNUMX OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
02052         MOVE ANVNUMX OF TS-VEHICULE(1) TO ECR-ANVNUMXO            EQW9Z0MQ
02053      ELSE                                                         EQW9Z0MQ
02054         IF ANVNUMX OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
02055            MOVE ANVNUMX OF TS-VEHICULE(2) TO ECR-ANVNUMXO         EQW9Z0MQ
02056         END-IF                                                    EQW9Z0MQ
02057      END-IF.                                                      EQW9Z0MQ
02058                                                                   EQW9Z0MQ
02050 *---NUMERO D'ORDRE DE L'ANCIEN SOUSCRIPTEUR DU CONTRAT PRECEDENT  EQW9Z0MQ
           IF RANVSOUS OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE
              MOVE ANVSOUS OF TS-VEHICULE(1) TO WSS-ORDRE-PERS
           ELSE
020           IF RANVSOUS OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
                 MOVE ANVSOUS OF TS-VEHICULE(2) TO WSS-ORDRE-PERS
02056         END-IF                                                    EQW9Z0MQ
02057      END-IF.                                                      EQW9Z0MQ
           IF  (RWSS-ORDRE-PERS NOT = SPACES AND LOW-VALUE)
           AND (WSS-ORDRE-PERS NUMERIC)
           AND (WSS-ORDRE-PERS > ZERO)
               SET PERSONNE-NON-TROUVE TO TRUE
               PERFORM READ-TS-TECHNIQUE THRU FIN-READ-TS-TECHNIQUE
               PERFORM VARYING I-RANG FROM 1 BY 1
                       UNTIL I-RANG > 40
                       OR TAB-PERS(I-RANG) = (SPACES OR LOW-VALUE)
                       OR PERSONNE-TROUVE
                  MOVE PERRANTS OF TS-TECHNIQUE(I-RANG) TO
                                                  COM-FB-RANG-TSPERS
                  PERFORM READ-TS-PERSONNE THRU FIN-READ-TS-PERSONNE
                  IF  (PERORDX OF TS-PERSONNE(1) = WSS-ORDRE-PERS OR
                       PERORDX OF TS-PERSONNE(2) = WSS-ORDRE-PERS)
                  AND (RPERSORD OF TS-PERSONNE(1) > SPACES AND
                       PERSORD OF TS-PERSONNE(1) = '99999999')
                      PERFORM VARYING I FROM 1 BY 1
                              UNTIL I > 4
                         EVALUATE PERSTAC OF TS-PERSONNE(1)
                            WHEN 'EN'
                                IF PRMTYPC OF TS-PERSONNE(1, I) > SPACES
                                   MOVE 'ENAP'   TO ECR-ANVSOUSO
                                   MOVE WSS-ORDRE-PERS TO ECR-ANVSOUNO
                                END-IF
                            WHEN 'PM'
                                 MOVE 'SOUS'            TO ECR-ANVSOUSO
                                 MOVE SPACES            TO ECR-ANVSOUNO
                            WHEN OTHER
                                 MOVE PERSTAC OF TS-PERSONNE(1)
                                                        TO ECR-ANVSOUSO
                                 MOVE SPACES            TO ECR-ANVSOUNO
                          END-EVALUATE
                      END-PERFORM
                      SET PERSONNE-TROUVE TO TRUE
                  ELSE
                      MOVE SPACES         TO ECR-ANVSOUSO
                      MOVE SPACES         TO ECR-ANVSOUNO
                  END-IF
               END-PERFORM
           ELSE
               MOVE SPACES                TO ECR-ANVSOUSO
               MOVE SPACES                TO ECR-ANVSOUNO
           END-IF.

      *---ANCIEN SOUSCRIPTEUR DU CONTRAT PRECEDENT ISSUE DE L'ECRAN
      *---CHOIX DES ENFANTS
           IF  COM-MA-ANVSOUS-STATUT NOT = SPACES AND LOW-VALUE
               MOVE COM-MA-ANVSOUS-STATUT     TO ECR-ANVSOUSO
               IF  (COM-MA-ANVSOUS-ORDRE NUMERIC)
               AND (COM-MA-ANVSOUS-ORDRE > ZERO)
                   MOVE COM-MA-ANVSOUS-ORDRE  TO ECR-ANVSOUNO
               END-IF
               MOVE SPACES                    TO COM-MA-ANVSOUS
           ELSE
               MOVE SPACES                    TO COM-MA-ANVSOUS
           END-IF.
02058                                                                   EQW9Z0MQ
02059 *---PRECEDENT CONTRAT - CODE CIE                                  EQW9Z0MQ
02060      IF WSS-APPEL-AIDE-CIE = 'N'                                  EQW9Z0MQ
02061         IF ANVCIEX OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
02062            MOVE ANVCIEX OF TS-VEHICULE(1) TO ECR-ANVCIEXO         EQW9Z0MQ
02063         ELSE                                                      EQW9Z0MQ
02064           IF ANVCIEX OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE EQW9Z0MQ
02065              MOVE ANVCIEX OF TS-VEHICULE(2) TO ECR-ANVCIEXO       EQW9Z0MQ
02066           END-IF                                                  EQW9Z0MQ
02067         END-IF                                                    EQW9Z0MQ
02068      END-IF.                                                      EQW9Z0MQ
02099 *---PRECEDENT CONTRAT - CODE CIE DE L'ECRAN D'AIDE                EQW90UNO
           IF COM-MA-CODCIEC NOT = SPACES AND LOW-VALUE
              MOVE COM-MA-CODCIEC TO ECR-ANVCIEXO
              MOVE SPACES         TO COM-MA-CODCIEC
02068      END-IF.                                                      EQW9Z0MQ
02069                                                                   EQW9Z0MQ
02070 *---LIBELLE COMPAGNIE PRECEDENTE                                  EQW9Z0MQ
02071      IF ANVCIEL OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
02072         MOVE ANVCIEL OF TS-VEHICULE(1) TO ECR-ANVCIELO            EQW9Z0MQ
02073      ELSE                                                         EQW9Z0MQ
02074         IF ANVCIEL OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
02075            MOVE ANVCIEL OF TS-VEHICULE(2) TO ECR-ANVCIELO         EQW9Z0MQ
02076         ELSE                                                      EQW9Z0MQ
02077            IF WSS-APPEL-AIDE-CIE = 'N'                            EQW9Z0MQ
02078               MOVE SPACES               TO ECR-ANVCIELO           EQW9Z0MQ
02079            END-IF                                                 EQW9Z0MQ
02080         END-IF                                                    EQW9Z0MQ
02081      END-IF.                                                      EQW9Z0MQ
02110 *---LIBELLE COMPAGNIE PRECEDENTE DE L'ECRAN D'AIDE                EQW90UNO
           IF COM-MA-LIBCIEL NOT = SPACES AND LOW-VALUE AND 'SANS'
              MOVE COM-MA-LIBCIEL TO ECR-ANVCIELO
              MOVE SPACES         TO COM-MA-LIBCIEL
02121      END-IF.                                                      EQW90UNO
02082                                                                   EQW9Z0MQ
02083 *---NOMBRE DE MOIS D'ASSURANCE DU CONTRAT PRECEDENT               EQW9Z0MQ
02084      IF RANVANCN OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE     EQW9Z0MQ
02085         MOVE ANVANCN OF TS-VEHICULE(1) TO ECR-ANVANCNO            EQW9Z0MQ
02086      ELSE                                                         EQW9Z0MQ
02087         IF RANVANCN OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
02088            MOVE ANVANCN OF TS-VEHICULE(2) TO ECR-ANVANCNO         EQW9Z0MQ
02089         END-IF                                                    EQW9Z0MQ
02090      END-IF.                                                      EQW9Z0MQ
02091                                                                   EQW9Z0MQ
02092 *---NOMBRE DE MOIS D'INTERRUPTION                                 EQW9Z0MQ
02093      IF RANVINTN OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE     EQW9Z0MQ
02094         MOVE ANVINTN OF TS-VEHICULE(1) TO ECR-ANVINTNO            EQW9Z0MQ
02095      ELSE                                                         EQW9Z0MQ
02096         IF RANVINTN OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
02097            MOVE ANVINTN OF TS-VEHICULE(2) TO ECR-ANVINTNO         EQW9Z0MQ
02098         END-IF                                                    EQW9Z0MQ
02099      END-IF.                                                      EQW9Z0MQ
02100                                                                   EQW9Z0MQ
02101 *---DATE DE RESIL DU CONTRAT PRECEDENT                            EQW9Z0MQ
02102      IF RANVRESD OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE     EQW9Z0MQ
02103         MOVE RANVRESD OF TS-VEHICULE(1) TO WSS-SSAAMMJJ           EQW9Z0MQ
02104         MOVE CORRESPONDING WSS-SSAAMMJJ TO WSS-JJMMSSAA           EQW9Z0MQ
02105         MOVE WSS-JJMMSSAA(3:6)          TO ECR-ANVRESDO           EQW9Z0MQ
02106      ELSE                                                         EQW9Z0MQ
02107         IF RANVRESD OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
02108            MOVE RANVRESD OF TS-VEHICULE(2) TO WSS-SSAAMMJJ        EQW9Z0MQ
02109            MOVE CORRESPONDING WSS-SSAAMMJJ TO WSS-JJMMSSAA        EQW9Z0MQ
02110            MOVE WSS-JJMMSSAA(3:6)          TO ECR-ANVRESDO        EQW9Z0MQ
02111         END-IF                                                    EQW9Z0MQ
02112      END-IF.                                                      EQW9Z0MQ
02113                                                                   EQW9Z0MQ
02114 *---INDICATEUR DE PRESENCE DE SINISTRES                           EQW9Z0MQ
02115      IF SIVINDC OF TS-VEHICULE(1) NOT = SPACE AND LOW-VALUE       EQW9Z0MQ
02116         MOVE SIVINDC OF TS-VEHICULE(1) TO ECR-SIVINDCO            EQW9Z0MQ
02117      END-IF.                                                      EQW9Z0MQ
02118                                                                   EQW9Z0MQ
02101 *---MOTIF DE RESIL DU CONTRAT PRECEDENT                           EQW9Z0MQ
02102      IF ANVMTRC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
02103         MOVE ANVMTRC OF TS-VEHICULE(1) TO ECR-ANVMTRCO            EQW9Z0MQ
02106      ELSE                                                         EQW9Z0MQ
02107         IF ANVMTRC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
02108            MOVE ANVMTRC OF TS-VEHICULE(2) TO ECR-ANVMTRCO         EQW9Z0MQ
02111         END-IF                                                    EQW9Z0MQ
02112      END-IF.                                                      EQW9Z0MQ
02113                                                                   EQW9Z0MQ
02119 *---ANCIEN COEFFICIENT DE REDUCTION-MAJORATION                    EQW9Z0MQ
02120      IF RANVBONT OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE     EQW9Z0MQ
02121         COMPUTE WSS-CRMZ ROUNDED = ANVBONT OF TS-VEHICULE(1) * 100EQW9Z0MQ
02122         MOVE WSS-CRMZ                  TO ECR-ANVBONTO            EQW9Z0MQ
02123      ELSE                                                         EQW9Z0MQ
02124         IF RANVBONT OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
02125            COMPUTE WSS-CRMZ ROUNDED =                             EQW9Z0MQ
02126                                   ANVBONT OF TS-VEHICULE(2) * 100 EQW9Z0MQ
02127            MOVE WSS-CRMZ                  TO ECR-ANVBONTO         EQW9Z0MQ
02128      END-IF.                                                      EQW9Z0MQ
02129                                                                   EQW9Z0MQ
02130 *---DATE D'ACQUISITION DU COEFFICIENT DE REDUCTION-MAJORATION     EQW9Z0MQ
02131      IF RANVBOND OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE     EQW9Z0MQ
02132         MOVE RANVBOND OF TS-VEHICULE(1) TO WSS-SSAAMMJJ           EQW9Z0MQ
02133         MOVE CORRESPONDING WSS-SSAAMMJJ TO WSS-JJMMSSAA           EQW9Z0MQ
02134         MOVE WSS-JJMMSSAA(3:6)          TO ECR-ANVBONDO           EQW9Z0MQ
02135      ELSE                                                         EQW9Z0MQ
02136         IF RANVBOND OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
02137            MOVE RANVBOND OF TS-VEHICULE(2) TO WSS-SSAAMMJJ        EQW9Z0MQ
02138            MOVE CORRESPONDING WSS-SSAAMMJJ TO WSS-JJMMSSAA        EQW9Z0MQ
02139            MOVE WSS-JJMMSSAA(3:6)          TO ECR-ANVBONDO        EQW9Z0MQ
02140         END-IF                                                    EQW9Z0MQ
02141      END-IF.                                                      EQW9Z0MQ
02142                                                                   EQW9Z0MQ
02143 *---DATE D'ACQUISITION DU CRM = 050                               EQW9Z0MQ
02144      IF RANVABOD OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE     EQW9Z0MQ
02145         MOVE RANVABOD OF TS-VEHICULE(1) TO WSS-SSAAMMJJ           EQW9Z0MQ
02146         MOVE CORRESPONDING WSS-SSAAMMJJ TO WSS-JJMMSSAA           EQW9Z0MQ
02147         MOVE WSS-SSAA OF WSS-JJMMSSAA   TO ECR-ANVABODO           EQW9Z0MQ
02148      ELSE                                                         EQW9Z0MQ
02149         IF RANVABOD OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
02150            MOVE RANVABOD OF TS-VEHICULE(2) TO WSS-SSAAMMJJ        EQW9Z0MQ
02151            MOVE CORRESPONDING WSS-SSAAMMJJ TO WSS-JJMMSSAA        EQW9Z0MQ
02152            MOVE WSS-SSAA OF WSS-JJMMSSAA   TO ECR-ANVABODO        EQW9Z0MQ
02153         END-IF                                                    EQW9Z0MQ
02154      END-IF.                                                      EQW9Z0MQ
02155                                                                   EQW9Z0MQ
02156 *                                                                 EQW9Z0MQ
02157  FIN-REMP-ZONES-NO-PROT.                                          EQW9Z0MQ
02158      EXIT.                                                        EQW9Z0MQ
02159 /                                                                 EQW9Z0MQ
02160 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
02161 * MODULE DE BASE    * FB04 * TRAITEMENT NORMAL                    EQW9Z0MQ
02162 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
02163 *                                                               * EQW9Z0MQ
02164 *              -----------------------------------              * EQW9Z0MQ
02165 *              I  TRAITEMENT            NORMAL   I              * EQW9Z0MQ
02166 *              -----------------------------------              * EQW9Z0MQ
02167 *                               I                               * EQW9Z0MQ
02168 *           -----------------------------------------           * EQW9Z0MQ
02169 *           I                   I                   I           * EQW9Z0MQ
02170 * -------------------- -------------------- ------------------- * EQW9Z0MQ
02171 * I CONTROLE SYNTAXE I I CONTROLE LOGIQUE I I TRAITEMENT TACHE I* EQW9Z0MQ
02172 * -------------------- -------------------- ------------------- * EQW9Z0MQ
02173 *                                                   I           * EQW9Z0MQ
02174 *                                           ------------------- * EQW9Z0MQ
02175 *                                           I DETERMI-ECR-SUIV I* EQW9Z0MQ
02176 *                                           ------------------- * EQW9Z0MQ
02177 *                                                               * EQW9Z0MQ
02178 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
02179 *                                                                 EQW9Z0MQ
02180  M-TRAITEMENT-NORMAL.                                             EQW9Z0MQ
02181 * ----------------- *                                             EQW9Z0MQ
02182       PERFORM RESTAURATION-ATTRIBUTS THRU                         EQW9Z0MQ
02183               FIN-RESTAURATION-ATTRIBUTS.                         EQW9Z0MQ
02184 *                                                                 EQW9Z0MQ
02185       PERFORM CONTROLE-SYNTAXE THRU                               EQW9Z0MQ
02186               FIN-CONTROLE-SYNTAXE.                               EQW9Z0MQ
02187 *                                                                 EQW9Z0MQ
02188 * PAS DE CONTROLE SI LE SUSPENS EST EMIS                          EQW9Z0MQ
02189       IF OK AND (INF-ETAT OF TS-SUSPENS1 NOT = '5' AND '7')       EQW9Z0MQ
02190          PERFORM CONTROLE-LOGIQUE THRU                            EQW9Z0MQ
02191                  FIN-CONTROLE-LOGIQUE                             EQW9Z0MQ
02192 *                                                                 EQW9Z0MQ
02193 *---NECESSAIRE EN REPRISE DE SUSPENS APRÈS EMMISSION              EQW9Z0MQ
02194 *   CAR ALORS PAS ACCES AU CONTROLE LOGIQUE                       EQW9Z0MQ
02195       ELSE                                                        EQW9Z0MQ
02196          MOVE GARINSC OF TS-VEHICULE(1)                           EQW9Z0MQ
02197                                    TO   WSS-CODE-INSEE            EQW9Z0MQ
02198       END-IF.                                                     EQW9Z0MQ
02199 *                                                                 EQW9Z0MQ
02200       IF OK                                                       EQW9Z0MQ
02201          IF  ECRAN-MODIFIE                                        EQW9Z0MQ
02202          OR (NOT ECRAN-MODIFIE AND COM-GENE-REAF = ' ')           EQW9Z0MQ
02203             PERFORM TRAITEMENT-TACHE THRU                         EQW9Z0MQ
02204                     FIN-TRAITEMENT-TACHE                          EQW9Z0MQ
02205          END-IF                                                   EQW9Z0MQ
02206 *                                                                 EQW9Z0MQ
02207         EVALUATE  TRUE                                            EQW9Z0MQ
02208             WHEN  W-REAF        =  ' '                            EQW9Z0MQ
02209              AND  COM-GENE-REAF =  'O'                            EQW9Z0MQ
02210                   MOVE SPACES            TO  COM-GENE-REAF        EQW9Z0MQ
02211                   MOVE SPACES            TO  COM-GENE-MESINF      EQW9Z0MQ
02212             WHEN  W-REAF        =  ' '                            EQW9Z0MQ
02213              AND  COM-GENE-REAF =  ' '                            EQW9Z0MQ
02214                   CONTINUE                                        EQW9Z0MQ
02215             WHEN  W-REAF        =  'O'                            EQW9Z0MQ
02216              AND  COM-GENE-REAF =  ' '                            EQW9Z0MQ
02217                   MOVE 'O'               TO  COM-GENE-REAF        EQW9Z0MQ
02218             WHEN  W-REAF        =  'O'                            EQW9Z0MQ
02219              AND  COM-GENE-REAF =  'O'                            EQW9Z0MQ
02220                   EVALUATE  TRUE                                  EQW9Z0MQ
02221                       WHEN  ECRAN-MODIFIE                         EQW9Z0MQ
02222                             CONTINUE                              EQW9Z0MQ
02223                       WHEN  OTHER                                 EQW9Z0MQ
02224                             MOVE SPACES      TO  COM-GENE-REAF    EQW9Z0MQ
02225                             MOVE SPACES      TO  COM-GENE-MESINF  EQW9Z0MQ
02226                  END-EVALUATE                                     EQW9Z0MQ
02227         END-EVALUATE                                              EQW9Z0MQ
02228         IF  COM-GENE-REAF   =  'O'                                EQW9Z0MQ
02229             MOVE CODE-TRAITEMENT-AUTOMATIQUE  TO  Z-FONCTION      EQW9Z0MQ
02230         ELSE                                                      EQW9Z0MQ
02231             IF  ECR-XCDECO  =  LOW-VALUE  OR  SPACES              EQW9Z0MQ
02232                 PERFORM  DETERMINATION-ECR-SUIV  THRU             EQW9Z0MQ
02233                          FIN-DETERMINATION-ECR-SUIV               EQW9Z0MQ
02234             END-IF                                                EQW9Z0MQ
02235         END-IF                                                    EQW9Z0MQ
02236      END-IF.                                                      EQW9Z0MQ
02237  FIN-M-TRAITEMENT-NORMAL.                                         EQW9Z0MQ
02238      EXIT.                                                        EQW9Z0MQ
02239 /                                                                 EQW9Z0MQ
02240 ***************************************************************** EQW9Z0MQ
02241 *    RESTAURATION DES ATTRIBUTS AVANT CONTROLES                   EQW9Z0MQ
02242 ***************************************************************** EQW9Z0MQ
02243 *                                                                 EQW9Z0MQ
02244  RESTAURATION-ATTRIBUTS.                                          EQW9Z0MQ
02245 *-----------------------                                          EQW9Z0MQ
AD548      MOVE 'OUI'      TO  WSS-CTRL-ANTECEDENT
02246      MOVE NOR-ASK    TO  ECR-XTRMTRACA.                           EQW9Z0MQ
02247      MOVE NOR-ASK    TO  ECR-XAPPLILA.                            EQW9Z0MQ
02248      MOVE NOR-ASK    TO  ECR-XJOURDA.                             EQW9Z0MQ
02249      MOVE NOR-ASK    TO  ECR-XRACFLA.                             EQW9Z0MQ
02250      MOVE NOR-ASK    TO  ECR-XHEUREDA.                            EQW9Z0MQ
02251      MOVE NOR-ASK    TO  ECR-GESCLIA.                             EQW9Z0MQ
02252      MOVE NOR-ASK    TO  ECR-RAICA.                               EQW9Z0MQ
02253      MOVE NOR-ASK    TO  ECR-NOMCA.                               EQW9Z0MQ
36835      MOVE NOR-ASK    TO  ECR-VEHCGNCA.                            EQW9Z0MQ
36835      MOVE NOR-ASK    TO  ECR-VEHCGPCA.                            EQW9Z0MQ
36835      MOVE NOR-ASK    TO  ECR-VEHCGSCA.                            EQW9Z0MQ
02254      MOVE NOR-ASK    TO  ECR-VEHTYPCA.                            EQW9Z0MQ
F3215      MOVE NOR-ASK    TO  ECR-VEHGROCA.                            EQW9Z0MQ
F3215      MOVE NOR-ASK    TO  ECR-VEHCLACA.                            EQW9Z0MQ
F3215      MOVE NOR-ASK    TO  ECR-ANVSOUNA.                            EQW9Z0MQ
02278      MOVE NOR-ASK    TO  ECR-ANVCIELA.                            EQW9Z0MQ
02256      MOVE NOR-ASK    TO  ECR-XCDECA.                              EQW9Z0MQ
02255                                                                   EQW9Z0MQ
           IF  (ECR-ANVCIELO NOT = SPACES AND LOW-VALUE)
           AND (ECR-ANVCIEXO = '999')
02278          MOVE BRT-ALP    TO  ECR-ANVCIELA                         EQW9Z0MQ
           END-IF.
02257                                                                   EQW9Z0MQ
02258      IF COM-MA-IND-BLOCAGE = 'O'                                  EQW9Z0MQ
AD548         MOVE 'NON'      TO  WSS-CTRL-ANTECEDENT
02259         MOVE NOR-ASK    TO  ECR-VEHCODCA                          EQW9Z0MQ
02260         MOVE NOR-ASK    TO  ECR-VEHGENCA                          EQW9Z0MQ
02261         MOVE NOR-ASK    TO  ECR-VEHMARLA                          EQW9Z0MQ
02262         MOVE NOR-ASK    TO  ECR-VEHMODLA                          EQW9Z0MQ
02263         MOVE NOR-ASK    TO  ECR-VEHCYLNA                          EQW9Z0MQ
02264         MOVE NOR-ASK    TO  ECR-VEHUSACA                          EQW9Z0MQ
02265         MOVE NOR-ASK    TO  ECR-VEHIMMXA                          EQW9Z0MQ
36835         MOVE NOR-ASK    TO  ECR-VEHCGNCA                          EQW9Z0MQ
36835         MOVE NOR-ASK    TO  ECR-VEHCGPCA                          EQW9Z0MQ
36835         MOVE NOR-ASK    TO  ECR-VEHCGSCA                          EQW9Z0MQ
02266         MOVE NOR-ASK    TO  ECR-VEHCIRDA                          EQW9Z0MQ
F3576         MOVE NOR-ASK    TO  ECR-VEHACQDA                          EQW9Z0MQ
02267         MOVE NOR-ASK    TO  ECR-VEHVALMA                          EQW9Z0MQ
02268         MOVE NOR-ASK    TO  ECR-VEHPRTCA                          EQW9Z0MQ
02269         MOVE NOR-ASK    TO  ECR-GARCODCA                          EQW9Z0MQ
02270         MOVE NOR-ASK    TO  ECR-VEHPOSCA                          EQW9Z0MQ
02271         MOVE NOR-ASK    TO  ECR-VEHPEFCA                          EQW9Z0MQ
02272         MOVE NOR-ASK    TO  ECR-GARCOPCA                          EQW9Z0MQ
02273         MOVE NOR-ASK    TO  ECR-GARVILLA                          EQW9Z0MQ
02274         MOVE NOR-ASK    TO  ECR-ANVREPCA                          EQW9Z0MQ
02275         MOVE NOR-ASK    TO  ECR-VEHFORCA                          EQW9Z0MQ
02276         MOVE NOR-ASK    TO  ECR-ANVNUMXA                          EQW9Z0MQ
02276         MOVE NOR-ASK    TO  ECR-ANVSOUSA                          EQW9Z0MQ
02277         MOVE NOR-ASK    TO  ECR-ANVCIEXA                          EQW9Z0MQ
02278         MOVE NOR-ASK    TO  ECR-ANVCIELA                          EQW9Z0MQ
02279         MOVE NOR-ASK    TO  ECR-ANVANCNA                          EQW9Z0MQ
02280         MOVE NOR-ASK    TO  ECR-ANVINTNA                          EQW9Z0MQ
02281         MOVE NOR-ASK    TO  ECR-ANVRESDA                          EQW9Z0MQ
02282         MOVE NOR-ASK    TO  ECR-SIVINDCA                          EQW9Z0MQ
02281         MOVE NOR-ASK    TO  ECR-ANVMTRCA                          EQW9Z0MQ
02283         MOVE NOR-ASK    TO  ECR-ANVBONTA                          EQW9Z0MQ
02284         MOVE NOR-ASK    TO  ECR-ANVBONDA                          EQW9Z0MQ
02285         MOVE NOR-ASK    TO  ECR-ANVABODA                          EQW9Z0MQ
02286      ELSE                                                         EQW9Z0MQ
02287         MOVE BRT-ALP    TO  ECR-VEHCODCA                          EQW9Z0MQ
02288         MOVE BRT-ALP    TO  ECR-VEHGENCA                          EQW9Z0MQ
02289         MOVE BRT-ALP    TO  ECR-VEHMARLA                          EQW9Z0MQ
02290         MOVE BRT-ALP    TO  ECR-VEHMODLA                          EQW9Z0MQ
02291         MOVE BRT-ALP    TO  ECR-VEHCYLNA                          EQW9Z0MQ
02292         MOVE BRT-ALP    TO  ECR-VEHUSACA                          EQW9Z0MQ
02293         MOVE BRT-ALP    TO  ECR-VEHIMMXA                          EQW9Z0MQ
36835         MOVE BRT-ALP    TO  ECR-VEHCGNCA                          EQW9Z0MQ
36835         MOVE BRT-ALP    TO  ECR-VEHCGPCA                          EQW9Z0MQ
36835         MOVE BRT-ALP    TO  ECR-VEHCGSCA                          EQW9Z0MQ
02294         MOVE BRT-ALP    TO  ECR-VEHCIRDA                          EQW9Z0MQ
F3576         MOVE BRT-ALP    TO  ECR-VEHACQDA                          EQW9Z0MQ
02295         MOVE BRT-ALP    TO  ECR-VEHVALMA                          EQW9Z0MQ
02296         MOVE BRT-ALP    TO  ECR-VEHPRTCA                          EQW9Z0MQ
02297         MOVE BRT-ALP    TO  ECR-GARCODCA                          EQW9Z0MQ
02298         MOVE BRT-ALP    TO  ECR-VEHPOSCA                          EQW9Z0MQ
02299         MOVE BRT-ALP    TO  ECR-VEHPEFCA                          EQW9Z0MQ
02300         MOVE BRT-ALP    TO  ECR-GARCOPCA                          EQW9Z0MQ
02301         MOVE BRT-ALP    TO  ECR-GARVILLA                          EQW9Z0MQ
02302         MOVE BRT-ALP    TO  ECR-ANVREPCA                          EQW9Z0MQ
02303         MOVE BRT-ALP    TO  ECR-VEHFORCA                          EQW9Z0MQ
02304         MOVE BRT-ALP    TO  ECR-ANVNUMXA                          EQW9Z0MQ
02304         MOVE BRT-ALP    TO  ECR-ANVSOUSA                          EQW9Z0MQ
02305         MOVE BRT-ALP    TO  ECR-ANVCIEXA                          EQW9Z0MQ
02307         MOVE BRT-ALP    TO  ECR-ANVANCNA                          EQW9Z0MQ
02308         MOVE BRT-ALP    TO  ECR-ANVINTNA                          EQW9Z0MQ
02309         MOVE BRT-ALP    TO  ECR-ANVRESDA                          EQW9Z0MQ
02310         MOVE BRT-ALP    TO  ECR-SIVINDCA                          EQW9Z0MQ
02309         MOVE BRT-ALP    TO  ECR-ANVMTRCA                          EQW9Z0MQ
02311         MOVE BRT-ALP    TO  ECR-ANVBONTA                          EQW9Z0MQ
02312         MOVE BRT-ALP    TO  ECR-ANVBONDA                          EQW9Z0MQ
02313         MOVE BRT-ALP    TO  ECR-ANVABODA                          EQW9Z0MQ
02314      END-IF.                                                      EQW9Z0MQ
02315                                                                   EQW9Z0MQ
02316 * EN 'AV' OU 'RV' SANS AJOUT DE VEHICULE LES ZONES DES CONNEXES   EQW9Z0MQ
02317 * NE SONT PAS MODIFIABLES                                         EQW9Z0MQ
33295 * SAUF EN 'AV','RV' AVEC FORCAGE A 'F'                            00269500
33295      MOVE 'N'                TO  WS-FORCAGE.
33295
33295      IF  CCO-FORCAG1 OF TS-SUSPENS1 = 'F'
33295       OR CCO-FORCAG2 OF TS-SUSPENS1 = 'F'
33295       OR CCO-FORCAG3 OF TS-SUSPENS1 = 'F'
33295       OR CCO-FORCAG4 OF TS-SUSPENS1 = 'F'
33295          IF  INF-NATMVT OF TS-SUSPENS1 = 'AV' OR 'RV'             00270200
33295              MOVE 'O'        TO  WS-FORCAGE
33295          END-IF
33295      END-IF.
33295
02318      IF INF-NATMVT OF TS-SUSPENS1 = 'AV' OR 'RV'                  EQW9Z0MQ
02319         IF COM-FB-CODE-ACTION NOT = '1' AND '2' AND '3'           EQW9Z0MQ
02320            IF  (VEHACTC OF TS-VEHICULE(1) NOT = 'I')              EQW9Z0MQ
AD548               MOVE 'NON'      TO  WSS-CTRL-ANTECEDENT
02321               MOVE NOR-ASK    TO  ECR-ANVREPCA                    EQW9Z0MQ
02322               MOVE NOR-ASK    TO  ECR-ANVNUMXA                    EQW9Z0MQ
02322               MOVE NOR-ASK    TO  ECR-ANVSOUSA                    EQW9Z0MQ
02323               MOVE NOR-ASK    TO  ECR-ANVCIEXA                    EQW9Z0MQ
02324               MOVE NOR-ASK    TO  ECR-ANVCIELA                    EQW9Z0MQ
02325               MOVE NOR-ASK    TO  ECR-ANVANCNA                    EQW9Z0MQ
02326               MOVE NOR-ASK    TO  ECR-SIVINDCA                    EQW9Z0MQ
02327               MOVE NOR-ASK    TO  ECR-ANVRESDA                    EQW9Z0MQ
02327               MOVE NOR-ASK    TO  ECR-ANVMTRCA                    EQW9Z0MQ
02328               MOVE NOR-ASK    TO  ECR-ANVINTNA                    EQW9Z0MQ
02329               MOVE NOR-ASK    TO  ECR-ANVBONTA                    EQW9Z0MQ
02330               MOVE NOR-ASK    TO  ECR-ANVBONDA                    EQW9Z0MQ
02331               MOVE NOR-ASK    TO  ECR-ANVABODA                    EQW9Z0MQ
02332            END-IF                                                 EQW9Z0MQ
02333         END-IF                                                    EQW9Z0MQ
02334      END-IF.                                                      EQW9Z0MQ
02335                                                                   EQW9Z0MQ
02316 * EN CAS DE CHANGEMENT DE VEHICULE, LES ZONES DES CONNEXES NE SONTEQW9Z0MQ
02317 * PAS MODIFIABLES                                                 EQW9Z0MQ
02320      IF VEHCHGC OF TS-VEHICULE(1) = '1' OR '2'                    EQW9Z0MQ
AD548         MOVE 'NON'      TO  WSS-CTRL-ANTECEDENT
02321         MOVE NOR-ASK    TO  ECR-ANVREPCA                          EQW9Z0MQ
02322         MOVE NOR-ASK    TO  ECR-ANVNUMXA                          EQW9Z0MQ
02322         MOVE NOR-ASK    TO  ECR-ANVSOUSA                          EQW9Z0MQ
02323         MOVE NOR-ASK    TO  ECR-ANVCIEXA                          EQW9Z0MQ
02324         MOVE NOR-ASK    TO  ECR-ANVCIELA                          EQW9Z0MQ
02325         MOVE NOR-ASK    TO  ECR-ANVANCNA                          EQW9Z0MQ
02326         MOVE NOR-ASK    TO  ECR-SIVINDCA                          EQW9Z0MQ
02327         MOVE NOR-ASK    TO  ECR-ANVRESDA                          EQW9Z0MQ
02327         MOVE NOR-ASK    TO  ECR-ANVMTRCA                          EQW9Z0MQ
02328         MOVE NOR-ASK    TO  ECR-ANVINTNA                          EQW9Z0MQ
02329         MOVE NOR-ASK    TO  ECR-ANVBONTA                          EQW9Z0MQ
02330         MOVE NOR-ASK    TO  ECR-ANVBONDA                          EQW9Z0MQ
02331         MOVE NOR-ASK    TO  ECR-ANVABODA                          EQW9Z0MQ
02334      END-IF.                                                      EQW9Z0MQ
02335                                                                   EQW9Z0MQ
02336 * EN 'AV' OU 'RV' SI AJOUT OU SUPPRESSION DE CONDUCTEURS ON DOIT  EQW9Z0MQ
02337 * POUVOIR MODIFIER LE NBRE DE MOIS DU RELEVE D'INFO ET LE NBRE DE EQW9Z0MQ
02338 * MOIS D'INTERRUPTION D'ASSURANCE                                 EQW9Z0MQ
02339      IF INF-NATMVT OF TS-SUSPENS1 = 'AV' OR 'RV'                  EQW9Z0MQ
02340         IF COM-FB-PERS-AJOUT = 'OUI' OR COM-FB-PERS-SUPPR = 'OUI' EQW9Z0MQ
02341            MOVE BRT-ALP    TO  ECR-ANVANCNA                       EQW9Z0MQ
02342            MOVE BRT-ALP    TO  ECR-ANVINTNA                       EQW9Z0MQ
02343         END-IF                                                    EQW9Z0MQ
02344      END-IF.                                                      EQW9Z0MQ
02345 *                                                                 EQW9Z0MQ
33295 * DÈBLOCAGE DES ANTECEDENTS PAR FORCAGE ==> LAISSER ‡ LA FIN      00275500
33295      IF WS-FORCAGE = 'O'                                          00275600
AD548         MOVE 'OUI'      TO  WSS-CTRL-ANTECEDENT
33295         MOVE BRT-ALP    TO  ECR-ANVREPCA                          00275700
33295         MOVE BRT-ALP    TO  ECR-ANVNUMXA                          00275800
33295         MOVE BRT-ALP    TO  ECR-ANVSOUSA                          00275900
33295         MOVE BRT-ALP    TO  ECR-ANVCIEXA                          00276000
33295         MOVE BRT-ALP    TO  ECR-ANVCIELA                          00276100
33295         MOVE BRT-ALP    TO  ECR-ANVANCNA                          00276200
33295         MOVE BRT-ALP    TO  ECR-SIVINDCA                          00276300
33295         MOVE BRT-ALP    TO  ECR-ANVRESDA                          00276400
33295         MOVE BRT-ALP    TO  ECR-ANVMTRCA                          00276500
33295         MOVE BRT-ALP    TO  ECR-ANVINTNA                          00276600
33295         MOVE BRT-ALP    TO  ECR-ANVBONTA                          00276700
33295         MOVE BRT-ALP    TO  ECR-ANVBONDA                          00276800
33295         MOVE BRT-ALP    TO  ECR-ANVABODA                          00276900
33295      END-IF.                                                      00277000
                                                                        00277100
02346  FIN-RESTAURATION-ATTRIBUTS.                                      EQW9Z0MQ
02347      EXIT.                                                        EQW9Z0MQ
02348 *                                                                 EQW9Z0MQ
02349 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
02350 * CONTROLES SYNTAXIQUES * FB04 * TRAITEMENT NORMAL                EQW9Z0MQ
02351 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
02352 *                                                                 EQW9Z0MQ
02353  CONTROLE-SYNTAXE.                                                EQW9Z0MQ
02354 * -------------- *                                                EQW9Z0MQ
02355 *                                                                 EQW9Z0MQ
02356 * TEST DES CODES MNEMONIQUES POUR L'ENCHAINEMENT DES              EQW9Z0MQ
02357 * TRANSACTIONS DANS UNE CONVERSATION                              EQW9Z0MQ
02358 *                                                                 EQW9Z0MQ
02359      IF ECR-XCDECO NOT = LOW-VALUE AND SPACES                     EQW9Z0MQ
02360         IF ECR-XCDECO = 'AIDE' OR 'GENR' OR 'USAG' OR 'VEHI'      EQW9Z0MQ
02361                                OR 'PROT' OR 'FORM' OR 'CIE'       EQW9Z0MQ
02362            IF ECR-XCDECO = 'AIDE'                                 EQW9Z0MQ
02363               MOVE 'AIDE'              TO COM-MA-GENRE-TXT        EQW9Z0MQ
02364               MOVE 'MA84'              TO NOM-TACHE-XCTL          EQW9Z0MQ
02365            END-IF                                                 EQW9Z0MQ
02366            IF ECR-XCDECO = 'VEHI'                                 EQW9Z0MQ
02367               IF ECR-VEHTYPCO = '2R '                             EQW9Z0MQ
02368                  MOVE 'FV08'           TO NOM-TACHE-XCTL          EQW9Z0MQ
02369               ELSE                                                EQW9Z0MQ
02370                  MOVE 'VE10'           TO NOM-TACHE-XCTL          EQW9Z0MQ
02371               END-IF                                              EQW9Z0MQ
02372               MOVE 'VEHI'              TO COM-MA-GENRE-TXT        EQW9Z0MQ
02373            END-IF                                                 EQW9Z0MQ
02374            IF ECR-XCDECO = 'GENR'                                 EQW9Z0MQ
02375               IF ECR-VEHTYPCO = '2R '                             EQW9Z0MQ
02376                  MOVE 'GEN2'           TO COM-MA-GENRE-TXT        EQW9Z0MQ
02377               ELSE                                                EQW9Z0MQ
02378                  MOVE 'GEN4'           TO COM-MA-GENRE-TXT        EQW9Z0MQ
02379               END-IF                                              EQW9Z0MQ
02380               MOVE 'MA84'              TO NOM-TACHE-XCTL          EQW9Z0MQ
02381            END-IF                                                 EQW9Z0MQ
02382            IF ECR-XCDECO = 'USAG'                                 EQW9Z0MQ
02383               MOVE 'USAG'              TO COM-MA-GENRE-TXT        EQW9Z0MQ
02384               MOVE 'MA84'              TO NOM-TACHE-XCTL          EQW9Z0MQ
02385            END-IF                                                 EQW9Z0MQ
02386            IF ECR-XCDECO = 'PROT'                                 EQW9Z0MQ
02387               MOVE 'PROT'              TO COM-MA-GENRE-TXT        EQW9Z0MQ
02388               MOVE 'MA84'              TO NOM-TACHE-XCTL          EQW9Z0MQ
02389            END-IF                                                 EQW9Z0MQ
02390            IF ECR-XCDECO = 'FORM'                                 EQW9Z0MQ
02391               IF ECR-VEHTYPCO = '2R '                             EQW9Z0MQ
02392                  MOVE 'FOR2'           TO COM-MA-GENRE-TXT        EQW9Z0MQ
02393               ELSE                                                EQW9Z0MQ
02394                  MOVE 'FOR4'           TO COM-MA-GENRE-TXT        EQW9Z0MQ
02395               END-IF                                              EQW9Z0MQ
02396               MOVE 'MA84'              TO NOM-TACHE-XCTL          EQW9Z0MQ
02397            END-IF                                                 EQW9Z0MQ
02398            IF ECR-XCDECO = 'CIE'                                  EQW9Z0MQ
02399               MOVE 'CIE'               TO COM-MA-GENRE-TXT        EQW9Z0MQ
02400               MOVE 'MA86'              TO NOM-TACHE-XCTL          EQW9Z0MQ
02401            END-IF                                                 EQW9Z0MQ
02402         ELSE                                                      EQW9Z0MQ
U3319            PERFORM  CONTROLE-CODE-COMMANDE  THRU                  EFUTSQP3
U3319                     FIN-CONTROLE-CODE-COMMANDE                    EFUTSQP3
02427         END-IF                                                    EQW9Z0MQ
02428      END-IF.                                                      EQW9Z0MQ
02429                                                                   EQW9Z0MQ
02430 *--- DATE DU JOUR                                                 EQW9Z0MQ
02431      MOVE COM-MA-DT-JOURJJ TO WSS-FB-DATJOUR-JJ.                  EQW9Z0MQ
02432      MOVE COM-MA-DT-JOURMM TO WSS-FB-DATJOUR-MM.                  EQW9Z0MQ
02433      MOVE COM-MA-DT-JOURSS TO WSS-FB-DATJOUR-SS.                  EQW9Z0MQ
02434      MOVE COM-MA-DT-JOURAA TO WSS-FB-DATJOUR-AA.                  EQW9Z0MQ
02435                                                                   EQW9Z0MQ
02436 *                                                                 EQW9Z0MQ
02437 * CONTROLE DE SYNTAXE DES ZONES SAISISSABLE                       EQW9Z0MQ
02438 *                                                                 EQW9Z0MQ
02439                                                                   EQW9Z0MQ
02440 *---CODE AUTO                                                     EQW9Z0MQ
02441      IF ECR-VEHCODCO = LOW-VALUE                                  EQW9Z0MQ
02442         MOVE SPACES TO ECR-VEHCODCO                               EQW9Z0MQ
02443      END-IF.                                                      EQW9Z0MQ
02444                                                                   EQW9Z0MQ
02445 *----------DETERMINATION DE L'APPEL A ECRAN D'AIDE CODES VEHICULESEQW9Z0MQ
02446      PERFORM DETER-AIDE-CDVEHI THRU FIN-DETER-AIDE-CDVEHI.        EQW9Z0MQ
02447                                                                   EQW9Z0MQ
02448 *---TYPE DU VEHICULE                                              EQW9Z0MQ
02449      IF ECR-VEHTYPCO = LOW-VALUE                                  EQW9Z0MQ
02450         MOVE SPACES TO ECR-VEHTYPCO                               EQW9Z0MQ
02451      END-IF.                                                      EQW9Z0MQ
02452                                                                   EQW9Z0MQ
02453 *---GENRE DU VEHICULE                                             EQW9Z0MQ
02454      IF ECR-VEHGENCO = LOW-VALUE                                  EQW9Z0MQ
02455         MOVE SPACES TO ECR-VEHGENCO                               EQW9Z0MQ
02456      END-IF.                                                      EQW9Z0MQ
02457                                                                   EQW9Z0MQ
F3215 *---GROUPE DU VEHICULE                                            EQW9Z0MQ
F3215      IF ECR-VEHGROCO = LOW-VALUE                                  EQW9Z0MQ
F3215         MOVE SPACES TO ECR-VEHGROCO                               EQW9Z0MQ
F3215      END-IF.                                                      EQW9Z0MQ
F3215                                                                   EQW9Z0MQ
F3215 *---CLASSE DU VEHICULE                                            EQW9Z0MQ
F3215      IF ECR-VEHCLACO = LOW-VALUE                                  EQW9Z0MQ
F3215         MOVE SPACES TO ECR-VEHCLACO                               EQW9Z0MQ
F3215      END-IF.                                                      EQW9Z0MQ
F3215                                                                   EQW9Z0MQ
02458 *----------DETERMINATION DE L'APPEL A ECRAN D'AIDE DES CODES GENREEQW9Z0MQ
02459      PERFORM DETER-AIDE-GENRE THRU FIN-DETER-AIDE-GENRE.          EQW9Z0MQ
02460                                                                   EQW9Z0MQ
02461 *----------LE CODE GENRE DOIT EXISTER DANS LA TABLE FBGENR01      EQW9Z0MQ
02462      IF ECR-VEHGENCO NOT = SPACES AND WSS-APPEL-AIDE-GENRE = 'N'  EQW9Z0MQ
02463         MOVE  SPACES                    TO XSPIPARM               EQW9Z0MQ
02464         MOVE  'GP'                      TO FONCTION  OF XSPIPARM  EQW9Z0MQ
02465         MOVE  'FBGENR'                  TO TABLE-PREF             EQW9Z0MQ
02466         MOVE  COM-GENE-CODCIE-PRINCIPAL TO TABLE-SUFF             EQW9Z0MQ
02467         MOVE  IDENT-TABLE               TO CODTAB OF XSPIPARM     EQW9Z0MQ
02468         MOVE  '= '                      TO OPERATEUR OF XSPIPARM  EQW9Z0MQ
02469         MOVE  ECR-VEHTYPCO              TO CLE-GENTYPV            EQW9Z0MQ
02470         MOVE  ECR-VEHGENCO              TO CLE-GENRE              EQW9Z0MQ
02471         MOVE  WSS-CLE-FBGENR            TO REF-POSTE OF XSPIPARM  EQW9Z0MQ
02472         PERFORM ACCES-SPI THRU FIN-ACCES-SPI                      EQW9Z0MQ
02473         IF RETCOD OF XSPIPARM  = ZERO                             EQW9Z0MQ
02474            MOVE IOAREA  OF XSPIPARM TO FBGENR01                   EQW9Z0MQ
02475         ELSE                                                      EQW9Z0MQ
02476            MOVE NOR-ALP TO ECR-VEHGENCA                           EQW9Z0MQ
02477            IF OK                                                  EQW9Z0MQ
02478               MOVE 'FB096' TO COM-GENE-MESANO                     EQW9Z0MQ
02479                               COM-CODERR                          EQW9Z0MQ
02480               MOVE CURSEUR TO ECR-VEHGENCL                        EQW9Z0MQ
02481               MOVE 1       TO KONTROL                             EQW9Z0MQ
02482               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02483            END-IF                                                 EQW9Z0MQ
02484         END-IF                                                    EQW9Z0MQ
02485      END-IF.                                                      EQW9Z0MQ
02486                                                                   EQW9Z0MQ
36835 *---CARTE GRISE NOM                                               EQW90UNO
36835      IF ECR-VEHCGNCO = LOW-VALUE                                  EQW90RZ9
36835         MOVE SPACES TO ECR-VEHCGNCO                               EQW90RZ9
36835      END-IF.                                                      EQW90RZ9
36835                                                                   EQW90UNO
36835 *---CARTE GRISE PRENOM                                            EQW90UNO
36835      IF ECR-VEHCGPCO = LOW-VALUE                                  EQW90RZ9
36835         MOVE SPACES TO ECR-VEHCGPCO                               EQW90RZ9
36835      END-IF.                                                      EQW90RZ9
36835                                                                   EQW90UNO
36835 *---CARTE GRISE STATUT                                            EQW90UNO
36835      IF ECR-VEHCGSCO = LOW-VALUE                                  EQW90RZ9
36835         MOVE SPACES TO ECR-VEHCGSCO                               EQW90RZ9
36835      END-IF.                                                      EQW90RZ9
36835
38635 *--- STATUT DE LA PERSONNE TITULAIRE DE LA CARTE GIRSE            EQW90RZ9
36835      MOVE ECR-VEHCGSCO TO W-STATUT.                               EQW90RZ9
36835      IF W-STATUT NOT = 'CF' AND 'CJ' AND 'AU' AND 'PM' AND 'EN'   EQW90RZ9
36835         MOVE NOR-ALP TO ECR-VEHCGSCA                              EQW90RZ9
36835         MOVE 'FB508' TO COM-GENE-MESANO                           EQW90RZ9
36835                         COM-CODERR                                EQW90RZ9
36835         MOVE CURSEUR TO ECR-VEHCGSCL                              EQW90RZ9
36835         MOVE 1       TO KONTROL                                   EQW90RZ9
36835         GO TO FIN-CONTROLE-SYNTAXE                                EQW90RZ9
36835      END-IF.                                                      EQW90RZ9
36835                                                                   EQW90RZ9
36835 *---EN TITULAIRE CG ET 1 SEUL 4R : ETABLIR CONTRAT MONO AU NOM DE EQW90RZ9
36835 *---L'ENFANT                                                      EQW90RZ9
36835 *                                                                 EQW90RZ9
36835 *    MOVE ZERO TO NB-VEHI-TRACTEUR.                               EQW90RZ9
36835 *    IF W-STATUT = 'EN'                                           EQW90RZ9
36835 *       MOVE TS-VEHICULE           TO WSS-TS-VEHICULE             EQW9Z0MQ
36835 *       MOVE COM-FB-RANG-TS-LIRE   TO WSS-RANG-TS-LIRE            EQW9Z0MQ
36835 *       PERFORM READ-TS-TECHNIQUE THRU FIN-READ-TS-TECHNIQUE
36835 *       PERFORM VARYING I-RANG FROM 1 BY 1 UNTIL I-RANG > 40      EQW90RZ9
36835 *                      OR TAB-VEHI(I-RANG) = (SPACES OR LOW-VALUE)EQW90RZ9
36835 *           IF VEHSORD OF TS-TECHNIQUE(I-RANG) = '99999999'       EQW90RZ9
36835 *           AND (VEHRANTS OF TS-TECHNIQUE (I-RANG)                EQW9Z0MQ
36835 *                                          NOT = WSS-RANG-TS-LIRE)EQW9Z0MQ
36835 *              MOVE VEHRANTS OF TS-TECHNIQUE(I-RANG)              EQW90RZ9
36835 *                               TO COM-FB-RANG-TS-LIRE            EQW9Z0MQ
36835 *              PERFORM READ-TS-VEHICULE THRU FIN-READ-TS-VEHICULE EQW90RZ9
36835 *              MOVE COM-FB-NBRE-VEHI-TRACTEUR TO NB-VEHI-TRACTEUR EQW90RZ9
36835 *              IF  (COM-FB-CODE-ACTION = '1' OR 'M')              EQW90RZ9
36835 *              AND  NB-VEHI-TRACTEUR >= 1                         EQW90RZ9
36835 *                   ADD 1 TO NB-VEHI-TRACTEUR                     EQW90RZ9
36835 *              END-IF                                             EQW90RZ9
36835 *              IF NB-VEHI-TRACTEUR < 2                            EQW90RZ9
36835 *                 MOVE NOR-ALP TO ECR-VEHCGSCA                    EQW90RZ9
36835 *                 MOVE 'FB510' TO COM-GENE-MESANO                 EQW90RZ9
36835 *                                 COM-CODERR                      EQW90RZ9
36835 *                 MOVE CURSEUR TO ECR-VEHCGSCL                    EQW90RZ9
36835 *                 MOVE 1       TO KONTROL                         EQW90RZ9
36835 *                 GO TO FIN-CONTROLE-SYNTAXE                      EQW90RZ9
36835 *              END-IF                                             EQW90RZ9
36835 *           END-IF                                                EQW90RZ9
36835 *       END-PERFORM                                               EQW90RZ9
36835 *       MOVE WSS-TS-VEHICULE       TO TS-VEHICULE                 EQW9Z0MQ
36835 *       MOVE WSS-RANG-TS-LIRE      TO COM-FB-RANG-TS-LIRE         EQW9Z0MQ
36835 *    END-IF.                                                      EQW90RZ9
36835 *    IF   W-STATUT = 'EN'                                         EQW90RZ9
36835 *    AND (COM-FB-CODE-ACTION = '1' OR 'M')                        EQW90RZ9
36835 *    AND  NB-VEHI-TRACTEUR = ZERO                                 EQW90RZ9
36835 *        MOVE NOR-ALP TO ECR-VEHCGSCA                             EQW90RZ9
36835 *        MOVE 'FB510' TO COM-GENE-MESANO                          EQW90RZ9
36835 *                        COM-CODERR                               EQW90RZ9
36835 *        MOVE CURSEUR TO ECR-VEHCGSCL                             EQW90RZ9
36835 *        MOVE 1       TO KONTROL                                  EQW90RZ9
36835 *        GO TO FIN-CONTROLE-SYNTAXE                               EQW90RZ9
36835 *    END-IF.                                                      EQW90RZ9
36835                                                                   EQW90RZ9
36835 *---NOM ET PRENOM TITULAIRE CG DIFFERENTS DE CEUX DES CF, CJ, EN  EQW90RZ9
36835      SET TITULAIRE-NON-TROUVE TO TRUE.                            EQW90RZ9
36835      MOVE ECR-VEHCGNCO TO WSS-NOM-CG.                             EQW90RZ9
36835      MOVE ECR-VEHCGPCO TO WSS-PRENOM-CG.                          EQW90RZ9
36835      IF  (WSS-NOM-CG    NOT = SPACES AND LOW-VALUE)
36835      AND (WSS-PRENOM-CG NOT = SPACES AND LOW-VALUE)
36835      AND (W-STATUT NOT = 'AU' AND 'PM')
36835      AND (ECR-VEHPOSCO NOT = 'O')
36835          PERFORM READ-TS-TECHNIQUE THRU FIN-READ-TS-TECHNIQUE
36835          PERFORM VARYING I-RANG FROM 1 BY 1
36835                  UNTIL I-RANG > 40
36835                  OR TAB-PERS(I-RANG) = (SPACES OR LOW-VALUE)
36835                  OR TITULAIRE-TROUVE
36835             IF PERSORD OF TS-TECHNIQUE(I-RANG) = '99999999'
36835                MOVE PERRANTS OF TS-TECHNIQUE(I-RANG) TO
36835                                                COM-FB-RANG-TSPERS
36835                PERFORM READ-TS-PERSONNE THRU FIN-READ-TS-PERSONNE
36835                PERFORM VARYING I FROM 1 BY 1
36835                        UNTIL I > 20
36835                        OR PERNOML OF TS-PERSONNE (I) > SPACES
36835                        OR PERPREL OF TS-PERSONNE (I) > SPACES
36835                END-PERFORM
36835                IF  I NOT > 20
36835                  IF PERNOML OF TS-PERSONNE (I)
36835                                         NOT = SPACES AND LOW-VALUE
36835                     MOVE PERNOML OF TS-PERSONNE (I) TO WSS-NOM
36835                     MOVE WSS-NOM                    TO WSS-TAMPON
36835                     PERFORM MAJUSCULE THRU F-MAJUSCULE
36835                     MOVE WSS-TAMPON                 TO WSS-NOM
36835                  END-IF
36835                  IF PERPREL OF TS-PERSONNE(I)
36835                                         NOT = SPACES AND LOW-VALUE
36835                     MOVE PERPREL OF TS-PERSONNE(I)  TO WSS-PRENOM
36835                     MOVE WSS-PRENOM                 TO WSS-TAMPON
36835                     PERFORM MAJUSCULE THRU F-MAJUSCULE
36835                     MOVE WSS-TAMPON                 TO WSS-PRENOM
36835                  END-IF
F52844                 IF PERSTAC OF TS-PERSONNE (I)
F52844                                       NOT = SPACES AND LOW-VALUE
F52844                    MOVE PERSTAC OF TS-PERSONNE (I) TO WSS-STATUT
F52844                    MOVE WSS-STATUT                 TO WSS-TAMPON
F52844                    PERFORM MAJUSCULE THRU F-MAJUSCULE
F52844                    MOVE WSS-TAMPON                 TO WSS-STATUT
F52844                 END-IF
36835                ELSE
36835                  SET TITULAIRE-NON-TROUVE TO TRUE
36835                END-IF
36835                IF  WSS-NOM = WSS-NOM-CG                           EQW90RZ9
36835                AND WSS-PRENOM  = WSS-PRENOM-CG                    EQW90RZ9
F52844               AND WSS-STATUT  = W-STATUT                         EQW90RZ9
36835                  SET TITULAIRE-TROUVE TO TRUE
36835                END-IF                                             EQW90RZ9
36835             END-IF                                                EQW90RZ9
36835          END-PERFORM                                              EQW90RZ9
36835          IF  TITULAIRE-NON-TROUVE                                 EQW90RZ9
36835              MOVE NOR-ALP TO ECR-VEHCGNCA                         EQW90RZ9
36835              MOVE 'FB509' TO COM-GENE-MESANO                      EQW90RZ9
36835                              COM-CODERR                           EQW90RZ9
36835              MOVE CURSEUR TO ECR-VEHCGNCL                         EQW90RZ9
36835              MOVE 1       TO KONTROL                              EQW90RZ9
36835              GO TO FIN-CONTROLE-SYNTAXE                           EQW90RZ9
36835          END-IF                                                   EQW90RZ9
36835      END-IF.                                                      EQW90RZ9
36835                                                                   EQW90RZ9
02487 *---MARQUE                                                        EQW9Z0MQ
02488      IF ECR-VEHMARLO = LOW-VALUE                                  EQW9Z0MQ
02489         MOVE SPACES TO ECR-VEHMARLO                               EQW9Z0MQ
02490      END-IF.                                                      EQW9Z0MQ
02491                                                                   EQW9Z0MQ
02492 *---MODELE                                                        EQW9Z0MQ
02493      IF ECR-VEHMODLO = LOW-VALUE                                  EQW9Z0MQ
02494         MOVE SPACES TO ECR-VEHMODLO                               EQW9Z0MQ
02495      END-IF.                                                      EQW9Z0MQ
02496                                                                   EQW9Z0MQ
02497 *---CYLINDRE/PUISSANCE                                            EQW9Z0MQ
02498      IF ECR-VEHCYLNO = LOW-VALUE                                  EQW9Z0MQ
02499         MOVE SPACES TO ECR-VEHCYLNO                               EQW9Z0MQ
02500      END-IF.                                                      EQW9Z0MQ
02501      IF ECR-VEHCYLNO NOT = SPACES                                 EQW9Z0MQ
02502         MOVE ECR-VEHCYLNO TO C-XKMTENTREE                         EQW9Z0MQ
02503         MOVE 5 TO C-XKMTLONG                                      EQW9Z0MQ
02504         MOVE ZERO TO C-XKMTDECIMALE                               EQW9Z0MQ
02505         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW9Z0MQ
02506         IF C-XKMTRETCOD NOT = ZERO                                EQW9Z0MQ
02507            MOVE NOR-ALP TO ECR-VEHCYLNA                           EQW9Z0MQ
02508            IF KONTROL = 0                                         EQW9Z0MQ
02509               MOVE 'FB095' TO COM-GENE-MESANO                     EQW9Z0MQ
02510                               COM-CODERR                          EQW9Z0MQ
02511               MOVE CURSEUR TO ECR-VEHCYLNL                        EQW9Z0MQ
02512               MOVE 1       TO KONTROL                             EQW9Z0MQ
02513               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02514            END-IF                                                 EQW9Z0MQ
02515         ELSE                                                      EQW9Z0MQ
02516           IF C-XKMTNUM-0D = ZERO                                  EQW9Z0MQ
02517              MOVE NOR-ALP TO ECR-VEHCYLNA                         EQW9Z0MQ
02518              IF KONTROL = 0                                       EQW9Z0MQ
02519                 MOVE 'FB095' TO COM-GENE-MESANO                   EQW9Z0MQ
02520                                 COM-CODERR                        EQW9Z0MQ
02521                 MOVE CURSEUR TO ECR-VEHCYLNL                      EQW9Z0MQ
02522                 MOVE 1       TO KONTROL                           EQW9Z0MQ
02523                 GO           TO FIN-CONTROLE-SYNTAXE              EQW9Z0MQ
02524              END-IF                                               EQW9Z0MQ
02525           ELSE                                                    EQW9Z0MQ
02526              MOVE C-XKMTNUM-0D TO WSS-CYL                         EQW9Z0MQ
02527                                   WSS-CYLZ                        EQW9Z0MQ
02528              MOVE WSS-CYLZ     TO ECR-VEHCYLNO                    EQW9Z0MQ
02529           END-IF                                                  EQW9Z0MQ
02530         END-IF                                                    EQW9Z0MQ
02531      END-IF.                                                      EQW9Z0MQ
02532                                                                   EQW9Z0MQ
02533 *---USAGE DU VEHICULE                                             EQW9Z0MQ
02534      IF ECR-VEHUSACO = LOW-VALUE                                  EQW9Z0MQ
02535         MOVE SPACES TO ECR-VEHUSACO                               EQW9Z0MQ
02536      END-IF.                                                      EQW9Z0MQ
02537                                                                   EQW9Z0MQ
02538 *----------DETERMINATION DE L'APPEL A ECRAN D'AIDE DES CODES USAGEEQW9Z0MQ
02539      PERFORM DETER-AIDE-USAGE THRU FIN-DETER-AIDE-USAGE.          EQW9Z0MQ
02540                                                                   EQW9Z0MQ
02541 *----------USAGE DOIT ETRE NUMERIQUE                              EQW9Z0MQ
02542      IF ECR-VEHUSACO NOT = SPACES AND                             EQW9Z0MQ
02543         ECR-VEHUSACO NOT NUMERIC AND WSS-APPEL-AIDE-USAGE = 'N'   EQW9Z0MQ
02544         MOVE NOR-ALP TO ECR-VEHUSACA                              EQW9Z0MQ
02545         IF OK                                                     EQW9Z0MQ
02546             MOVE 'FB098' TO COM-GENE-MESANO                       EQW9Z0MQ
02547                             COM-CODERR                            EQW9Z0MQ
02548             MOVE CURSEUR TO ECR-VEHUSACL                          EQW9Z0MQ
02549             MOVE 1       TO KONTROL                               EQW9Z0MQ
02550             GO           TO FIN-CONTROLE-SYNTAXE                  EQW9Z0MQ
02551         END-IF                                                    EQW9Z0MQ
02552      END-IF.                                                      EQW9Z0MQ
02553                                                                   EQW9Z0MQ
02554 *----------LE CODE USAGE DOIT EXISTER DANS LA TABLE FBUSAP01      EQW9Z0MQ
02555      IF ECR-VEHUSACO NOT = SPACES AND WSS-APPEL-AIDE-USAGE = 'N'  EQW9Z0MQ
02556         MOVE  SPACES                    TO XSPIPARM               EQW9Z0MQ
02557         MOVE  'GP'                      TO FONCTION  OF XSPIPARM  EQW9Z0MQ
02558         MOVE  'FBUSAP'                  TO TABLE-PREF             EQW9Z0MQ
02559         MOVE  COM-GENE-CODCIE-PRINCIPAL TO TABLE-SUFF             EQW9Z0MQ
02560         MOVE  IDENT-TABLE               TO CODTAB OF XSPIPARM     EQW9Z0MQ
02561         MOVE  '= '                      TO OPERATEUR OF XSPIPARM  EQW9Z0MQ
02562 *---------------------------------------------------------------  EQW9Z0MQ
02563 *--- ACCES AVEC VALEUR '***' CAR LE CODE USAGE/PROFESSION NE      EQW9Z0MQ
02564 *---                         DEPEND PAS DU TYPE DE VEHICULE       EQW9Z0MQ
02565 *---------------------------------------------------------------  EQW9Z0MQ
39250 **MISE EN PLACE D'UN REJET POUR USAGE 14 EN 2 ROUES
39250 **SI PAS DE POSTE TROUVE 2E ACCES A FBUSAP AVEC VEHTYPC
39250 **SI 2 ROUES ET USAGE 14 MESSAGE DE REJET REMONTE DE SPI
39250         MOVE  ECR-VEHTYPCO              TO CLE-USATYPV            EQW9Z0MQ
02568         MOVE  ECR-VEHUSACO              TO CLE-USAGE              EQW9Z0MQ
02569         MOVE  WSS-CLE-FBUSAP            TO REF-POSTE OF XSPIPARM  EQW9Z0MQ
02570         PERFORM ACCES-SPI THRU FIN-ACCES-SPI                      EQW9Z0MQ
02571         IF RETCOD OF XSPIPARM  = ZERO                             EQW9Z0MQ
02572            MOVE IOAREA  OF XSPIPARM TO FBUSAP01                   EQW9Z0MQ
02573            IF USASTAC OF FBUSAP01 NOT = 'O'                       EQW9Z0MQ
02574               MOVE USAMSGC TO COM-GENE-MESANO                     EQW9Z0MQ
02575                               COM-CODERR                          EQW9Z0MQ
02576               MOVE CURSEUR TO ECR-VEHUSACL                        EQW9Z0MQ
02577               MOVE 1       TO KONTROL                             EQW9Z0MQ
02578               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02579            END-IF                                                 EQW9Z0MQ
02580         ELSE                                                      EQW9Z0MQ
02567            MOVE  '***'           TO CLE-USATYPV                   EQW9Z0MQ
39250            MOVE  WSS-CLE-FBUSAP  TO REF-POSTE OF XSPIPARM         EQW9Z0MQ
39250            PERFORM ACCES-SPI THRU FIN-ACCES-SPI                   EQW9Z0MQ
39250            IF RETCOD OF XSPIPARM  = ZERO                          EQW9Z0MQ
39250               MOVE IOAREA  OF XSPIPARM TO FBUSAP01                EQW9Z0MQ
39250               IF USASTAC OF FBUSAP01 NOT = 'O'                    EQW9Z0MQ
39250                  MOVE USAMSGC TO COM-GENE-MESANO                  EQW9Z0MQ
39250                                  COM-CODERR                       EQW9Z0MQ
39250                  MOVE CURSEUR TO ECR-VEHUSACL                     EQW9Z0MQ
39250                  MOVE 1       TO KONTROL                          EQW9Z0MQ
39250                  GO           TO FIN-CONTROLE-SYNTAXE             EQW9Z0MQ
39250               END-IF                                              EQW9Z0MQ
39250            ELSE
02581              MOVE NOR-ALP TO ECR-VEHUSACA                         EQW9Z0MQ
02582              IF OK                                                EQW9Z0MQ
02583                 MOVE 'FB098' TO COM-GENE-MESANO                   EQW9Z0MQ
02584                                 COM-CODERR                        EQW9Z0MQ
02585                 MOVE CURSEUR TO ECR-VEHUSACL                      EQW9Z0MQ
02586                 MOVE 1       TO KONTROL                           EQW9Z0MQ
02587                 GO           TO FIN-CONTROLE-SYNTAXE              EQW9Z0MQ
02588              END-IF                                               EQW9Z0MQ
02588            END-IF                                                 EQW9Z0MQ
02589         END-IF                                                    EQW9Z0MQ
02590      END-IF.                                                      EQW9Z0MQ
02591                                                                   EQW9Z0MQ
02592 *---IMMATRICULATION                                               EQW9Z0MQ
02593      IF ECR-VEHIMMXO = LOW-VALUE                                  EQW9Z0MQ
02594         MOVE SPACES TO ECR-VEHIMMXO                               EQW9Z0MQ
02595      END-IF.                                                      EQW9Z0MQ
02596                                                                   EQW9Z0MQ
02597 *---DATE DE PREMIERE MISE EN CIRCULATION                          EQW9Z0MQ
02598      IF ECR-VEHCIRDO = LOW-VALUE                                  EQW9Z0MQ
02599         MOVE SPACES TO ECR-VEHCIRDO                               EQW9Z0MQ
02600      END-IF.                                                      EQW9Z0MQ
02601      IF ECR-VEHCIRDO NOT = SPACES                                 EQW9Z0MQ
02602         MOVE ECR-VEHCIRDO TO C-XKMTENTREE                         EQW9Z0MQ
02603         MOVE 6 TO C-XKMTLONG                                      EQW9Z0MQ
02604         MOVE ZERO TO C-XKMTDECIMALE                               EQW9Z0MQ
02605         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW9Z0MQ
02606         IF C-XKMTRETCOD NOT = ZERO                                EQW9Z0MQ
02607            MOVE NOR-ALP TO ECR-VEHCIRDA                           EQW9Z0MQ
02608            IF KONTROL = 0                                         EQW9Z0MQ
02609               MOVE 'FB097' TO COM-GENE-MESANO                     EQW9Z0MQ
02610                               COM-CODERR                          EQW9Z0MQ
02611               MOVE CURSEUR TO ECR-VEHCIRDL                        EQW9Z0MQ
02612               MOVE 1       TO KONTROL                             EQW9Z0MQ
02613               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02614            END-IF                                                 EQW9Z0MQ
02615         ELSE                                                      EQW9Z0MQ
02616            MOVE C-XKMTSORTIE  TO   WSS-DATE-TRAV                  EQW9Z0MQ
02618            PERFORM VERIF-DATE THRU FIN-VERIF-DATE                 EQW9Z0MQ
02619            IF WSS-DATE-OK = 'N'                                   EQW9Z0MQ
02620               MOVE NOR-ALP TO ECR-VEHCIRDA                        EQW9Z0MQ
02621               IF KONTROL = 0                                      EQW9Z0MQ
02622                  MOVE 'FB097' TO COM-GENE-MESANO                  EQW9Z0MQ
02623                                  COM-CODERR                       EQW9Z0MQ
02624                  MOVE CURSEUR TO ECR-VEHCIRDL                     EQW9Z0MQ
02625                  MOVE 1       TO KONTROL                          EQW9Z0MQ
02626                  GO           TO FIN-CONTROLE-SYNTAXE             EQW9Z0MQ
02627               END-IF                                              EQW9Z0MQ
02628            ELSE                                                   EQW9Z0MQ
F3576               MOVE ECR-VEHCIRDO     TO   WSS-DATE-TEST
02629               PERFORM VERIF-DATE-OK THRU FIN-VERIF-DATE-OK        EQW9Z0MQ
02630               IF WSS-DATE-OK = 'N'                                EQW9Z0MQ
02631                  MOVE NOR-ALP TO ECR-VEHCIRDA                     EQW9Z0MQ
02632                  IF KONTROL = 0                                   EQW9Z0MQ
02633                     MOVE 'FB428' TO COM-GENE-MESANO               EQW9Z0MQ
02634                                     COM-CODERR                    EQW9Z0MQ
02635                     MOVE CURSEUR TO ECR-VEHCIRDL                  EQW9Z0MQ
02636                     MOVE 1       TO KONTROL                       EQW9Z0MQ
02637                     GO           TO FIN-CONTROLE-SYNTAXE          EQW9Z0MQ
02638                  END-IF                                           EQW9Z0MQ
02639               END-IF                                              EQW9Z0MQ
02640            END-IF                                                 EQW9Z0MQ
02641         END-IF                                                    EQW9Z0MQ
02642      END-IF.                                                      EQW9Z0MQ
02643                                                                   EQW9Z0MQ
F3576 *---DATE D'ACQUISITION DU VEHICULE                                EQW9Z0MQ
F3576      IF ECR-VEHACQDO = LOW-VALUE                                  EQW9Z0MQ
F3576         MOVE SPACES TO ECR-VEHACQDO                               EQW9Z0MQ
F3576      END-IF.                                                      EQW9Z0MQ
F3576      IF ECR-VEHACQDO NOT = SPACES                                 EQW9Z0MQ
F3576         MOVE ECR-VEHACQDO TO C-XKMTENTREE                         EQW9Z0MQ
F3576         MOVE 6 TO C-XKMTLONG                                      EQW9Z0MQ
F3576         MOVE ZERO TO C-XKMTDECIMALE                               EQW9Z0MQ
F3576         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW9Z0MQ
F3576         IF C-XKMTRETCOD NOT = ZERO                                EQW9Z0MQ
F3576            MOVE NOR-ALP TO ECR-VEHACQDA                           EQW9Z0MQ
F3576            IF KONTROL = 0                                         EQW9Z0MQ
F3576               MOVE 'FB469' TO COM-GENE-MESANO                     EQW9Z0MQ
F3576                               COM-CODERR                          EQW9Z0MQ
F3576               MOVE CURSEUR TO ECR-VEHACQDL                        EQW9Z0MQ
F3576               MOVE 1       TO KONTROL                             EQW9Z0MQ
F3576               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
F3576            END-IF                                                 EQW9Z0MQ
F3576         ELSE                                                      EQW9Z0MQ
F3576            MOVE C-XKMTSORTIE  TO   WSS-DATE-TRAV                  EQW9Z0MQ
F3576            PERFORM VERIF-DATE THRU FIN-VERIF-DATE                 EQW9Z0MQ
F3576            IF WSS-DATE-OK = 'N'                                   EQW9Z0MQ
F3576               MOVE NOR-ALP TO ECR-VEHACQDA                        EQW9Z0MQ
F3576               IF KONTROL = 0                                      EQW9Z0MQ
F3576                  MOVE 'FB469' TO COM-GENE-MESANO                  EQW9Z0MQ
F3576                                  COM-CODERR                       EQW9Z0MQ
F3576                  MOVE CURSEUR TO ECR-VEHACQDL                     EQW9Z0MQ
F3576                  MOVE 1       TO KONTROL                          EQW9Z0MQ
F3576                  GO           TO FIN-CONTROLE-SYNTAXE             EQW9Z0MQ
F3576               END-IF                                              EQW9Z0MQ
F3576            ELSE                                                   EQW9Z0MQ
F3576               MOVE ECR-VEHACQDO     TO   WSS-DATE-TEST
F3576               PERFORM VERIF-DATE-OK THRU FIN-VERIF-DATE-OK        EQW9Z0MQ
F3576               IF WSS-DATE-OK = 'N'                                EQW9Z0MQ
F3576                  MOVE NOR-ALP TO ECR-VEHACQDA                     EQW9Z0MQ
F3576                  IF KONTROL = 0                                   EQW9Z0MQ
F3576                     MOVE 'FB468' TO COM-GENE-MESANO               EQW9Z0MQ
F3576                                     COM-CODERR                    EQW9Z0MQ
F3576                     MOVE CURSEUR TO ECR-VEHACQDL                  EQW9Z0MQ
F3576                     MOVE 1       TO KONTROL                       EQW9Z0MQ
F3576                     GO           TO FIN-CONTROLE-SYNTAXE          EQW9Z0MQ
F3576                  END-IF                                           EQW9Z0MQ
F3576               END-IF                                              EQW9Z0MQ
F3576            END-IF                                                 EQW9Z0MQ
F3576         END-IF                                                    EQW9Z0MQ
F3576      END-IF.                                                      EQW9Z0MQ
F3576                                                                   EQW9Z0MQ
02644 *---VALEUR ‡ NEUF                                                 EQW9Z0MQ
02645      IF ECR-VEHVALMO = LOW-VALUE                                  EQW9Z0MQ
02646         MOVE SPACES TO ECR-VEHVALMO                               EQW9Z0MQ
02647      END-IF.                                                      EQW9Z0MQ
02648                                                                   EQW9Z0MQ
02649      IF ECR-VEHVALMO NOT = SPACES                                 EQW9Z0MQ
02650         MOVE ECR-VEHVALMO TO C-XKMTENTREE                         EQW9Z0MQ
02651         MOVE 7 TO C-XKMTLONG                                      EQW9Z0MQ
02652         MOVE ZERO TO C-XKMTDECIMALE                               EQW9Z0MQ
02653         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW9Z0MQ
02654         IF C-XKMTRETCOD NOT = ZERO                                EQW9Z0MQ
02655            MOVE NOR-ALP TO ECR-VEHVALMA                           EQW9Z0MQ
02656            IF KONTROL = 0                                         EQW9Z0MQ
02657               MOVE 'FB099' TO COM-GENE-MESANO                     EQW9Z0MQ
02658                               COM-CODERR                          EQW9Z0MQ
02659               MOVE CURSEUR TO ECR-VEHVALML                        EQW9Z0MQ
02660               MOVE 1       TO KONTROL                             EQW9Z0MQ
02661               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02662            END-IF                                                 EQW9Z0MQ
02663         ELSE                                                      EQW9Z0MQ
02664            MOVE C-XKMTNUM-0D TO WSS-VALNEUF                       EQW9Z0MQ
02665                                 WSS-VALNEUFZ                      EQW9Z0MQ
02666            MOVE WSS-VALNEUFZ TO ECR-VEHVALMO                      EQW9Z0MQ
02667         END-IF                                                    EQW9Z0MQ
02668      ELSE                                                         EQW9Z0MQ
02669         MOVE ZERO            TO WSS-VALNEUF                       EQW9Z0MQ
02670      END-IF.                                                      EQW9Z0MQ
02671                                                                   EQW9Z0MQ
02672 *--- EN FLOTTE LA VALEUR A NEUF NE PEUT EXCEDER 50310 EUR CAR     EQW9Z0MQ
02673 *--- HAUT DE GAMME INTERDIT                                       EQW9Z0MQ
02674      IF ECR-VEHVALMO NOT = SPACES                                 EQW9Z0MQ
02675         IF WSS-VALNEUF > 50310                                    EQW9Z0MQ
02676            MOVE NOR-ALP TO ECR-VEHVALMA                           EQW9Z0MQ
02677            IF KONTROL = 0                                         EQW9Z0MQ
02678               MOVE 'FB434' TO COM-GENE-MESANO                     EQW9Z0MQ
02679                               COM-CODERR                          EQW9Z0MQ
02680               MOVE CURSEUR TO ECR-VEHVALML                        EQW9Z0MQ
02681               MOVE 1       TO KONTROL                             EQW9Z0MQ
02682               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02683         END-IF                                                    EQW9Z0MQ
02684      END-IF.                                                      EQW9Z0MQ
02685 *---TYPE DE PROTECTION VOL                                        EQW9Z0MQ
02686      IF ECR-VEHPRTCO = LOW-VALUE                                  EQW9Z0MQ
02687         MOVE SPACES TO ECR-VEHPRTCO                               EQW9Z0MQ
02688      END-IF.                                                      EQW9Z0MQ
02689                                                                   EQW9Z0MQ
02690 *----------DETERMINATION DE L'APPEL A ECRAN D'AIDE DES PROTECTIONSEQW9Z0MQ
02691      PERFORM DETER-AIDE-PROTECTION THRU FIN-DETER-AIDE-PROTECTION.EQW9Z0MQ
02692                                                                   EQW9Z0MQ
02693 *----------LE CODE PROTECTION DOIT EXISTER DANS LA TABLE FBPROT01 EQW9Z0MQ
02694      IF ECR-VEHPRTCO NOT = SPACES AND WSS-APPEL-AIDE-PROT = 'N'   EQW9Z0MQ
02695         MOVE  SPACES                    TO XSPIPARM               EQW9Z0MQ
02696         MOVE  'GP'                      TO FONCTION  OF XSPIPARM  EQW9Z0MQ
02697         MOVE  'FBPROT'                  TO TABLE-PREF             EQW9Z0MQ
02698         MOVE  COM-GENE-CODCIE-PRINCIPAL TO TABLE-SUFF             EQW9Z0MQ
02699         MOVE  IDENT-TABLE               TO CODTAB OF XSPIPARM     EQW9Z0MQ
02700         MOVE  '= '                      TO OPERATEUR OF XSPIPARM  EQW9Z0MQ
02701         MOVE  ECR-VEHTYPCO              TO CLE-PRTTYPV            EQW9Z0MQ
02702         MOVE  ECR-VEHPRTCO              TO CLE-NIVEAU             EQW9Z0MQ
02703         MOVE  WSS-CLE-FBPROT            TO REF-POSTE OF XSPIPARM  EQW9Z0MQ
02704         PERFORM ACCES-SPI THRU FIN-ACCES-SPI                      EQW9Z0MQ
02705         IF RETCOD OF XSPIPARM  = ZERO                             EQW9Z0MQ
02706            MOVE IOAREA  OF XSPIPARM TO FBPROT01                   EQW9Z0MQ
02707            IF PRTSTAC OF FBPROT01 = 'I'                           EQW9Z0MQ
02708               MOVE 'FB101' TO COM-GENE-MESANO                     EQW9Z0MQ
02709                               COM-CODERR                          EQW9Z0MQ
02710               MOVE CURSEUR TO ECR-VEHPRTCL                        EQW9Z0MQ
02711               MOVE 1       TO KONTROL                             EQW9Z0MQ
02712               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02713            END-IF                                                 EQW9Z0MQ
02714         ELSE                                                      EQW9Z0MQ
02715            MOVE NOR-ALP TO ECR-VEHPRTCA                           EQW9Z0MQ
02716            IF OK                                                  EQW9Z0MQ
02717               MOVE 'FB101' TO COM-GENE-MESANO                     EQW9Z0MQ
02718                               COM-CODERR                          EQW9Z0MQ
02719               MOVE CURSEUR TO ECR-VEHPRTCL                        EQW9Z0MQ
02720               MOVE 1       TO KONTROL                             EQW9Z0MQ
02721               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02722            END-IF                                                 EQW9Z0MQ
02723         END-IF                                                    EQW9Z0MQ
02724      END-IF.                                                      EQW9Z0MQ
02725                                                                   EQW9Z0MQ
02726 *---TYPE DE GARAGE                                                EQW9Z0MQ
02727      IF ECR-GARCODCO = LOW-VALUE                                  EQW9Z0MQ
02728         MOVE SPACES TO ECR-GARCODCO                               EQW9Z0MQ
02729      END-IF.                                                      EQW9Z0MQ
02730                                                                   EQW9Z0MQ
02731      IF ECR-GARCODCO NOT = SPACES                                 EQW9Z0MQ
02732         IF ECR-GARCODCO NOT = 'O' AND 'N'                         EQW9Z0MQ
02733            MOVE NOR-ALP TO ECR-GARCODCA                           EQW9Z0MQ
02734            IF KONTROL = 0                                         EQW9Z0MQ
02735               MOVE 'FB102' TO COM-GENE-MESANO                     EQW9Z0MQ
02736                               COM-CODERR                          EQW9Z0MQ
02737               MOVE CURSEUR TO ECR-GARCODCL                        EQW9Z0MQ
02738               MOVE 1       TO KONTROL                             EQW9Z0MQ
02739               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02740            END-IF                                                 EQW9Z0MQ
02741         END-IF                                                    EQW9Z0MQ
02742      END-IF.                                                      EQW9Z0MQ
02743                                                                   EQW9Z0MQ
02744 *---MODE D'ACQUISITION                                            EQW9Z0MQ
02745      IF ECR-VEHPOSCO = LOW-VALUE                                  EQW9Z0MQ
02746         MOVE SPACES TO ECR-VEHPOSCO                               EQW9Z0MQ
02747      END-IF.                                                      EQW9Z0MQ
02748                                                                   EQW9Z0MQ
02749      IF ECR-VEHPOSCO NOT = SPACES                                 EQW9Z0MQ
02750         IF ECR-VEHPOSCO NOT = 'O' AND 'N'                         EQW9Z0MQ
02751            MOVE NOR-ALP TO ECR-VEHPOSCA                           EQW9Z0MQ
02752            IF KONTROL = 0                                         EQW9Z0MQ
02753               MOVE 'FB100' TO COM-GENE-MESANO                     EQW9Z0MQ
02754                               COM-CODERR                          EQW9Z0MQ
02755               MOVE CURSEUR TO ECR-VEHPOSCL                        EQW9Z0MQ
02756               MOVE 1       TO KONTROL                             EQW9Z0MQ
02757               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02758            END-IF                                                 EQW9Z0MQ
02759         END-IF                                                    EQW9Z0MQ
02760      END-IF.                                                      EQW9Z0MQ
02761                                                                   EQW9Z0MQ
02762 *---INDICATEUR GARANTIE PERTE FINANCIERES                         EQW9Z0MQ
02763      IF ECR-VEHPEFCO = LOW-VALUE                                  EQW9Z0MQ
02764         MOVE SPACES TO ECR-VEHPEFCO                               EQW9Z0MQ
02765      END-IF.                                                      EQW9Z0MQ
02766                                                                   EQW9Z0MQ
02767      IF ECR-VEHPEFCO NOT = SPACES                                 EQW9Z0MQ
02768         IF ECR-VEHPEFCO NOT = 'O' AND 'N'                         EQW9Z0MQ
02769            MOVE NOR-ALP TO ECR-VEHPEFCO                           EQW9Z0MQ
02770            IF KONTROL = 0                                         EQW9Z0MQ
02771               MOVE 'FB147' TO COM-GENE-MESANO                     EQW9Z0MQ
02772                               COM-CODERR                          EQW9Z0MQ
02773               MOVE CURSEUR TO ECR-VEHPEFCL                        EQW9Z0MQ
02774               MOVE 1       TO KONTROL                             EQW9Z0MQ
02775               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02776            END-IF                                                 EQW9Z0MQ
02777         END-IF                                                    EQW9Z0MQ
02778      END-IF.                                                      EQW9Z0MQ
02779                                                                   EQW9Z0MQ
02780 *---CODE POSTAL                                                   EQW9Z0MQ
02781      IF ECR-GARCOPCO = LOW-VALUE                                  EQW9Z0MQ
02782         MOVE SPACES TO ECR-GARCOPCO                               EQW9Z0MQ
02783      END-IF.                                                      EQW9Z0MQ
02784                                                                   EQW9Z0MQ
02785 *----------DETERMINATION DE L'APPEL A ECRAN D'AIDE CODES POSTAUX  EQW9Z0MQ
02786      PERFORM DETER-AIDE-CP THRU FIN-DETER-AIDE-CP.                EQW9Z0MQ
02787                                                                   EQW9Z0MQ
02788      IF ECR-GARCOPCO NOT = SPACES AND WSS-APPEL-AIDE-CP = 'N'     EQW9Z0MQ
02789         MOVE ECR-GARCOPCO TO C-XKMTENTREE                         EQW9Z0MQ
02790         MOVE 5 TO C-XKMTLONG                                      EQW9Z0MQ
02791         MOVE ZERO TO C-XKMTDECIMALE                               EQW9Z0MQ
02792         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW9Z0MQ
02793         IF C-XKMTRETCOD NOT = ZERO                                EQW9Z0MQ
02794            MOVE NOR-ALP TO ECR-GARCOPCA                           EQW9Z0MQ
02795            IF KONTROL = 0                                         EQW9Z0MQ
02796               MOVE 'FB103' TO COM-GENE-MESANO                     EQW9Z0MQ
02797                               COM-CODERR                          EQW9Z0MQ
02798               MOVE CURSEUR TO ECR-GARCOPCL                        EQW9Z0MQ
02799               MOVE 1       TO KONTROL                             EQW9Z0MQ
02800               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02801            END-IF                                                 EQW9Z0MQ
02802         END-IF                                                    EQW9Z0MQ
02803      END-IF.                                                      EQW9Z0MQ
02804                                                                   EQW9Z0MQ
02805      IF WSS-APPEL-AIDE-CP = 'O'                                   EQW9Z0MQ
02806         IF WSS-CODE-POSTAL-SAISI NOT = SPACES                     EQW9Z0MQ
02807            MOVE WSS-CODE-POSTAL-SAISI  TO C-XKMTENTREE            EQW9Z0MQ
02808            MOVE 5 TO C-XKMTLONG                                   EQW9Z0MQ
02809            MOVE ZERO TO C-XKMTDECIMALE                            EQW9Z0MQ
02810            PERFORM CADRAGE THRU FIN-CADRAGE                       EQW9Z0MQ
02811            IF C-XKMTRETCOD NOT = ZERO                             EQW9Z0MQ
02812               MOVE NOR-ALP TO ECR-GARCOPCA                        EQW9Z0MQ
02813               IF KONTROL = 0                                      EQW9Z0MQ
02814                  MOVE 'FB103' TO COM-GENE-MESANO                  EQW9Z0MQ
02815                                  COM-CODERR                       EQW9Z0MQ
02816                  MOVE CURSEUR TO ECR-GARCOPCL                     EQW9Z0MQ
02817                  MOVE 1       TO KONTROL                          EQW9Z0MQ
02818                  GO           TO FIN-CONTROLE-SYNTAXE             EQW9Z0MQ
02819               END-IF                                              EQW9Z0MQ
02820            END-IF                                                 EQW9Z0MQ
02821         END-IF                                                    EQW9Z0MQ
02822      END-IF.                                                      EQW9Z0MQ
02823                                                                   EQW9Z0MQ
02824 *---COMMUNE DU LIEU DE GARAGE                                     EQW9Z0MQ
02825      IF ECR-GARVILLO = LOW-VALUE                                  EQW9Z0MQ
02826         MOVE SPACES TO ECR-GARVILLO                               EQW9Z0MQ
02827      END-IF.                                                      EQW9Z0MQ
02828                                                                   EQW9Z0MQ
02829 *----------DETERMINATION DE L'APPEL A ECRAN D'AIDE COMMUNES       EQW9Z0MQ
02830      PERFORM DETER-AIDE-COM THRU FIN-DETER-AIDE-COM.              EQW9Z0MQ
02831                                                                   EQW9Z0MQ
02832 *---INDICATEUR VEHICULE SUPPLEMENTAIRE AU FOYER                   EQW9Z0MQ
02833      IF ECR-ANVREPCO = LOW-VALUE                                  EQW9Z0MQ
02834         MOVE SPACES TO ECR-ANVREPCO                               EQW9Z0MQ
02835      END-IF.                                                      EQW9Z0MQ
02836                                                                   EQW9Z0MQ
02837      IF ECR-ANVREPCO NOT = SPACES                                 EQW9Z0MQ
02838         IF ECR-ANVREPCO NOT = 'O' AND 'N'                         EQW9Z0MQ
02839            MOVE NOR-ALP TO ECR-ANVREPCO                           EQW9Z0MQ
02840            IF KONTROL = 0                                         EQW9Z0MQ
02841               MOVE 'FB104' TO COM-GENE-MESANO                     EQW9Z0MQ
02842                               COM-CODERR                          EQW9Z0MQ
02843               MOVE CURSEUR TO ECR-ANVREPCL                        EQW9Z0MQ
02844               MOVE 1       TO KONTROL                             EQW9Z0MQ
02845               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02846            END-IF                                                 EQW9Z0MQ
02847         END-IF                                                    EQW9Z0MQ
02848      END-IF.                                                      EQW9Z0MQ
02849                                                                   EQW9Z0MQ
02850 *---FORMULE                                                       EQW9Z0MQ
02851      IF ECR-VEHFORCO = LOW-VALUE                                  EQW9Z0MQ
02852         MOVE SPACES TO ECR-VEHFORCO                               EQW9Z0MQ
02853      END-IF.                                                      EQW9Z0MQ
02854                                                                   EQW9Z0MQ
02855 *----------DETERMINATION DE L'APPEL A ECRAN D'AIDE DES FORMULES   EQW9Z0MQ
02856      PERFORM DETER-AIDE-FORMULE THRU FIN-DETER-AIDE-FORMULE.      EQW9Z0MQ
02857                                                                   EQW9Z0MQ
02858 *----------LE CODE FORMULE DOIT EXISTER DANS LA TABLE FBFORM01    EQW9Z0MQ
02859      IF (WSS-APPEL-AIDE-FORM = 'N')                               EQW9Z0MQ
02860        AND (ECR-VEHFORCO NOT = SPACES AND 'V ' AND 'M ')          EQW9Z0MQ
02861         MOVE  SPACES                    TO XSPIPARM               EQW9Z0MQ
02862         MOVE  'GP'                      TO FONCTION  OF XSPIPARM  EQW9Z0MQ
02863         MOVE  'FBFORM'                  TO TABLE-PREF             EQW9Z0MQ
02864         MOVE  COM-GENE-CODCIE-PRINCIPAL TO TABLE-SUFF             EQW9Z0MQ
02865         MOVE  IDENT-TABLE               TO CODTAB OF XSPIPARM     EQW9Z0MQ
02866         MOVE  '= '                      TO OPERATEUR OF XSPIPARM  EQW9Z0MQ
02867         MOVE  ECR-VEHTYPCO              TO CLE-FORTYPV            EQW9Z0MQ
02868         MOVE  ECR-VEHFORCO              TO CLE-FORMULE            EQW9Z0MQ
02869         MOVE  WSS-CLE-FBFORM            TO REF-POSTE OF XSPIPARM  EQW9Z0MQ
02870         PERFORM ACCES-SPI THRU FIN-ACCES-SPI                      EQW9Z0MQ
02871         IF RETCOD OF XSPIPARM  = ZERO                             EQW9Z0MQ
02872            MOVE IOAREA  OF XSPIPARM TO FBFORM01                   EQW9Z0MQ
02873         ELSE                                                      EQW9Z0MQ
02874            MOVE NOR-ALP TO ECR-VEHFORCA                           EQW9Z0MQ
02875            IF OK                                                  EQW9Z0MQ
02876               MOVE 'FB105' TO COM-GENE-MESANO                     EQW9Z0MQ
02877                               COM-CODERR                          EQW9Z0MQ
02878               MOVE CURSEUR TO ECR-VEHFORCL                        EQW9Z0MQ
02879               MOVE 1       TO KONTROL                             EQW9Z0MQ
02880               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02881            END-IF                                                 EQW9Z0MQ
02882         END-IF                                                    EQW9Z0MQ
02883      END-IF.                                                      EQW9Z0MQ
02884                                                                   EQW9Z0MQ
02885 *---NUMERO DE CONTRAT PRECEDENT                                   EQW9Z0MQ
02886      IF ECR-ANVNUMXO = LOW-VALUE                                  EQW9Z0MQ
02887         MOVE SPACES TO ECR-ANVNUMXO                               EQW9Z0MQ
02888      END-IF.                                                      EQW9Z0MQ
02889                                                                   EQW9Z0MQ
02885 *---ANCIEN SOUSCRIPTEUR DU CONTRAT PRECEDENT                      EQW9Z0MQ
02886      IF ECR-ANVSOUSO = LOW-VALUE                                  EQW9Z0MQ
02887         MOVE SPACES TO ECR-ANVSOUSO                               EQW9Z0MQ
02888      END-IF.                                                      EQW9Z0MQ
02894                                                                   EQW9Z0MQ
02885 *---NUMERO D'ORDRE ANCIEN SOUSCRIPTEUR DU CONTRAT PRECEDENT       EQW9Z0MQ
02886      IF ECR-ANVSOUNO = LOW-VALUE                                  EQW9Z0MQ
02887         MOVE SPACES TO ECR-ANVSOUNO                               EQW9Z0MQ
02888      END-IF.                                                      EQW9Z0MQ
02894                                                                   EQW9Z0MQ
02895 *----------DETERMINATION DE L'APPEL A ECRAN D'AIDE DU CHOIX DE    EQW9Z0MQ
02895 *----------L'ENFANT                                               EQW9Z0MQ
02896      PERFORM DETER-AIDE-CHOIX-ENFANT THRU                         EQW9Z0MQ
02896              FIN-DETER-AIDE-CHOIX-ENFANT.                         EQW9Z0MQ
02889                                                                   EQW9Z0MQ
02890 *---PRECEDENT CONTRAT - CODE CIE                                  EQW9Z0MQ
02891      IF ECR-ANVCIEXO = LOW-VALUE                                  EQW9Z0MQ
02892         MOVE SPACES TO ECR-ANVCIEXO                               EQW9Z0MQ
02893      END-IF.                                                      EQW9Z0MQ
02894                                                                   EQW9Z0MQ
02895 *----------DETERMINATION DE L'APPEL A ECRAN D'AIDE CODE COMPAGNIESEQW9Z0MQ
02896      PERFORM DETER-AIDE-CIE THRU FIN-DETER-AIDE-CIE.              EQW9Z0MQ
02897                                                                   EQW9Z0MQ
02898                                                                   EQW9Z0MQ
02899 *---LIBELLE COMPAGNIE PRECEDENTE                                  EQW9Z0MQ
02900      IF ECR-ANVCIELO = LOW-VALUE                                  EQW9Z0MQ
02901         MOVE SPACES TO ECR-ANVCIELO                               EQW9Z0MQ
02902      END-IF.                                                      EQW9Z0MQ
02903                                                                   EQW9Z0MQ
02904 *---NOMBRE DE MOIS D'ASSURANCE DU CONTRAT PRECEDENT               EQW9Z0MQ
02905      IF ECR-ANVANCNO = LOW-VALUE                                  EQW9Z0MQ
02906         MOVE SPACES TO ECR-ANVANCNO                               EQW9Z0MQ
02907      END-IF.                                                      EQW9Z0MQ
02908                                                                   EQW9Z0MQ
02909      IF ECR-ANVANCNO NOT = SPACES                                 EQW9Z0MQ
02910         MOVE ECR-ANVANCNO TO C-XKMTENTREE                         EQW9Z0MQ
02911         MOVE 2 TO C-XKMTLONG                                      EQW9Z0MQ
02912         MOVE 0 TO C-XKMTDECIMALE                                  EQW9Z0MQ
02913         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW9Z0MQ
02914         IF C-XKMTRETCOD NOT = ZERO                                EQW9Z0MQ
02915            MOVE NOR-ALP TO ECR-ANVANCNA                           EQW9Z0MQ
02916            IF KONTROL = 0                                         EQW9Z0MQ
02917               MOVE 'FB169' TO COM-GENE-MESANO                     EQW9Z0MQ
02918                               COM-CODERR                          EQW9Z0MQ
02919               MOVE CURSEUR TO ECR-ANVANCNL                        EQW9Z0MQ
02920               MOVE 1       TO KONTROL                             EQW9Z0MQ
02921               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02922            END-IF                                                 EQW9Z0MQ
02923         ELSE                                                      EQW9Z0MQ
02924            MOVE C-XKMTNUM-0D TO WSS-NBR                           EQW9Z0MQ
02925            MOVE C-XKMTSORTIE TO ECR-ANVANCNO                      EQW9Z0MQ
02926            IF C-XKMTNUM-0D > 36                                   EQW9Z0MQ
02927               MOVE NOR-ALP TO ECR-ANVANCNA                        EQW9Z0MQ
02928               IF KONTROL = 0                                      EQW9Z0MQ
02929                  MOVE 'FB338' TO COM-GENE-MESANO                  EQW9Z0MQ
02930                                  COM-CODERR                       EQW9Z0MQ
02931                  MOVE CURSEUR TO ECR-ANVANCNL                     EQW9Z0MQ
02932                  MOVE 1       TO KONTROL                          EQW9Z0MQ
02933                  GO           TO FIN-CONTROLE-SYNTAXE             EQW9Z0MQ
02934               END-IF                                              EQW9Z0MQ
02935            END-IF                                                 EQW9Z0MQ
02936         END-IF                                                    EQW9Z0MQ
02937      END-IF.                                                      EQW9Z0MQ
02938                                                                   EQW9Z0MQ
02939 *---NOMBRE DE MOIS D'INTERRUPTION                                 EQW9Z0MQ
02940      IF ECR-ANVINTNO = LOW-VALUE                                  EQW9Z0MQ
02941         MOVE SPACES TO ECR-ANVINTNO                               EQW9Z0MQ
02942      END-IF.                                                      EQW9Z0MQ
02943                                                                   EQW9Z0MQ
02944      IF ECR-ANVINTNO NOT = SPACES                                 EQW9Z0MQ
02945         MOVE ECR-ANVINTNO  TO C-XKMTENTREE                        EQW9Z0MQ
02946         MOVE 2             TO C-XKMTLONG                          EQW9Z0MQ
02947         MOVE 0             TO C-XKMTDECIMALE                      EQW9Z0MQ
02948         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW9Z0MQ
02949         IF C-XKMTRETCOD NOT = 0                                   EQW9Z0MQ
02950            MOVE NOR-ALP TO ECR-ANVINTNA                           EQW9Z0MQ
02951            IF KONTROL = 0                                         EQW9Z0MQ
02952               MOVE 'FB249' TO COM-GENE-MESANO                     EQW9Z0MQ
02953                               COM-CODERR                          EQW9Z0MQ
02954               MOVE CURSEUR TO ECR-ANVINTNL                        EQW9Z0MQ
02955               MOVE 1       TO KONTROL                             EQW9Z0MQ
02956               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02957            END-IF                                                 EQW9Z0MQ
02958         ELSE                                                      EQW9Z0MQ
02959            IF C-XKMTNUM-0D > 12                                   EQW9Z0MQ
02960               MOVE NOR-ALP TO ECR-ANVINTNA                        EQW9Z0MQ
02961               IF KONTROL = 0                                      EQW9Z0MQ
02962                  MOVE 'FB249' TO COM-GENE-MESANO                  EQW9Z0MQ
02963                                  COM-CODERR                       EQW9Z0MQ
02964                  MOVE CURSEUR TO ECR-ANVINTNL                     EQW9Z0MQ
02965                  MOVE 1       TO KONTROL                          EQW9Z0MQ
02966                  GO           TO FIN-CONTROLE-SYNTAXE             EQW9Z0MQ
02967               END-IF                                              EQW9Z0MQ
02968            ELSE                                                   EQW9Z0MQ
02969               MOVE C-XKMTNUM-0D TO WSS-NBMOIS-INTER               EQW9Z0MQ
02970               MOVE C-XKMTSORTIE TO ECR-ANVINTNO                   EQW9Z0MQ
02971            END-IF                                                 EQW9Z0MQ
02972         END-IF                                                    EQW9Z0MQ
02973      END-IF.                                                      EQW9Z0MQ
02974                                                                   EQW9Z0MQ
02975 *---DATE DE RESIL DU CONTRAT PRECEDENT                            EQW9Z0MQ
02976      IF ECR-ANVRESDO = LOW-VALUE                                  EQW9Z0MQ
02977         MOVE SPACES TO ECR-ANVRESDO                               EQW9Z0MQ
02978      END-IF.                                                      EQW9Z0MQ
02979                                                                   EQW9Z0MQ
02980      IF ECR-ANVRESDO NOT = SPACES                                 EQW9Z0MQ
02981         MOVE ECR-ANVRESDO TO C-XKMTENTREE                         EQW9Z0MQ
02982         MOVE 6 TO C-XKMTLONG                                      EQW9Z0MQ
02983         MOVE ZERO TO C-XKMTDECIMALE                               EQW9Z0MQ
02984         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW9Z0MQ
02985         IF C-XKMTRETCOD NOT = ZERO                                EQW9Z0MQ
02986            MOVE NOR-ALP TO ECR-ANVRESDA                           EQW9Z0MQ
02987            IF KONTROL = 0                                         EQW9Z0MQ
02988               MOVE 'FB151' TO COM-GENE-MESANO                     EQW9Z0MQ
02989                               COM-CODERR                          EQW9Z0MQ
02990               MOVE CURSEUR TO ECR-ANVRESDL                        EQW9Z0MQ
02991               MOVE 1       TO KONTROL                             EQW9Z0MQ
02992               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
02993            END-IF                                                 EQW9Z0MQ
02994         ELSE                                                      EQW9Z0MQ
02995            MOVE C-XKMTSORTIE  TO   WSS-DATE-TRAV                  EQW9Z0MQ
02996                                    WSS-DATE-RESIL                 EQW9Z0MQ
02997            PERFORM VERIF-DATE THRU FIN-VERIF-DATE                 EQW9Z0MQ
02998            IF WSS-DATE-OK = 'N'                                   EQW9Z0MQ
02999               MOVE NOR-ALP TO ECR-ANVRESDA                        EQW9Z0MQ
03000               IF KONTROL = 0                                      EQW9Z0MQ
03001                  MOVE 'FB151' TO COM-GENE-MESANO                  EQW9Z0MQ
03002                                  COM-CODERR                       EQW9Z0MQ
03003                  MOVE CURSEUR TO ECR-ANVRESDL                     EQW9Z0MQ
03004                  MOVE 1       TO KONTROL                          EQW9Z0MQ
03005                  GO           TO FIN-CONTROLE-SYNTAXE             EQW9Z0MQ
03006               END-IF                                              EQW9Z0MQ
03007            ELSE                                                   EQW9Z0MQ
03008               MOVE WSS-DATE-RESIL TO ECR-ANVRESDO                 EQW9Z0MQ
03009            END-IF                                                 EQW9Z0MQ
03010         END-IF                                                    EQW9Z0MQ
03011      END-IF.                                                      EQW9Z0MQ
03012                                                                   EQW9Z0MQ
03013 *---INDICATEUR DE PRESENCE DE SINISTRES                           EQW9Z0MQ
03014      IF ECR-SIVINDCO = LOW-VALUE                                  EQW9Z0MQ
03015         MOVE SPACES TO ECR-SIVINDCO                               EQW9Z0MQ
03016      END-IF.                                                      EQW9Z0MQ
03017                                                                   EQW9Z0MQ
03018      IF ECR-SIVINDCO NOT = SPACES                                 EQW9Z0MQ
03019         IF ECR-SIVINDCO NOT = 'O' AND 'N'                         EQW9Z0MQ
03020            MOVE NOR-ALP TO ECR-SIVINDCA                           EQW9Z0MQ
03021            IF KONTROL = 0                                         EQW9Z0MQ
03022               MOVE 'FB019' TO COM-GENE-MESANO                     EQW9Z0MQ
03023                               COM-CODERR                          EQW9Z0MQ
03024               MOVE CURSEUR TO ECR-SIVINDCL                        EQW9Z0MQ
03025               MOVE 1       TO KONTROL                             EQW9Z0MQ
03026               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
03027            END-IF                                                 EQW9Z0MQ
03028         END-IF                                                    EQW9Z0MQ
03029      END-IF.                                                      EQW9Z0MQ
03030                                                                   EQW9Z0MQ
      *---MOTIF DE RESIL DU CONTRAT PRECEDENT
           IF ECR-ANVMTRCO = LOW-VALUE
                MOVE SPACES TO ECR-ANVMTRCO
           END-IF.

           IF ECR-ANVMTRCO NOT = SPACES
                IF ECR-ANVMTRCO NOT = '0' AND '1'
                    MOVE NOR-ALP TO ECR-ANVMTRCA
                    IF KONTROL = 0
                       MOVE 'FB435' TO COM-GENE-MESANO
                                       COM-CODERR
                       MOVE CURSEUR TO ECR-ANVMTRCL
                       MOVE 1       TO KONTROL
                       GO           TO FIN-CONTROLE-SYNTAXE
                    END-IF
03031           END-IF                                                  EQW9Z0MQ
03032      END-IF.                                                      EQW9Z0MQ

03031 *---ANCIEN COEFFICIENT DE REDUCTION-MAJORATION                    EQW9Z0MQ
03032      IF ECR-ANVBONTO = LOW-VALUE                                  EQW9Z0MQ
03033         MOVE SPACES TO ECR-ANVBONTO                               EQW9Z0MQ
03034      END-IF.                                                      EQW9Z0MQ
03035                                                                   EQW9Z0MQ
03036      IF ECR-ANVBONTO NOT = SPACES                                 EQW9Z0MQ
03037         MOVE ECR-ANVBONTO  TO C-XKMTENTREE                        EQW9Z0MQ
03038         MOVE 3             TO C-XKMTLONG                          EQW9Z0MQ
03039         MOVE 0             TO C-XKMTDECIMALE                      EQW9Z0MQ
03040         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW9Z0MQ
03041         IF C-XKMTRETCOD NOT = 0                                   EQW9Z0MQ
03042            MOVE NOR-ALP TO ECR-ANVBONTA                           EQW9Z0MQ
03043            IF KONTROL = 0                                         EQW9Z0MQ
03044               MOVE 'FB152' TO COM-GENE-MESANO                     EQW9Z0MQ
03045                               COM-CODERR                          EQW9Z0MQ
03046               MOVE CURSEUR TO ECR-ANVBONTL                        EQW9Z0MQ
03047               MOVE 1       TO KONTROL                             EQW9Z0MQ
03048               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
03049            END-IF                                                 EQW9Z0MQ
03050         ELSE                                                      EQW9Z0MQ
03051            MOVE C-XKMTNUM-0D TO WSS-CRM                           EQW9Z0MQ
03052                                 WSS-CRMZ                          EQW9Z0MQ
03053            MOVE WSS-CRMZ     TO ECR-ANVBONTO                      EQW9Z0MQ
03054            IF WSS-CRM < 050 OR > 350                              EQW9Z0MQ
03055               MOVE NOR-ALP TO ECR-ANVBONTA                        EQW9Z0MQ
03056               IF KONTROL = 0                                      EQW9Z0MQ
03057                  MOVE 'FB152' TO COM-GENE-MESANO                  EQW9Z0MQ
03058                                  COM-CODERR                       EQW9Z0MQ
03059                  MOVE CURSEUR TO ECR-ANVBONTL                     EQW9Z0MQ
03060                  MOVE 1       TO KONTROL                          EQW9Z0MQ
03061                  GO           TO FIN-CONTROLE-SYNTAXE             EQW9Z0MQ
03062               END-IF                                              EQW9Z0MQ
03063            END-IF                                                 EQW9Z0MQ
03064         END-IF                                                    EQW9Z0MQ
03065      END-IF.                                                      EQW9Z0MQ
03066                                                                   EQW9Z0MQ
03067 *---DATE D'ACQUISITION DU COEFFICIENT DE REDUCTION-MAJORATION     EQW9Z0MQ
03068      IF ECR-ANVBONDO = LOW-VALUE                                  EQW9Z0MQ
03069         MOVE SPACES TO ECR-ANVBONDO                               EQW9Z0MQ
03070      END-IF.                                                      EQW9Z0MQ
03071                                                                   EQW9Z0MQ
03072      IF ECR-ANVBONDO NOT = SPACES                                 EQW9Z0MQ
03073         MOVE ECR-ANVBONDO TO C-XKMTENTREE                         EQW9Z0MQ
03074         MOVE 6 TO C-XKMTLONG                                      EQW9Z0MQ
03075         MOVE ZERO TO C-XKMTDECIMALE                               EQW9Z0MQ
03076         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW9Z0MQ
03077         IF C-XKMTRETCOD NOT = ZERO                                EQW9Z0MQ
03078            MOVE NOR-ALP TO ECR-ANVBONDA                           EQW9Z0MQ
03079            IF KONTROL = 0                                         EQW9Z0MQ
03080               MOVE 'FB153' TO COM-GENE-MESANO                     EQW9Z0MQ
03081                               COM-CODERR                          EQW9Z0MQ
03082               MOVE CURSEUR TO ECR-ANVBONDL                        EQW9Z0MQ
03083               MOVE 1       TO KONTROL                             EQW9Z0MQ
03084               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
03085            END-IF                                                 EQW9Z0MQ
03086         ELSE                                                      EQW9Z0MQ
03087            MOVE C-XKMTSORTIE  TO WSS-DATE-TRAV                    EQW9Z0MQ
03088                                  WSS-DATE-ACQUI                   EQW9Z0MQ
03089            PERFORM VERIF-DATE THRU FIN-VERIF-DATE                 EQW9Z0MQ
03090            IF WSS-DATE-OK = 'N'                                   EQW9Z0MQ
03091               MOVE NOR-ALP TO ECR-ANVBONDA                        EQW9Z0MQ
03092               IF KONTROL = 0                                      EQW9Z0MQ
03093                  MOVE 'FB153' TO COM-GENE-MESANO                  EQW9Z0MQ
03094                                  COM-CODERR                       EQW9Z0MQ
03095                  MOVE CURSEUR TO ECR-ANVBONDL                     EQW9Z0MQ
03096                  MOVE 1       TO KONTROL                          EQW9Z0MQ
03097                  GO           TO FIN-CONTROLE-SYNTAXE             EQW9Z0MQ
03098               END-IF                                              EQW9Z0MQ
03099            ELSE                                                   EQW9Z0MQ
03100               MOVE WSS-DATE-TRAV(1:2) TO WSS-MM   OF WSS-SSAAMMJJ EQW9Z0MQ
03101               MOVE WSS-DATE-TRAV(3:4) TO WSS-SSAA OF WSS-SSAAMMJJ EQW9Z0MQ
03102               MOVE WSS-FB-DATJOUR-SS  TO WSS-DATE-JOUR-SAMJ-S     EQW9Z0MQ
03103               MOVE WSS-FB-DATJOUR-AA  TO WSS-DATE-JOUR-SAMJ-A     EQW9Z0MQ
03104               MOVE WSS-FB-DATJOUR-MM  TO WSS-DATE-JOUR-SAMJ-M     EQW9Z0MQ
03105               MOVE WSS-FB-DATJOUR-JJ  TO WSS-DATE-JOUR-SAMJ-J     EQW9Z0MQ
03106               IF WSS-SSAAMMJJ(1:6) > WSS-DATE-JOUR-SAMJ(1:6)      EQW9Z0MQ
03107                  MOVE NOR-ALP TO ECR-ANVBONDA                     EQW9Z0MQ
03108                  IF KONTROL = 0                                   EQW9Z0MQ
03109                     MOVE 'FB153' TO COM-GENE-MESANO               EQW9Z0MQ
03110                                     COM-CODERR                    EQW9Z0MQ
03111                     MOVE CURSEUR TO ECR-ANVBONDL                  EQW9Z0MQ
03112                     MOVE 1       TO KONTROL                       EQW9Z0MQ
03113                     GO           TO FIN-CONTROLE-SYNTAXE          EQW9Z0MQ
03114                  END-IF                                           EQW9Z0MQ
03115               ELSE                                                EQW9Z0MQ
03116                  MOVE WSS-DATE-ACQUI TO ECR-ANVBONDO              EQW9Z0MQ
03117               END-IF                                              EQW9Z0MQ
03118            END-IF                                                 EQW9Z0MQ
03119         END-IF                                                    EQW9Z0MQ
03120      END-IF.                                                      EQW9Z0MQ
03121                                                                   EQW9Z0MQ
03122 *---DATE D'ACQUISITION DU CRM = 050                               EQW9Z0MQ
03123      IF ECR-ANVABODO = LOW-VALUE                                  EQW9Z0MQ
03124         MOVE SPACES TO ECR-ANVABODO                               EQW9Z0MQ
03125      END-IF.                                                      EQW9Z0MQ
03126                                                                   EQW9Z0MQ
03127      IF ECR-ANVABODO NOT = SPACES                                 EQW9Z0MQ
03128         MOVE ECR-ANVABODO  TO C-XKMTENTREE                        EQW9Z0MQ
03129         MOVE 4             TO C-XKMTLONG                          EQW9Z0MQ
03130         MOVE 0             TO C-XKMTDECIMALE                      EQW9Z0MQ
03131         PERFORM CADRAGE THRU FIN-CADRAGE                          EQW9Z0MQ
03132         IF C-XKMTRETCOD NOT = 0                                   EQW9Z0MQ
03133            MOVE NOR-ALP TO ECR-ANVABODA                           EQW9Z0MQ
03134            IF KONTROL = 0                                         EQW9Z0MQ
03135               MOVE 'FB154' TO COM-GENE-MESANO                     EQW9Z0MQ
03136                               COM-CODERR                          EQW9Z0MQ
03137               MOVE CURSEUR TO ECR-ANVABODL                        EQW9Z0MQ
03138               MOVE 1       TO KONTROL                             EQW9Z0MQ
03139               GO           TO FIN-CONTROLE-SYNTAXE                EQW9Z0MQ
03140            END-IF                                                 EQW9Z0MQ
03141         ELSE                                                      EQW9Z0MQ
03142            MOVE C-XKMTSORTIE TO WSS-DATE-TRAV(3:4)                EQW9Z0MQ
03143                                 WSS-DATE50                        EQW9Z0MQ
03144            MOVE '01'         TO WSS-DATE-TRAV(1:2)                EQW9Z0MQ
03145            PERFORM VERIF-DATE THRU FIN-VERIF-DATE                 EQW9Z0MQ
03146            IF WSS-DATE-OK = 'N'                                   EQW9Z0MQ
03147               MOVE NOR-ALP TO ECR-ANVABODA                        EQW9Z0MQ
03148               IF KONTROL = 0                                      EQW9Z0MQ
03149                  MOVE 'FB154' TO COM-GENE-MESANO                  EQW9Z0MQ
03150                                  COM-CODERR                       EQW9Z0MQ
03151                  MOVE CURSEUR TO ECR-ANVABODL                     EQW9Z0MQ
03152                  MOVE 1       TO KONTROL                          EQW9Z0MQ
03153                  GO           TO FIN-CONTROLE-SYNTAXE             EQW9Z0MQ
03154               END-IF                                              EQW9Z0MQ
03155            ELSE                                                   EQW9Z0MQ
03156               MOVE WSS-FB-DATJOUR-SS  TO WSS-DATE-JOUR-SAMJ-S     EQW9Z0MQ
03157               MOVE WSS-FB-DATJOUR-AA  TO WSS-DATE-JOUR-SAMJ-A     EQW9Z0MQ
03158               MOVE WSS-FB-DATJOUR-MM  TO WSS-DATE-JOUR-SAMJ-M     EQW9Z0MQ
03159               MOVE WSS-FB-DATJOUR-JJ  TO WSS-DATE-JOUR-SAMJ-J     EQW9Z0MQ
03160               IF (WSS-DATE50) > (WSS-DATE-JOUR-SAMJ (1:4))        EQW9Z0MQ
03161                  MOVE NOR-ALP TO ECR-ANVABODA                     EQW9Z0MQ
03162                  IF KONTROL = 0                                   EQW9Z0MQ
03163                     MOVE 'FB154' TO COM-GENE-MESANO               EQW9Z0MQ
03164                                     COM-CODERR                    EQW9Z0MQ
03165                     MOVE CURSEUR TO ECR-ANVABODL                  EQW9Z0MQ
03166                     MOVE  1      TO KONTROL                       EQW9Z0MQ
03167                     GO           TO FIN-CONTROLE-SYNTAXE          EQW9Z0MQ
03168                  END-IF                                           EQW9Z0MQ
03169               ELSE                                                EQW9Z0MQ
03170                  MOVE WSS-DATE50 TO ECR-ANVABODO                  EQW9Z0MQ
03171               END-IF                                              EQW9Z0MQ
03172            END-IF                                                 EQW9Z0MQ
03173         END-IF                                                    EQW9Z0MQ
03174      END-IF.                                                      EQW9Z0MQ
03977                                                                   EQW9Z0MQ
           IF  (INF-NATMVT OF TS-SUSPENS1 = 'AN' OR 'RP')               EQW9Z0MQ
33295       OR (INF-NATMVT OF TS-SUSPENS1 = 'AV'   AND                  EQW9Z0MQ
33295           (CCO-FORCAG1 OF TS-SUSPENS1 = 'F'
33295         OR CCO-FORCAG2 OF TS-SUSPENS1 = 'F'
33295         OR CCO-FORCAG3 OF TS-SUSPENS1 = 'F'
33295         OR CCO-FORCAG4 OF TS-SUSPENS1 = 'F'))
33295       OR ((INF-NATMVT OF TS-SUSPENS1 = 'RV')                      EQW9Z0MQ
                 AND ((COM-FB-CODE-ACTION = '1' OR '2' OR '3')          EQW9Z0MQ
                 OR (VEHACTC OF TS-VEHICULE(1) = 'I'                    EQW9Z0MQ
                  AND VEHCHGC OF TS-VEHICULE(1) NOT = '1' AND '2')))    EQW9Z0MQ
33295       OR (INF-NATMVT OF TS-SUSPENS1 = 'AV'   AND                  EQW9Z0MQ
33295           (CCO-FORCAG1 OF TS-SUSPENS1 NOT = 'F'
33295        AND CCO-FORCAG2 OF TS-SUSPENS1 NOT = 'F'
33295        AND CCO-FORCAG3 OF TS-SUSPENS1 NOT = 'F'
33295        AND CCO-FORCAG4 OF TS-SUSPENS1 NOT = 'F') AND
33295           ((COM-FB-CODE-ACTION = '1' OR '2' OR '3')               EQW9Z0MQ
33295         OR (VEHACTC OF TS-VEHICULE(1) = 'I' AND                   EQW9Z0MQ
33295             VEHCHGC OF TS-VEHICULE(1) NOT = '1' AND '2')))        EQW9Z0MQ
03978 *---LES VALEURS POSSIBLES POUR L'ANCIEN SOUSCRIPTEUR DU           EQW9Z0MQ
03978 *---PRECEDENT CONTRAT SONT 'CF', 'CJ', 'ENAP' ET 'SOUS'           EQW9Z0MQ
               IF ECR-ANVSOUSO NOT = SPACES AND LOW-VALUE                   EQW9
                  IF  (ECR-ANVSOUSO NOT = 'CF  ')                           EQW9
                  AND (ECR-ANVSOUSO NOT = 'CJ  ')                           EQW9
                  AND (ECR-ANVSOUSO NOT = 'SOUS')                           EQW9
                  AND (ECR-ANVSOUSO NOT = 'ENAP')                           EQW9
                      MOVE NOR-ALP    TO ECR-ANVSOUSA                       EQW9
                      IF KONTROL = 0                                        EQW9
                         MOVE 'FBTD5' TO COM-GENE-MESANO                    EQW9
                                         COM-CODERR                         EQW9
                         MOVE CURSEUR TO ECR-ANVSOUSL                       EQW9
                         MOVE 1       TO KONTROL                            EQW9
                         GO           TO FIN-CONTROLE-SYNTAXE               EQW9
                      END-IF                                                EQW9
03989             END-IF                                                EQW9Z0MQ
               END-IF                                                       EQW9
03991      END-IF.                                                      EQW9Z0MQ
03175                                                                   EQW9Z0MQ
03176  FIN-CONTROLE-SYNTAXE.                                            EQW9Z0MQ
03177      EXIT.                                                        EQW9Z0MQ
03178 *                                                                 EQW9Z0MQ
03179 **************************************************************    EQW9Z0MQ
03180 *    TEST DU CODE VEHICULE POUR SAVOIR SI IL Y A APPEL A     *    EQW9Z0MQ
03181 *            L'ECRAN D'AIDE VEHICULE                         *    EQW9Z0MQ
03182 **************************************************************    EQW9Z0MQ
03183  DETER-AIDE-CDVEHI.                                               EQW9Z0MQ
03184 *------------------                                               EQW9Z0MQ
03185      MOVE ZERO  TO WSS-NB-POINT-INTERRO.                          EQW9Z0MQ
03186      INSPECT ECR-VEHCODCO TALLYING WSS-NB-POINT-INTERRO           EQW9Z0MQ
03187                           FOR ALL '?'.                            EQW9Z0MQ
03188                                                                   EQW9Z0MQ
03189      IF WSS-NB-POINT-INTERRO = ZERO                               EQW9Z0MQ
03190         MOVE 'N'   TO WSS-APPEL-AIDE-CDVEHI                       EQW9Z0MQ
03191      ELSE                                                         EQW9Z0MQ
03192         MOVE 'O'   TO WSS-APPEL-AIDE-CDVEHI                       EQW9Z0MQ
03193      END-IF.                                                      EQW9Z0MQ
03194                                                                   EQW9Z0MQ
03195  FIN-DETER-AIDE-CDVEHI.                                           EQW9Z0MQ
03196      EXIT.                                                        EQW9Z0MQ
03197 *                                                                 EQW9Z0MQ
03198 **************************************************************    EQW9Z0MQ
03199 *    TEST DU CODE GENRE POUR SAVOIR SI IL Y APPEL DE L'ECRAN *    EQW9Z0MQ
03200 *                    D'AIDE GENRE                            *    EQW9Z0MQ
03201 **************************************************************    EQW9Z0MQ
03202  DETER-AIDE-GENRE.                                                EQW9Z0MQ
03203 *-----------------                                                EQW9Z0MQ
03204      MOVE ZERO  TO WSS-NB-POINT-INTERRO.                          EQW9Z0MQ
03205      INSPECT ECR-VEHGENCO TALLYING WSS-NB-POINT-INTERRO           EQW9Z0MQ
03206                          FOR ALL '?'.                             EQW9Z0MQ
03207                                                                   EQW9Z0MQ
03208      IF WSS-NB-POINT-INTERRO = ZERO                               EQW9Z0MQ
03209         MOVE 'N'   TO WSS-APPEL-AIDE-GENRE                        EQW9Z0MQ
03210      ELSE                                                         EQW9Z0MQ
03211         MOVE 'O'   TO WSS-APPEL-AIDE-GENRE                        EQW9Z0MQ
03212      END-IF.                                                      EQW9Z0MQ
03213                                                                   EQW9Z0MQ
03214  FIN-DETER-AIDE-GENRE.                                            EQW9Z0MQ
03215      EXIT.                                                        EQW9Z0MQ
03216 *                                                                 EQW9Z0MQ
03217 **************************************************************    EQW9Z0MQ
03218 *    TEST DU CODE USAGE POUR SAVOIR SI IL Y APPEL DE L'ECRAN *    EQW9Z0MQ
03219 *                    D'AIDE USAGE                            *    EQW9Z0MQ
03220 **************************************************************    EQW9Z0MQ
03221  DETER-AIDE-USAGE.                                                EQW9Z0MQ
03222 *-----------------                                                EQW9Z0MQ
03223      MOVE ZERO  TO WSS-NB-POINT-INTERRO.                          EQW9Z0MQ
03224      INSPECT ECR-VEHUSACO TALLYING WSS-NB-POINT-INTERRO           EQW9Z0MQ
03225                          FOR ALL '?'.                             EQW9Z0MQ
03226                                                                   EQW9Z0MQ
03227      IF WSS-NB-POINT-INTERRO = ZERO                               EQW9Z0MQ
03228         MOVE 'N' TO WSS-APPEL-AIDE-USAGE                          EQW9Z0MQ
03229      ELSE                                                         EQW9Z0MQ
03230         MOVE 'O' TO WSS-APPEL-AIDE-USAGE                          EQW9Z0MQ
03231      END-IF.                                                      EQW9Z0MQ
03232                                                                   EQW9Z0MQ
03233  FIN-DETER-AIDE-USAGE.                                            EQW9Z0MQ
03234      EXIT.                                                        EQW9Z0MQ
03235 *                                                                 EQW9Z0MQ
03236 **************************************************************    EQW9Z0MQ
03237 *    TEST DU CODE PROTECTION POUR SAVOIR S'IL Y A APPEL DE   *    EQW9Z0MQ
03238 *                    L'ECRAN D'AIDE NIVEAU DE PROTECTION     *    EQW9Z0MQ
03239 **************************************************************    EQW9Z0MQ
03240  DETER-AIDE-PROTECTION.                                           EQW9Z0MQ
03241 *----------------------                                           EQW9Z0MQ
03242      MOVE ZERO  TO WSS-NB-POINT-INTERRO.                          EQW9Z0MQ
03243      INSPECT ECR-VEHPRTCO TALLYING WSS-NB-POINT-INTERRO           EQW9Z0MQ
03244                          FOR ALL '?'.                             EQW9Z0MQ
03245                                                                   EQW9Z0MQ
03246      IF WSS-NB-POINT-INTERRO = ZERO                               EQW9Z0MQ
03247         MOVE 'N' TO WSS-APPEL-AIDE-PROT                           EQW9Z0MQ
03248      ELSE                                                         EQW9Z0MQ
03249         MOVE 'O' TO WSS-APPEL-AIDE-PROT                           EQW9Z0MQ
03250      END-IF.                                                      EQW9Z0MQ
03251                                                                   EQW9Z0MQ
03252  FIN-DETER-AIDE-PROTECTION.                                       EQW9Z0MQ
03253      EXIT.                                                        EQW9Z0MQ
03254 *                                                                 EQW9Z0MQ
03255 ****************************************************************  EQW9Z0MQ
03256 *  TEST DU CODE POSTAL AFIN DE DETERMINER SI IL Y A APPEL DE   *  EQW9Z0MQ
03257 *  L'ECRAN D'AIDE CODE POSTAL                                  *  EQW9Z0MQ
03258 ****************************************************************  EQW9Z0MQ
03259  DETER-AIDE-CP.                                                   EQW9Z0MQ
03260 *--------------                                                   EQW9Z0MQ
03261      MOVE SPACES TO COM-MA-CODPOST.                               EQW9Z0MQ
03262      MOVE 'N'    TO WSS-APPEL-AIDE-CP.                            EQW9Z0MQ
03263      MOVE 'N'    TO WSS-BLANC-TROUVER.                            EQW9Z0MQ
03264      MOVE SPACES TO WSS-CODE-POSTAL-SAISI.                        EQW9Z0MQ
03265      MOVE ZERO   TO POS.                                          EQW9Z0MQ
03266      PERFORM VARYING J FROM 1 BY 1                                EQW9Z0MQ
03267                      UNTIL J > 5                                  EQW9Z0MQ
03268                      OR WSS-APPEL-AIDE-CP = 'O'                   EQW9Z0MQ
03269                      OR ECR-GARCOPCO = SPACES                     EQW9Z0MQ
03270          IF ECR-GARCOPCO (J:1) = '?'                              EQW9Z0MQ
03271             MOVE 'O'           TO WSS-APPEL-AIDE-CP               EQW9Z0MQ
03272             MOVE ECR-GARCOPCO  TO COM-MA-CODPOST                  EQW9Z0MQ
03273             COMPUTE POS = J - 1                                   EQW9Z0MQ
03274          ELSE                                                     EQW9Z0MQ
03275             MOVE ECR-GARCOPCO  TO COM-MA-CODPOST                  EQW9Z0MQ
03276          END-IF                                                   EQW9Z0MQ
03277      END-PERFORM.                                                 EQW9Z0MQ
03278                                                                   EQW9Z0MQ
03279      IF WSS-APPEL-AIDE-CP = 'O'                                   EQW9Z0MQ
03280         IF POS > 0                                                EQW9Z0MQ
03281            IF ECR-GARCOPCO (1:POS) NOT = SPACES                   EQW9Z0MQ
03282               MOVE ECR-GARCOPCO (1:POS) TO WSS-CODE-POSTAL-SAISI  EQW9Z0MQ
03283               COMPUTE POS = POS + 1                               EQW9Z0MQ
03284               PERFORM VARYING J FROM POS BY 1                     EQW9Z0MQ
03285                               UNTIL J > 5                         EQW9Z0MQ
03286                     MOVE '0' TO WSS-CODE-POSTAL-SAISI (J:1)       EQW9Z0MQ
03287               END-PERFORM                                         EQW9Z0MQ
03288            ELSE                                                   EQW9Z0MQ
03289               MOVE SPACES TO WSS-CODE-POSTAL-SAISI                EQW9Z0MQ
03290            END-IF                                                 EQW9Z0MQ
03291         ELSE                                                      EQW9Z0MQ
03292            MOVE SPACES TO WSS-CODE-POSTAL-SAISI                   EQW9Z0MQ
03293         END-IF                                                    EQW9Z0MQ
03294      ELSE                                                         EQW9Z0MQ
03295         MOVE ZERO TO POS                                          EQW9Z0MQ
03296         PERFORM VARYING J FROM 1 BY 1                             EQW9Z0MQ
03297                         UNTIL J > 5                               EQW9Z0MQ
03298                         OR WSS-BLANC-TROUVER = 'O'                EQW9Z0MQ
03299             IF ECR-GARCOPCO (J:1) = SPACES                        EQW9Z0MQ
03300                MOVE 'O' TO WSS-BLANC-TROUVER                      EQW9Z0MQ
03301                COMPUTE POS = J - 1                                EQW9Z0MQ
03302             END-IF                                                EQW9Z0MQ
03303         END-PERFORM                                               EQW9Z0MQ
03304                                                                   EQW9Z0MQ
03305         IF WSS-BLANC-TROUVER = 'O'                                EQW9Z0MQ
03306            IF POS > 0                                             EQW9Z0MQ
03307               MOVE ECR-GARCOPCO (1:POS) TO WSS-CODE-POSTAL-SAISI  EQW9Z0MQ
03308               COMPUTE POS = POS + 1                               EQW9Z0MQ
03309               PERFORM VARYING J FROM POS BY 1                     EQW9Z0MQ
03310                               UNTIL J > 5                         EQW9Z0MQ
03311                     MOVE '0' TO WSS-CODE-POSTAL-SAISI (J:1)       EQW9Z0MQ
03312               END-PERFORM                                         EQW9Z0MQ
03313            ELSE                                                   EQW9Z0MQ
03314               MOVE SPACES TO WSS-CODE-POSTAL-SAISI                EQW9Z0MQ
03315            END-IF                                                 EQW9Z0MQ
03316         ELSE                                                      EQW9Z0MQ
03317            MOVE ECR-GARCOPCO TO WSS-CODE-POSTAL-SAISI             EQW9Z0MQ
03318         END-IF                                                    EQW9Z0MQ
03319      END-IF.                                                      EQW9Z0MQ
03320                                                                   EQW9Z0MQ
03321  FIN-DETER-AIDE-CP.                                               EQW9Z0MQ
03322      EXIT.                                                        EQW9Z0MQ
03323                                                                   EQW9Z0MQ
03324 ****************************************************************  EQW9Z0MQ
03325 *  TEST DE LA COMMUNE AFIN DE DETERMINER SI IL Y A APPEL DE    *  EQW9Z0MQ
03326 *  L'ECRAN D'AIDE COMMUNE                                      *  EQW9Z0MQ
03327 ****************************************************************  EQW9Z0MQ
03328  DETER-AIDE-COM.                                                  EQW9Z0MQ
03329 *---------------                                                  EQW9Z0MQ
03330      MOVE SPACES TO COM-MA-COMMUNE.                               EQW9Z0MQ
03331      MOVE 'N'    TO WSS-APPEL-AIDE-COM.                           EQW9Z0MQ
03332      MOVE SPACES TO WSS-COMMUNE-SAISIE.                           EQW9Z0MQ
03333      PERFORM VARYING J FROM 1 BY 1                                EQW9Z0MQ
03334                         UNTIL J > 30                              EQW9Z0MQ
03335                      OR WSS-APPEL-AIDE-COM = 'O'                  EQW9Z0MQ
03336                      OR ECR-GARVILLO = SPACES                     EQW9Z0MQ
03337        IF ECR-GARVILLO (J:1) = '?'                                EQW9Z0MQ
03338           MOVE 'O' TO WSS-APPEL-AIDE-COM                          EQW9Z0MQ
03339           MOVE ECR-GARVILLO    TO  COM-MA-COMMUNE                 EQW9Z0MQ
03340           COMPUTE POS = J - 1                                     EQW9Z0MQ
03341        ELSE                                                       EQW9Z0MQ
03342           MOVE ECR-GARVILLO    TO  COM-MA-COMMUNE                 EQW9Z0MQ
03343        END-IF                                                     EQW9Z0MQ
03344      END-PERFORM.                                                 EQW9Z0MQ
03345                                                                   EQW9Z0MQ
03346      IF WSS-APPEL-AIDE-COM = 'O'                                  EQW9Z0MQ
03347         IF POS > 0                                                EQW9Z0MQ
03348            MOVE ECR-GARVILLO (1:POS)                              EQW9Z0MQ
03349                             TO WSS-COMMUNE-SAISIE                 EQW9Z0MQ
03350            COMPUTE POS = POS + 1                                  EQW9Z0MQ
03351            PERFORM VARYING J FROM POS BY 1                        EQW9Z0MQ
03352                          UNTIL J > 30                             EQW9Z0MQ
03353               MOVE SPACES TO WSS-COMMUNE-SAISIE (J:1)             EQW9Z0MQ
03354            END-PERFORM                                            EQW9Z0MQ
03355         ELSE                                                      EQW9Z0MQ
03356            MOVE SPACES TO WSS-COMMUNE-SAISIE                      EQW9Z0MQ
03357         END-IF                                                    EQW9Z0MQ
03358      ELSE                                                         EQW9Z0MQ
03359         MOVE ECR-GARVILLO TO WSS-COMMUNE-SAISIE                   EQW9Z0MQ
03360      END-IF.                                                      EQW9Z0MQ
03361                                                                   EQW9Z0MQ
03362  FIN-DETER-AIDE-COM.                                              EQW9Z0MQ
03363      EXIT.                                                        EQW9Z0MQ
03364 *                                                                 EQW9Z0MQ
03365 **************************************************************    EQW9Z0MQ
03366 * TEST DU CODE FORMULE POUR SAVOIR SI IL Y APPEL DE L'ECRAN  *    EQW9Z0MQ
03367 *                    D'AIDE FORMULE                          *    EQW9Z0MQ
03368 **************************************************************    EQW9Z0MQ
03369  DETER-AIDE-FORMULE.                                              EQW9Z0MQ
03370 *-------------------                                              EQW9Z0MQ
03371      MOVE ZERO  TO WSS-NB-POINT-INTERRO.                          EQW9Z0MQ
03372      INSPECT ECR-VEHFORCO TALLYING WSS-NB-POINT-INTERRO           EQW9Z0MQ
03373                          FOR ALL '?'.                             EQW9Z0MQ
03374                                                                   EQW9Z0MQ
03375      IF WSS-NB-POINT-INTERRO = ZERO                               EQW9Z0MQ
03376         MOVE 'N'   TO WSS-APPEL-AIDE-FORM                         EQW9Z0MQ
03377      ELSE                                                         EQW9Z0MQ
03378         MOVE 'O'   TO WSS-APPEL-AIDE-FORM                         EQW9Z0MQ
03379      END-IF.                                                      EQW9Z0MQ
03380                                                                   EQW9Z0MQ
03381  FIN-DETER-AIDE-FORMULE.                                          EQW9Z0MQ
03382      EXIT.                                                        EQW9Z0MQ
03383 *                                                                 EQW9Z0MQ
03384 **************************************************************    EQW9Z0MQ
03385 *          ACCES BASE GEBCOMM                                *    EQW9Z0MQ
03386 **************************************************************    EQW9Z0MQ
03387  ACCES-GEBCOMM.                                                   EQW9Z0MQ
03388 *--------------                                                   EQW9Z0MQ
03389      MOVE ECR-GARVILLO TO WSS-COMMUNE.                            EQW9Z0MQ
03390      MOVE ECR-GARCOPCO TO WSS-CODE-POSTAL.                        EQW9Z0MQ
03391      EXEC SQL                                                     EQW9Z0MQ
03392        SELECT  INSEEC,                                            EQW9Z0MQ
03393                ADRCOML,                                           EQW9Z0MQ
03394                ADRPSTC                                            EQW9Z0MQ
03395          INTO  :DCLGEBCOMM.INSEEC,                                EQW9Z0MQ
03396                :DCLGEBCOMM.ADRCOML,                               EQW9Z0MQ
03397                :DCLGEBCOMM.ADRPSTC                                EQW9Z0MQ
03398          FROM  DB2CIE.GEBCOMM                                     EQW9Z0MQ
F2755          WHERE SUBSTR (ADRCOML, 1, 30) = :WSS-COMMUNE             00345300
03400            AND ADRPSTC = :WSS-CODE-POSTAL                         EQW9Z0MQ
03401      END-EXEC.                                                    EQW9Z0MQ
03402                                                                   EQW9Z0MQ
03403      IF SQLCODE = ZERO OR  -811                                   EQW9Z0MQ
03404         MOVE INSEEC OF DCLGEBCOMM TO  WSS-CODE-INSEE              EQW9Z0MQ
03405      ELSE                                                         EQW9Z0MQ
PDO89         PERFORM ACCESBIS-GEBCOMM THRU FIN-ACCESBIS-GEBCOMM
03413      END-IF.                                                      EQW9Z0MQ
03414 *                                                                 EQW9Z0MQ
03415  FIN-ACCES-GEBCOMM.                                               EQW9Z0MQ
03416      EXIT.                                                        EQW9Z0MQ
PDO89 *
PDO89 **************************************************************
PDO89 *          ACCES BASE GEBCOMM BIS (SUR BUREAU DISTRIBUTEUR)  *
PDO89 **************************************************************
PDO89  ACCESBIS-GEBCOMM.
PDO89 *--------------
PDO89      MOVE ECR-GARVILLO TO WS-LIGACHL.
PDO89      MOVE ECR-GARCOPCO TO WSS-CODE-POSTAL.
PDO89      EXEC SQL
PDO89        SELECT  INSEEC,
PDO89                ADRCOML,
PDO89                ADRPSTC,
PDO89                LIGACHL
PDO89          INTO  :DCLGEBCOMM.INSEEC,
PDO89                :DCLGEBCOMM.ADRCOML,
PDO89                :DCLGEBCOMM.ADRPSTC,
PDO89                :DCLGEBCOMM.LIGACHL
PDO89          FROM  DB2CIE.GEBCOMM
PDO89          WHERE LIGACHL = :WS-LIGACHL
PDO89            AND ADRPSTC = :WSS-CODE-POSTAL
PDO89      END-EXEC.
PDO89
PDO89      IF SQLCODE = ZERO OR  -811
PDO89         MOVE INSEEC OF DCLGEBCOMM TO  WSS-CODE-INSEE
PDO89         MOVE ADRCOML OF DCLGEBCOMM TO WSS-COMMUNE
PDO89      ELSE
03406         MOVE NOR-ALP TO ECR-GARVILLA                              EQW9Z0MQ
03407         IF KONTROL = 0                                            EQW9Z0MQ
03408            MOVE 'FB106' TO COM-GENE-MESANO                        EQW9Z0MQ
03409                            COM-CODERR                             EQW9Z0MQ
03410            MOVE CURSEUR TO ECR-GARVILLL                           EQW9Z0MQ
03411            MOVE 1       TO KONTROL                                EQW9Z0MQ
03412         END-IF                                                    EQW9Z0MQ
03413      END-IF.                                                      EQW9Z0MQ
03414 *                                                                 EQW9Z0MQ
PDO89  FIN-ACCESBIS-GEBCOMM.
03416      EXIT.                                                        EQW9Z0MQ
03417 /                                                                 EQW9Z0MQ
03418 **************************************************************    EQW9Z0MQ
03419 *    TEST DU CODE COMPAGNIE POUR SAVOIR SI IL Y A APPEL A    *    EQW9Z0MQ
03420 *            L'ECRAN D'AIDE COMPAGNIE                        *    EQW9Z0MQ
03421 **************************************************************    EQW9Z0MQ
03422  DETER-AIDE-CIE.                                                  EQW9Z0MQ
           IF  (INF-NATMVT OF TS-SUSPENS1 = 'AN' OR 'RP')               EQW9Z0MQ
            OR ((INF-NATMVT OF TS-SUSPENS1 = 'AV' OR 'RV')              EQW9Z0MQ
                 AND ((COM-FB-CODE-ACTION = '1' OR '2' OR '3')          EQW9Z0MQ
33295              OR (VEHACTC OF TS-VEHICULE(1) = 'I'                  EQW9Z0MQ
           AND VEHCHGC OF TS-VEHICULE(1) NOT = '1' AND '2')))           EQW9Z0MQ
           OR WS-FORCAGE = 'O'
           MOVE ZERO  TO WSS-NB-POINT-INTERRO                           EQW9Z0MQ
               INSPECT ECR-ANVCIEXO TALLYING WSS-NB-POINT-INTERRO       EQW9Z0MQ
                                    FOR ALL '?'                         EQW9Z0MQ
03427                                                                   EQW9Z0MQ
               EVALUATE TRUE
                  WHEN WSS-NB-POINT-INTERRO NOT = ZERO
                       MOVE 'O'              TO WSS-APPEL-AIDE-CIE
                       MOVE SPACES           TO COM-MA-CODCIEC
                       MOVE SPACES           TO COM-MA-LIBCIEL
                       MOVE SPACES           TO COM-MA-NOMREDL
                  WHEN ECR-ANVCIEXO = '999'
                       IF ECR-ANVCIELO NOT = SPACES AND LOW-VALUE
                          MOVE 'N'       TO WSS-APPEL-AIDE-CIE              0035
                          IF ECR-ANVCIELO NOT = COM-MA-LIBCIEL
                             MOVE SPACES    TO COM-MA-CODCIEC
                             MOVE SPACES    TO COM-MA-LIBCIEL
                             MOVE SPACES    TO COM-MA-NOMREDL
                          END-IF
                       ELSE
                           MOVE ECR-ANVCIEXO TO COM-MA-CODCIEC
                           MOVE SPACES       TO COM-MA-LIBCIEL
                           MOVE SPACES       TO COM-MA-NOMREDL
                           MOVE 'O'          TO WSS-APPEL-AIDE-CIE
                       END-IF
                  WHEN OTHER
                       MOVE 'N'              TO WSS-APPEL-AIDE-CIE
               END-EVALUATE
03432      END-IF.                                                      EQW9Z0MQ
03433                                                                   EQW9Z0MQ
03434  FIN-DETER-AIDE-CIE.                                              EQW9Z0MQ
03435      EXIT.                                                        EQW9Z0MQ
03417 /                                                                 EQW9Z0MQ
03418 **************************************************************    EQW9Z0MQ
03419 *    TEST DU CODE ANCIEN SOUSCRIPTEUR POUR L'APPEL OU NON    *    EQW9Z0MQ
03420 *            A L'ECRAN D'AIDE AU CHOIX DE L'ENFANT           *    EQW9Z0MQ
03421 **************************************************************    EQW9Z0MQ
03422  DETER-AIDE-CHOIX-ENFANT.                                         EQW9Z0MQ
034        MOVE 'N'          TO WSS-APPEL-AIDE-CHOIX-ENFANT.            00354200
           MOVE SPACES       TO COM-MA-ANVSOUS.
           IF  (INF-NATMVT OF TS-SUSPENS1 = 'AN' OR 'RP')               EQW9Z0MQ
33295       OR (INF-NATMVT OF TS-SUSPENS1 = 'AV'   AND                  EQW9Z0MQ
33295           (CCO-FORCAG1 OF TS-SUSPENS1 = 'F'
33295         OR CCO-FORCAG2 OF TS-SUSPENS1 = 'F'
33295         OR CCO-FORCAG3 OF TS-SUSPENS1 = 'F'
33295         OR CCO-FORCAG4 OF TS-SUSPENS1 = 'F'))
33295       OR ((INF-NATMVT OF TS-SUSPENS1 = 'RV')                      EQW9Z0MQ
                 AND ((COM-FB-CODE-ACTION = '1' OR '2' OR '3')          EQW9Z0MQ
                   OR (VEHACTC OF TS-VEHICULE(1) = 'I'                  EQW9Z0MQ
33295              AND VEHCHGC OF TS-VEHICULE(1) NOT = '1' AND '2')))   EQW9Z0MQ
33295       OR (INF-NATMVT OF TS-SUSPENS1 = 'AV'   AND                  EQW9Z0MQ
33295           (CCO-FORCAG1 OF TS-SUSPENS1 NOT = 'F'
33295        AND CCO-FORCAG2 OF TS-SUSPENS1 NOT = 'F'
33295        AND CCO-FORCAG3 OF TS-SUSPENS1 NOT = 'F'
33295        AND CCO-FORCAG4 OF TS-SUSPENS1 NOT = 'F') AND
33295           ((COM-FB-CODE-ACTION = '1' OR '2' OR '3')               EQW9Z0MQ
              OR (VEHACTC OF TS-VEHICULE(1) = 'I'  AND                  EQW9Z0MQ
                  VEHCHGC OF TS-VEHICULE(1) NOT = '1' AND '2')))        EQW9Z0MQ
               IF  (ECR-ANVSOUSO = 'ENAP')
               AND (ECR-ANVSOUNO = SPACES OR LOW-VALUE)
                   MOVE 'O'          TO WSS-APPEL-AIDE-CHOIX-ENFANT
                   MOVE ECR-ANVSOUSO TO COM-MA-ANVSOUS-STATUT
               END-IF
           END-IF.
03433                                                                   EQW9Z0MQ
03434  FIN-DETER-AIDE-CHOIX-ENFANT. EXIT.                               EQW9Z0MQ
03436 /                                                                 EQW9Z0MQ
03437 ***************************************************************   EQW9Z0MQ
03438 *          RECHERCHE DU LIBELLE DE LA COMPAGNIE               *   EQW9Z0MQ
03439 ***************************************************************   EQW9Z0MQ
03440 *                                                                 EQW9Z0MQ
03441  ACCES-GECGTA.                                                    EQW9Z0MQ
03442 *-------------                                                    EQW9Z0MQ
03443      MOVE SPACE                   TO XSPIPARM.                    EQW9Z0MQ
03444      MOVE  'GP'                   TO FONCTION   OF XSPIPARM.      EQW9Z0MQ
03445      MOVE  'GECGTA99'             TO CODTAB     OF XSPIPARM.      EQW9Z0MQ
03446      MOVE  '= '                   TO OPERATEUR  OF XSPIPARM.      EQW9Z0MQ
03447      MOVE  ECR-ANVCIEXO           TO REF-POSTE  OF XSPIPARM.      EQW9Z0MQ
03448      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
03449      IF  RETCOD OF XSPIPARM  = ZERO                               EQW9Z0MQ
03450          MOVE 'O'                 TO WSS-CDCIE-TROUVE             EQW9Z0MQ
03451          MOVE IOAREA OF XSPIPARM  TO GECGTA99                     EQW9Z0MQ
03452      ELSE                                                         EQW9Z0MQ
03453          MOVE 'N'                 TO WSS-CDCIE-TROUVE             EQW9Z0MQ
03454      END-IF.                                                      EQW9Z0MQ
03455  FIN-ACCES-GECGTA.                                                EQW9Z0MQ
03456      EXIT.                                                        EQW9Z0MQ
03457 *                                                                 EQW9Z0MQ
36835  MAJUSCULE.
36835
36835      INSPECT WSS-TAMPON REPLACING     ALL 'a' BY 'A'
36835                                       ALL 'b' BY 'B'
36835                                       ALL 'c' BY 'C'
36835                                       ALL 'd' BY 'D'
36835                                       ALL 'e' BY 'E'
36835                                       ALL 'f' BY 'F'
36835                                       ALL 'g' BY 'G'
36835                                       ALL 'h' BY 'H'
36835                                       ALL 'i' BY 'I'
36835                                       ALL 'j' BY 'J'
36835                                       ALL 'k' BY 'K'
36835                                       ALL 'l' BY 'L'
36835                                       ALL 'm' BY 'M'
36835                                       ALL 'n' BY 'N'
36835                                       ALL 'o' BY 'O'
36835                                       ALL 'p' BY 'P'
36835                                       ALL 'q' BY 'Q'
36835                                       ALL 'r' BY 'R'
36835                                       ALL 's' BY 'S'
36835                                       ALL 't' BY 'T'
36835                                       ALL 'u' BY 'U'
36835                                       ALL 'v' BY 'V'
36835                                       ALL 'w' BY 'W'
36835                                       ALL 'x' BY 'X'
36835                                       ALL 'y' BY 'Y'
36835                                       ALL 'z' BY 'Z'
36835                                       ALL 'È' BY 'E'
36835                                       ALL 'Ë' BY 'E'
36835                                       ALL 'Í' BY 'E'
36835                                       ALL '‡' BY 'A'
36835                                       ALL '˘' BY 'U'
36835                                       ALL 'Ù' BY 'O' .
36835
36835  F-MAJUSCULE.
36835      EXIT.
03458 *                                                                 EQW9Z0MQ
03459 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
03460 * CONTROLES LOGIQUES    * FB04 * TRAITEMENT NORMAL                EQW9Z0MQ
03461 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
03462  CONTROLE-LOGIQUE.                                                EQW9Z0MQ
03463 *----------------*                                                EQW9Z0MQ
03464      MOVE SPACES TO VEHGENC  OF TS-VEHICULE(2).                   EQW9Z0MQ
03465      MOVE SPACES TO VEHMARL  OF TS-VEHICULE(2).                   EQW9Z0MQ
03466      MOVE SPACES TO VEHMODL  OF TS-VEHICULE(2).                   EQW9Z0MQ
03467      MOVE SPACES TO RVEHCYLN OF TS-VEHICULE(2).                   EQW9Z0MQ
03468      MOVE SPACES TO ANVCIEL  OF TS-VEHICULE(2).                   EQW9Z0MQ
F3215      MOVE SPACES TO VEHGROC  OF TS-VEHICULE(2).                   EQW9Z0MQ
F3215      MOVE SPACES TO VEHCLAC  OF TS-VEHICULE(2).                   EQW9Z0MQ
36835      MOVE SPACES TO VEHNRJC  OF TS-VEHICULE(2).                   EQW9Z0MQ
03469                                                                   EQW9Z0MQ
03470      IF ECR-VEHTYPCO = '2R '                                      EQW9Z0MQ
03471         PERFORM CONTROLE-LOGIQUE-2R THRU FIN-CONTROLE-LOGIQUE-2R  EQW9Z0MQ
03472      END-IF.                                                      EQW9Z0MQ
03473                                                                   EQW9Z0MQ
03474      IF ECR-VEHTYPCO = '4R ' OR 'CC '                             EQW9Z0MQ
03475         PERFORM CONTROLE-LOGIQUE-4R THRU FIN-CONTROLE-LOGIQUE-4R  EQW9Z0MQ
03476      END-IF.                                                      EQW9Z0MQ
03477                                                                   EQW9Z0MQ
03478 *--- LE GENRE DU VEHICULE NE DOIT PAS ETRE MODIFIE SI LE          EQW9Z0MQ
03479 *--- CODE VEHICULE EST DIFFERENT DE 9999999                       EQW9Z0MQ
03480      IF WSS-APPEL-AIDE-CDVEHI = 'N'                               EQW9Z0MQ
03481       AND WSS-MODIF-CODE-AUTO NOT = 'O'                           EQW9Z0MQ
03482         IF (ECR-VEHCODCO NOT = SPACES AND '9999999')              EQW9Z0MQ
03483            IF ECR-VEHGENCO NOT = SPACES                           EQW9Z0MQ
03484               AND WSS-APPEL-AIDE-GENRE = 'N'                      EQW9Z0MQ
03485               IF ECR-VEHGENCO NOT = VEHGENC OF TS-VEHICULE(2)     EQW9Z0MQ
39101                  IF ECR-VEHGENCO = 'D' AND
39101                     VEHGENC OF TS-VEHICULE(2) = 'M'
39101                     NEXT SENTENCE
39101                  ELSE
39101                     MOVE NOR-ALP TO ECR-VEHGENCA                  EQW9Z0MQ
39101                     IF KONTROL = 0                                EQW9Z0MQ
39101                        MOVE 'FB110' TO COM-GENE-MESANO            EQW9Z0MQ
39101                                        COM-CODERR                 EQW9Z0MQ
39101                        MOVE CURSEUR TO ECR-VEHGENCL               EQW9Z0MQ
39101                        MOVE 1       TO KONTROL                    EQW9Z0MQ
39101                        GO           TO FIN-CONTROLE-LOGIQUE       EQW9Z0MQ
39101                     END-IF                                        EQW9Z0MQ
03493                  END-IF                                           EQW9Z0MQ
03494               END-IF                                              EQW9Z0MQ
03495            END-IF                                                 EQW9Z0MQ
03496         END-IF                                                    EQW9Z0MQ
03497      END-IF.                                                      EQW9Z0MQ
03498                                                                   EQW9Z0MQ
03499 *--- LA MARQUE DU VEHICULE NE DOIT PAS ETRE MODIFIE SI LE         EQW9Z0MQ
03500 *--- CODE VEHICULE EST DIFFERENT DE 9999999                       EQW9Z0MQ
03501      IF WSS-APPEL-AIDE-CDVEHI = 'N'                               EQW9Z0MQ
03502       AND WSS-MODIF-CODE-AUTO NOT = 'O'                           EQW9Z0MQ
03503         IF (ECR-VEHCODCO NOT = SPACES AND '9999999')              EQW9Z0MQ
03504            IF ECR-VEHMARLO NOT = SPACES                           EQW9Z0MQ
03505               IF ECR-VEHMARLO NOT = VEHMARL OF TS-VEHICULE(2)     EQW9Z0MQ
03506                  MOVE NOR-ALP TO ECR-VEHMARLA                     EQW9Z0MQ
03507                  IF KONTROL = 0                                   EQW9Z0MQ
03508                     MOVE 'FB107' TO COM-GENE-MESANO               EQW9Z0MQ
03509                                     COM-CODERR                    EQW9Z0MQ
03510                     MOVE CURSEUR TO ECR-VEHMARLL                  EQW9Z0MQ
03511                     MOVE 1       TO KONTROL                       EQW9Z0MQ
03512                     GO           TO FIN-CONTROLE-LOGIQUE          EQW9Z0MQ
03513                  END-IF                                           EQW9Z0MQ
03514               END-IF                                              EQW9Z0MQ
03515            END-IF                                                 EQW9Z0MQ
03516         END-IF                                                    EQW9Z0MQ
03517      END-IF.                                                      EQW9Z0MQ
03518                                                                   EQW9Z0MQ
03519 *--- LE MODELE DU VEHICULE NE DOIT PAS ETRE MODIFIE SI LE         EQW9Z0MQ
03520 *--- CODE VEHICULE EST DIFFERENT DE 9999999                       EQW9Z0MQ
03521      IF WSS-APPEL-AIDE-CDVEHI = 'N'                               EQW9Z0MQ
03522       AND WSS-MODIF-CODE-AUTO NOT = 'O'                           EQW9Z0MQ
03523         IF (ECR-VEHCODCO NOT = SPACES AND '9999999')              EQW9Z0MQ
03524            IF ECR-VEHMODLO NOT = SPACES                           EQW9Z0MQ
03525               IF ECR-VEHMODLO NOT = VEHMODL OF TS-VEHICULE(2)     EQW9Z0MQ
03526                  MOVE NOR-ALP TO ECR-VEHMODLA                     EQW9Z0MQ
03527                  IF KONTROL = 0                                   EQW9Z0MQ
03528                     MOVE 'FB108' TO COM-GENE-MESANO               EQW9Z0MQ
03529                                     COM-CODERR                    EQW9Z0MQ
03530                     MOVE CURSEUR TO ECR-VEHMODLL                  EQW9Z0MQ
03531                     MOVE 1       TO KONTROL                       EQW9Z0MQ
03532                     GO           TO FIN-CONTROLE-LOGIQUE          EQW9Z0MQ
03533                  END-IF                                           EQW9Z0MQ
03534               END-IF                                              EQW9Z0MQ
03535            END-IF                                                 EQW9Z0MQ
03536         END-IF                                                    EQW9Z0MQ
03537      END-IF.                                                      EQW9Z0MQ
03538                                                                   EQW9Z0MQ
03539 *--- LA PUISSANCE DU VEHICULE NE DOIT PAS ETRE MODIFIE SI LE      EQW9Z0MQ
03540 *--- CODE VEHICULE EST DIFFERENT DE 9999999                       EQW9Z0MQ
03541      IF WSS-APPEL-AIDE-CDVEHI = 'N'                               EQW9Z0MQ
03542       AND WSS-MODIF-CODE-AUTO NOT = 'O'                           EQW9Z0MQ
03543         IF (ECR-VEHCODCO NOT = SPACES AND '9999999')              EQW9Z0MQ
03544            IF ECR-VEHCYLNO NOT = SPACES                           EQW9Z0MQ
03545               MOVE VEHCYLN OF TS-VEHICULE(2) TO  WSS-CYLZ         EQW9Z0MQ
03546               IF ECR-VEHCYLNO NOT = WSS-CYLZ                      EQW9Z0MQ
03547                  MOVE NOR-ALP TO ECR-VEHCYLNA                     EQW9Z0MQ
03548                  IF KONTROL = 0                                   EQW9Z0MQ
03549                     MOVE 'FB109' TO COM-GENE-MESANO               EQW9Z0MQ
03550                                     COM-CODERR                    EQW9Z0MQ
03551                     MOVE CURSEUR TO ECR-VEHCYLNL                  EQW9Z0MQ
03552                     MOVE 1       TO KONTROL                       EQW9Z0MQ
03553                     GO           TO FIN-CONTROLE-LOGIQUE          EQW9Z0MQ
03554                  END-IF                                           EQW9Z0MQ
03555               END-IF                                              EQW9Z0MQ
03556            END-IF                                                 EQW9Z0MQ
03557         END-IF                                                    EQW9Z0MQ
03558      END-IF.                                                      EQW9Z0MQ
03559                                                                   EQW9Z0MQ
03560 *--- STATUT PM OBLIGATOIRE POUR USAGE COMMERCANT/ARTISAN          EQW9Z0MQ
03561 *--- USAGE DU VEHICULE DOIT ÍTRE ËGAL ‡ '209', '309', '210', '310'EQW9Z0MQ
03562 *--- COMMERCANT/ARTISAN SI LE STATUT PM EST PRÈSENT AU CONTRAT    EQW9Z0MQ
03563 *    IF WSS-APPEL-AIDE-USAGE = 'N'                                EQW9Z0MQ
03564 *       IF ECR-VEHUSACO = '209' OR '309' OR '210' OR '310'        EQW9Z0MQ
03565 *          IF COM-FB-NBRE-PERS-PM < 1                             EQW9Z0MQ
03566 *             MOVE NOR-ALP    TO ECR-VEHUSACA                     EQW9Z0MQ
03567 *             IF KONTROL = 0                                      EQW9Z0MQ
03568 *                MOVE 'FB230' TO COM-GENE-MESANO                  EQW9Z0MQ
03569 *                                COM-CODERR                       EQW9Z0MQ
03570 *                MOVE CURSEUR TO ECR-VEHUSACL                     EQW9Z0MQ
03571 *                MOVE 1       TO KONTROL                          EQW9Z0MQ
03572 *                GO           TO FIN-CONTROLE-LOGIQUE             EQW9Z0MQ
03573 *             END-IF                                              EQW9Z0MQ
03574 *          END-IF                                                 EQW9Z0MQ
03575 *       END-IF                                                    EQW9Z0MQ
03576 *    END-IF.                                                      EQW9Z0MQ
03577 *                                                                 EQW9Z0MQ
F2980 *--- SI PRESENCE D'UN USAGE PRIVE (109, 110 OU 113) ET            EQW9Z0MQ
F2980 *--- GENRE DU VEHICULE (G, H OU J)                                EQW9Z0MQ
F2980      IF (ECR-VEHUSACO = '109' OR '110' OR '113')                  EQW9Z0MQ
F2980      AND (ECR-VEHGENCO = 'G' OR 'H' OR 'J')                       EQW9Z0MQ
F2980           MOVE NOR-ALP    TO ECR-VEHUSACA                         EQW9Z0MQ
F2980           IF KONTROL = 0                                          EQW9Z0MQ
F2980                  MOVE 'FBT29' TO COM-GENE-MESANO                  EQW9Z0MQ
F2980                                  COM-CODERR                       EQW9Z0MQ
F2980                  MOVE CURSEUR TO ECR-VEHUSACL                     EQW9Z0MQ
F2980                  MOVE 1       TO KONTROL                          EQW9Z0MQ
F2980                  GO           TO FIN-CONTROLE-LOGIQUE             EQW9Z0MQ
F2980           END-IF                                                  EQW9Z0MQ
F2980       END-IF.                                                     EQW9Z0MQ

03578 *--- SI PRESENCE D'UN USAGE AGRICOLE (201 OU 202) ET              EQW9Z0MQ
03579 *--- GENRE DU VEHICULE (G OU J) TOUT AUTRE CODE USAGE EST INTERDITEQW9Z0MQ
03580 *    IF WSS-APPEL-AIDE-USAGE = 'N'                                EQW9Z0MQ
03581 *       IF (ECR-VEHUSACO = '201' OR '202')                        EQW9Z0MQ
03582 *         AND (ECR-VEHGENCO = 'G' OR 'J')                         EQW9Z0MQ
03583 *          IF COM-FB-TOP-SANS-TAXE-AGRI = 'O'                     EQW9Z0MQ
03584 *             MOVE NOR-ALP    TO ECR-VEHUSACA                     EQW9Z0MQ
03585 *             IF KONTROL = 0                                      EQW9Z0MQ
03586 *                MOVE 'FB249' TO COM-GENE-MESANO                  EQW9Z0MQ
03587 *                                COM-CODERR                       EQW9Z0MQ
03588 *                MOVE CURSEUR TO ECR-VEHUSACL                     EQW9Z0MQ
03589 *                MOVE 1       TO KONTROL                          EQW9Z0MQ
03590 *                GO           TO FIN-CONTROLE-LOGIQUE             EQW9Z0MQ
03591 *             END-IF                                              EQW9Z0MQ
03592 *          END-IF                                                 EQW9Z0MQ
03593 *       END-IF                                                    EQW9Z0MQ
03594 *    END-IF.                                                      EQW9Z0MQ
03595 *                                                                 EQW9Z0MQ
03596 *    IF WSS-APPEL-AIDE-USAGE = 'N'                                EQW9Z0MQ
03597 *       IF (ECR-VEHUSACO NOT = '201' AND '202')                   EQW9Z0MQ
03598 *         AND (ECR-VEHGENCO NOT = 'G' AND 'J')                    EQW9Z0MQ
03599 *          IF COM-FB-TOP-AVEC-TAXE-AGRI = 'O'                     EQW9Z0MQ
03600 *             MOVE NOR-ALP    TO ECR-VEHUSACA                     EQW9Z0MQ
03601 *             IF KONTROL = 0                                      EQW9Z0MQ
03602 *                MOVE 'FB249' TO COM-GENE-MESANO                  EQW9Z0MQ
03603 *                                COM-CODERR                       EQW9Z0MQ
03604 *                MOVE CURSEUR TO ECR-VEHUSACL                     EQW9Z0MQ
03605 *                MOVE 1       TO KONTROL                          EQW9Z0MQ
03606 *                GO           TO FIN-CONTROLE-LOGIQUE             EQW9Z0MQ
03607 *             END-IF                                              EQW9Z0MQ
03608 *          END-IF                                                 EQW9Z0MQ
03609 *       END-IF                                                    EQW9Z0MQ
03610 *    END-IF.                                                      EQW9Z0MQ
03611                                                                   EQW9Z0MQ
F3576 *--- LA DATE D'ACQUISITION DU VEHICULE NE DOIT PAS ETRE ANTERIEUREEQW9Z0MQ
F3576 *--- A SA DATE DE PREMIERE MISE EN CIRCULATION                    EQW9Z0MQ
F3576                                                                   EQW9Z0MQ
F3576      IF (ECR-VEHCIRDO NOT = SPACE AND LOW-VALUE) AND              EQW9Z0MQ
F3576         (ECR-VEHACQDO NOT = SPACE AND LOW-VALUE)                  EQW9Z0MQ
F3576        MOVE ECR-VEHCIRDO (3:4) TO WSS-VEHCIRD (1:4)               EQW9Z0MQ
F3576        MOVE ECR-VEHCIRDO (1:2) TO WSS-VEHCIRD (5:2)               EQW9Z0MQ
F3576        MOVE '01'               TO WSS-VEHCIRD (7:2)               EQW9Z0MQ
F3576        MOVE ECR-VEHACQDO (3:4) TO WSS-VEHACQD (1:4)               EQW9Z0MQ
F3576        MOVE ECR-VEHACQDO (1:2) TO WSS-VEHACQD (5:2)               EQW9Z0MQ
F3576        MOVE '01'               TO WSS-VEHACQD (7:2)               EQW9Z0MQ
F3576        IF WSS-VEHACQD < WSS-VEHCIRD
F3576          MOVE NOR-ALP TO ECR-VEHACQDA                             EQW9Z0MQ
F3576          IF KONTROL = 0                                           EQW9Z0MQ
F3576             MOVE 'FB476' TO COM-GENE-MESANO                       EQW9Z0MQ
F3576                             COM-CODERR                            EQW9Z0MQ
F3576             MOVE CURSEUR TO ECR-VEHACQDL                          EQW9Z0MQ
F3576             MOVE 1       TO KONTROL                               EQW9Z0MQ
F3576             GO           TO FIN-CONTROLE-LOGIQUE                  EQW9Z0MQ
F3576          END-IF                                                   EQW9Z0MQ
F3576        END-IF
F3576      END-IF.
F3576                                                                   EQW9Z0MQ
03612 *--- SI LE MODE D'ACQUISITION LOA/LLD EST <> 'O', L'INDICATEUR    EQW9Z0MQ
03613 *--- GARANTIE PERTES FINANCIERES NE DOIT PAS ETRE RENSEIGNE       EQW9Z0MQ
03614      IF (ECR-VEHPOSCO = ' ' OR 'N')                               EQW9Z0MQ
03615         AND  (ECR-VEHPEFCO = 'O' OR 'N')                          EQW9Z0MQ
03616         MOVE NOR-ALP TO ECR-VEHPEFCA                              EQW9Z0MQ
03617         IF KONTROL = 0                                            EQW9Z0MQ
03618            MOVE 'FB111' TO COM-GENE-MESANO                        EQW9Z0MQ
03619                            COM-CODERR                             EQW9Z0MQ
03620            MOVE CURSEUR TO ECR-VEHPEFCL                           EQW9Z0MQ
03621            MOVE 1       TO KONTROL                                EQW9Z0MQ
03622            GO           TO FIN-CONTROLE-LOGIQUE                   EQW9Z0MQ
03623         END-IF                                                    EQW9Z0MQ
03624      END-IF.                                                      EQW9Z0MQ
03625                                                                   EQW9Z0MQ
03626 *--- L'INDICATEUR PERTES FINANCIERE EST OBLIGATOIREMENT ‡ 'N' S'ILEQW9Z0MQ
03627 *--- S'AGIT D'UN 2 ROUES                                          EQW9Z0MQ
03628      IF ECR-VEHTYPCO = '2R ' AND ECR-VEHPEFCO = 'O'               EQW9Z0MQ
03629         MOVE NOR-ALP TO ECR-VEHPEFCA                              EQW9Z0MQ
03630         IF KONTROL = 0                                            EQW9Z0MQ
03631            MOVE 'FB067' TO COM-GENE-MESANO                        EQW9Z0MQ
03632                            COM-CODERR                             EQW9Z0MQ
03633            MOVE CURSEUR TO ECR-VEHPEFCL                           EQW9Z0MQ
03634            MOVE 1       TO KONTROL                                EQW9Z0MQ
03635            GO           TO FIN-CONTROLE-LOGIQUE                   EQW9Z0MQ
03636         END-IF                                                    EQW9Z0MQ
03637      END-IF.                                                      EQW9Z0MQ
03638                                                                   EQW9Z0MQ
03639 *--- CODE POSTAL ET COMMUNE                                       EQW9Z0MQ
03640      IF WSS-APPEL-AIDE-CP = 'N' AND WSS-APPEL-AIDE-COM = 'N' AND  EQW9Z0MQ
03641         ECR-GARCOPCO NOT = SPACES AND ECR-GARVILLO NOT = SPACES   EQW9Z0MQ
03642         IF ECR-GARCOPCO NOT = '99000'                             EQW9Z0MQ
03643            PERFORM ACCES-GEBCOMM THRU FIN-ACCES-GEBCOMM           EQW9Z0MQ
03644         ELSE                                                      EQW9Z0MQ
03645            MOVE '99000'          TO  WSS-CODE-INSEE               EQW9Z0MQ
03646         END-IF                                                    EQW9Z0MQ
03647      END-IF.                                                      EQW9Z0MQ
03648                                                                   EQW9Z0MQ
03649 *--- DETECTION D'UN CHANGEMENT DE VEHICULE                        EQW9Z0MQ
03650 *---EN AVENANT, SI PAS AJOUT DE VEHICULE INTERDICTION DE MODIFIER EQW9Z0MQ
03651 *---EN AVENANT, SI PAS AJOUT DE VEHICULE INTERDICTION DE MODIFIER EQW9Z0MQ
03652 *---PLUS D'UNE INFORMATION PARMI :                                EQW9Z0MQ
03653 *    - LE CODE VEHICULE,                                          EQW9Z0MQ
03654 *    - L'IMMATRICULATION,                                         EQW9Z0MQ
03655 *    - LA DATE DE 1ERE MISE EN CIRCULATION                        EQW9Z0MQ
03656      IF (COM-FB-CODE-ACTION = 'M')                                EQW9Z0MQ
03657        AND (VEHACTC OF TS-VEHICULE(1) NOT = 'I')                  EQW9Z0MQ
03658         IF ECR-VEHCODCO NOT = SPACES AND                          EQW9Z0MQ
03659            ECR-VEHIMMXO NOT = SPACES AND                          EQW9Z0MQ
03660            ECR-VEHCIRDO NOT = SPACES                              EQW9Z0MQ
03661            PERFORM DETER-CHANGE-VEHI THRU FIN-DETER-CHANGE-VEHI   EQW9Z0MQ
03662            IF WSS-CHANGE-VEHICULE = 'O'                           EQW9Z0MQ
03663               IF KONTROL = 0                                      EQW9Z0MQ
03664                  MOVE NOR-ALP TO ECR-VEHCODCA                     EQW9Z0MQ
03665                  MOVE 'FB112' TO COM-GENE-MESANO                  EQW9Z0MQ
03666                                  COM-CODERR                       EQW9Z0MQ
03667                  MOVE CURSEUR TO ECR-VEHCODCL                     EQW9Z0MQ
03668                  MOVE 1       TO KONTROL                          EQW9Z0MQ
03669                  GO TO FIN-CONTROLE-LOGIQUE                       EQW9Z0MQ
03670               END-IF                                              EQW9Z0MQ
03671            END-IF                                                 EQW9Z0MQ
03672         END-IF                                                    EQW9Z0MQ
03673      END-IF.                                                      EQW9Z0MQ
03674                                                                   EQW9Z0MQ
03675 *                                                                 EQW9Z0MQ
03676 *-----------------------------------------------------------------EQW9Z0MQ
03677 *          ==> TRAITEMENT DE CONNEXES                             EQW9Z0MQ
03678 *-----------------------------------------------------------------EQW9Z0MQ
03679      IF ECR-ANVREPCO = 'N' OR SPACES                              EQW9Z0MQ
03680         MOVE SPACES              TO CONNEXES OF FBMISPTR-IT1      EQW9Z0MQ
03681         MOVE SPACES              TO RANVBONT OF TS-VEHICULE(2)    EQW9Z0MQ
03682         MOVE SPACES              TO RANVBOND OF TS-VEHICULE(2)    EQW9Z0MQ
03683         MOVE SPACES              TO RANVABOD OF TS-VEHICULE(2)    EQW9Z0MQ
F9674         IF ECR-VEHTYPCO = '2R '
F9674           IF WSS-CYLX NOT = SPACES AND LOW-VALUE
F9674             IF WSS-CYL NOT > 80
F9674                MOVE SPACES TO RANVBONT OF TS-VEHICULE(1)          EQW9Z0MQ
F9674                MOVE SPACES TO RANVBOND OF TS-VEHICULE(1)          EQW9Z0MQ
F9674                MOVE SPACES TO RANVABOD OF TS-VEHICULE(1)          EQW9Z0MQ
F9674                MOVE SPACES TO ECR-ANVBONTO
F9674                MOVE SPACES TO ECR-ANVBONDO
F9674                MOVE SPACES TO ECR-ANVABODO
F9674                PERFORM REMISE-BLANC-CRM THRU FIN-REMISE-BLANC-CRM
F9674             END-IF
F9674           END-IF
F9674         END-IF
03684      END-IF.                                                      EQW9Z0MQ
03685 * RECHERCHE DES CONNEXES QU'EN 'AN' ET 'RP' OU 'AV' + FORCAGE 'F' EQW9Z0MQ
33295 * OU 'AV' SANS FORCAGE 'F' ET 'RV', AVEC AJOUT DE VEHICULE        EQW9Z0MQ
03687      IF CONNEXES OF FBMISPTR-IT1 = SPACES OR LOW-VALUE            EQW9Z0MQ
03688         MOVE '2'                      TO CONNEXES OF FBMISPTR-IT1 EQW9Z0MQ
03689      END-IF.                                                      EQW9Z0MQ
03690      IF ECR-ANVREPCO = 'O'                                        EQW9Z0MQ
              IF  INF-NATMVT OF TS-SUSPENS1 = 'AN' OR 'RP'              EQW9Z0MQ
33295          OR (INF-NATMVT OF TS-SUSPENS1 = 'AV'   AND               EQW9Z0MQ
33295              (CCO-FORCAG1 OF TS-SUSPENS1 = 'F'
33295            OR CCO-FORCAG2 OF TS-SUSPENS1 = 'F'
33295            OR CCO-FORCAG3 OF TS-SUSPENS1 = 'F'
33295            OR CCO-FORCAG4 OF TS-SUSPENS1 = 'F'))
03692            PERFORM CALCUL-CRM-MOYEN   THRU FIN-CALCUL-CRM-MOYEN   EQW9Z0MQ
03693         ELSE                                                      EQW9Z0MQ
33295            IF INF-NATMVT OF TS-SUSPENS1 = 'RV'                    EQW9Z0MQ
03695               IF (COM-FB-CODE-ACTION = '1' OR '2' OR '3')         EQW9Z0MQ
03696                OR (VEHACTC OF TS-VEHICULE(1) = 'I'                EQW9Z0MQ
03696                   AND VEHCHGC OF TS-VEHICULE(1) NOT = '1' AND '2')EQW9Z0MQ
03697              PERFORM CALCUL-CRM-MOYEN   THRU FIN-CALCUL-CRM-MOYEN EQW9Z0MQ
03698 *               PERFORM RECHERCHE-CONNEXES                        EQW9Z0MQ
03699 *                  THRU FIN-RECHERCHE-CONNEXES                    EQW9Z0MQ
03700               END-IF                                              EQW9Z0MQ
03701            END-IF                                                 EQW9Z0MQ
33295            IF (INF-NATMVT OF TS-SUSPENS1 = 'AV'   AND             EQW9Z0MQ
33295                (CCO-FORCAG1 OF TS-SUSPENS1 NOT = 'F'
33295             AND CCO-FORCAG2 OF TS-SUSPENS1 NOT = 'F'
33295             AND CCO-FORCAG3 OF TS-SUSPENS1 NOT = 'F'
33295             AND CCO-FORCAG4 OF TS-SUSPENS1 NOT = 'F') AND
33295               ((COM-FB-CODE-ACTION = '1' OR '2' OR '3')           EQW9Z0MQ
33295             OR (VEHACTC OF TS-VEHICULE(1) = 'I' AND               EQW9Z0MQ
33295                 VEHCHGC OF TS-VEHICULE(1) NOT = '1' AND '2')))    EQW9Z0MQ
03697              PERFORM CALCUL-CRM-MOYEN   THRU FIN-CALCUL-CRM-MOYEN EQW9Z0MQ
03702            END-IF                                                 EQW9Z0MQ
03702         END-IF                                                    EQW9Z0MQ
03703      END-IF.                                                      EQW9Z0MQ
03704                                                                   EQW9Z0MQ
03705 *---------------------------------------------------------------* EQW9Z0MQ
03706 *------     CONTROLES DES ZONES RELATIVES AUX CONNEXES    ------* EQW9Z0MQ
03707 *---------------------------------------------------------------* EQW9Z0MQ
03708                                                                   EQW9Z0MQ
AD548      IF WSS-CTRL-ANTECEDENT = 'OUI'
AD548         PERFORM CTRL-LOG-ANTE THRU FIN-CTRL-LOG-ANTE
AD548      ELSE
AD548         IF INF-NATMVT OF TS-SUSPENS1 = 'AV' OR 'RV'               EQW9Z0MQ
AD548            IF COM-FB-PERS-AJOUT = 'OUI'                           EQW9Z0MQ
AD548            OR COM-FB-PERS-SUPPR = 'OUI'                           EQW9Z0MQ
AD548               PERFORM CTRL-LOG-ANTE2 THRU FIN-CTRL-LOG-ANTE2      EQW9Z0MQ
AD548            END-IF                                                 EQW9Z0MQ
AD548         END-IF                                                    EQW9Z0MQ
AD548      END-IF.
03709                                                                   EQW9Z0MQ
04012                                                                   EQW9Z0MQ
F2980 * EN AVENANT ET REMISE EN VIGUEUR S'IL EXISTE DEJA PLUSIEURS VEHICEQW9Z0MQ
F2980 * ULES EN USAGES 213 OU 313                                       EQW9Z0MQ
F2980      IF  INF-NATMVT OF TS-SUSPENS1 = 'AV' OR 'RV'                 EQW9Z0MQ
F2980       IF COM-FB-NBRE-VEHI-USA > ZERO
F2980          IF ((VEHACTC OF TS-VEHICULE(1) = 'I')  AND
F2980              (ECR-VEHUSACO = '209' OR '309' OR '210' OR '310'
F2980                           OR '213' OR '313')
F2980              AND (VEHCHGC OF TS-VEHICULE(1) NOT = '2'))
F8556          AND (COM-FB-CODE-ACTION = '1')                           EQW9Z0MQ
F2980               MOVE NOR-ALP    TO ECR-VEHUSACA                     EQW9Z0MQ
F2980               IF KONTROL = 0                                      EQW9Z0MQ
F2980                  MOVE 'FB471' TO COM-GENE-MESANO                  EQW9Z0MQ
F2980                                  COM-CODERR                       EQW9Z0MQ
F2980                  MOVE CURSEUR TO ECR-VEHUSACL                     EQW9Z0MQ
F2980                  MOVE 1       TO KONTROL                          EQW9Z0MQ
F2980                  GO           TO FIN-CONTROLE-LOGIQUE             EQW9Z0MQ
F2980               END-IF                                              EQW9Z0MQ
F2980          END-IF                                                   EQW9Z0MQ
F2980       ELSE                                                        EQW9Z0MQ
F2980        IF COM-FB-NBRE-VEHI-USA > 1
F2980        AND WSS-APPEL-AIDE-USAGE = 'N'
F2980          IF  ((VEHACTC OF TS-VEHICULE(1) = 'I') AND
F2980               (VEHCHGC OF TS-VEHICULE(1) = '2') AND
F2980               (ECR-VEHUSACO
F2980                               NOT = VEHUSAC OF TS-VEHICULE(1)))
F2980          OR  ((VEHACTC OF TS-VEHICULE(1) = 'U' OR 'V') AND
F2980               (ECR-VEHUSACO
F2980                               NOT = VEHUSAC OF TS-VEHICULE(1)))
F2980               MOVE NOR-ALP    TO ECR-VEHUSACA                     EQW9Z0MQ
F2980               IF KONTROL = 0                                      EQW9Z0MQ
F2980                  MOVE 'FB471' TO COM-GENE-MESANO                  EQW9Z0MQ
F2980                                  COM-CODERR                       EQW9Z0MQ
F2980                  MOVE CURSEUR TO ECR-VEHUSACL                     EQW9Z0MQ
F2980                  MOVE 1       TO KONTROL                          EQW9Z0MQ
F2980                  GO           TO FIN-CONTROLE-LOGIQUE             EQW9Z0MQ
F2980               END-IF                                              EQW9Z0MQ
F2980          END-IF                                                   EQW9Z0MQ
F2980        END-IF                                                     EQW9Z0MQ
F2980       END-IF                                                      EQW9Z0MQ
F2980      END-IF.                                                      EQW9Z0MQ
F2980      MOVE ZERO  TO WSS-FB-NBRE-VEHI-USA
F2980      MOVE VEHUSAC OF TS-VEHICULE(1)  TO WSS-USAGE-PROF (1:1)
F2980      MOVE VEHPROC OF TS-VEHICULE(1)  TO WSS-USAGE-PROF (2:2)
F2980      IF  INF-NATMVT OF TS-SUSPENS1 = 'AN' OR 'RP' OR 'AV' OR 'RV' EQW9Z0MQ
F2980          IF (ECR-VEHUSACO = '209' OR '309' OR '210'
F2980              OR '310' OR '213' OR '313')
F2980          AND NOT ((VEHUSAC OF TS-VEHICULE(1) = '2' OR '3')
F2980          AND (VEHPROC OF TS-VEHICULE(1) = '09' OR '10' OR '13'))
F2980              ADD 1 TO WSS-FB-NBRE-VEHI-USA
F2980          END-IF                                                   EQW9Z0MQ
F2980          IF (ECR-VEHUSACO NOT = '209' AND '309' AND '210'
F2980              AND '310' AND '213' AND '313')
F2980          AND (VEHUSAC OF TS-VEHICULE(1) = '2' OR '3')
F2980          AND (VEHPROC OF TS-VEHICULE(1) = '09' OR '10' OR '13')
F2980              SUBTRACT 1 FROM WSS-FB-NBRE-VEHI-USA
F2980          END-IF
F2980          IF ECR-VEHUSACO NOT = WSS-USAGE-PROF
F2980             ADD COM-FB-NBRE-VEHI-USA TO WSS-FB-NBRE-VEHI-USA
F2980          END-IF
F2980          IF WSS-FB-NBRE-VEHI-USA > 1
F2980          AND WSS-APPEL-AIDE-USAGE = 'N'
F2980               MOVE NOR-ALP    TO ECR-VEHUSACA                     EQW9Z0MQ
F2980               IF KONTROL = 0                                      EQW9Z0MQ
F2980                  MOVE 'FB471' TO COM-GENE-MESANO                  EQW9Z0MQ
F2980                                  COM-CODERR                       EQW9Z0MQ
F2980                  MOVE CURSEUR TO ECR-VEHUSACL                     EQW9Z0MQ
F2980                  MOVE 1       TO KONTROL                          EQW9Z0MQ
F2980                  GO           TO FIN-CONTROLE-LOGIQUE             EQW9Z0MQ
F2980               END-IF                                              EQW9Z0MQ
F2980          END-IF                                                   EQW9Z0MQ
F2980      END-IF.                                                      EQW9Z0MQ
F2980
F2980 *---CONTROLE SUR LE VDR "E" EN CAS D'AVENANT OU DE REMISE
F2980 *---           POUR UN CHANGEMENT DE VEHICULE SEUL
F2980 *---           POUR UN CHANGEMENT DE VEHICULE SEUL + GIES
F2980 *---           POUR UN AJOUT DE VEHICULE + GIES
F2980      IF  (INF-NATMVT OF TS-SUSPENS1 = 'AV' OR 'RV')
F2980      AND ((ECR-VEHUSACO NOT = '209' AND '309' AND '210' AND '310'
F2980                          AND '213' AND '313')
F2980        OR (COM-FB-VDR NOT = 'O'))
F2980        IF  ((VEHACTC OF TS-VEHICULE(1) = 'I') AND
F2980             (VEHCHGC OF TS-VEHICULE(1) = '2'))
F2980          IF  (ECR-VEHUSACO NOT = '209' AND '309' AND '210' AND
F2980                  '310' AND '213' AND '313')
F2980          AND (COM-FB-VDR = 'O')
F2980               MOVE NOR-ALP    TO ECR-VEHUSACA                     EQW9Z0MQ
F2980               IF KONTROL = 0                                      EQW9Z0MQ
F2980                  MOVE 'FB482' TO COM-GENE-MESANO                  EQW9Z0MQ
F2980                                  COM-CODERR                       EQW9Z0MQ
F2980                  MOVE CURSEUR TO ECR-VEHUSACL                     EQW9Z0MQ
F2980                  MOVE 1       TO KONTROL                          EQW9Z0MQ
F2980                  GO           TO FIN-CONTROLE-LOGIQUE             EQW9Z0MQ
F2980               END-IF                                              EQW9Z0MQ
F2980          ELSE
F2980            CONTINUE
F2980          END-IF
F2980        END-IF
F2980      END-IF.                                                      EQW9Z0MQ
F2980
04013  FIN-CONTROLE-LOGIQUE.                                            EQW9Z0MQ
04014      EXIT.                                                        EQW9Z0MQ
04015 *                                                                 EQW9Z0MQ
AD548  CTRL-LOG-ANTE.                                                   EQW9Z0MQ
      *
03750 *---LE NUMERO DE CONTRAT PRECEDENT NE PEUT ÍTRE SAISIE QUE SI LE  EQW9Z0MQ
03751 *---CODE CIE EST RENSEIGNE                                        EQW9Z0MQ
03752      IF WSS-APPEL-AIDE-CIE = 'N'                                  EQW9Z0MQ
03753         IF ECR-ANVNUMXO NOT = SPACES AND LOW-VALUE                EQW9Z0MQ
03754            IF ECR-ANVCIEXO = SPACES OR LOW-VALUE                  EQW9Z0MQ
03755               MOVE NOR-ALP TO ECR-ANVNUMXA                        EQW9Z0MQ
03756               IF KONTROL = 0                                      EQW9Z0MQ
03757                  MOVE 'FB339' TO COM-GENE-MESANO                  EQW9Z0MQ
03758                                  COM-CODERR                       EQW9Z0MQ
03759                  MOVE CURSEUR TO ECR-ANVNUMXL                     EQW9Z0MQ
03760                  MOVE 1       TO KONTROL                          EQW9Z0MQ
AD548                  GO           TO FIN-CTRL-LOG-ANTE                EQW9Z0MQ
03762               END-IF                                              EQW9Z0MQ
03763            END-IF                                                 EQW9Z0MQ
03764         END-IF                                                    EQW9Z0MQ
03765      END-IF.                                                      EQW9Z0MQ
03709                                                                   EQW9Z0MQ
03710 *---LE CODE COMPAGNIE NE DOIT PAS ÍTRE RENSEIGNE SI RECHERCHE DE  EQW9Z0MQ
03711 *---CONNEXES OK                                                   EQW9Z0MQ
03712      IF WSS-APPEL-AIDE-CIE = 'N'                                  EQW9Z0MQ
03713         IF CONNEXES OF FBMISPTR-IT1 = '0'                         EQW9Z0MQ
03714            IF ECR-ANVCIEXO NOT = SPACES AND LOW-VALUE             EQW9Z0MQ
03715               MOVE NOR-ALP TO ECR-ANVCIEXA                        EQW9Z0MQ
03716               IF KONTROL = 0                                      EQW9Z0MQ
03717                  MOVE 'FB334' TO COM-GENE-MESANO                  EQW9Z0MQ
03718                                  COM-CODERR                       EQW9Z0MQ
03719                  MOVE CURSEUR TO ECR-ANVCIEXL                     EQW9Z0MQ
03720                  MOVE 1       TO KONTROL                          EQW9Z0MQ
AD548                  GO           TO FIN-CTRL-LOG-ANTE                EQW9Z0MQ
03722               END-IF                                              EQW9Z0MQ
03723            END-IF                                                 EQW9Z0MQ
03724         END-IF                                                    EQW9Z0MQ
03725      END-IF.                                                      EQW9Z0MQ
03726                                                                   EQW9Z0MQ
03727 *---LE CODE COMPAGNIE DOIT ÍTRE DANS LA TABLE GECGTA99            EQW9Z0MQ
03728      IF WSS-APPEL-AIDE-CIE = 'N'                                  EQW9Z0MQ
03729         IF ((ECR-ANVCIEXO NOT = SPACES AND '999')                 EQW9Z0MQ
F9674                              AND NOT
F9674            ((INF-NATMVT OF TS-SUSPENS1 = 'AV' OR 'RV')
F9674              AND VEHACTC OF TS-VEHICULE(1) NOT = 'I')
F9674                              AND NOT
F9674            ((INF-NATMVT OF TS-SUSPENS1 = 'AV' OR 'RV')
F9674              AND VEHACTC OF TS-VEHICULE(1) = 'I'
F9674              AND (VEHCHGC OF TS-VEHICULE(1) = '1' OR '2')))
03729         OR ((ECR-ANVCIEXO NOT = SPACES AND '999')                 EQW9Z0MQ
                   AND WS-FORCAGE = 'O')

03730            MOVE 'N'        TO WSS-MODIF-CODE-CIE                  EQW9Z0MQ
03731            IF ECR-ANVCIEXO NOT = ANVCIEX OF TS-VEHICULE(1)        EQW9Z0MQ
03732               MOVE 'O'     TO WSS-MODIF-CODE-CIE                  EQW9Z0MQ
03733            END-IF                                                 EQW9Z0MQ
03734            PERFORM ACCES-GECGTA THRU FIN-ACCES-GECGTA             EQW9Z0MQ
03735            IF WSS-CDCIE-TROUVE = 'N'                              EQW9Z0MQ
03736               MOVE NOR-ALP TO ECR-ANVCIEXA                        EQW9Z0MQ
03737               IF KONTROL = 0                                      EQW9Z0MQ
03738                  MOVE 'FB150' TO COM-GENE-MESANO                  EQW9Z0MQ
03739                                  COM-CODERR                       EQW9Z0MQ
03740                  MOVE CURSEUR TO ECR-ANVCIEXL                     EQW9Z0MQ
03741                  MOVE 1       TO KONTROL                          EQW9Z0MQ
AD548                  GO           TO FIN-CTRL-LOG-ANTE                EQW9Z0MQ
03743               END-IF                                              EQW9Z0MQ
03744            ELSE                                                   EQW9Z0MQ
03745               MOVE GECGTA-GTANOML TO ANVCIEL OF TS-VEHICULE(2)    EQW9Z0MQ
03746            END-IF                                                 EQW9Z0MQ
03747         END-IF                                                    EQW9Z0MQ
03748      END-IF.                                                      EQW9Z0MQ
03749                                                                   EQW9Z0MQ
03767 *---LE LIBELLE COMPAGNIE DOIT ÍTRE A BLANC SI MODIF DU CODE CIE   EQW9Z0MQ
03768      IF WSS-APPEL-AIDE-CIE = 'N' AND WSS-MODIF-CODE-CIE NOT = 'O' EQW9Z0MQ
03769         IF ECR-ANVCIEXO NOT = SPACES AND LOW-VALUE AND '999'      EQW9Z0MQ
03770            IF ANVCIEX OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUEEQW9Z0MQ
03771               IF ECR-ANVCIEXO NOT = ANVCIEX OF TS-VEHICULE(1)     EQW9Z0MQ
03772                  IF ECR-ANVCIELO NOT = SPACES AND LOW-VALUE       EQW9Z0MQ
03773                     MOVE NOR-ALP TO ECR-ANVCIELA                  EQW9Z0MQ
03774                     IF KONTROL = 0                                EQW9Z0MQ
03775                        MOVE 'FB255' TO COM-GENE-MESANO            EQW9Z0MQ
03776                                        COM-CODERR                 EQW9Z0MQ
03777                        MOVE CURSEUR TO ECR-ANVCIELL               EQW9Z0MQ
03778                        MOVE 1       TO KONTROL                    EQW9Z0MQ
AD548                        GO           TO FIN-CTRL-LOG-ANTE          EQW9Z0MQ
03780                     END-IF                                        EQW9Z0MQ
03781                  END-IF                                           EQW9Z0MQ
03782               ELSE                                                EQW9Z0MQ
03783                  IF ECR-ANVCIELO NOT = SPACES AND LOW-VALUE       EQW9Z0MQ
03784                     IF ECR-ANVCIELO(1:30) NOT =                   EQW9Z0MQ
F9674                           ANVCIEL OF TS-VEHICULE(2) AND           EQW9Z0MQ
F9674                           ANVCIEL OF TS-VEHICULE(1)               EQW9Z0MQ
03786                        MOVE NOR-ALP TO ECR-ANVCIELA               EQW9Z0MQ
03787                        IF KONTROL = 0                             EQW9Z0MQ
03788                           MOVE 'FB255' TO COM-GENE-MESANO         EQW9Z0MQ
03789                                           COM-CODERR              EQW9Z0MQ
03790                           MOVE CURSEUR TO ECR-ANVCIELL            EQW9Z0MQ
03791                           MOVE 1       TO KONTROL                 EQW9Z0MQ
AD548                           GO           TO FIN-CTRL-LOG-ANTE       EQW9Z0MQ
03793                        END-IF                                     EQW9Z0MQ
03794                     END-IF                                        EQW9Z0MQ
03795                  END-IF                                           EQW9Z0MQ
03796               END-IF                                              EQW9Z0MQ
03797            END-IF                                                 EQW9Z0MQ
03798         END-IF                                                    EQW9Z0MQ
03799      END-IF.                                                      EQW9Z0MQ
03800                                                                   EQW9Z0MQ
03801 *---LIBELLE COMPAGNIE PRECEDENTE SANS CODE CIE                    EQW9Z0MQ
03802      IF WSS-APPEL-AIDE-CIE = 'N'                                  EQW9Z0MQ
03803         IF ECR-ANVCIELO NOT = SPACES                              EQW9Z0MQ
03804            IF ECR-ANVCIEXO = SPACES                               EQW9Z0MQ
03805               MOVE NOR-ALP TO ECR-ANVCIELA                        EQW9Z0MQ
03806               IF KONTROL = 0                                      EQW9Z0MQ
03807                  MOVE 'FB155' TO COM-GENE-MESANO                  EQW9Z0MQ
03808                                  COM-CODERR                       EQW9Z0MQ
03809                  MOVE CURSEUR TO ECR-ANVCIELL                     EQW9Z0MQ
03810                  MOVE 1   TO KONTROL                              EQW9Z0MQ
AD548                  GO       TO FIN-CTRL-LOG-ANTE                    EQW9Z0MQ
03812               END-IF                                              EQW9Z0MQ
03813            END-IF                                                 EQW9Z0MQ
03814         END-IF                                                    EQW9Z0MQ
03815      END-IF.                                                      EQW9Z0MQ
03816                                                                   EQW9Z0MQ
AD548      PERFORM CTRL-LOG-ANTE2 THRU FIN-CTRL-LOG-ANTE2
AD548 *
03851                                                                   EQW9Z0MQ
03852 *---SAISIE DATE DE RESILIATION IMPOSSIBLE SANS CODE CIE           EQW9Z0MQ
03853      IF WSS-APPEL-AIDE-CIE = 'N'                                  EQW9Z0MQ
03854         IF ECR-ANVRESDO NOT = SPACES AND LOW-VALUE                EQW9Z0MQ
03855            IF ECR-ANVCIEXO = SPACES OR LOW-VALUE                  EQW9Z0MQ
03856               MOVE NOR-ALP TO ECR-ANVRESDA                        EQW9Z0MQ
03857               IF KONTROL = 0                                      EQW9Z0MQ
03858                  MOVE 'FB341' TO COM-GENE-MESANO                  EQW9Z0MQ
03859                                  COM-CODERR                       EQW9Z0MQ
03860                  MOVE CURSEUR TO ECR-ANVRESDL                     EQW9Z0MQ
03861                  MOVE 1       TO KONTROL                          EQW9Z0MQ
AD548                  GO           TO FIN-CTRL-LOG-ANTE                EQW9Z0MQ
03863               END-IF                                              EQW9Z0MQ
03864            END-IF                                                 EQW9Z0MQ
03865         END-IF                                                    EQW9Z0MQ
03866      END-IF.                                                      EQW9Z0MQ
03867                                                                   EQW9Z0MQ
03868 *---L'INDICATEUR DE PRESENCE DE SINISTRE NE PEUT ÍTRE SAISIE QUE  EQW9Z0MQ
03869 *---SI LE CODE CIE EST RENSEIGNE                                  EQW9Z0MQ
      *---SAUF SI DEJA PRESENT
03870      IF WSS-APPEL-AIDE-CIE = 'N'                                  EQW9Z0MQ
03871         IF ECR-SIVINDCO = 'O'                                     EQW9Z0MQ
03872            IF (ECR-ANVCIEXO = SPACES OR LOW-VALUE)                EQW9Z0MQ
03872            AND (SIVINDC OF TS-VEHICULE(1) NOT = ECR-SIVINDCO)     EQW9Z0MQ
03873               MOVE NOR-ALP TO ECR-SIVINDCA                        EQW9Z0MQ
03874               IF KONTROL = 0                                      EQW9Z0MQ
03875                  MOVE 'FB342' TO COM-GENE-MESANO                  EQW9Z0MQ
03876                                  COM-CODERR                       EQW9Z0MQ
03877                  MOVE CURSEUR TO ECR-SIVINDCL                     EQW9Z0MQ
03878                  MOVE 1       TO KONTROL                          EQW9Z0MQ
AD548                  GO           TO FIN-CTRL-LOG-ANTE                EQW9Z0MQ
03880               END-IF                                              EQW9Z0MQ
03881            END-IF                                                 EQW9Z0MQ
03882         END-IF                                                    EQW9Z0MQ
03883      END-IF.                                                      EQW9Z0MQ
03884                                                                   EQW9Z0MQ
03884 *---SAISIE MOTIF DE RESIL IMPOSSIBLE SANS DATE DE RESIL           EQW9Z0MQ
              IF ECR-ANVMTRCO NOT = SPACES AND LOW-VALUE
                 IF ECR-ANVRESDO = SPACES OR LOW-VALUE
                    MOVE NOR-ALP TO ECR-ANVMTRCA
                    IF KONTROL = 0
                       MOVE 'FB436' TO COM-GENE-MESANO
                                       COM-CODERR
                       MOVE CURSEUR TO ECR-ANVMTRCL
                       MOVE 1       TO KONTROL
AD548                  GO           TO FIN-CTRL-LOG-ANTE
                    END-IF
                 END-IF
              END-IF.

03885 *---DERNIER CRM NON MODIFIABLE SI RECHERCHE DE CONNEXES OK        EQW9Z0MQ
03886      IF CONNEXES OF FBMISPTR-IT1 = '0'                            EQW9Z0MQ
03887        IF ECR-ANVBONTO NOT = SPACES AND LOW-VALUE                 EQW9Z0MQ
03888          IF RANVBONT OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE EQW9Z0MQ
U3921             IF ECR-ANVBONTO NOT = WSS-CRM-MOYENZ(3:3)             00489400
03890                MOVE NOR-ALP TO ECR-ANVBONTA                       EQW9Z0MQ
03891                IF KONTROL = 0                                     EQW9Z0MQ
03892                   MOVE 'FB344' TO COM-GENE-MESANO                 EQW9Z0MQ
03893                                   COM-CODERR                      EQW9Z0MQ
03894                   MOVE CURSEUR TO ECR-ANVBONTL                    EQW9Z0MQ
03895                   MOVE 1       TO KONTROL                         EQW9Z0MQ
AD548                   GO           TO FIN-CTRL-LOG-ANTE               EQW9Z0MQ
03897                END-IF                                             EQW9Z0MQ
03898             END-IF                                                EQW9Z0MQ
03899          END-IF                                                   EQW9Z0MQ
03900        END-IF                                                     EQW9Z0MQ
03901      END-IF.                                                      EQW9Z0MQ
03902                                                                   EQW9Z0MQ
03903 *---DERNIER CRM MODIFIABLE SI RECHECHE DE CONNEXES NON OK ET      EQW9Z0MQ
03904 *---SAISIR CODE CIE PRECEDENT CONTRAT                             EQW9Z0MQ
03905      IF CONNEXES OF FBMISPTR-IT1 = '1'                            EQW9Z0MQ
03906         IF ECR-ANVBONTO NOT = SPACES AND LOW-VALUE                EQW9Z0MQ
03907            IF ECR-ANVBONTO NOT = '100'                            EQW9Z0MQ
03908               IF WSS-APPEL-AIDE-CIE = 'N'                         EQW9Z0MQ
03909                  IF ECR-ANVCIEXO = SPACES OR LOW-VALUE            EQW9Z0MQ
03910                     MOVE NOR-ALP    TO ECR-ANVBONTA               EQW9Z0MQ
03911                     IF KONTROL = 0                                EQW9Z0MQ
03912                        MOVE 'FB343' TO COM-GENE-MESANO            EQW9Z0MQ
03913                                        COM-CODERR                 EQW9Z0MQ
03914                        MOVE CURSEUR TO ECR-ANVBONTL               EQW9Z0MQ
03915                        MOVE 1       TO KONTROL                    EQW9Z0MQ
AD548                        GO           TO FIN-CTRL-LOG-ANTE          EQW9Z0MQ
03917                     END-IF                                        EQW9Z0MQ
03918                  END-IF                                           EQW9Z0MQ
03919               END-IF                                              EQW9Z0MQ
03920            END-IF                                                 EQW9Z0MQ
03921         END-IF                                                    EQW9Z0MQ
03922      END-IF.                                                      EQW9Z0MQ
03923                                                                   EQW9Z0MQ
03924 *---DATE D'ACQUISITION DU COEFFICIENT DE REDUCTION-MAJORATION     EQW9Z0MQ
03925 *---INTERDITE SANS PRESENCE DU DERNIER CRM                        EQW9Z0MQ
03926      IF ECR-ANVBONDO NOT = SPACES                                 EQW9Z0MQ
03927         IF ECR-ANVBONTO = SPACES                                  EQW9Z0MQ
03928            MOVE NOR-ALP    TO ECR-ANVBONDA                        EQW9Z0MQ
03929            IF KONTROL = 0                                         EQW9Z0MQ
03930               MOVE 'FB157' TO COM-GENE-MESANO                     EQW9Z0MQ
03931                               COM-CODERR                          EQW9Z0MQ
03932               MOVE CURSEUR TO ECR-ANVBONDL                        EQW9Z0MQ
03933               MOVE 1       TO KONTROL                             EQW9Z0MQ
AD548               GO           TO FIN-CTRL-LOG-ANTE                   EQW9Z0MQ
03935            END-IF                                                 EQW9Z0MQ
03936         END-IF                                                    EQW9Z0MQ
03937         IF ECR-ANVRESDO NOT = SPACES                              EQW9Z0MQ
03938            MOVE ECR-ANVRESDO(3:4) TO WSS-RESIL-SSAA               EQW9Z0MQ
03939            MOVE ECR-ANVRESDO(1:2) TO WSS-RESIL-MM                 EQW9Z0MQ
03940            MOVE ECR-ANVBONDO(3:4) TO WSS-ACQUI-SSAA               EQW9Z0MQ
03941            MOVE ECR-ANVBONDO(1:2) TO WSS-ACQUI-MM                 EQW9Z0MQ
03942            IF WSS-ACQUI > WSS-RESIL                               EQW9Z0MQ
03943               MOVE NOR-ALP    TO ECR-ANVBONDA                     EQW9Z0MQ
03944               IF KONTROL = 0                                      EQW9Z0MQ
03945                  MOVE 'FB158' TO COM-GENE-MESANO                  EQW9Z0MQ
03946                                  COM-CODERR                       EQW9Z0MQ
03947                  MOVE CURSEUR TO ECR-ANVBONDL                     EQW9Z0MQ
03948                  MOVE 1       TO KONTROL                          EQW9Z0MQ
AD548                  GO           TO FIN-CTRL-LOG-ANTE                EQW9Z0MQ
03950               END-IF                                              EQW9Z0MQ
03951            END-IF                                                 EQW9Z0MQ
03952         END-IF                                                    EQW9Z0MQ
03953      END-IF.                                                      EQW9Z0MQ
03954                                                                   EQW9Z0MQ
03955 *---DATE D'ACQUISITION DU COEFFICIENT DE REDUCTION-MAJORATION     EQW9Z0MQ
03956 *---NON MODIFIABLE SI RECHERCHE DE CONNEXES OK                    EQW9Z0MQ
03957      IF CONNEXES OF FBMISPTR-IT1 = '0'                            EQW9Z0MQ
03958        IF  (ECR-ANVBONTO NOT = SPACES AND LOW-VALUE)              EQW9Z0MQ
03959        AND (ECR-ANVBONDO NOT = SPACES AND LOW-VALUE)              EQW9Z0MQ
03960             MOVE RANVBOND OF TS-VEHICULE(2) (1:4)                 EQW9Z0MQ
03961                           TO WSS-ACQUI2-SSAA                      EQW9Z0MQ
03962             MOVE RANVBOND OF TS-VEHICULE(2) (5:2)                 EQW9Z0MQ
03963                           TO WSS-ACQUI2-MM                        EQW9Z0MQ
03964             IF ECR-ANVBONDO NOT = WSS-ACQUI2                      EQW9Z0MQ
03965                MOVE NOR-ALP TO ECR-ANVBONDA                       EQW9Z0MQ
03966                IF KONTROL = 0                                     EQW9Z0MQ
03967                   MOVE 'FB431' TO COM-GENE-MESANO                 EQW9Z0MQ
03968                                   COM-CODERR                      EQW9Z0MQ
03969                   MOVE CURSEUR TO ECR-ANVBONDL                    EQW9Z0MQ
03970                   MOVE 1       TO KONTROL                         EQW9Z0MQ
AD548                   GO           TO FIN-CTRL-LOG-ANTE               EQW9Z0MQ
03972                END-IF                                             EQW9Z0MQ
03973             END-IF                                                EQW9Z0MQ
03974        END-IF                                                     EQW9Z0MQ
03975      END-IF.                                                      EQW9Z0MQ
03976                                                                   EQW9Z0MQ
03977                                                                   EQW9Z0MQ
03978 *---DATE D'ACQUISITION DU CRM = 050 SAISIE MAIS ANCIEN CRM<>050   EQW9Z0MQ
03979      IF ECR-ANVABODO NOT = SPACES                                 EQW9Z0MQ
03980         IF (ECR-ANVBONTO NOT = SPACES AND LOW-VALUE)              EQW9Z0MQ
03981          AND ECR-ANVBONTO NOT = '050' AND ' 50'                   EQW9Z0MQ
03982            MOVE NOR-ALP TO ECR-ANVABODA                           EQW9Z0MQ
03983            IF KONTROL = 0                                         EQW9Z0MQ
03984               MOVE 'FB159' TO COM-GENE-MESANO                     EQW9Z0MQ
03985                               COM-CODERR                          EQW9Z0MQ
03986               MOVE CURSEUR TO ECR-ANVABODL                        EQW9Z0MQ
03987               MOVE 1       TO KONTROL                             EQW9Z0MQ
AD548               GO           TO FIN-CTRL-LOG-ANTE                   EQW9Z0MQ
03989            END-IF                                                 EQW9Z0MQ
03990         END-IF                                                    EQW9Z0MQ
03991      END-IF.                                                      EQW9Z0MQ
03992                                                                   EQW9Z0MQ
03993 *---DATE D'ACQUISITION DU CRM A 050 NON MODIFIABLE SI RECHERCHE   EQW9Z0MQ
03994 *---DE CONNEXES OK                                                EQW9Z0MQ
03995      IF CONNEXES OF FBMISPTR-IT1 = '0'                            EQW9Z0MQ
03996        IF  (ECR-ANVBONTO  = '050' OR ' 50')                       EQW9Z0MQ
03997        AND (ECR-ANVABODO NOT = SPACES AND LOW-VALUE)              EQW9Z0MQ
03998             MOVE RANVABOD OF TS-VEHICULE(2) (1:4)                 EQW9Z0MQ
03999                           TO WSS-ACQUI2-SSAA                      EQW9Z0MQ
04000             IF ECR-ANVABODO NOT = WSS-ACQUI2-SSAA                 EQW9Z0MQ
04001                MOVE NOR-ALP TO ECR-ANVABODA                       EQW9Z0MQ
04002                IF KONTROL = 0                                     EQW9Z0MQ
04003                   MOVE 'FB432' TO COM-GENE-MESANO                 EQW9Z0MQ
04004                                   COM-CODERR                      EQW9Z0MQ
04005                   MOVE CURSEUR TO ECR-ANVABODL                    EQW9Z0MQ
04006                   MOVE 1       TO KONTROL                         EQW9Z0MQ
AD548                   GO           TO FIN-CTRL-LOG-ANTE               EQW9Z0MQ
04008                END-IF                                             EQW9Z0MQ
04009             END-IF                                                EQW9Z0MQ
04010        END-IF                                                     EQW9Z0MQ
04011      END-IF.                                                      EQW9Z0MQ
04012                                                                   EQW9Z0MQ
           IF  (INF-NATMVT OF TS-SUSPENS1 = 'AN' OR 'RP')               EQW9Z0MQ
33295       OR (INF-NATMVT OF TS-SUSPENS1 = 'AV'   AND                  EQW9Z0MQ
33295           (CCO-FORCAG1 OF TS-SUSPENS1 = 'F'
33295         OR CCO-FORCAG2 OF TS-SUSPENS1 = 'F'
33295         OR CCO-FORCAG3 OF TS-SUSPENS1 = 'F'
33295         OR CCO-FORCAG4 OF TS-SUSPENS1 = 'F'))
33295       OR ((INF-NATMVT OF TS-SUSPENS1 = 'RV')                      EQW9Z0MQ
                 AND ((COM-FB-CODE-ACTION = '1' OR '2' OR '3')          EQW9Z0MQ
                 OR (VEHACTC OF TS-VEHICULE(1) = 'I'                    EQW9Z0MQ
                  AND VEHCHGC OF TS-VEHICULE(1) NOT = '1' AND '2')))    EQW9Z0MQ
33295       OR (INF-NATMVT OF TS-SUSPENS1 = 'AV'   AND                  EQW9Z0MQ
33295           (CCO-FORCAG1 OF TS-SUSPENS1 NOT = 'F'
33295        AND CCO-FORCAG2 OF TS-SUSPENS1 NOT = 'F'
33295        AND CCO-FORCAG3 OF TS-SUSPENS1 NOT = 'F'
33295        AND CCO-FORCAG4 OF TS-SUSPENS1 NOT = 'F') AND
33295           ((COM-FB-CODE-ACTION = '1' OR '2' OR '3')               EQW9Z0MQ
              OR (VEHACTC OF TS-VEHICULE(1) = 'I' AND                   EQW9Z0MQ
                  VEHCHGC OF TS-VEHICULE(1) NOT = '1' AND '2')))        EQW9Z0MQ
03978 *---ANCIEN SOUSCRIPTEUR DU PRECEDENT CONTRAT OBLIGATOIRE SI       EQW9Z0MQ
03978 *---AU MOINS L'UNE DES ZONES CONNEXES EST RENSEIGNÈE EXCEPTÈ      EQW9Z0MQ
03978 *---LES INFORMATIONS DE LA LIGNE 'DERNIER CRM'.                   EQW9Z0MQ
               IF ECR-ANVSOUSO = SPACES OR LOW-VALUE                        EQW9
                  IF (ECR-ANVNUMXO NOT = SPACES AND LOW-VALUE)              EQW9
                  OR (ECR-ANVCIEXO NOT = SPACES AND LOW-VALUE)              EQW9
                  OR (ECR-ANVCIELO NOT = SPACES AND LOW-VALUE)              EQW9
                  OR (ECR-ANVANCNO NOT = SPACES AND LOW-VALUE)              EQW9
                  OR (ECR-ANVINTNO NOT = SPACES AND LOW-VALUE)              EQW9
                  OR (ECR-ANVRESDO NOT = SPACES AND LOW-VALUE)              EQW9
                  OR ((ECR-SIVINDCO NOT = SPACES AND LOW-VALUE)             EQW9
                      AND (ECR-ANVREPCO = 'N'))
                  OR (ECR-ANVMTRCO NOT = SPACES AND LOW-VALUE)              EQW9
                     MOVE SPACES     TO ECR-ANVSOUNO                        EQW9
                     MOVE NOR-ALP TO ECR-ANVSOUSA                           EQW9
                     IF KONTROL = 0                                         EQW9
                        MOVE 'FBTD4' TO COM-GENE-MESANO                     EQW9
                                        COM-CODERR                          EQW9
                        MOVE CURSEUR TO ECR-ANVSOUSL                        EQW9
                        MOVE 1       TO KONTROL                             EQW9
AD548                   GO           TO FIN-CTRL-LOG-ANTE                   EQW9
                     END-IF                                                 EQW9
                  END-IF                                                    EQW9
               END-IF                                                       EQW9
04012                                                                   EQW9Z0MQ
03978 *---STATUT ANCIEN SOUSCRIPTEUR DU PRECEDENT CONTRAT DOIT EXISTER  EQW9Z0MQ
03978 *---DANS LE PRESENT CONTRAT                                       EQW9Z0MQ
               IF ECR-ANVSOUSO NOT = SPACES AND LOW-VALUE                   EQW9
                  SET PERSONNE-NON-TROUVE TO TRUE
                  PERFORM READ-TS-TECHNIQUE THRU FIN-READ-TS-TECHNIQUE
                  PERFORM VARYING I-RANG FROM 1 BY 1
                          UNTIL I-RANG > 40
                          OR TAB-PERS(I-RANG) = (SPACES OR LOW-VALUE)
                          OR PERSONNE-TROUVE
                     MOVE PERRANTS OF TS-TECHNIQUE(I-RANG) TO
                                                     COM-FB-RANG-TSPERS
                     PERFORM READ-TS-PERSONNE THRU FIN-READ-TS-PERSONNE
                     PERFORM VERIF-PERSONNE   THRU FIN-VERIF-PERSONNE
                  END-PERFORM
                  IF PERSONNE-NON-TROUVE
                     MOVE NOR-ALP TO ECR-ANVSOUSA                           EQW9
                     IF KONTROL = 0                                         EQW9
                       MOVE 'FBTD6' TO COM-GENE-MESANO                      EQW9
                                       COM-CODERR                           EQW9
                       MOVE CURSEUR TO ECR-ANVSOUSL                         EQW9
                       MOVE 1       TO KONTROL                              EQW9
AD548                  GO           TO FIN-CTRL-LOG-ANTE                    EQW9
                     END-IF                                                 EQW9
                  END-IF
               END-IF                                                       EQW9
04012                                                                   EQW9Z0MQ
01673 *--- CODE CIE A '999'                                             EQW92BG2
03916 * --> MESSAGE INDIQUANT QUE LE PASSAGE PAR L'ECRAN D'AIDE         EQW90UNO
03916 * --> COMPAGNIE GTA EST OBLIGATOIRE SI LE LIBELLE COMPAGNIE       EQW90UNO
03916 * --> PRECEDENT EST A BLANC.                                      EQW90UNO
               IF  ((ECR-ANVCIEXO = '999')                              EQW92BG2
               AND (ECR-ANVCIELO = SPACES OR LOW-VALUE))                EQW92BG2
               OR  (ECR-ANVCIEXO = '999' AND MODIF-CIE-OUI)                  EQW
                    MOVE 'FBTD7' TO COM-GENE-MESINF                          EQW
                    MOVE 'O'     TO COM-MA-IND-EMISSION
                    MOVE 'O'     TO W-REAF                                   EQW
AD548               GO           TO FIN-CTRL-LOG-ANTE                        EQW
               END-IF                                                        EQW
04012                                                                   EQW9Z0MQ
01673 *--- CODE CIE A '999'                                             EQW92BG2
03916 * --> MESSAGE INDIQUANT QUE LE LIBELLE COMPAGNIE PRECEDENT EST    EQW90UNO
03916 * --> MODIFIABLE.                                                 EQW90UNO
               IF  ECR-ANVCIEXO = '999'                                      EQW
               AND MODIF-CIE-NON                                             EQW
                   MOVE 'FBTD1' TO COM-GENE-MESINF                           EQW
                   MOVE 'O'     TO COM-MA-IND-EMISSION
                   MOVE 'O'     TO W-REAF                                    EQW
AD548              GO           TO FIN-CTRL-LOG-ANTE                         EQW
               END-IF                                                        EQW
01688      END-IF.                                                      EQW92BG2
      *
AD548  FIN-CTRL-LOG-ANTE.                                               EQW9Z0MQ
AD548      EXIT.                                                        EQW9Z0MQ
AD548 *                                                                 EQW9Z0MQ
AD548  CTRL-LOG-ANTE2.                                                  EQW9Z0MQ
      *
03817 *---SAISIE DU NOMBRE DE MOIS DU RELEVE INFO INTERDITE SI CODE CIE EQW9Z0MQ
03818 *---NON RENSEIGNE                                                 EQW9Z0MQ
03819      IF WSS-APPEL-AIDE-CIE = 'N'                                  EQW9Z0MQ
03820         IF ECR-ANVANCNO NOT = SPACES AND LOW-VALUE                EQW9Z0MQ
03821            IF ECR-ANVCIEXO = SPACES OR LOW-VALUE                  EQW9Z0MQ
03822               MOVE NOR-ALP TO ECR-ANVANCNA                        EQW9Z0MQ
F2980               IF KONTROL = 0                                      EQW9Z0MQ
03824                  MOVE 'FB340' TO COM-GENE-MESANO                  EQW9Z0MQ
F2980                                  COM-CODERR                       EQW9Z0MQ
03826                  MOVE CURSEUR TO ECR-ANVANCNL                     EQW9Z0MQ
F2980                  MOVE 1       TO KONTROL                          EQW9Z0MQ
AD548                  GO           TO FIN-CTRL-LOG-ANTE2               EQW9Z0MQ
F2980               END-IF                                              EQW9Z0MQ
03830            END-IF                                                 EQW9Z0MQ
03831         END-IF                                                    EQW9Z0MQ
F2980      END-IF.                                                      EQW9Z0MQ
F2980
03834 *---SAISIE DU NOMBRE DE MOIS D'INTERRUPTION DES 12 DERNIERS MOIS  EQW9Z0MQ
03835 *---INTERDIT SI PAS DE RELEVE D'INFO                              EQW9Z0MQ
03836      IF ECR-ANVINTNO NOT = SPACES AND LOW-VALUE AND ' 0' AND '0 ' EQW9Z0MQ
03837                            AND '00'                               EQW9Z0MQ
03838         IF ECR-ANVANCNO = SPACES OR LOW-VALUE OR '0 ' OR '0 '     EQW9Z0MQ
03839                           OR '00'                                 EQW9Z0MQ
03840            MOVE NOR-ALP TO ECR-ANVINTNA                           EQW9Z0MQ
03841            IF KONTROL = 0                                         EQW9Z0MQ
03842               MOVE 'FB030' TO COM-GENE-MESANO                     EQW9Z0MQ
03843                               COM-CODERR                          EQW9Z0MQ
03844               MOVE CURSEUR TO ECR-ANVINTNL                        EQW9Z0MQ
03845               MOVE 1       TO KONTROL                             EQW9Z0MQ
AD548               GO           TO FIN-CTRL-LOG-ANTE2                  EQW9Z0MQ
03763            END-IF                                                 EQW9Z0MQ
03764         END-IF                                                    EQW9Z0MQ
F2980      END-IF.                                                      EQW9Z0MQ
F2980
AD548  FIN-CTRL-LOG-ANTE2.                                              EQW9Z0MQ
04014      EXIT.                                                        EQW9Z0MQ
04015 *                                                                 EQW9Z0MQ
04016 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
04017 * CONTROLES LOGIQUES 2 ROUES   * TRAITEMENT NORMAL                EQW9Z0MQ
04018 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
04019  CONTROLE-LOGIQUE-2R.                                             EQW9Z0MQ
04020 *--------------------                                             EQW9Z0MQ
04021      MOVE SPACES TO TAB-FVVEHI.                                   EQW9Z0MQ
04022      MOVE SPACES TO FB2CLA01.                                     EQW9Z0MQ
04023      MOVE SPACES TO FB2GRP01.                                     EQW9Z0MQ
04024                                                                   EQW9Z0MQ
04025 *--- ON DETECTE LES CHANGEMENTS DE CODE VÈHICULE                  EQW9Z0MQ
04026      IF WSS-APPEL-AIDE-CDVEHI = 'N'                               EQW9Z0MQ
04027       AND (ECR-VEHCODCO NOT = SPACES AND LOW-VALUE AND '9999999') EQW9Z0MQ
04028       AND (VEHCODC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE)  EQW9Z0MQ
04029         IF ECR-VEHCODCO NOT = VEHCODC OF TS-VEHICULE(1)           EQW9Z0MQ
04030            MOVE 'O'       TO WSS-MODIF-CODE-AUTO                  EQW9Z0MQ
04031         ELSE                                                      EQW9Z0MQ
04032            MOVE 'N'       TO WSS-MODIF-CODE-AUTO                  EQW9Z0MQ
04033         END-IF                                                    EQW9Z0MQ
04034      ELSE                                                         EQW9Z0MQ
04035         MOVE 'N'       TO WSS-MODIF-CODE-AUTO                     EQW9Z0MQ
04036      END-IF.                                                      EQW9Z0MQ
04037                                                                   EQW9Z0MQ
04038 *--- SI LE CODE VEHICULE EST DIFFERENT DE 9999999 IL DOIT EXISTER EQW9Z0MQ
04039 *--- DANS LA TABLE FVVEHI01 POUR 2R                               EQW9Z0MQ
04040      IF WSS-APPEL-AIDE-CDVEHI = 'N'                               EQW9Z0MQ
04041         IF (ECR-VEHCODCO NOT = SPACES AND '9999999')              EQW9Z0MQ
04042            PERFORM ACCES-FVVEHI THRU FIN-ACCES-FVVEHI             EQW9Z0MQ
04043            IF WSS-CDVEHI-TROUVE = 'N'                             EQW9Z0MQ
04044               MOVE NOR-ALP TO ECR-VEHCODCA                        EQW9Z0MQ
04045               IF KONTROL = 0                                      EQW9Z0MQ
04046                  MOVE 'FB113' TO COM-GENE-MESANO                  EQW9Z0MQ
04047                                  COM-CODERR                       EQW9Z0MQ
04048                  MOVE CURSEUR TO ECR-VEHCODCL                     EQW9Z0MQ
04049                  MOVE 1       TO KONTROL                          EQW9Z0MQ
04050                  GO           TO FIN-CONTROLE-LOGIQUE-2R          EQW9Z0MQ
04051               END-IF                                              EQW9Z0MQ
04052            ELSE                                                   EQW9Z0MQ
04053               MOVE MARQUE OF TAB-FVVEHI TO                        EQW9Z0MQ
04054                              VEHMARL OF TS-VEHICULE(2)            EQW9Z0MQ
04055               MOVE CYLINDR TO C-XKMTENTREE                        EQW9Z0MQ
04056               MOVE 4 TO C-XKMTLONG                                EQW9Z0MQ
04057               MOVE ZERO TO C-XKMTDECIMALE                         EQW9Z0MQ
04058               PERFORM CADRAGE THRU FIN-CADRAGE                    EQW9Z0MQ
04059               IF C-XKMTRETCOD NOT = ZERO                          EQW9Z0MQ
04060                  MOVE NOR-ALP TO ECR-VEHCYLNA                     EQW9Z0MQ
04061                  IF KONTROL = 0                                   EQW9Z0MQ
04062                     MOVE 'FB095' TO COM-GENE-MESANO               EQW9Z0MQ
04063                                     COM-CODERR                    EQW9Z0MQ
04064                     MOVE CURSEUR TO ECR-VEHCYLNL                  EQW9Z0MQ
04065                     MOVE 1       TO KONTROL                       EQW9Z0MQ
04066                     GO           TO FIN-CONTROLE-LOGIQUE-2R       EQW9Z0MQ
04067                  END-IF                                           EQW9Z0MQ
04068               ELSE                                                EQW9Z0MQ
04069                  IF C-XKMTNUM-0D = ZERO                           EQW9Z0MQ
04070                     MOVE NOR-ALP TO ECR-VEHCYLNA                  EQW9Z0MQ
04071                     IF KONTROL = 0                                EQW9Z0MQ
04072                        MOVE 'FB095' TO COM-GENE-MESANO            EQW9Z0MQ
04073                                        COM-CODERR                 EQW9Z0MQ
04074                        MOVE CURSEUR TO ECR-VEHCYLNL               EQW9Z0MQ
04075                        MOVE 1       TO KONTROL                    EQW9Z0MQ
04076                        GO           TO FIN-CONTROLE-LOGIQUE-2R    EQW9Z0MQ
04077                     END-IF                                        EQW9Z0MQ
04078                  ELSE                                             EQW9Z0MQ
04079                     MOVE ZERO   TO VEHCYLN OF TS-VEHICULE(2)      EQW9Z0MQ
04080                     ADD  C-XKMTNUM-0D                             EQW9Z0MQ
04081                                 TO VEHCYLN OF TS-VEHICULE(2)      EQW9Z0MQ
04082                  END-IF                                           EQW9Z0MQ
04083               END-IF                                              EQW9Z0MQ
04084               MOVE CATEGOR OF TAB-FVVEHI TO                       EQW9Z0MQ
04085                               VEHGENC OF TS-VEHICULE(2)           EQW9Z0MQ
04086               MOVE GROUPE  OF TAB-FVVEHI TO                       EQW9Z0MQ
04087                               VEHGROC OF TS-VEHICULE(2)           EQW9Z0MQ
U4172                               VEHGROC OF TS-VEHICULE(1)           EQW9Z0MQ
04088               MOVE CLASSE  OF TAB-FVVEHI TO                       EQW9Z0MQ
04089                               VEHCLAC OF TS-VEHICULE(2)           EQW9Z0MQ
U4172                               VEHCLAC OF TS-VEHICULE(1)           EQW9Z0MQ
04090               MOVE APPCOMM OF TAB-FVVEHI TO                       EQW9Z0MQ
04091                               VEHMODL OF TS-VEHICULE(2)           EQW9Z0MQ
04092            END-IF                                                 EQW9Z0MQ
04093         END-IF                                                    EQW9Z0MQ
04094      END-IF.                                                      EQW9Z0MQ
04095                                                                   EQW9Z0MQ
04096 *--- SI LE CODE AUTO EST EGAL A 9999999, ON ACCEDE A LA TABLE     EQW9Z0MQ
04097 *--- FB2CLA01 POUR RECUPERER LA CLASSE A PARTIR DU GENRE SAISI    EQW9Z0MQ
04098 *--- ET DE LA VALEUR A NEUF SAISIE                                EQW9Z0MQ
04099                                                                   EQW9Z0MQ
04100      IF WSS-APPEL-AIDE-GENRE = 'N'                                EQW9Z0MQ
04101         IF ECR-VEHCODCO = '9999999'                               EQW9Z0MQ
04102            IF ECR-VEHGENCO NOT = SPACES                           EQW9Z0MQ
04103               AND ECR-VEHVALMO NOT = SPACES                       EQW9Z0MQ
04104               PERFORM ACCES-FB2CLA THRU FIN-ACCES-FB2CLA          EQW9Z0MQ
04105               IF WSS-CLASSE-TROUVE = 'N'                          EQW9Z0MQ
04106                  MOVE NOR-ALP TO ECR-VEHGENCA                     EQW9Z0MQ
04107                  IF KONTROL = 0                                   EQW9Z0MQ
04108                     MOVE 'FB114' TO COM-GENE-MESANO               EQW9Z0MQ
04109                                     COM-CODERR                    EQW9Z0MQ
04110                     MOVE CURSEUR TO ECR-VEHGENCL                  EQW9Z0MQ
04111                     MOVE 1       TO KONTROL                       EQW9Z0MQ
04112                     GO           TO FIN-CONTROLE-LOGIQUE-2R       EQW9Z0MQ
04113                  END-IF                                           EQW9Z0MQ
04114               ELSE                                                EQW9Z0MQ
04115                  MOVE CL2CLAC OF FB2CLA01 TO                      EQW9Z0MQ
04116                                  VEHCLAC OF TS-VEHICULE(2)        EQW9Z0MQ
      * VEHCLAC(2) EST REDÈTERMINÈ, MAIS VEHCLAC(1) N'EST MIS ‡ JOUR
      * DANS FB90T10 QUE SI ‡ BLANC. DANS LES AUTRES CAS, IL N'EST
      * JAMAIS REDÈTERMINÈ ==> NÈCESSITÈ DE FORCER VEHCLAC(1)
04115                  MOVE CL2CLAC OF FB2CLA01 TO                      EQW9Z0MQ
04116                                  VEHCLAC OF TS-VEHICULE(1)        EQW9Z0MQ
04117               END-IF                                              EQW9Z0MQ
04118            END-IF                                                 EQW9Z0MQ
04119         END-IF                                                    EQW9Z0MQ
04120      END-IF.                                                      EQW9Z0MQ
04121                                                                   EQW9Z0MQ
04122 *--- SI LE CODE AUTO EST EGAL A 9999999, ON ACCEDE A LA TABLE     EQW9Z0MQ
04123 *--- FB2GRP01 POUR RECUPERER LE GROUPE A PARTIR DU GENRE SAISI    EQW9Z0MQ
04124 *--- ET DE LA CYLINDREE SAISIE                                    EQW9Z0MQ
04125      IF WSS-APPEL-AIDE-GENRE = 'N'                                EQW9Z0MQ
04126         IF ECR-VEHCODCO = '9999999'                               EQW9Z0MQ
04127            IF ECR-VEHGENCO NOT = SPACES                           EQW9Z0MQ
04128               AND ECR-VEHCYLNO NOT = SPACES                       EQW9Z0MQ
04129               PERFORM ACCES-FB2GRP THRU FIN-ACCES-FB2GRP          EQW9Z0MQ
04130               IF WSS-GRP-TROUVE = 'N'                             EQW9Z0MQ
04131                  MOVE NOR-ALP TO ECR-VEHGENCA                     EQW9Z0MQ
04132                  IF KONTROL = 0                                   EQW9Z0MQ
04133                     MOVE 'FB115' TO COM-GENE-MESANO               EQW9Z0MQ
04134                                     COM-CODERR                    EQW9Z0MQ
04135                     MOVE CURSEUR TO ECR-VEHGENCL                  EQW9Z0MQ
04136                     MOVE 1       TO KONTROL                       EQW9Z0MQ
04137                     GO           TO FIN-CONTROLE-LOGIQUE-2R       EQW9Z0MQ
04138                  END-IF                                           EQW9Z0MQ
04139               ELSE                                                EQW9Z0MQ
04140                  MOVE GR2GRPC OF FB2GRP01 TO                      EQW9Z0MQ
04141                                  VEHGROC OF TS-VEHICULE(2)        EQW9Z0MQ
      * VEHGROC(2) EST REDÈTERMINÈ, MAIS VEHGROC(1) N'EST MIS ‡ JOUR
      * DANS FB90T10 QUE SI ‡ BLANC. DANS LES AUTRES CAS, IL N'EST
      * JAMAIS REDÈTERMINÈ ==> NÈCESSITÈ DE FORCER VEHGROC(1)
04140                  MOVE GR2GRPC OF FB2GRP01 TO                      EQW9Z0MQ
04141                                  VEHGROC OF TS-VEHICULE(1)        EQW9Z0MQ
04142               END-IF                                              EQW9Z0MQ
04143            END-IF                                                 EQW9Z0MQ
04144         END-IF                                                    EQW9Z0MQ
04145      END-IF.                                                      EQW9Z0MQ
04146                                                                   EQW9Z0MQ
04147 *--- LA VALEUR A NEUF DOIT ETRE AU MINIMUM DE 500 EURO POUR LES 2REQW9Z0MQ
04148      IF ECR-VEHVALMO NOT = SPACES AND LOW-VALUE                   EQW9Z0MQ
04149         IF WSS-VALNEUF < 500                                      EQW9Z0MQ
04150            MOVE NOR-ALP TO ECR-VEHVALMA                           EQW9Z0MQ
04151            IF KONTROL = 0                                         EQW9Z0MQ
04152               MOVE 'FB292' TO COM-GENE-MESANO                     EQW9Z0MQ
04153                               COM-CODERR                          EQW9Z0MQ
04154               MOVE CURSEUR TO ECR-VEHVALML                        EQW9Z0MQ
04155               MOVE 1       TO KONTROL                             EQW9Z0MQ
04156               GO           TO FIN-CONTROLE-LOGIQUE-2R             EQW9Z0MQ
04157            END-IF                                                 EQW9Z0MQ
04158         END-IF                                                    EQW9Z0MQ
04159      END-IF.                                                      EQW9Z0MQ
04160                                                                   EQW9Z0MQ
04161 *--- SEULE LES FORMULES SANS GARANTIE VOL (M1 ET M4) SONT         EQW9Z0MQ
04162 *--- POSSIBLES SANS NIVEAU DE PROTECTION VOL                      EQW9Z0MQ
04163      IF ECR-VEHPRTCO = 'N'                                        EQW9Z0MQ
04164         IF ECR-VEHFORCO  = 'M2' OR 'M3'                           EQW9Z0MQ
04165            MOVE NOR-ALP TO ECR-VEHFORCA                           EQW9Z0MQ
04166            IF KONTROL = 0                                         EQW9Z0MQ
04167               MOVE 'FB333' TO COM-GENE-MESANO                     EQW9Z0MQ
04168                               COM-CODERR                          EQW9Z0MQ
04169               MOVE CURSEUR TO ECR-VEHFORCL                        EQW9Z0MQ
04170               MOVE 1       TO KONTROL                             EQW9Z0MQ
04171               GO           TO FIN-CONTROLE-LOGIQUE-2R             EQW9Z0MQ
04172            END-IF                                                 EQW9Z0MQ
04173         END-IF                                                    EQW9Z0MQ
04174      END-IF.                                                      EQW9Z0MQ

04161 *--- EN CAS DE REMPLACEMENT (CHANGEMENT) DE VÈHICULE 2R, ON NE    EQW9Z0MQ
04162 *--- PEUT REMPLACER QUE DES VÈHICULES DE MEME CATEGORIES          EQW9Z0MQ
           IF WSS-APPEL-AIDE-GENRE = 'N'
              IF WSS-APPEL-AIDE-CDVEHI = 'N'
                 IF VEHCHGC OF TS-VEHICULE (1) = '2'
04163               IF VEHACTC OF TS-VEHICULE (1) = 'I'                 EQW9Z0MQ
04164                 PERFORM CONTROLE-REPL-2R THRU FIN-CONTROLE-REPL-2REQW9Z0MQ
04174               END-IF                                              EQW9Z0MQ
04172            END-IF                                                 EQW9Z0MQ
04173         END-IF                                                    EQW9Z0MQ
04174      END-IF.                                                      EQW9Z0MQ

F8556      IF INF-NATMVT OF TS-SUSPENS1 = 'RV'                          EQW9Z0MQ
F8556       AND COM-FB-CODE-ACTION = 'M'                                EQW9Z0MQ
F8556        AND (VEHACTC OF TS-VEHICULE(1) NOT = 'I')                  EQW9Z0MQ
F8556         IF ECR-VEHCODCO NOT = SPACES AND                          EQW9Z0MQ
F8556            ECR-VEHIMMXO NOT = SPACES AND                          EQW9Z0MQ
F8556            ECR-VEHCIRDO NOT = SPACES                              EQW9Z0MQ
F8556            PERFORM DETER-CHANGE-VEHI THRU FIN-DETER-CHANGE-VEHI   EQW9Z0MQ
F8556         END-IF
F8556      END-IF.
F8556
F8556      IF INF-NATMVT OF TS-SUSPENS1 = 'AN'
F8556       OR (INF-NATMVT OF TS-SUSPENS1 = 'AV' AND
F2033           VEHACTC OF TS-VEHICULE(1) NOT = 'I' AND
F8556           VEHUSAC OF TS-VEHICULE(4)NOT = '3')
F8556       OR (INF-NATMVT OF TS-SUSPENS1 = 'RV' AND
F2033           VEHACTC OF TS-VEHICULE(1) NOT = 'I' AND
F8556           VEHUSAC OF TS-VEHICULE(4)NOT = '3')
F2033 ***   OR (INF-NATMVT OF TS-SUSPENS1 = 'AV' AND
F2033 ***       VEHACTC OF TS-VEHICULE(1) = 'I')
F2033 ***   OR (INF-NATMVT OF TS-SUSPENS1 = 'RV' AND
F2033 ***       VEHACTC OF TS-VEHICULE(1) = 'I')
F8556       OR (INF-NATMVT OF TS-SUSPENS1 = 'RV' AND
F2033           VEHACTC OF TS-VEHICULE(1) NOT = 'I' AND
F8556           WSS-CHANGE-VEHICULE = 'O')
F8556           PERFORM CTRL-USAGE THRU FIN-CTRL-USAGE
F8556      END-IF.
04175                                                                   EQW9Z0MQ
04176  FIN-CONTROLE-LOGIQUE-2R.                                         EQW9Z0MQ
04177      EXIT.                                                        EQW9Z0MQ
04178
F8556  CTRL-USAGE.
F8556      IF ECR-VEHUSACO NOT = SPACES
F8556         MOVE ECR-VEHUSACO TO WSS-VEHUSAC
F8556         IF DEB-VEHUSAC = '3'
F8556            MOVE NOR-ALP TO ECR-VEHUSACA                           EQW9Z0MQ
F8556            IF KONTROL = 0                                         EQW9Z0MQ
F8556               MOVE 'FVV48' TO COM-GENE-MESANO                     EQW9Z0MQ
F8556                               COM-CODERR                          EQW9Z0MQ
F8556               MOVE CURSEUR TO ECR-VEHUSACL                        EQW9Z0MQ
F8556               MOVE 1       TO KONTROL                             EQW9Z0MQ
F8556               GO           TO FIN-CTRL-USAGE                      EQW9Z0MQ
F8556            END-IF
F8556         END-IF
F8556      END-IF.
F8556  FIN-CTRL-USAGE. EXIT.
04178 *                                                                 EQW9Z0MQ
04179 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
04180 * CONTROLES REMPLACEMENT 2R    * TRAITEMENT NORMAL                EQW9Z0MQ
04181 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
04182  CONTROLE-REPL-2R.                                                EQW9Z0MQ
04183 *-----------------                                                EQW9Z0MQ
           MOVE SPACES TO WSS-ENSEMBLE-2R-ORIGINE.
           MOVE SPACES TO WSS-ENSEMBLE-2R-CHANGER.
           MOVE SPACES TO WSS-NOUV-CYLX.
           MOVE SPACES TO WSS-NOUV-GENRE.
           MOVE ZERO   TO WSS-RANG-TS-CHANGER.
           MOVE 'N'    TO WSS-FIN-VEHI.
           PERFORM READ-TS-VEHI-CHANGER THRU FIN-READ-TS-VEHI-CHANGER
           UNTIL VEHORCX OF TS-VEHICULE(1) = VEHORDX
                                              OF TS-VEHICULE-CHANGER(1)
              OR WSS-FIN-VEHI = 'O'.
           IF WSS-FIN-VEHI = 'N'
              IF VEHGENC OF TS-VEHICULE-CHANGER (1) = '0' OR '1' OR '2' EQW9Z0MQ
                                                OR '3'                  EQW9Z0MQ
                 MOVE 'E2' TO WSS-ENSEMBLE-2R-ORIGINE
              ELSE
                 IF (VEHGENC OF TS-VEHICULE-CHANGER (1) = 'S' OR 'C')   EQW9Z0MQ
                   AND (VEHCYLN OF TS-VEHICULE-CHANGER (1) <= 50)       EQW9Z0MQ
                    MOVE 'E1' TO WSS-ENSEMBLE-2R-ORIGINE
                 ELSE
                    MOVE 'E3' TO WSS-ENSEMBLE-2R-ORIGINE
04177            END-IF                                                 EQW9Z0MQ
04177         END-IF                                                    EQW9Z0MQ

              IF ECR-VEHGENCO = SPACES OR LOW-VALUE                     EQW9Z0MQ
                 MOVE VEHGENC OF TS-VEHICULE(2) TO WSS-NOUV-GENRE
              ELSE
                 MOVE ECR-VEHGENCO TO WSS-NOUV-GENRE
              END-IF
              IF ECR-VEHCYLNO = SPACES OR LOW-VALUE                     EQW9Z0MQ
                 MOVE VEHCYLN OF TS-VEHICULE(2) TO WSS-NOUV-CYL
              ELSE
                 MOVE WSS-CYL TO WSS-NOUV-CYL
              END-IF
              IF (WSS-NOUV-GENRE = SPACES OR LOW-VALUE)                 EQW9Z0MQ
                OR (WSS-NOUV-CYLX = SPACES OR LOW-VALUE)                EQW9Z0MQ
                 MOVE SPACE TO WSS-ENSEMBLE-2R-CHANGER
              ELSE
                 IF WSS-NOUV-GENRE = '0' OR '1' OR '2' OR '3'           EQW9Z0MQ
                    MOVE 'E2' TO WSS-ENSEMBLE-2R-CHANGER
                 ELSE
                    IF (WSS-NOUV-GENRE = 'S' OR 'C')                    EQW9Z0MQ
                      AND (WSS-NOUV-CYL <= 50)                          EQW9Z0MQ
                       MOVE 'E1' TO WSS-ENSEMBLE-2R-CHANGER
                    ELSE
                       MOVE 'E3' TO WSS-ENSEMBLE-2R-CHANGER
                    END-IF
                 END-IF
              END-IF
           END-IF.

           IF WSS-ENSEMBLE-2R-ORIGINE NOT = WSS-ENSEMBLE-2R-CHANGER
              MOVE NOR-ALP TO ECR-VEHGENCA
              IF KONTROL = 0
                 MOVE 'FB446' TO COM-GENE-MESANO
                                 COM-CODERR
                 MOVE CURSEUR TO ECR-VEHGENCL
                 MOVE 1       TO KONTROL
                 GO           TO FIN-CONTROLE-REPL-2R                   EQW9Z0MQ
04177         END-IF                                                    EQW9Z0MQ
04177      END-IF.                                                      EQW9Z0MQ

04182  FIN-CONTROLE-REPL-2R.                                            EQW9Z0MQ
04177      EXIT.                                                        EQW9Z0MQ
04178 *                                                                 EQW9Z0MQ
04179 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
04180 * CONTROLES LOGIQUES 4 ROUES   * TRAITEMENT NORMAL                EQW9Z0MQ
04181 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
04182  CONTROLE-LOGIQUE-4R.                                             EQW9Z0MQ
04183 *--------------------                                             EQW9Z0MQ
04184      MOVE SPACES TO TAB-MIAUTO.                                   EQW9Z0MQ
04185      MOVE SPACES TO FB4CLA01.                                     EQW9Z0MQ
04186      MOVE SPACES TO FB4GRP01.                                     EQW9Z0MQ
04187      MOVE SPACES TO WSS-MODELE.                                   EQW9Z0MQ
04188      MOVE SPACES TO WSS-MODELE-VERSION.                           EQW9Z0MQ
04189                                                                   EQW9Z0MQ
04190 *--- SI LE VEHICULE EST UN CAMPING-CAR, LE CODE AUTO DOIT ÍTRE    EQW9Z0MQ
04191 *--- A 9999999.                                                   EQW9Z0MQ
04192      IF WSS-APPEL-AIDE-CDVEHI = 'N'                               EQW9Z0MQ
04193         IF ECR-VEHTYPCO = 'CC '                                   EQW9Z0MQ
04194            IF ECR-VEHCODCO NOT = '9999999'                        EQW9Z0MQ
04195               MOVE NOR-ALP TO ECR-VEHCODCA                        EQW9Z0MQ
04196               IF KONTROL = 0                                      EQW9Z0MQ
04197                  MOVE 'FB168' TO COM-GENE-MESANO                  EQW9Z0MQ
04198                                  COM-CODERR                       EQW9Z0MQ
04199                  MOVE CURSEUR TO ECR-VEHCODCL                     EQW9Z0MQ
04200                  MOVE 1       TO KONTROL                          EQW9Z0MQ
04201                  GO           TO FIN-CONTROLE-LOGIQUE-4R          EQW9Z0MQ
04202               END-IF                                              EQW9Z0MQ
04203            END-IF                                                 EQW9Z0MQ
04204         END-IF                                                    EQW9Z0MQ
04205      END-IF.                                                      EQW9Z0MQ
04206                                                                   EQW9Z0MQ
04207 *--- ON DETECTE LES CHANGEMENTS DE CODE VÈHICULE                  EQW9Z0MQ
04208      IF WSS-APPEL-AIDE-CDVEHI = 'N'                               EQW9Z0MQ
04209       AND (ECR-VEHCODCO NOT = SPACES AND LOW-VALUE AND '9999999') EQW9Z0MQ
04210       AND (VEHCODC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE)  EQW9Z0MQ
04211         IF ECR-VEHCODCO NOT = VEHCODC OF TS-VEHICULE(1)           EQW9Z0MQ
04212            MOVE 'O'       TO WSS-MODIF-CODE-AUTO                  EQW9Z0MQ
04213         ELSE                                                      EQW9Z0MQ
04214            MOVE 'N'       TO WSS-MODIF-CODE-AUTO                  EQW9Z0MQ
04215         END-IF                                                    EQW9Z0MQ
04216      ELSE                                                         EQW9Z0MQ
04217         MOVE 'N'       TO WSS-MODIF-CODE-AUTO                     EQW9Z0MQ
04218      END-IF.                                                      EQW9Z0MQ
04219                                                                   EQW9Z0MQ
04220 *--- SI LE CODE VEHICULE EST DIFFERENT DE 9999999 IL DOIT EXISTER EQW9Z0MQ
04221 *--- DANS LA TABLE MIAUTO01 POUR 4R                               EQW9Z0MQ
04222      IF WSS-APPEL-AIDE-CDVEHI = 'N'                               EQW9Z0MQ
04223         IF (ECR-VEHCODCO NOT = SPACES AND '9999999')              EQW9Z0MQ
04224            PERFORM ACCES-MIAUTO THRU FIN-ACCES-MIAUTO             EQW9Z0MQ
04225            IF WSS-CDVEHI-TROUVE = 'N'                             EQW9Z0MQ
04226               MOVE NOR-ALP TO ECR-VEHCODCA                        EQW9Z0MQ
04227               IF KONTROL = 0                                      EQW9Z0MQ
04228                  MOVE 'FB113' TO COM-GENE-MESANO                  EQW9Z0MQ
04229                                  COM-CODERR                       EQW9Z0MQ
04230                  MOVE CURSEUR TO ECR-VEHCODCL                     EQW9Z0MQ
04231                  MOVE 1       TO KONTROL                          EQW9Z0MQ
04232                  GO           TO FIN-CONTROLE-LOGIQUE-4R          EQW9Z0MQ
04233               END-IF                                              EQW9Z0MQ
04234            ELSE                                                   EQW9Z0MQ
04235               MOVE MARQUEL OF TAB-MIAUTO TO                       EQW9Z0MQ
04236                               VEHMARL OF TS-VEHICULE(2)           EQW9Z0MQ
04237               MOVE MODELEC OF TAB-MIAUTO TO WSS-MODELE            EQW9Z0MQ
04238               PERFORM VARYING IND-A FROM 20 BY -1                 EQW9Z0MQ
04239                 UNTIL WSS-MODELE(IND-A:1) NOT = SPACE             EQW9Z0MQ
04240               END-PERFORM                                         EQW9Z0MQ
04241               MOVE WSS-MODELE(1:IND-A) TO WSS-MODELE-VERSION      EQW9Z0MQ
04242               ADD 1                    TO IND-A                   EQW9Z0MQ
04243               MOVE ' '              TO WSS-MODELE-VERSION(IND-A:1)EQW9Z0MQ
04244               ADD 1                    TO   IND-A                 EQW9Z0MQ
04245               COMPUTE IND-B = 30 - IND-A                          EQW9Z0MQ
04246               MOVE VERSIOL OF TAB-MIAUTO TO                       EQW9Z0MQ
04247                               WSS-MODELE-VERSION(IND-A:IND-B)     EQW9Z0MQ
04248               MOVE WSS-MODELE-VERSION TO VEHMODL OF TS-VEHICULE(2)EQW9Z0MQ
04249               MOVE PUISFCO TO C-XKMTENTREE                        EQW9Z0MQ
04250               MOVE 2 TO C-XKMTLONG                                EQW9Z0MQ
04251               MOVE ZERO TO C-XKMTDECIMALE                         EQW9Z0MQ
04252               PERFORM CADRAGE THRU FIN-CADRAGE                    EQW9Z0MQ
04253               IF C-XKMTRETCOD NOT = ZERO                          EQW9Z0MQ
04254                  MOVE NOR-ALP TO ECR-VEHCYLNA                     EQW9Z0MQ
04255                  IF KONTROL = 0                                   EQW9Z0MQ
04256                     MOVE 'FB095' TO COM-GENE-MESANO               EQW9Z0MQ
04257                                     COM-CODERR                    EQW9Z0MQ
04258                     MOVE CURSEUR TO ECR-VEHCYLNL                  EQW9Z0MQ
04259                     MOVE 1       TO KONTROL                       EQW9Z0MQ
04260                     GO           TO FIN-CONTROLE-LOGIQUE-4R       EQW9Z0MQ
04261                  END-IF                                           EQW9Z0MQ
04262               ELSE                                                EQW9Z0MQ
04263                  IF C-XKMTNUM-0D = ZERO                           EQW9Z0MQ
04264                     MOVE NOR-ALP TO ECR-VEHCYLNA                  EQW9Z0MQ
04265                     IF KONTROL = 0                                EQW9Z0MQ
04266                        MOVE 'FB095' TO COM-GENE-MESANO            EQW9Z0MQ
04267                                        COM-CODERR                 EQW9Z0MQ
04268                        MOVE CURSEUR TO ECR-VEHCYLNL               EQW9Z0MQ
04269                        MOVE 1       TO KONTROL                    EQW9Z0MQ
04270                        GO           TO FIN-CONTROLE-LOGIQUE-4R    EQW9Z0MQ
04271                     END-IF                                        EQW9Z0MQ
04272                  ELSE                                             EQW9Z0MQ
04273                     MOVE ZERO   TO VEHCYLN OF TS-VEHICULE(2)      EQW9Z0MQ
04274                     ADD  C-XKMTNUM-0D                             EQW9Z0MQ
04275                                 TO VEHCYLN OF TS-VEHICULE(2)      EQW9Z0MQ
04276                  END-IF                                           EQW9Z0MQ
04277               END-IF                                              EQW9Z0MQ
04278               MOVE GENRE   OF TAB-MIAUTO TO                       EQW9Z0MQ
04279                               VEHGENC OF TS-VEHICULE(2)           EQW9Z0MQ
      * FORCAGE DANS ITEM 1 CAR SINON PAS MIS ‡ JOUR
04280               MOVE GROUPEC OF TAB-MIAUTO TO                       EQW9Z0MQ
04281                               VEHGROC OF TS-VEHICULE(2)           EQW9Z0MQ
04281                               VEHGROC OF TS-VEHICULE(1)           EQW9Z0MQ
      * FORCAGE DANS ITEM 1 CAR SINON PAS MIS ‡ JOUR
04282               MOVE CLASSEC OF TAB-MIAUTO TO                       EQW9Z0MQ
04283                               VEHCLAC OF TS-VEHICULE(2)           EQW9Z0MQ
04283                               VEHCLAC OF TS-VEHICULE(1)           EQW9Z0MQ
04284               IF ANTVOLC OF TAB-MIAUTO = 'S4 ' OR 'S5 ' OR 'S6 '  EQW9Z0MQ
04285                                       OR 'S7 '                    EQW9Z0MQ
04286                  MOVE '4' TO VEHPRTC OF TS-VEHICULE(2)            EQW9Z0MQ
04287               END-IF                                              EQW9Z0MQ
36835               MOVE ALIMVEC OF TAB-MIAUTO TO                       EQW9Z0MQ
36835                               VEHNRJC OF TS-VEHICULE(2)           EQW9Z0MQ
04288            END-IF                                                 EQW9Z0MQ
04289         END-IF                                                    EQW9Z0MQ
04290      END-IF.                                                      EQW9Z0MQ
04291                                                                   EQW9Z0MQ
04292 *--- SI LE CODE AUTO EST EGAL A 9999999, ON ACCEDE A LA TABLE     EQW9Z0MQ
04293 *--- FB4CLA01 POUR RECUPERER LA CLASSE A PARTIR DU GENRE SAISI    EQW9Z0MQ
04294 *--- ET DE LA VALEUR A NEUF SAISIE                                EQW9Z0MQ
04295      IF WSS-APPEL-AIDE-GENRE = 'N'                                EQW9Z0MQ
04296         IF ECR-VEHCODCO = '9999999'                               EQW9Z0MQ
04297            IF ECR-VEHGENCO NOT = SPACES                           EQW9Z0MQ
04298               AND ECR-VEHVALMO NOT = SPACES                       EQW9Z0MQ
04299               PERFORM ACCES-FB4CLA THRU FIN-ACCES-FB4CLA          EQW9Z0MQ
04300               IF WSS-CLASSE-TROUVE = 'N'                          EQW9Z0MQ
04301                  MOVE NOR-ALP TO ECR-VEHGENCA                     EQW9Z0MQ
04302                  IF KONTROL = 0                                   EQW9Z0MQ
04303                     MOVE 'FB114' TO COM-GENE-MESANO               EQW9Z0MQ
04304                                     COM-CODERR                    EQW9Z0MQ
04305                     MOVE CURSEUR TO ECR-VEHGENCL                  EQW9Z0MQ
04306                     MOVE 1       TO KONTROL                       EQW9Z0MQ
04307                     GO           TO FIN-CONTROLE-LOGIQUE-4R       EQW9Z0MQ
04308                  END-IF                                           EQW9Z0MQ
04309               ELSE                                                EQW9Z0MQ
04310                  MOVE CL4CLAC OF FB4CLA01 TO                      EQW9Z0MQ
04311                                  VEHCLAC OF TS-VEHICULE(2)        EQW9Z0MQ
      * VEHCLAC(2) EST REDÈTERMINÈ, MAIS VEHCLAC(1) N'EST MIS ‡ JOUR
      * DANS FB90T10 QUE SI ‡ BLANC. DANS LES AUTRES CAS, IL N'EST
      * JAMAIS REDÈTERMINÈ ==> NÈCESSITÈ DE FORCER VEHCLAC(1)
04310                  MOVE CL4CLAC OF FB4CLA01 TO                      EQW9Z0MQ
04311                                  VEHCLAC OF TS-VEHICULE(1)        EQW9Z0MQ
04312               END-IF                                              EQW9Z0MQ
04313            END-IF                                                 EQW9Z0MQ
04314         END-IF                                                    EQW9Z0MQ
04315      END-IF.                                                      EQW9Z0MQ
04316                                                                   EQW9Z0MQ
04317 *--- SI LE CODE AUTO EST EGAL A 9999999, ON ACCEDE A LA TABLE     EQW9Z0MQ
04318 *--- FB4GRP01 POUR RECUPERER LE GROUPE A PARTIR DU GENRE SAISI    EQW9Z0MQ
04319 *--- ET DE LA CYLINDREE SAISIE                                    EQW9Z0MQ
04320      IF WSS-APPEL-AIDE-GENRE = 'N'                                EQW9Z0MQ
04321         IF ECR-VEHCODCO = '9999999'                               EQW9Z0MQ
04322            IF ECR-VEHGENCO NOT = SPACES                           EQW9Z0MQ
04323               AND ECR-VEHCYLNO NOT = SPACES                       EQW9Z0MQ
04324               PERFORM ACCES-FB4GRP THRU FIN-ACCES-FB4GRP          EQW9Z0MQ
04325               IF WSS-GRP-TROUVE = 'N'                             EQW9Z0MQ
04326                  MOVE NOR-ALP TO ECR-VEHGENCA                     EQW9Z0MQ
04327                  IF KONTROL = 0                                   EQW9Z0MQ
04328                     MOVE 'FB115' TO COM-GENE-MESANO               EQW9Z0MQ
04329                                     COM-CODERR                    EQW9Z0MQ
04330                     MOVE CURSEUR TO ECR-VEHGENCL                  EQW9Z0MQ
04331                     MOVE 1       TO KONTROL                       EQW9Z0MQ
04332                     GO           TO FIN-CONTROLE-LOGIQUE-4R       EQW9Z0MQ
04333                  END-IF                                           EQW9Z0MQ
04334               ELSE                                                EQW9Z0MQ
04335                  MOVE ECR-VEHCIRDO (3:4) TO WSS-ANCIRC            EQW9Z0MQ
04336                  EVALUATE  TRUE                                   EQW9Z0MQ
04337                    WHEN  WSS-ANCIRC NOT > GR4AN1D OF FB4GRP01     EQW9Z0MQ
04338                          MOVE GR4GR1C OF FB4GRP01 TO              EQW9Z0MQ
04339                                       VEHGROC OF TS-VEHICULE(2)   EQW9Z0MQ
04340                    WHEN  WSS-ANCIRC NOT > GR4AN2D OF FB4GRP01     EQW9Z0MQ
04341                          MOVE GR4GR2C OF FB4GRP01 TO              EQW9Z0MQ
04342                                       VEHGROC OF TS-VEHICULE(2)   EQW9Z0MQ
04343                    WHEN  WSS-ANCIRC NOT > GR4AN3D OF FB4GRP01     EQW9Z0MQ
04344                          MOVE GR4GR3C OF FB4GRP01 TO              EQW9Z0MQ
04345                                       VEHGROC OF TS-VEHICULE(2)   EQW9Z0MQ
04346                    WHEN  OTHER                                    EQW9Z0MQ
04347                          MOVE GR4GR4C OF FB4GRP01 TO              EQW9Z0MQ
04348                                       VEHGROC OF TS-VEHICULE(2)   EQW9Z0MQ
04349                  END-EVALUATE                                     EQW9Z0MQ
      * VEHGROC(2) EST REDÈTERMINÈ, MAIS VEHGROC(1) N'EST MIS ‡ JOUR
      * DANS FB90T10 QUE SI ‡ BLANC. DANS LES AUTRES CAS, IL N'EST
      * JAMAIS REDÈTERMINÈ ==> NÈCESSITÈ DE FORCER VEHGROC(1)
                       IF VEHGROC OF TS-VEHICULE(2)
                                         NOT = SPACE AND LOW-VALUE
                          MOVE VEHGROC OF TS-VEHICULE(2)
                                         TO VEHGROC OF TS-VEHICULE(1)
                       END-IF
04350               END-IF                                              EQW9Z0MQ
04351            END-IF                                                 EQW9Z0MQ
04352         END-IF                                                    EQW9Z0MQ
04353      END-IF.                                                      EQW9Z0MQ
04354                                                                   EQW9Z0MQ
04355 *--- SI LA CLASSE EST ST EGAL A 9999999, ON ACCEDE A LA TABLE     EQW9Z0MQ
04356 *--- FB4GRP01 POUR RECUPERER LE GROUPE A PARTIR DU GENRE SAISI    EQW9Z0MQ
04357 *--- ET DE LA CYLINDREE SAISIE                                    EQW9Z0MQ
04358 *----------------------------------------------------------------*EQW9Z0MQ
04359 *    SI LA CLASSE EST DIFFERENTE DE 'X', ALORS:                  *EQW9Z0MQ
04360 *                                                                *EQW9Z0MQ
04361 *      - LA VALEUR A NEUF EST INTERDITE                          *EQW9Z0MQ
04362 *                                                                *EQW9Z0MQ
04363 *----------------------------------------------------------------*EQW9Z0MQ
04364                                                                   EQW9Z0MQ
04365      IF  (VEHCLAC OF TS-VEHICULE(2) NOT = 'X')                    EQW9Z0MQ
04366      AND (ECR-VEHCODCO           NOT = SPACES AND '9999999')      EQW9Z0MQ
04367      AND (ECR-VEHVALMO           NOT = SPACES)                    EQW9Z0MQ
04368          MOVE NOR-ALP TO ECR-VEHVALMA                             EQW9Z0MQ
04369          IF KONTROL = 0                                           EQW9Z0MQ
04370             MOVE 'FB116' TO COM-GENE-MESANO                       EQW9Z0MQ
04371                             COM-CODERR                            EQW9Z0MQ
04372             MOVE CURSEUR TO ECR-VEHVALML                          EQW9Z0MQ
04373             MOVE 1       TO KONTROL                               EQW9Z0MQ
04374             GO           TO FIN-CONTROLE-LOGIQUE-4R               EQW9Z0MQ
04375          END-IF                                                   EQW9Z0MQ
04376      END-IF.                                                      EQW9Z0MQ
04377                                                                   EQW9Z0MQ
04378 *--- SI LA CLASSE VAUT X, LA VALEUR A NEUF DOIT ETRE SUPERIEURE   EQW9Z0MQ
04379 *--- OU EGALE A 29000 EURO                                        EQW9Z0MQ
04380      IF ECR-VEHVALMO NOT = SPACES AND                             EQW9Z0MQ
04381                                   VEHCLAC OF TS-VEHICULE(2) = 'X' EQW9Z0MQ
04382         IF WSS-VALNEUF < 29000                                    EQW9Z0MQ
04383            MOVE NOR-ALP TO ECR-VEHVALMA                           EQW9Z0MQ
04384            IF KONTROL = 0                                         EQW9Z0MQ
04385               MOVE 'FB117' TO COM-GENE-MESANO                     EQW9Z0MQ
04386                               COM-CODERR                          EQW9Z0MQ
04387               MOVE CURSEUR TO ECR-VEHVALML                        EQW9Z0MQ
04388               MOVE 1       TO KONTROL                             EQW9Z0MQ
04389               GO           TO FIN-CONTROLE-LOGIQUE-4R             EQW9Z0MQ
04390            END-IF                                                 EQW9Z0MQ
04391         END-IF                                                    EQW9Z0MQ
04392      END-IF.                                                      EQW9Z0MQ
04393                                                                   EQW9Z0MQ
04394 *--- LA VALEUR A NEUF DOIT ETRE AU MINIMUM DE 6000 EURO POUR LES  EQW9Z0MQ
04395 *--- 4R ET CC                                                     EQW9Z0MQ
04396      IF ECR-VEHVALMO NOT = SPACES AND LOW-VALUE                   EQW9Z0MQ
04397         IF WSS-VALNEUF < 6000                                     EQW9Z0MQ
04398            MOVE NOR-ALP TO ECR-VEHVALMA                           EQW9Z0MQ
04399            IF KONTROL = 0                                         EQW9Z0MQ
04400               MOVE 'FB291' TO COM-GENE-MESANO                     EQW9Z0MQ
04401                               COM-CODERR                          EQW9Z0MQ
04402               MOVE CURSEUR TO ECR-VEHVALML                        EQW9Z0MQ
04403               MOVE 1       TO KONTROL                             EQW9Z0MQ
04404               GO           TO FIN-CONTROLE-LOGIQUE-4R             EQW9Z0MQ
04405            END-IF                                                 EQW9Z0MQ
04406         END-IF                                                    EQW9Z0MQ
04407      END-IF.                                                      EQW9Z0MQ
04408                                                                   EQW9Z0MQ
04409  FIN-CONTROLE-LOGIQUE-4R.                                         EQW9Z0MQ
04410      EXIT.                                                        EQW9Z0MQ
04411 *                                                                 EQW9Z0MQ
04412 ******************************************************************EQW9Z0MQ
04413 *   DETERMINATION DU CHANGEMENT DU VEHICULE                      *EQW9Z0MQ
04414 ******************************************************************EQW9Z0MQ
04415  DETER-CHANGE-VEHI.                                               EQW9Z0MQ
04416 *------------------                                               EQW9Z0MQ
04417      MOVE 'N' TO WSS-CHANGE-VEHICULE.                             EQW9Z0MQ
04418      MOVE SPACES             TO WSS-VEHCIRD.                      EQW9Z0MQ
04419      MOVE ECR-VEHCIRDO (3:4) TO WSS-VEHCIRD (1:4).                EQW9Z0MQ
04420      MOVE ECR-VEHCIRDO (1:2) TO WSS-VEHCIRD (5:2).                EQW9Z0MQ
04421      MOVE '01'               TO WSS-VEHCIRD (7:2).                EQW9Z0MQ
04422                                                                   EQW9Z0MQ
04423      IF (((ECR-VEHCODCO NOT = VEHCODC  OF TS-VEHICULE(4)) AND     EQW9Z0MQ
04424           (ECR-VEHIMMXO NOT = VEHIMMX  OF TS-VEHICULE(4)))        EQW9Z0MQ
04425        OR                                                         EQW9Z0MQ
04426          ((ECR-VEHCODCO NOT = VEHCODC  OF TS-VEHICULE(4)) AND     EQW9Z0MQ
04427           (WSS-VEHCIRD  NOT = RVEHCIRD OF TS-VEHICULE(4)))        EQW9Z0MQ
04428        OR                                                         EQW9Z0MQ
04429          ((ECR-VEHIMMXO NOT = VEHIMMX  OF TS-VEHICULE(4)) AND     EQW9Z0MQ
04430           (WSS-VEHCIRD  NOT = RVEHCIRD OF TS-VEHICULE(4))))       EQW9Z0MQ
04431         MOVE 'O' TO WSS-CHANGE-VEHICULE                           EQW9Z0MQ
04432      ELSE                                                         EQW9Z0MQ
04433         MOVE 'N' TO WSS-CHANGE-VEHICULE                           EQW9Z0MQ
04434      END-IF.                                                      EQW9Z0MQ
04435 *                                                                 EQW9Z0MQ
04436  FIN-DETER-CHANGE-VEHI.                                           EQW9Z0MQ
04437      EXIT.                                                        EQW9Z0MQ
04438 *                                                                 EQW9Z0MQ
04439 ***************************************************************   EQW9Z0MQ
04440 *          RECHERCHE  DES CONNEXES                            *   EQW9Z0MQ
04441 ***************************************************************   EQW9Z0MQ
04442 *                                                                 EQW9Z0MQ
04443  RECHERCHE-CONNEXES.                                              EQW9Z0MQ
04444 *-------------------                                              EQW9Z0MQ
04445      PERFORM APPEL-FB90T09 THRU FIN-APPEL-FB90T09.                EQW9Z0MQ
04446      IF FB90C09-CONT-CONNEXE = 'OUI'                              EQW9Z0MQ
04447         MOVE '0'    TO CONNEXES OF FBMISPTR-IT1                   EQW9Z0MQ
04448      ELSE                                                         EQW9Z0MQ
04449         MOVE '1'    TO CONNEXES OF FBMISPTR-IT1                   EQW9Z0MQ
04450      END-IF.                                                      EQW9Z0MQ
04451                                                                   EQW9Z0MQ
04452      IF ECR-VEHTYPCO = '2R '                                      EQW9Z0MQ
04453         IF RVEHCYLN OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
04454            IF VEHCYLN OF TS-VEHICULE(1) NOT > 50                  EQW9Z0MQ
04455               MOVE '2'    TO CONNEXES OF FBMISPTR-IT1             EQW9Z0MQ
04456               MOVE SPACES TO RANVBONT OF TS-VEHICULE(1)           EQW9Z0MQ
04457               MOVE SPACES TO RANVBOND OF TS-VEHICULE(1)           EQW9Z0MQ
04458               MOVE SPACES TO RANVABOD OF TS-VEHICULE(1)           EQW9Z0MQ
04459               MOVE SPACES TO RANVBONT OF TS-VEHICULE(2)           EQW9Z0MQ
04460               MOVE SPACES TO RANVBOND OF TS-VEHICULE(2)           EQW9Z0MQ
04461               MOVE SPACES TO RANVABOD OF TS-VEHICULE(2)           EQW9Z0MQ
04462               MOVE SPACES TO ECR-ANVBONTO                         EQW9Z0MQ
04463               MOVE SPACES TO ECR-ANVBONDO                         EQW9Z0MQ
04464               MOVE SPACES TO ECR-ANVABODO                         EQW9Z0MQ
04465            ELSE                                                   EQW9Z0MQ
04466               PERFORM ALIM-CRM THRU FIN-ALIM-CRM                  EQW9Z0MQ
04467            END-IF                                                 EQW9Z0MQ
04468         ELSE                                                      EQW9Z0MQ
04469            IF RVEHCYLN OF TS-VEHICULE(2) NOT = SPACES             EQW9Z0MQ
04470                                                      AND LOW-VALUEEQW9Z0MQ
04471               IF VEHCYLN OF TS-VEHICULE(2) NOT > 50               EQW9Z0MQ
04472                  MOVE '2'    TO CONNEXES OF FBMISPTR-IT1          EQW9Z0MQ
04473                  MOVE SPACES TO RANVBONT OF TS-VEHICULE(1)        EQW9Z0MQ
04474                  MOVE SPACES TO RANVBOND OF TS-VEHICULE(1)        EQW9Z0MQ
04475                  MOVE SPACES TO RANVABOD OF TS-VEHICULE(1)        EQW9Z0MQ
04476                  MOVE SPACES TO RANVBONT OF TS-VEHICULE(2)        EQW9Z0MQ
04477                  MOVE SPACES TO RANVBOND OF TS-VEHICULE(2)        EQW9Z0MQ
04478                  MOVE SPACES TO RANVABOD OF TS-VEHICULE(2)        EQW9Z0MQ
04479                  MOVE SPACES TO ECR-ANVBONTO                      EQW9Z0MQ
04480                  MOVE SPACES TO ECR-ANVBONDO                      EQW9Z0MQ
04481                  MOVE SPACES TO ECR-ANVABODO                      EQW9Z0MQ
04482               ELSE                                                EQW9Z0MQ
04483                  PERFORM ALIM-CRM THRU FIN-ALIM-CRM               EQW9Z0MQ
04484               END-IF                                              EQW9Z0MQ
04485            ELSE                                                   EQW9Z0MQ
04486 *             PERFORM ALIM-CRM THRU FIN-ALIM-CRM                  EQW9Z0MQ
04487               MOVE '2'    TO CONNEXES OF FBMISPTR-IT1             EQW9Z0MQ
04488               MOVE SPACES TO RANVBONT OF TS-VEHICULE(1)           EQW9Z0MQ
04489               MOVE SPACES TO RANVBOND OF TS-VEHICULE(1)           EQW9Z0MQ
04490               MOVE SPACES TO RANVABOD OF TS-VEHICULE(1)           EQW9Z0MQ
04491               MOVE SPACES TO RANVBONT OF TS-VEHICULE(2)           EQW9Z0MQ
04492               MOVE SPACES TO RANVBOND OF TS-VEHICULE(2)           EQW9Z0MQ
04493               MOVE SPACES TO RANVABOD OF TS-VEHICULE(2)           EQW9Z0MQ
04494               MOVE SPACES TO ECR-ANVBONTO                         EQW9Z0MQ
04495               MOVE SPACES TO ECR-ANVBONDO                         EQW9Z0MQ
04496               MOVE SPACES TO ECR-ANVABODO                         EQW9Z0MQ
04497            END-IF                                                 EQW9Z0MQ
04498         END-IF                                                    EQW9Z0MQ
04499      ELSE                                                         EQW9Z0MQ
04500         PERFORM ALIM-CRM THRU FIN-ALIM-CRM                        EQW9Z0MQ
04501      END-IF.                                                      EQW9Z0MQ
04502                                                                   EQW9Z0MQ
04503  FIN-RECHERCHE-CONNEXES.                                          EQW9Z0MQ
04504      EXIT.                                                        EQW9Z0MQ
04505 *                                                                 EQW9Z0MQ
04506  ALIM-CRM.                                                        EQW9Z0MQ
04507 *---------                                                        EQW9Z0MQ
04508      MOVE FB90C09-CRM          TO ANVBONT OF TS-VEHICULE (2).     EQW9Z0MQ
04509      COMPUTE WSS-CRM-MOYEN = FB90C09-CRM * 100.                   EQW9Z0MQ
04510      MOVE WSS-CRM-MOYEN        TO WSS-CRM-MOYENZ.                 EQW9Z0MQ
04511                                                                   EQW9Z0MQ
04512      MOVE COM-MA-DT-JOUR       TO RANVBOND OF TS-VEHICULE(2).     EQW9Z0MQ
04513      MOVE '01'                 TO RANVBOND OF TS-VEHICULE(2)(7:2).EQW9Z0MQ
04514      IF WSS-CRM-MOYEN <= 50                                       EQW9Z0MQ
04515         IF FB90C09-DATE50 NOT = SPACE AND LOW-VALUE AND HIGH-VALUEEQW9Z0MQ
04516            MOVE FB90C09-DATE50 TO RANVABOD OF TS-VEHICULE(2)      EQW9Z0MQ
04517         ELSE                                                      EQW9Z0MQ
04518            MOVE COM-MA-DT-JOUR TO RANVABOD OF TS-VEHICULE(2)      EQW9Z0MQ
04519            MOVE '0101'         TO RANVABOD OF TS-VEHICULE(2)(5:4) EQW9Z0MQ
04520         END-IF                                                    EQW9Z0MQ
04521      END-IF.                                                      EQW9Z0MQ
04522  FIN-ALIM-CRM.                                                    EQW9Z0MQ
04523      EXIT.                                                        EQW9Z0MQ
04524 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
04525 * TRAITEMENT DE LA TACHE * FB04 * TRAITEMENT NORMAL               EQW9Z0MQ
04526 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
04527 *                                                                 EQW9Z0MQ
04528  TRAITEMENT-TACHE.                                                EQW9Z0MQ
04529 *-----------------                                                EQW9Z0MQ
04530      IF ECRAN-MODIFIE                                             EQW9Z0MQ
04531         MOVE 'O' TO W-REAF                                        EQW9Z0MQ
04532         MOVE 'N' TO COM-MA-IND-EMISSION                           EQW9Z0MQ
04533         MOVE 'O' TO COM-MA-IND-MODIF                              EQW9Z0MQ
04534      END-IF.                                                      EQW9Z0MQ
04535 *                                                                 EQW9Z0MQ
04536      PERFORM TRAITEMENT-FICHIER THRU                              EQW9Z0MQ
04537              FIN-TRAITEMENT-FICHIER.                              EQW9Z0MQ
04538 *                                                                 EQW9Z0MQ
04539 *    PERFORM TRAITEMENT-COMMAREA THRU                             EQW9Z0MQ
04540 *            FIN-TRAITEMENT-COMMAREA.                             EQW9Z0MQ
04541 *                                                                 EQW9Z0MQ
04542 *    PERFORM TRAITEMENT-MAP THRU                                  EQW9Z0MQ
04543 *            FIN-TRAITEMENT-MAP.                                  EQW9Z0MQ
04544 *                                                                 EQW9Z0MQ
04545  FIN-TRAITEMENT-TACHE.  EXIT.                                     EQW9Z0MQ
04546 *                                                                 EQW9Z0MQ
04547 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
04548 *  GESTION DES FICHIERS     * FB04 * TRAITEMENT NORMAL            EQW9Z0MQ
04549 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
04550 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
04551 *                                                               * EQW9Z0MQ
04552 *                -------------------------------                * EQW9Z0MQ
04553 *                -    GESTION DES FICHIERS     -                * EQW9Z0MQ
04554 *                -------------------------------                * EQW9Z0MQ
04555 *                               -                               * EQW9Z0MQ
04556 *           -----------------------------------------           * EQW9Z0MQ
04557 *           -            -             -            -           * EQW9Z0MQ
04558 * ---------------- ------------ ---------------- -------------- * EQW9Z0MQ
04559 * - DETERMINATION- - CREATION - - MODIFICATION - - SUPRESSION - * EQW9Z0MQ
04560 * ---------------- ------------ ---------------- -------------- * EQW9Z0MQ
04561 *                                                               * EQW9Z0MQ
04562 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
04563  TRAITEMENT-FICHIER.                                              EQW9Z0MQ
04564 *------------------*                                              EQW9Z0MQ
04565      PERFORM MAJ-CODE-ACTION-DB2 THRU FIN-MAJ-CODE-ACTION-DB2.    EQW9Z0MQ
04566                                                                   EQW9Z0MQ
04567      PERFORM MISE-A-JOUR-TS      THRU FIN-MISE-A-JOUR-TS.         EQW9Z0MQ
04568 *                                                                 EQW9Z0MQ
04569      IF WSS-READ-TSVEHI = 'N' AND                                 EQW9Z0MQ
04570        (DONNEES-VEHICULE OF TS-VEHICULE(1) NOT = SPACES           EQW9Z0MQ
04571                                             AND  LOW-VALUE)       EQW9Z0MQ
04572         PERFORM REECRITURE-TS-SUSPENS-DL1 THRU                    EQW9Z0MQ
04573                 FIN-REECRITURE-TS-SUSPENS-DL1                     EQW9Z0MQ
04574         IF INF-EFFET OF TS-SUSPENS1 NOT = SPACES AND LOW-VALUE    EQW9Z0MQ
04575            MOVE INF-EFFET OF TS-SUSPENS1 TO                       EQW9Z0MQ
04576                                         RVEHENTD OF TS-VEHICULE(1)EQW9Z0MQ
04577            ELSE                                                   EQW9Z0MQ
04578               MOVE INF-EFFET OF TS-SUSPENS2 TO                    EQW9Z0MQ
04579                                         RVEHENTD OF TS-VEHICULE(1)EQW9Z0MQ
04580         END-IF                                                    EQW9Z0MQ
04581         MOVE '99999999' TO VEHSORD OF TS-VEHICULE(1)              EQW9Z0MQ
04582         PERFORM ECRITURE-TS-VEHICULE THRU                         EQW9Z0MQ
04583                 FIN-ECRITURE-TS-VEHICULE                          EQW9Z0MQ
04584      END-IF.                                                      EQW9Z0MQ
04585 *                                                                 EQW9Z0MQ
04586      IF WSS-READ-TSVEHI = 'O'                                     EQW9Z0MQ
04587         PERFORM REECRITURE-TS-SUSPENS-DL1 THRU                    EQW9Z0MQ
04588                 FIN-REECRITURE-TS-SUSPENS-DL1                     EQW9Z0MQ
04589         PERFORM REECRITURE-TS-VEHICULE THRU                       EQW9Z0MQ
04590                 FIN-REECRITURE-TS-VEHICULE                        EQW9Z0MQ
04591      END-IF.                                                      EQW9Z0MQ
04592 *                                                                 EQW9Z0MQ
04593      PERFORM APPEL-MA90T00 THRU FIN-APPEL-MA90T00.                EQW9Z0MQ
04594                                                                   EQW9Z0MQ
04595      IF  (COM-GENE-MESINF     = SPACES OR LOW-VALUE)              EQW9Z0MQ
04596      AND (COM90C00-MESINF NOT = SPACES AND LOW-VALUE)             EQW9Z0MQ
04597           MOVE COM90C00-MESINF TO COM-GENE-MESINF                 EQW9Z0MQ
04598                                   COM-CODERR                      EQW9Z0MQ
04599           MOVE 'O' TO W-REAF                                      EQW9Z0MQ
04600      END-IF.                                                      EQW9Z0MQ
04601                                                                   EQW9Z0MQ
04602      IF  (COM-GENE-MESANO     = SPACES OR LOW-VALUE)              EQW9Z0MQ
04603      AND (COM90C00-MESANO NOT = SPACES AND LOW-VALUE)             EQW9Z0MQ
04604           MOVE COM90C00-MESANO TO COM-GENE-MESANO                 EQW9Z0MQ
04605                                   COM-CODERR                      EQW9Z0MQ
04606           MOVE 'O' TO W-REAF                                      EQW9Z0MQ
04607      END-IF.                                                      EQW9Z0MQ
04608                                                                   EQW9Z0MQ
04609      IF INF-ETAT OF TS-SUSPENS1 NOT = '5' AND '7'                 EQW9Z0MQ
04610         IF (INF-ETAT OF TS-SUSPENS1 = '4' AND ECRAN-MODIFIE)      EQW9Z0MQ
04611         OR  INF-ETAT OF TS-SUSPENS1 NOT = '4'                     EQW9Z0MQ
04612             PERFORM APPEL-MA90T20 THRU FIN-APPEL-MA90T20          EQW9Z0MQ
04613         END-IF                                                    EQW9Z0MQ
04614      END-IF.                                                      EQW9Z0MQ
04615                                                                   EQW9Z0MQ
04616      IF  (MA90C20-NB-REJETS > 1)                                  EQW9Z0MQ
04617      AND (COM-GENE-MESINF = SPACES OR LOW-VALUE)                  EQW9Z0MQ
04618           MOVE 'FBH01'  TO COM-GENE-MESINF                        EQW9Z0MQ
04619                            COM-CODERR                             EQW9Z0MQ
04620           MOVE 'O' TO W-REAF                                      EQW9Z0MQ
04621      ELSE                                                         EQW9Z0MQ
04622          IF  (MA90C20-MESINF  NOT = SPACES AND LOW-VALUE)         EQW9Z0MQ
04623          AND (COM-GENE-MESINF = SPACES OR LOW-VALUE)              EQW9Z0MQ
04624               MOVE MA90C20-MESINF TO COM-GENE-MESINF              EQW9Z0MQ
04625                                      COM-CODERR                   EQW9Z0MQ
04626               MOVE 'O' TO W-REAF                                  EQW9Z0MQ
04627          END-IF                                                   EQW9Z0MQ
04628      END-IF.                                                      EQW9Z0MQ
04629                                                                   EQW9Z0MQ
04630      IF ECR-XCDECO  = 'REJ' AND MA90C20-NB-REJETS = 0             EQW9Z0MQ
04631         MOVE 'FBH02' TO COM-GENE-MESINF                           EQW9Z0MQ
04632                         COM-CODERR                                EQW9Z0MQ
04633         MOVE 'O'     TO  W-REAF                                   EQW9Z0MQ
04634         MOVE  SPACES TO ECR-XCDECI                                EQW9Z0MQ
04635      END-IF.                                                      EQW9Z0MQ
04636 *                                                                 EQW9Z0MQ
04637  FIN-TRAITEMENT-FICHIER.                                          EQW9Z0MQ
04638      EXIT.                                                        EQW9Z0MQ
04639 *                                                                 EQW9Z0MQ
04640  MAJ-CODE-ACTION-DB2.                                             EQW9Z0MQ
04641 *--------------------                                             EQW9Z0MQ
04642                                                                   EQW9Z0MQ
04643 * CAS OU ON EST EN MODIF SUITE ‡ UN AJOUT                         EQW9Z0MQ
04644      EVALUATE COM-FB-CODE-ACTION                                  EQW9Z0MQ
04645         WHEN 'M'                                                  EQW9Z0MQ
04646               IF VEHACTC OF TS-VEHICULE(1) NOT = 'I'              EQW9Z0MQ
04647                  MOVE 'U' TO VEHACTC OF TS-VEHICULE(1)            EQW9Z0MQ
04648               END-IF                                              EQW9Z0MQ
04649         WHEN '1'                                                  EQW9Z0MQ
04650               MOVE 'I' TO VEHACTC OF TS-VEHICULE(1)               EQW9Z0MQ
04651         WHEN '2'                                                  EQW9Z0MQ
04652               MOVE 'I' TO VEHACTC OF TS-VEHICULE(1)               EQW9Z0MQ
04653         WHEN '3'                                                  EQW9Z0MQ
04654               MOVE 'I' TO VEHACTC OF TS-VEHICULE(1)               EQW9Z0MQ
04655      END-EVALUATE.                                                EQW9Z0MQ
04656                                                                   EQW9Z0MQ
04657  FIN-MAJ-CODE-ACTION-DB2.                                         EQW9Z0MQ
04658      EXIT.                                                        EQW9Z0MQ
04659 *                                                                 EQW9Z0MQ
04660 ************************************************************      EQW9Z0MQ
04661 *   STOCKAGE DES INFORMATIONS DANS LA TS                   *      EQW9Z0MQ
04662 ************************************************************      EQW9Z0MQ
04663  MISE-A-JOUR-TS.                                                  EQW9Z0MQ
04664 *---------------                                                  EQW9Z0MQ
36835 *---STATUT DU TITULAIRE CARTE GRISE                               EQW90RZ9
36835      IF ECR-VEHCGSCO NOT = SPACES                                 EQW90RZ9
36835         MOVE ECR-VEHCGSCO TO VEHCGSC OF TS-VEHICULE(1)            EQW90RZ9
36835      ELSE                                                         EQW90RZ9
36835         MOVE SPACES       TO VEHCGSC OF TS-VEHICULE(1)            EQW90RZ9
36835      END-IF.                                                      EQW90RZ9
36835                                                                   EQW90RZ9
36835 *---NOM DE LA PERSONNE TITULAIRE DE LA CARTE GRISE                EQW90RZ9
36835      IF ECR-VEHCGNCO NOT = SPACES                                 EQW90RZ9
36835         MOVE ECR-VEHCGNCO TO VEHCGNC OF TS-VEHICULE(1)            EQW90RZ9
36835      ELSE                                                         EQW90RZ9
36835         MOVE SPACES       TO VEHCGNC OF TS-VEHICULE(1)            EQW90RZ9
36835      END-IF.                                                      EQW90RZ9
36835                                                                   EQW90RZ9
36835 *---PRENOM DE LA PERSONNE TITULIARE DE LA CARTE GRISE             EQW90RZ9
36835      IF ECR-VEHCGPCO NOT = SPACES                                 EQW90RZ9
36835         MOVE ECR-VEHCGPCO TO VEHCGPC OF TS-VEHICULE(1)            EQW90RZ9
36835      ELSE                                                         EQW90RZ9
36835         MOVE SPACES       TO VEHCGPC OF TS-VEHICULE(1)            EQW90RZ9
36835      END-IF.                                                      EQW90RZ9

04665 *---CODE VEHICULE                                                 EQW9Z0MQ
04666      IF WSS-APPEL-AIDE-CDVEHI = 'N'                               EQW9Z0MQ
04667         IF ECR-VEHCODCO NOT = SPACES                              EQW9Z0MQ
04668            MOVE ECR-VEHCODCO TO VEHCODC OF TS-VEHICULE(1)         EQW9Z0MQ
04669         ELSE                                                      EQW9Z0MQ
04670            MOVE SPACES       TO VEHCODC OF TS-VEHICULE(1)         EQW9Z0MQ
04671         END-IF                                                    EQW9Z0MQ
04672      END-IF.                                                      EQW9Z0MQ
04673                                                                   EQW9Z0MQ
04674 *---TYPE VEHICULE                                                 EQW9Z0MQ
04675      MOVE ECR-VEHTYPCO       TO VEHTYPC OF TS-VEHICULE(1).        EQW9Z0MQ
04676                                                                   EQW9Z0MQ
04677 *--- GENRE DU VEHICULE                                            EQW9Z0MQ
04678      IF WSS-APPEL-AIDE-GENRE = 'N'                                EQW9Z0MQ
04679         IF ECR-VEHGENCO NOT = SPACES                              EQW9Z0MQ
04680            IF ECR-VEHGENCO NOT = VEHGENC OF TS-VEHICULE(2)        EQW9Z0MQ
04681             AND WSS-MODIF-CODE-AUTO NOT = 'O'                     EQW9Z0MQ
04682               MOVE ECR-VEHGENCO TO VEHGENC OF TS-VEHICULE(1)      EQW9Z0MQ
04683            ELSE                                                   EQW9Z0MQ
04684               MOVE SPACES       TO VEHGENC OF TS-VEHICULE(1)      EQW9Z0MQ
04685            END-IF                                                 EQW9Z0MQ
04686         ELSE                                                      EQW9Z0MQ
04687            MOVE SPACES          TO VEHGENC OF TS-VEHICULE(1)      EQW9Z0MQ
04688         END-IF                                                    EQW9Z0MQ
04689      END-IF.                                                      EQW9Z0MQ
04690                                                                   EQW9Z0MQ
04691 *---MARQUE VEHICULE                                               EQW9Z0MQ
04692      IF ECR-VEHMARLO NOT = SPACES                                 EQW9Z0MQ
04693         IF ECR-VEHMARLO NOT = VEHMARL OF TS-VEHICULE(2)           EQW9Z0MQ
04694          AND WSS-MODIF-CODE-AUTO NOT = 'O'                        EQW9Z0MQ
04695            MOVE ECR-VEHMARLO TO VEHMARL OF TS-VEHICULE(1)         EQW9Z0MQ
04696         ELSE                                                      EQW9Z0MQ
04697            MOVE SPACES       TO VEHMARL OF TS-VEHICULE(1)         EQW9Z0MQ
04698         END-IF                                                    EQW9Z0MQ
04699      ELSE                                                         EQW9Z0MQ
04700         MOVE SPACES          TO VEHMARL OF TS-VEHICULE(1)         EQW9Z0MQ
04701      END-IF.                                                      EQW9Z0MQ
04702                                                                   EQW9Z0MQ
04703 *--- MODELE                                                       EQW9Z0MQ
04704      IF ECR-VEHMODLO NOT = SPACES                                 EQW9Z0MQ
04705         IF ECR-VEHMODLO NOT = VEHMODL OF TS-VEHICULE(2)           EQW9Z0MQ
04706          AND WSS-MODIF-CODE-AUTO NOT = 'O'                        EQW9Z0MQ
04707            MOVE ECR-VEHMODLO TO VEHMODL OF TS-VEHICULE(1)         EQW9Z0MQ
04708         ELSE                                                      EQW9Z0MQ
04709            MOVE SPACES       TO VEHMODL OF TS-VEHICULE(1)         EQW9Z0MQ
04710         END-IF                                                    EQW9Z0MQ
04711      ELSE                                                         EQW9Z0MQ
04712         MOVE SPACES          TO VEHMODL OF TS-VEHICULE(1)         EQW9Z0MQ
04713      END-IF.                                                      EQW9Z0MQ
04714                                                                   EQW9Z0MQ
04715 *--- PUISSANCE/CYLINDREE                                          EQW9Z0MQ
04716      IF ECR-VEHCYLNO NOT = SPACES                                 EQW9Z0MQ
04717 *       IF ECR-VEHCYLNO NOT = VEHCYLN OF TS-VEHICULE(2)           EQW9Z0MQ
04717         IF WSS-CYL      NOT = VEHCYLN OF TS-VEHICULE(2)           EQW9Z0MQ
04718          AND WSS-MODIF-CODE-AUTO NOT = 'O'                        EQW9Z0MQ
04719            MOVE WSS-CYL      TO VEHCYLN  OF TS-VEHICULE(1)        EQW9Z0MQ
04720         ELSE                                                      EQW9Z0MQ
04721            MOVE SPACES       TO RVEHCYLN OF TS-VEHICULE(1)        EQW9Z0MQ
04722         END-IF                                                    EQW9Z0MQ
04723      ELSE                                                         EQW9Z0MQ
04724         MOVE SPACES          TO RVEHCYLN OF TS-VEHICULE(1)        EQW9Z0MQ
04725      END-IF.                                                      EQW9Z0MQ
04726                                                                   EQW9Z0MQ
04727 *---USAGE DU VEHICULE                                             EQW9Z0MQ
04728      IF ECR-VEHUSACO NOT = SPACES                                 EQW9Z0MQ
04729         MOVE ECR-VEHUSACO(1:1) TO VEHUSAC OF TS-VEHICULE(1)       EQW9Z0MQ
04730         MOVE ECR-VEHUSACO(2:2) TO VEHPROC OF TS-VEHICULE(1)       EQW9Z0MQ
04731      ELSE                                                         EQW9Z0MQ
04732         MOVE SPACES            TO VEHUSAC OF TS-VEHICULE(1)       EQW9Z0MQ
04733                                   VEHPROC OF TS-VEHICULE(1)       EQW9Z0MQ
04734      END-IF.                                                      EQW9Z0MQ
04735                                                                   EQW9Z0MQ
04736 *--- IMMATRICULATION VEHICULE                                     EQW9Z0MQ
04737      IF ECR-VEHIMMXO NOT = SPACES                                 EQW9Z0MQ
04738         MOVE ECR-VEHIMMXO    TO VEHIMMX OF TS-VEHICULE(1)         EQW9Z0MQ
04739      ELSE                                                         EQW9Z0MQ
04740         MOVE SPACES          TO VEHIMMX OF TS-VEHICULE(1)         EQW9Z0MQ
04741      END-IF.                                                      EQW9Z0MQ
04742                                                                   EQW9Z0MQ
04743 *--- DATE PREMIERE MISE EN CIRCULATION                            EQW9Z0MQ
04744      IF ECR-VEHCIRDO NOT = SPACES AND LOW-VALUE                   EQW9Z0MQ
04745         MOVE ECR-VEHCIRDO    TO WSS-JJMMSSAA(3:6)                 EQW9Z0MQ
04746         MOVE CORRESPONDING WSS-JJMMSSAA TO WSS-SSAAMMJJ           EQW9Z0MQ
04747         MOVE '01'            TO WSS-JJ OF WSS-SSAAMMJJ            EQW9Z0MQ
04748         MOVE WSS-SSAAMMJJ    TO RVEHCIRD OF TS-VEHICULE(1)        EQW9Z0MQ
04749      ELSE                                                         EQW9Z0MQ
04750         MOVE SPACES          TO RVEHCIRD OF TS-VEHICULE(1)        EQW9Z0MQ
04751      END-IF.                                                      EQW9Z0MQ
04752                                                                   EQW9Z0MQ
F3576 *--- DATE D'ACQUISITION DU VEHICULE                               EQW9Z0MQ
F3576      IF ECR-VEHACQDO NOT = SPACES AND LOW-VALUE                   EQW9Z0MQ
F3576         MOVE ECR-VEHACQDO    TO WSS-JJMMSSAA(3:6)                 EQW9Z0MQ
F3576         MOVE CORRESPONDING WSS-JJMMSSAA TO WSS-SSAAMMJJ           EQW9Z0MQ
F3576         MOVE '01'            TO WSS-JJ OF WSS-SSAAMMJJ            EQW9Z0MQ
F3576         MOVE WSS-SSAAMMJJ    TO RVEHACQD OF TS-VEHICULE(1)        EQW9Z0MQ
F3576      ELSE                                                         EQW9Z0MQ
F3576         MOVE SPACES          TO RVEHACQD OF TS-VEHICULE(1)        EQW9Z0MQ
F3576      END-IF.                                                      EQW9Z0MQ
F3576                                                                   EQW9Z0MQ
04753 *--- VALEUR A NEUF                                                EQW9Z0MQ
04754      IF ECR-VEHVALMO NOT = SPACES                                 EQW9Z0MQ
04755         MOVE WSS-VALNEUF     TO VEHVALM  OF TS-VEHICULE(1)        EQW9Z0MQ
04756      ELSE                                                         EQW9Z0MQ
04757         MOVE SPACES          TO RVEHVALM OF TS-VEHICULE(1)        EQW9Z0MQ
04758      END-IF.                                                      EQW9Z0MQ
04759                                                                   EQW9Z0MQ
04760 *---TYPE DE PROTECTION VOL                                        EQW9Z0MQ
04761      IF ECR-VEHPRTCO NOT = SPACES                                 EQW9Z0MQ
04762         IF ECR-VEHPRTCO NOT = VEHPRTC OF TS-VEHICULE(2)           EQW9Z0MQ
04763            MOVE ECR-VEHPRTCO TO VEHPRTC OF TS-VEHICULE(1)         EQW9Z0MQ
04764         ELSE                                                      EQW9Z0MQ
04765            MOVE SPACES       TO VEHPRTC OF TS-VEHICULE(1)         EQW9Z0MQ
04766         END-IF                                                    EQW9Z0MQ
04767      ELSE                                                         EQW9Z0MQ
04768         MOVE SPACES          TO VEHPRTC OF TS-VEHICULE(1)         EQW9Z0MQ
04769      END-IF.                                                      EQW9Z0MQ
04770                                                                   EQW9Z0MQ
04771 *---TYPE DE GARAGE                                                EQW9Z0MQ
04772      IF ECR-GARCODCO NOT = SPACES                                 EQW9Z0MQ
04773         MOVE ECR-GARCODCO    TO GARCODC OF TS-VEHICULE(1)         EQW9Z0MQ
04774      ELSE                                                         EQW9Z0MQ
04775         MOVE SPACES          TO GARCODC OF TS-VEHICULE(1)         EQW9Z0MQ
04776      END-IF.                                                      EQW9Z0MQ
04777                                                                   EQW9Z0MQ
04778 *---TYPE DE POSSESSION                                            EQW9Z0MQ
04779      IF ECR-VEHPOSCO NOT = SPACES                                 EQW9Z0MQ
04780         MOVE ECR-VEHPOSCO    TO VEHPOSC OF TS-VEHICULE(1)         EQW9Z0MQ
04781      ELSE                                                         EQW9Z0MQ
04782         MOVE SPACES          TO VEHPOSC OF TS-VEHICULE(1)         EQW9Z0MQ
04783      END-IF.                                                      EQW9Z0MQ
04784                                                                   EQW9Z0MQ
04785 *---INDICATEUR GARANTIE PERTE FINANCIERES                         EQW9Z0MQ
04786      IF ECR-VEHPEFCO NOT = SPACES                                 EQW9Z0MQ
04787         MOVE ECR-VEHPEFCO    TO VEHPEFC OF TS-VEHICULE(1)         EQW9Z0MQ
04788      ELSE                                                         EQW9Z0MQ
04789         MOVE SPACES          TO VEHPEFC OF TS-VEHICULE(1)         EQW9Z0MQ
04790      END-IF.                                                      EQW9Z0MQ
04791                                                                   EQW9Z0MQ
04792 *--- CODE POSTAL LIEU DE GARAGE                                   EQW9Z0MQ
04793      IF WSS-APPEL-AIDE-CP = 'N'                                   EQW9Z0MQ
04794         IF ECR-GARCOPCO NOT = SPACES                              EQW9Z0MQ
04795            MOVE ECR-GARCOPCO TO GARCOPC OF TS-VEHICULE(1)         EQW9Z0MQ
04796         ELSE                                                      EQW9Z0MQ
04797            MOVE SPACES       TO GARCOPC OF TS-VEHICULE(1)         EQW9Z0MQ
04798         END-IF                                                    EQW9Z0MQ
04799      END-IF.                                                      EQW9Z0MQ
04800                                                                   EQW9Z0MQ
04801 *--- COMMUNE DU LIEU DE GARAGE                                    EQW9Z0MQ
04802      IF WSS-APPEL-AIDE-COM = 'N'                                  EQW9Z0MQ
04803         IF ECR-GARVILLO NOT = SPACES                              EQW9Z0MQ
04804            MOVE ECR-GARVILLO TO GARVILL OF TS-VEHICULE(1)         EQW9Z0MQ
04805         ELSE                                                      EQW9Z0MQ
04806            MOVE SPACES       TO GARVILL OF TS-VEHICULE(1)         EQW9Z0MQ
04807         END-IF                                                    EQW9Z0MQ
04808      END-IF.                                                      EQW9Z0MQ
04809                                                                   EQW9Z0MQ
04810 *--- CODE INSEE DE LA COMMUNE DU LIEU DE GARAGE                   EQW9Z0MQ
04811 ***  MOVE WSS-CODE-INSEE     TO GARINSC OF TS-VEHICULE(2).        EQW9Z0MQ
      * ALIM ITEM1 CAR L'ITEM 1 N'EST MIS ‡ JOUR ‡ PARTIR DE L'ITEM 2
      * QUE LOSQU'IL EST ‡ BLANC, DONC EN AN LA 1ERE FOIS UNIQUEMENT
04811      MOVE WSS-CODE-INSEE     TO GARINSC OF TS-VEHICULE(1).        EQW9Z0MQ
04812                                                                   EQW9Z0MQ
04813 *--- ZONE DE GARAGE (RC ET VOL)                                   EQW9Z0MQ
04814      IF WSS-CODE-INSEE NOT = SPACES AND LOW-VALUE                 EQW9Z0MQ
04815         PERFORM RECHERCHE-ZONE      THRU FIN-RECHERCHE-ZONE       EQW9Z0MQ
04816         IF OK-ZONIER = 'O'                                        EQW9Z0MQ
04817            MOVE ZONERCC OF FOZONE01 TO WSS-ZONE-RC-VOL(1:1)       EQW9Z0MQ
04818            MOVE ZONVOLC OF FOZONE01 TO WSS-ZONE-RC-VOL(2:1)       EQW9Z0MQ
04819            MOVE WSS-ZONE-RC-VOL     TO GARZONC OF TS-VEHICULE(1)  EQW9Z0MQ
04820         END-IF                                                    EQW9Z0MQ
04821      ELSE                                                         EQW9Z0MQ
04822         MOVE SPACES                 TO GARZONC OF TS-VEHICULE(1)  EQW9Z0MQ
04823      END-IF.                                                      EQW9Z0MQ
04824                                                                   EQW9Z0MQ
04825 *---INDICATEUR VEHICULE SUPPLEMENTAIRE AU FOYER                   EQW9Z0MQ
04826      IF ECR-ANVREPCO NOT = SPACES                                 EQW9Z0MQ
04827         MOVE ECR-ANVREPCO    TO ANVREPC OF TS-VEHICULE(1)         EQW9Z0MQ
04828      ELSE                                                         EQW9Z0MQ
04829         MOVE SPACES          TO ANVREPC OF TS-VEHICULE(1)         EQW9Z0MQ
04830      END-IF.                                                      EQW9Z0MQ
04831                                                                   EQW9Z0MQ
04832 *---FORMULE                                                       EQW9Z0MQ
04833      IF ECR-VEHFORCO NOT = SPACES                                 EQW9Z0MQ
04834         MOVE ECR-VEHFORCO    TO VEHFORC OF TS-VEHICULE(1)         EQW9Z0MQ
04835      ELSE                                                         EQW9Z0MQ
04836         MOVE SPACES          TO VEHFORC OF TS-VEHICULE(1)         EQW9Z0MQ
04837      END-IF.                                                      EQW9Z0MQ
04838                                                                   EQW9Z0MQ
04839 *---NUMERO DE CONTRAT PRECEDENT                                   EQW9Z0MQ
04840      IF ECR-ANVNUMXO NOT = SPACES                                 EQW9Z0MQ
04841         MOVE ECR-ANVNUMXO TO ANVNUMX OF TS-VEHICULE(1)            EQW9Z0MQ
04842      ELSE                                                         EQW9Z0MQ
04843         MOVE SPACES       TO ANVNUMX OF TS-VEHICULE(1)            EQW9Z0MQ
04844      END-IF.                                                      EQW9Z0MQ
04845                                                                   EQW9Z0MQ
04839 *---ANCIEN SOUSCRIPTEUR DU CONTRAT PRECEDENT                      EQW9Z0MQ
           MOVE SPACES       TO RANVSOUS OF TS-VEHICULE(1).
04840      IF ECR-ANVSOUSO NOT = SPACES AND LOW-VALUE                   EQW9Z0MQ
              IF ECR-ANVSOUSO NOT = 'ENAP'
                 SET PERSONNE-NON-TROUVE TO TRUE
                 PERFORM READ-TS-TECHNIQUE THRU FIN-READ-TS-TECHNIQUE
                 PERFORM VARYING I-RANG FROM 1 BY 1
                         UNTIL I-RANG > 40
                         OR TAB-PERS(I-RANG) = (SPACES OR LOW-VALUE)
                         OR PERSONNE-TROUVE
                    MOVE PERRANTS OF TS-TECHNIQUE(I-RANG) TO
                                                    COM-FB-RANG-TSPERS
                    PERFORM READ-TS-PERSONNE THRU FIN-READ-TS-PERSONNE
                    IF (PERSTAC OF TS-PERSONNE(1) = ECR-ANVSOUSO(1:2))
                    OR (PERSTAC OF TS-PERSONNE(1) = 'PM' AND
                                               ECR-ANVSOUSO = 'SOUS')
                       IF PERORDX OF TS-PERSONNE(1) NOT = SPACES AND
                                                          LOW-VALUE
U3925                     IF PERSORD OF TS-PERSONNE(1) = '99999999'
U3925                     OR INF-NATMVT OF TS-SUSPENS1 = 'AN'           00588800
U3925                        MOVE PERORDX OF TS-PERSONNE(1) TO
U3925                                     ANVSOUS OF TS-VEHICULE(1)
U3925                        SET PERSONNE-TROUVE TO TRUE
U3925                     END-IF
                       ELSE
                          IF PERORDX OF TS-PERSONNE(2) NOT = SPACES
                                                       AND LOW-VALUE
U3925                       IF PERSORD OF TS-PERSONNE(2) = '99999999'
U3925                       OR INF-NATMVT OF TS-SUSPENS1 = 'AN'         00589600
U3925                       OR ((INF-CODE-CONV OF TS-SUSPENS1(7:2)      00588800
                                 = 'TR')
U3925                       AND (PERSORD OF TS-PERSONNE(2) = LOW-VALUE))
U3925                           MOVE PERORDX OF TS-PERSONNE(2) TO
                                             ANVSOUS OF TS-VEHICULE(1)
U3925                           SET PERSONNE-TROUVE TO TRUE
U3925                        END-IF
                          END-IF
                       END-IF
                    END-IF
                 END-PERFORM
04850         ELSE                                                      EQW9Z0MQ
                 IF ECR-ANVSOUNO NOT = SPACES AND LOW-VALUE
                    MOVE ECR-ANVSOUNO TO ANVSOUS OF TS-VEHICULE(1)
                 END-IF
04852         END-IF                                                    EQW9Z0MQ
04844      END-IF.                                                      EQW9Z0MQ
04845                                                                   EQW9Z0MQ
04846 *---CODE ANCIENNE COMPAGNIE                                       EQW9Z0MQ
04848      IF ECR-ANVCIEXO NOT = SPACES                                 EQW9Z0MQ
04849         MOVE ECR-ANVCIEXO TO ANVCIEX OF TS-VEHICULE(1)            EQW9Z0MQ
04850      ELSE                                                         EQW9Z0MQ
04851         MOVE SPACES       TO ANVCIEX OF TS-VEHICULE(1)            EQW9Z0MQ
04853      END-IF.                                                      EQW9Z0MQ
04854                                                                   EQW9Z0MQ
04855 *---LIBELLE ANCIENNE COMPAGNIE                                    EQW9Z0MQ
04856      IF  ECR-ANVCIEXO = '999'                                     EQW9Z0MQ
04857      AND MODIF-CIE-OUI                                            EQW9Z0MQ
04858          MOVE SPACES          TO ANVCIEL OF TS-VEHICULE(1)        EQW9Z0MQ
04859          MOVE SPACES          TO ANVCIEL OF TS-VEHICULE(2)        EQW9Z0MQ
04860          IF MODIF-LIB-NON                                         EQW9Z0MQ
04861             MOVE SPACES       TO ECR-ANVCIELO                     EQW9Z0MQ
04862          END-IF                                                   EQW9Z0MQ
04863      END-IF.                                                      EQW9Z0MQ
04864      IF ECR-ANVCIELO NOT = SPACES                                 EQW9Z0MQ
04865         IF ECR-ANVCIELO NOT = ANVCIEL OF TS-VEHICULE(2)           EQW9Z0MQ
04866          AND WSS-MODIF-CODE-CIE NOT = 'O'                         EQW9Z0MQ
04867            MOVE ECR-ANVCIELO TO ANVCIEL OF TS-VEHICULE(1)         EQW9Z0MQ
04868         ELSE                                                      EQW9Z0MQ
04869            MOVE SPACES       TO ANVCIEL OF TS-VEHICULE(1)         EQW9Z0MQ
04870         END-IF                                                    EQW9Z0MQ
04871      ELSE                                                         EQW9Z0MQ
04872         MOVE SPACES          TO ANVCIEL OF TS-VEHICULE(1)         EQW9Z0MQ
04873      END-IF.                                                      EQW9Z0MQ
04874                                                                   EQW9Z0MQ
04875 *---NOMBRE DE MOIS D'ASSURANCE DU CONTRAT PRECEDENT               EQW9Z0MQ
04876      IF ECR-ANVANCNO NOT = SPACES AND LOW-VALUE                   EQW9Z0MQ
04877         MOVE WSS-NBR         TO ANVANCN  OF TS-VEHICULE(1)        EQW9Z0MQ
04878      ELSE                                                         EQW9Z0MQ
04879         MOVE SPACES          TO RANVANCN OF TS-VEHICULE(1)        EQW9Z0MQ
04880      END-IF.                                                      EQW9Z0MQ
04881                                                                   EQW9Z0MQ
04882 *---NB DE MOIS D'INTERRUPTION                                     EQW9Z0MQ
04883      IF ECR-ANVINTNO NOT = SPACES                                 EQW9Z0MQ
04884         MOVE WSS-NBMOIS-INTER TO ANVINTN  OF TS-VEHICULE(1)       EQW9Z0MQ
04885      ELSE                                                         EQW9Z0MQ
04886         MOVE SPACES           TO RANVINTN OF TS-VEHICULE(1)       EQW9Z0MQ
04887      END-IF.                                                      EQW9Z0MQ
04888                                                                   EQW9Z0MQ
04889 *---DATE DE RESILIATION CONTRAT PRECEDENT                         EQW9Z0MQ
04890      IF ECR-ANVRESDO NOT = SPACES AND LOW-VALUE                   EQW9Z0MQ
04891         MOVE ECR-ANVRESDO    TO WSS-JJMMSSAA(3:6)                 EQW9Z0MQ
04892         MOVE CORRESPONDING WSS-JJMMSSAA TO WSS-SSAAMMJJ           EQW9Z0MQ
04893         MOVE '01'            TO WSS-JJ   OF WSS-SSAAMMJJ          EQW9Z0MQ
04894         MOVE WSS-SSAAMMJJ    TO ANVRESD  OF TS-VEHICULE(1)        EQW9Z0MQ
04895      ELSE                                                         EQW9Z0MQ
04896         MOVE SPACES          TO RANVRESD OF TS-VEHICULE(1)        EQW9Z0MQ
04897      END-IF.                                                      EQW9Z0MQ
04898                                                                   EQW9Z0MQ
      *---MOTIF DE RESILIATION CONTRAT PRECEDENT
           IF ECR-ANVMTRCO NOT = SPACES
              MOVE ECR-ANVMTRCO TO ANVMTRC OF TS-VEHICULE(1)
           ELSE
              MOVE SPACES       TO ANVMTRC OF TS-VEHICULE(1)
           END-IF.

04899 *---INDICATEUR DE PRESENCE DE SINISTRES                           EQW9Z0MQ
04900      IF ECR-SIVINDCO NOT = SPACES                                 EQW9Z0MQ
04901         MOVE ECR-SIVINDCO TO SIVINDC OF TS-VEHICULE(1)            EQW9Z0MQ
04902         IF ECR-SIVINDCO = 'N'                                     EQW9Z0MQ
04903            MOVE SPACES    TO SINISTRE-VEH OF TS-VEHICULE(1)       EQW9Z0MQ
04904            MOVE SPACES    TO RANVSINN OF TS-VEHICULE(1)           EQW9Z0MQ
04905         END-IF                                                    EQW9Z0MQ
04906      ELSE                                                         EQW9Z0MQ
04907         MOVE SPACES       TO SIVINDC OF TS-VEHICULE(1)            EQW9Z0MQ
04908      END-IF.                                                      EQW9Z0MQ
04909                                                                   EQW9Z0MQ
04910 *---ANCIEN COEFFICIENT DE REDUCTION-MAJORATION                    EQW9Z0MQ
04911      IF ECR-ANVBONTO NOT = SPACES                                 EQW9Z0MQ
04912         COMPUTE ANVBONT OF TS-VEHICULE(1) = WSS-CRM / 100         EQW9Z0MQ
04913      ELSE                                                         EQW9Z0MQ
04914          MOVE RANVBONT OF TS-VEHICULE(2)                          EQW9Z0MQ
04915                              TO RANVBONT OF TS-VEHICULE(1)        EQW9Z0MQ
04916 *        MOVE SPACES         TO RANVBONT OF TS-VEHICULE(1)        EQW9Z0MQ
04917      END-IF.                                                      EQW9Z0MQ
04918                                                                   EQW9Z0MQ
04919 *---DATE D'ACQUISITION DU COEFFICIENT DE REDUCTION-MAJORATION     EQW9Z0MQ
04920      IF ECR-ANVBONDO NOT = SPACES AND LOW-VALUE                   EQW9Z0MQ
04921         MOVE ECR-ANVBONDO    TO WSS-JJMMSSAA(3:6)                 EQW9Z0MQ
04922         MOVE CORRESPONDING WSS-JJMMSSAA TO WSS-SSAAMMJJ           EQW9Z0MQ
04923         MOVE '01'            TO WSS-JJ   OF WSS-SSAAMMJJ          EQW9Z0MQ
04924         MOVE WSS-SSAAMMJJ    TO RANVBOND OF TS-VEHICULE(1)        EQW9Z0MQ
04925      ELSE                                                         EQW9Z0MQ
04926         MOVE RANVBOND OF TS-VEHICULE(2)                           EQW9Z0MQ
04927                              TO RANVBOND OF TS-VEHICULE(1)        EQW9Z0MQ
04928 *       MOVE SPACES          TO RANVBOND OF TS-VEHICULE(1)        EQW9Z0MQ
04929      END-IF.                                                      EQW9Z0MQ
04930                                                                   EQW9Z0MQ
04931 *---DATE D'ACQUISITION DU CRM = 050                               EQW9Z0MQ
04932      IF ECR-ANVABODO NOT = SPACES AND LOW-VALUE                   EQW9Z0MQ
04933         MOVE ECR-ANVABODO    TO WSS-JJMMSSAA(5:4)                 EQW9Z0MQ
04934         MOVE CORRESPONDING WSS-JJMMSSAA TO WSS-SSAAMMJJ           EQW9Z0MQ
04935         MOVE '01'            TO WSS-JJ   OF WSS-SSAAMMJJ          EQW9Z0MQ
04936         MOVE '01'            TO WSS-MM   OF WSS-SSAAMMJJ          EQW9Z0MQ
04937         MOVE WSS-SSAAMMJJ    TO RANVABOD OF TS-VEHICULE(1)        EQW9Z0MQ
04938      ELSE                                                         EQW9Z0MQ
04939 *       MOVE RANVABOD OF TS-VEHICULE(2)                           EQW9Z0MQ
04940 *                            TO RANVABOD OF TS-VEHICULE(1)        EQW9Z0MQ
04941         MOVE SPACES          TO RANVABOD OF TS-VEHICULE(1)        EQW9Z0MQ
04942      END-IF.                                                      EQW9Z0MQ
04943                                                                   EQW9Z0MQ
04944      MOVE FBMISPTR-IT1           TO SEGTRA OF TS-SUSPENS1.        EQW9Z0MQ
04945                                                                   EQW9Z0MQ
04946 *--- RENSEIGNEMENT CODES GARANTIES SELON FORMULE (OCCURS 2)       EQW9Z0MQ
04947      IF WSS-APPEL-AIDE-FORM = 'N'                                 EQW9Z0MQ
04948         IF ECR-VEHFORCO NOT = SPACES AND LOW-VALUE AND 'V '       EQW9Z0MQ
04949                                                    AND 'M '       EQW9Z0MQ
04950            PERFORM PRE-AFFICHAGE-GTIE THRU FIN-PRE-AFFICHAGE-GTIE EQW9Z0MQ
04951         END-IF                                                    EQW9Z0MQ
04952      END-IF.                                                      EQW9Z0MQ
04953                                                                   EQW9Z0MQ
04954  FIN-MISE-A-JOUR-TS.                                              EQW9Z0MQ
04955      EXIT.                                                        EQW9Z0MQ
04956 *                                                                 EQW9Z0MQ
04957 ******************************************************************EQW9Z0MQ
04958 * PRE-AFFICHAGE GTI EN AJOUT VEHICULE A PARTIR DES TABLES FBFOGA01EQW9Z0MQ
F41702* FB2FRA01, FB4FRM01, ET FBMTLT01 POUR RENSEIGNER LA ZONE         EQW9Z0MQ
04960 * GARANTIE OF TS-VEHICULE(2)                                      EQW9Z0MQ
04961 * CE PARAGRAPHE N'EST EFFECTUÈ QU'UNE SEULE FOIS TS-VEHICULE(2)   EQW9Z0MQ
04962 ******************************************************************EQW9Z0MQ
04963  PRE-AFFICHAGE-GTIE.                                              EQW9Z0MQ
04964 *-------------------                                              EQW9Z0MQ
04965      MOVE SPACES TO GARANTIE-VEH OF TS-VEHICULE(2).               EQW9Z0MQ
04966      MOVE ZERO   TO IND-GTI2.                                     EQW9Z0MQ
04967                                                                   EQW9Z0MQ
04968 *---ASSISTANCE                                                    EQW9Z0MQ
04969      MOVE 'ASB'           TO   WSS-CODE-GTI.                      EQW9Z0MQ
04970      PERFORM ACCES-FBFOGA THRU FIN-ACCES-FBFOGA.                  EQW9Z0MQ
04971      IF WSS-GTI-TROUVE = 'N'                                      EQW9Z0MQ
04972         MOVE 'FB178' TO COM-GENE-MESANO                           EQW9Z0MQ
04973                         COM-CODERR                                EQW9Z0MQ
04974         MOVE SPACES  TO WSS-FLAG-GTI                              EQW9Z0MQ
04975      ELSE                                                         EQW9Z0MQ
04976         IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE         EQW9Z0MQ
04977            MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI               EQW9Z0MQ
04978         ELSE                                                      EQW9Z0MQ
04979            IF VEHTYPC OF TS-VEHICULE(1) = '2R '                   EQW9Z0MQ
04980               IF RVEHCYLN OF TS-VEHICULE(1) NOT = SPACES          EQW9Z0MQ
04981                                              AND LOW-VALUE        EQW9Z0MQ
04982                  IF VEHCYLN OF TS-VEHICULE(1) <= 125              EQW9Z0MQ
04983                     MOVE 'N' TO WSS-FLAG-GTI                      EQW9Z0MQ
04984                  ELSE                                             EQW9Z0MQ
F2931                     MOVE SPACES TO WSS-FLAG-GTI                   00506000
04986                  END-IF                                           EQW9Z0MQ
04987               ELSE                                                EQW9Z0MQ
04988                  IF RVEHCYLN OF TS-VEHICULE(2) NOT = SPACES       EQW9Z0MQ
04989                                                 AND LOW-VALUE     EQW9Z0MQ
04990                     IF VEHCYLN OF TS-VEHICULE(2) <= 125           EQW9Z0MQ
04991                        MOVE 'N' TO WSS-FLAG-GTI                   EQW9Z0MQ
04992                     ELSE                                          EQW9Z0MQ
F2931                        MOVE SPACES TO WSS-FLAG-GTI                00506800
04994                     END-IF                                        EQW9Z0MQ
04995                  END-IF                                           EQW9Z0MQ
04996               END-IF                                              EQW9Z0MQ
04997            ELSE                                                   EQW9Z0MQ
04998               MOVE SPACES              TO WSS-FLAG-GTI            EQW9Z0MQ
04999            END-IF                                                 EQW9Z0MQ
05000         END-IF                                                    EQW9Z0MQ
05001      END-IF.                                                      EQW9Z0MQ
05002      PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2.          EQW9Z0MQ
05003                                                                   EQW9Z0MQ
05004 *---VEHICULE DE REMPLACEMENT 'ASE' ENTREPRISE                     EQW9Z0MQ
05005      IF VEHTYPC OF TS-VEHICULE(1) = '4R ' OR 'CC '                EQW9Z0MQ
05006         MOVE 'ASE'                     TO   WSS-CODE-GTI          EQW9Z0MQ
05007         PERFORM ACCES-FBFOGA THRU FIN-ACCES-FBFOGA                EQW9Z0MQ
05008         IF WSS-GTI-TROUVE = 'N'                                   EQW9Z0MQ
05009            MOVE 'FB178' TO COM-GENE-MESANO                        EQW9Z0MQ
05010                            COM-CODERR                             EQW9Z0MQ
05011            MOVE SPACES  TO WSS-FLAG-GTI                           EQW9Z0MQ
05012         ELSE                                                      EQW9Z0MQ
05013            IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
05014               IF FOGPAFC OF FBFOGA01 = 'O' AND                    EQW9Z0MQ
05015                                            COM-FB-NBRE-PERS-PM < 1EQW9Z0MQ
05016                  MOVE 'N'                 TO WSS-FLAG-GTI         EQW9Z0MQ
05017               ELSE                                                EQW9Z0MQ
05018                  MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI         EQW9Z0MQ
05019               END-IF                                              EQW9Z0MQ
05020            ELSE                                                   EQW9Z0MQ
05021               MOVE SPACES              TO WSS-FLAG-GTI            EQW9Z0MQ
05022            END-IF                                                 EQW9Z0MQ
05023         END-IF                                                    EQW9Z0MQ
05024         PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2        EQW9Z0MQ
05025      END-IF.                                                      EQW9Z0MQ
05026                                                                   EQW9Z0MQ
05027 *---VEHICULE DE REMPLACEMENT 'ASP' PARTICULIER                    EQW9Z0MQ
05028      IF VEHTYPC OF TS-VEHICULE(1) = '4R ' OR 'CC '                EQW9Z0MQ
05029         MOVE 'ASP'                     TO   WSS-CODE-GTI          EQW9Z0MQ
05030         PERFORM ACCES-FBFOGA THRU FIN-ACCES-FBFOGA                EQW9Z0MQ
05031         IF WSS-GTI-TROUVE = 'N'                                   EQW9Z0MQ
05032            MOVE 'FB178' TO COM-GENE-MESANO                        EQW9Z0MQ
05033                            COM-CODERR                             EQW9Z0MQ
05034            MOVE SPACES  TO WSS-FLAG-GTI                           EQW9Z0MQ
05035         ELSE                                                      EQW9Z0MQ
05036            IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
05037               MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI            EQW9Z0MQ
05038            ELSE                                                   EQW9Z0MQ
05039               MOVE SPACES              TO WSS-FLAG-GTI            EQW9Z0MQ
05040            END-IF                                                 EQW9Z0MQ
05041         END-IF                                                    EQW9Z0MQ
05042         PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2        EQW9Z0MQ
05043      END-IF.                                                      EQW9Z0MQ
05044                                                                   EQW9Z0MQ
05045 *---GARANTIE RC                                                   EQW9Z0MQ
05046      MOVE 'RC '                     TO   WSS-CODE-GTI.            EQW9Z0MQ
05047      PERFORM ACCES-FBFOGA           THRU FIN-ACCES-FBFOGA.        EQW9Z0MQ
05048      IF WSS-GTI-TROUVE = 'N'                                      EQW9Z0MQ
05049         MOVE 'FB178' TO COM-GENE-MESANO                           EQW9Z0MQ
05050                         COM-CODERR                                EQW9Z0MQ
05051         MOVE SPACES  TO WSS-FLAG-GTI                              EQW9Z0MQ
05052      ELSE                                                         EQW9Z0MQ
05053         IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE         EQW9Z0MQ
05054            MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI               EQW9Z0MQ
05055         ELSE                                                      EQW9Z0MQ
05056            MOVE SPACES              TO WSS-FLAG-GTI               EQW9Z0MQ
05057         END-IF                                                    EQW9Z0MQ
05058      END-IF.                                                      EQW9Z0MQ
05059      PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2.          EQW9Z0MQ
05060                                                                   EQW9Z0MQ
F36835*---GARANTIE CONDUCTEUR CND                                       EQW9Z0MQ
05062      MOVE 'CND'                     TO   WSS-CODE-GTI.            EQW9Z0MQ
05063      IF VEHTYPC OF TS-VEHICULE(1) = '2R '                         EQW9Z0MQ
05064         IF RVEHCYLN OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
05065            IF VEHCYLN OF TS-VEHICULE(1) < 80                      EQW9Z0MQ
05066               MOVE 'N' TO WSS-FLAG-GTI                            EQW9Z0MQ
05067            ELSE                                                   EQW9Z0MQ
05068               PERFORM ACCES-FBFOGA  THRU FIN-ACCES-FBFOGA         EQW9Z0MQ
05069               IF WSS-GTI-TROUVE = 'N'                             EQW9Z0MQ
05070                  MOVE 'FB178' TO COM-GENE-MESANO                  EQW9Z0MQ
05071                                  COM-CODERR                       EQW9Z0MQ
05072                  MOVE SPACES  TO WSS-FLAG-GTI                     EQW9Z0MQ
05073               ELSE                                                EQW9Z0MQ
05074                  IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUEEQW9Z0MQ
05075                     MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI      EQW9Z0MQ
05076                  ELSE                                             EQW9Z0MQ
05077                     MOVE SPACES              TO WSS-FLAG-GTI      EQW9Z0MQ
05078                  END-IF                                           EQW9Z0MQ
05079               END-IF                                              EQW9Z0MQ
05080            END-IF                                                 EQW9Z0MQ
05081         ELSE                                                      EQW9Z0MQ
05082           IF RVEHCYLN OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUEEQW9Z0MQ
05083              IF VEHCYLN OF TS-VEHICULE(2) < 80                    EQW9Z0MQ
05084                 MOVE 'N' TO WSS-FLAG-GTI                          EQW9Z0MQ
05085              ELSE                                                 EQW9Z0MQ
05086                 PERFORM ACCES-FBFOGA  THRU FIN-ACCES-FBFOGA       EQW9Z0MQ
05087                 IF WSS-GTI-TROUVE = 'N'                           EQW9Z0MQ
05088                    MOVE 'FB178' TO COM-GENE-MESANO                EQW9Z0MQ
05089                                    COM-CODERR                     EQW9Z0MQ
05090                    MOVE SPACES  TO WSS-FLAG-GTI                   EQW9Z0MQ
05091                 ELSE                                              EQW9Z0MQ
05092                    IF FOGPAFC OF FBFOGA01 NOT = SPACES            EQW9Z0MQ
05093                                              AND LOW-VALUE        EQW9Z0MQ
05094                       MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI    EQW9Z0MQ
05095                    ELSE                                           EQW9Z0MQ
05096                       MOVE SPACES              TO WSS-FLAG-GTI    EQW9Z0MQ
05097                    END-IF                                         EQW9Z0MQ
05098                 END-IF                                            EQW9Z0MQ
05099              END-IF                                               EQW9Z0MQ
05100           END-IF                                                  EQW9Z0MQ
05101         END-IF                                                    EQW9Z0MQ
05102      ELSE                                                         EQW9Z0MQ
05103         PERFORM ACCES-FBFOGA           THRU FIN-ACCES-FBFOGA      EQW9Z0MQ
05104         IF WSS-GTI-TROUVE = 'N'                                   EQW9Z0MQ
05105            MOVE 'FB178' TO COM-GENE-MESANO                        EQW9Z0MQ
05106                            COM-CODERR                             EQW9Z0MQ
05107            MOVE SPACES  TO WSS-FLAG-GTI                           EQW9Z0MQ
05108         ELSE                                                      EQW9Z0MQ
05109            IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
05110               MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI            EQW9Z0MQ
05111            ELSE                                                   EQW9Z0MQ
05112               MOVE SPACES              TO WSS-FLAG-GTI            EQW9Z0MQ
05113            END-IF                                                 EQW9Z0MQ
05114         END-IF                                                    EQW9Z0MQ
05115      END-IF.                                                      EQW9Z0MQ
05116      PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2.          EQW9Z0MQ
05117      IF GTIFLGC OF TS-VEHICULE(2, IND-GTI2) = 'O' OR SPACE        EQW9Z0MQ
05118         PERFORM ACCES-FBMTLT        THRU FIN-ACCES-FBMTLT         EQW9Z0MQ
05119         IF WSS-MONTANT-TROUVE = 'N'                               EQW9Z0MQ
05120            MOVE 'FB183' TO COM-GENE-MESANO                        EQW9Z0MQ
05121                            COM-CODERR                             EQW9Z0MQ
05122            MOVE SPACES  TO RGTIMONM    OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05123            MOVE SPACES  TO RGTIFRTM    OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05124         ELSE                                                      EQW9Z0MQ
05125            MOVE MTLSAIM OF FBMTLT01                               EQW9Z0MQ
05126                            TO  GTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05127            IF VEHTYPC OF TS-VEHICULE(1) = '2R '                   EQW9Z0MQ
05128               MOVE MTLHABM OF FBMTLT01                            EQW9Z0MQ
05129                            TO  GTIFRTM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05130            END-IF                                                 EQW9Z0MQ
05131         END-IF                                                    EQW9Z0MQ
05132      ELSE                                                         EQW9Z0MQ
05133         MOVE SPACES        TO RGTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05134         MOVE SPACES        TO RGTIFRTM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05135      END-IF.                                                      EQW9Z0MQ
05136                                                                   EQW9Z0MQ
05060                                                                   EQW9Z0MQ
F36835*---GARANTIE CONDUCTEUR CNF                                       EQW9Z0MQ
F36835     IF VEHTYPC OF TS-VEHICULE(1) = '4R ' OR 'CC '                EQW9Z0MQ
F36835        MOVE 'CNF'                     TO   WSS-CODE-GTI          EQW9Z0MQ
F36835        PERFORM ACCES-FBFOGA           THRU FIN-ACCES-FBFOGA      EQW9Z0MQ
F36835        IF WSS-GTI-TROUVE = 'N'                                   EQW9Z0MQ
F36835           MOVE 'FB178' TO COM-GENE-MESANO                        EQW9Z0MQ
F36835                           COM-CODERR                             EQW9Z0MQ
F36835           MOVE SPACES  TO WSS-FLAG-GTI                           EQW9Z0MQ
F36835        ELSE                                                      EQW9Z0MQ
F36835           IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
F36835              MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI            EQW9Z0MQ
F36835           ELSE                                                   EQW9Z0MQ
F36835              MOVE SPACES              TO WSS-FLAG-GTI            EQW9Z0MQ
F36835           END-IF                                                 EQW9Z0MQ
F36835        END-IF                                                    EQW9Z0MQ
F36835     END-IF.                                                      EQW9Z0MQ
F36835     PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2.          EQW9Z0MQ
F36835     IF GTIFLGC OF TS-VEHICULE(2, IND-GTI2) = 'O' OR SPACE        EQW9Z0MQ
F36835        PERFORM ACCES-FBMTLT        THRU FIN-ACCES-FBMTLT         EQW9Z0MQ
F36835        IF WSS-MONTANT-TROUVE = 'N'                               EQW9Z0MQ
F36835           MOVE 'FB183' TO COM-GENE-MESANO                        EQW9Z0MQ
F36835                           COM-CODERR                             EQW9Z0MQ
F36835           MOVE SPACES  TO RGTIMONM    OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
F36835           MOVE SPACES  TO RGTIFRTM    OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
F36835        ELSE                                                      EQW9Z0MQ
F36835           MOVE MTLSAIM OF FBMTLT01                               EQW9Z0MQ
F36835                           TO  GTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
F36835           MOVE MTLHABM OF FBMTLT01                               EQW9Z0MQ
F36835                           TO  GTIFRTM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
F36835        END-IF                                                    EQW9Z0MQ
F36835     ELSE                                                         EQW9Z0MQ
F36835        MOVE SPACES        TO RGTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
F36835        MOVE SPACES        TO RGTIFRTM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
F36835     END-IF.                                                      EQW9Z0MQ
F36835                                                                  EQW9Z0MQ
05137 *---EVENEMENTS MAJEURS                                            EQW9Z0MQ
05138 *(ZONE NON MODIF MAIS RENSEIGNEE ICI CAR DEPEND DE DONNEE SAISIE)*EQW9Z0MQ
05139      MOVE 'EVM'                     TO   WSS-CODE-GTI.            EQW9Z0MQ
05140      PERFORM ACCES-FBFOGA           THRU FIN-ACCES-FBFOGA.        EQW9Z0MQ
05141      IF WSS-GTI-TROUVE = 'N'                                      EQW9Z0MQ
05142         MOVE 'FB178' TO COM-GENE-MESANO                           EQW9Z0MQ
05143                         COM-CODERR                                EQW9Z0MQ
05144         MOVE SPACES  TO WSS-FLAG-GTI                              EQW9Z0MQ
05145      ELSE                                                         EQW9Z0MQ
05146         IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE         EQW9Z0MQ
05147            MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI               EQW9Z0MQ
05148         ELSE                                                      EQW9Z0MQ
05149            MOVE SPACES                 TO WSS-FLAG-GTI            EQW9Z0MQ
05150         END-IF                                                    EQW9Z0MQ
05151      END-IF.                                                      EQW9Z0MQ
05152      PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2.          EQW9Z0MQ
05153                                                                   EQW9Z0MQ
05154 *---RECOURS AMIABLE ET JUDICIAIRE                                 EQW9Z0MQ
05155      MOVE 'RAJ'                     TO   WSS-CODE-GTI.            EQW9Z0MQ
05156      PERFORM ACCES-FBFOGA           THRU FIN-ACCES-FBFOGA.        EQW9Z0MQ
05157      IF WSS-GTI-TROUVE = 'N'                                      EQW9Z0MQ
05158         MOVE 'FB178' TO COM-GENE-MESANO                           EQW9Z0MQ
05159                         COM-CODERR                                EQW9Z0MQ
05160         MOVE SPACES  TO WSS-FLAG-GTI                              EQW9Z0MQ
05161      ELSE                                                         EQW9Z0MQ
05162         IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE         EQW9Z0MQ
05163            MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI               EQW9Z0MQ
05164         ELSE                                                      EQW9Z0MQ
05165            MOVE SPACES              TO WSS-FLAG-GTI               EQW9Z0MQ
05166         END-IF                                                    EQW9Z0MQ
05167      END-IF.                                                      EQW9Z0MQ
05168      PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2.          EQW9Z0MQ
05169                                                                   EQW9Z0MQ
05170 *---GARANTIE INCENDIE                                             EQW9Z0MQ
05171      MOVE 'INC'                     TO   WSS-CODE-GTI.            EQW9Z0MQ
05172      PERFORM ACCES-FBFOGA           THRU FIN-ACCES-FBFOGA.        EQW9Z0MQ
05173      IF WSS-GTI-TROUVE = 'N'                                      EQW9Z0MQ
05174         MOVE 'FB178' TO COM-GENE-MESANO                           EQW9Z0MQ
05175                         COM-CODERR                                EQW9Z0MQ
05176         MOVE SPACES  TO WSS-FLAG-GTI                              EQW9Z0MQ
05177      ELSE                                                         EQW9Z0MQ
05178         IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE         EQW9Z0MQ
05179            MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI               EQW9Z0MQ
05180         ELSE                                                      EQW9Z0MQ
05181            MOVE SPACES              TO WSS-FLAG-GTI               EQW9Z0MQ
05182         END-IF                                                    EQW9Z0MQ
05183      END-IF.                                                      EQW9Z0MQ
05184      PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2.          EQW9Z0MQ
05185                                                                   EQW9Z0MQ
F36835*---GARANTIE PROTECTION JURIDIQUE QUE POUR 4R ET CC               EQW9Z0MQ
F36835     IF VEHTYPC OF TS-VEHICULE(1) = '4R ' OR 'CC '                EQW9Z0MQ
F36835        MOVE 'PJ'                      TO   WSS-CODE-GTI          EQW9Z0MQ
F36835        PERFORM ACCES-FBFOGA           THRU FIN-ACCES-FBFOGA      EQW9Z0MQ
F36835        IF WSS-GTI-TROUVE = 'N'                                   EQW9Z0MQ
F36835           MOVE 'FB178' TO COM-GENE-MESANO                        EQW9Z0MQ
F36835                           COM-CODERR                             EQW9Z0MQ
F36835           MOVE SPACES  TO WSS-FLAG-GTI                           EQW9Z0MQ
F36835        ELSE                                                      EQW9Z0MQ
F36835           IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
F36835              MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI            EQW9Z0MQ
F36835           ELSE                                                   EQW9Z0MQ
F36835              MOVE SPACES              TO WSS-FLAG-GTI            EQW9Z0MQ
F36835           END-IF                                                 EQW9Z0MQ
F36835        END-IF                                                    EQW9Z0MQ
F36835        PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2        EQW9Z0MQ
F36835     END-IF.                                                      EQW9Z0MQ
F36835
05186 *---GARANTIE BRIS DE GLACE QUE POUR 4R ET CC                      EQW9Z0MQ
05187      IF VEHTYPC OF TS-VEHICULE(1) = '4R ' OR 'CC '                EQW9Z0MQ
05188         MOVE 'BDG'                     TO   WSS-CODE-GTI          EQW9Z0MQ
05189         PERFORM ACCES-FBFOGA           THRU FIN-ACCES-FBFOGA      EQW9Z0MQ
05190         IF WSS-GTI-TROUVE = 'N'                                   EQW9Z0MQ
05191            MOVE 'FB178' TO COM-GENE-MESANO                        EQW9Z0MQ
05192                            COM-CODERR                             EQW9Z0MQ
05193            MOVE SPACES  TO WSS-FLAG-GTI                           EQW9Z0MQ
05194         ELSE                                                      EQW9Z0MQ
05195            IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
05196               MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI            EQW9Z0MQ
05197            ELSE                                                   EQW9Z0MQ
05198               MOVE SPACES              TO WSS-FLAG-GTI            EQW9Z0MQ
05199            END-IF                                                 EQW9Z0MQ
05200         END-IF                                                    EQW9Z0MQ
05201         PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2        EQW9Z0MQ
05202 *---FRANCHISE BRIS DE GLACE                                       EQW9Z0MQ
05203         IF GTIFLGC OF TS-VEHICULE(2, IND-GTI2) = 'O' OR SPACE     EQW9Z0MQ
05204            IF VEHFORC OF TS-VEHICULE(1) = 'V4'                    EQW9Z0MQ
05205               PERFORM ACCES-FB4BDG           THRU FIN-ACCES-FB4BDGEQW9Z0MQ
05206               IF WSS-FRANCHISE-TROUVE = 'N'                       EQW9Z0MQ
05207                  MOVE 'FB251' TO COM-GENE-MESANO                  EQW9Z0MQ
05208                                  COM-CODERR                       EQW9Z0MQ
05209                  MOVE SPACES  TO RGTIFRSM                         EQW9Z0MQ
05210                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05211               ELSE                                                EQW9Z0MQ
05212                  MOVE BD4FRSM OF FB4BDG01 TO GTIFRSM              EQW9Z0MQ
05213                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05214               END-IF                                              EQW9Z0MQ
05215            ELSE                                                   EQW9Z0MQ
05216               IF VEHFORC OF TS-VEHICULE(1) = 'V2' OR 'V3'         EQW9Z0MQ
05217                  MOVE ZERO    TO  GTIFRSM                         EQW9Z0MQ
05218                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05219               ELSE                                                EQW9Z0MQ
05220                  MOVE SPACES  TO RGTIFRSM                         EQW9Z0MQ
05221                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05222               END-IF                                              EQW9Z0MQ
05223            END-IF                                                 EQW9Z0MQ
05224         ELSE                                                      EQW9Z0MQ
05225            MOVE SPACES  TO RGTIFRSM OF TS-VEHICULE(2, IND-GTI2)   EQW9Z0MQ
05226         END-IF                                                    EQW9Z0MQ
05227      END-IF.                                                      EQW9Z0MQ
05228                                                                   EQW9Z0MQ
05229 *---GARANTIE VOL                                                  EQW9Z0MQ
05230      MOVE 'VOL'                     TO   WSS-CODE-GTI.            EQW9Z0MQ
05231      PERFORM ACCES-FBFOGA           THRU FIN-ACCES-FBFOGA.        EQW9Z0MQ
05232      IF WSS-GTI-TROUVE = 'N'                                      EQW9Z0MQ
05233         MOVE 'FB178' TO COM-GENE-MESANO                           EQW9Z0MQ
05234                         COM-CODERR                                EQW9Z0MQ
05235         MOVE SPACES  TO WSS-FLAG-GTI                              EQW9Z0MQ
05236      ELSE                                                         EQW9Z0MQ
05237         IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE         EQW9Z0MQ
05238            MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI               EQW9Z0MQ
05239         ELSE                                                      EQW9Z0MQ
05240            MOVE SPACES              TO WSS-FLAG-GTI               EQW9Z0MQ
05241         END-IF                                                    EQW9Z0MQ
05242      END-IF.                                                      EQW9Z0MQ
05243      PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2.          EQW9Z0MQ
05244                                                                   EQW9Z0MQ
05245 *---FRANCHISE VOL POUR 4R OU CC OU 2R                             EQW9Z0MQ
05246      IF GTIFLGC OF TS-VEHICULE(2, IND-GTI2) = 'O' OR SPACE        EQW9Z0MQ
05247         IF VEHTYPC OF TS-VEHICULE(1) = '4R ' OR 'CC '             EQW9Z0MQ
05248            IF VEHFORC OF TS-VEHICULE(1) = 'V2' OR 'V3'            EQW9Z0MQ
F41702              PERFORM ACCES-FB4FRM           THRU FIN-ACCES-FB4FRMEQW9Z0MQ
05250               IF WSS-FRANCHISE-TROUVE = 'N'                       EQW9Z0MQ
05251                  MOVE SPACES TO RGTIFRSM                          EQW9Z0MQ
05252                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05253               ELSE                                                EQW9Z0MQ
F41702                 MOVE FM4FRAN OF FB4FRM01 TO GTIFRSM              EQW9Z0MQ
05255                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05256               END-IF                                              EQW9Z0MQ
05257            ELSE                                                   EQW9Z0MQ
05258               IF VEHFORC OF TS-VEHICULE(1) = 'V4'                 EQW9Z0MQ
F41702                 PERFORM ACCES-FB4FVM        THRU FIN-ACCES-FB4FVMEQW9Z0MQ
05260                  IF WSS-FRANCHISE-TROUVE = 'N'                    EQW9Z0MQ
05261                     MOVE 'FB252' TO COM-GENE-MESANO               EQW9Z0MQ
05262                                     COM-CODERR                    EQW9Z0MQ
05263                     MOVE SPACES  TO RGTIFRSM                      EQW9Z0MQ
05264                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05265                  ELSE                                             EQW9Z0MQ
F41702                    MOVE FV4FVMF OF FB4FVM01 TO GTIFRSM           EQW9Z0MQ
05267                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05268                  END-IF                                           EQW9Z0MQ
05269               ELSE                                                EQW9Z0MQ
05270                  MOVE SPACES TO RGTIFRSM                          EQW9Z0MQ
05271                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05272               END-IF                                              EQW9Z0MQ
05273            END-IF                                                 EQW9Z0MQ
05274         ELSE                                                      EQW9Z0MQ
05275            PERFORM ACCES-FB2FRA           THRU FIN-ACCES-FB2FRA   EQW9Z0MQ
05276            IF WSS-FRANCHISE-TROUVE = 'N'                          EQW9Z0MQ
05277 *----CE TEST PERMET DEVITER L'AFFICHAGE DU MESSAGE QUAND LE NIVEAUEQW9Z0MQ
05278 *----POUR UN 2R EST A 'N' CAR LE POSTE N'EST PAS DANS LA TABLE SPIEQW9Z0MQ
05279               IF CLE-FR2PRTC = 'N'                                EQW9Z0MQ
05280                  CONTINUE                                         EQW9Z0MQ
05281               ELSE                                                EQW9Z0MQ
05282                  MOVE 'FB296' TO COM-GENE-MESANO                  EQW9Z0MQ
05283                                   COM-CODERR                      EQW9Z0MQ
05284               END-IF                                              EQW9Z0MQ
05285               MOVE SPACES TO RGTIFRAM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
U4080               MOVE SPACES TO RGTIFRZM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05286               MOVE SPACES TO RGTIFRTM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05287            ELSE                                                   EQW9Z0MQ
U4080               MOVE FR2FMIN OF FB2FRA01 TO GTIFRAM                 EQW9Z0MQ
U4080                                       OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
U4080               MOVE FR2FMAX OF FB2FRA01 TO GTIFRZM                 EQW9Z0MQ
05289                                       OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05290               MOVE FR2PCTT OF FB2FRA01 TO GTIFRTM                 EQW9Z0MQ
05291                                       OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05292            END-IF                                                 EQW9Z0MQ
05293         END-IF                                                    EQW9Z0MQ
05294      ELSE                                                         EQW9Z0MQ
05295         MOVE SPACES       TO RGTIFRSM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05296         MOVE SPACES       TO RGTIFRTM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05297         MOVE SPACES       TO RGTIFRAM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
U4080         MOVE SPACES       TO RGTIFRZM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05298      END-IF.                                                      EQW9Z0MQ
05299                                                                   EQW9Z0MQ
05300 *---GARANTIE DOMMAGES COLLISION                                   EQW9Z0MQ
05301      MOVE 'DOC'                     TO   WSS-CODE-GTI.            EQW9Z0MQ
05302      PERFORM ACCES-FBFOGA           THRU FIN-ACCES-FBFOGA.        EQW9Z0MQ
05303      IF WSS-GTI-TROUVE = 'N'                                      EQW9Z0MQ
05304         MOVE 'FB178' TO COM-GENE-MESANO                           EQW9Z0MQ
05305                         COM-CODERR                                EQW9Z0MQ
05306         MOVE SPACES  TO WSS-FLAG-GTI                              EQW9Z0MQ
05307      ELSE                                                         EQW9Z0MQ
05308         IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE         EQW9Z0MQ
05309            MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI               EQW9Z0MQ
05310         ELSE                                                      EQW9Z0MQ
05311            MOVE SPACES              TO WSS-FLAG-GTI               EQW9Z0MQ
05312         END-IF                                                    EQW9Z0MQ
05313      END-IF.                                                      EQW9Z0MQ
05314      PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2.          EQW9Z0MQ
05315                                                                   EQW9Z0MQ
05316 *---FRANCHISE DOMMAGES COLLISION POUR 4R OU CC OU 2R              EQW9Z0MQ
05317      IF (GTIFLGC OF TS-VEHICULE(2, IND-GTI2) = 'O' OR SPACE)      EQW9Z0MQ
05317      OR (GTIFLGC OF TS-VEHICULE(1, IND-GTI2) = 'O' OR SPACE)      EQW9Z0MQ
05318         IF VEHTYPC OF TS-VEHICULE(1) = '4R ' OR 'CC '             EQW9Z0MQ
05319            IF VEHFORC OF TS-VEHICULE(1) = 'V1' OR 'V2' OR 'V3'    EQW9Z0MQ
F41702              PERFORM ACCES-FB4FRM           THRU FIN-ACCES-FB4FRMEQW9Z0MQ
05321               IF WSS-FRANCHISE-TROUVE = 'N'                       EQW9Z0MQ
05322                  MOVE SPACES TO RGTIFRSM                          EQW9Z0MQ
05323                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05324               ELSE                                                EQW9Z0MQ
F41702                 MOVE FM4FRAN OF FB4FRM01 TO GTIFRSM              EQW9Z0MQ
05326                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05327               END-IF                                              EQW9Z0MQ
05328            ELSE                                                   EQW9Z0MQ
05329               IF VEHFORC OF TS-VEHICULE(1) = 'V4'                 EQW9Z0MQ
F41702                 PERFORM ACCES-FB4FDM        THRU FIN-ACCES-FB4FDMEQW9Z0MQ
05331                  IF WSS-FRANCHISE-TROUVE = 'N'                    EQW9Z0MQ
05332                     MOVE 'FB254' TO COM-GENE-MESANO               EQW9Z0MQ
05333                                     COM-CODERR                    EQW9Z0MQ
05334                     MOVE SPACES  TO RGTIFRSM                      EQW9Z0MQ
05335                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05336                  ELSE                                             EQW9Z0MQ
F41702                    MOVE FD4FDMF OF FB4FDM01 TO GTIFRSM           EQW9Z0MQ
05338                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05339                  END-IF                                           EQW9Z0MQ
05340               ELSE                                                EQW9Z0MQ
05341                  MOVE SPACES TO RGTIFRSM                          EQW9Z0MQ
05342                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05343               END-IF                                              EQW9Z0MQ
05344            END-IF                                                 EQW9Z0MQ
05345         ELSE                                                      EQW9Z0MQ
05346            PERFORM ACCES-FB2FRA           THRU FIN-ACCES-FB2FRA   EQW9Z0MQ
05347            IF WSS-FRANCHISE-TROUVE = 'N'                          EQW9Z0MQ
05348               MOVE 'FB296' TO COM-GENE-MESANO                     EQW9Z0MQ
05349                               COM-CODERR                          EQW9Z0MQ
05350               MOVE SPACES TO RGTIFRAM  OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
U4080               MOVE SPACES TO RGTIFRZM  OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05351               MOVE SPACES TO RGTIFRTM  OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05352            ELSE                                                   EQW9Z0MQ
U4080               MOVE FR2FMIN OF FB2FRA01 TO GTIFRAM
U4080                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
U4080               MOVE FR2FMAX OF FB2FRA01 TO GTIFRZM                 EQW9Z0MQ
05354                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05355               MOVE FR2PCTT OF FB2FRA01 TO GTIFRTM                 EQW9Z0MQ
05356                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05357            END-IF                                                 EQW9Z0MQ
05358         END-IF                                                    EQW9Z0MQ
05359      ELSE                                                         EQW9Z0MQ
05360         MOVE SPACES       TO RGTIFRSM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05361         MOVE SPACES       TO RGTIFRTM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05362         MOVE SPACES       TO RGTIFRAM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
U4080         MOVE SPACES       TO RGTIFRZM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05363      END-IF.                                                      EQW9Z0MQ
05364                                                                   EQW9Z0MQ
05365 *---GARANTIE DOMMAGES TOUS ACCIDENTS                              EQW9Z0MQ
05366      MOVE 'DTA'                     TO   WSS-CODE-GTI.            EQW9Z0MQ
05367      PERFORM ACCES-FBFOGA           THRU FIN-ACCES-FBFOGA.        EQW9Z0MQ
05368      IF WSS-GTI-TROUVE = 'N'                                      EQW9Z0MQ
05369         MOVE 'FB178' TO COM-GENE-MESANO                           EQW9Z0MQ
05370                         COM-CODERR                                EQW9Z0MQ
05371         MOVE SPACES  TO WSS-FLAG-GTI                              EQW9Z0MQ
05372      ELSE                                                         EQW9Z0MQ
05373         IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE         EQW9Z0MQ
05374            MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI               EQW9Z0MQ
05375         ELSE                                                      EQW9Z0MQ
05376            MOVE SPACES              TO WSS-FLAG-GTI               EQW9Z0MQ
05377         END-IF                                                    EQW9Z0MQ
05378      END-IF.                                                      EQW9Z0MQ
05379      PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2.          EQW9Z0MQ
05380                                                                   EQW9Z0MQ
05381 *---FRANCHISE DOMMAGES TOUS ACCIDENTS POUR 4R OU CC OU 2R         EQW9Z0MQ
05382      IF GTIFLGC OF TS-VEHICULE(2, IND-GTI2) = 'O' OR SPACE        EQW9Z0MQ
05383         IF VEHTYPC OF TS-VEHICULE(1) = '4R ' OR 'CC '             EQW9Z0MQ
05384            IF VEHFORC OF TS-VEHICULE(1) = 'V3'                    EQW9Z0MQ
F41702              PERFORM ACCES-FB4FRM           THRU FIN-ACCES-FB4FRMEQW9Z0MQ
05386               IF WSS-FRANCHISE-TROUVE = 'N'                       EQW9Z0MQ
05387                  MOVE SPACES TO RGTIFRSM                          EQW9Z0MQ
05388                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05389               ELSE                                                EQW9Z0MQ
F41702                 MOVE FM4FRAN OF FB4FRM01 TO GTIFRSM              EQW9Z0MQ
05391                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05392               END-IF                                              EQW9Z0MQ
05393            ELSE                                                   EQW9Z0MQ
05394               IF VEHFORC OF TS-VEHICULE(1) = 'V4'                 EQW9Z0MQ
F41702                 PERFORM ACCES-FB4FDM        THRU FIN-ACCES-FB4FDMEQW9Z0MQ
05396                  IF WSS-FRANCHISE-TROUVE = 'N'                    EQW9Z0MQ
05397                     MOVE 'FB254' TO COM-GENE-MESANO               EQW9Z0MQ
05398                                     COM-CODERR                    EQW9Z0MQ
05399                     MOVE SPACES  TO RGTIFRSM                      EQW9Z0MQ
05400                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05401                  ELSE                                             EQW9Z0MQ
F41702                    MOVE FD4FDMF OF FB4FDM01 TO GTIFRSM           EQW9Z0MQ
05403                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05404                  END-IF                                           EQW9Z0MQ
05405               ELSE                                                EQW9Z0MQ
05406                  MOVE SPACES TO RGTIFRSM                          EQW9Z0MQ
05407                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05408               END-IF                                              EQW9Z0MQ
05409            END-IF                                                 EQW9Z0MQ
05410         ELSE                                                      EQW9Z0MQ
05411            PERFORM ACCES-FB2FRA           THRU FIN-ACCES-FB2FRA   EQW9Z0MQ
05412            IF WSS-FRANCHISE-TROUVE = 'N'                          EQW9Z0MQ
05413               MOVE 'FB296' TO COM-GENE-MESANO                     EQW9Z0MQ
05414                               COM-CODERR                          EQW9Z0MQ
05415               MOVE SPACES  TO RGTIFRAM                            EQW9Z0MQ
05416                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
U4080               MOVE SPACES  TO RGTIFRZM                            EQW9Z0MQ
05418                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05417               MOVE SPACES  TO RGTIFRTM                            EQW9Z0MQ
05418                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05419            ELSE                                                   EQW9Z0MQ
U4080               MOVE FR2FMIN OF FB2FRA01 TO GTIFRAM                 EQW9Z0MQ
U4080                                       OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
U4080               MOVE FR2FMAX OF FB2FRA01 TO GTIFRZM                 EQW9Z0MQ
U4080                                       OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05422               MOVE FR2PCTT OF FB2FRA01 TO GTIFRTM                 EQW9Z0MQ
05423                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05424            END-IF                                                 EQW9Z0MQ
05425         END-IF                                                    EQW9Z0MQ
05426      ELSE                                                         EQW9Z0MQ
05427         MOVE SPACES       TO RGTIFRSM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05428         MOVE SPACES       TO RGTIFRTM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05429         MOVE SPACES       TO RGTIFRAM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
U4080         MOVE SPACES       TO RGTIFRZM OF TS-VEHICULE(2, IND-GTI2) EQW9Z0MQ
05430      END-IF.                                                      EQW9Z0MQ
05431                                                                   EQW9Z0MQ
05432 *---MT GARANTIE ACCESSOIRES-OBJET PERSONNELS                      EQW9Z0MQ
05433      MOVE 'AEP'                     TO   WSS-CODE-GTI.            EQW9Z0MQ
05434      PERFORM ACCES-FBFOGA           THRU FIN-ACCES-FBFOGA.        EQW9Z0MQ
05435      IF WSS-GTI-TROUVE = 'N'                                      EQW9Z0MQ
05436         MOVE 'FB178' TO COM-GENE-MESANO                           EQW9Z0MQ
05437                         COM-CODERR                                EQW9Z0MQ
05438         MOVE SPACES  TO WSS-FLAG-GTI                              EQW9Z0MQ
05439      ELSE                                                         EQW9Z0MQ
05440         IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE         EQW9Z0MQ
05441            MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI               EQW9Z0MQ
05442         ELSE                                                      EQW9Z0MQ
05443            MOVE SPACES              TO WSS-FLAG-GTI               EQW9Z0MQ
05444         END-IF                                                    EQW9Z0MQ
05445      END-IF.                                                      EQW9Z0MQ
05446      PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2.          EQW9Z0MQ
05447                                                                   EQW9Z0MQ
05448      IF VEHTYPC OF TS-VEHICULE(1) = '2R '                         EQW9Z0MQ
05449        AND VEHUSAC OF TS-VEHICULE(1) = '3'                        EQW9Z0MQ
05450         MOVE ZERO         TO GTIMONM OF TS-VEHICULE(2, IND-GTI2)  EQW9Z0MQ
05451      ELSE                                                         EQW9Z0MQ
05452         IF GTIFLGC OF TS-VEHICULE(2, IND-GTI2) = 'O' OR SPACE     EQW9Z0MQ
05453            IF VEHFORC OF TS-VEHICULE (1) = 'M2' OR 'V2'           EQW9Z0MQ
05454                                         OR 'M3' OR 'V3'           EQW9Z0MQ
05455               PERFORM ACCES-FBMTLT        THRU FIN-ACCES-FBMTLT   EQW9Z0MQ
05456               IF WSS-MONTANT-TROUVE = 'N'                         EQW9Z0MQ
05457                  MOVE 'FB183' TO COM-GENE-MESANO                  EQW9Z0MQ
05458                                  COM-CODERR                       EQW9Z0MQ
05459                  MOVE SPACES  TO RGTIMONM                         EQW9Z0MQ
05460                                        OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05461               ELSE                                                EQW9Z0MQ
05462                  IF MTLSAIM OF FBMTLT01 > ZERO                    EQW9Z0MQ
05463                     IF VEHTYPC OF TS-VEHICULE(1) = '2R '          EQW9Z0MQ
05464                        IF GTIFLGC OF TS-VEHICULE(2, 6)   = 'O'    EQW9Z0MQ
05465                          OR GTIFLGC OF TS-VEHICULE(2, 7) = 'O'    EQW9Z0MQ
05466                          OR GTIFLGC OF TS-VEHICULE(2, 8) = 'O'    EQW9Z0MQ
05467                          OR GTIFLGC OF TS-VEHICULE(2, 9) = 'O'    EQW9Z0MQ
05468                           MOVE MTLSAIM OF FBMTLT01                EQW9Z0MQ
05469                            TO  GTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05470                        ELSE                                       EQW9Z0MQ
05471                           MOVE SPACES                             EQW9Z0MQ
05472                            TO RGTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05473                        END-IF                                     EQW9Z0MQ
05474                     ELSE                                          EQW9Z0MQ
05475                        IF GTIFLGC OF TS-VEHICULE(2, 8)    = 'O'   EQW9Z0MQ
05476                          OR GTIFLGC OF TS-VEHICULE(2, 10) = 'O'   EQW9Z0MQ
05477                          OR GTIFLGC OF TS-VEHICULE(2, 11) = 'O'   EQW9Z0MQ
05478                          OR GTIFLGC OF TS-VEHICULE(2, 12) = 'O'   EQW9Z0MQ
05479                           MOVE MTLSAIM OF FBMTLT01                EQW9Z0MQ
05480                            TO  GTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05481                        ELSE                                       EQW9Z0MQ
05482                           MOVE SPACES                             EQW9Z0MQ
05483                            TO RGTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05484                        END-IF                                     EQW9Z0MQ
05485                     END-IF                                        EQW9Z0MQ
05486                  END-IF                                           EQW9Z0MQ
05487               END-IF                                              EQW9Z0MQ
05488            ELSE                                                   EQW9Z0MQ
05489               MOVE SPACES  TO RGTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05490            END-IF                                                 EQW9Z0MQ
05491         ELSE                                                      EQW9Z0MQ
05492            MOVE SPACES     TO RGTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05493         END-IF                                                    EQW9Z0MQ
05494      END-IF.                                                      EQW9Z0MQ
05495                                                                   EQW9Z0MQ
05496 *---------------------------*                                     EQW9Z0MQ
05497 *---GARANTIES QUE POUR 2R---*                                     EQW9Z0MQ
05498 *---------------------------*                                     EQW9Z0MQ
05499 *---MT GARANTIE ACCESSOIRES DE SECURITE                           EQW9Z0MQ
05500      IF VEHTYPC OF TS-VEHICULE(1) = '2R '                         EQW9Z0MQ
05501         MOVE 'ADS'                  TO   WSS-CODE-GTI             EQW9Z0MQ
05502         PERFORM ACCES-FBFOGA        THRU FIN-ACCES-FBFOGA         EQW9Z0MQ
05503         IF WSS-GTI-TROUVE = 'N'                                   EQW9Z0MQ
05504            MOVE 'FB178' TO COM-GENE-MESANO                        EQW9Z0MQ
05505                            COM-CODERR                             EQW9Z0MQ
05506            MOVE SPACES  TO WSS-FLAG-GTI                           EQW9Z0MQ
05507         ELSE                                                      EQW9Z0MQ
05508            IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
05509               MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI            EQW9Z0MQ
05510            ELSE                                                   EQW9Z0MQ
05511               MOVE SPACES              TO WSS-FLAG-GTI            EQW9Z0MQ
05512            END-IF                                                 EQW9Z0MQ
05513         END-IF                                                    EQW9Z0MQ
05514         PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2        EQW9Z0MQ
05515         IF (VEHFORC OF TS-VEHICULE(1) = 'M2' OR 'M3')             EQW9Z0MQ
05516           OR (VEHUSAC OF TS-VEHICULE(1) = '3')                    EQW9Z0MQ
05517            PERFORM ACCES-FBMTLT        THRU FIN-ACCES-FBMTLT      EQW9Z0MQ
05518            IF WSS-MONTANT-TROUVE = 'N'                            EQW9Z0MQ
05519               MOVE 'FB183' TO COM-GENE-MESANO                     EQW9Z0MQ
05520                               COM-CODERR                          EQW9Z0MQ
05521               MOVE SPACES  TO RGTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05522            ELSE                                                   EQW9Z0MQ
05523               IF MTLSAIM OF FBMTLT01 > ZERO                       EQW9Z0MQ
05524                  IF GTIFLGC OF TS-VEHICULE(2, 8)    = 'O'         EQW9Z0MQ
05525                    OR GTIFLGC OF TS-VEHICULE(2, 9)  = 'O'         EQW9Z0MQ
05526                    OR GTIFLGC OF TS-VEHICULE(2, 10) = 'O'         EQW9Z0MQ
05527                    OR GTIFLGC OF TS-VEHICULE(2, 11) = 'O'         EQW9Z0MQ
05528                     MOVE MTLSAIM OF FBMTLT01                      EQW9Z0MQ
05529                         TO GTIMONM OF TS-VEHICULE(2, IND-GTI2)    EQW9Z0MQ
05530                  ELSE                                             EQW9Z0MQ
05531                     MOVE SPACES                                   EQW9Z0MQ
05532                         TO RGTIMONM OF TS-VEHICULE(2, IND-GTI2)   EQW9Z0MQ
05533                  END-IF                                           EQW9Z0MQ
05534               END-IF                                              EQW9Z0MQ
05535            END-IF                                                 EQW9Z0MQ
05536         ELSE                                                      EQW9Z0MQ
05537            MOVE SPACES  TO RGTIMONM OF TS-VEHICULE(2, IND-GTI2)   EQW9Z0MQ
05538         END-IF                                                    EQW9Z0MQ
05539      END-IF.                                                      EQW9Z0MQ
05540                                                                   EQW9Z0MQ
05541 *---------------------------------*                               EQW9Z0MQ
05542 *---GARANTIES QUE POUR 4R OU CC---*                               EQW9Z0MQ
05543 *---------------------------------*                               EQW9Z0MQ
05544 *---GARANTIE VALEUR CONVENTIONNELLE                               EQW9Z0MQ
05545      IF VEHTYPC OF TS-VEHICULE(1) = '4R ' OR 'CC '                EQW9Z0MQ
05546         MOVE 'VCO'                  TO   WSS-CODE-GTI             EQW9Z0MQ
05547         PERFORM ACCES-FBFOGA        THRU FIN-ACCES-FBFOGA         EQW9Z0MQ
05548         IF WSS-GTI-TROUVE = 'N'                                   EQW9Z0MQ
05549            MOVE 'FB178' TO COM-GENE-MESANO                        EQW9Z0MQ
05550                            COM-CODERR                             EQW9Z0MQ
05551            MOVE SPACES  TO WSS-FLAG-GTI                           EQW9Z0MQ
05552         ELSE                                                      EQW9Z0MQ
05553            IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
05554               IF FOGPAFC OF FBFOGA01 = 'O' AND RVEHCIRD           EQW9Z0MQ
05555                 OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
05556                  MOVE COM-MA-DT-JOUR TO WSS-DATE-JOUR             EQW9Z0MQ
05557                  COMPUTE WSS-ANCIENNETE = WSS-DATE-JOUR-9         EQW9Z0MQ
05558                                       - VEHCIRD OF TS-VEHICULE(1) EQW9Z0MQ
05559                  IF WSS-ANCIENNETE > 50000                        EQW9Z0MQ
05560                     MOVE 'N'                 TO WSS-FLAG-GTI      EQW9Z0MQ
05561                  ELSE                                             EQW9Z0MQ
05562                     MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI      EQW9Z0MQ
05563                  END-IF                                           EQW9Z0MQ
05564               ELSE                                                EQW9Z0MQ
05565                  MOVE FOGPAFC OF FBFOGA01    TO WSS-FLAG-GTI      EQW9Z0MQ
05566               END-IF                                              EQW9Z0MQ
05567            ELSE                                                   EQW9Z0MQ
05568               MOVE SPACES                    TO WSS-FLAG-GTI      EQW9Z0MQ
05569            END-IF                                                 EQW9Z0MQ
05570         END-IF                                                    EQW9Z0MQ
05571         PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2        EQW9Z0MQ
05572      END-IF.                                                      EQW9Z0MQ
05573                                                                   EQW9Z0MQ
05574 *---------------------------*                                     EQW9Z0MQ
05575 *---GARANTIES QUE POUR CC---*                                     EQW9Z0MQ
05576 *---------------------------*                                     EQW9Z0MQ
05577      IF VEHTYPC OF TS-VEHICULE(1) = 'CC '                         EQW9Z0MQ
05578 *---MONTANT AMENAGEMENT INTERIEUR                                 EQW9Z0MQ
05579         MOVE 'AIN'         TO WSS-CODE-GTI                        EQW9Z0MQ
05580         PERFORM ACCES-FBFOGA        THRU FIN-ACCES-FBFOGA         EQW9Z0MQ
05581         IF WSS-GTI-TROUVE = 'N'                                   EQW9Z0MQ
05582            MOVE 'FB178' TO COM-GENE-MESANO                        EQW9Z0MQ
05583                            COM-CODERR                             EQW9Z0MQ
05584            MOVE SPACES  TO WSS-FLAG-GTI                           EQW9Z0MQ
05585         ELSE                                                      EQW9Z0MQ
05586            IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
05587               MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI            EQW9Z0MQ
05588            ELSE                                                   EQW9Z0MQ
05589               MOVE SPACES              TO WSS-FLAG-GTI            EQW9Z0MQ
05590            END-IF                                                 EQW9Z0MQ
05591         END-IF                                                    EQW9Z0MQ
05592         PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2        EQW9Z0MQ
05593         MOVE SPACES        TO RGTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05594 *---MONTANT DU CONTENU                                            EQW9Z0MQ
05595         MOVE 'CNT'         TO WSS-CODE-GTI                        EQW9Z0MQ
05596         PERFORM ACCES-FBFOGA        THRU FIN-ACCES-FBFOGA         EQW9Z0MQ
05597         IF WSS-GTI-TROUVE = 'N'                                   EQW9Z0MQ
05598            MOVE 'FB178' TO COM-GENE-MESANO                        EQW9Z0MQ
05599                            COM-CODERR                             EQW9Z0MQ
05600            MOVE SPACES  TO WSS-FLAG-GTI                           EQW9Z0MQ
05601         ELSE                                                      EQW9Z0MQ
05602            IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
05603               MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI            EQW9Z0MQ
05604            ELSE                                                   EQW9Z0MQ
05605               MOVE SPACES              TO WSS-FLAG-GTI            EQW9Z0MQ
05606            END-IF                                                 EQW9Z0MQ
05607         END-IF                                                    EQW9Z0MQ
05608         PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2        EQW9Z0MQ
05609         MOVE SPACES        TO RGTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05610 *---MONTANT DU AUVENT                                             EQW9Z0MQ
05611         MOVE 'AUV'          TO WSS-CODE-GTI                       EQW9Z0MQ
05612         PERFORM ACCES-FBFOGA        THRU FIN-ACCES-FBFOGA         EQW9Z0MQ
05613         IF WSS-GTI-TROUVE = 'N'                                   EQW9Z0MQ
05614            MOVE 'FB178' TO COM-GENE-MESANO                        EQW9Z0MQ
05615                            COM-CODERR                             EQW9Z0MQ
05616            MOVE SPACES  TO WSS-FLAG-GTI                           EQW9Z0MQ
05617         ELSE                                                      EQW9Z0MQ
05618            IF FOGPAFC OF FBFOGA01 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
05619               MOVE FOGPAFC OF FBFOGA01 TO WSS-FLAG-GTI            EQW9Z0MQ
05620            ELSE                                                   EQW9Z0MQ
05621               MOVE SPACES              TO WSS-FLAG-GTI            EQW9Z0MQ
05622            END-IF                                                 EQW9Z0MQ
05623         END-IF                                                    EQW9Z0MQ
05624         PERFORM ALIM-GTI-TS-VEH2 THRU FIN-ALIM-GTI-TS-VEH2        EQW9Z0MQ
05625         MOVE SPACES        TO RGTIMONM OF TS-VEHICULE(2, IND-GTI2)EQW9Z0MQ
05626      END-IF.                                                      EQW9Z0MQ
05627                                                                   EQW9Z0MQ
05628  FIN-PRE-AFFICHAGE-GTIE.                                          EQW9Z0MQ
05629      EXIT.                                                        EQW9Z0MQ
05630 *                                                                 EQW9Z0MQ
05631 ***************************************************************** EQW9Z0MQ
05632 * ALIMENTATION DES GARANTIES DE TS-VEHICULE(2) EN CAS DE PRE-AFFI EQW9Z0MQ
05633 ***************************************************************** EQW9Z0MQ
05634  ALIM-GTI-TS-VEH2.                                                EQW9Z0MQ
05635 *-----------------                                                EQW9Z0MQ
05636      ADD 1 TO IND-GTI2.                                           EQW9Z0MQ
05637      MOVE WSS-CODE-GTI TO GTICODC OF TS-VEHICULE(2, IND-GTI2).    EQW9Z0MQ
05638      MOVE WSS-FLAG-GTI TO GTIFLGC OF TS-VEHICULE(2, IND-GTI2).    EQW9Z0MQ
F41702     MOVE '1'          TO GTITARC OF TS-VEHICULE(2, IND-GTI2).    EQW9Z0MQ
05639  FIN-ALIM-GTI-TS-VEH2.                                            EQW9Z0MQ
05640      EXIT.                                                        EQW9Z0MQ
05641 *                                                                 EQW9Z0MQ
05642 ***************************************************************   EQW9Z0MQ
05643 *          APPEL DU MODULE DE CONTROLE TECHNIQUE              *   EQW9Z0MQ
05644 ***************************************************************   EQW9Z0MQ
05645  APPEL-MA90T00.                                                   EQW9Z0MQ
05646 *--------------                                                   EQW9Z0MQ
05647      MOVE SPACES           TO MAI90C00.                           EQW9Z0MQ
05648      MOVE NOM-TACHE        TO COM90C00-TACHE.                     EQW9Z0MQ
05649      MOVE IDENT-TS-APP     TO COM90C00-IDENT-TS.                  EQW9Z0MQ
05650      MOVE +1               TO COM90C00-ITEM-TS.                   EQW9Z0MQ
05651      MOVE SPACES           TO COM90C00-ACTION.                    EQW9Z0MQ
05652      MOVE COM-GENE-CODCIE  TO COM90C00-CIE.                       EQW9Z0MQ
05653      MOVE COM-GENE-TYPCICS TO COM90C00-TYPCICS.                   EQW9Z0MQ
05654      MOVE COM-MA           TO COM90C00-MAICOMM.                   EQW9Z0MQ
05655                                                                   EQW9Z0MQ
05656      EXEC CICS LINK PROGRAM  ('MA90T00')                          EQW9Z0MQ
05657                     COMMAREA (MAI90C00)                           EQW9Z0MQ
05658                     LENGTH   (LENGTH OF MAI90C00)                 EQW9Z0MQ
05659      END-EXEC.                                                    EQW9Z0MQ
05660                                                                   EQW9Z0MQ
05661      IF EIBRCODE = LOW-VALUE                                      EQW9Z0MQ
05662         MOVE COM90C00-MAICOMM TO COM-MA                           EQW9Z0MQ
05663      ELSE                                                         EQW9Z0MQ
05664         MOVE 'FB01 : ERREUR LINK MA90T00' TO MESS                 EQW9Z0MQ
05665         GO TO ABANDON-TACHE                                       EQW9Z0MQ
05666      END-IF.                                                      EQW9Z0MQ
05667 *                                                                 EQW9Z0MQ
05668  FIN-APPEL-MA90T00.                                               EQW9Z0MQ
05669      EXIT.                                                        EQW9Z0MQ
05670 *                                                                 EQW9Z0MQ
05671 ***************************************************************   EQW9Z0MQ
05672 *          APPEL DU MODULE DE CONTROLE D'HABILITATION         *   EQW9Z0MQ
05673 ***************************************************************   EQW9Z0MQ
05674  APPEL-MA90T20.                                                   EQW9Z0MQ
05675 *--------------                                                   EQW9Z0MQ
05676      MOVE SPACES           TO MAI90C20.                           EQW9Z0MQ
05677      MOVE NOM-TACHE        TO MA90C20-TRANSAC.                    EQW9Z0MQ
05678      MOVE IDENT-TS-APP     TO MA90C20-IDENT-TS.                   EQW9Z0MQ
05679      MOVE IDENT-TS-CONF    TO MA90C20-IDENT-TS-CONF.              EQW9Z0MQ
05680      MOVE COM-GENE-LNGCNV  TO MA90C20-LG-TS-CONF.                 EQW9Z0MQ
05681      MOVE NOM-TACHE        TO MA90C20-TRANSAC.                    EQW9Z0MQ
05682      MOVE SPACES           TO MA90C20-ACTION.                     EQW9Z0MQ
05683      MOVE 'N'              TO MA90C20-MAJ-TS-APP.                 EQW9Z0MQ
05684      MOVE Z-COMMAREA-USER  TO MA90C20-MAICOMM.                    EQW9Z0MQ
05685      MOVE COM-GENE-TYPCICS TO MA90C20-TYPCICS.                    EQW9Z0MQ
05686      MOVE COM-GENE-CODCIE  TO MA90C20-CODCIE.                     EQW9Z0MQ
05687      MOVE '1'              TO MA90C20-SWAP.                       EQW9Z0MQ
05688      MOVE 'C'              TO MA90C20-TYPETS.                     EQW9Z0MQ
05689      MOVE 'O'              TO MA90C20-RAB.                        EQW9Z0MQ
05690                                                                   EQW9Z0MQ
05691      EXEC CICS LINK PROGRAM  ('MA90T20')                          EQW9Z0MQ
05692                     COMMAREA (MAI90C20)                           EQW9Z0MQ
05693                     LENGTH   (LENGTH OF MAI90C20)                 EQW9Z0MQ
05694                     NOHANDLE                                      EQW9Z0MQ
05695      END-EXEC.                                                    EQW9Z0MQ
05696                                                                   EQW9Z0MQ
05697      IF EIBRCODE = LOW-VALUE                                      EQW9Z0MQ
05698         MOVE MA90C20-MAICOMM TO COM-MA                            EQW9Z0MQ
05699      ELSE                                                         EQW9Z0MQ
05700         MOVE 'FB02 : ERREUR LINK MA90T20' TO MESS                 EQW9Z0MQ
05701         GO TO ABANDON-TACHE                                       EQW9Z0MQ
05702      END-IF.                                                      EQW9Z0MQ
05703 *                                                                 EQW9Z0MQ
05704  FIN-APPEL-MA90T20.                                               EQW9Z0MQ
05705      EXIT.                                                        EQW9Z0MQ
05706 *                                                                 EQW9Z0MQ
05707 ***************************************************************   EQW9Z0MQ
05708 *          APPEL DU MODULE DE RECHERCHE DES CONNEXES          *   EQW9Z0MQ
05709 ***************************************************************   EQW9Z0MQ
05710  APPEL-FB90T09.                                                   EQW9Z0MQ
05711 *--------------                                                   EQW9Z0MQ
05712      MOVE SPACES                    TO FBI90C09.                  EQW9Z0MQ
05713      MOVE COM-GENE-TYPCICS          TO FB90C09-TYPCICS.           EQW9Z0MQ
05714      MOVE IDENT-TS-APP              TO FB90C09-ID-TS-SUSPENS.     EQW9Z0MQ
05715      MOVE COM-FB-IDENT-TSCONT       TO FB90C09-IDENT-TSCNTPROD.   EQW9Z0MQ
05716      MOVE COM-MA-CODCNV(1:6)        TO FB90C09-CODCNV.            EQW9Z0MQ
05717      IF INF-CIE OF TS-SUSPENS1 NOT = SPACES AND LOW-VALUE         EQW9Z0MQ
05718         MOVE INF-CIE OF TS-SUSPENS1 TO FB90C09-CIECODC            EQW9Z0MQ
05719      ELSE                                                         EQW9Z0MQ
05720         MOVE INF-CIE OF TS-SUSPENS2 TO FB90C09-CIECODC            EQW9Z0MQ
05721      END-IF.                                                      EQW9Z0MQ
05722      IF INF-NOPOL OF TS-SUSPENS1 NOT = SPACES AND LOW-VALUE       EQW9Z0MQ
05723         MOVE INF-NOPOL OF TS-SUSPENS1 TO FB90C09-POLNUMX          EQW9Z0MQ
05724      ELSE                                                         EQW9Z0MQ
05725         MOVE INF-NOPOL OF TS-SUSPENS2 TO FB90C09-POLNUMX          EQW9Z0MQ
05726      END-IF.                                                      EQW9Z0MQ
05727      IF INF-GES OF TS-SUSPENS1 NOT = SPACES AND LOW-VALUE         EQW9Z0MQ
05728         MOVE INF-GES OF TS-SUSPENS1 TO FB90C09-INTERM             EQW9Z0MQ
05729      ELSE                                                         EQW9Z0MQ
05730         MOVE INF-GES OF TS-SUSPENS2 TO FB90C09-INTERM             EQW9Z0MQ
05731      END-IF.                                                      EQW9Z0MQ
05732      IF NOM-CLI OF TS-SUSPENS1 NOT = SPACES AND LOW-VALUE         EQW9Z0MQ
05733         MOVE NOM-CLI OF TS-SUSPENS1 TO FB90C09-NUMCLI             EQW9Z0MQ
05734      ELSE                                                         EQW9Z0MQ
05735         MOVE NOM-CLI OF TS-SUSPENS2 TO FB90C09-NUMCLI             EQW9Z0MQ
05736      END-IF.                                                      EQW9Z0MQ
05737                                                                   EQW9Z0MQ
05738      EXEC CICS LINK PROGRAM  ('FB90T09')                          EQW9Z0MQ
05739                     COMMAREA (FB90C09)                            EQW9Z0MQ
05740                     LENGTH   (LENGTH OF FB90C09)                  EQW9Z0MQ
05741                     NOHANDLE                                      EQW9Z0MQ
05742      END-EXEC.                                                    EQW9Z0MQ
05743                                                                   EQW9Z0MQ
05744      IF FB90C09-CODRET NOT = '00'                                 EQW9Z0MQ
05745         MOVE 'FB03 : ERREUR LINK FB90T09' TO MESS                 EQW9Z0MQ
05746         GO TO ABANDON-TACHE                                       EQW9Z0MQ
05747      END-IF.                                                      EQW9Z0MQ
05748 *                                                                 EQW9Z0MQ
05749  FIN-APPEL-FB90T09.                                               EQW9Z0MQ
05750      EXIT.                                                        EQW9Z0MQ
05751 *                                                                 EQW9Z0MQ
05752 ***************************************************************   EQW9Z0MQ
05753 *          REECRITURE TS SUSPENS                              *   EQW9Z0MQ
05754 ***************************************************************   EQW9Z0MQ
05755  REECRITURE-TS-SUSPENS-DL1.                                       EQW9Z0MQ
05756 *--------------------------                                       EQW9Z0MQ
05757      MOVE +1 TO RANG-TS.                                          EQW9Z0MQ
05758      EXEC CICS WRITEQ TS QUEUE  (IDENT-TS-APP)                    EQW9Z0MQ
05759                          FROM   (TS-SUSPENS1)                     EQW9Z0MQ
05760                          LENGTH (LONG-TS-SUSPENS)                 EQW9Z0MQ
05761                          ITEM   (RANG-TS)                         EQW9Z0MQ
05762                          REWRITE                                  EQW9Z0MQ
05763                          MAIN                                     EQW9Z0MQ
05764                          NOHANDLE                                 EQW9Z0MQ
05765      END-EXEC.                                                    EQW9Z0MQ
05766      IF EIBRCODE NOT = LOW-VALUE                                  EQW9Z0MQ
05767         MOVE 'FBE1 ERR.REWRITE TS-SUSPENS1' TO MESS               EQW9Z0MQ
05768         GO TO ABANDON-TACHE                                       EQW9Z0MQ
05769      END-IF.                                                      EQW9Z0MQ
05770                                                                   EQW9Z0MQ
05771  FIN-REECRITURE-TS-SUSPENS-DL1.                                   EQW9Z0MQ
05772      EXIT.                                                        EQW9Z0MQ
05773 ***************************************************************   EQW9Z0MQ
05774 *            ECRITURE TS VEHICULE                             *   EQW9Z0MQ
05775 ***************************************************************   EQW9Z0MQ
05776  ECRITURE-TS-VEHICULE.                                            EQW9Z0MQ
05777 *---------------------                                            EQW9Z0MQ
05778      ADD 1 TO COM-FB-RANG-MAX-TSVEHI.                             EQW9Z0MQ
05779      ADD 1 TO COM-FB-NBRE-VEHI-ENC.                               EQW9Z0MQ
05780      ADD 1 TO COM-FB-ORDN-MAX-VEHI.                               EQW9Z0MQ
05781      MOVE COM-FB-ORDN-MAX-VEHI TO VEHORDX OF TS-VEHICULE(2).      EQW9Z0MQ
05782      IF VEHTYPC OF TS-VEHICULE(1) = '4R ' OR 'CC '                EQW9Z0MQ
05783         ADD 1 TO COM-FB-NBRE-VEHI-TRACTEUR                        EQW9Z0MQ
05784      END-IF.                                                      EQW9Z0MQ
05785 *    IF (ECR-VEHGENCO = 'G' OR 'J')                               EQW9Z0MQ
05786 *      AND (ECR-VEHUSACO = '201' OR '202')                        EQW9Z0MQ
05787 *       MOVE 'O' TO COM-FB-TOP-AVEC-TAXE-AGRI                     EQW9Z0MQ
05788 *       MOVE 'N' TO COM-FB-TOP-SANS-TAXE-AGRI                     EQW9Z0MQ
05789 *    ELSE                                                         EQW9Z0MQ
05790 *       MOVE 'N' TO COM-FB-TOP-AVEC-TAXE-AGRI                     EQW9Z0MQ
05791 *       MOVE 'O' TO COM-FB-TOP-SANS-TAXE-AGRI                     EQW9Z0MQ
05792 *    END-IF.                                                      EQW9Z0MQ
05793      MOVE COM-FB-RANG-MAX-TSVEHI TO COM-FB-RANG-TS-LIRE.          EQW9Z0MQ
05794      EXEC CICS WRITEQ TS QUEUE  (COM-FB-IDENT-TSVEHI)             EQW9Z0MQ
05795                          FROM   (TS-VEHICULE)                     EQW9Z0MQ
05796                          LENGTH (LENGTH OF TS-VEHICULE)           EQW9Z0MQ
05797                          ITEM   (COM-FB-RANG-TS-LIRE)             EQW9Z0MQ
05798                          NOHANDLE                                 EQW9Z0MQ
05799      END-EXEC.                                                    EQW9Z0MQ
05800                                                                   EQW9Z0MQ
05801      IF EIBRCODE NOT = LOW-VALUE                                  EQW9Z0MQ
05802         MOVE 'VER1 ERR.WRITE TS-VEHICULE' TO MESS                 EQW9Z0MQ
05803         GO TO ABANDON-TACHE                                       EQW9Z0MQ
05804      END-IF.                                                      EQW9Z0MQ
05805                                                                   EQW9Z0MQ
05806  FIN-ECRITURE-TS-VEHICULE.                                        EQW9Z0MQ
05807      EXIT.                                                        EQW9Z0MQ
05808 *                                                                 EQW9Z0MQ
05809 ***************************************************************   EQW9Z0MQ
05810 *          REECRITURE TS VEHICULE                             *   EQW9Z0MQ
05811 ***************************************************************   EQW9Z0MQ
05812  REECRITURE-TS-VEHICULE.                                          EQW9Z0MQ
05813 *-----------------------                                          EQW9Z0MQ
05814      EXEC CICS WRITEQ TS QUEUE  (COM-FB-IDENT-TSVEHI)             EQW9Z0MQ
05815                          FROM   (TS-VEHICULE)                     EQW9Z0MQ
05816                          LENGTH (LENGTH OF TS-VEHICULE)           EQW9Z0MQ
05817                          ITEM   (COM-FB-RANG-TS-LIRE)             EQW9Z0MQ
05818                          REWRITE                                  EQW9Z0MQ
05819                          NOHANDLE                                 EQW9Z0MQ
05820      END-EXEC.                                                    EQW9Z0MQ
05821                                                                   EQW9Z0MQ
05822      IF EIBRCODE NOT = LOW-VALUE                                  EQW9Z0MQ
05823         MOVE 'VER2 ERR.REWRITE TS-VEHICULE' TO MESS               EQW9Z0MQ
05824         GO TO ABANDON-TACHE                                       EQW9Z0MQ
05825      END-IF.                                                      EQW9Z0MQ
05826                                                                   EQW9Z0MQ
05827  FIN-REECRITURE-TS-VEHICULE.                                      EQW9Z0MQ
05828      EXIT.                                                        EQW9Z0MQ
05829 *                                                                 EQW9Z0MQ
05830 ***************************************************************** EQW9Z0MQ
05831 *  RECHERCHE DE LA ZONE RC ET DE LA ZONE VOL A PARTIR DE LA DATE* EQW9Z0MQ
05832 *  D'APPLICATION OU DE LA DATE EFFET, DU CODE INSEE DU LIEU DE  * EQW9Z0MQ
05833 *  GARAGE HABITUEL. SI LE POSTE N'EST PAS TROUVE RECHERCHE DE   * EQW9Z0MQ
05834 *  CELUI DONT LE CODE DEPARTEMENT EST EGAL AUX 2 PREMIERS       * EQW9Z0MQ
05835 *  CARACTERES DU CODE INSEE                                     * EQW9Z0MQ
05836 ***************************************************************** EQW9Z0MQ
05837  RECHERCHE-ZONE.                                                  EQW9Z0MQ
05838 *---------------                                                  EQW9Z0MQ
05839      IF YA-PB-DETERMINATION                                       EQW9Z0MQ
05840       GO TO FIN-RECHERCHE-ZONE                                    EQW9Z0MQ
05841      END-IF.                                                      EQW9Z0MQ
05842                                                                   EQW9Z0MQ
05843      SET POSTE-PAS-TROUVE          TO TRUE.                       EQW9Z0MQ
05844                                                                   EQW9Z0MQ
05845      EVALUATE TRUE                                                EQW9Z0MQ
05846         WHEN INF-EFFET OF TS-SUSPENS1 NOT = SPACE AND LOW-VALUE   EQW9Z0MQ
05847              MOVE INF-EFFET OF TS-SUSPENS1     TO W-EFFET         EQW9Z0MQ
05848         WHEN OTHER                                                EQW9Z0MQ
05849              MOVE INF-EFFET OF TS-SUSPENS2     TO W-EFFET         EQW9Z0MQ
05850      END-EVALUATE.                                                EQW9Z0MQ
05851                                                                   EQW9Z0MQ
05852      MOVE W-EFFET                  TO WSS-SAMJ.                   EQW9Z0MQ
05853                                                                   EQW9Z0MQ
05854      PERFORM UNTIL POSTE-TROUVE                                   EQW9Z0MQ
05855                 OR DEBUT-TABLE                                    EQW9Z0MQ
05856                 OR PROBLEME-TABLE                                 EQW9Z0MQ
05857                                                                   EQW9Z0MQ
05858         MOVE SPACE                  TO   XSPIPARM                 EQW9Z0MQ
05859         MOVE HIGH-VALUE             TO   FOZONE01                 EQW9Z0MQ
05860         MOVE WSS-SAMJ               TO   DATAPPD  OF FOZONE01     EQW9Z0MQ
05861         MOVE FOZONE01               TO   REF-POSTE OF XSPIPARM    EQW9Z0MQ
05862         MOVE 'FOZONE'               TO   TABLE-PREF               EQW9Z0MQ
05863         MOVE INF-CIE OF TS-SUSPENS1 TO   TABLE-SUFF               EQW9Z0MQ
05864         MOVE 'GP'                   TO   FONCTION  OF XSPIPARM    EQW9Z0MQ
05865         MOVE IDENT-TABLE            TO   CODTAB    OF XSPIPARM    EQW9Z0MQ
05866         MOVE '< '                   TO   OPERATEUR OF XSPIPARM    EQW9Z0MQ
05867         PERFORM ACCES-SPI           THRU FIN-ACCES-SPI            EQW9Z0MQ
05868         IF RETCOD OF XSPIPARM = '00'                              EQW9Z0MQ
05869            MOVE IOAREA  OF XSPIPARM TO FOZONE01                   EQW9Z0MQ
05870            MOVE DATAPPD OF FOZONE01 TO WSS-SAMJ                   EQW9Z0MQ
05871         ELSE                                                      EQW9Z0MQ
05872            IF RETCOD OF XSPIPARM = '02'                           EQW9Z0MQ
05873                SET DEBUT-TABLE      TO TRUE                       EQW9Z0MQ
05874            ELSE                                                   EQW9Z0MQ
05875                SET PROBLEME-TABLE   TO TRUE                       EQW9Z0MQ
05876            END-IF                                                 EQW9Z0MQ
05877         END-IF                                                    EQW9Z0MQ
05878                                                                   EQW9Z0MQ
05879      MOVE WSS-CODE-DEPT TO DPTCOMX OF CLE-FOZONE01                EQW9Z0MQ
05880      MOVE WSS-CODE-COMM TO NUMCOMX OF CLE-FOZONE01                EQW9Z0MQ
05881      PERFORM LECTURE-ZONIER THRU FIN-LECTURE-ZONIER               EQW9Z0MQ
05882      IF OK-ZONIER = 'N'                                           EQW9Z0MQ
05883         MOVE WSS-CODE-DEPT TO DPTCOMX OF CLE-FOZONE01             EQW9Z0MQ
05884         MOVE SPACES        TO NUMCOMX OF CLE-FOZONE01             EQW9Z0MQ
05885         PERFORM LECTURE-ZONIER THRU FIN-LECTURE-ZONIER            EQW9Z0MQ
05886         IF OK-ZONIER = 'N'                                        EQW9Z0MQ
05887            MOVE NOR-ALP TO ECR-GARVILLA                           EQW9Z0MQ
05888            IF KONTROL = 0                                         EQW9Z0MQ
05889               MOVE 'FB118' TO COM-GENE-MESANO                     EQW9Z0MQ
05890                               COM-CODERR                          EQW9Z0MQ
05891               MOVE CURSEUR TO ECR-GARVILLL                        EQW9Z0MQ
05892               MOVE 1       TO KONTROL                             EQW9Z0MQ
05893            END-IF                                                 EQW9Z0MQ
05894         END-IF                                                    EQW9Z0MQ
05895      END-IF                                                       EQW9Z0MQ
05896                                                                   EQW9Z0MQ
05897      END-PERFORM.                                                 EQW9Z0MQ
05898                                                                   EQW9Z0MQ
05899      IF DEBUT-TABLE OR PROBLEME-TABLE                             EQW9Z0MQ
05900          MOVE 'N'                  TO OK-ZONIER                   EQW9Z0MQ
05901      END-IF.                                                      EQW9Z0MQ
05902                                                                   EQW9Z0MQ
05903  FIN-RECHERCHE-ZONE.                                              EQW9Z0MQ
05904      EXIT.                                                        EQW9Z0MQ
05905                                                                   EQW9Z0MQ
05906 ******************************************************************EQW9Z0MQ
05907 * ACCES A LA TABLE FOZONE01 POUR RECUPERER LA ZONE RC ET LA ZONE *EQW9Z0MQ
05908 * VOL                                                            *EQW9Z0MQ
05909 ******************************************************************EQW9Z0MQ
05910  LECTURE-ZONIER.                                                  EQW9Z0MQ
05911 *---------------                                                  EQW9Z0MQ
05912      IF NOT PROBLEME-TABLE OR DEBUT-TABLE                         EQW9Z0MQ
05913                                                                   EQW9Z0MQ
05914         MOVE SPACE                  TO   XSPIPARM                 EQW9Z0MQ
05915         MOVE WSS-SAMJ               TO   DATAPPD  OF FOZONE01     EQW9Z0MQ
05916         MOVE CLE-FOZONE01           TO   REF-POSTE OF XSPIPARM    EQW9Z0MQ
05917         MOVE 'FOZONE'               TO   TABLE-PREF               EQW9Z0MQ
05918         MOVE INF-CIE OF TS-SUSPENS1 TO   TABLE-SUFF               EQW9Z0MQ
05919         MOVE 'GP'                   TO   FONCTION  OF XSPIPARM    EQW9Z0MQ
05920         MOVE IDENT-TABLE            TO   CODTAB    OF XSPIPARM    EQW9Z0MQ
05921         MOVE '= '                   TO   OPERATEUR OF XSPIPARM    EQW9Z0MQ
05922         PERFORM ACCES-SPI           THRU FIN-ACCES-SPI            EQW9Z0MQ
05923         IF RETCOD OF XSPIPARM = '00'                              EQW9Z0MQ
05924             MOVE IOAREA OF XSPIPARM TO FOZONE01                   EQW9Z0MQ
05925             MOVE 'O' TO OK-ZONIER                                 EQW9Z0MQ
05926             SET POSTE-TROUVE TO TRUE                              EQW9Z0MQ
05927         ELSE                                                      EQW9Z0MQ
05928            IF RETCOD OF XSPIPARM = '02'                           EQW9Z0MQ
05929               MOVE DATAPPD OF FOZONE01 TO WSS-SAMJ-X              EQW9Z0MQ
05930               SUBTRACT 1 FROM WSS-SAMJ                            EQW9Z0MQ
05931            ELSE                                                   EQW9Z0MQ
05932               SET PROBLEME-TABLE TO TRUE                          EQW9Z0MQ
05933            END-IF                                                 EQW9Z0MQ
05934         END-IF                                                    EQW9Z0MQ
05935      END-IF.                                                      EQW9Z0MQ
05936                                                                   EQW9Z0MQ
05937  FIN-LECTURE-ZONIER.                                              EQW9Z0MQ
05938      EXIT.                                                        EQW9Z0MQ
05939 *                                                                 EQW9Z0MQ
05940 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
05941 * GESTION DE LA COMMAREA    * FB04 * TRAITEMENT NORMAL            EQW9Z0MQ
05942 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
05943 *                                                                 EQW9Z0MQ
05944 *TRAITEMENT-COMMAREA.                                             EQW9Z0MQ
05945 *--------------------                                             EQW9Z0MQ
05946 *    MOVE ZONE-MAP          TO ZONE-COMMAREA.                     EQW9Z0MQ
05947 *                                                                 EQW9Z0MQ
05948 *FIN-TRAITEMENT-COMMAREA.                                         EQW9Z0MQ
05949 *    EXIT.                                                        EQW9Z0MQ
05950 *                                                                 EQW9Z0MQ
05951 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
05952 * DETERMINATION ECRAN SUIVANT * FB04 * TRAITEMENT NORMAL          EQW9Z0MQ
05953 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
05954  DETERMINATION-ECR-SUIV.                                          EQW9Z0MQ
05955 * -------------------- *                                          EQW9Z0MQ
05979 *                                                                 EQW9Z0MQ
U3319  ++INCLUDE SQKCECRS
05979 *                                                                 EQW9Z0MQ
05980 *  ----------------------------------------------------------- *  EQW9Z0MQ
05981 *  GESTION DES PASSAGES AUX ECRANS OPTIONNELS SUIVANTS :          EQW9Z0MQ
05982 *  ----------------------------------------------------------- *  EQW9Z0MQ
05983 *                                                                 EQW9Z0MQ
05984      IF WSS-APPEL-AIDE-CDVEHI = 'O'                               EQW9Z0MQ
05985         IF ECR-VEHTYPCO = '2R '                                   EQW9Z0MQ
05986            MOVE 'FV08'               TO NOM-TACHE-XCTL            EQW9Z0MQ
05987         ELSE                                                      EQW9Z0MQ
05988            MOVE 'VE10'               TO NOM-TACHE-XCTL            EQW9Z0MQ
05989         END-IF                                                    EQW9Z0MQ
05990         MOVE CODE-TRAITEMENT-NORMAL  TO Z-FONCTION                EQW9Z0MQ
05991         MOVE 'O'                     TO COM-GENE-ECROPT           EQW9Z0MQ
05992         GO                           TO FIN-DETERMINATION-ECR-SUIVEQW9Z0MQ
05993      END-IF.                                                      EQW9Z0MQ
05994 *                                                                 EQW9Z0MQ
05995      IF WSS-APPEL-AIDE-CP    = 'O'                                EQW9Z0MQ
05996         MOVE 'MA85'                  TO NOM-TACHE-XCTL            EQW9Z0MQ
05997         MOVE CODE-TRAITEMENT-NORMAL  TO Z-FONCTION                EQW9Z0MQ
05998         MOVE 'O'                     TO COM-GENE-ECROPT           EQW9Z0MQ
05999         GO                           TO FIN-DETERMINATION-ECR-SUIVEQW9Z0MQ
06000      END-IF.                                                      EQW9Z0MQ
06001 *                                                                 EQW9Z0MQ
06002      IF WSS-APPEL-AIDE-COM   = 'O'                                EQW9Z0MQ
06003         MOVE 'MA85'                  TO NOM-TACHE-XCTL            EQW9Z0MQ
06004         MOVE CODE-TRAITEMENT-NORMAL  TO Z-FONCTION                EQW9Z0MQ
06005         MOVE 'O'                     TO COM-GENE-ECROPT           EQW9Z0MQ
06006         GO                           TO FIN-DETERMINATION-ECR-SUIVEQW9Z0MQ
06007      END-IF.                                                      EQW9Z0MQ
06008 *                                                                 EQW9Z0MQ
06009      IF WSS-APPEL-AIDE-CIE = 'O'                                  EQW9Z0MQ
06010         MOVE 'MA86'                 TO NOM-TACHE-XCTL             EQW9Z0MQ
06011         MOVE CODE-TRAITEMENT-NORMAL TO Z-FONCTION                 EQW9Z0MQ
06012         MOVE 'O'                    TO COM-GENE-ECROPT            EQW9Z0MQ
06013         GO                          TO FIN-DETERMINATION-ECR-SUIV EQW9Z0MQ
06014      END-IF.                                                      EQW9Z0MQ
06015 *                                                                 EQW9Z0MQ
05731      IF WSS-APPEL-AIDE-CHOIX-ENFANT = 'O'                         EQW90UNO
06010         MOVE 'FB57'                 TO NOM-TACHE-XCTL             EQW9Z0MQ
06011         MOVE CODE-TRAITEMENT-NORMAL TO Z-FONCTION                 EQW9Z0MQ
06012         MOVE 'O'                    TO COM-GENE-ECROPT            EQW9Z0MQ
06013         GO                          TO FIN-DETERMINATION-ECR-SUIV EQW9Z0MQ
06014      END-IF.                                                      EQW9Z0MQ
06015 *                                                                 EQW9Z0MQ
06016 * DEBRANCHEMENT VERS ECRAN AIDE GENRE, USAGE, NIVEAU DE PROTECTIONEQW9Z0MQ
06017 * ET FORMULE                                                      EQW9Z0MQ
06018 * (TEXTE SOUS SPI)                                                EQW9Z0MQ
06019      IF WSS-APPEL-AIDE-GENRE = 'O' OR                             EQW9Z0MQ
06020         WSS-APPEL-AIDE-USAGE = 'O' OR                             EQW9Z0MQ
06021         WSS-APPEL-AIDE-PROT  = 'O' OR                             EQW9Z0MQ
06022         WSS-APPEL-AIDE-FORM  = 'O'                                EQW9Z0MQ
06023         MOVE 'MA84'  TO  NOM-TACHE-XCTL                           EQW9Z0MQ
06024         MOVE  SPACES                 TO COM-GENE-MESINF           EQW9Z0MQ
06025                                         COM-GENE-MESANO           EQW9Z0MQ
06026                                         COM-CODERR                EQW9Z0MQ
06027         MOVE CODE-TRAITEMENT-NORMAL  TO Z-FONCTION                EQW9Z0MQ
06028         MOVE 'O'                     TO COM-GENE-ECROPT           EQW9Z0MQ
06029         SUBTRACT  1                  FROM   IA                    EQW9Z0MQ
06030         MOVE IA                      TO COM-MA-STD-CLICHE         EQW9Z0MQ
06031         MOVE 'N'                     TO COM-GENE-REAF             EQW9Z0MQ
06032         IF WSS-APPEL-AIDE-GENRE = 'O'                             EQW9Z0MQ
06033            IF ECR-VEHTYPCO = '2R '                                EQW9Z0MQ
06034               MOVE 'GEN2'            TO COM-MA-GENRE-TXT          EQW9Z0MQ
06035            ELSE                                                   EQW9Z0MQ
06036               MOVE 'GEN4'            TO COM-MA-GENRE-TXT          EQW9Z0MQ
06037            END-IF                                                 EQW9Z0MQ
06038         END-IF                                                    EQW9Z0MQ
06039         IF WSS-APPEL-AIDE-USAGE = 'O'                             EQW9Z0MQ
06040            MOVE 'USAG'               TO COM-MA-GENRE-TXT          EQW9Z0MQ
06041         END-IF                                                    EQW9Z0MQ
06042         IF WSS-APPEL-AIDE-PROT  = 'O'                             EQW9Z0MQ
06043            MOVE 'PROT'               TO COM-MA-GENRE-TXT          EQW9Z0MQ
06044         END-IF                                                    EQW9Z0MQ
06045         IF WSS-APPEL-AIDE-FORM  = 'O'                             EQW9Z0MQ
06046            IF ECR-VEHTYPCO = '2R '                                EQW9Z0MQ
06047               MOVE 'FOR2'           TO COM-MA-GENRE-TXT           EQW9Z0MQ
06048            ELSE                                                   EQW9Z0MQ
06049               MOVE 'FOR4'           TO COM-MA-GENRE-TXT           EQW9Z0MQ
06050            END-IF                                                 EQW9Z0MQ
06051         END-IF                                                    EQW9Z0MQ
06052         GO                           TO FIN-DETERMINATION-ECR-SUIVEQW9Z0MQ
06053      END-IF.                                                      EQW9Z0MQ
06054 *                                                                 EQW9Z0MQ
06055 * APPEL A L'ECRAN ANTECEDENTS VEHICULE 'FB05' UNIQUEMENT :        EQW9Z0MQ
06056 *    EN 'AN' OU 'RP'                                              EQW9Z0MQ
33295 *    OU EN 'AV' SI CODE FORCAGE ‡ 'F'
06057 * OU EN 'AV' AVEC AJOUT DE VEHICULE ET CODE FORCAGE A 'F'         EQW9Z0MQ
06058 * OU EN 'RV' AVEC AJOUT DE VEHICULE                               EQW9Z0MQ
06059 * AVEC RÈPONSE 'O' ‡ SINISTRES SUR LES 36 DERNIERS MOIS           EQW9Z0MQ
06060      IF  (INF-NATMVT OF TS-SUSPENS1 = 'AN' OR 'RP')               EQW9Z0MQ
33295       OR (INF-NATMVT OF TS-SUSPENS1 = 'AV' AND                    EQW9Z0MQ
33295           (CCO-FORCAG1 OF TS-SUSPENS1 = 'F'
33295         OR CCO-FORCAG2 OF TS-SUSPENS1 = 'F'
33295         OR CCO-FORCAG3 OF TS-SUSPENS1 = 'F'
33295         OR CCO-FORCAG4 OF TS-SUSPENS1 = 'F'))
33295       OR (INF-NATMVT OF TS-SUSPENS1 = 'AV'   AND                  EQW9Z0MQ
33295           (CCO-FORCAG1 OF TS-SUSPENS1 NOT = 'F'
33295        AND CCO-FORCAG2 OF TS-SUSPENS1 NOT = 'F'
33295        AND CCO-FORCAG3 OF TS-SUSPENS1 NOT = 'F'
33295        AND CCO-FORCAG4 OF TS-SUSPENS1 NOT = 'F')
06062            AND VEHACTC OF TS-VEHICULE(1) = 'I'                    EQW9Z0MQ
06062            AND (VEHCHGC OF TS-VEHICULE(1) NOT = '1' AND '2'))     EQW9Z0MQ
06063       OR (INF-NATMVT OF TS-SUSPENS1 = 'RV'                        EQW9Z0MQ
06064            AND VEHACTC OF TS-VEHICULE(1) = 'I'                    EQW9Z0MQ
06062            AND (VEHCHGC OF TS-VEHICULE(1) NOT = '1' AND '2'))     EQW9Z0MQ
06065         IF ECR-SIVINDCO = 'O'                                     EQW9Z0MQ
06066            MOVE 'FB05'                  TO NOM-TACHE-XCTL         EQW9Z0MQ
06067            MOVE CODE-TRAITEMENT-NORMAL  TO Z-FONCTION             EQW9Z0MQ
06068            MOVE 'O'                     TO COM-GENE-ECROPT        EQW9Z0MQ
06069         ELSE                                                      EQW9Z0MQ
06070            IF VEHTYPC OF TS-VEHICULE(1) = '4R ' OR 'CC '          EQW9Z0MQ
06071               MOVE 'FB06'                  TO NOM-TACHE-XCTL      EQW9Z0MQ
06072            ELSE                                                   EQW9Z0MQ
06073               MOVE 'FB08'                  TO NOM-TACHE-XCTL      EQW9Z0MQ
06074            END-IF                                                 EQW9Z0MQ
06075            MOVE CODE-TRAITEMENT-NORMAL  TO Z-FONCTION             EQW9Z0MQ
06076            MOVE 'O'                     TO COM-GENE-ECROPT        EQW9Z0MQ
06077         END-IF                                                    EQW9Z0MQ
06078      ELSE                                                         EQW9Z0MQ
06079         IF VEHTYPC OF TS-VEHICULE(1) = '4R ' OR 'CC '             EQW9Z0MQ
06080            MOVE 'FB06'                  TO NOM-TACHE-XCTL         EQW9Z0MQ
06081         ELSE                                                      EQW9Z0MQ
06082            MOVE 'FB08'                  TO NOM-TACHE-XCTL         EQW9Z0MQ
06083         END-IF                                                    EQW9Z0MQ
06084         MOVE CODE-TRAITEMENT-NORMAL     TO Z-FONCTION             EQW9Z0MQ
06085         MOVE 'O'                        TO COM-GENE-ECROPT        EQW9Z0MQ
06086      END-IF.                                                      EQW9Z0MQ
06087 *                                                                 EQW9Z0MQ
06088 *----------------------                                           EQW9Z0MQ
06089 * REMISE A BLANC DE LA ZONE DE RECHERCHE DE CONNEXES DANS LE      EQW9Z0MQ
06090 * SEGMENT TRANSIT CAR NE SERT QUE POUR UN SEUL VEHICULE           EQW9Z0MQ
06091 *----------------------                                           EQW9Z0MQ
06092      MOVE SPACES       TO CONNEXES OF FBMISPTR-IT1.               EQW9Z0MQ
06093      MOVE FBMISPTR-IT1 TO SEGTRA OF TS-SUSPENS1.                  EQW9Z0MQ
06094      PERFORM REECRITURE-TS-SUSPENS-DL1                            EQW9Z0MQ
06095         THRU FIN-REECRITURE-TS-SUSPENS-DL1.                       EQW9Z0MQ
06096 *                                                                 EQW9Z0MQ
06097  FIN-DETERMINATION-ECR-SUIV.                                      EQW9Z0MQ
06098      EXIT.                                                        EQW9Z0MQ
06099 /                                                                 EQW9Z0MQ
06100 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
06101 * GESTION DE LA MAP         * FB04 * TRAITEMENT NORMAL            EQW9Z0MQ
06102 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
06103 *                                                                 EQW9Z0MQ
06104 *TRAITEMENT-MAP.                                                  EQW9Z0MQ
06105 * ------------ *                                                  EQW9Z0MQ
06106 *                                                                 EQW9Z0MQ
06107 *FIN-TRAITEMENT-MAP.                                              EQW9Z0MQ
06108 *    EXIT.                                                        EQW9Z0MQ
06109 /                                                                 EQW9Z0MQ
06110 ***************************************************************** EQW9Z0MQ
06111 ***************************************************************** EQW9Z0MQ
06112 ***********************  MODULE SORTIE  ************************* EQW9Z0MQ
06113 ***************************************************************** EQW9Z0MQ
06114 ***************************************************************** EQW9Z0MQ
06115 *                                                                 EQW9Z0MQ
06116 ***************************************************************** EQW9Z0MQ
06117 * MODULE DE SORTIE GENERALISE                                   * EQW9Z0MQ
06118 ***************************************************************** EQW9Z0MQ
06119 *                                                                 EQW9Z0MQ
06120  MODULE-SORTIE.                                                   EQW9Z0MQ
06121 *-------------*                                                   EQW9Z0MQ
06122      IF  TRAITEMENT-AUTOMATIQUE                                   EQW9Z0MQ
06123          MOVE     SPACES  TO  COM-GENE-NEWMEN                     EQW9Z0MQ
06124          PERFORM  SORTIE-AFFICHAGE-FORMAT THRU                    EQW9Z0MQ
06125                   FIN-SORTIE-AFFICHAGE-FORMAT                     EQW9Z0MQ
06126      END-IF.                                                      EQW9Z0MQ
06127      IF  NOT OK                                                   EQW9Z0MQ
06128          PERFORM  SORTIE-ERREUR THRU                              EQW9Z0MQ
06129                   FIN-SORTIE-ERREUR                               EQW9Z0MQ
06130      END-IF.                                                      EQW9Z0MQ
06131 *                                                                 EQW9Z0MQ
06132      IF  TRAITEMENT-NORMAL                                        EQW9Z0MQ
06133          PERFORM  SORTIE-SUITE THRU                               EQW9Z0MQ
06134                   FIN-SORTIE-SUITE                                EQW9Z0MQ
06135      END-IF.                                                      EQW9Z0MQ
06136 *                                                                 EQW9Z0MQ
06137      IF  LEVEL-SUP                                                EQW9Z0MQ
06138          PERFORM  SORTIE-LEVEL-SUPERIEUR THRU                     EQW9Z0MQ
06139                   FIN-SORTIE-LEVEL-SUPERIEUR                      EQW9Z0MQ
06140      END-IF.                                                      EQW9Z0MQ
06141 *                                                                 EQW9Z0MQ
06142      IF  LEVEL-SIGN                                               EQW9Z0MQ
06143          PERFORM  SORTIE-LEVEL-SIGNATURE THRU                     EQW9Z0MQ
06144                   FIN-SORTIE-LEVEL-SIGNATURE                      EQW9Z0MQ
06145      END-IF.                                                      EQW9Z0MQ
06146 *                                                                 EQW9Z0MQ
06147      IF  LEVEL-MAX OR JUMP                                        EQW9Z0MQ
06148          PERFORM  SORTIE-LEVEL-MAX THRU                           EQW9Z0MQ
06149                   FIN-SORTIE-LEVEL-MAX                            EQW9Z0MQ
06150      END-IF.                                                      EQW9Z0MQ
06151 *                                                                 EQW9Z0MQ
06152      IF  LEVEL-PREC                                               EQW9Z0MQ
06153          PERFORM  SORTIE-LEVEL-PREC THRU                          EQW9Z0MQ
06154                   FIN-SORTIE-LEVEL-PREC                           EQW9Z0MQ
06155      END-IF.                                                      EQW9Z0MQ
06156 *                                                                 EQW9Z0MQ
06157      IF  ERREUR-MANIPULATION                                      EQW9Z0MQ
06158          PERFORM  SORTIE-ERREUR-MANIP THRU                        EQW9Z0MQ
06159                   FIN-SORTIE-ERREUR-MANIP                         EQW9Z0MQ
06160      END-IF.                                                      EQW9Z0MQ
06161 *                                                                 EQW9Z0MQ
06162 * ABANDON * * * * * * * * * * * * * * * * * * * * * * * * * * * * EQW9Z0MQ
06163 *                                                                 EQW9Z0MQ
06164      MOVE 'SK57.ERREUR CODE FONCTION DANS MODULE-SORTIE' TO MESS  EQW9Z0MQ
06165      GO TO ABANDON-TACHE.                                         EQW9Z0MQ
06166 *                                                                 EQW9Z0MQ
06167  FIN-MODULE-SORTIE.   EXIT.                                       EQW9Z0MQ
06168 *                                                                 EQW9Z0MQ
06169 ***************************************************************** EQW9Z0MQ
06170 * AFFICHAGE DE LA MAP NORMALE PUIS  RETURN TRANSID AU MEME PGM  * EQW9Z0MQ
06171 ***************************************************************** EQW9Z0MQ
06172  SORTIE-AFFICHAGE-FORMAT.                                         EQW9Z0MQ
06173 *-----------------------*                                         EQW9Z0MQ
06174      IF COM-GENE-REAF = 'O'                                       EQW9Z0MQ
06175         PERFORM RESTAURATION-TS-ECRAN  THRU                       EQW9Z0MQ
06176                 FIN-RESTAURATION-TS-ECRAN                         EQW9Z0MQ
06177      ELSE                                                         EQW9Z0MQ
06178         PERFORM REMPLISSAGE-TS-ECRAN THRU                         EQW9Z0MQ
06179                 FIN-REMPLISSAGE-TS-ECRAN                          EQW9Z0MQ
06180      END-IF.                                                      EQW9Z0MQ
06181 *                                                                 EQW9Z0MQ
06182      IF COM-GENE-MESANO  NOT = SPACES AND                         EQW9Z0MQ
06183                                LOW-VALUE                          EQW9Z0MQ
06184         PERFORM LECTURE-ERREUR THRU                               EQW9Z0MQ
06185                 FIN-LECTURE-ERREUR                                EQW9Z0MQ
06186         MOVE    SPACES   TO COM-GENE-MESANO                       EQW9Z0MQ
06187         MOVE    W-ERREUR TO ECR-XMSGALO                           EQW9Z0MQ
06188      END-IF.                                                      EQW9Z0MQ
06189 *                                                                 EQW9Z0MQ
06190      PERFORM SEND-MAP THRU                                        EQW9Z0MQ
06191              FIN-SEND-MAP.                                        EQW9Z0MQ
06192 *                                                                 EQW9Z0MQ
06193      MOVE    SPACES    TO Z-COMMAREA-TACHE-JUMP.                  EQW9Z0MQ
06194      MOVE    NOM-TACHE TO NOM-TACHE-RETOUR.                       EQW9Z0MQ
06195 *                                                                 EQW9Z0MQ
06196      PERFORM RETOUR-COMMAREA THRU                                 EQW9Z0MQ
06197              FIN-RETOUR-COMMAREA.                                 EQW9Z0MQ
06198  FIN-SORTIE-AFFICHAGE-FORMAT.  EXIT.                              EQW9Z0MQ
06199 *                                                                 EQW9Z0MQ
06200 ***************************************************************** EQW9Z0MQ
06201 *  AFFICHAGE DE LA MAP EN ERREUR ET RETURN AU MEME PROGRAMME    * EQW9Z0MQ
06202 ***************************************************************** EQW9Z0MQ
06203  SORTIE-ERREUR.                                                   EQW9Z0MQ
06204 *-------------*                                                   EQW9Z0MQ
06205      IF COM-GENE-MESANO  NOT = SPACES AND                         EQW9Z0MQ
06206                                LOW-VALUE                          EQW9Z0MQ
06207         PERFORM LECTURE-ERREUR THRU                               EQW9Z0MQ
06208                 FIN-LECTURE-ERREUR                                EQW9Z0MQ
06209         MOVE    SPACES   TO COM-GENE-MESANO                       EQW9Z0MQ
06210         MOVE    W-ERREUR TO ECR-XMSGALO                           EQW9Z0MQ
06211      END-IF.                                                      EQW9Z0MQ
06212 *                                                                 EQW9Z0MQ
06213      IF ERREUR-DISPATCH                                           EQW9Z0MQ
06214         MOVE    SPACES        TO  Z-COMMAREA-SELECT               EQW9Z0MQ
06215 *       MOVE    SPACES        TO  COM-GENE-NEWMEN                 EQW9Z0MQ
06216         MOVE    ZONE-TS-ECRAN TO  Z-MAP                           EQW9Z0MQ
06217         PERFORM RESTAURATION-TS-ECRAN THRU                        EQW9Z0MQ
06218                 FIN-RESTAURATION-TS-ECRAN                         EQW9Z0MQ
06219         MOVE    CURSEUR       TO  ECR-XCDECL                      EQW9Z0MQ
06220         MOVE    W-ERREUR      TO  ECR-XMSGALO                     EQW9Z0MQ
06221         PERFORM SEND-MAP-ERREUR   THRU                            EQW9Z0MQ
06222                 FIN-SEND-MAP-ERREUR                               EQW9Z0MQ
06223      END-IF.                                                      EQW9Z0MQ
06224 *                                                                 EQW9Z0MQ
06225      IF ERREUR                                                    EQW9Z0MQ
06226         PERFORM RESTAURATION-MAP THRU                             EQW9Z0MQ
06227                 FIN-RESTAURATION-MAP                              EQW9Z0MQ
06228         PERFORM SEND-MAP-ERREUR-DATAONLY THRU                     EQW9Z0MQ
06229                 FIN-SEND-MAP-ERREUR-DATAONLY                      EQW9Z0MQ
06230      END-IF.                                                      EQW9Z0MQ
06231 *                                                                 EQW9Z0MQ
06232      IF ERREUR-SORTIE                                             EQW9Z0MQ
06233         PERFORM SEND-MAP-NO-ERASE-DATAONLY THRU                   EQW9Z0MQ
06234                 FIN-SEND-MAP-NO-ERASE-DATAONLY                    EQW9Z0MQ
06235      END-IF.                                                      EQW9Z0MQ
06236      MOVE SPACE     TO COM-GENE-REAF.                             EQW9Z0MQ
06237      MOVE NOM-TACHE TO NOM-TACHE-RETOUR.                          EQW9Z0MQ
06238      PERFORM RETOUR-COMMAREA THRU                                 EQW9Z0MQ
06239              FIN-RETOUR-COMMAREA.                                 EQW9Z0MQ
06240  FIN-SORTIE-ERREUR.  EXIT.                                        EQW9Z0MQ
06241 /                                                                 EQW9Z0MQ
06242 ***************************************************************** EQW9Z0MQ
06243 *  XCTL OU START POUR APPELER LE PROGRAMME SUIVANT              * EQW9Z0MQ
06244 ***************************************************************** EQW9Z0MQ
06245  SORTIE-SUITE.                                                    EQW9Z0MQ
06246 *------------*                                                    EQW9Z0MQ
06247 *                                                                 EQW9Z0MQ
06248 * ON NE PEUT PAS PASSER LA MAIN A L'ELEMENT DE CONVERSATION       EQW9Z0MQ
06249 * SUIVANT SI ON A ATTEINT LES 50 TRANSACTIONS POUR UNE            EQW9Z0MQ
06250 * CONVERSATION                                                    EQW9Z0MQ
06251 *                                                                 EQW9Z0MQ
06252      IF COM-GENE-INDCNV = 50                                      EQW9Z0MQ
06253         MOVE    'SQ004' TO COM-GENE-MESANO                        EQW9Z0MQ
06254                            COM-CODERR                             EQW9Z0MQ
06255         MOVE    2       TO KONTROL                                EQW9Z0MQ
06256         MOVE    ZONE-TS-ECRAN TO Z-MAP                            EQW9Z0MQ
06257         PERFORM RESTAURATION-TS-ECRAN THRU                        EQW9Z0MQ
06258                 FIN-RESTAURATION-TS-ECRAN                         EQW9Z0MQ
06259         PERFORM SORTIE-ERREUR THRU                                EQW9Z0MQ
06260                 FIN-SORTIE-ERREUR                                 EQW9Z0MQ
06261      END-IF.                                                      EQW9Z0MQ
06262      PERFORM DELETE-TS-ECRAN THRU                                 EQW9Z0MQ
06263              FIN-DELETE-TS-ECRAN.                                 EQW9Z0MQ
06264 *                     DOUBLE AFFICHAGE                            EQW9Z0MQ
06265      MOVE    SPACE    TO COM-GENE-REAF.                           EQW9Z0MQ
06266      MOVE    NOM-PROG TO COM-PGMPRC.                              EQW9Z0MQ
06267      PERFORM XCTL-PROG-COMMAREA THRU                              EQW9Z0MQ
06268              FIN-XCTL-PROG-COMMAREA.                              EQW9Z0MQ
06269 *                                                                 EQW9Z0MQ
06270  FIN-SORTIE-SUITE.   EXIT.                                        EQW9Z0MQ
06271 *                                                                 EQW9Z0MQ
06272 ***************************************************************** EQW9Z0MQ
06273 *    RETOUR APRES     PF3    AU MENU SUPERIEUR                    EQW9Z0MQ
06274 ***************************************************************** EQW9Z0MQ
06275 *                                                                 EQW9Z0MQ
06276  SORTIE-LEVEL-SUPERIEUR.                                          EQW9Z0MQ
06277 *----------------------*                                          EQW9Z0MQ
06278      PERFORM          DELETE-TS-PLAN THRU                         EQW9Z0MQ
06279                       FIN-DELETE-TS-PLAN.                         EQW9Z0MQ
06280 *  REMISE A BLANC DE LA PILE DES CONVERSATIONS                    EQW9Z0MQ
06281 *  ET DES ENCHAINEMENTS                                           EQW9Z0MQ
06282      MOVE SPACES TO COM-GENE-CNVPIL.                              EQW9Z0MQ
06283      MOVE SPACES TO COM-GENE-ENCCNV.                              EQW9Z0MQ
06284      MOVE ZERO   TO COM-GENE-INDCNV.                              EQW9Z0MQ
06285 *                   DOUBLE AFFICHAGE                              EQW9Z0MQ
06286      MOVE SPACE  TO COM-GENE-REAF.                                EQW9Z0MQ
06287 *  RECUPERATION DU DERNIER MENU                                   EQW9Z0MQ
06288      MOVE COM-GENE-PILMEN(COM-GENE-INDMEN) TO NOM-TACHE-START,    EQW9Z0MQ
06289                                               COM-GENE-NEWMEN.    EQW9Z0MQ
06290      IF   COM-GENE-TYPMEN(COM-GENE-INDMEN) = 'O'                  EQW9Z0MQ
06291      MOVE COM-GENE-EXPTRNID                TO NOM-TACHE-START     EQW9Z0MQ
06292      END-IF.                                                      EQW9Z0MQ
06293      MOVE LONG-COMMAREA  TO  LONG-START.                          EQW9Z0MQ
06294      MOVE EIBTRMID       TO  TERM-START.                          EQW9Z0MQ
06295      MOVE NOM-PROG       TO  COM-PGMPRC.                          EQW9Z0MQ
06296 *  DELETE DE LA TS ECRAN                                          EQW9Z0MQ
06297      PERFORM          DELETE-TS-ECRAN THRU                        EQW9Z0MQ
06298                       FIN-DELETE-TS-ECRAN.                        EQW9Z0MQ
06299 *    DELETE DES TS APPLICATIVES ET CONFIDENTIALITE CONVERSATION   EQW9Z0MQ
06300      PERFORM          DELETE-TS-CONF-CONV THRU                    EQW9Z0MQ
06301                       FIN-DELETE-TS-CONF-CONV.                    EQW9Z0MQ
06302      MOVE SPACES                 TO COM-GENE-SWPCNV.              EQW9Z0MQ
06303      PERFORM START-TACHE THRU                                     EQW9Z0MQ
06304              FIN-START-TACHE.                                     EQW9Z0MQ
06305      PERFORM RETOUR      THRU                                     EQW9Z0MQ
06306              FIN-RETOUR.                                          EQW9Z0MQ
06307  FIN-SORTIE-LEVEL-SUPERIEUR.   EXIT.                              EQW9Z0MQ
06308 *                                                                 EQW9Z0MQ
06309 ***************************************************************** EQW9Z0MQ
06310 *    RETOUR APRES PF12 AU NIVEAU SUPERIEUR DANS UNE CONVERSATION  EQW9Z0MQ
06311 ***************************************************************** EQW9Z0MQ
06312  SORTIE-LEVEL-PREC.                                               EQW9Z0MQ
06313 *-----------------*                                               EQW9Z0MQ
06314 *       ON NE PEUT PAS SORTIR DU PREMIER NIVEAU PAR PF12          EQW9Z0MQ
06315      IF COM-GENE-PILCNV(1) = NOM-TACHE                            EQW9Z0MQ
06316         MOVE 'SQ002' TO COM-GENE-MESANO                           EQW9Z0MQ
06317                         COM-CODERR                                EQW9Z0MQ
06318         MOVE 2       TO KONTROL                                   EQW9Z0MQ
06319         MOVE ZONE-TS-ECRAN TO Z-MAP                               EQW9Z0MQ
06320         PERFORM RESTAURATION-TS-ECRAN THRU                        EQW9Z0MQ
06321                 FIN-RESTAURATION-TS-ECRAN                         EQW9Z0MQ
06322         PERFORM SORTIE-ERREUR THRU                                EQW9Z0MQ
06323                 FIN-SORTIE-ERREUR                                 EQW9Z0MQ
06324      END-IF.                                                      EQW9Z0MQ
06325 *                                                                 EQW9Z0MQ
06326 *  REMISE A BLANC DU POSTE ACTUEL DANS LA PILE DES CONVERSATIONS  EQW9Z0MQ
06327 *                                                                 EQW9Z0MQ
06328      MOVE SPACES TO COM-GENE-PILCNV(COM-GENE-INDCNV).             EQW9Z0MQ
06329 *                                                                 EQW9Z0MQ
06330 * RECUPERATION DU NIVEAU SUPERIEUR DANS LA PILE DES CONVERSATIONS EQW9Z0MQ
06331      SUBTRACT 1 FROM COM-GENE-INDCNV.                             EQW9Z0MQ
06332      MOVE COM-GENE-PILCNV(COM-GENE-INDCNV) TO NOM-TACHE-XCTL.     EQW9Z0MQ
06333 *  DELETE DE LA TS ECRAN                                          EQW9Z0MQ
06334      PERFORM          DELETE-TS-ECRAN THRU                        EQW9Z0MQ
06335                       FIN-DELETE-TS-ECRAN.                        EQW9Z0MQ
06336 *                                                                 EQW9Z0MQ
06337      MOVE SPACE        TO COM-GENE-REAF.                          EQW9Z0MQ
06338 *                                                                 EQW9Z0MQ
06339      MOVE NOM-PROG     TO COM-PGMPRC.                             EQW9Z0MQ
06340      PERFORM XCTL-PROG-COMMAREA THRU                              EQW9Z0MQ
06341              FIN-XCTL-PROG-COMMAREA.                              EQW9Z0MQ
06342  FIN-SORTIE-LEVEL-PREC.   EXIT.                                   EQW9Z0MQ
06343 *                                                                 EQW9Z0MQ
06344 ***************************************************************** EQW9Z0MQ
06345 *    RETOUR APRES CLEAR (OU SI EIBCALEN = 0) AU PROGRAMME DE      EQW9Z0MQ
06346 *    SIGNATURE TOUJOURS PAR START                                 EQW9Z0MQ
06347 ***************************************************************** EQW9Z0MQ
06348  SORTIE-LEVEL-SIGNATURE.                                          EQW9Z0MQ
06349 *----------------------*                                          EQW9Z0MQ
06350      PERFORM          DELETE-TS-PLAN THRU                         EQW9Z0MQ
06351                       FIN-DELETE-TS-PLAN.                         EQW9Z0MQ
06352 *    DELETE DES TS APPLICATIVES      CONVERSATION                 EQW9Z0MQ
06353 *    DELETE DE LA TS CONFIDENTIALITE CONVERSATION                 EQW9Z0MQ
06354      PERFORM          DELETE-TS-CONF-CONV THRU                    EQW9Z0MQ
06355                       FIN-DELETE-TS-CONF-CONV.                    EQW9Z0MQ
06356      MOVE LONG-COMMAREA  TO  LONG-START.                          EQW9Z0MQ
06357      MOVE EIBTRMID       TO  TERM-START.                          EQW9Z0MQ
06358      MOVE 'AA00'         TO  NOM-TACHE-START.                     EQW9Z0MQ
06359      MOVE NOM-PROG       TO  COM-PGMPRC.                          EQW9Z0MQ
06360 *    DELETE DE LA TS ECRAN                                        EQW9Z0MQ
06361      PERFORM DELETE-TS-ECRAN THRU                                 EQW9Z0MQ
06362              FIN-DELETE-TS-ECRAN.                                 EQW9Z0MQ
06363 *                                                                 EQW9Z0MQ
06364      MOVE SPACES                 TO COM-GENE-SWPCNV.              EQW9Z0MQ
06365      MOVE    SPACE       TO COM-GENE-REAF.                        EQW9Z0MQ
06366 *                                                                 EQW9Z0MQ
06367      PERFORM START-TACHE THRU                                     EQW9Z0MQ
06368              FIN-START-TACHE.                                     EQW9Z0MQ
06369      PERFORM RETOUR      THRU                                     EQW9Z0MQ
06370              FIN-RETOUR.                                          EQW9Z0MQ
06371 *                                                                 EQW9Z0MQ
06372  FIN-SORTIE-LEVEL-SIGNATURE.         EXIT.                        EQW9Z0MQ
06373 *                                                                 EQW9Z0MQ
06374 ***************************************************************** EQW9Z0MQ
06375 *    RETOUR APRES PF4 AU MENU  PRINCIPAL                          EQW9Z0MQ
06376 ***************************************************************** EQW9Z0MQ
06377 *                                                                 EQW9Z0MQ
06378  SORTIE-LEVEL-MAX.                                                EQW9Z0MQ
06379 *----------------*                                                EQW9Z0MQ
06380      PERFORM          DELETE-TS-PLAN THRU                         EQW9Z0MQ
06381                       FIN-DELETE-TS-PLAN.                         EQW9Z0MQ
06382 *  REMISE A BLANC DE LA PILE DES CONVERSATIONS                    EQW9Z0MQ
06383 *  ET DES ENCHAINEMENTS                                           EQW9Z0MQ
06384      MOVE SPACES TO COM-GENE-CNVPIL.                              EQW9Z0MQ
06385      MOVE SPACES TO COM-GENE-ENCCNV.                              EQW9Z0MQ
06386      MOVE ZERO   TO COM-GENE-INDCNV.                              EQW9Z0MQ
06387 *  RECUPERATION DU MENU PRINCIPAL                                 EQW9Z0MQ
06388 *  REMISE A ZERO DE L'INDICE MENU                                 EQW9Z0MQ
06389 *  REMISE A BLANC DE LA PILE DES MENUS                            EQW9Z0MQ
06390 *                                                                 EQW9Z0MQ
06391      MOVE COM-GENE-PILMEN(1) TO COM-GENE-NEWMEN.                  EQW9Z0MQ
06392      MOVE COM-GENE-EXPTRNID  TO NOM-TACHE-START.                  EQW9Z0MQ
06393 */                                                                EQW9Z0MQ
06394      MOVE ZERO   TO COM-GENE-INDMEN.                              EQW9Z0MQ
06395      MOVE SPACES TO COM-GENE-MENPIL.                              EQW9Z0MQ
06396 *                                                                 EQW9Z0MQ
06397      MOVE SPACE        TO COM-GENE-REAF.                          EQW9Z0MQ
06398 *  DELETE DE LA TS ECRAN                                          EQW9Z0MQ
06399      PERFORM          DELETE-TS-ECRAN THRU                        EQW9Z0MQ
06400                       FIN-DELETE-TS-ECRAN.                        EQW9Z0MQ
06401      MOVE    LONG-COMMAREA  TO  LONG-START.                       EQW9Z0MQ
06402      MOVE    EIBTRMID       TO  TERM-START.                       EQW9Z0MQ
06403      MOVE    NOM-PROG       TO  COM-PGMPRC.                       EQW9Z0MQ
06404 *    DELETE  DES TS APPLICATIVES ET CONFIDENTIALITE CONVERSATION  EQW9Z0MQ
06405      PERFORM DELETE-TS-CONF-CONV THRU                             EQW9Z0MQ
06406              FIN-DELETE-TS-CONF-CONV.                             EQW9Z0MQ
06407      MOVE SPACES                 TO COM-GENE-SWPCNV.              EQW9Z0MQ
06408      PERFORM START-TACHE THRU                                     EQW9Z0MQ
06409              FIN-START-TACHE.                                     EQW9Z0MQ
06410      PERFORM RETOUR      THRU                                     EQW9Z0MQ
06411              FIN-RETOUR.                                          EQW9Z0MQ
06412  FIN-SORTIE-LEVEL-MAX.  EXIT.                                     EQW9Z0MQ
06413 *                                                                 EQW9Z0MQ
06414 ***************************************************************** EQW9Z0MQ
06415 * SORTIE ERREUR MANIPULATION DES TOUCHES FONCTION               * EQW9Z0MQ
06416 ***************************************************************** EQW9Z0MQ
06417  SORTIE-ERREUR-MANIP.                                             EQW9Z0MQ
06418 *-------------------*                                             EQW9Z0MQ
06419      MOVE   'SQ007'        TO COM-GENE-MESANO                     EQW9Z0MQ
06420                               COM-CODERR                          EQW9Z0MQ
06421      MOVE    2             TO KONTROL                             EQW9Z0MQ
06422      MOVE    ZONE-TS-ECRAN TO Z-MAP                               EQW9Z0MQ
06423      PERFORM RESTAURATION-TS-ECRAN THRU                           EQW9Z0MQ
06424              FIN-RESTAURATION-TS-ECRAN                            EQW9Z0MQ
06425      PERFORM SORTIE-ERREUR THRU                                   EQW9Z0MQ
06426              FIN-SORTIE-ERREUR.                                   EQW9Z0MQ
06427  FIN-SORTIE-ERREUR-MANIP.  EXIT.                                  EQW9Z0MQ
06428 *                                                                 EQW9Z0MQ
06429      EXIT.                                                        EQW9Z0MQ
06430 * **************************************************              EQW9Z0MQ
06431 *    RESTAURATION-MAP  PAR RESTAURATION TS-ECRAN                  EQW9Z0MQ
06432 * **************************************************              EQW9Z0MQ
06433 *                                                                 EQW9Z0MQ
06434 ***************************************************************** EQW9Z0MQ
06435 *    RESTAURATION MAP                                             EQW9Z0MQ
06436 ***************************************************************** EQW9Z0MQ
06437 *                                                                 EQW9Z0MQ
06438  RESTAURATION-MAP.                                                EQW9Z0MQ
06439 *                                                                 EQW9Z0MQ
06440      PERFORM RESTAURATION-TS-ECRAN THRU                           EQW9Z0MQ
06441              FIN-RESTAURATION-TS-ECRAN.                           EQW9Z0MQ
06442 *                                                                 EQW9Z0MQ
06443      MOVE LOW-VALUE  TO  ECR-XTRMTRACO.                           EQW9Z0MQ
06444      MOVE LOW-VALUE  TO  ECR-XAPPLILO.                            EQW9Z0MQ
06445      MOVE LOW-VALUE  TO  ECR-XJOURDO.                             EQW9Z0MQ
06446      MOVE LOW-VALUE  TO  ECR-XRACFLO.                             EQW9Z0MQ
06447      MOVE LOW-VALUE  TO  ECR-XHEUREDO.                            EQW9Z0MQ
06448      MOVE LOW-VALUE  TO  ECR-GESCLIO.                             EQW9Z0MQ
06449      MOVE LOW-VALUE  TO  ECR-RAICO.                               EQW9Z0MQ
06450      MOVE LOW-VALUE  TO  ECR-NOMCO.                               EQW9Z0MQ
36835      MOVE LOW-VALUE  TO  ECR-VEHCGNCO.                            EQW9Z0MQ
36835      MOVE LOW-VALUE  TO  ECR-VEHCGPCO.                            EQW9Z0MQ
36835      MOVE LOW-VALUE  TO  ECR-VEHCGSCO.                            EQW9Z0MQ
06451      MOVE LOW-VALUE  TO  ECR-VEHCODCO.                            EQW9Z0MQ
06452      MOVE LOW-VALUE  TO  ECR-VEHTYPCO.                            EQW9Z0MQ
06453      MOVE LOW-VALUE  TO  ECR-VEHGENCO.                            EQW9Z0MQ
06454      MOVE LOW-VALUE  TO  ECR-VEHMARLO.                            EQW9Z0MQ
06455      MOVE LOW-VALUE  TO  ECR-VEHMODLO.                            EQW9Z0MQ
06456      MOVE LOW-VALUE  TO  ECR-VEHCYLNO.                            EQW9Z0MQ
06457      MOVE LOW-VALUE  TO  ECR-VEHUSACO.                            EQW9Z0MQ
06458      MOVE LOW-VALUE  TO  ECR-VEHIMMXO.                            EQW9Z0MQ
06459      MOVE LOW-VALUE  TO  ECR-VEHCIRDO.                            EQW9Z0MQ
F3576      MOVE LOW-VALUE  TO  ECR-VEHACQDO.                            EQW9Z0MQ
06460      MOVE LOW-VALUE  TO  ECR-VEHVALMO.                            EQW9Z0MQ
F3215      MOVE LOW-VALUE  TO  ECR-VEHGROCO.
F3215      MOVE LOW-VALUE  TO  ECR-VEHCLACO.
06461      MOVE LOW-VALUE  TO  ECR-VEHPRTCO.                            EQW9Z0MQ
06462      MOVE LOW-VALUE  TO  ECR-GARCODCO.                            EQW9Z0MQ
06463      MOVE LOW-VALUE  TO  ECR-VEHPOSCO.                            EQW9Z0MQ
06464      MOVE LOW-VALUE  TO  ECR-VEHPEFCO.                            EQW9Z0MQ
06465      MOVE LOW-VALUE  TO  ECR-GARCOPCO.                            EQW9Z0MQ
06466      MOVE LOW-VALUE  TO  ECR-GARVILLO.                            EQW9Z0MQ
06467      MOVE LOW-VALUE  TO  ECR-ANVREPCO.                            EQW9Z0MQ
06468      MOVE LOW-VALUE  TO  ECR-VEHFORCO.                            EQW9Z0MQ
06469      MOVE LOW-VALUE  TO  ECR-ANVNUMXO.                            EQW9Z0MQ
06469      MOVE LOW-VALUE  TO  ECR-ANVSOUSO.                            EQW9Z0MQ
06469      MOVE LOW-VALUE  TO  ECR-ANVSOUNO.                            EQW9Z0MQ
06470      MOVE LOW-VALUE  TO  ECR-ANVCIEXO.                            EQW9Z0MQ
06471      MOVE LOW-VALUE  TO  ECR-ANVCIELO.                            EQW9Z0MQ
06472      MOVE LOW-VALUE  TO  ECR-ANVANCNO.                            EQW9Z0MQ
06473      MOVE LOW-VALUE  TO  ECR-ANVINTNO.                            EQW9Z0MQ
06474      MOVE LOW-VALUE  TO  ECR-ANVRESDO.                            EQW9Z0MQ
06475      MOVE LOW-VALUE  TO  ECR-SIVINDCO.                            EQW9Z0MQ
06475      MOVE LOW-VALUE  TO  ECR-ANVMTRCO.                            EQW9Z0MQ
06476      MOVE LOW-VALUE  TO  ECR-ANVBONTO.                            EQW9Z0MQ
06477      MOVE LOW-VALUE  TO  ECR-ANVBONDO.                            EQW9Z0MQ
06478      MOVE LOW-VALUE  TO  ECR-ANVABODO.                            EQW9Z0MQ
06479      MOVE LOW-VALUE  TO  ECR-XCDECO.                              EQW9Z0MQ
06480 *                                                                 EQW9Z0MQ
06481  FIN-RESTAURATION-MAP.  EXIT.                                     EQW9Z0MQ
06482                                                                   EQW9Z0MQ
06483 *                                                                 EQW9Z0MQ
06484 ***************************************************************** EQW9Z0MQ
06485 * DELETE       DE LA TS DE CONFIDENTIALITE CONVERSATION           EQW9Z0MQ
06486 ***************************************************************** EQW9Z0MQ
06487  DELETE-TS-CONF-CONV.                                             EQW9Z0MQ
06488 *-------------------*                                             EQW9Z0MQ
06489      MOVE    IDENT-TS-CONF  TO IDENT-TS.                          EQW9Z0MQ
06490      PERFORM DELETE-TS THRU                                       EQW9Z0MQ
06491              FIN-DELETE-TS.                                       EQW9Z0MQ
DELTS *
DELTS  ++INCLUDE MAIDELTS
DELTS *
06492  FIN-DELETE-TS-CONF-CONV.   EXIT.                                 EQW9Z0MQ
06493 *                                                                 EQW9Z0MQ
06494 *          DELETE  DE LA TS 'PLAN'                                EQW9Z0MQ
06495  ++INCLUDE SQKCPLDE                                               EQW9Z0MQ
06496 *                                                                 EQW9Z0MQ
06497 ***************************************************************** EQW9Z0MQ
06498 *  APPEL DES ORDRES CICS LES PLUS USITES                        * EQW9Z0MQ
06499 ***************************************************************** EQW9Z0MQ
06500 *                                                                 EQW9Z0MQ
06501 ****************************************************************  EQW9Z0MQ
06502 * RETOUR AVEC COMMAREA                                            EQW9Z0MQ
06503 ***************************************************************** EQW9Z0MQ
06504 *                                                                 EQW9Z0MQ
06505  ++INCLUDE SQKCRTCO                                               EQW9Z0MQ
06506 *                                                                 EQW9Z0MQ
06507 ****************************************************************  EQW9Z0MQ
06508 * RETOUR A CICS                                                   EQW9Z0MQ
06509 ***************************************************************** EQW9Z0MQ
06510 *                                                                 EQW9Z0MQ
06511  ++INCLUDE SQKCRTNO                                               EQW9Z0MQ
06512 ***************************************************************   EQW9Z0MQ
06513 * SEND MAP ERREUR                                                 EQW9Z0MQ
06514 ***************************************************************   EQW9Z0MQ
06515 *                                                                 EQW9Z0MQ
06516  ++INCLUDE SQKCSMER                                               EQW9Z0MQ
06517 *                                                                 EQW9Z0MQ
06518 ***************************************************************** EQW9Z0MQ
06519 * ENVOI MAP SIMPLE : SEND-MAP     ET   SEND-MAP-CURSOR            EQW9Z0MQ
06520 ***************************************************************** EQW9Z0MQ
06521 *                                                                 EQW9Z0MQ
06522  ++INCLUDE SQKCSM00                                               EQW9Z0MQ
06523 *                                                                 EQW9Z0MQ
06524 ****************************************************************  EQW9Z0MQ
06525 *  PASSAGE DU CONTROLE A UNE AUTRE TACHE                          EQW9Z0MQ
06526 ****************************************************************  EQW9Z0MQ
06527 *                                                                 EQW9Z0MQ
06528  ++INCLUDE SQKCSTRT                                               EQW9Z0MQ
06529 *                                                                 EQW9Z0MQ
06530 ***************************************************************** EQW9Z0MQ
06531 * PASSAGE DU CONTROL A UN NOUVEAU PROGRAMME                       EQW9Z0MQ
06532 ***************************************************************** EQW9Z0MQ
06533 *                                                                 EQW9Z0MQ
06534  ++INCLUDE SQKCXCTL                                               EQW9Z0MQ
06535 *                                                                 EQW9Z0MQ
06536 ***************************************************************** EQW9Z0MQ
06537 * RETRIEVE DES DATA EN PROVENANCE D'UN START                      EQW9Z0MQ
06538 ***************************************************************** EQW9Z0MQ
06539 *                                                                 EQW9Z0MQ
06540  ++INCLUDE SQKCRETR                                               EQW9Z0MQ
06541 *                                                                 EQW9Z0MQ
06542 ****************************************************************  EQW9Z0MQ
06543 *  PASSAGE DU CONTROLE A UN PROGRAMME DE LA MEME TACHE            EQW9Z0MQ
06544 ****************************************************************  EQW9Z0MQ
06545 *                                                                 EQW9Z0MQ
06546  ++INCLUDE SQKCLNKB                                               EQW9Z0MQ
06547 *                                                                 EQW9Z0MQ
06548 ***************************************************************** EQW9Z0MQ
06549 * CONSULTATION DE LA TEMPORARY STORAGE                            EQW9Z0MQ
06550 ***************************************************************** EQW9Z0MQ
06551 *                                                                 EQW9Z0MQ
06552  ++INCLUDE SQKCTRDB                                               EQW9Z0MQ
06553 *                                                                 EQW9Z0MQ
06554  ++INCLUDE SQKCTSPL                                               EQW9Z0MQ
06555 ***************************************************************** EQW9Z0MQ
06556 * DELETE       DE LA TEMPORARY STORAGE                            EQW9Z0MQ
06557 ***************************************************************** EQW9Z0MQ
06558 *                                                                 EQW9Z0MQ
06559  ++INCLUDE SQKCTSDE                                               EQW9Z0MQ
06560 *                                                                 EQW9Z0MQ
06561 ***************************************************************** EQW9Z0MQ
06562 * ENVOI MAP SANS ERASE DATAONLY                                   EQW9Z0MQ
06563 ***************************************************************** EQW9Z0MQ
06564 *                                                                 EQW9Z0MQ
06565  ++INCLUDE SQKCSMDO                                               EQW9Z0MQ
06566 *                                                                 EQW9Z0MQ
06567 ***************************************************************** EQW9Z0MQ
06568 * SEND MAP ERREUR MDT OFF                                         EQW9Z0MQ
06569 ***************************************************************** EQW9Z0MQ
06570 *                                                                 EQW9Z0MQ
06571  ++INCLUDE SQKCSEDO                                               EQW9Z0MQ
06572 *                                                                 EQW9Z0MQ
06573 *                                                                 EQW9Z0MQ
06574 ***************************************************************** EQW9Z0MQ
06575 *   MODULES DE CONTROLE ET DE TRAITEMENT SPECIFIQUES            * EQW9Z0MQ
06576 ***************************************************************** EQW9Z0MQ
06577 *                                                                 EQW9Z0MQ
06578 /                                                                 EQW9Z0MQ
06579 ******************************************************************EQW9Z0MQ
06580 * ACCES A L'INTERFACE AUAAL00 QUI CONSTRUIT                       EQW9Z0MQ
06581 * LA TS DE CONFIDENTIALITE DE LA CONVERSATION : COM-GENE-CODCNV   EQW9Z0MQ
06582 ******************************************************************EQW9Z0MQ
06583  INTERFACE-CONFIDENTIALITE.                                       EQW9Z0MQ
06584 *-------------------------*                                       EQW9Z0MQ
06585      MOVE SPACES                    TO COM-AU-AUAAC.              EQW9Z0MQ
06586      MOVE COM-GENE-CODCIE-PRINCIPAL TO COM-AU-CIE.                EQW9Z0MQ
06587      MOVE COM-GENE-CODSIT           TO COM-AU-SITE.               EQW9Z0MQ
06588      MOVE COM-GENE-CODCNV           TO COM-AU-CONVERS.            EQW9Z0MQ
06589      MOVE COM-GENE-CODUSR           TO COM-AU-USAGER.             EQW9Z0MQ
06590 *              COM-AU-SWAP   (1 : SWAP N∞1 / 2 : SWAP N∞2)        EQW9Z0MQ
06591      MOVE '1'                       TO COM-AU-SWAP.               EQW9Z0MQ
06592 *              COM-AU-TYPETS (M : MENU / C : CONVERSATION)        EQW9Z0MQ
06593      MOVE 'C'                       TO COM-AU-TYPETS.             EQW9Z0MQ
06594 *                                                                 EQW9Z0MQ
06595      EXEC CICS LINK PROGRAM  ('AUAAL00')                          EQW9Z0MQ
06596                     COMMAREA (COM-AU-AUAAC)                       EQW9Z0MQ
06597                     LENGTH   (COM-AU-LONG-AUAAC)                  EQW9Z0MQ
06598      END-EXEC.                                                    EQW9Z0MQ
06599 *                                                                 EQW9Z0MQ
06600      MOVE COM-AU-LONG-TS          TO COM-GENE-LNGCNV.             EQW9Z0MQ
06601      MOVE COM-AU-MESSAGE          TO COM-GENE-MESANO.             EQW9Z0MQ
06602  FIN-INTERFACE-CONFIDENTIALITE. EXIT.                             EQW9Z0MQ
06603 *                                                                 EQW9Z0MQ
06604 ******************************************************************EQW9Z0MQ
06605 * ACCES A L'INTERFACE AUAAL04 QUI REFAIT  UN CONTROLE D'ACCES     EQW9Z0MQ
06606 * (MEME CONTROLE QU'AU NIVEAU  MENU )                             EQW9Z0MQ
06607 ******************************************************************EQW9Z0MQ
06608  INTERFACE-CONTROLE-ACCES.                                        EQW9Z0MQ
06609 *------------------------*                                        EQW9Z0MQ
06610      MOVE SPACES                    TO COM-AU-AUAAC.              EQW9Z0MQ
06611      MOVE COM-GENE-CODCIE-PRINCIPAL TO COM-AU-CIE.                EQW9Z0MQ
06612      MOVE COM-GENE-CODSIT           TO COM-AU-SITE.               EQW9Z0MQ
06613      MOVE COM-GENE-CODCNV           TO COM-AU-CONVERS.            EQW9Z0MQ
06614      MOVE COM-GENE-CODUSR           TO COM-AU-USAGER.             EQW9Z0MQ
06615 *              COM-AU-SWAP   (1 : SWAP N∞1 / 2 : SWAP N∞2)        EQW9Z0MQ
06616      MOVE '1'                       TO COM-AU-SWAP.               EQW9Z0MQ
06617 *                                                                 EQW9Z0MQ
06618      EXEC CICS LINK PROGRAM  ('AUAAL04')                          EQW9Z0MQ
06619                     COMMAREA (COM-AU-AUAAC)                       EQW9Z0MQ
06620                     LENGTH   (COM-AU-LONG-AUAAC)                  EQW9Z0MQ
06621      END-EXEC.                                                    EQW9Z0MQ
06622  FIN-INTERFACE-CONTROLE-ACCES.  EXIT.                             EQW9Z0MQ
06623 /                                                                 EQW9Z0MQ
06624 ***************************************************************** EQW9Z0MQ
06625 *   LECTURE DES MESSAGES D'INFORMATION ET D'ANOMALIE            * EQW9Z0MQ
06626 ***************************************************************** EQW9Z0MQ
06627  LECTURE-ERREUR.                                                  EQW9Z0MQ
06628 *--------------*                                                  EQW9Z0MQ
06629      MOVE  SPACES                 TO XSPIPARM.                    EQW9Z0MQ
06630      IF    COM-GENE-MESINF NOT = SPACES AND LOW-VALUE             EQW9Z0MQ
06631            MOVE COM-GENE-MESINF   TO W-CODERR                     EQW9Z0MQ
06632            MOVE '*CD'             TO EL-DEMANDES OF XSPIPARM      EQW9Z0MQ
06633      ELSE                                                         EQW9Z0MQ
06634            MOVE COM-GENE-MESANO   TO W-CODERR                     EQW9Z0MQ
06635      END-IF.                                                      EQW9Z0MQ
06636      MOVE  'GP'                   TO FONCTION  OF XSPIPARM.       EQW9Z0MQ
06637      MOVE  'MSGETUDE'             TO CODTAB    OF XSPIPARM.       EQW9Z0MQ
06638      MOVE  '= '                   TO OPERATEUR OF XSPIPARM.       EQW9Z0MQ
06639      MOVE   W-CODERR              TO REF-POSTE OF XSPIPARM.       EQW9Z0MQ
06640      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
06641      IF  RETCOD OF XSPIPARM  = ZERO                               EQW9Z0MQ
06642          MOVE 0       TO CODE-RETOUR                              EQW9Z0MQ
06643          MOVE IOAREA  OF XSPIPARM TO W-ERREUR                     EQW9Z0MQ
06644          IF   COM-GENE-MESINF = SPACES OR LOW-VALUE               EQW9Z0MQ
06645               MOVE SPACES  TO W-CODERR                            EQW9Z0MQ
06646          END-IF                                                   EQW9Z0MQ
06647          MOVE SPACES  TO W-SUFERR                                 EQW9Z0MQ
06648      ELSE                                                         EQW9Z0MQ
06649          MOVE SPACES  TO W-LIBERR                                 EQW9Z0MQ
06650                          W-SUFERR                                 EQW9Z0MQ
06651          MOVE 1       TO CODE-RETOUR                              EQW9Z0MQ
06652      END-IF.                                                      EQW9Z0MQ
06653  FIN-LECTURE-ERREUR.                                              EQW9Z0MQ
06654      EXIT.                                                        EQW9Z0MQ
06655 *                                                                 EQW9Z0MQ
06656 ***************************************************************   EQW9Z0MQ
06657 *             ACCES TABLE FVVEHI                              *   EQW9Z0MQ
06658 ***************************************************************   EQW9Z0MQ
06659  ACCES-FVVEHI.                                                    EQW9Z0MQ
06660 *-------------                                                    EQW9Z0MQ
06661      MOVE SPACES                 TO XSPIPARM.                     EQW9Z0MQ
06662      MOVE SPACES                 TO WSS-CDVEHI-TROUVE.            EQW9Z0MQ
06663      MOVE 'GP'                   TO FONCTION  OF XSPIPARM.        EQW9Z0MQ
06664      MOVE 'FVVEHI'               TO TABLE-PREF.                   EQW9Z0MQ
06665      MOVE INF-CIE OF TS-SUSPENS1 TO TABLE-SUFF.                   EQW9Z0MQ
06666      MOVE IDENT-TABLE            TO CODTAB    OF XSPIPARM.        EQW9Z0MQ
06667      MOVE HIGH-VALUE             TO WSS-MARQUE-VEHI.              EQW9Z0MQ
06668      MOVE HIGH-VALUE             TO WSS-PUISSANCE-VEHI.           EQW9Z0MQ
06669 * LE PREMIER CARACTERE CODE MOTO CORRESPOND INITIALE MARQUE       EQW9Z0MQ
06670      MOVE ECR-VEHCODCO (1:1)     TO WSS-MARQUE-VEHI (1:1).        EQW9Z0MQ
06671 * LA TABLE SPI EST TRIEE PAR MARQUE / PUISSANCE / CODE VEHICULE   EQW9Z0MQ
06672 *  ===> ACCES SEQUENTIELLE SUCCESSIF JUSQU'A OBTENTION DU CODE    EQW9Z0MQ
06673 * VEHICULE (DECROISSANT)                                          EQW9Z0MQ
06674      PERFORM  UNTIL WSS-CDVEHI-TROUVE NOT = SPACES                EQW9Z0MQ
06675         MOVE WSS-MARQUE-VEHI      TO REF-POSTE OF XSPIPARM (1:15) EQW9Z0MQ
06676         MOVE WSS-PUISSANCE-VEHI   TO REF-POSTE OF XSPIPARM (16:4) EQW9Z0MQ
06677         MOVE '< '                 TO OPERATEUR OF XSPIPARM        EQW9Z0MQ
06678         PERFORM ACCES-SPI THRU FIN-ACCES-SPI                      EQW9Z0MQ
06679         IF RETCOD OF XSPIPARM  = 00                               EQW9Z0MQ
06680            MOVE IOAREA  OF XSPIPARM   TO TAB-FVVEHI               EQW9Z0MQ
06681            MOVE MARQUE  OF TAB-FVVEHI TO WSS-MARQUE-VEHI          EQW9Z0MQ
06682            MOVE CYLINDR OF TAB-FVVEHI TO WSS-PUISSANCE-VEHI       EQW9Z0MQ
06683            IF ECR-VEHCODCO = CODEVEH OF TAB-FVVEHI                EQW9Z0MQ
06684               MOVE 'O' TO WSS-CDVEHI-TROUVE                       EQW9Z0MQ
06685            END-IF                                                 EQW9Z0MQ
06686         ELSE                                                      EQW9Z0MQ
06687            IF RETCOD OF XSPIPARM = 13                             EQW9Z0MQ
06688               MOVE 'N' TO WSS-CDVEHI-TROUVE                       EQW9Z0MQ
06689            ELSE                                                   EQW9Z0MQ
06690               MOVE NOR-ALP TO ECR-VEHCODCA                        EQW9Z0MQ
06691               IF KONTROL = 0                                      EQW9Z0MQ
06692                  MOVE 'FB119' TO COM-GENE-MESANO                  EQW9Z0MQ
06693                                  COM-CODERR                       EQW9Z0MQ
06694                  MOVE CURSEUR TO ECR-VEHCODCL                     EQW9Z0MQ
06695                  MOVE 1       TO KONTROL                          EQW9Z0MQ
06696               END-IF                                              EQW9Z0MQ
06697            END-IF                                                 EQW9Z0MQ
06698         END-IF                                                    EQW9Z0MQ
06699      END-PERFORM.                                                 EQW9Z0MQ
06700                                                                   EQW9Z0MQ
06701  FIN-ACCES-FVVEHI.                                                EQW9Z0MQ
06702      EXIT.                                                        EQW9Z0MQ
06703 *                                                                 EQW9Z0MQ
06704 ***************************************************************   EQW9Z0MQ
06705 *             ACCES TABLE MIAUTO                              *   EQW9Z0MQ
06706 ***************************************************************   EQW9Z0MQ
06707  ACCES-MIAUTO.                                                    EQW9Z0MQ
06708 *-------------                                                    EQW9Z0MQ
06709      MOVE SPACES               TO XSPIPARM.                       EQW9Z0MQ
06710      MOVE 'GP'                 TO FONCTION  OF XSPIPARM.          EQW9Z0MQ
06711      MOVE 'MIAUTO'             TO TABLE-PREF.                     EQW9Z0MQ
06712      MOVE INF-CIE OF TS-SUSPENS1 TO TABLE-SUFF.                   EQW9Z0MQ
06713      MOVE IDENT-TABLE          TO CODTAB    OF XSPIPARM.          EQW9Z0MQ
06714      MOVE ECR-VEHCODCO         TO WSS-CLE-MIAUTO.                 EQW9Z0MQ
06715      MOVE WSS-CLE-MIAUTO       TO REF-POSTE OF XSPIPARM.          EQW9Z0MQ
06716      MOVE '= '                 TO OPERATEUR OF XSPIPARM.          EQW9Z0MQ
06717                                                                   EQW9Z0MQ
06718      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
06719      IF RETCOD OF XSPIPARM  = 00                                  EQW9Z0MQ
06720         MOVE 'O' TO WSS-CDVEHI-TROUVE                             EQW9Z0MQ
06721         MOVE IOAREA OF XSPIPARM TO TAB-MIAUTO                     EQW9Z0MQ
06722      ELSE                                                         EQW9Z0MQ
06723         MOVE 'N' TO WSS-CDVEHI-TROUVE                             EQW9Z0MQ
06724      END-IF.                                                      EQW9Z0MQ
06725 *                                                                 EQW9Z0MQ
06726  FIN-ACCES-MIAUTO.                                                EQW9Z0MQ
06727      EXIT.                                                        EQW9Z0MQ
06728 *                                                                 EQW9Z0MQ
06729 **************************************************************    EQW9Z0MQ
06730 *         ACCES TABLE FB2CLA                                 *    EQW9Z0MQ
06731 **************************************************************    EQW9Z0MQ
06732  ACCES-FB2CLA.                                                    EQW9Z0MQ
06733 *-------------                                                    EQW9Z0MQ
06734      MOVE SPACES                 TO XSPIPARM.                     EQW9Z0MQ
06735      MOVE 'GP'                   TO FONCTION  OF XSPIPARM.        EQW9Z0MQ
06736      MOVE 'FB2CLA'               TO TABLE-PREF.                   EQW9Z0MQ
06737      MOVE INF-CIE OF TS-SUSPENS1 TO TABLE-SUFF.                   EQW9Z0MQ
06738      MOVE IDENT-TABLE            TO CODTAB    OF XSPIPARM.        EQW9Z0MQ
06739      MOVE ECR-VEHGENCO           TO WSS-CLE-GENRE-FB2CLA.         EQW9Z0MQ
06740      MOVE WSS-VALNEUFZ           TO WSS-CLE-VAL-FB2CLA.           EQW9Z0MQ
06741      MOVE WSS-CLE-FB2CLA         TO REF-POSTE OF XSPIPARM.        EQW9Z0MQ
06742      MOVE '>='                   TO OPERATEUR OF XSPIPARM.        EQW9Z0MQ
06743      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
06744      IF RETCOD OF XSPIPARM  = 00                                  EQW9Z0MQ
06745         MOVE IOAREA OF XSPIPARM TO FB2CLA01                       EQW9Z0MQ
06746         IF CL2GENC OF FB2CLA01 NOT = WSS-CLE-GENRE-FB2CLA         EQW9Z0MQ
06747            MOVE 'N' TO WSS-CLASSE-TROUVE                          EQW9Z0MQ
06748         ELSE                                                      EQW9Z0MQ
06749            MOVE 'O' TO WSS-CLASSE-TROUVE                          EQW9Z0MQ
06750         END-IF                                                    EQW9Z0MQ
06751      ELSE                                                         EQW9Z0MQ
06752         MOVE 'N' TO WSS-CLASSE-TROUVE                             EQW9Z0MQ
06753      END-IF.                                                      EQW9Z0MQ
06754                                                                   EQW9Z0MQ
06755  FIN-ACCES-FB2CLA.                                                EQW9Z0MQ
06756      EXIT.                                                        EQW9Z0MQ
06757 *                                                                 EQW9Z0MQ
06758 ***************************************************************   EQW9Z0MQ
06759 *                  ACCES FB2GRP                               *   EQW9Z0MQ
06760 ***************************************************************   EQW9Z0MQ
06761  ACCES-FB2GRP.                                                    EQW9Z0MQ
06762 *-------------                                                    EQW9Z0MQ
06763      MOVE SPACES                 TO XSPIPARM.                     EQW9Z0MQ
06764      MOVE 'GP'                   TO FONCTION  OF XSPIPARM.        EQW9Z0MQ
06765      MOVE 'FB2GRP'               TO TABLE-PREF.                   EQW9Z0MQ
06766      MOVE INF-CIE OF TS-SUSPENS1 TO TABLE-SUFF.                   EQW9Z0MQ
06767      MOVE IDENT-TABLE            TO CODTAB    OF XSPIPARM.        EQW9Z0MQ
06768      MOVE ECR-VEHGENCO           TO WSS-CLE-GENRE-FB2GRP.         EQW9Z0MQ
06769      MOVE WSS-CYL                TO WSS-CLE-CYL-FB2GRP.           EQW9Z0MQ
06770      MOVE WSS-CLE-FB2GRP         TO REF-POSTE OF XSPIPARM.        EQW9Z0MQ
06771      MOVE '>='                   TO OPERATEUR OF XSPIPARM.        EQW9Z0MQ
06772      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
06773      IF RETCOD OF XSPIPARM  = 00                                  EQW9Z0MQ
06774         MOVE IOAREA OF XSPIPARM TO FB2GRP01                       EQW9Z0MQ
06775         IF GR2GENC OF FB2GRP01 NOT = WSS-CLE-GENRE-FB2GRP         EQW9Z0MQ
06776            MOVE 'N' TO WSS-GRP-TROUVE                             EQW9Z0MQ
06777         ELSE                                                      EQW9Z0MQ
06778            MOVE 'O' TO WSS-GRP-TROUVE                             EQW9Z0MQ
06779         END-IF                                                    EQW9Z0MQ
06780      ELSE                                                         EQW9Z0MQ
06781         MOVE 'N' TO WSS-GRP-TROUVE                                EQW9Z0MQ
06782      END-IF.                                                      EQW9Z0MQ
06783                                                                   EQW9Z0MQ
06784  FIN-ACCES-FB2GRP.                                                EQW9Z0MQ
06785      EXIT.                                                        EQW9Z0MQ
06786 *                                                                 EQW9Z0MQ
06787 **************************************************************    EQW9Z0MQ
06788 *         ACCES TABLE FB4CLA                                 *    EQW9Z0MQ
06789 **************************************************************    EQW9Z0MQ
06790  ACCES-FB4CLA.                                                    EQW9Z0MQ
06791 *-------------                                                    EQW9Z0MQ
06792      MOVE SPACES                 TO XSPIPARM.                     EQW9Z0MQ
06793      MOVE 'GP'                   TO FONCTION  OF XSPIPARM.        EQW9Z0MQ
06794      MOVE 'FB4CLA'               TO TABLE-PREF.                   EQW9Z0MQ
06795      MOVE INF-CIE OF TS-SUSPENS1 TO TABLE-SUFF.                   EQW9Z0MQ
06796      MOVE IDENT-TABLE            TO CODTAB    OF XSPIPARM.        EQW9Z0MQ
06797      MOVE ECR-VEHGENCO           TO WSS-CLE-GENRE-FB4CLA.         EQW9Z0MQ
06798      MOVE WSS-VALNEUFZ           TO WSS-CLE-VAL-FB4CLA.           EQW9Z0MQ
06799      MOVE WSS-CLE-FB4CLA         TO REF-POSTE OF XSPIPARM.        EQW9Z0MQ
06800      MOVE '>='                   TO OPERATEUR OF XSPIPARM.        EQW9Z0MQ
06801      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
06802      IF RETCOD OF XSPIPARM  = 00                                  EQW9Z0MQ
06803         MOVE IOAREA OF XSPIPARM TO FB4CLA01                       EQW9Z0MQ
06804         IF CL4GENC OF FB4CLA01 NOT = WSS-CLE-GENRE-FB4CLA         EQW9Z0MQ
06805            MOVE 'N' TO WSS-CLASSE-TROUVE                          EQW9Z0MQ
06806         ELSE                                                      EQW9Z0MQ
06807            MOVE 'O' TO WSS-CLASSE-TROUVE                          EQW9Z0MQ
06808         END-IF                                                    EQW9Z0MQ
06809      ELSE                                                         EQW9Z0MQ
06810         MOVE 'N' TO WSS-CLASSE-TROUVE                             EQW9Z0MQ
06811      END-IF.                                                      EQW9Z0MQ
06812                                                                   EQW9Z0MQ
06813  FIN-ACCES-FB4CLA.                                                EQW9Z0MQ
06814      EXIT.                                                        EQW9Z0MQ
06815 *                                                                 EQW9Z0MQ
06816 ***************************************************************   EQW9Z0MQ
06817 *                  ACCES FB4GRP                               *   EQW9Z0MQ
06818 ***************************************************************   EQW9Z0MQ
06819  ACCES-FB4GRP.                                                    EQW9Z0MQ
06820 *-------------                                                    EQW9Z0MQ
06821      MOVE SPACES                 TO XSPIPARM.                     EQW9Z0MQ
06822      MOVE 'GP'                   TO FONCTION  OF XSPIPARM.        EQW9Z0MQ
06823      MOVE 'FB4GRP'               TO TABLE-PREF.                   EQW9Z0MQ
06824      MOVE INF-CIE OF TS-SUSPENS1 TO TABLE-SUFF.                   EQW9Z0MQ
06825      MOVE IDENT-TABLE            TO CODTAB    OF XSPIPARM.        EQW9Z0MQ
06826      MOVE ECR-VEHGENCO           TO WSS-CLE-GENRE-FB4GRP.         EQW9Z0MQ
06827      MOVE WSS-CYL                TO WSS-CLE-CYL-FB4GRP.           EQW9Z0MQ
06828      MOVE WSS-CLE-FB4GRP         TO REF-POSTE OF XSPIPARM.        EQW9Z0MQ
06829      MOVE '>='                   TO OPERATEUR OF XSPIPARM.        EQW9Z0MQ
06830      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
06831      IF RETCOD OF XSPIPARM  = 00                                  EQW9Z0MQ
06832         MOVE IOAREA OF XSPIPARM TO FB4GRP01                       EQW9Z0MQ
06833         IF GR4GENC OF FB4GRP01 NOT = WSS-CLE-GENRE-FB4GRP         EQW9Z0MQ
06834            MOVE 'N' TO WSS-GRP-TROUVE                             EQW9Z0MQ
06835         ELSE                                                      EQW9Z0MQ
06836            MOVE 'O' TO WSS-GRP-TROUVE                             EQW9Z0MQ
06837         END-IF                                                    EQW9Z0MQ
06838      ELSE                                                         EQW9Z0MQ
06839         MOVE 'N' TO WSS-GRP-TROUVE                                EQW9Z0MQ
06840      END-IF.                                                      EQW9Z0MQ
06841                                                                   EQW9Z0MQ
06842  FIN-ACCES-FB4GRP.                                                EQW9Z0MQ
06843      EXIT.                                                        EQW9Z0MQ
06844 *                                                                 EQW9Z0MQ
06845 ***************************************************************   EQW9Z0MQ
06846 * MODULE DE CONTROLE DE VALIDITE D'UNE DATE SAISIE JJMMSSAA   *   EQW9Z0MQ
06847 ***************************************************************   EQW9Z0MQ
06848  VERIF-DATE.                                                      EQW9Z0MQ
06849 *-----------                                                      EQW9Z0MQ
06850      MOVE SPACES              TO K2COM-DATES.                     EQW9Z0MQ
06851      MOVE '1'                 TO K2-FONC.                         EQW9Z0MQ
06852      MOVE '01'                TO WSS-DATE-A-VERIFIER-JJ.          EQW9Z0MQ
06853      MOVE WSS-DATE-TRAV(1:2)  TO WSS-DATE-A-VERIFIER-MM.          EQW9Z0MQ
06854      MOVE WSS-DATE-TRAV(3:2)  TO WSS-DATE-A-VERIFIER-SS.          EQW9Z0MQ
06855      MOVE WSS-DATE-TRAV(5:2)  TO WSS-DATE-A-VERIFIER-AA.          EQW9Z0MQ
06856      MOVE WSS-DATE-A-VERIFIER TO K2-DATE1.                        EQW9Z0MQ
06857      MOVE '1'                 TO K2-FORM1.                        EQW9Z0MQ
06858      EXEC CICS LINK PROGRAM  ('K200LDATE')                        EQW9Z0MQ
06859                     COMMAREA (K2COM-DATES)                        EQW9Z0MQ
06860                     LENGTH   (LENGTH OF K2COM-DATES)              EQW9Z0MQ
06861      END-EXEC.                                                    EQW9Z0MQ
06862      IF K2-RETCOD = '0'                                           EQW9Z0MQ
06863         MOVE 'O' TO WSS-DATE-OK                                   EQW9Z0MQ
06864       ELSE                                                        EQW9Z0MQ
06865         MOVE 'N' TO WSS-DATE-OK                                   EQW9Z0MQ
06866      END-IF.                                                      EQW9Z0MQ
06867  FIN-VERIF-DATE.                                                  EQW9Z0MQ
06868      EXIT.                                                        EQW9Z0MQ
06869 *                                                                 EQW9Z0MQ
06870 ************************************************************      EQW9Z0MQ
06871 *   MODULE DE CADRAGE                                      *      EQW9Z0MQ
06872 ************************************************************      EQW9Z0MQ
06873  CADRAGE.                                                         EQW9Z0MQ
06874 *--------                                                         EQW9Z0MQ
06875      MOVE ZERO TO C-XKMTRETCOD.                                   EQW9Z0MQ
06876      EXEC CICS LINK PROGRAM ('XKMTCADR')                          EQW9Z0MQ
06877                COMMAREA (XKMTWCOMMA)                              EQW9Z0MQ
06878                LENGTH   (LENGTH OF XKMTWCOMMA)                    EQW9Z0MQ
06879      END-EXEC.                                                    EQW9Z0MQ
06880      IF EIBRCODE NOT = LOW-VALUE                                  EQW9Z0MQ
06881         MOVE 'FBXK-ERREUR LINK XKMTCADR' TO MESS                  EQW9Z0MQ
06882         GO TO ABANDON-TACHE                                       EQW9Z0MQ
06883      END-IF.                                                      EQW9Z0MQ
06884  FIN-CADRAGE.                                                     EQW9Z0MQ
06885      EXIT.                                                        EQW9Z0MQ
06886 *                                                                 EQW9Z0MQ
06887 **************************************************************    EQW9Z0MQ
06888 *   TEST DE LA DATE DE MISE EN CIRCULATION QUI NE DOIT PAS   *    EQW9Z0MQ
06889 *      EXCEDER LA DATE DU JOUR DE PLUS D'UN MOIS             *    EQW9Z0MQ
F3576 * (UTILISE EGALEMENT POUR LA DATE D'ACQUISITION DU VEHICULE, *    EQW9Z0MQ
F3576 *  POUR LAQUELLE LA MEME REGLE EST APPLIQUEE)                *    EQW9Z0MQ
06890 **************************************************************    EQW9Z0MQ
06891  VERIF-DATE-OK.                                                   EQW9Z0MQ
06892 *--------------                                                   EQW9Z0MQ
F3576      IF (WSS-FB-DATJOUR NOT = SPACES AND LOW-VALUE) AND
F3576         (WSS-DATE-TEST  NOT = SPACES AND LOW-VALUE)
06894         MOVE 01                     TO WSS-DATE-A-VERIFIER-JJ     EQW9Z0MQ
F3576         MOVE WSS-DATE-TEST (1:2)    TO WSS-DATE-A-VERIFIER-MM     EQW9Z0MQ
F3576         MOVE WSS-DATE-TEST (3:2)    TO WSS-DATE-A-VERIFIER-SS     EQW9Z0MQ
F3576         MOVE WSS-DATE-TEST (5:2)    TO WSS-DATE-A-VERIFIER-AA     EQW9Z0MQ
06898         MOVE WSS-DATE-A-VERIFIER-JJ TO WSS-DATE-SAMJ-J            EQW9Z0MQ
06899         MOVE WSS-DATE-A-VERIFIER-MM TO WSS-DATE-SAMJ-M            EQW9Z0MQ
06900         MOVE WSS-DATE-A-VERIFIER-AA TO WSS-DATE-SAMJ-A            EQW9Z0MQ
06901         MOVE WSS-DATE-A-VERIFIER-SS TO WSS-DATE-SAMJ-S            EQW9Z0MQ
06902         MOVE WSS-FB-DATJOUR-SS      TO WSS-DATE-JOUR-SAMJ-S       EQW9Z0MQ
06903         MOVE WSS-FB-DATJOUR-AA      TO WSS-DATE-JOUR-SAMJ-A       EQW9Z0MQ
06904         MOVE WSS-FB-DATJOUR-MM      TO WSS-DATE-JOUR-SAMJ-M       EQW9Z0MQ
06905         MOVE WSS-FB-DATJOUR-JJ      TO WSS-DATE-JOUR-SAMJ-J       EQW9Z0MQ
06906         IF WSS-DATE-SAMJ >= WSS-DATE-JOUR-SAMJ                    EQW9Z0MQ
06907            MOVE SPACES              TO K2COM-DATES                EQW9Z0MQ
06908            MOVE '3'                 TO K2-FONC                    EQW9Z0MQ
06909            MOVE WSS-FB-DATJOUR      TO K2-DATE1                   EQW9Z0MQ
06910            MOVE '1'                 TO K2-FORM1                   EQW9Z0MQ
06911            MOVE WSS-DATE-A-VERIFIER TO K2-DATE2                   EQW9Z0MQ
06912            MOVE '1'                 TO K2-FORM2                   EQW9Z0MQ
06913            EXEC CICS LINK PROGRAM  ('K200LDATE')                  EQW9Z0MQ
06914                           COMMAREA (K2COM-DATES)                  EQW9Z0MQ
06915                           LENGTH   (LENGTH OF K2COM-DATES)        EQW9Z0MQ
06916            END-EXEC                                               EQW9Z0MQ
06917            IF K2-RETCOD = '0'                                     EQW9Z0MQ
06918               IF K2-NBJOURS > 30                                  EQW9Z0MQ
06919                  MOVE 'N' TO WSS-DATE-OK                          EQW9Z0MQ
06920               ELSE                                                EQW9Z0MQ
06921                  MOVE 'O' TO WSS-DATE-OK                          EQW9Z0MQ
06922               END-IF                                              EQW9Z0MQ
06923            ELSE                                                   EQW9Z0MQ
06924               MOVE 'N' TO WSS-DATE-OK                             EQW9Z0MQ
06925            END-IF                                                 EQW9Z0MQ
06926         END-IF                                                    EQW9Z0MQ
06927      END-IF.                                                      EQW9Z0MQ
06928   FIN-VERIF-DATE-OK.                                              EQW9Z0MQ
06929      EXIT.                                                        EQW9Z0MQ
06930 *                                                                 EQW9Z0MQ
06931 ***************************************************************   EQW9Z0MQ
06932 *                  ACCES FBFOGA01                             *   EQW9Z0MQ
06933 ***************************************************************   EQW9Z0MQ
06934  ACCES-FBFOGA.                                                    EQW9Z0MQ
06935 *-------------                                                    EQW9Z0MQ
06936      MOVE SPACES                 TO XSPIPARM.                     EQW9Z0MQ
06937      MOVE 'GP'                   TO FONCTION  OF XSPIPARM.        EQW9Z0MQ
06938      MOVE 'FBFOGA'               TO TABLE-PREF.                   EQW9Z0MQ
06939      MOVE INF-CIE OF TS-SUSPENS1 TO TABLE-SUFF.                   EQW9Z0MQ
06940      MOVE IDENT-TABLE            TO CODTAB    OF XSPIPARM.        EQW9Z0MQ
06941      MOVE ECR-VEHTYPCO           TO CLE-FOGTYVC.                  EQW9Z0MQ
06942      MOVE ECR-VEHFORCO           TO CLE-FOGFORC.                  EQW9Z0MQ
06943      MOVE WSS-CODE-GTI           TO CLE-FOGGTIC.                  EQW9Z0MQ
06944      MOVE WSS-CLE-FBFOGA         TO REF-POSTE OF XSPIPARM.        EQW9Z0MQ
06945      MOVE '= '                   TO OPERATEUR OF XSPIPARM.        EQW9Z0MQ
06946      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
06947      IF RETCOD OF XSPIPARM  = ZERO                                EQW9Z0MQ
06948         MOVE IOAREA  OF XSPIPARM TO FBFOGA01                      EQW9Z0MQ
06949         MOVE 'O'                 TO WSS-GTI-TROUVE                EQW9Z0MQ
06950      ELSE                                                         EQW9Z0MQ
06951         MOVE 'N'                 TO WSS-GTI-TROUVE                EQW9Z0MQ
06952      END-IF.                                                      EQW9Z0MQ
06953  FIN-ACCES-FBFOGA.                                                EQW9Z0MQ
06954      EXIT.                                                        EQW9Z0MQ
06955                                                                   EQW9Z0MQ
F41702*----------------------------------------------------------------*EQW9Z0MQ
F41702*--> ACCES FB4FRM01 : F41702 REMPLACE FB4FRA ET FB4NFR            EQW9Z0MQ
F41702*----------------------------------------------------------------*EQW9Z0MQ
F41702 ACCES-FB4FRM.                                                    EQW9Z0MQ
F41702*------------*                                                    EQW9Z0MQ
F41702
06961      MOVE SPACES                 TO XSPIPARM.                     EQW9Z0MQ
06962      MOVE 'GP'                   TO FONCTION  OF XSPIPARM.        EQW9Z0MQ
F41702     MOVE 'FB4FRM'               TO TABLE-PREF.                   EQW9Z0MQ
06964      MOVE INF-CIE OF TS-SUSPENS1 TO TABLE-SUFF.                   EQW9Z0MQ
06965      MOVE IDENT-TABLE            TO CODTAB    OF XSPIPARM.        EQW9Z0MQ
06966      IF CTE-NIVPRM OF TS-SUSPENS1 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
F41702        MOVE CTE-NIVPRM OF TS-SUSPENS1 TO CLE-FM4PRMC             EQW9Z0MQ
06968      ELSE                                                         EQW9Z0MQ
06969         IF CTE-NIVPRM OF TS-SUSPENS2 NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
F41702           MOVE CTE-NIVPRM OF TS-SUSPENS2 TO CLE-FM4PRMC          EQW9Z0MQ
06971         END-IF                                                    EQW9Z0MQ
06972      END-IF.                                                      EQW9Z0MQ
F41702     MOVE ECR-VEHTYPCO           TO CLE-FM4TYPE.                  EQW9Z0MQ
06974      IF VEHCLAC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
F41702        MOVE VEHCLAC OF TS-VEHICULE(1) TO CLE-FM4CLAC             EQW9Z0MQ
06976      ELSE                                                         EQW9Z0MQ
06977         IF VEHCLAC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
F41702           MOVE VEHCLAC OF TS-VEHICULE(2) TO CLE-FM4CLAC          EQW9Z0MQ
06979         END-IF                                                    EQW9Z0MQ
06980      END-IF.                                                      EQW9Z0MQ
F41702     MOVE WSS-CODE-GTI           TO CLE-FM4GARA.                  EQW9Z0MQ
F41702     MOVE 'P'                    TO CLE-FM4NIVF.                  EQW9Z0MQ
F41702     MOVE WSS-CLE-FB4FRM         TO REF-POSTE OF XSPIPARM.        EQW9Z0MQ
06982      MOVE '= '                   TO OPERATEUR OF XSPIPARM.        EQW9Z0MQ
06983      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
06984      IF RETCOD OF XSPIPARM  = ZERO                                EQW9Z0MQ
F41702        MOVE IOAREA  OF XSPIPARM TO FB4FRM01                      EQW9Z0MQ
F41702        MOVE 'O'                 TO WSS-FRANCHISE-TROUVE          EQW9Z0MQ
06987      ELSE                                                         EQW9Z0MQ
06988         MOVE 'N'                 TO WSS-FRANCHISE-TROUVE          EQW9Z0MQ
F41702        MOVE 'FB035'             TO COM-GENE-MESANO               EQW9Z0MQ
06990                                     COM-CODERR                    EQW9Z0MQ
06991      END-IF.                                                      EQW9Z0MQ
F41702 FIN-ACCES-FB4FRM.                                                EQW9Z0MQ
06993      EXIT.                                                        EQW9Z0MQ
F41702                                                                  EQW9Z0MQ
F41702*----------------------------------------------------------------*EQW9Z0MQ
07030 *                  ACCES FB2FRA01                             *   EQW9Z0MQ
F41702*----------------------------------------------------------------*EQW9Z0MQ
07032  ACCES-FB2FRA.                                                    EQW9Z0MQ
07033 *-------------                                                    EQW9Z0MQ
07034      MOVE SPACES                 TO XSPIPARM.                     EQW9Z0MQ
07035      MOVE 'GP'                   TO FONCTION  OF XSPIPARM.        EQW9Z0MQ
07036      MOVE 'FB2FRA'               TO TABLE-PREF.                   EQW9Z0MQ
07037      MOVE INF-CIE OF TS-SUSPENS1 TO TABLE-SUFF.                   EQW9Z0MQ
07038      MOVE IDENT-TABLE            TO CODTAB    OF XSPIPARM.        EQW9Z0MQ
07039      IF CTE-NIVPRM OF TS-SUSPENS1 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
07040         MOVE CTE-NIVPRM OF TS-SUSPENS1 TO CLE-FR2PRMC             EQW9Z0MQ
07041      ELSE                                                         EQW9Z0MQ
07042         IF CTE-NIVPRM OF TS-SUSPENS2 NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
07043            MOVE CTE-NIVPRM OF TS-SUSPENS2 TO CLE-FR2PRMC          EQW9Z0MQ
07044         END-IF                                                    EQW9Z0MQ
07045      END-IF.                                                      EQW9Z0MQ
07046      MOVE WSS-CODE-GTI           TO CLE-FR2GTIC.                  EQW9Z0MQ
07047      IF VEHCLAC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
07048         MOVE VEHCLAC OF TS-VEHICULE(1) TO CLE-FR2CLAC             EQW9Z0MQ
07049      ELSE                                                         EQW9Z0MQ
07050         IF VEHCLAC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
07051            MOVE VEHCLAC OF TS-VEHICULE(2) TO CLE-FR2CLAC          EQW9Z0MQ
07052         END-IF                                                    EQW9Z0MQ
07053      END-IF.                                                      EQW9Z0MQ
07054      IF VEHPRTC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
07055         MOVE VEHPRTC OF TS-VEHICULE(1) TO CLE-FR2PRTC             EQW9Z0MQ
07056      ELSE                                                         EQW9Z0MQ
07057         IF VEHPRTC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
07058            MOVE VEHPRTC OF TS-VEHICULE(2) TO CLE-FR2PRTC          EQW9Z0MQ
07059         END-IF                                                    EQW9Z0MQ
07060      END-IF.                                                      EQW9Z0MQ
07061      MOVE WSS-CLE-FB2FRA         TO REF-POSTE OF XSPIPARM.        EQW9Z0MQ
07062      MOVE '= '                   TO OPERATEUR OF XSPIPARM.        EQW9Z0MQ
07063      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
07064      IF RETCOD OF XSPIPARM  = ZERO                                EQW9Z0MQ
07065         MOVE IOAREA  OF XSPIPARM TO FB2FRA01                      EQW9Z0MQ
07066         MOVE 'O'                 TO WSS-FRANCHISE-TROUVE          EQW9Z0MQ
07067      ELSE                                                         EQW9Z0MQ
07068         MOVE 'N'                 TO WSS-FRANCHISE-TROUVE          EQW9Z0MQ
07069      END-IF.                                                      EQW9Z0MQ
07070  FIN-ACCES-FB2FRA.                                                EQW9Z0MQ
07071      EXIT.                                                        EQW9Z0MQ
07072 *                                                                 EQW9Z0MQ
07073 ***************************************************************   EQW9Z0MQ
07074 *                  ACCES FBMTLT01                             *   EQW9Z0MQ
07075 ***************************************************************   EQW9Z0MQ
07076  ACCES-FBMTLT.                                                    EQW9Z0MQ
07077 *-------------                                                    EQW9Z0MQ
07078      MOVE SPACES                 TO XSPIPARM.                     EQW9Z0MQ
07079      MOVE 'GP'                   TO FONCTION  OF XSPIPARM.        EQW9Z0MQ
07080      MOVE 'FBMTLT'               TO TABLE-PREF.                   EQW9Z0MQ
07081      MOVE INF-CIE OF TS-SUSPENS1 TO TABLE-SUFF.                   EQW9Z0MQ
07082      MOVE IDENT-TABLE            TO CODTAB    OF XSPIPARM.        EQW9Z0MQ
07083      MOVE ECR-VEHTYPCO           TO CLE-MTLTYVC.                  EQW9Z0MQ
07084      MOVE WSS-CODE-GTI           TO CLE-MTLGTIC.                  EQW9Z0MQ
07085      MOVE WSS-CLE-FBMTLT         TO REF-POSTE OF XSPIPARM.        EQW9Z0MQ
07086      MOVE '= '                   TO OPERATEUR OF XSPIPARM.        EQW9Z0MQ
07087      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
07088      IF RETCOD OF XSPIPARM  = ZERO                                EQW9Z0MQ
07089         MOVE IOAREA  OF XSPIPARM TO FBMTLT01                      EQW9Z0MQ
07090         MOVE 'O'                 TO WSS-MONTANT-TROUVE            EQW9Z0MQ
07091      ELSE                                                         EQW9Z0MQ
07092         MOVE 'N'                 TO WSS-MONTANT-TROUVE            EQW9Z0MQ
07093      END-IF.                                                      EQW9Z0MQ
07094  FIN-ACCES-FBMTLT.                                                EQW9Z0MQ
07095      EXIT.                                                        EQW9Z0MQ
07096 *                                                                 EQW9Z0MQ
07097 ***************************************************************   EQW9Z0MQ
07098 *                  ACCES FB4BDG01                             *   EQW9Z0MQ
07099 ***************************************************************   EQW9Z0MQ
07100  ACCES-FB4BDG.                                                    EQW9Z0MQ
07101 *-------------                                                    EQW9Z0MQ
07102      MOVE SPACES                 TO XSPIPARM.                     EQW9Z0MQ
07103      MOVE 'GP'                   TO FONCTION  OF XSPIPARM.        EQW9Z0MQ
07104      MOVE 'FB4BDG'               TO TABLE-PREF.                   EQW9Z0MQ
07105      MOVE INF-CIE OF TS-SUSPENS1 TO TABLE-SUFF.                   EQW9Z0MQ
07106      MOVE IDENT-TABLE            TO CODTAB    OF XSPIPARM.        EQW9Z0MQ
07107      IF CTE-NIVPRM OF TS-SUSPENS1 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
07108         MOVE CTE-NIVPRM OF TS-SUSPENS1 TO CLE-BD4PRMC             EQW9Z0MQ
07109      ELSE                                                         EQW9Z0MQ
07110         IF CTE-NIVPRM OF TS-SUSPENS2 NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
07111            MOVE CTE-NIVPRM OF TS-SUSPENS2 TO CLE-BD4PRMC          EQW9Z0MQ
07112         END-IF                                                    EQW9Z0MQ
07113      END-IF.                                                      EQW9Z0MQ
07114      IF VEHCLAC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
07115         MOVE VEHCLAC OF TS-VEHICULE(1) TO CLE-BD4CLAC             EQW9Z0MQ
07116      ELSE                                                         EQW9Z0MQ
07117         IF VEHCLAC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
07118            MOVE VEHCLAC OF TS-VEHICULE(2) TO CLE-BD4CLAC          EQW9Z0MQ
07119         END-IF                                                    EQW9Z0MQ
07120      END-IF.                                                      EQW9Z0MQ
07121      MOVE WSS-CLE-FB4BDG         TO REF-POSTE OF XSPIPARM.        EQW9Z0MQ
07122      MOVE '= '                   TO OPERATEUR OF XSPIPARM.        EQW9Z0MQ
07123      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
07124      IF RETCOD OF XSPIPARM  = ZERO                                EQW9Z0MQ
07125         MOVE IOAREA  OF XSPIPARM TO FB4BDG01                      EQW9Z0MQ
07126         MOVE 'O'                 TO WSS-FRANCHISE-TROUVE          EQW9Z0MQ
07127      ELSE                                                         EQW9Z0MQ
07128         MOVE 'N'                 TO WSS-FRANCHISE-TROUVE          EQW9Z0MQ
07129      END-IF.                                                      EQW9Z0MQ
07130  FIN-ACCES-FB4BDG.                                                EQW9Z0MQ
07131      EXIT.                                                        EQW9Z0MQ
07132 *                                                                 EQW9Z0MQ
F41702*---------------------------------------------------------------* EQW9Z0MQ
F41702*--> ACCES FB4FVM01 : F41702 REMPLACE FB4FVO01                    EQW9Z0MQ
F41702*---------------------------------------------------------------* EQW9Z0MQ
F41702 ACCES-FB4FVM.                                                    EQW9Z0MQ
F41702*------------*                                                    EQW9Z0MQ

07138      MOVE SPACES                 TO XSPIPARM.                     EQW9Z0MQ
07139      MOVE 'GP'                   TO FONCTION  OF XSPIPARM.        EQW9Z0MQ
F41702     MOVE 'FB4FVM'               TO TABLE-PREF.                   EQW9Z0MQ
07141      MOVE INF-CIE OF TS-SUSPENS1 TO TABLE-SUFF.                   EQW9Z0MQ
07142      MOVE IDENT-TABLE            TO CODTAB    OF XSPIPARM.        EQW9Z0MQ
07143      IF CTE-NIVPRM OF TS-SUSPENS1 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
07144         MOVE CTE-NIVPRM OF TS-SUSPENS1 TO CLE-FV4PRMC             EQW9Z0MQ
07145      ELSE                                                         EQW9Z0MQ
07146         IF CTE-NIVPRM OF TS-SUSPENS2 NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
07147            MOVE CTE-NIVPRM OF TS-SUSPENS2 TO CLE-FV4PRMC          EQW9Z0MQ
07148         END-IF                                                    EQW9Z0MQ
07149      END-IF.                                                      EQW9Z0MQ
07150      IF VEHCLAC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
07151         MOVE VEHCLAC OF TS-VEHICULE(1) TO CLE-FV4CLAC             EQW9Z0MQ
07152      ELSE                                                         EQW9Z0MQ
07153         IF VEHCLAC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
07154            MOVE VEHCLAC OF TS-VEHICULE(2) TO CLE-FV4CLAC          EQW9Z0MQ
07155         END-IF                                                    EQW9Z0MQ
07156      END-IF.                                                      EQW9Z0MQ
F41702     MOVE '1'                    TO CLE-FV4TARI.                  EQW9Z0MQ
F41702     MOVE WSS-CLE-FB4FVM         TO REF-POSTE OF XSPIPARM.        EQW9Z0MQ
07158      MOVE '= '                   TO OPERATEUR OF XSPIPARM.        EQW9Z0MQ
07159      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
07160      IF RETCOD OF XSPIPARM  = ZERO                                EQW9Z0MQ
F41702        MOVE IOAREA  OF XSPIPARM TO FB4FVM01                      EQW9Z0MQ
07162         MOVE 'O'                 TO WSS-FRANCHISE-TROUVE          EQW9Z0MQ
07163      ELSE                                                         EQW9Z0MQ
07164         MOVE 'N'                 TO WSS-FRANCHISE-TROUVE          EQW9Z0MQ
07165      END-IF.                                                      EQW9Z0MQ
F41702 FIN-ACCES-FB4FVM.                                                EQW9Z0MQ
07167      EXIT.                                                        EQW9Z0MQ
F41702                                                                  EQW9Z0MQ
F41702*---------------------------------------------------------------* EQW9Z0MQ
F41702*--> ACCES FB4FDM01 : F41702 REMPLACE FB4FDO01                    EQW9Z0MQ
F41702*---------------------------------------------------------------* EQW9Z0MQ
F41702 ACCES-FB4FDM.                                                    EQW9Z0MQ
F41702*------------*                                                    EQW9Z0MQ

07174      MOVE SPACES                 TO XSPIPARM.                     EQW9Z0MQ
07175      MOVE 'GP'                   TO FONCTION  OF XSPIPARM.        EQW9Z0MQ
F41702     MOVE 'FB4FDM'               TO TABLE-PREF.                   EQW9Z0MQ
07177      MOVE INF-CIE OF TS-SUSPENS1 TO TABLE-SUFF.                   EQW9Z0MQ
07178      MOVE IDENT-TABLE            TO CODTAB    OF XSPIPARM.        EQW9Z0MQ
07179      IF CTE-NIVPRM OF TS-SUSPENS1 NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
07180         MOVE CTE-NIVPRM OF TS-SUSPENS1 TO CLE-FD4PRMC             EQW9Z0MQ
07181      ELSE                                                         EQW9Z0MQ
07182         IF CTE-NIVPRM OF TS-SUSPENS2 NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
07183            MOVE CTE-NIVPRM OF TS-SUSPENS2 TO CLE-FD4PRMC          EQW9Z0MQ
07184         END-IF                                                    EQW9Z0MQ
07185      END-IF.                                                      EQW9Z0MQ
07186      IF VEHCLAC OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE      EQW9Z0MQ
07187         MOVE VEHCLAC OF TS-VEHICULE(1) TO CLE-FD4CLAC             EQW9Z0MQ
07188      ELSE                                                         EQW9Z0MQ
07189         IF VEHCLAC OF TS-VEHICULE(2) NOT = SPACES AND LOW-VALUE   EQW9Z0MQ
07190            MOVE VEHCLAC OF TS-VEHICULE(2) TO CLE-FD4CLAC          EQW9Z0MQ
07191         END-IF                                                    EQW9Z0MQ
07192      END-IF.                                                      EQW9Z0MQ
F41702     MOVE '1'                    TO CLE-FD4TARI.                  EQW9Z0MQ
F41702     MOVE WSS-CLE-FB4FDM         TO REF-POSTE OF XSPIPARM.        EQW9Z0MQ
07194      MOVE '= '                   TO OPERATEUR OF XSPIPARM.        EQW9Z0MQ
07195      PERFORM ACCES-SPI THRU FIN-ACCES-SPI.                        EQW9Z0MQ
07196      IF RETCOD OF XSPIPARM  = ZERO                                EQW9Z0MQ
F41702        MOVE IOAREA  OF XSPIPARM TO FB4FDM01                      EQW9Z0MQ
07198         MOVE 'O'                 TO WSS-FRANCHISE-TROUVE          EQW9Z0MQ
07199      ELSE                                                         EQW9Z0MQ
07200         MOVE 'N'                 TO WSS-FRANCHISE-TROUVE          EQW9Z0MQ
07201      END-IF.                                                      EQW9Z0MQ
F41702 FIN-ACCES-FB4FDM.                                                EQW9Z0MQ
07203      EXIT.                                                        EQW9Z0MQ
07204 *                                                                 EQW9Z0MQ
07205 * EN AN OU RP CHAQUE VEHICULE SUPPLÈMENTAIRE SANS RELEVÈ INFO     EQW9Z0MQ
07206 * BÈNÈFICIE D'UN CRM MOYEN ÈGAL ‡ LA MOYENNE DES CRM DES          EQW9Z0MQ
07207 * VEHICULES NON SUPPLÈMENTAIRE ET DES VÈHICULES SUPPLÈMENTAIRE    EQW9Z0MQ
07208 * AVEC RELEVÈ INFO : EN RÈALITÈ DONC DE TOUS LES VÈHICULES EN     EQW9Z0MQ
07209 * COURS AU CONTRAT (LE CAS D'UN VÈHICULE NON SUPPLÈMENTAIRE       EQW9Z0MQ
07210 * SANS RI ETANT IMPOSSIBLE) ET SORTIS DEPEUIS MOINS D'UN AN       EQW9Z0MQ
07211 *                                                                 EQW9Z0MQ
07212  CALCUL-CRM-MOYEN.                                                EQW9Z0MQ
07213 *                                                                 EQW9Z0MQ
07214 * EXCLUSION DES 2R < OU = 80CM3                                   EQW9Z0MQ
07215 *                                                                 EQW9Z0MQ
07216      IF ECR-VEHTYPCO = '2R '                                      EQW9Z0MQ
07217         IF RVEHCYLN OF TS-VEHICULE(1) NOT = SPACES AND LOW-VALUE  EQW9Z0MQ
07218            IF VEHCYLN OF TS-VEHICULE(1) NOT > 80                  EQW9Z0MQ
07219               MOVE '2'    TO CONNEXES OF FBMISPTR-IT1             EQW9Z0MQ
07220               MOVE SPACES TO RANVBONT OF TS-VEHICULE(1)           EQW9Z0MQ
07221               MOVE SPACES TO RANVBOND OF TS-VEHICULE(1)           EQW9Z0MQ
07222               MOVE SPACES TO RANVABOD OF TS-VEHICULE(1)           EQW9Z0MQ
07223               MOVE SPACES TO RANVBONT OF TS-VEHICULE(2)           EQW9Z0MQ
07224               MOVE SPACES TO RANVBOND OF TS-VEHICULE(2)           EQW9Z0MQ
07225               MOVE SPACES TO RANVABOD OF TS-VEHICULE(2)           EQW9Z0MQ
07226               MOVE SPACES TO ECR-ANVBONTO                         EQW9Z0MQ
07227               MOVE SPACES TO ECR-ANVBONDO                         EQW9Z0MQ
07228               MOVE SPACES TO ECR-ANVABODO                         EQW9Z0MQ
07229               PERFORM REMISE-BLANC-CRM THRU FIN-REMISE-BLANC-CRM  EQW9Z0MQ
07230               GO TO FIN-CALCUL-CRM-MOYEN                          EQW9Z0MQ
07231            END-IF                                                 EQW9Z0MQ
07232         ELSE                                                      EQW9Z0MQ
07233            IF RVEHCYLN OF TS-VEHICULE(2) NOT = SPACES             EQW9Z0MQ
07234                                                      AND LOW-VALUEEQW9Z0MQ
07235               IF VEHCYLN OF TS-VEHICULE(2) NOT > 80               EQW9Z0MQ
07236                  MOVE '2'    TO CONNEXES OF FBMISPTR-IT1          EQW9Z0MQ
07237                  MOVE SPACES TO RANVBONT OF TS-VEHICULE(1)        EQW9Z0MQ
07238                  MOVE SPACES TO RANVBOND OF TS-VEHICULE(1)        EQW9Z0MQ
07239                  MOVE SPACES TO RANVABOD OF TS-VEHICULE(1)        EQW9Z0MQ
07240                  MOVE SPACES TO RANVBONT OF TS-VEHICULE(2)        EQW9Z0MQ
07241                  MOVE SPACES TO RANVBOND OF TS-VEHICULE(2)        EQW9Z0MQ
07242                  MOVE SPACES TO RANVABOD OF TS-VEHICULE(2)        EQW9Z0MQ
07243                  MOVE SPACES TO ECR-ANVBONTO                      EQW9Z0MQ
07244                  MOVE SPACES TO ECR-ANVBONDO                      EQW9Z0MQ
07245                  MOVE SPACES TO ECR-ANVABODO                      EQW9Z0MQ
07246                 PERFORM REMISE-BLANC-CRM THRU FIN-REMISE-BLANC-CRMEQW9Z0MQ
07247                  GO TO FIN-CALCUL-CRM-MOYEN                       EQW9Z0MQ
07248               END-IF                                              EQW9Z0MQ
07249            ELSE                                                   EQW9Z0MQ
07250               MOVE '2'    TO CONNEXES OF FBMISPTR-IT1             EQW9Z0MQ
07251               MOVE SPACES TO RANVBONT OF TS-VEHICULE(1)           EQW9Z0MQ
07252               MOVE SPACES TO RANVBOND OF TS-VEHICULE(1)           EQW9Z0MQ
07253               MOVE SPACES TO RANVABOD OF TS-VEHICULE(1)           EQW9Z0MQ
07254               MOVE SPACES TO RANVBONT OF TS-VEHICULE(2)           EQW9Z0MQ
07255               MOVE SPACES TO RANVBOND OF TS-VEHICULE(2)           EQW9Z0MQ
07256               MOVE SPACES TO RANVABOD OF TS-VEHICULE(2)           EQW9Z0MQ
07257               MOVE SPACES TO ECR-ANVBONTO                         EQW9Z0MQ
07258               MOVE SPACES TO ECR-ANVBONDO                         EQW9Z0MQ
07259               MOVE SPACES TO ECR-ANVABODO                         EQW9Z0MQ
07260               PERFORM REMISE-BLANC-CRM THRU FIN-REMISE-BLANC-CRM  EQW9Z0MQ
07261               GO TO FIN-CALCUL-CRM-MOYEN                          EQW9Z0MQ
07262            END-IF                                                 EQW9Z0MQ
07263         END-IF                                                    EQW9Z0MQ
07264      END-IF.                                                      EQW9Z0MQ
07265                                                                   EQW9Z0MQ
07266      PERFORM READ-TS-TECHNIQUE THRU FIN-READ-TS-TECHNIQUE         EQW9Z0MQ
07267      MOVE ZERO                 TO IND-D.                          EQW9Z0MQ
07268      MOVE ZERO                 TO ANVBONT OF TS-VEHICULE(2).      EQW9Z0MQ
07269      MOVE ZERO                 TO WSS-CRM-MOYEN.                  EQW9Z0MQ
07270      PERFORM VARYING IND-C FROM 1 BY 1 UNTIL IND-C > 40 OR        EQW9Z0MQ
07271                       TAB-VEHI(IND-C) = (SPACES OR LOW-VALUE)     EQW9Z0MQ
07272          IF  (VEHSORD OF TS-TECHNIQUE(IND-C) = '99999999')        EQW9Z0MQ
07273          AND (VEHRANTS OF TS-TECHNIQUE (IND-C) NOT =              EQW9Z0MQ
07274                                              COM-FB-RANG-TS-LIRE) EQW9Z0MQ
07275             IF ((RVEHCRMT OF TS-TECHNIQUE(IND-C) NOT = SPACES AND EQW9Z0MQ
07276                                                        LOW-VALUE) EQW9Z0MQ
07277             AND (VEHCRMC OF TS-TECHNIQUE(IND-C) = 'N'))           EQW9Z0MQ
07278                COMPUTE WSS-CRM-MOYEN  = (WSS-CRM-MOYEN +          EQW9Z0MQ
07279                           VEHCRMT OF TS-TECHNIQUE(IND-C) * 100)   EQW9Z0MQ
07280                ADD 1           TO IND-D                           EQW9Z0MQ
07281             END-IF                                                EQW9Z0MQ
07282          ELSE                                                     EQW9Z0MQ
07283             IF (VEHSORD OF TS-TECHNIQUE(IND-C) NOT = '99999999')  EQW9Z0MQ
07284                AND (VEHRANTS OF TS-TECHNIQUE (IND-C) NOT =        EQW9Z0MQ
07285                                              COM-FB-RANG-TS-LIRE) EQW9Z0MQ
07286                AND (RVEHSORD OF TAB-VEHI (IND-C)                  EQW9Z0MQ
07287                           NOT < RVEHENTD OF TAB-VEHI (IND-C))     EQW9Z0MQ
07288                EVALUATE TRUE                                      EQW9Z0MQ
07289                WHEN INF-EFFET OF TS-SUSPENS1 NOT = SPACE          EQW9Z0MQ
07290                                              AND LOW-VALUE        EQW9Z0MQ
07291                   MOVE INF-EFFET OF TS-SUSPENS1     TO W-EFFET    EQW9Z0MQ
07292                WHEN OTHER                                         EQW9Z0MQ
07293                   MOVE INF-EFFET OF TS-SUSPENS2     TO W-EFFET    EQW9Z0MQ
07294                END-EVALUATE                                       EQW9Z0MQ
07295                                                                   EQW9Z0MQ
07296                MOVE W-EFFET          TO  WSS-DATE-MOINS-1-AN      EQW9Z0MQ
07297                SUBTRACT 1 FROM       WSS-DATE-A-1-AN              EQW9Z0MQ
07298                                                                   EQW9Z0MQ
07299                IF  (VEHSORD OF TAB-VEHI (IND-C) >                 EQW9Z0MQ
07300                                  WSS-DATE-MOINS-1-AN)             EQW9Z0MQ
07301                AND (VEHCRMC  OF TAB-VEHI (IND-C) NOT = 'O')       EQW9Z0MQ
07302                AND (RVEHCRMT OF TAB-VEHI (IND-C) NOT = SPACE AND  EQW9Z0MQ
07303                                                     LOW-VALUE)    EQW9Z0MQ
07304                   COMPUTE WSS-CRM-MOYEN  =                        EQW9Z0MQ
07305                          (WSS-CRM-MOYEN +                         EQW9Z0MQ
07306                           VEHCRMT OF TS-TECHNIQUE(IND-C) * 100)   EQW9Z0MQ
07307                   ADD 1           TO IND-D                        EQW9Z0MQ
07308                END-IF                                             EQW9Z0MQ
07309             END-IF                                                EQW9Z0MQ
07310          END-IF                                                   EQW9Z0MQ
07311      END-PERFORM.                                                 EQW9Z0MQ
07312      IF IND-D > ZERO                                              EQW9Z0MQ
07313         COMPUTE WSS-CRM-MOYEN = WSS-CRM-MOYEN / IND-D             EQW9Z0MQ
07314         MOVE '0'               TO CONNEXES OF FBMISPTR-IT1        EQW9Z0MQ
07315      ELSE                                                         EQW9Z0MQ
07316         MOVE 100               TO WSS-CRM-MOYEN                   EQW9Z0MQ
07317         MOVE '1'               TO CONNEXES OF FBMISPTR-IT1        EQW9Z0MQ
07318      END-IF.                                                      EQW9Z0MQ
07319      MOVE WSS-CRM-MOYEN        TO WSS-CRM-MOYENZ.                 EQW9Z0MQ
07320      COMPUTE ANVBONT OF TS-VEHICULE (2) =                         EQW9Z0MQ
07321              WSS-CRM-MOYEN / 100                                  EQW9Z0MQ
07322      EVALUATE TRUE                                                EQW9Z0MQ
07323         WHEN INF-EFFET OF TS-SUSPENS1 NOT = SPACE AND LOW-VALUE   EQW9Z0MQ
07324              MOVE INF-EFFET OF TS-SUSPENS1     TO W-EFFET         EQW9Z0MQ
07325         WHEN OTHER                                                EQW9Z0MQ
07326              MOVE INF-EFFET OF TS-SUSPENS2     TO W-EFFET         EQW9Z0MQ
07327      END-EVALUATE.                                                EQW9Z0MQ
07328                                                                   EQW9Z0MQ
F9674      MOVE WSS-FB-DATJOUR-SS          TO WSS-DATE-JOUR-SAMJ-S      EQW9Z0MQ
F9674      MOVE WSS-FB-DATJOUR-AA          TO WSS-DATE-JOUR-SAMJ-A      EQW9Z0MQ
F9674      MOVE WSS-FB-DATJOUR-MM          TO WSS-DATE-JOUR-SAMJ-M      EQW9Z0MQ
F9674      MOVE WSS-FB-DATJOUR-JJ          TO WSS-DATE-JOUR-SAMJ-J      EQW9Z0MQ
F9674      IF W-EFFET(1:6) > WSS-DATE-JOUR-SAMJ(1:6)
F9674         MOVE WSS-DATE-JOUR-SAMJ(1:6) TO RANVBOND OF TS-VEHICULE(2)
F9674      ELSE
F9674         MOVE W-EFFET (1:6)           TO RANVBOND OF TS-VEHICULE(2)EQW9Z0MQ
F9674      END-IF.
07330      MOVE '01'                 TO RANVBOND OF TS-VEHICULE(2)(7:2).EQW9Z0MQ

07331      IF WSS-CRM-MOYEN <= 50                                       EQW9Z0MQ
07332         PERFORM RECH-DATE50 THRU FIN-RECH-DATE50                  EQW9Z0MQ
07333         IF WSS-DATE50-MIN NOT = SPACE AND LOW-VALUE AND HIGH-VALUEEQW9Z0MQ
07334            MOVE WSS-DATE50-MIN TO RANVABOD OF TS-VEHICULE(2)      EQW9Z0MQ
07335         ELSE                                                      EQW9Z0MQ
F9674           IF W-EFFET(1:6) > WSS-DATE-JOUR-SAMJ(1:6)
F9674             MOVE WSS-DATE-JOUR-SAMJ(1:4)
F9674                                 TO RANVABOD OF TS-VEHICULE(2)
F9674             MOVE '0101'         TO RANVABOD OF TS-VEHICULE(2)(5:4)EQW9Z0MQ
F9674           ELSE
07336             MOVE W-EFFET (1:4)  TO RANVABOD OF TS-VEHICULE(2)     EQW9Z0MQ
                  MOVE '0101'         TO RANVABOD OF TS-VEHICULE(2)(5:4)EQW9Z0MQ
F9674           END-IF
07338         END-IF                                                    EQW9Z0MQ
07339      END-IF.                                                      EQW9Z0MQ
07340 *                                                                 EQW9Z0MQ
07341 *                                                                 EQW9Z0MQ
07342  FIN-CALCUL-CRM-MOYEN.                                            EQW9Z0MQ
07343      EXIT.                                                        EQW9Z0MQ
07344 *                                                                 EQW9Z0MQ
07345  REMISE-BLANC-CRM.                                                EQW9Z0MQ
07346                                                                   EQW9Z0MQ
07347      MOVE '1'                       TO WSS-COE-TYPE.              EQW9Z0MQ
07348      MOVE 1                         TO I-OCCURS.                  EQW9Z0MQ
07349                                                                   EQW9Z0MQ
07350      PERFORM VARYING I-COE-TYPE FROM 1 BY 1                       EQW9Z0MQ
07351          UNTIL I-COE-TYPE > NB-COEV-MAX                           EQW9Z0MQ
07352             OR COVTYPC OF TS-VEHICULE (I-OCCURS, I-COE-TYPE)      EQW9Z0MQ
07353                                              = WSS-COE-TYPE       EQW9Z0MQ
07354      END-PERFORM.                                                 EQW9Z0MQ
07355                                                                   EQW9Z0MQ
07356      IF I-COE-TYPE NOT > NB-COEV-MAX                              EQW9Z0MQ
07357         IF RCOVTAUT OF TS-VEHICULE (I-OCCURS, I-COE-TYPE)         EQW9Z0MQ
07358                                         NOT = SPACE AND LOW-VALUE EQW9Z0MQ
07359            MOVE SPACES                TO                          EQW9Z0MQ
07360                 RCOVTAUT OF TS-VEHICULE (I-OCCURS, I-COE-TYPE)    EQW9Z0MQ
07361                 RCOVDATD OF TS-VEHICULE (I-OCCURS, I-COE-TYPE)    EQW9Z0MQ
07362         END-IF                                                    EQW9Z0MQ
07363      END-IF.                                                      EQW9Z0MQ
07364                                                                   EQW9Z0MQ
07365      MOVE '7'                       TO WSS-COE-TYPE.              EQW9Z0MQ
07366      MOVE 1                         TO I-OCCURS.                  EQW9Z0MQ
07367                                                                   EQW9Z0MQ
07368      PERFORM VARYING I-COE-TYPE FROM 1 BY 1                       EQW9Z0MQ
07369          UNTIL I-COE-TYPE > NB-COEV-MAX                           EQW9Z0MQ
07370             OR COVTYPC OF TS-VEHICULE (I-OCCURS, I-COE-TYPE)      EQW9Z0MQ
07371                                              = WSS-COE-TYPE       EQW9Z0MQ
07372      END-PERFORM.                                                 EQW9Z0MQ
07373                                                                   EQW9Z0MQ
07374      IF I-COE-TYPE NOT > NB-COEV-MAX                              EQW9Z0MQ
07375         IF RCOVTAUT OF TS-VEHICULE (I-OCCURS, I-COE-TYPE)         EQW9Z0MQ
07376                                         NOT = SPACE AND LOW-VALUE EQW9Z0MQ
07377            MOVE SPACES                TO                          EQW9Z0MQ
07378                 RCOVTAUT OF TS-VEHICULE (I-OCCURS, I-COE-TYPE)    EQW9Z0MQ
07379                 RCOVDATD OF TS-VEHICULE (I-OCCURS, I-COE-TYPE)    EQW9Z0MQ
07380         END-IF                                                    EQW9Z0MQ
07381      END-IF.                                                      EQW9Z0MQ
07382                                                                   EQW9Z0MQ
07383                                                                   EQW9Z0MQ
07384  FIN-REMISE-BLANC-CRM.                                            EQW9Z0MQ
07385      EXIT.                                                        EQW9Z0MQ
07386 *                                                                 EQW9Z0MQ
07387  RECH-DATE50.                                                     EQW9Z0MQ
07388                                                                   EQW9Z0MQ
07389      MOVE TS-VEHICULE           TO WSS-TS-VEHICULE.               EQW9Z0MQ
07390      MOVE COM-FB-RANG-TS-LIRE   TO WSS-RANG-TS-LIRE.              EQW9Z0MQ
07391                                                                   EQW9Z0MQ
07392      MOVE HIGH-VALUE            TO WSS-DATE50-MIN.                EQW9Z0MQ
07393                                                                   EQW9Z0MQ
07394      PERFORM VARYING IND-C FROM 1 BY 1                            EQW9Z0MQ
07395                UNTIL IND-C > 40                                   EQW9Z0MQ
07396                   OR TAB-VEHI(IND-C) = (SPACES OR LOW-VALUE)      EQW9Z0MQ
07397                                                                   EQW9Z0MQ
07398         IF  (VEHSORD OF TS-TECHNIQUE(IND-C) = '99999999')         EQW9Z0MQ
07399         AND (VEHRANTS OF TS-TECHNIQUE (IND-C)                     EQW9Z0MQ
07400                                         NOT = WSS-RANG-TS-LIRE)   EQW9Z0MQ
07401                                                                   EQW9Z0MQ
07402             IF  (RVEHCRMT OF TS-TECHNIQUE(IND-C) NOT = SPACE AND  EQW9Z0MQ
07403                                                        LOW-VALUE) EQW9Z0MQ
07404             AND (VEHCRMC OF TS-TECHNIQUE(IND-C) = 'N')            EQW9Z0MQ
07405             AND (VEHCRMT OF TS-TECHNIQUE(IND-C) <= 0,5)           EQW9Z0MQ
07406                                                                   EQW9Z0MQ
07407                MOVE VEHRANTS OF TS-TECHNIQUE (IND-C)              EQW9Z0MQ
07408                                 TO COM-FB-RANG-TS-LIRE            EQW9Z0MQ
07409                PERFORM  READ-TS-VEHICULE  THRU                    EQW9Z0MQ
07410                         FIN-READ-TS-VEHICULE                      EQW9Z0MQ
07411                                                                   EQW9Z0MQ
07412                PERFORM VARYING I-COE-TYPE FROM 1 BY 1             EQW9Z0MQ
07413                  UNTIL I-COE-TYPE > NB-COEV-MAX                   EQW9Z0MQ
07414                     OR COVTYPC OF TS-VEHICULE (1, I-COE-TYPE) = 7 EQW9Z0MQ
07415                END-PERFORM                                        EQW9Z0MQ
07416                IF I-COE-TYPE NOT > NB-COEV-MAX                    EQW9Z0MQ
07417                   IF RCOVDAFD OF TS-VEHICULE (1, I-COE-TYPE)      EQW9Z0MQ
07418                                         NOT = SPACE AND LOW-VALUE EQW9Z0MQ
07419                      IF RCOVDAFD OF TS-VEHICULE (1, I-COE-TYPE)   EQW9Z0MQ
07420                                                  < WSS-DATE50-MIN EQW9Z0MQ
07421                         MOVE RCOVDAFD OF TS-VEHICULE              EQW9Z0MQ
07422                                                   (1, I-COE-TYPE) EQW9Z0MQ
07423                                 TO WSS-DATE50-MIN                 EQW9Z0MQ
07424                      END-IF                                       EQW9Z0MQ
07425                   ELSE                                            EQW9Z0MQ
07426                      IF RCOVDATD OF TS-VEHICULE (1, I-COE-TYPE)   EQW9Z0MQ
07427                                                  < WSS-DATE50-MIN EQW9Z0MQ
07428                         MOVE RCOVDATD OF TS-VEHICULE              EQW9Z0MQ
07429                                                   (1, I-COE-TYPE) EQW9Z0MQ
07430                                 TO WSS-DATE50-MIN                 EQW9Z0MQ
07431                      END-IF                                       EQW9Z0MQ
07432                   END-IF                                          EQW9Z0MQ
07433                END-IF                                             EQW9Z0MQ
07434                                                                   EQW9Z0MQ
07435             END-IF                                                EQW9Z0MQ
07436         END-IF                                                    EQW9Z0MQ
07437      END-PERFORM.                                                 EQW9Z0MQ
07438                                                                   EQW9Z0MQ
07439      MOVE WSS-TS-VEHICULE       TO TS-VEHICULE.                   EQW9Z0MQ
07440      MOVE WSS-RANG-TS-LIRE      TO COM-FB-RANG-TS-LIRE.           EQW9Z0MQ
07441                                                                   EQW9Z0MQ
07442  FIN-RECH-DATE50.                                                 EQW9Z0MQ
07443      EXIT.                                                        EQW9Z0MQ
U4080 ***************************************************************** EQW90EUK
U4080 * RECHERCHE SI LA GARANTIE EXISTE DANS LA TS VEHICULE OCCURS 4  * EQW90EUK
U4080 ***************************************************************** EQW90EUK
U4080  RECHERCHE-GTIE-OCCS1.                                            EQW90EUK
U4080 *---------------------                                            EQW90EUK
U4080                                                                   EQW90EUK
U4080      PERFORM VARYING I-GTI-CODE FROM 1 BY 1                       EQW90EUK
U4080         UNTIL GTICODC OF TS-VEHICULE(1, I-GTI-CODE) = WSS-CODE-GTIEQW90EUK
U4080             OR I-GTI-CODE > 30                                    EQW90EUK
U4080      END-PERFORM.                                                 EQW90EUK
U4080                                                                   EQW90EUK
U4080      IF I-GTI-CODE > 30                                           EQW90EUK
U4080         MOVE 0         TO I-GTI-CODE                              EQW90EUK
U4080      ELSE                                                         EQW90EUK
U4080         MOVE GTIFRAM OF TS-VEHICULE(1, I-GTI-CODE)                EQW90EUK
U4080           TO GTIFRAM OF TS-VEHICULE(2, IND-GTI2)                  EQW90EUK
U4080         MOVE GTIFRZM OF TS-VEHICULE(1, I-GTI-CODE)                EQW90EUK
U4080           TO GTIFRZM OF TS-VEHICULE(2, IND-GTI2)                  EQW90EUK
U4080         MOVE GTIFRTM OF TS-VEHICULE(1, I-GTI-CODE)                EQW90EUK
U4080           TO GTIFRTM OF TS-VEHICULE(2, IND-GTI2)                  EQW90EUK
F41702        MOVE GTITARC OF TS-VEHICULE(1, I-GTI-CODE)                EQW90EUK
F41702          TO GTITARC OF TS-VEHICULE(2, IND-GTI2)                  EQW90EUK
U4080      END-IF.                                                      EQW90EUK
U4080 *                                                                 EQW90EUK
U4080  FIN-RECHERCHE-GTIE-OCCS1.                                        EQW90EUK
U4080      EXIT.                                                        EQW90EUK
07444 *                                                                 EQW9Z0MQ
07444 *                                                                 EQW9Z0MQ
07445 ***************************************************************** EQW9Z0MQ
07446 * ACCES SPITAB                                                    EQW9Z0MQ
07447 ***************************************************************** EQW9Z0MQ
07448  ++INCLUDE SQKCSPI2                                               EQW9Z0MQ
07449 ***************************************************************** EQW9Z0MQ
07450 * SORTIE ABANDON POUR ERREURS    NON PREVUES                      EQW9Z0MQ
07451 ***************************************************************** EQW9Z0MQ
07452  ABANDON-TACHE.                                                   EQW9Z0MQ
07453  ++INCLUDE SQKCMROB                                               EQW9Z0MQ
U3319  ++INCLUDE SQKCCON2                                               EFUTSV6O
07454 ** FIN DE PROGRAMME  FB04T00  CREE LE  01/03/02  A  09:39  .      EQW9Z0MQ
